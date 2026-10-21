;;; org-dock.el --- Take a heading out to work on it, then float it back  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines
;; URL: https://github.com/yoshzucker/org-dock

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A ship that needs work is taken out of the water into a dry dock,
;; worked on there, and floated back.  This does the same to a heading:
;; `org-dock' takes the heading at point, with everything under it, out
;; into a copy of its own, and C-c C-c in the copy writes it back.  While
;; it is out, the heading cannot be changed in its file, so what is done in
;; the copy is all that changes, and nothing outside the heading changes
;; at all.
;;
;; That bound is what lets an agent be asked without a second thought.
;; C-c C-; in the copy makes `org-dock-agent-function' ready to work on it:
;; the copy is the one file the agent is to write; the file the heading
;; came from and the files its links point to are there to be read.  What
;; the agent writes comes back as the copy's file changing, and what
;; reaches the heading is what the copy holds when C-c C-c writes it back.
;;
;; `org-dock'            take the heading at point out into its copy;
;;                       before the first heading, or with C-u, the whole file
;; `org-dock-finish'     C-c C-c in the copy: write it back, and save
;; `org-dock-abort'      C-c C-k: let the copy go; the heading stays as it was
;; `org-dock-ask-agent'  C-c C-; : make an agent ready to work on the copy
;;
;; See README.org.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function org-attach-expand "org-attach" (file))
(defvar org-attach-id-dir)

(defgroup org-dock nil
  "Take a heading out into a copy to work on, with you or an agent."
  :group 'org
  :prefix "org-dock-")

(defcustom org-dock-directory (locate-user-emacs-file "org-dock/")
  "Directory the copies are kept in while their headings are out.
Away from the files the headings come from, so that nothing gathering
those files -- an index of notes, a search, version control -- meets a
copy, which carries the same ids as its heading."
  :type 'directory
  :group 'org-dock)

(defcustom org-dock-agent-function nil
  "Function making an agent ready to work on a copy, or nil.
Called by \\[org-dock-ask-agent] with a plist:

  :copy     the copy, the one file the agent is to write
  :origin   the file the heading was taken out of, to read and not write
  :heading  the heading's title, or the file's when :whole
  :whole    non-nil when the copy is the whole of the file
  :lines    (FIRST . LAST), the lines of that file the copy is of, or nil
            when the heading is not found there as it was taken out
  :request  a file telling the agent all of this, to be read first
  :links    the copy's links that point to files, as (NAME . FILE)
  :files    those files, each once
  :line     the line point is on in the copy
  :region   (FIRST . LAST), the lines selected in the copy, or nil

What is asked is said where the agent is spoken to; the function only
gets the agent ready to be asked, and the place to ask it.  What the agent
writes into the copy comes back as the copy's file changing, which the
copy's buffer follows."
  :type '(choice (const :tag "None" nil) function)
  :group 'org-dock)

(defvar org-dock-link-file-functions (list #'org-dock-file-link-file)
  "Functions finding the file a link in a copy points to.
Each is called with the link's type and path, as strings, with point on
the link in the copy, and returns a file, or (FILE . NAME) to have the
file named NAME rather than by the link, or nil when the link is not of
its kind.  The first found is the link's.  A package with links of its
own adds a function here, so that what they point to is handed to an
agent with the rest.")

(defvar org-dock-request-functions nil
  "Functions adding to what an agent is told about a copy.
Each is called in the copy's buffer with the plist
`org-dock-agent-function' gets, but for :request, and returns a section
to add to the request file, headed \"## TITLE\", or nil.  A package whose
files have rules of their own says them here.")

;;;; A heading taken out

(cl-defstruct (org-dock--session
               (:constructor org-dock--make-session)
               (:copier nil))
  "A heading out in its copy."
  origin        ; the buffer the heading is in
  file          ; that buffer's file
  beg end       ; markers around the heading and what is under it
  original      ; their text when it was taken out
  copy          ; the copy's file
  heading       ; the heading's title
  whole         ; non-nil when what is out is the whole file
  windows       ; the window configuration to put back, or nil
  finish        ; called once the copy is written back, or nil
  abort)        ; called once the copy is let go, or nil

(defvar-local org-dock--session nil
  "In a copy's buffer, the heading it is a copy of.")
(put 'org-dock--session 'permanent-local t)

(defconst org-dock--locked
  "This heading is out in its copy; C-c C-c there writes it back"
  "What changing a heading that is out says.")

(defun org-dock--meta-file (copy)
  "Return the file kept beside COPY saying where it came from."
  (concat (file-name-sans-extension copy) ".eld"))

(defun org-dock--request-file (copy)
  "Return the file telling an agent about COPY."
  (concat (file-name-sans-extension copy) ".request.md"))

(defun org-dock--new-copy-file (heading)
  "Return a file in `org-dock-directory' for a new copy of HEADING.
Named by the time and the heading, with nothing in the name a shell or
an agent's prompt would take apart."
  (let* ((slug (replace-regexp-in-string
                "[][/\\\\:*?\"<>|@#$%&;`'(){}!~^[:space:][:cntrl:]]+" "-" heading))
         (slug (string-trim (truncate-string-to-width slug 40) "-+" "-+"))
         (stem (concat (format-time-string "%Y%m%dT%H%M%S")
                       (if (string-empty-p slug) "" (concat "-" slug))))
         (n 1)
         file)
    (while (file-exists-p
            (setq file (expand-file-name
                        (concat stem (if (= n 1) "" (format "-%d" n)) ".org")
                        org-dock-directory)))
      (cl-incf n))
    file))

(defun org-dock--delete-files (copy)
  "Delete COPY and the files kept beside it."
  (dolist (file (list copy (org-dock--meta-file copy) (org-dock--request-file copy)))
    (when (file-exists-p file) (delete-file file))))

(defun org-dock--write (file text)
  "Write TEXT to FILE, as UTF-8."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file (insert text))))

(defun org-dock--read-meta (file)
  "Return the plist in FILE, written by `org-dock--write-meta'."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (read (current-buffer))))

(defun org-dock--write-meta (session)
  "Write beside SESSION's copy where it came from and what it was.
So that a copy left when Emacs stopped can be gone on with."
  (let ((print-length nil) (print-level nil))
    (org-dock--write (org-dock--meta-file (org-dock--session-copy session))
                     (prin1-to-string
                      (list :file (org-dock--session-file session)
                            :heading (org-dock--session-heading session)
                            :original (org-dock--session-original session)
                            :time (format-time-string "%F %R"))))))

(defun org-dock--origin-name (session)
  "Return the name of SESSION's file, or of its buffer when it has none."
  (if (org-dock--session-file session)
      (file-name-nondirectory (org-dock--session-file session))
    (buffer-name (org-dock--session-origin session))))

(defun org-dock--bounds (pos &optional whole)
  "Return (BEG . END) of the heading at POS with everything under it.
With WHOLE, of the whole buffer."
  (org-with-wide-buffer
   (goto-char pos)
   (if whole
       (cons (point-min) (point-max))
     (org-back-to-heading t)
     (cons (point) (progn (org-end-of-subtree t t) (point))))))

(defun org-dock--title ()
  "Return what names the whole of this buffer: its title, or its file's name."
  (or (org-get-title)
      (if buffer-file-name
          (file-name-base buffer-file-name)
        (buffer-name))))

;;;; Keeping the heading as it was

(defun org-dock--lock (session)
  "Keep SESSION's heading from being changed in its file while it is out.
Text can still be written right before and right after it: before, since
read-only text does not reach forward, and after, because its last
character is told not to reach back -- Emacs asks that character alone,
whatever `text-property-default-nonsticky' says."
  (with-current-buffer (org-dock--session-origin session)
    (let ((beg (org-dock--session-beg session))
          (end (org-dock--session-end session)))
      (with-silent-modifications
        (add-text-properties beg end (list 'read-only org-dock--locked 'org-dock session))
        (when (> end beg)
          (let ((old (get-text-property (1- end) 'rear-nonsticky)))
            (unless (eq old t)
              (put-text-property (1- end) end 'rear-nonsticky
                                 (append '(read-only org-dock) old)))))))))

(defun org-dock--unlock (session)
  "Let SESSION's heading be changed in its file again, wherever it is now."
  (let ((buffer (org-dock--session-origin session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (org-with-wide-buffer
         (with-silent-modifications
           (let ((pos (point-min)))
             (while (setq pos (text-property-any pos (point-max) 'org-dock session))
               (let* ((end (or (next-single-property-change pos 'org-dock) (point-max)))
                      (tail (get-text-property (1- end) 'rear-nonsticky)))
                 (remove-text-properties pos end '(read-only nil org-dock nil))
                 (when (and (consp tail) (memq 'org-dock tail))
                   (let ((rest (cl-set-difference tail '(read-only org-dock))))
                     (if rest
                         (put-text-property (1- end) end 'rear-nonsticky rest)
                       (remove-text-properties (1- end) end '(rear-nonsticky nil)))))
                 (setq pos end))))))))))

(defun org-dock--origin-buffer (session)
  "Return the buffer of SESSION's file, visiting it again if it was closed."
  (let ((buffer (org-dock--session-origin session)))
    (unless (buffer-live-p buffer)
      (unless (org-dock--session-file session)
        (user-error "org-dock: the buffer the heading was in is gone; the copy is kept, %s"
                    (abbreviate-file-name (org-dock--session-copy session))))
      (setq buffer (find-file-noselect (org-dock--session-file session)))
      (setf (org-dock--session-origin session) buffer))
    buffer))

(defun org-dock--find (session)
  "Return (BEG . END) of SESSION's heading in this buffer, as it was taken out.
Where its markers say, while the text there is still the text taken out;
else wherever that text is found once, and only once.  Nil when it is not."
  (let* ((text (org-dock--session-original session))
         (beg (org-dock--session-beg session))
         (end (org-dock--session-end session))
         (beg (and (markerp beg) (eq (marker-buffer beg) (current-buffer))
                   (marker-position beg)))
         (end (and beg (markerp end) (marker-position end))))
    (if (and beg end (string= (buffer-substring-no-properties beg end) text))
        (cons beg end)
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((case-fold-search nil))
         (when (search-forward text nil t)
           (let ((found (cons (match-beginning 0) (match-end 0))))
             (unless (search-forward text nil t)
               found))))))))

(defun org-dock--release-markers (session)
  "Let go of SESSION's markers, so they no longer slow its buffer."
  (dolist (marker (list (org-dock--session-beg session) (org-dock--session-end session)))
    (when (markerp marker) (set-marker marker nil))))

(defun org-dock--place (session bounds)
  "Point SESSION's markers at BOUNDS in its buffer, and keep it as it is.
Text written right before the heading goes before the start, and right
after it after the end, so the markers hold the heading alone."
  (org-dock--release-markers session)
  (with-current-buffer (org-dock--session-origin session)
    (setf (org-dock--session-beg session) (copy-marker (car bounds) t)
          (org-dock--session-end session) (copy-marker (cdr bounds) nil))
    (add-hook 'after-revert-hook #'org-dock--relock nil t))
  (org-dock--lock session))

(defun org-dock--relock ()
  "Keep the headings of this buffer that are out as they are, after a revert.
A revert can take the text's properties with it: the file changed on
disk, a sync from another machine, say.  A heading found as it was taken
out is kept again; one that is not is left for its copy to say so."
  (dolist (session (org-dock--live-sessions))
    (when (eq (org-dock--session-origin session) (current-buffer))
      (if-let* ((bounds (org-dock--find session)))
          (org-dock--place session bounds)
        (message "org-dock: %s changed on disk while a heading was out"
                 (org-dock--origin-name session))))))

;;;; The copy

(defvar org-dock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-dock-finish)
    (define-key map (kbd "C-c C-k") #'org-dock-abort)
    (define-key map (kbd "C-c C-;") #'org-dock-ask-agent)
    map)
  "Keys of a copy.")

(define-minor-mode org-dock-mode
  "A copy of a heading taken out of its file, to be written back.
\\{org-dock-mode-map}"
  :lighter " Dock"
  (when org-dock-mode
    (setq header-line-format
          (substitute-command-keys
           (format "\\<org-dock-mode-map>Out of %s.  \\[org-dock-finish] write back, \\[org-dock-abort] let go%s"
                   (org-dock--origin-name org-dock--session)
                   (if org-dock-agent-function
                       ", \\[org-dock-ask-agent] ask an agent"
                     ""))))))

(defun org-dock--here ()
  "Return the session of the copy in this buffer, or signal."
  (or org-dock--session (user-error "org-dock: this is not a copy")))

(defun org-dock--open-copy (session)
  "Return the buffer of SESSION's copy, set up to be worked on."
  (let* ((buffer (find-file-noselect (org-dock--session-copy session)))
         (origin (org-dock--session-origin session))
          (dir (if (org-dock--session-file session)
                  (file-name-directory (org-dock--session-file session))
                (buffer-local-value 'default-directory origin))))
    (with-current-buffer buffer
      (setq org-dock--session session)
      ;; Links and attachments are read as they would be in the heading's
      ;; own file: relative to its directory, by its settings.
      (setq default-directory dir)
      (when (and (boundp 'org-attach-id-dir) (buffer-live-p origin))
        (setq-local org-attach-id-dir
                    (expand-file-name (buffer-local-value 'org-attach-id-dir origin) dir)))
      (unless org-dock-mode (org-dock-mode 1))
      ;; An agent writes the copy's file; the buffer follows.
      (unless (bound-and-true-p auto-revert-mode) (auto-revert-mode 1)))
    buffer))

(defun org-dock--copy-text (session)
  "Return the text of SESSION's copy, ended as the heading was."
  (let ((text (with-current-buffer (find-buffer-visiting (org-dock--session-copy session))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (if (and (string-suffix-p "\n" (org-dock--session-original session))
             (not (string-suffix-p "\n" text)))
        (concat text "\n")
      text)))

(defun org-dock--live-sessions ()
  "Return the sessions whose copies are open."
  (delq nil (mapcar (lambda (buffer) (buffer-local-value 'org-dock--session buffer))
                    (buffer-list))))

(defun org-dock--leftover (file text)
  "Return a copy of TEXT taken out of FILE that is left in the dock, or nil.
As a plist from its meta file, with :copy."
  (when (file-directory-p org-dock-directory)
    (seq-some
     (lambda (meta-file)
       (let* ((meta (ignore-errors (org-dock--read-meta meta-file)))
              (copy (concat (file-name-sans-extension meta-file) ".org")))
         (and meta
              (equal (plist-get meta :original) text)
              (file-exists-p copy)
              (plist-get meta :file)
              (file-equal-p (plist-get meta :file) file)
              (not (find-buffer-visiting copy))
              (plist-put meta :copy copy))))
     (directory-files org-dock-directory t "\\.eld\\'"))))

(defun org-dock--show (buffer)
  "Show the copy BUFFER in another window, as `org-capture' does."
  (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-pop-up-window))))

;;;###autoload
(defun org-dock (&optional pos whole)
  "Take the heading at point out into a copy, with everything under it.
Show the copy, to be worked on.  In it,
\\<org-dock-mode-map>\\[org-dock-finish] writes it back and saves the file,
and \\[org-dock-abort] lets it go.  While it is out the heading cannot be
changed in its file.  On a heading that is out, show its copy.  A copy of
the heading left from before -- Emacs stopped with it open -- is offered
to go on with.  POS, a position in the current buffer, is used for point.

Before the first heading, or with WHOLE, interactively the prefix
argument, the whole file is taken out: for a draft of all of it, say,
written by an agent at one go."
  (interactive (list nil current-prefix-arg))
  (org-dock-heading (or pos (point)) :whole whole))

(cl-defun org-dock-heading (pos &key whole at finish abort (display t) (restore t))
  "Take the heading at POS in the current buffer out into its copy.
Return the copy's buffer, shown unless DISPLAY is nil.  With WHOLE, or
before the first heading, the whole buffer is taken out.

AT, a position in the heading, is where point goes in the copy; else it
is at the heading.  FINISH is called in the heading's buffer with the
heading's new bounds once the copy is written back, before the file is
saved; ABORT is called there once the copy is let go.  With RESTORE nil
the windows are left as they are when the copy is done with; else the
window configuration from before is put back."
  (when org-dock--session
    (user-error "org-dock: this is a copy already; %s writes it back"
                (substitute-command-keys "\\<org-dock-mode-map>\\[org-dock-finish]")))
  (unless (derived-mode-p 'org-mode)
    (user-error "org-dock: not in an Org buffer"))
  ;; The configuration is taken with the selected window's buffer current,
  ;; since that is where the user is: putting it back goes back there, not
  ;; to whatever buffer a caller happened to be in.
  (let* ((windows (and restore (with-current-buffer (window-buffer (selected-window))
                                 (current-window-configuration))))
         (out (get-text-property (if (and (= pos (point-max)) (> pos (point-min)))
                                     (1- pos)
                                   pos)
                                 'org-dock))
         (buffer (if (and out (or (not whole) (org-dock--session-whole out)))
                     (org-dock--open-copy out)
                   (org-dock--take-out pos whole at windows finish abort))))
    (when display (org-dock--show buffer))
    buffer))

(defun org-dock--take-out (pos whole at windows finish abort)
  "Take the heading at POS, which is not out, out into its copy.
Return the copy's buffer.  WHOLE, AT, WINDOWS, FINISH and ABORT are as
for `org-dock-heading', WINDOWS being the configuration to put back."
  (let* ((whole (or whole (org-with-wide-buffer
                           (goto-char pos)
                           (org-before-first-heading-p))))
         (bounds (org-dock--bounds pos whole))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (text-property-not-all beg end 'org-dock nil)
      (user-error (if whole
                      "org-dock: a heading of this file is out; write it back first"
                    "org-dock: a heading under this one is out; write it back first")))
    (let* ((text (buffer-substring-no-properties beg end))
           (session (or (org-dock--live-session text)
                        (org-dock--new-session
                         text (if whole
                                  (org-dock--title)
                                (save-excursion (goto-char beg) (org-get-heading t t t t)))
                         whole))))
      (setf (org-dock--session-origin session) (current-buffer)
            (org-dock--session-windows session) windows
            (org-dock--session-finish session) finish
            (org-dock--session-abort session) abort)
      (org-dock--place session bounds)
      (let ((buffer (org-dock--open-copy session)))
        (when (and at (<= beg at end))
          (with-current-buffer buffer
            (goto-char (min (point-max) (1+ (- at beg))))))
        buffer))))

(defun org-dock--live-session (text)
  "Return the session of TEXT out of this buffer's file, with its copy open.
Out of this buffer itself, when it visits no file.  Nil when there is none."
  (seq-find (lambda (s)
              (and (equal (org-dock--session-original s) text)
                   (if buffer-file-name
                       (and (org-dock--session-file s)
                            (file-equal-p (org-dock--session-file s) buffer-file-name))
                     (eq (org-dock--session-origin s) (current-buffer)))))
            (org-dock--live-sessions)))

(defun org-dock--new-session (text heading &optional whole)
  "Return a session of TEXT, the heading HEADING of this buffer, with a copy.
WHOLE says TEXT is the whole buffer, HEADING its title.  The copy is one
left from before when it is to be gone on with, or else one written now."
  (let ((session (org-dock--make-session
                  :file buffer-file-name :original text :heading heading
                  :whole whole
                  :copy (org-dock--leftover-copy text))))
    (unless (org-dock--session-copy session)
      (make-directory org-dock-directory t)
      (setf (org-dock--session-copy session) (org-dock--new-copy-file heading))
      (org-dock--write (org-dock--session-copy session) text)
      ;; A heading of a buffer with no file cannot be gone on with
      ;; once Emacs stops: nothing would say where to write it back.
      (when buffer-file-name
        (org-dock--write-meta session)))
    session))

(defun org-dock--leftover-copy (text)
  "Return a copy of TEXT left from this buffer's file, if it is gone on with.
A copy left and not gone on with is thrown away, and nil returned."
  (when-let* ((left (and buffer-file-name
                         (org-dock--leftover buffer-file-name text))))
    (if (y-or-n-p (format "A copy of this heading from %s is left; go on with it (no throws it away)? "
                          (plist-get left :time)))
        (plist-get left :copy)
      (org-dock--delete-files (plist-get left :copy))
      nil)))

(defun org-dock--close (session)
  "Close SESSION's copy, delete what it left in the dock, put the windows back."
  (let ((copy (org-dock--session-copy session))
        (windows (org-dock--session-windows session)))
    (when-let* ((buffer (find-buffer-visiting copy)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (setq org-dock--session nil))
      (kill-buffer buffer))
    (org-dock--delete-files copy)
    (org-dock--release-markers session)
    (when (and windows (frame-live-p (window-configuration-frame windows)))
      (set-window-configuration windows))))

(defun org-dock--replace (beg end text)
  "Make the text from BEG to END be TEXT, changing only what differs.
So that what is folded and where point is survive where nothing changed."
  (let ((source (generate-new-buffer " *org-dock*" t)))
    (unwind-protect
        (progn
          (with-current-buffer source (insert text))
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)
              (replace-buffer-contents source 1.0))))
      (kill-buffer source))))

(defun org-dock--above-line (text original)
  "Return the line of TEXT with a heading above ORIGINAL's first, or nil.
Above is fewer stars: written back, such a heading would take in the
headings that come after ORIGINAL's heading in its file.  A heading at
its level, or below it, is only written where the heading was."
  (when (string-match "\\`\\(\\*+\\)[ \t]" original)
    (let ((level (length (match-string 1 original)))
          (start 0)
          found)
      (while (and (not found)
                  (string-match "^\\(\\*+\\)[ \t]" text start))
        (if (< (length (match-string 1 text)) level)
            (setq found (1+ (cl-count ?\n (substring text 0 (match-beginning 0)))))
          (setq start (match-end 0))))
      found)))

(defun org-dock-finish ()
  "Write this copy back over its heading, save the heading's file, and close it.
The heading is found where it was taken out, or wherever its text is,
unchanged, once; when it is not, the copy is kept and nothing is written.
Nor is it when the copy has a heading above its first heading's level,
which written back would take in the headings after it in the file;
headings at that level, or below, are written where the heading was."
  (interactive)
  (let ((session (org-dock--here)))
    (when (buffer-modified-p) (save-buffer))
    (let ((text (org-dock--copy-text session))
          (copy (org-dock--session-copy session)))
      (when-let* ((line (org-dock--above-line text (org-dock--session-original session))))
        (goto-char (point-min))
        (forward-line (1- line))
        (user-error "org-dock: line %d is a heading above the copy's own level; written back it would take in the headings after it"
                    line))
      (with-current-buffer (org-dock--origin-buffer session)
        (org-with-wide-buffer
         (let ((bounds (org-dock--find session))
               (tick (buffer-chars-modified-tick)))
           (unless bounds
             (user-error "org-dock: the heading has changed in its file since it was taken out; the copy is kept, %s"
                         (abbreviate-file-name copy)))
           (org-dock--unlock session)
           (unless (equal text (org-dock--session-original session))
             (org-dock--replace (car bounds) (cdr bounds) text))
           (when-let* ((finish (org-dock--session-finish session)))
             (funcall finish (car bounds) (+ (car bounds) (length text))))
           (unless (or (= tick (buffer-chars-modified-tick))
                       (not buffer-file-name))
             (save-buffer)))))
      (org-dock--close session)
      (message "org-dock: written back into %s" (org-dock--origin-name session)))))

(defun org-dock-abort ()
  "Let this copy go, leaving its heading as it was taken out."
  (interactive)
  (let ((session (org-dock--here)))
    (when (or (equal (org-dock--copy-text session) (org-dock--session-original session))
              (y-or-n-p "Let go of what was changed in the copy? "))
      (org-dock--unlock session)
      (when-let* ((abort (org-dock--session-abort session))
                  (buffer (org-dock--session-origin session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (funcall abort))))
      (org-dock--close session))))

;;;; Asking an agent

(defun org-dock-file-link-file (type path)
  "Return the file a `file:' or `attachment:' link of PATH points to, or nil.
TYPE says which.  A file is read relative to the heading's own directory,
which is the copy's `default-directory'."
  (let ((name (car (split-string path "::"))))
    (pcase type
      ("file" (let ((file (expand-file-name name)))
                (and (file-exists-p file) file)))
      ("attachment" (require 'org-attach)
       (let ((file (ignore-errors (org-attach-expand name))))
         (and file (file-exists-p file) file))))))

(defun org-dock--link-type (path)
  "Return (TYPE . TARGET) of a link to PATH, or nil when it has no type.
A path with no type that starts as a file name does is a `file:' link;
any other, to a heading or a target, is not one to a file."
  (cond ((string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):\\(.*\\)\\'" path)
         (cons (match-string 1 path) (match-string 2 path)))
        ((string-match-p "\\`\\(?:/\\|\\./\\|\\.\\./\\|~\\)" path)
         (cons "file" path))))

(defun org-dock-links ()
  "Return the links in this buffer that point to files, as (NAME . FILE).
FILE is what `org-dock-link-file-functions' finds for a link, and NAME
the name it gives the file, or else the link as written; a file is given
once, for the first link to it.  A link in a property counts, since a
property can hold one."
  (let (found)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-link-bracket-re nil t)
       (let* ((link (match-string-no-properties 0))
              (at (match-beginning 0))
              (parsed (org-dock--link-type (org-link-unescape (match-string-no-properties 1))))
              (file (and parsed
                         (save-excursion
                           (goto-char at)
                           (run-hook-with-args-until-success
                            'org-dock-link-file-functions (car parsed) (cdr parsed))))))
         (let ((name (if (consp file) (cdr file) link))
               (file (if (consp file) (car file) file)))
           (when (and file (not (rassoc file found)))
             (push (cons name file) found))))))
    (nreverse found)))

(defun org-dock--line-at (pos &optional end)
  "Return the line of POS; with END, of the last character before it."
  (line-number-at-pos (if (and end (> pos (point-min))
                               (save-excursion (goto-char pos) (bolp)))
                          (1- pos)
                        pos)))

(defun org-dock-request-text (request)
  "Return what an agent is told about the copy REQUEST names.
REQUEST is the plist `org-dock-agent-function' gets.  The copy, the only
file to edit; the file it came from, not to be edited, and which of its
lines the copy is; where `here' is; and the files the links point to, a
directory to be read through.  Then what `org-dock-request-functions'
add, each a section of its own.  The same words whatever was taken out,
a heading or the whole file; only what they name differs."
  (let ((region (plist-get request :region))
        (lines (plist-get request :lines))
        (links (plist-get request :links)))
    (concat
     "## Files\n\n"
     (format "- The copy, the only file to edit:\n  %s\n" (plist-get request :copy))
     (format "- The file it came from: read it when you need what is around the copy,\n  and do not edit it.%s\n  %s\n"
             (cond ((plist-get request :whole) "  The copy is all of it.")
                   (lines (format "  The copy is lines %d to %d of it." (car lines) (cdr lines)))
                   (t ""))
             (plist-get request :origin))
     "\n## Here\n\n"
     (if region
         (format "When my instruction points at a place, \"here\" is line %d of the copy,\nand \"the selection\" is lines %d to %d of the copy.\n"
                 (plist-get request :line) (car region) (cdr region))
       (format "When my instruction points at a place, \"here\" is line %d of the copy.\n"
               (plist-get request :line)))
     (when links
       (concat "\n## Linked files\n\n"
               "The links in the copy point to these files.  Read a file when you need what it says;\na directory, the files in it.\n\n"
               (mapconcat (lambda (l)
                            (format "- %s\n  %s" (car l)
                                    (if (file-directory-p (cdr l))
                                        (file-name-as-directory (cdr l))
                                      (cdr l))))
                          links "\n")
               "\n"))
     (mapconcat (lambda (part) (concat "\n" (string-trim part) "\n"))
                (delq nil (mapcar (lambda (fn) (funcall fn request))
                                  org-dock-request-functions))
                ""))))

(defun org-dock-ask-agent ()
  "Make `org-dock-agent-function' ready to work on this copy.
The copy is saved first, and what the agent is to know is written to a
file beside it; see `org-dock-agent-function'.  What to do is asked where
the agent is spoken to."
  (interactive)
  (let ((session (org-dock--here)))
    (unless org-dock-agent-function
      (user-error "org-dock: no `org-dock-agent-function'"))
    (when (buffer-modified-p) (save-buffer))
    (let* ((links (org-dock-links))
           (copy (org-dock--session-copy session))
           (bounds (with-current-buffer (org-dock--origin-buffer session)
                     (org-with-wide-buffer
                      (when-let* ((b (org-dock--find session)))
                        (cons (line-number-at-pos (car b))
                              (org-dock--line-at (cdr b) t))))))
           (request (list :copy copy
                          :origin (or (org-dock--session-file session)
                                      (org-dock--origin-name session))
                          :heading (org-dock--session-heading session)
                          :whole (org-dock--session-whole session)
                          :lines bounds
                          :links links
                          :files (delete-dups (mapcar #'cdr links))
                          :line (line-number-at-pos)
                          :region (and (use-region-p)
                                       (cons (org-dock--line-at (region-beginning))
                                             (org-dock--line-at (region-end) t)))))
           (file (org-dock--request-file copy)))
      (org-dock--write file (org-dock-request-text request))
      (funcall org-dock-agent-function (append request (list :request file))))))

;;;; Asked of a copy by other packages

(defun org-dock-origin-file (&optional buffer)
  "Return the file BUFFER's heading was taken out of, when BUFFER is a copy.
For reading the copy as its heading would be read where it lives."
  (let ((session (buffer-local-value 'org-dock--session (or buffer (current-buffer)))))
    (and session (org-dock--session-file session))))

(defun org-dock-copy-origin (copy)
  "Return where the copy in the file COPY came from, or nil when it is no copy.
As a plist: :file, the file its heading was taken out of, and :original,
the text taken out.  Read from the file kept beside COPY, so that it is
known outside the Emacs that took the heading out: to a command an agent
runs, say."
  (let ((meta (org-dock--meta-file copy)))
    (when (file-exists-p meta)
      (let ((plist (ignore-errors (org-dock--read-meta meta))))
        (and (plist-get plist :file) (stringp (plist-get plist :original))
             (list :file (plist-get plist :file) :original (plist-get plist :original)))))))

(defun org-dock-copy-of (pos)
  "Return the buffer of the copy of the heading out at POS, or nil.
POS is a position in the current buffer.  The copy is opened if its
buffer was closed.  For writing there what would be written at POS,
where the heading cannot be changed while it is out."
  (when-let* ((session (get-text-property pos 'org-dock)))
    (org-dock--open-copy session)))

(provide 'org-dock)
;;; org-dock.el ends here
