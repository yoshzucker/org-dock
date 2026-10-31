;;; org-dock-test.el --- Tests for org-dock  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L test -l test/org-dock-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-dock)

(defconst org-dock-test--text
  (concat "#+title: Notes\n"
          "* Before\nabove\n"
          "* Topic\n:PROPERTIES:\n:ID: abc\n:END:\nBody.\n** Child\nunder\n"
          "* After\nbelow\n")
  "A file with a heading to take out, and headings around it.")

(defconst org-dock-test--topic
  "* Topic\n:PROPERTIES:\n:ID: abc\n:END:\nBody.\n** Child\nunder\n"
  "The heading Topic, with what is under it.")

(defmacro org-dock-test--with-file (text &rest body)
  "Run BODY visiting a file holding TEXT, in a directory of its own.
`dir' is the directory, `file' the file; the dock is inside `dir'."
  (declare (indent 1) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "org-dock-test" t)))
          (org-dock-directory (expand-file-name "dock/" dir))
          (file (expand-file-name "notes.org" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           (with-current-buffer (find-file-noselect file)
             (save-window-excursion ,@body)))
       (dolist (buffer (buffer-list))
         (let ((name (buffer-file-name buffer)))
           (when (and name (file-in-directory-p name dir))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(defun org-dock-test--goto (heading)
  "Move to the start of HEADING's line."
  (goto-char (point-min))
  (re-search-forward (concat "^\\*+ " (regexp-quote heading) "$"))
  (beginning-of-line))

(defun org-dock-test--dock (&rest args)
  "Take the heading at point out as `org-dock' does, staying in this buffer.
ARGS are those of `org-dock-heading' after its position."
  (save-current-buffer (apply #'org-dock-heading (point) args)))

(defun org-dock-test--header-line ()
  "Return what this copy's header line says.
As its form, which redisplay evaluates and batch does not."
  (should (equal header-line-format '(:eval (org-dock--header-line))))
  (org-dock--header-line))

(defun org-dock-test--file-text (file)
  "Return what FILE holds on disk."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest org-dock-test-takes-the-heading-out ()
  "The copy holds the heading and what is under it, read as in its own
directory.  In the file the heading cannot be changed, while text can be
written right before and right after it."
  (org-dock-test--with-file org-dock-test--text
    (let ((origin (current-buffer)))
      (org-dock-test--goto "Topic")
      (let ((copy (org-dock-test--dock)))
        (with-current-buffer copy
          (should org-dock-mode)
          (should (equal (buffer-string) org-dock-test--topic))
          (should (file-in-directory-p buffer-file-name org-dock-directory))
          (should (equal default-directory dir))
          (should (equal (org-dock-origin-file) file)))
        (with-current-buffer origin
          (org-dock-test--goto "Topic")
          (forward-char 2)
          (should-error (insert "x") :type 'text-read-only)
          (org-dock-test--goto "Child")
          (should-error (delete-char 1) :type 'text-read-only)
          (org-dock-test--goto "Topic")
          (insert "before\n")
          (org-dock-test--goto "After")
          (insert "after\n")
          (should (string-match-p "above\nbefore\n\\* Topic" (buffer-string)))
          (should (string-match-p "under\nafter\n\\* After" (buffer-string))))))))

(ert-deftest org-dock-test-finish-writes-back ()
  "C-c C-c writes the copy over the heading, saves the file, and leaves
nothing in the dock.  The heading can be changed again."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* ((copy (org-dock-test--dock))
           (copy-file (buffer-file-name copy)))
      (with-current-buffer copy
        (goto-char (point-max))
        (insert "** New\n")
        (org-dock-finish))
      (should-not (buffer-live-p copy))
      (should-not (directory-files org-dock-directory nil "\\`[^.]"))
      (should-not (file-exists-p copy-file))
      (should (string-match-p "under\n\\*\\* New\n\\* After" (buffer-string)))
      (should-not (buffer-modified-p))
      (should (equal (org-dock-test--file-text file) (buffer-string)))
      (org-dock-test--goto "Topic")
      (insert "x")
      (should (looking-at "\\* Topic")))))

(ert-deftest org-dock-test-finish-unchanged-touches-nothing ()
  "A copy written back as it was taken out changes and saves nothing."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (with-current-buffer (org-dock-test--dock)
      (org-dock-finish))
    (should-not (buffer-modified-p))
    (should (equal (buffer-string) org-dock-test--text))))

(ert-deftest org-dock-test-abort-lets-the-copy-go ()
  "C-c C-k leaves the heading as it was, after asking when the copy was
changed, and calls what the caller gave for it in the heading's buffer."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* ((origin (current-buffer))
           called
           (copy (org-dock-test--dock :abort (lambda () (setq called (current-buffer))))))
      (with-current-buffer copy
        (insert "changed ")
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (org-dock-abort)))
      (should (eq called origin))
      (should-not (buffer-live-p copy))
      (should (equal (buffer-string) org-dock-test--text))
      (should-not (directory-files org-dock-directory nil "\\`[^.]"))
      (org-dock-test--goto "Topic")
      (insert "x"))))

(ert-deftest org-dock-test-at-and-finish ()
  "AT puts point in the copy where it is in the heading; FINISH is given
the heading's new bounds."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* ((body (save-excursion (search-forward "Body.") (match-beginning 0)))
           written
           (copy (org-dock-test--dock
                  :at body
                  :finish (lambda (beg end)
                            (setq written (buffer-substring-no-properties beg end))))))
      (with-current-buffer copy
        (should (looking-at "Body\\."))
        (insert "New ")
        (org-dock-finish))
      (should (equal written (string-replace "Body." "New Body." org-dock-test--topic))))))

(ert-deftest org-dock-test-heading-moved-in-its-file ()
  "Text written above the heading while it is out does not move where the
copy goes back."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let ((copy (org-dock-test--dock)))
      (goto-char (point-min))
      (insert "#+startup: overview\n")
      (with-current-buffer copy
        (goto-char (point-max))
        (insert "added\n")
        (org-dock-finish))
      (should (string-prefix-p "#+startup: overview\n#+title" (buffer-string)))
      (should (string-match-p "above\n\\* Topic" (buffer-string)))
      (should (string-match-p "under\nadded\n\\* After" (buffer-string))))))

(ert-deftest org-dock-test-heading-changed-in-its-file ()
  "When the heading was changed in its file all the same, the copy is not
written over it: it is kept, to say so."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let ((copy (org-dock-test--dock)))
      (let ((inhibit-read-only t))
        (search-forward "Body.")
        (insert " Forced."))
      (with-current-buffer copy
        (insert "x")
        (should-error (org-dock-finish) :type 'user-error)
        (should (file-exists-p buffer-file-name)))
      (should (buffer-live-p copy))
      (should (string-match-p "Body\\. Forced\\." (buffer-string))))))

(ert-deftest org-dock-test-revert-keeps-it-out ()
  "A file changed on disk and reverted keeps the heading out."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let ((copy (org-dock-test--dock)))
      (with-temp-file file (insert "# from elsewhere\n" org-dock-test--text))
      (revert-buffer t t t)
      (org-dock-test--goto "Topic")
      (should (get-text-property (point) 'org-dock))
      (forward-char 2)
      (should-error (insert "x") :type 'text-read-only)
      (with-current-buffer copy
        (goto-char (point-max))
        (insert "added\n")
        (org-dock-finish))
      (should (string-prefix-p "# from elsewhere\n" (buffer-string)))
      (should (string-match-p "under\nadded\n\\* After" (buffer-string))))))

(ert-deftest org-dock-test-out-once ()
  "A heading that is out shows its copy; one with a heading out under it,
and a copy itself, are not taken out."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Child")
    (let ((copy (org-dock-test--dock)))
      (org-dock-test--goto "Child")
      (should (eq (org-dock-test--dock) copy))
      (org-dock-test--goto "Topic")
      (should-error (org-dock-test--dock) :type 'user-error)
      (with-current-buffer copy
        (should-error (org-dock-test--dock) :type 'user-error)))))

(ert-deftest org-dock-test-goes-on-with-a-copy-left ()
  "A copy left when Emacs stopped is offered when its heading, unchanged,
is taken out again, and is gone on with."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* ((copy (org-dock-test--dock))
           (copy-file (buffer-file-name copy)))
      (with-current-buffer copy
        (goto-char (point-max))
        (insert "kept\n")
        (save-buffer))
      ;; Emacs stops: the copy's buffer goes, and the heading's keeping.
      (kill-buffer copy)
      (let ((inhibit-read-only t))
        (with-silent-modifications
          (set-text-properties (point-min) (point-max) nil)))
      (org-dock-test--goto "Topic")
      (let ((again (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                     (org-dock-test--dock))))
        (should (equal (buffer-file-name again) copy-file))
        (should (string-suffix-p "kept\n" (with-current-buffer again (buffer-string))))
        (with-current-buffer again (org-dock-finish))
        (should (string-match-p "under\nkept\n\\* After" (buffer-string)))))))

(ert-deftest org-dock-test-a-file-named-by-its-function ()
  "A function may name the file a link points to, in place of the link."
  (org-dock-test--with-file "* Topic\n[[thing:x1][a thing]]\n"
    (let ((org-dock-link-file-functions
           (list (lambda (type path)
                   (and (equal type "thing")
                        (cons (expand-file-name (concat path ".pdf") dir) (concat "Thing " path)))))))
      (goto-char (point-min))
      (with-current-buffer (org-dock-test--dock)
        (should (equal (org-dock-links)
                       (list (cons "Thing x1" (expand-file-name "x1.pdf" dir)))))))))

(ert-deftest org-dock-test-links-point-to-files ()
  "Links in the copy, in its text and its properties, are given with the
files they point to: a file by a path relative to the heading's own
directory, and a link of another kind by a function given for it.  A file
linked twice is given once, with the first link to it."
  (org-dock-test--with-file
      (concat "* Topic\n:PROPERTIES:\n:SOURCE: [[thing:x1][a thing]]\n:END:\n"
              "See [[file:data.txt][data]], [[./data.txt]], [[file:missing.txt]] and [[Topic]].\n")
    (with-temp-file (expand-file-name "data.txt" dir) (insert "d"))
    (let ((org-dock-link-file-functions
           (cons (lambda (type path)
                   (and (equal type "thing") (expand-file-name (concat path ".pdf") dir)))
                 org-dock-link-file-functions)))
      (goto-char (point-min))
      (with-current-buffer (org-dock-test--dock)
        (should (equal (org-dock-links)
                       (list (cons "[[thing:x1][a thing]]" (expand-file-name "x1.pdf" dir))
                             (cons "[[file:data.txt][data]]" (expand-file-name "data.txt" dir)))))))))

(ert-deftest org-dock-test-ask-agent-hands-over-the-copy ()
  "The agent is handed the copy, saved, and a request file saying what to
write, what to read, where point is, and what others add."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* (got
           (org-dock-agent-function (lambda (request) (setq got request)))
           (org-dock-request-functions (list (lambda (_) "Rules of mine."))))
      (with-current-buffer (org-dock-test--dock)
        (goto-char (point-max))
        (insert "unsaved\n")
        (forward-line -1)
        (org-dock-ask-agent)
        (should-not (buffer-modified-p))
        (should (equal (plist-get got :copy) buffer-file-name))
        (should (equal (plist-get got :origin) file))
        (should (equal (plist-get got :heading) "Topic"))
        (should (= (plist-get got :line) 8))
        (should-not (plist-get got :region))
        (let ((text (org-dock-test--file-text (plist-get got :request))))
          (should (string-match-p (regexp-quote buffer-file-name) text))
          (should (string-match-p (regexp-quote file) text))
          (should (string-match-p "\"here\" is line 8 of the copy" text))
          (should (equal (plist-get got :lines) '(4 . 10)))
          (should (string-match-p "The copy is lines 4 to 10 of it\\." text))
          (should (string-match-p "Rules of mine\\." text)))))))

(ert-deftest org-dock-test-headings-beside-not-above ()
  "A heading written in the copy at the heading's level goes back beside it;
one above that level, which would take in the headings after it, stops
the copy being written back, and point is put on it."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Child")
    (let ((copy (org-dock-test--dock)))
      (with-current-buffer copy
        (goto-char (point-max))
        (insert "* Above\n")
        (goto-char (point-min))
        (should-error (org-dock-finish) :type 'user-error)
        (should (looking-at "\\* Above"))
        (delete-region (point) (point-max))
        (insert "** Beside\nnew\n")
        (org-dock-finish))
      (should-not (buffer-live-p copy))
      (should (string-match-p "under\n\\*\\* Beside\nnew\n\\* After" (buffer-string))))))

(ert-deftest org-dock-test-a-buffer-with-no-file ()
  "A heading of an Org buffer visiting no file is taken out and written back
the same, with nothing saved."
  (let ((org-dock-directory (make-temp-file "org-dock-test" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert org-dock-test--text)
          (org-dock-test--goto "Topic")
          (let ((copy (org-dock-test--dock)))
            (with-current-buffer copy
              (should (string-match-p "Out of" (org-dock-test--header-line)))
              (goto-char (point-max))
              (insert "added\n")
              (org-dock-finish)))
          (should (string-match-p "under\nadded\n\\* After" (buffer-string))))
      (delete-directory org-dock-directory t))))

(ert-deftest org-dock-test-the-whole-file ()
  "Before the first heading, the whole file is taken out, named by its
title; a heading at any level is written back, and the agent is told the
copy is the whole of the file.  A directory a link points to is given as
one."
  (org-dock-test--with-file (concat org-dock-test--text "[[file:drafts/]]\n")
    (make-directory (expand-file-name "drafts" dir))
    (let* ((origin (current-buffer))
           (whole (buffer-string))
           got
           (org-dock-agent-function (lambda (request) (setq got request))))
      (goto-char (point-min))
      (let ((copy (org-dock-test--dock)))
        (with-current-buffer copy
          (should (equal (buffer-string) whole))
          (should (equal (org-dock--session-heading org-dock--session) "Notes"))
          (org-dock-ask-agent)
          (should (plist-get got :whole))
          (should (equal (plist-get got :files) (list (expand-file-name "drafts/" dir))))
          (let ((text (org-dock-test--file-text (plist-get got :request))))
            (should (string-match-p "The copy is all of it\\." text))
            (should (string-match-p (regexp-quote (file-name-as-directory
                                                   (expand-file-name "drafts" dir)))
                                    text))))
        (with-current-buffer origin
          (goto-char (point-max))
          (forward-line -1)
          (should-error (insert "x") :type 'text-read-only)
          (org-dock-test--goto "Topic")
          (should (eq (org-dock-test--dock) copy)))
        (with-current-buffer copy
          (goto-char (point-max))
          (insert "* New\n")
          (org-dock-finish))
        (should (equal (buffer-string) (concat whole "* New\n")))
        (should (equal (org-dock-test--file-text file) (buffer-string)))))))

(ert-deftest org-dock-test-the-whole-file-by-prefix ()
  "With WHOLE, the whole file is taken out from inside a heading too; not
while a heading of it is out."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Child")
    (with-current-buffer (org-dock-test--dock :whole t)
      (should (equal (buffer-string) org-dock-test--text))
      (org-dock-abort))
    (org-dock-test--goto "Child")
    (org-dock-test--dock)
    (should-error (org-dock-test--dock :whole t) :type 'user-error)))

(ert-deftest org-dock-test-copy-origin ()
  "Where a copy came from is read from its file alone, as a command run
outside Emacs would; a file that is no copy came from nowhere."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let ((copy (buffer-file-name (org-dock-test--dock))))
      (should (equal (org-dock-copy-origin copy)
                     (list :file file :original org-dock-test--topic)))
      (should-not (org-dock-copy-origin file)))))

(ert-deftest org-dock-test-copy-file-name ()
  "A copy is named by the time and its heading, with nothing in the name a
shell or a prompt would take apart."
  (let ((org-dock-directory temporary-file-directory))
    (should (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-承認は-部長が-先-a-b\\.org\\'"
                            (file-name-nondirectory
                             (org-dock--new-copy-file "承認は 部長が/先: a&b"))))))


(ert-deftest org-dock-test-busy-keeps-the-copy ()
  "While something else writes a copy, it is read-only, its header line
says who, and it is neither written back, let go, nor handed to an agent;
once it is done, the buffer takes up what was written."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let ((org-dock-agent-function #'ignore)
          (org-dock--busy nil))
      (with-current-buffer (org-dock-test--dock)
        (org-dock-set-busy buffer-file-name "Agent")
        (should buffer-read-only)
        (should (string-match-p "\\`Agent is writing" (org-dock-test--header-line)))
        (should-error (org-dock-finish) :type 'user-error)
        (should-error (org-dock-abort) :type 'user-error)
        (should-error (org-dock-ask-agent) :type 'user-error)
        ;; The agent writes the file, and is done.
        (with-temp-file buffer-file-name
          (insert org-dock-test--topic "written\n"))
        (set-file-times buffer-file-name (time-add nil 2))
        (org-dock-set-busy buffer-file-name nil)
        (should-not buffer-read-only)
        (should (string-suffix-p "written\n" (buffer-string)))
        (org-dock-finish))
      (should (string-match-p "under\nwritten\n\\* After" (buffer-string))))))

(ert-deftest org-dock-test-header-line-says-the-keys ()
  "The header line gives the keys of a copy, an agent's when there is one."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let ((org-dock-agent-function nil)
          (org-dock-background-agent-function nil))
      (with-current-buffer (org-dock-test--dock)
        (let ((line (org-dock-test--header-line)))
          (should (string-match-p "C-c C-c write back" line))
          (should (string-match-p "C-c C-k let go" line))
          (should (string-match-p "C-c C-= changes" line))
          (should-not (string-match-p "agent\\|background" line)))
        (let* ((org-dock-agent-function #'ignore)
               (org-dock-background-agent-function #'ignore)
               (line (org-dock-test--header-line)))
          (should (string-match-p "C-c C-; ask an agent" line))
          (should (string-match-p "C-c C-' ask in the background" line)))))))

(ert-deftest org-dock-test-ask-agent-background ()
  "The background agent is handed what the agent is, and a copy with no
such function says so."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* (got
           (org-dock-background-agent-function (lambda (request) (setq got request))))
      (with-current-buffer (org-dock-test--dock)
        (org-dock-ask-agent-background)
        (should (equal (plist-get got :copy) buffer-file-name))
        (should (file-exists-p (plist-get got :request)))
        (let ((org-dock-background-agent-function nil))
          (should-error (org-dock-ask-agent-background) :type 'user-error))))))

(ert-deftest org-dock-test-files-beside-go-with-the-copy ()
  "Written back, a copy takes with it every file named by it, and no other."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (with-current-buffer (org-dock-test--dock)
      (let* ((stem (file-name-sans-extension buffer-file-name))
             (beside (concat stem ".result.json"))
             (other (concat stem "-2.org")))
        (with-temp-file beside (insert "{}"))
        (with-temp-file other (insert "* Other\n"))
        (org-dock-finish)
        (should-not (file-exists-p beside))
        (should-not (file-exists-p (concat stem ".org")))
        (should-not (file-exists-p (concat stem ".eld")))
        (should (file-exists-p other))))))

(ert-deftest org-dock-test-list-the-copies ()
  "The list has a line for each copy, open or not, saying what it is in,
with what the status functions add; a copy not open is taken out again
from there, gone on with without asking."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* ((org-dock-status-functions (list (lambda (_) "done")))
           (copy (org-dock-test--dock))
           (copy-file (buffer-file-name copy)))
      (with-current-buffer copy
        (goto-char (point-max))
        (insert "kept\n")
        (save-buffer))
      (let ((entry (assoc copy-file (org-dock--list-entries))))
        (should (equal (car entry) copy-file))
        (should (equal (aref (cadr entry) 1) "Topic"))
        (should (equal (aref (cadr entry) 2) "notes.org"))
        (should (equal (aref (cadr entry) 3) "changed"))
        (should (equal (aref (cadr entry) 4) "done")))
      ;; Emacs stops: the copy's buffer goes, and the heading's keeping.
      (kill-buffer copy)
      (let ((inhibit-read-only t))
        (with-silent-modifications
          (set-text-properties (point-min) (point-max) nil)))
      (should (equal (aref (cadr (assoc copy-file (org-dock--list-entries))) 3)
                     "changed, not open"))
      (with-temp-buffer
        (org-dock-list-mode)
        (setq tabulated-list-entries (org-dock--list-entries))
        (tabulated-list-print)
        (goto-char (point-min))
        (while (not (equal (tabulated-list-get-id) copy-file)) (forward-line 1))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
          (save-window-excursion (org-dock-list-open))))
      (let ((again (find-buffer-visiting copy-file)))
        (should again)
        (with-current-buffer again (org-dock-finish)))
      (should (string-match-p "under\nkept\n\\* After" (buffer-string))))))

(ert-deftest org-dock-test-list-a-copy-with-nowhere-to-go ()
  "A copy whose heading has changed in its file since is not taken out
again from the list; it is thrown away when that is agreed to, and kept
when not."
  (org-dock-test--with-file org-dock-test--text
    (org-dock-test--goto "Topic")
    (let* ((copy (org-dock-test--dock))
           (copy-file (buffer-file-name copy))
           asked)
      (with-current-buffer copy
        (goto-char (point-max))
        (insert "kept\n")
        (save-buffer))
      ;; Emacs stops, and the heading is changed in its file after.
      (kill-buffer copy)
      (let ((inhibit-read-only t))
        (with-silent-modifications
          (set-text-properties (point-min) (point-max) nil))
        (org-dock-test--goto "Topic")
        (insert "* Renamed\n")
        (delete-line))
      (with-temp-buffer
        (org-dock-list-mode)
        (setq tabulated-list-entries (org-dock--list-entries))
        (tabulated-list-print)
        (goto-char (point-min))
        (while (not (equal (tabulated-list-get-id) copy-file)) (forward-line 1))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) nil)))
          (save-window-excursion (org-dock-list-open)))
        (should (string-match-p "the changed copy" asked))
        (should (file-exists-p copy-file))
        (should-not (find-buffer-visiting copy-file))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (save-window-excursion (org-dock-list-open)))
        (should-not (file-exists-p copy-file))
        (should-not (file-exists-p (concat (file-name-sans-extension copy-file) ".eld")))
        (should-not (assoc copy-file tabulated-list-entries))))))

(provide 'org-dock-test)
;;; org-dock-test.el ends here
