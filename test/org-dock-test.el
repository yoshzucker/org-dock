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
              (should (string-match-p "Out of" (format "%s" header-line-format)))
              (goto-char (point-max))
              (insert "added\n")
              (org-dock-finish)))
          (should (string-match-p "under\nadded\n\\* After" (buffer-string))))
      (delete-directory org-dock-directory t))))

(ert-deftest org-dock-test-copy-file-name ()
  "A copy is named by the time and its heading, with nothing in the name a
shell or a prompt would take apart."
  (let ((org-dock-directory temporary-file-directory))
    (should (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-承認は-部長が-先-a-b\\.org\\'"
                            (file-name-nondirectory
                             (org-dock--new-copy-file "承認は 部長が/先: a&b"))))))

(provide 'org-dock-test)
;;; org-dock-test.el ends here
