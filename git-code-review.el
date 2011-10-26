(require 'magit)

(defun git-code-review-get-value (name)
  (with-temp-buffer
    (let ((ret (process-file "git" nil t nil "config" (concat "codereview." name))))
      (if (= ret 0)
          (progn
            (goto-char (point-min))
            (buffer-substring (line-beginning-position) (line-end-position)))
        nil))))

(defun git-code-review-set-value (name value)
  (process-file "git" nil nil nil "config" (concat "codereview." name) value))

(defun git-code-review ()
  "Run code review for current git repository"
  (interactive)
  (switch-to-buffer (get-buffer-create "*code-review*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((since (git-code-review-get-value "since")))
    (message "<%s>"
             (concat "git log  --graph --pretty=oneline --decorate=full "
                     (if since
                         (concat since "..")
                       "")
                     "origin/master"))

    (insert
     (shell-command-to-string
      (concat "git log  --graph --pretty=oneline --decorate=full "
              (if since
                  (concat since "..")
                "")
              "origin/master"))))
  (beginning-of-buffer)
  (magit-wash-log)
  (setq buffer-read-only t)
  (git-code-review-mode))

(defun git-code-review-current-commit ()
  "Returns commit sha for current line"
  (save-excursion
    (beginning-of-line)
    (current-word)))

(defun git-code-review-view-diff ()
  "View diff for revision at point"
  (interactive)
  (magit-show-commit (git-code-review-current-commit)))

(defun git-code-review-view-in-github ()
  "Open commit in github"
  (interactive)
  (browse-url (concat
               "https://github.com/nearbuy/storenet/commit/"
               (git-code-review-current-commit))))

(defun git-code-review-mark-all-as-reviewed ()
  "Mark all unviewed commits as reviewed"
  (interactive)
  (when (y-or-n-p "Are you sure you want to mark all as reviewed?")
    (git-code-review-set-value "since" (magit-rev-parse "origin/master"))))

(defvar git-code-review-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'git-code-review-view-diff)
    (define-key map (kbd "g") 'git-code-review-view-in-github)
    map))

(define-derived-mode git-code-review-mode fundamental-mode "Git Code Review"
  "Mode for reviewing commits to a git repository
\\{git-code-review-mode-map}")

(provide 'git-code-review)
