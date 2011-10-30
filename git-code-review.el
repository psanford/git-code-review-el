(require 'magit)

(defun git-code-review-get-value (name)
  (with-temp-buffer
    (let ((ret (process-file "git" nil t nil "config" (concat "codereview." name))))
      (if (= ret 0)
          (progn
            (goto-char (point-min))
            (buffer-substring (line-beginning-position) (line-end-position)))
        nil))))

(defun git-code-review-get-all-values (name)
  (ignore-errors
    (process-lines "git" "config" "--get-all" (concat "codereview." name))))

(defun git-code-review-set-value (name value)
  (process-file "git" nil nil nil "config" (concat "codereview." name) value))

(defun git-code-review-add-value (name value)
  (process-file "git" nil nil nil "config" "--add" (concat "codereview." name) value))

(defun git-code-review-clear-all-values (name)
  (process-file "git" nil nil nil "config" "--unset-all" (concat "codereview." name)))

(defun git-code-review-filter-commits ()
  "Hide commits that have already been reviewed"
  (save-excursion
    (goto-char (point-min))
    (let ((reviewed-commits (git-code-review-get-all-values "skip")))
      (while (not (eobp))
        (beginning-of-line)
        (forward-word)
        (if (member (current-word) reviewed-commits)
            (delete-region (line-beginning-position) (+ 1 (line-end-position)))
          (forward-line 1))))))

(defun git-code-review ()
  "Run code review for current git repository"
  (interactive)
  (switch-to-buffer (get-buffer-create "*code-review*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq default-directory (locate-dominating-file default-directory ".git"))
  (let ((since (git-code-review-get-value "since")))
    (insert
     (shell-command-to-string
      (concat "git log  --graph --pretty=oneline --decorate=full "
              (if since
                  (concat since "..")
                "")
              "origin/master"))))
  (git-code-review-filter-commits)
  (beginning-of-buffer)
  (magit-wash-log)
  (if (and (bobp) (eobp))
      (insert "Nothing to review!"))
  (setq buffer-read-only t)
  (git-code-review-mode))

(defun git-code-review-current-commit ()
  "Returns commit sha for current line"
  (save-excursion
    (beginning-of-line)
    (magit-rev-parse (current-word))))

(defun git-code-review-view-diff ()
  "View diff for revision at point"
  (interactive)
  (magit-show-commit (git-code-review-current-commit)))

(defun git-code-review-current-commit-region ()
  "Returns a list of point min and point max for current commit"
  (list
   (line-beginning-position)
   (save-excursion
     (forward-line 1)
     (line-beginning-position))))

(defvar git-code-review-highlight-overlay nil)
(defun git-code-review-highlight-line ()
  "Highlight current line"
  (if (not git-code-review-highlight-overlay)
      (let ((ov (make-overlay 1 1)))
        (overlay-put ov 'face 'magit-item-highlight)
        (setq git-code-review-highlight-overlay ov)))
  (apply 'move-overlay git-code-review-highlight-overlay
         (git-code-review-current-commit-region))0)

(defun git-code-review-post-command-hook ()
  (git-code-review-highlight-line))

;;XXX Make this get the url based on remotes
(defun git-code-review-view-in-github ()
  "Open commit in github"
  (interactive)
  (browse-url (concat
               "https://github.com/nearbuy/storenet/commit/"
               (git-code-review-current-commit))))

(defun git-code-review-mark-all-as-reviewed ()
  "Mark all unviewed commits as reviewed"
  (interactive)
  (when (y-or-n-p "Are you sure you want to mark all as reviewed? ")
    (git-code-review-set-value "since" (magit-rev-parse "origin/master"))
    (git-code-review-clear-all-values "skip")
    (git-code-review)))

(defun git-code-review-mark-commit-as-reviewed ()
  "Mark current commit as reviewed"
  (interactive)
  (let ((commit (git-code-review-current-commit)))
    (when (y-or-n-p
           (format "Are you sure you want to mark %s as reviewed? " commit))
      (git-code-review-add-value "skip" commit)
      (git-code-review))))

(defvar git-code-review-marked-commits '())
(make-variable-buffer-local 'git-code-review-marked-commits)

(defun git-code-review-unmark-commit (line-move-number)
  "Unmark current commit"
  (interactive "p")
  (let ((commit (git-code-review-current-commit)))
    (setq git-code-review-marked-commits
          (remove commit git-code-review-marked-commits))
    (apply 'remove-overlays
           (append
            (git-code-review-current-commit-region)
            '(git-code-review-type mark))))
  (forward-line (or line-move-number 1)))

(defun git-code-review-mark-commit (line-move-number)
  "Mark current commit"
  (interactive "p")
  (let ((commit (git-code-review-current-commit))
        (overlay))
      (add-to-list 'git-code-review-marked-commits commit)
      (setq overlay (apply 'make-overlay (git-code-review-current-commit-region)))
      (overlay-put overlay 'face 'magit-item-mark)
      (overlay-put overlay 'git-code-review-type 'mark))
  (forward-line (or line-move-number 1)))

(defvar git-code-review-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'git-code-review-view-diff)
    (define-key map (kbd "g") 'git-code-review-view-in-github)
    (define-key map (kbd "r") 'git-code-review-mark-commit-as-reviewed)
    (define-key map (kbd "m") 'git-code-review-mark-commit)
    (define-key map (kbd "M") '(lambda ()
                                 (interactive)
                                 (git-code-review-mark-commit -1)))
    (define-key map (kbd "u") 'git-code-review-unmark-commit)
    (define-key map (kbd "U") '(lambda ()
                                 (interactive)
                                 (git-code-review-unmark-commit -1)))
    map))

(define-derived-mode git-code-review-mode fundamental-mode "Git Code Review"
  "Mode for reviewing commits to a git repository
\\{git-code-review-mode-map}"

  (add-hook 'post-command-hook #'git-code-review-post-command-hook t t))

(provide 'git-code-review)
