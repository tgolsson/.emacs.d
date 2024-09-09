(defun random-uuid ()
  "Insert a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5"
  (interactive)
  (let ((my-str (md5 (format "%s%s%s%s%s%s%s%s%s%s" (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))
    (insert (format "%s-%s-4%s-%s%s-%s" (substring my-str 0 8)
                    (substring my-str 8 12)
                    (substring my-str 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring my-str 17 20)
                    (substring my-str 20 32)))))


;; Resize the whole frame, and not only a window
;; Adapted from https://stackoverflow.com/a/24714383/5103881
(defun acg/zoom-frame (&optional amt frame)
  "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (face-attribute 'default :font frame))
         (size (font-get font :size))
         (amt (or amt 1))
         (new-size (+ size amt)))
    (set-frame-font (font-spec :size new-size) t `(,frame))
    (message "Frame's font new size: %d" new-size)))

(defun acg/zoom-frame-out (&optional amt frame)
  "Call `acg/zoom-frame' with negative argument."
  (interactive "p")
  (acg/zoom-frame (- (or amt 1)) frame))


(defun rename-current-buffer-file ()
  "Renames current buffer and file it is
visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename
                  (file-exists-p filename)))
        (error
         "Buffer '%s' is not visiting a file!"
         name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error
             "A buffer named '%s' already exists!"
             new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory
                                                                  new-name)))))))


(defun copy-selected-text (start end)
  (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
          (shell-command (concat "echo '" text "' | clip.exe")))))

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))


(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond ((point-is-in-string-p)
           (move-point-forward-out-of-string))
          ((looking-at "(\\|{\\|\\[")
           (forward-list))
          (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond ((point-is-in-string-p)
           (move-point-backward-out-of-string))
          ((looking-back ")\\|}\\|\\]")
           (backward-list))
          (t (backward-char)))))

(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or
{p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
						((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion (goto-char end-of-first)
                                         (move-backward-out-of-param)
                                         (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion (goto-char start-of-last)
                                      (move-forward-out-of-param)
                                      (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun untabify-buffer ()
  (interactive)
  (untabify 1 (point-max))
  (if (not (eq major-mode 'mew-draft-mode))
      ;; delete-trailing-whitespace does not work in mew-draft-mode.
      (delete-trailing-whitespace)))

(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))


(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore
notation for the symbol at point."
  (interactive)
  (save-excursion (let* ((bounds (bounds-of-thing-at-point 'symbol))
                         (start (car bounds))
                         (end (cdr bounds))
                         (currently-using-underscores-p (progn (goto-char start)
                                                               (re-search-forward "_" end t))))
                    (if currently-using-underscores-p (progn (upcase-initials-region start end)
                                                             (replace-string "_" "" nil start end)
                                                             (downcase-region start (1+ start)))
                      (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
                      (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))
(use-package abbrev-mode
  :straight (:type built-in)
  :hook (lua-mode lisp-mode))
