;;; anzu-overlay.el --- Inline search count overlays using anzu -*- lexical-binding: t; -*-

;; Author: You
;; Version: 0.1
;; Keywords: search, evil, anzu, convenience
;; URL: https://github.com/kwrooijen/anzu-overlay

;;; Commentary:

;; Shows inline "[current/total]" search counts at end-of-line, synchronized
;; with anzu and evil-search. This behaves similarly to Neovim's search count
;; display, inline in the buffer.

;;; Code:

(defvar anzu-overlay--inline-overlay nil
  "Overlay used for inline anzu search count display.")

;;;###autoload
(defface anzu-overlay-face
  '((t :foreground "#ffcc00" :weight bold))
  "Face for inline anzu search count overlays."
  :group 'anzu-overlay)

;;;###autoload
(defcustom anzu-overlay-format "  [%d/%d]"
  "Format string for displaying search counts.
The format should accept two integers: current match and total matches."
  :type 'string
  :group 'anzu-overlay)

(defun anzu-overlay--clear (&rest _)
  "Clear the inline anzu overlay, if present."
  (when anzu-overlay--inline-overlay
    (delete-overlay anzu-overlay--inline-overlay)
    (setq anzu-overlay--inline-overlay nil)))

(defun anzu-overlay--active-p ()
  "Return non-nil if anzu is currently displaying a search result."
  (and (bound-and-true-p anzu--state)
       anzu--total-matched))

(defun anzu-overlay--update (&rest _)
  "Display inline formatted search count using `anzu-overlay-format'."
  (anzu-overlay--clear)
  (when (anzu-overlay--active-p)
    (let* ((here  anzu--current-position)
           (total anzu--total-matched)
           (text  (propertize (format anzu-overlay-format here total)
                              'face 'anzu-overlay-face))
           (ov    (make-overlay (line-end-position)
                                (line-end-position))))

      (overlay-put ov 'after-string text)
      (setq anzu-overlay--inline-overlay ov))))

(defun anzu-overlay--post-command ()
  "Clear overlay if search is no longer active."
  (unless (anzu-overlay--active-p)
    (anzu-overlay--clear)))

(defvar anzu-overlay--advice-functions
  '(evil-search-next
    evil-search-previous
    evil-ex-search-next
    evil-ex-search-previous
    evil-search-forward
    evil-search-backward)
  "Functions that should trigger updating the anzu inline overlay.")

;;;###autoload
(define-minor-mode anzu-overlay-mode
  "Display inline anzu match counts at end of search result line."
  :global t
  :group 'anzu-overlay

  (if anzu-overlay-mode
      (progn
        (dolist (fn anzu-overlay--advice-functions)
          (advice-add fn :after #'anzu-overlay--update))
        (add-hook 'post-command-hook #'anzu-overlay--post-command))
    (progn
      (dolist (fn anzu-overlay--advice-functions)
        (advice-remove fn #'anzu-overlay--update))
      (remove-hook 'post-command-hook #'anzu-overlay--post-command)
      (anzu-overlay--clear))))

(provide 'anzu-overlay)
;;; anzu-overlay.el ends here
