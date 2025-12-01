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
       anzu--total-matched
       (numberp anzu--current-position)
       (numberp anzu--total-matched)
       (< 0 (+ anzu--current-position
               anzu--total-matched))))

(defun anzu-overlay--iedit-active-p ()
  "Return non-nil if iedit has active occurrences."
  (and (fboundp 'iedit-mode)
       (bound-and-true-p iedit-mode)
       (bound-and-true-p iedit-occurrences-overlays)
       iedit-occurrences-overlays))

(defun anzu-overlay--iedit-count ()
  "Return (current . total) for iedit, or nil if unavailable."
  (when (anzu-overlay--iedit-active-p)
    (let* ((total (length iedit-occurrences-overlays))
           (this-oc
            (or (let ((inhibit-message t))
                  (iedit-find-current-occurrence-overlay))
                (save-excursion
                  (iedit-prev-occurrence)
                  (iedit-find-current-occurrence-overlay)))))
      (when this-oc
        (let* ((sorted (sort (append iedit-occurrences-overlays nil)
                             #'doom-modeline-themes--overlay-sort))
               (idx (1+ (cl-position this-oc sorted))))
          (cons idx total))))))

(defun anzu-overlay--display (current total)
  "Create an inline overlay showing CURRENT/TOTAL."
  (anzu-overlay--clear)
  (let* ((text (propertize (format anzu-overlay-format current total)
                           'face 'anzu-overlay-face))
         (ov   (make-overlay (line-end-position)
                             (line-end-position))))
    (overlay-put ov 'after-string text)
    (setq anzu-overlay--inline-overlay ov)))

(defun anzu-overlay--update (&rest _)
  "Display inline formatted search count for anzu or iedit."
  (cond
   ;; ANZU
   ((anzu-overlay--active-p)
    (anzu-overlay--display anzu--current-position
                           anzu--total-matched))

   ;; IEDIT
   ((anzu-overlay--iedit-active-p)
    (let ((ct (anzu-overlay--iedit-count)))
      (when ct
        (anzu-overlay--display (car ct) (cdr ct)))))))

(defun anzu-overlay--post-command ()
  "Clear overlay if search is no longer active."
  (unless (or (anzu-overlay--active-p)
              (anzu-overlay--iedit-active-p))
    (anzu-overlay--clear)))

(defvar anzu-overlay--advice-functions
  '(evil-search-next
    evil-search-previous
    evil-ex-search-next
    evil-ex-search-previous
    evil-search-forward
    evil-search-backward
    iedit-mode
    iedit-next-occurrence
    iedit-prev-occurrence)
  "Functions that should trigger updating the anzu inline overlay.")

;;;###autoload
(define-minor-mode anzu-overlay-mode
  "Display inline anzu match counts at end of search result line."
  :global t
  :group 'anzu-overlay

  (if anzu-overlay-mode
      (progn
        (dolist (fn anzu-overlay--advice-functions)
          (when (fboundp fn)
            (advice-add fn :after #'anzu-overlay--update)))
        (add-hook 'post-command-hook #'anzu-overlay--post-command))
    (progn
      (dolist (fn anzu-overlay--advice-functions)
        (when (fboundp fn)
          (advice-remove fn #'anzu-overlay--update)))
      (remove-hook 'post-command-hook #'anzu-overlay--post-command)
      (anzu-overlay--clear))))

(provide 'anzu-overlay)
;;; anzu-overlay.el ends here
