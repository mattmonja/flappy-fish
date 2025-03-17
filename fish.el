;; fish.el --- Show a flappy fish in the mode line -*- lexical-binding: t -*-

;; Author: Matthew Monjarrez
;; Version: 1.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: games, fun
;; URL: https://github.com/mattmonja/flappy-fish

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package shows a flapping fish in your mode-line that moves
;; according to your buffer position. The right-pointing fish is only
;; displayed in the active buffer.
;;
;; Usage:
;;   1. Put this file in your load-path
;;   2. Add (require 'fish) to your init file
;;   3. Set up the fish in your mode-line:
;;
;;   (require 'fish)
;;   (fish-start-timer)  ;; or (fish-start-timer 'swimming)
;;   (setq global-mode-string '("" fish-mode-line-string))
;;
;;   The fish supports two display styles:
;;   - 'static: Shows a static fish that flaps in place (default)
;;   - 'swimming: Shows a fish that swims across the mode line based on buffer position

;;; Code:

(defgroup fish nil
  "Scrolling Flappy Fish in the mode line."
  :group 'convenience)

(defcustom fish-update-period 0.2
  "Time in seconds between fish flaps."
  :type 'float
  :group 'fish)

(defcustom fish-bar-length 32
  "Width of the 'water' in the mode line."
  :type 'integer
  :group 'fish)

(defcustom fish-wave-character "~"
  "Character used for the background."
  :type 'string
  :group 'fish)

(defcustom fish-prefix " "
  "Text before the fish."
  :type 'string
  :group 'fish)

(defcustom fish-suffix ""
  "Text after the fish."
  :type 'string
  :group 'fish)

(defvar fish-frames ["𓆝" "𓆟" "𓆞"]
  "Fish animation frames.")

(defvar fish-index 0
  "Current frame number.")
(make-variable-buffer-local 'fish-index)

(defface fish-face
  '((t :family "Noto Sans Egyptian Hieroglyphs"))
  "Face for the fish."
  :group 'fish)

(defvar fish--timer nil
  "Animation timer.")

(defvar fish-mode-line-string ""
  "Mode-line string for fish display.")
(make-variable-buffer-local 'fish-mode-line-string)
(put 'fish-mode-line-string 'risky-local-variable t)

(defvar fish--current-display-type 'static
  "Display type: 'static or 'swimming.")

(defun fish--next-frame ()
  "Move to next fish frame."
  (let ((active-buf (window-buffer (selected-window))))
    (when (buffer-live-p active-buf)
      (with-current-buffer active-buf
        (setq fish-index (mod (1+ fish-index) (length fish-frames)))
        (fish--update-display))))
  (force-mode-line-update nil))

(defun fish--get-position ()
  "Get position as fraction between 0 and 1."
  (let ((max (point-max))
        (min (point-min)))
    (if (<= max min)
        0.0
      (/ (float (- (point) min)) (float (- max min))))))

(defun fish--go-to-position (fraction buf)
  "Go to FRACTION of BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (+ (point-min)
                    (floor (* fraction (- (point-max) (point-min)))))))))

(defun fish--make-clickable (string fraction buffer)
  "Make STRING clickable to move to FRACTION in BUFFER."
  (propertize string
              'mouse-face 'highlight
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             (lambda ()
                               (interactive)
                               (fish--go-to-position fraction buffer)))
                           map)))

(defun fish--swimming-display ()
  "Create swimming fish display."
  (let* ((progress (fish--get-position))
         (fish-pos (min (floor (* progress fish-bar-length))
                        (1- fish-bar-length)))
         (wave "")
         (buf (current-buffer)))
    (dotimes (i fish-bar-length)
      (if (= i fish-pos)
          (setq wave (concat wave
                             (fish--make-clickable
                              (propertize (aref fish-frames fish-index)
                                          'face 'fish-face)
                              (/ (float i) fish-bar-length)
                              buf)))
        (setq wave (concat wave
                           (fish--make-clickable fish-wave-character
                                                 (/ (float i) fish-bar-length)
                                                 buf)))))
    wave))

(defun fish--static-display ()
  "Create static fish display."
  (propertize (concat " " (aref fish-frames fish-index)) 'face 'fish-face))

(defun fish--update-display ()
  "Update fish display."
  (setq fish-mode-line-string
        (concat fish-prefix
                (if (eq fish--current-display-type 'swimming)
                    (if (eq (current-buffer) (window-buffer (selected-window)))
                        (fish--swimming-display)
                      "")
                  (if (eq (current-buffer) (window-buffer (selected-window)))
                      (fish--static-display)
                    ""))
                fish-suffix))
  fish-mode-line-string)

(defun fish--update-on-buffer-change (_)
  "Update fish when buffer changes."
  (when (buffer-live-p (window-buffer (selected-window)))
    (with-current-buffer (window-buffer (selected-window))
      (fish--update-display))))

(defun fish-start-timer (&optional display-type)
  "Start fish animation with DISPLAY-TYPE ('static or 'swimming)."
  (interactive)
  (when fish--timer
    (cancel-timer fish--timer))
  
  (setq fish--current-display-type (or display-type 'static))
  (setq fish--timer (run-at-time nil fish-update-period 'fish--next-frame))
  
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (fish--update-display)))
  
  (add-hook 'window-buffer-change-functions #'fish--update-on-buffer-change)
  (fish--update-display))

(defun fish-stop ()
  "Stop the fish animation."
  (interactive)
  (when fish--timer
    (cancel-timer fish--timer)
    (setq fish--timer nil))
  
  (remove-hook 'window-buffer-change-functions #'fish--update-on-buffer-change)
  
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq fish-mode-line-string "")))
  
  (force-mode-line-update t))

(provide 'fish)
;;; fish.el ends here
