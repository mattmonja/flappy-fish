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

(defcustom fish-wave-character " "
  "Character used for the background of the fish swimming area."
  :type 'string
  :group 'fish)

(defcustom fish-prefix " "
  "Text to display before the fish in the mode-line."
  :type 'string
  :group 'fish)

(defcustom fish-suffix ""
  "Text to display after the fish in the mode-line."
  :type 'string
  :group 'fish)

(defvar fish-frames ["𓆝" "𓆟" "𓆞"]
  "Fish frames for the flapping animation.")

(defvar fish-index 0
  "Which frame in `fish-frames' is currently displayed.")
(make-variable-buffer-local 'fish-index)

(defface fish-face
  '((t :family "Noto Sans Egyptian Hieroglyphs"))
  "Face for the flappy fish glyph in the mode line."
  :group 'fish)

(defvar fish--timer nil
  "Animation timer for Flappy Fish scrolling mode.")

(defvar fish-mode-line-string ""
  "Buffered mode-line string containing the fish display.")
(make-variable-buffer-local 'fish-mode-line-string)
(put 'fish-mode-line-string 'risky-local-variable t)

(defvar fish--current-display-type 'static
  "Current display type: 'static or 'swimming.")

(defun fish--advance-frame ()
  "Move to the next fish frame and update mode lines."
  (let ((active-buf (window-buffer (selected-window))))
    (when (buffer-live-p active-buf)
      (with-current-buffer active-buf
        (setq fish-index (mod (1+ fish-index)
                              (length fish-frames)))
        (fish--update-mode-line-string))))
  (force-mode-line-update nil))

(defun fish--fraction ()
  "Return point's fraction (0..1) between buffer limits."
  (let ((max (point-max))
        (min (point-min)))
    (if (<= max min)
        0.0
      (/ (float (- (point) min)) (float (- max min))))))

(defun fish--scroll-buffer (fraction buf)
  "Scroll BUF to FRACTION (0..1) of its length."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (+ (point-min)
                    (floor (* fraction (- (point-max) (point-min)))))))))

(defun fish--add-click (string fraction buffer)
  "Propertize STRING so clicking moves BUFFER to FRACTION."
  (propertize string
              'mouse-face 'highlight
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             (lambda ()
                               (interactive)
                               (fish--scroll-buffer fraction buffer)))
                           map)))

(defun fish--position-line ()
  "Build a line with the fish at the correct position."
  (let* ((progress (fish--fraction))
         (fish-pos (min (floor (* progress fish-bar-length))
                        (1- fish-bar-length)))
         (wave "")
         (buf (current-buffer)))
    (dotimes (i fish-bar-length)
      (if (= i fish-pos)
          (setq wave (concat wave
                             (fish--add-click
                              (propertize (aref fish-frames fish-index)
                                          'face 'fish-face)
                              (/ (float i) fish-bar-length)
                              buf)))
        (setq wave (concat wave
                           (fish--add-click fish-wave-character
                                            (/ (float i) fish-bar-length)
                                            buf)))))
    wave))

(defun fish--create-swimming ()
  "Return the mode-line string for the swimming fish."
  (if (eq (current-buffer) (window-buffer (selected-window)))
      (fish--position-line)
    ""))

(defun fish--create-static ()
  "Return the mode-line string for a static flapping fish."
  (if (eq (current-buffer) (window-buffer (selected-window)))
      (propertize (concat " " (aref fish-frames fish-index)) 'face 'fish-face)
    ""))

(defun fish--update-mode-line-string ()
  "Update `fish-mode-line-string' with current fish display."
  (setq fish-mode-line-string
        (propertize 
         (concat fish-prefix
                 (if (eq fish--current-display-type 'swimming)
                     (fish--create-swimming)
                   (fish--create-static))
                 fish-suffix)
         'help-echo "Flappy fish! Click to navigate."))
  fish-mode-line-string)

(defun fish--update-on-buffer-change (_)
  "Update fish display when buffer changes."
  (when (buffer-live-p (window-buffer (selected-window)))
    (with-current-buffer (window-buffer (selected-window))
      (fish--update-mode-line-string))))

;;;###autoload
(defun fish-start-timer (&optional display-type)
  "Start the fish animation timer with DISPLAY-TYPE ('static or 'swimming)."
  (interactive)
  (when fish--timer
    (cancel-timer fish--timer))
  
  (setq fish--current-display-type (or display-type 'static))
  (setq fish--timer (run-at-time nil fish-update-period 'fish--advance-frame))
  ;; Initialize fish-mode-line-string for all buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (fish--update-mode-line-string)))
  ;; Add hook to update fish when buffer changes
  (add-hook 'window-buffer-change-functions #'fish--update-on-buffer-change)
  (fish--update-mode-line-string))

;;;###autoload
(defun fish-stop ()
  "Stop the fish animation and clear the mode-line string."
  (interactive)
  (when fish--timer
    (cancel-timer fish--timer)
    (setq fish--timer nil))
  ;; Remove the buffer change hook
  (remove-hook 'window-buffer-change-functions #'fish--update-on-buffer-change)
  ;; Clear fish-mode-line-string in all buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq fish-mode-line-string "")))
  (force-mode-line-update t))

(provide 'fish)
;;; fish.el ends here
