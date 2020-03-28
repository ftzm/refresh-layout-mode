;;; refresh-layout.el --- Automatically arrange windows -*- lexical-binding: t -*-

;; Copyright (C) 2020 Matthew Fitzsimmons
;;
;; Author: Matthew Fitzsimmons <m@ftzm.org>
;; Version: 0.0.1
;; URL: https://github.com/ftzm/refresh-layout-mode
;; Keywords: window, layout
;; Package-Requires: ((emacs "25.1") (rotate "0.1.0"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:

;; refresh-layout.el provides a minor mode which automatically refreshes
;; the window layout when windows are created or destroyed.  It extends rotate.el.

;;;; Code:

(require 'cl-lib)
(require 'rotate)

(defun rotate:3column-right-bias (num)
  "Three column layout with vertical splits biased rightward.
Reverts to plain tiling when NUM > 9."

  (cond ((< num 4) (rotate:horizontally-n num))
  ((> num 9) (rotate:tiled-n num))
  ((> num 3)
	   (rotate:horizontally-n 3)
	   (cl-decf num 3)
	   (other-window -1)
	   (while (> num 0)
	     (let ((i (min 2 num)))
	       (rotate:vertically-n (+ 1 i))
	       (cl-decf num i))
	     (other-window -1)))))

(setq rotate-current-layout #'rotate:3column-right-bias)

(defvar refresh-layout-disabled nil
   "Holds the disabled state for temporary suspension.")

(defvar refresh-layout-in-process nil
   "Holds in-progress state to prevent recursion.")

(defvar refresh-layout-window-count (length (window-list))
  "Keeps track of window count.
Doing so allows us to limit refresh actions to window creation or
destruction events.")

(defun disable-refresh-layout (&rest args)
  "Hook function to disable refresh-layout.  ARGS discarded."
  (setq refresh-layout-disabled t))

(defun enable-refresh-layout ()
  "Hook function to (re-)enable refresh-layout."
  (setq refresh-layout-disabled nil))

(defun refresh-layout ()
  "Refresh the window layout if appropriate."
  (let ((current-window-count (length (window-list))))
    (when
      (and
        (not refresh-layout-disabled)
        (not (= refresh-layout-window-count current-window-count))
	(not refresh-layout-in-process)
	(not (active-minibuffer-window)))
  	;(not (member nil (mapcar (lambda (w) (if (window-live-p w) (buffer-file-name (window-buffer w)) (message "guard") nil)) (window-list)))))
      (setq refresh-layout-window-count current-window-count)
      (let ((inhibit-message t))
	(setq refresh-layout-in-process t)
        (rotate:refresh-window rotate-current-layout)
	(setq refresh-layout-in-process nil)))))


(defun refresh-layout-setup ()
  "Set up hooks and advice."
  ;; Avoid conflict with hydra
  (advice-add 'lv-message :before 'disable-refresh-layout)
  (advice-add 'lv-delete-window :after 'enable-refresh-layout)

  ;; Avoid conflict with which-key
  (advice-add 'which-key--show-popup :before 'disable-refresh-layout)
  (advice-add 'which-key--hide-popup :after 'enable-refresh-layout)

  ;; Avoid conflict with transient (used by magit among others)
  (advice-add 'transient-setup :before 'disable-refresh-layout)
  (add-hook 'post-transient-hook 'enable-refresh-layout)

  (add-hook 'window-configuration-change-hook 'refresh-layout))

(defun refresh-layout-teardown ()
  "Remove hooks and advice."
  ;; Avoid conflict with hydra
  (advice-remove 'lv-message 'disable-refresh-layout)
  (advice-remove 'lv-delete-window 'enable-refresh-layout)

  ;; Avoid conflict with which-key
  (advice-remove 'which-key--show-popup 'disable-refresh-layout)
  (advice-remove 'which-key--hide-popup 'enable-refresh-layout)

  ;; Avoid conflict with transient (used by magit among others)
  (advice-remove 'transient-setup 'disable-refresh-layout)
  (remove-hook 'post-transient-hook 'enable-refresh-layout)

  (remove-hook 'window-configuration-change-hook 'refresh-layout))

(define-minor-mode refresh-layout-mode
  :initial-value nil
  :lighter " RL"
  :global t
  (if refresh-layout-mode
      (progn
	(refresh-layout-setup)
	(refresh-layout))
    (progn
      (refresh-layout-teardown))))

(provide 'refresh-layout)
;;; refresh-layout.el ends here
