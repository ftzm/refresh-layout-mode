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


;; Override horizontally-n for compatability with olivetti mode

(defun total-window-width ()
  "Get the total width of the current window, including margins."
  (apply '+ (list
	     (window-width)
	     (or (car (window-margins)) 0)
	     (or (cdr (window-margins)) 0))))

(defun rotate:horizontally-n (num)
  (if (<= num 2)
      (split-window-horizontally)
    (split-window-horizontally
     (- (total-window-width) (/ (total-window-width) num)))
    (rotate:horizontally-n (- num 1))))

;; end override

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

(defvar refresh-layout-disablers (list)
   "Holds the disabled state for temporary suspension.")

(defvar refresh-layout-in-process nil
   "Holds in-progress state to prevent recursion.")

(defvar refresh-layout-window-count (length (window-list))
  "Keeps track of window count.
Doing so allows us to limit refresh actions to window creation or
destruction events.")

(defun disable-refresh-layout (tag)
  "Hook function to disable refresh-layout.  ARGS discarded."
  (push tag refresh-layout-disablers))

(defun enable-refresh-layout (tag)
  "Hook function to (re-)enable refresh-layout.  ARGS discarded."
  (setq refresh-layout-disablers (delete* tag refresh-layout-disablers :count 1)))

(defun disable-refresh-layout-around (f &rest args)
  (if refresh-layout-disabled
      (apply f args)
    (progn
      (setq refresh-layout-disabled t)
      (apply f args)
      (setq refresh-layout-disabled nil))))

(defun refresh-layout ()
  "Refresh the window layout if appropriate."
  (let ((current-window-count (length (window-list))))
    (when
      (and
       (null refresh-layout-disablers)
        (not (= refresh-layout-window-count current-window-count))
	    (not refresh-layout-in-process)
	    (not (active-minibuffer-window))
        (not (member major-mode (list 'mu4e-view-mode 'mu4e-headers-mode))))
  	;(not (member nil (mapcar (lambda (w) (if (window-live-p w) (buffer-file-name (window-buffer w)) (message "guard") nil)) (window-list)))))
      (setq refresh-layout-window-count current-window-count)
      (let ((inhibit-message t))
	(setq refresh-layout-in-process t)
        (rotate:refresh-window rotate-current-layout)
	(setq refresh-layout-in-process nil)))))

(cl-defun add-refresh-advice (before &key (after nil))
  (advice-add before :before (lambda (&rest args) (disable-refresh-layout before)))
  (advice-add (or after before) :after (lambda (&rest args) (enable-refresh-layout before))))

(cl-defun remove-refresh-advice (before &key (after nil))
  (advice-remove before :before (lambda (&rest args) (disable-refresh-layout before)))
  (unless before-only (advice-remove (or after before) :after (lambda (&rest args) (enable-refresh-layout before)))))

(defun refresh-layout-setup ()
  "Set up hooks and advice."

  ;; Avoid conflict with hydra
  (add-refresh-advice 'lv-message :after 'lv-delete-window)

  ;; Avoid conflict with which-key
  (add-refresh-advice 'which-key--show-popup :after 'which-key--hide-popup)

  ;; Avoid conflict with persp-mode
  (add-refresh-advice 'persp-switch)

  ;; Avoid conflict with process windows
  (add-refresh-advice 'window--adjust-process-windows)

  ;; Avoid conflict with transient
  (advice-add 'transient-setup :before (lambda (&rest args) (disable-refresh-layout 'transient)))
  (add-hook 'post-transient-hook (lambda (&rest args) (enable-refresh-layout 'transient)))

  (add-hook 'window-configuration-change-hook 'refresh-layout))

(defun refresh-layout-teardown ()
  "Remove hooks and advice."

  ;; Avoid conflict with hydra
  (remove-refresh-advice 'lv-message :after 'lv-delete-window)

  ;; Avoid conflict with which-key
  (remove-refresh-advice 'which-key--show-popup :after 'which-key--hide-popup)

  ;; Avoid conflict with persp-mode
  (remove-refresh-advice 'persp-switch)

  ;; Avoid conflict with process windows
  (remove-refresh-advice 'window--adjust-process-windows)

  ;; Avoid conflict with transient (used by magit among others)
  (advice-remove'transient-setup :before (lambda (&rest args) (disable-refresh-layout 'transient)))
  (remove-hook 'post-transient-hook (lambda (&rest args) (enable-refresh-layout 'transient)))

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
