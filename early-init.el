(defvar enable-debug-p nil "non-nil to enable debug.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))


(setq load-prefer-newer noninteractive
	  read-process-output-max (* 512 1024))  ; 512kb

(setq package-enable-at-startup nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil
	  ad-redefinition-action 'accept
	  warning-suppress-types '((lexical-binding))
	  ffap-machine-p-known 'reject
	  idle-update-delay 1.0
	  frame-inhibit-implied-resize t
	  auto-mode-case-fold nil
	  inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
	  initial-buffer-choice nil
      inhibit-startup-buffer-menu t)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)


(setq debug-on-error enable-debug-p
      jka-compr-verbose enable-debug-p)

(setq native-comp-speed -1
	  no-native-compile t
	  native-comp-async-report-warnings-errors 'silent
	  native-comp-warning-on-missing-source enable-debug-p
	  byte-compile-warnings enable-debug-p
	  byte-compile-verbose enable-debug-p)

(setq inhibit-splash-screen t)
(set-scroll-bar-mode nil)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq horizontal-scroll-bar-mode nil)
(setq blink-cursor-mode nil)
(tooltip-mode -1)

(setq use-file-dialog nil
	  use-dialog-box nil)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
(defalias #'view-hello-file #'ignore)  ; never show the hello file
