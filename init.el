;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup load path and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-load-path ()
  (add-to-list 'load-path
               (format "%s/.emacs.d" (getenv "HOME")))
  (add-to-list 'load-path
               (format "%s/.emacs.d/site-lisp" (getenv "HOME"))))

(defun bc-setup-package-manager ()
  (require 'package)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-input()
  (electric-pair-mode 1)

  (require 'evil)
  (evil-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup search and navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-search-and-navigation ()
  (setq dabbrev-case-fold-search nil)
  (setq ffip-patterns
        '("*.c" "*.cc" "*.cpp" "*.cxx" "*.h" "*.hpp" "*.js" "*.html" "*.java"
          "*.m" "*.mm" "*.py" "*.sh"))

  (require 'cl)
  (require 'ace-jump-mode)

  (require 'expand-region)

  (require 'flex-isearch)
  (flex-isearch-mode 1)
  (setq flex-isearch-auto t)

  (icomplete-mode 1)

  (autoload 'idomenu "idomenu" nil t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

  (require 'semantic/ia)
  (semantic-mode 1)
  (global-ede-mode t)
  (load "init-ede-projects"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-key-bindings ()
  (global-set-key (kbd "<f5>") 'er/expand-region)
  (global-set-key (kbd "<f6>") 'ace-jump-mode)
  (global-set-key (kbd "<f7>") 'find-file-in-project)
  (global-set-key (kbd "<f8>") 'semantic-ia-fast-jump)
  (global-set-key (kbd "\C-s") 'flex-isearch-forward)
  (global-set-key (kbd "\C-r") 'flex-isearch-backward)
  (global-set-key (kbd "\M-/") 'hippie-expand)
  (global-set-key (kbd "\C-c SPC") 'semantic-complete-symbol)
  (global-set-key (kbd "\C-c m") 'execute-extended-command)
  (global-set-key (kbd "\C-c c") 'compile)
  (global-set-key (kbd "\C-c d") 'semantic-ia-fast-jump)
  (global-set-key (kbd "\C-c f") 'find-name-dired)
  (global-set-key (kbd "\C-c h") 'bc-set-current-frame-full-height)
  (global-set-key (kbd "\C-c k") 'clipboard-kill-ring-save)
  (global-set-key (kbd "\C-c g") 'rgrep)
  (global-set-key (kbd "\C-c j") 'idomenu)
  (global-set-key (kbd "\C-c p") 'find-file-in-project)
  (global-set-key (kbd "\C-c y") 'clipboard-yank)
  (global-set-key (kbd "\C-c x") 'er/expand-region)
  (global-set-key (kbd "\C-c n") 'senator-next-tag)
  (global-set-key (kbd "\C-c p") 'senator-previous-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-wrapping ()
  (setq sentence-end-double-space nil)
  (setq fill-column 80)

  (require 'fill-column-indicator)
  (fci-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-saving ()
  (setq backup-inhibited t)
  (setq auto-save-default nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup file types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-file-types()
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-appearance()
  (bc-setup-common-appearance)
  (when window-system
    (bc-setup-windowed-mode-appearance)))

(defun bc-setup-common-appearance()
  (setq inhibit-startup-screen 1)
  (menu-bar-mode -1)

  (line-number-mode 1)
  (column-number-mode 1)
  (load-theme 'solarized-dark))

(defun bc-setup-fonts ()
   (cond ((eq system-type 'darwin)
         (set-frame-font "Menlo-12" nil t))
        ((eq system-type 'gnu/linux)
         (set-frame-font "Ubuntu Mono-11" nil t))
        ((eq system-type 'windows-nt)
         (set-frame-font "Consolas-10" nil t))))

(defun bc-setup-windowed-mode-appearance ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(fullscreen . fullboth))
  (bc-setup-fonts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-eshell ()
  (setq eshell-prompt-function
      (lambda ()
        "$ "))
  (setq eshell-prompt-regexp "$ "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-ediff()
  (setq ediff-split-window-function 'split-window-horizontally))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-w3m ()
  (add-to-list 'load-path
               (format "%s/.emacs.d/site-lisp/w3m" (getenv "HOME")))
  (require 'w3m-load))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke setup functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bc-setup-load-path)
(bc-setup-package-manager)
(bc-setup-input)
(bc-setup-search-and-navigation)
(bc-setup-key-bindings)
(bc-setup-wrapping)
(bc-setup-saving)
(bc-setup-file-types)
(bc-setup-appearance)
(bc-setup-eshell)
(bc-setup-ediff)
(bc-setup-w3m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-prog-mode-hook ()
  (flex-isearch-mode 1))
(add-hook 'prog-mode-hook 'bc-prog-mode-hook)

(defun bc-c-mode-common-hook ()
  (subword-mode)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (require 'google-c-style)
  (google-set-c-style))
(add-hook 'c-mode-common-hook 'bc-c-mode-common-hook)

(defun bc-java-mode-hook ()
  (subword-mode)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'bc-java-mode-hook)

(defun bc-js-mode-hook ()
  (subword-mode)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'bc-js-mode-hook)

(defun bc-lisp-mode-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'lisp-mode-hook 'bc-lisp-mode-hook)

(defun bc-python-mode-hook ()
  (subword-mode)
  (setq indent-tabs-mode nil)
  (setq python-indent 2))

(defun bc-sh-mode-hook ()
  (subword-mode)
  (setq indent-tabs-mode nil)
  (setq sh-basic-offset 2))
(add-hook 'sh-mode-hook 'bc-sh-mode-hook)

(defun bc-align-load-hook ()
  (add-to-list 'align-rules-list
               '(js-declaration
                 (regexp . "^\s*[^\s:]+:\\(\s*\\)[^\s].*,?\s*$")
                 (group 1)
                 (modes quote
                        (js-mode)))))
(add-hook 'align-load-hook 'bc-align-load-hook)

(defun bc-strip-whitespace ()
  (untabify 0 (buffer-end 1))
  (delete-trailing-whitespace))
(add-hook 'before-save-hook 'bc-strip-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" default)))
 '(ecb-options-version "2.40")
 '(ede-project-directories (quote nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
