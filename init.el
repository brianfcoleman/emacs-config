(defun my-append-path (&rest path-components)
  (let ((path-separator "/"))
    (mapconcat (lambda (path-component)
     path-component)
         path-components
         path-separator)))

(add-to-list 'load-path
       (format "%s/.emacs.d/site-lisp" (getenv "HOME")))

(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(add-to-list 'load-path
       (format "%s/.emacs.d/site-lisp/w3m" (getenv "HOME")))
(require 'w3m-load)
(setq w3m-command "/usr/local/bin/w3m")

(defun my-set-frame-full-height (frame)
  (when window-system
    (modify-frame-parameters frame '((fullscreen . fullheight)))))

(defun my-set-current-frame-full-height ()
  (interactive)
  (my-set-frame-full-height (window-frame nil)))

(defun my-previous-window ()
  (interactive)
  (other-window -1))

(defun my-next-window ()
  (interactive)
  (other-window 1))

;; Make dabbrev-expand expansion case-sensitive
(setq dabbrev-case-fold-search nil)

(global-set-key (kbd "\M-/") 'hippie-expand)
(global-set-key (kbd "\C-c m") 'execute-extended-command)
(global-set-key (kbd "\C-c c") 'compile)
(global-set-key (kbd "\C-c f") 'find-name-dired)
(global-set-key (kbd "\C-c h") 'my-set-current-frame-full-height)
(global-set-key (kbd "\C-c k") 'clipboard-kill-ring-save)
(global-set-key (kbd "\C-c g") 'rgrep)
(global-set-key (kbd "\C-c j") 'imenu)
(global-set-key (kbd "\C-c y") 'clipboard-yank)
(global-set-key (kbd "\C-c n") 'my-next-window)
(global-set-key (kbd "\C-c p") 'my-previous-window)

(setq backup-inhibited t)
(setq auto-save-default nil)

(setq my-use-color-theme t)

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; For Unity the default width of the launcher is 60 pixels
  (cond ((eq system-type 'gnu/linux)
	 (add-to-list 'default-frame-alist '(left . 60))))
  ;; 82 - 80 columns for text and 1 column on the left and the right
  ;; for wrapped lines
  (add-to-list 'default-frame-alist '(width . 82))
  (add-to-list 'default-frame-alist '(fullscreen . fullheight))
  (if my-use-color-theme
      (load-theme 'zenburn)
    (progn (setq frame-background-mode 'dark)
	   (invert-face 'default)))
  (cond ((eq system-type 'darwin)
	 (set-frame-font "Droid Sans Mono-12" nil t))
	((eq system-type 'gnu/linux)
	 (set-frame-font "Ubuntu Mono-10" nil t))
	((eq system-type 'windows-nt)
	 (set-frame-font "Consolas-10" nil t))))

(unless window-system
  (if my-use-color-theme
      (load-theme 'zenburn)
    (setq frame-background-mode 'dark)))

(setq inhibit-startup-screen 1)
(menu-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)

;; Wrap paragraphs to 80 characters
(setq fill-column 80)
;; Sentences end with a single space
(setq sentence-end-double-space nil)

(setq eshell-prompt-function
      (lambda ()
	"$ "))
(setq eshell-prompt-regexp "$ ")

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(defun my-c-mode-common-hook ()
  (subword-mode)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (cond ((eq (string-match viewersdk-root buffer-file-name) 0)
	 (require 'viewersdk-c-style)
	 (viewersdk-set-c-style))
	(t
	 (require 'google-c-style)
	 (google-set-c-style))))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-mode-hook ()
  (subword-mode)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-js-mode-hook ()
  (subword-mode)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'my-js-mode-hook)

(defun my-lisp-mode-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)

(defun my-python-mode-hook ()
  (subword-mode)
  (setq indent-tabs-mode nil)
  (setq python-indent 2))

(defun my-sh-mode-hook ()
  (subword-mode)
  (setq indent-tabs-mode nil)
  (setq sh-basic-offset 2))
(add-hook 'sh-mode-hook 'my-sh-mode-hook)

(defun my-align-load-hook ()
  (add-to-list 'align-rules-list
	       '(js-declaration
		 (regexp . "^\s*[^\s:]+:\\(\s*\\)[^\s].*,?\s*$")
		 (group 1)
		 (modes quote
			(js-mode)))))
(add-hook 'align-load-hook 'my-align-load-hook)

(desktop-save-mode 1)

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
