;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup load path and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-load-path ()
  (add-to-list 'load-path
               (format "%s/.emacs.d" (getenv "HOME")))
  (add-to-list 'load-path
               (format "%s/.emacs.d/site-lisp" (getenv "HOME")))
  (add-to-list 'load-path
               (format "%s/.emacs.d/site-lisp/ido-vertical-mode"
                       (getenv "HOME"))))

(defun bc-setup-customization ()
  (setq custom-file
        (format "%s/.emacs.d/init-customization.el" (getenv "HOME")))
  (load custom-file))

(defvar bc-package-list
  '(auto-complete
    clang-format
    evil
    evil-leader
    flex-isearch
    flx
    flx-ido
    flycheck
    god-mode
    grizzl
    idomenu
    ido-ubiquitous
    ipython
    key-chord
    member-function
    popup
    popwin
    pos-tip
    projectile
    python-mode
    smex
    undo-tree
    yasnippet
    w3m
    zenburn-theme))
;; TODO Consider using irony-mode, clang-tags and function-args

(defun bc-setup-package-manager ()
  (require 'package)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (mapcar (lambda (package)
            (if (not (package-installed-p package))
                (package-install package)))
          bc-package-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bc-dark-background-p t)

(defun bc-setup-input()
  (electric-pair-mode 1)
  (show-paren-mode 1)

  (require 'key-chord)
  (setq key-chord-two-keys-delay 1.0)
  (key-chord-mode 1)

  (require 'god-mode)
  (require 'evil)
  (require 'evil-leader)
  (when bc-dark-background-p
    (setq evil-default-cursor "#FFFFFF"))
  (evil-mode 1)
  (global-evil-leader-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-completion ()
  "Setup autocompletion"
  (require 'yasnippet)
  (yas-global-mode 1)
  (require 'pos-tip)
  (require 'auto-complete-config)
  (ac-config-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup search and navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (tag-name)
                (push (prin1-to-string tag-name t) tag-names))
              tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun bc-ido-find-file-in-tag-files ()
  "Find a file in the current tags table using ido"
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(defun bc-list-tags-current-buffer ()
  "List the tags in the current buffer"
  (interactive)
  (require 'cl)
  (save-excursion
    (let ((current-file (expand-file-name (buffer-file-name))))
      ;; TODO Do I need to visit the tags table buffer?
      (visit-tags-table-buffer)
      (let ((tags-files
             (remove-if-not (lambda (tags-file)
                              (string-equal (expand-file-name tags-file)
                                            current-file))
                            (tags-table-files))))
        (if (>= (length tags-files) 0)
            (let ((tags-file (elt tags-files 0)))
              (list-tags tags-file)))))))

(defadvice visit-tags-table (before bc-reset-tags)
  "Clear the tags completion table and the current tags table
before visiting a new tags table"
  (setq tags-completion-table nil)
  (tags-reset-tags-tables))

(defun bc-setup-search-and-navigation ()
  (setq dabbrev-case-fold-search nil)

  (require 'flex-isearch)
  (flex-isearch-mode 1)
  (setq flex-isearch-auto t)

  (require 'flx-ido)
  (require 'idomenu)
  (require 'ido-ubiquitous)
  (require 'ido-vertical-mode)
  (setq ido-use-faces nil)
  (ido-mode 1)
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)
  (flx-ido-mode 1)

  (require 'smex)
  (smex-initialize)

  (projectile-global-mode)
  ;; TODO Add a function to add a project to projectile-known-projects
  (setq projectile-completion-system 'grizzl)

  (require 'semantic/ia)
  (semantic-mode 1)
  (global-ede-mode t)
  (load "init-ede-projects")
  (bc-setup-ede-project)

  (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))
  (add-hook 'semantic-init-hook (lambda ()
    (setq semantic-imenu-bucketize-file nil)
    (imenu-add-to-menubar "TAGS")))

  (ad-activate 'visit-tags-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-next-window ()
  "Switch to the next window"
  (interactive)
  (other-window 1))
(defun bc-previous-window ()
  "Switch to the previous window"
  (interactive)
  (other-window -1))

(defun bc-find-next-tag ()
  "Find the next occurence of the last tag to be found"
  (interactive)
  (message "%s" last-tag)
  (find-tag last-tag t nil))

(defun bc-find-previous-tag ()
  "Find the previous occurence of the last tag to be found"
  (interactive)
  (find-tag last-tag '- nil))

(defun bc-indent-buffer ()
  "Indent the buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

(defvar bc-last-search-direction 'forward
  "The direction of the last search")

(defun bc-reverse-direction (direction)
  "Reverse direction"
  (cond ((eq direction 'forward) 'backward)
        ((eq direction 'backward) 'forward)))

(defun bc-search (direction)
  "Search forward or backward"
  (setq bc-last-search-direction direction)
  (cond ((eq direction 'forward) (flex-isearch-forward))
        ((eq direction 'backward) (flex-isearch-backward))))

(defun bc-search-forward ()
  "Search forward"
  (interactive)
  (bc-search 'forward))

(defun bc-search-backward ()
  "Search backward"
  (interactive)
  (bc-search 'backward))

(defun bc-repeat-search (direction)
  "Repeat a search forward or backward"
  (cond ((eq direction 'forward) (isearch-repeat-forward))
        ((eq direction 'backward) (isearch-repeat-backward))))

(defun bc-repeat-last-search ()
  "Repeat the last search"
  (interactive)
  (bc-repeat-search bc-last-search-direction))

(defun bc-repeat-last-search-reversed ()
  "Repeat the last search but in the reverse direction"
  (interactive)
  (bc-repeat-search (bc-reverse-direction bc-last-search-direction)))

(defun bc-setup-key-chords ()
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state))

(defun bc-emacs-state ()
  (interactive)
  (evil-emacs-state)
  (god-local-mode))

(defun bc-exit-emacs-state ()
  (interactive)
  (god-local-mode)
  (evil-exit-emacs-state))

(defun bc-setup-evil-mode-key-bindings ()
  (define-key evil-motion-state-map ":" nil)
  (define-key evil-motion-state-map ";" nil)
  (define-key evil-motion-state-map ":" 'evil-ex)
  (define-key evil-motion-state-map ";" 'smex)
  (define-key evil-motion-state-map "\\" 'bc-emacs-state)
  (define-key evil-normal-state-map "/" 'bc-search-forward)
  (define-key evil-normal-state-map "?" 'bc-search-backward)
  (define-key evil-normal-state-map "n" 'bc-repeat-last-search)
  (define-key evil-normal-state-map "N" 'bc-repeat-last-search-reversed)
  (define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent))

(defun bc-setup-evil-leader-key-bindings ()
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "f" 'projectile-find-file)
  (evil-leader/set-key "d" 'projectile-find-dir)
  (evil-leader/set-key "b" 'ido-switch-buffer)
  (evil-leader/set-key "j" 'end-of-defun)
  (evil-leader/set-key "k" 'beginning-of-defun))

(defun bc-setup-god-mode-key-bindings ()
  (define-key god-local-mode-map (kbd "<escape>") 'bc-exit-emacs-state)
  (define-key god-local-mode-map (kbd "\\") 'bc-exit-emacs-state))

(defun bc-setup-minimap-key-bindings ()
  (define-key minibuffer-local-map (kbd "<escape>") 'exit-minibuffer))

(defun bc-setup-key-bindings ()
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'control)
    (setq mac-control-modifier 'meta))
  (bc-setup-key-chords)
  (bc-setup-evil-mode-key-bindings)
  (bc-setup-evil-leader-key-bindings)
  (bc-setup-god-mode-key-bindings)
  (bc-setup-minimap-key-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-wrapping ()
  (setq sentence-end-double-space nil)
  (setq fill-column 80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup whitespace mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-whitespace-mode ()
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-line-column 80)
  (whitespace-mode 1)
  (set-face-foreground 'whitespace-line
                       (cdr (assoc "zenburn-fg" zenburn-colors-alist)))
  (set-face-background 'whitespace-line
                       (cdr (assoc "zenburn-bg-1" zenburn-colors-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-saving ()
  (setq backup-inhibited t)
  (setq auto-save-default nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup file types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-file-types()
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-appearance()
  (bc-setup-common-appearance)
  (when window-system
    (bc-setup-windowed-mode-appearance)))

(defun bc-setup-common-appearance()
  (setq inhibit-startup-screen 1)
  (menu-bar-mode 1)

  (line-number-mode 1)
  (column-number-mode 1)

  (require 'popwin)
  (popwin-mode 1))

(defun bc-setup-fonts ()
  (cond ((eq system-type 'darwin)
         (set-frame-font "Menlo-14" nil t))
        ((eq system-type 'gnu/linux)
         (set-frame-font "Liberation Mono-11" nil t))
        ((eq system-type 'windows-nt)
         (set-frame-font "Consolas-10" nil t))))

(defun bc-setup-windowed-mode-appearance ()
  (if (eq system-type 'darwin)
    (tool-bar-mode 1)
    (tool-bar-mode -1))
  (scroll-bar-mode -1)
  (when (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(fullscreen . fullboth)))
  (bc-setup-fonts)
  (load-theme 'zenburn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-eshell ()
  (setq eshell-prompt-function
        (lambda ()
          "$ "))
  (setq eshell-prompt-regexp "$ "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-ediff ()
  (setq ediff-split-window-function 'split-window-horizontally))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-w3m ()
  (when (eq system-type 'darwin)
    (setq browse-url-browser-function 'w3m-browse-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-flycheck ()
  ;; TODO Configure flycheck mode properly
  (add-hook 'after-init-hook (lambda () (global-flycheck-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke setup functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bc-setup-load-path)
(bc-setup-package-manager)
(bc-setup-customization)
(bc-setup-input)
(bc-setup-completion)
(bc-setup-search-and-navigation)
;; TODO Load all required files in a more structured manner
(load "init-aliases")
(bc-setup-aliases)
(bc-setup-key-bindings)
(bc-setup-wrapping)
(bc-setup-saving)
(bc-setup-file-types)
(bc-setup-appearance)
(bc-setup-eshell)
(bc-setup-ediff)
(bc-setup-w3m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-prog-mode-hook ()
  (flex-isearch-mode 1)
  (bc-setup-wrapping)
  (bc-setup-whitespace-mode))
(add-hook 'prog-mode-hook 'bc-prog-mode-hook)

(defun bc-c-mode-common-hook ()
  (subword-mode)
  (hs-minor-mode)
  (require 'google-c-style)
  (google-set-c-style))
(add-hook 'c-mode-hook 'bc-c-mode-common-hook)
(add-hook 'c++-mode-hook 'bc-c-mode-common-hook)
(add-hook 'objc-mode-hook 'bc-c-mode-common-hook)

(defun bc-c++-mode-hook ()
  (autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t))
(add-hook 'c++-mode-hook 'bc-c++-mode-hook)

(defun bc-java-mode-hook ()
  (subword-mode)
  (setq whitespace-line-column 100)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'bc-java-mode-hook)

(defun bc-js-mode-hook ()
  (subword-mode)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'bc-js-mode-hook)

(defun bc-lisp-mode-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (show-paren-mode 1))
(add-hook 'lisp-mode-hook 'bc-lisp-mode-hook)

(defun bc-setup-python-mode ()
  ;; TODO Python mode does not seem to activate until the second python source
  ;; file is opened.
  (autoload 'python-mode "python-mode" "Python Mode." t)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode)))
(bc-setup-python-mode)

(defun bc-python-mode-hook ()
  (subword-mode)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq python-indent 4))
(add-hook 'python-mode-hook 'bc-python-mode-hook)

(defun bc-sh-mode-hook ()
  (subword-mode)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq sh-basic-offset 4))
(add-hook 'sh-mode-hook 'bc-sh-mode-hook)

(defun bc-strip-whitespace ()
  (untabify 0 (buffer-end 1))
  (delete-trailing-whitespace))
(add-hook 'before-save-hook 'bc-strip-whitespace)

(defun bc-setup-compilation-buffer ()
  (require 'ansi-color)
  (defun bc-color-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'bc-color-compilation-buffer))
(bc-setup-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-desktop-save-mode ()
  (desktop-save-mode 1))
(bc-setup-desktop-save-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
