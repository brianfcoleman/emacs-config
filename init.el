;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup load path and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-load-path ()
  (add-to-list 'load-path
               (format "%s/.emacs.d" (getenv "HOME")))
  (add-to-list 'load-path
               (format "%s/.emacs.d/site-lisp" (getenv "HOME"))))

(defun bc-setup-customization ()
  (setq custom-file
        (format "%s/.emacs.d/init-customization.el" (getenv "HOME")))
  (load custom-file))

(defvar bc-package-list
  '(ace-jump-mode
    auto-complete
    auto-complete-clang
    clojure-mode
    ecb
    evil
    expand-region
    expectations-mode
    fill-column-indicator
    find-things-fast
    flex-isearch
    helm
    idomenu
    key-chord
    magit
    monokai-theme
    nrepl
    popup
    smex
    solarized-theme
    undo-tree
    yasnippet
    zenburn-theme))

(defun bc-setup-package-manager ()
  (require 'package)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
  (mapcar (lambda (package)
            (if (not (package-installed-p package))
                (package-install package)))
          bc-package-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-input()
  (electric-pair-mode 1)

  (require 'evil)
  (setq evil-default-cursor "#FFFFFF")
  (evil-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-completion ()
  "Setup autocompletion"
  (require 'yasnippet)
  (yas-global-mode 1)
  (require 'auto-complete-config)
  (ac-config-default)
  (defadvice ac-cc-mode-setup (after bc-complete-clang)
    "Add the clang completion source to autocomplete in c mode"
    (require 'auto-complete-clang)
    (setq ac-clang-flags (append '("-std=c++0x" "-I.") (bc-include-path)))
    (setq ac-sources (append '(ac-source-clang ac-source-semantic) ac-sources)))
  ;; TODO clang and semantic are very slow. clang fails because I have setup the
  ;; include paths incorrectly.
  ;; (ad-activate 'ac-cc-mode-setup)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup search and navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
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
      ;; TODO Do I need to visit the tags table buffer
      (visit-tags-table-buffer)
      (let ((tags-files
             (remove-if-not (lambda (tags-file)
                              (string-equal (expand-file-name tags-file)
                                            current-file))
                            (tags-table-files))))
        (if (>= (length tags-files) 0)
            ;; TODO car does not seem to work here. Why?
            (let ((tags-file (elt tags-files 0)))
              (list-tags tags-file)))))))

(defadvice visit-tags-table (before bc-reset-tags)
  "Clear the tags completion table and the current tags table
before visiting a new tags table"
  (setq tags-completion-table nil)
  (tags-reset-tags-tables))

(defun bc-setup-search-and-navigation ()
  (setq dabbrev-case-fold-search nil)

  (require 'find-file-in-project)
  (setq ffip-limit 1000000)
  (setq ffip-patterns
        '("*.c" "*.cc" "*.cpp" "*.cxx" "*.el" "*.h" "*.hpp" "*.js" "*.html"
          "*.java" "*.m" "*.mm" "*.py" "*.sh"))

  (require 'cl)
  (require 'ace-jump-mode)

  (require 'expand-region)

  (require 'flex-isearch)
  (flex-isearch-mode 1)
  (setq flex-isearch-auto t)

  (icomplete-mode 1)

  (setq imenu-auto-rescan t)
  (autoload 'idomenu "idomenu" nil t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

  (require 'smex)
  (smex-initialize)

  (require 'semantic/ia)
  (semantic-mode 1)
  (global-ede-mode t)
  (load "init-ede-projects")
  (bc-setup-ede-project)

  (ad-activate 'visit-tags-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun bc-end-statement ()
  "End a statement in a C-like language by inserting a semicolon."
  (interactive)
  (search-forward-regexp "$")
  (replace-match ";"))

(defun bc-trim-newlines ()
  "Replace occurences of 3 or more consecutive newlines with 2 newlines."
  (interactive)
  (save-excursion
    (replace-regexp "[\n]\\{3,\\}" "\n\n" nil (point-min) (point-max))))

(defun bc-implement-member-function ()
  "Convert a member function declaration into a member function definition."
  (interactive)
  (search-backward-regexp "^")
  (search-forward-regexp "^ *\\(virtual\\)?\\([^~(;]*\\)\\(~?\\<[^(;]+([^);]*)\\)\\([^=;]*\\)\\(= *0\\)?\\([^;]*\\);$")
  (message "[%s][%s][%s]" (match-string 2) (match-string 3) (match-string 4))
  (replace-match (format "%s\n%s%s;" (match-string 2) (match-string 3) (match-string 4)))
  (search-backward-regexp "^")
  (insert (format "%s::" (file-name-base (buffer-file-name))))
  (search-forward ";")
  (replace-match "\n{\n}"))

(defun bc-insert-pair (open-char close-char)
  "Insert a pair of characters at the current point in the buffer"
  (let ((start-point (point)))
    (insert-char open-char)
    (insert-char close-char)
    (goto-char (+ start-point 1))))

(defun bc-setup-key-bindings ()
  (global-set-key (kbd "<f5>") 'bc-implement-member-function)
  (global-set-key (kbd "<f6>") 'bc-trim-newlines)
  (global-set-key (kbd "<f7>") 'bc-end-statement)
  (global-set-key (kbd "<f8>") (lambda () (interactive) (bc-insert-pair ?( ?))))
  (global-set-key (kbd "<f9>") (lambda () (interactive) (bc-insert-pair ?{ ?})))
  (global-set-key (kbd "<f10>") (lambda () (interactive) (bc-insert-pair ?[ ?])))
  (bc-setup-evil-mode-key-bindings))

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

(defun bc-setup-evil-mode-key-bindings ()
  (require 'key-chord)
  (setq key-chord-two-keys-delay 1.0)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (key-chord-mode 1)
  (define-key evil-motion-state-map ":" nil)
  (define-key evil-motion-state-map ";" nil)
  (define-key evil-motion-state-map ":" 'smex)
  (define-key evil-motion-state-map ";" 'evil-ex)
  (define-key evil-normal-state-map "/" 'bc-search-forward)
  (define-key evil-normal-state-map "?" 'bc-search-backward)
  (define-key evil-normal-state-map "n" 'bc-repeat-last-search)
  (define-key evil-normal-state-map "N" 'bc-repeat-last-search-reversed)
  (define-key evil-ex-map "ib" 'ido-switch-buffer)
  (define-key evil-ex-map "id" 'ido-dired)
  (define-key evil-ex-map "ie" 'ido-find-file)
  (define-key evil-ex-map "o" 'other-window)
  (define-key evil-ex-map "tn" 'bc-find-next-tag)
  (define-key evil-ex-map "tp" 'bc-find-previous-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-setup-wrapping ()
  (setq sentence-end-double-space nil)
  (setq fill-column 80)

  (require 'fill-column-indicator)
  (setq fci-rule-color "#ffffff")
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

  (global-linum-mode -1))

(defun bc-setup-fonts ()
  (cond ((eq system-type 'darwin)
         (set-frame-font "Menlo-12" nil t))
        ((eq system-type 'gnu/linux)
         (set-frame-font "Liberation Mono-10" nil t))
        ((eq system-type 'windows-nt)
         (set-frame-font "Consolas-10" nil t))))

(defun bc-setup-windowed-mode-appearance ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(fullscreen . fullboth))
  (bc-setup-fonts)
  (load-theme 'monokai))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bc-prog-mode-hook ()
  (flex-isearch-mode 1)
  (bc-setup-wrapping))
(add-hook 'prog-mode-hook 'bc-prog-mode-hook)

(defun bc-c-mode-common-hook ()
  (subword-mode)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (require 'vnc-c-style)
  (vnc-set-c-style))
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
  (setq indent-tabs-mode nil)
  (show-paren-mode 1))
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

(defun bc-setup-compilation-buffer ()
  (require 'ansi-color)
  (defun bc-color-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'bc-color-compilation-buffer))

(bc-setup-compilation-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(desktop-save-mode 1)
