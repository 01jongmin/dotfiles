(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) 
(setq straight-use-package-by-default t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)          ; Disable visible scrollbar
(tool-bar-mode -1)            ; Disable the toolbar
(tooltip-mode -1)             ; Disable the tooltips
(set-fringe-mode 10)          ; Give some breathing room
(setq scroll-margin 10)       ; auto scroll
(setq scroll-step 1)          ; auto scroll amount

(menu-bar-mode -1)            ; Disable the menu bar

(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "red4")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg))))

(set-face-attribute 'default nil :font "fira code" :height 150)

(use-package doom-themes
  :init (load-theme 'doom-one t))

;; NOTE: The first time loading the configuration on a new machine, you would need to run the following command
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-height 40))

;; Line numbering
(column-number-mode)                      ; Display current line or column
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width 3)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook
                elfeed-show-mode-hook
                mu4e-main-mode-hook
                mu4e-headers-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'mu4e-headers-mode-hook (lambda () (setq-local line-spacing 0.7)))

;; Change window bar appearance in emacs
(setq frame-title-format nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))

(require 'ansi-color)
(defun my-compilation-filter ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'compilation-filter-hook 'my-compilation-filter)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq mac-command-modifier 'control)

(use-package evil
  :init
  (setq evil-want-integration t)     ; Integrate Evil with other modules 
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "0" 'evil-beginning-of-visual-line)
  )

(use-package evil-collection
  :after evil
  :config
  (setq forge-add-default-bindings nil)
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-window-nav (:timeout 4)
  ("h" evil-window-decrease-width "shrink horizontally" :column "Sizing")
  ("H" evil-window-increase-width "enlarge horizontally")
  ("v" evil-window-decrease-height "shrink vertically")
  ("V" evil-window-increase-height "enlarge vertically")
  ("b" balance-windows "balance window height")
  ("m" maximize-window "maximize current window")
  ("M" minimize-window "minimize current window"))

(use-package general
  :config
  (general-create-definer jongmin/leader-keys
    :keymaps '(normal visual emacs eww)
    :prefix "SPC"))

(jongmin/leader-keys
  "d" '(:ignore d  :which-key "directory")
  "dt" '(treemacs  :which-key "tree")
  "dl" '(lsp-treemacs-symbols  :which-key "lsp-tree")
  "s" '(:ignore s :which-key "scale")
  "st" '(hydra-text-scale/body :which-key "scale text")
  "sw"  '(hydra-window-nav/body :which-key "window management"))

(defun jongmin/org-babel-tangle-config-new ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/emacs/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))


(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook          #'jongmin/org-babel-tangle-config-new)))

(defun jongmin/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)) ; auto-wrap

(use-package org
  ;; :straight (org :type git
  ;;                :host github
  ;;                :repo "emacs-straight/org-mode"
  ;;                :tag "e.6.9")

  :straight (:type built-in)
  :hook (org-mode . jongmin/org-mode-setup)
  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))
  ;; Save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-agenda-files
        '("~/org/agenda/Tasks.org"))
          ;;"~/org/agenda/Birthdays.org"
          ;;"~/org/agenda/Habits.org"))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▾")
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/org/agenda/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))
  (plist-put org-format-latex-options :scale 2)
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun jongmin/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jongmin/org-mode-visual-fill))

(use-package org-roam
  :hook org
  :custom 
  (org-roam-directory "~/RoamNotes/")
  (setq org-roam-dailies-directory "journal/")
  :config
  (org-roam-setup)
  )

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(jongmin/leader-keys
  "o" '(:ignore o :which-key "org")
  "oa" '(org-agenda-list :which-key "org agenda")
  "on" '(org-roam-node-find :which-key "org roam node find")
  "oi" '(org-roam-node-insert :which-key "org roam node insert")
  "oc" '(org-capture :which-key "org capture")
  "og" '(org-roam-tag-add :which-key "org tag add")
  "ot" '(lambda() (interactive)(find-file "~/org/agenda/Tasks.org") :which-key "org tasks")
  "od" '(lambda() (interactive)(find-file "~/dotfiles/emacs/Emacs.org") :which-key "org configuration")
  "of" '(lambda() (interactive)(find-file "~/org/agenda/femto.org") :which-key "femto")
  "oj" '(:ignore oj :which-key "org journal")
  "ojt" '(org-roam-dailies-capture-today :which-key "today")
  "ojg" '(:ignore ojg :which-key "org journal goto")
  "ojgt" '(org-roam-dailies-goto-today :which-key "today")
  )

;; create the autosave dir if necessary, since emacs won't.
                                        ;(make-directory "~/.emacs.d/autosaves/" t)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (define-key undo-tree-map (kbd "C-/") nil)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  )

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (require 'dired-x)

  ;; Move delete files to the Trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.local/share/Trash")

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  )

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(jongmin/leader-keys
  "f" '(:ignore f :which-key "file")
  "fj" '(dired-jump :which-key "dired jump")
  "fu" '(undo-tree-visualize :which-key "undo tree"))

(use-package embark-consult)

(use-package vertico
  :bind (:map minibuffer-local-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  (:map vertico-multiform-map
        ("C-c C-o" . vertico-multiform-buffer)
        ("C-c C-e" . embark-collect)
        ("C-c C-x" . vertico-exit-input))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  )

(use-package vertico-posframe
  :init
  (setq vertico-multiform-commands
        '((consult-line (:not posframe))
          (consult-ripgrep (:not posframe) buffer indexed)
          (t posframe)))
  (vertico-posframe-mode 1)
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line))
  :config
  (consult-customize
   consult-grep
   :preview-key nil)
  )

(jongmin/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bs" 'consult-buffer)

(setq-default indent-tabs-mode nil)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :commands magit-status)

(jongmin/leader-keys
  "g" '(:ignore g :which-key "git")
  "gb" '(magit-blame :which-key "git blame")
  "gs" '(magit-status :which-key "status"))

(jongmin/leader-keys
  "r" '(:ignore r :which-key "regex")
  "rr" 'query-replace-regexp)

(use-package lsp-mode
  :init
  ;; Defaults to improve performance
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-log-io nil)
  (setq lsp-auto-guess-root nil)
  (setq project-vc-extra-root-markers '(".git" ".gitmodules"))
  (setq lsp-clients-clangd-executable "/usr/bin/clangd")

  ;; Combine both sets of arguments
  (setq lsp-clients-clangd-args '("--limit-references=0"))

  :config
  ;; Function to dynamically set compile-commands-dir
  (defun my-lsp-clangd-set-compile-commands-dir ()
    "Set clangd compile-commands-dir to current project root."
    (when-let ((proj (project-current)))
      (setq-local lsp-clients-clangd-args 
                  (append '("--limit-references=0") 
                          `("--compile-commands-dir" ,(project-root proj))))))

  ;; Apply this when LSP starts
  (add-hook 'lsp-mode-hook #'my-lsp-clangd-set-compile-commands-dir)
  )

(add-hook 'prog-mode-hook 'lsp)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-ui
  :config
  ;; (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-diagnostics-provider :flycheck)
  )
;; (setq lsp-ui-sideline-show-hover t)

(jongmin/leader-keys
  "l"  '(:ignore t :which-key "lsp")
  "ln" 'lsp-ui-find-next-reference
  "lf" 'lsp-find-definition
  "lc" 'lsp-find-declaration
  "lr" 'lsp-rename
  "le" 'lsp-treemacs-errors-list
  "la" 'lsp-execute-code-action
  "lh" 'lsp-treemacs-call-hierarchy
  "lp"  '(:ignore t :which-key "lsp peek")
  "lpr" 'lsp-ui-peek-find-references
  "lg"  '(:ignore t :which-key "lsp global")
  "lgr" 'lsp-find-references
  )

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common)))

;; company frontend with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package cmake-mode)

(use-package clang-format
  :after lsp
  :config 
  (global-set-key C-M-\ 'clang-format-region))

(setq-default c-basic-offset 4)
(defun my/clang-format-buffer ()
  "Run clang-format on the current buffer."
  (when (or (string-equal (file-name-extension buffer-file-name) "cpp")
            (string-equal (file-name-extension buffer-file-name) "h")
            (string-equal (file-name-extension buffer-file-name) "hpp"))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'my/clang-format-buffer)

(use-package csharp-mode)

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  :hook (protobuf-mode . lsp))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("buf" "beta" "lsp"))
  :major-modes '(protobuf-mode)
  :server-id 'buf-lsp))

(use-package treemacs
  :config
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (setq treemacs--project-follow-delay 0.1)
  (setq treemacs-file-follow-delay 0.1)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-follow-after-init t)
  )

(use-package lsp-treemacs
  :after lsp
  :config (lsp-treemacs-sync-mode 1))

(use-package project)

  (jongmin/leader-keys
    "p" '(:ignore t :which-key "project")
    "pp" 'project-switch-project
    "pc" 'project-compile
    "pf" 'project-find-file
    "pb" 'project-switch-to-buffer
    "psr" 'consult-ripgrep
    "prr"     'project-query-replace-regexp
    )

(defun my-project-root (dir)
  (let ((root (locate-dominating-file dir ".project")))
    (when root
      (cons 'vc root))))

(add-hook 'project-find-functions #'my-project-root)

(use-package which-key
  :defer 0 
  :config 
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package tldr)
