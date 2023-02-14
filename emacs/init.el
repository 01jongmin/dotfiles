;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; For debugging purposes
;(setq use-package-verbose t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)          ; Disable visible scrollbar
(tool-bar-mode -1)            ; Disable the toolbar
(tooltip-mode -1)             ; Disable the tooltips
(set-fringe-mode 10)          ; Give some breathing room
(setq scroll-margin 10)       ; auto scroll
(setq scroll-step 1)          ; auto scroll amount

(menu-bar-mode -1)            ; Disable the menu bar

;; Disable visible bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil :font "fira code retina" :height 170)

(use-package doom-themes
  :init (load-theme 'doom-one t))

;; NOTE: The first time loading the configuration on a new machine, you would need to run the following command
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Line numbering
(column-number-mode)                      ; Display current line or column
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)
(setq display-line-numbers-width 3)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Fill screen
(setq frame-resize-pixelwise t)
;; Change window bar appearance in emacs
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq mac-command-modifier 'control)

(use-package evil
  :init
  (setq evil-want-integration t)     ; Integrate Evil with other modules 
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

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
  "g" '(magit-status :which-key "magit")
  "d" '(:ignore d  :which-key "directory")
  "dt" '(treemacs  :which-key "tree")
  "dl" '(lsp-treemacs-symbols  :which-key "lsp-tree")
  "s" '(:ignore s :which-key "scale")
  "st" '(hydra-text-scale/body :which-key "scale text")
  "sw"  '(hydra-window-nav/body :which-key "window management"))

(defun jongmin/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/dotfiles/emacs/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook          #'jongmin/org-babel-tangle-config)))

(defun jongmin/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)) ; auto-wrap

(use-package org
  :pin org
  :hook (org-mode . jongmin/org-mode-setup)
  :config
  (setq org-agenda-files
        '("~/org/agenda.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-ellipsis " ▾"))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :commands magit-status
  :config
  (message "magit")
  )

;; token stored in plaintext in ~/.authinfo
(use-package forge
  :after magit)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-lens-enable nil)
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-enable-snippet nil)
  (lsp-enable-which-key-integration t))

;; Show informations of the symbols on the current lin
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;(add-hook 'c++-mode-hook 'lsp)
(use-package ccls
  :hook ((c-mode c++-mode objc-mode ) . lsp-deferred))

 ;:init (setq ccls-executable "/Users/jongmin/.nix-profile/bin/ccls")

;; (use-package rustic
;;   :hook rust-mode)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common)))

(use-package lsp-treemacs
  :after lsp
  :config (lsp-treemacs-sync-mode 1))

;; (use-package projectile
;;   :ensure t
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)    :init
;;   (when (file-directory-p "~/Developer")
;;     (setq projectile-project-search-path '("~/Developer")))
;;   (setq projectile-switch-project-action #'projectile-dired))

(use-package projectile
  :init
  (projectile-mode 1))
;;   :bind (:map projectile-mode-map
;;               ("s-p" . projectile-command-map)
;;               ("C-c p" . projectile-command-map)))
;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

(jongmin/leader-keys
  "p" '(projectile-command-map :which-key "project"))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :init
  (counsel-mode t)
  (ivy-mode 1))

;; Adds information to switch-buffer and other ivy commands
(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package which-key
  :defer 0 
  :config 
  (which-key-mode)
  (setq which-key-idle-delay 0.3))
