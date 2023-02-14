(add-to-list 'load-path "~/.emacs.d/lisp")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)              ; Disable visible scrollbar (tool-bar-mode -1)
(tool-bar-mode -1)                ; Disable the toolbar
(tooltip-mode -1)                 ; Disable tooltips
(set-fringe-mode 10)              ; Give some breathing room

(menu-bar-mode -1)                ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

(setq ring-bell-function 'ignore)
(setq scroll-step 1
        scroll-conservatively 10000)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                mu4e-headers-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "fira code retina" :height 170)

;; Modifier Key remap
(setq mac-command-modifier 'control)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package general
  :config
  (general-create-definer jongmin/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (jongmin/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose themes")))

(general-define-key "C-M-j" 'counsel-switch-buffer)


(jongmin/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Use visual line mode
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'message-buffer-mode 'normal)
)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start search with ^

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; First time when you load these configuration you need to run the following commands
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;:hook (lsp-mode . jongmin/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package magit)

(use-package forge
  :config
  (setq auth-source '("~/.authinfo")))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Developer")
    (setq projectile-project-search-path '("~/Developer")))
  (setq projectile-switch-project-action #'projectile-dired))


(use-package counsel-projectile
  :config (counsel-projectile-mode))

(setq backup-directory-alist `(("." . "~/.saves/")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(defun jongmin/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . jongmin/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO" "PROGRESS" "|" "DONE" "BLOCKED")))
  (setq org-agenda-files
        '("~/OrgFiles/Tasks.org"
          "~/OrgFiles/Birthdays.org")))
  ;(jongmin/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun jongmin/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . jongmin/org-mode-visual-fill))

;; Automatically tangle our Emacs.org config file when we save it
(defun jongmin/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "/Users/jongmin/dotfiles/emacs/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jongmin/org-babel-tangle-config)))

(setq org-src-tab-acts-natively t)
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :custom
  (mu4e-context-policy #'pick-first)
  (mu4e-headers-precise-alignment 't)
  :config
  (setq mu4e-mu-binary (executable-find "mu"))

  ;; this is the directory we created before:
  (setq mu4e-maildir "~/.maildir")

  ;; this command is called to sync imap servers:
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; how often to call it in seconds:
  (setq mu4e-update-interval 300)

  ;; save attachment to desktop by default
  ;; or another choice of yours:
  (setq mu4e-attachment-dir "~/Desktop")

  ;; rename files when moving - needed for mbsync:
  (setq mu4e-change-filenames-when-moving t)

  ;; list of your email adresses:
  (setq mu4e-user-mail-address-list '("jongmin@seas.upenn.edu"))
  (setq mu4e-view-show-addresses 't)

  (setq smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 465
          smtpmail-stream-type 'ssl)

  (setq message-send-mail-function 'smtpmail-send-it)

  (setq mu4e-compose-format-flowed t)

  (setq mu4e-contexts
      (list

      (make-mu4e-context

      :name "Engineering"
      :match-func
      (lambda (msg)
          (when msg
          (string-prefix-p "/seas" (mu4e-message-field msg :maildir))))
      :vars '((user-mail-address . "jongmin@seas.upenn.edu")
              (user-full-name    . "Jong Min Choi")
              (mu4e-drafts-folder  . "/seas/drafts")
              (mu4e-sent-folder  . "/seas/sent")
              (mu4e-refile-folder  . "/seas/all")
              (mu4e-trash-folder  . "/seas/trash")
              (mu4e-maildir-shortcuts . ( ("/seas/all" . ?a)
                                          ("/seas/INBOX" . ?i)
                                          ("/seas/sent" . ?s)
                                          ("/seas/trash" . ?t)
                                          ("/seas/drafts" . ?d)))))

      (make-mu4e-context

      :name "Wharton"
      :match-func
      (lambda (msg)
          (when msg
          (string-prefix-p "/wharton" (mu4e-message-field msg :maildir))))
      :vars '((user-mail-address . "jongmin@wharton.upenn.edu")
              (user-full-name    . "Jong Min Choi")
              (mu4e-drafts-folder  . "/wharton/drafts")
              (mu4e-sent-folder  . "/wharton/sent")
              (mu4e-refile-folder  . "/wharton/all")
              (mu4e-trash-folder  . "/wharton/trash")
              (mu4e-maildir-shortcuts . ( ("/wharton/all" . ?a)
                                          ("/wharton/INBOX" . ?i)
                                          ("/wharton/sent" . ?s)
                                          ("/wharton/trash" . ?t)
                                          ("/wharton/drafts" . ?d)))))))
  (mu4e t))

(use-package org-mime
       :ensure t)

  (require 'mu4e-thread-folding)

  (setq mu4e-thread-folding-root-folded-prefix-string (propertize " "))

  (setq mu4e-thread-folding-root-unfolded-prefix-string (propertize " " ))

  (add-to-list 'mu4e-header-info-custom
             '(:empty . (:name "Empty"
                         :shortname ""
                         :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))

(define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
(define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
(define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)



(use-package eshell-git-prompt)
(use-package eshell
   :config
    (eshell-git-prompt-use-theme 'powerline))
