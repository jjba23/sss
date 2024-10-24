;;; init.el --- Custom editor configuration for JJBA -*- lexical-binding: t -*-

;; Copyright (C) 2024 Josep Bigorra

;; Version: 0.6.0
;; Author: Josep Bigorra <jjbigorra@gmail.com>
;; Maintainer: Josep Bigorra <jjbigorra@gmail.com>
;; URL: https://github.com/jjba23/sss
;; Package: emacs
;; Package-Requires: ((emacs "29.1"))

;; sss is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sss is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sss.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil 
                              :depth 1 
                              :files (:defaults "elpaca-test.el" (:exclude "extensions")) 
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory)) 
       (build (expand-file-name "elpaca/" elpaca-builds-directory)) 
       (order (cdr elpaca-order)) 
       (default-directory repo)) 
  (add-to-list 'load-path (if (file-exists-p build) build repo)) 
  (unless (file-exists-p repo) 
    (make-directory repo t) 
    (when (< emacs-major-version 28) 
      (require 'subr-x)) 
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*")) 
		 ((zerop (apply #'call-process `("git" nil ,buffer t "clone" ,@(when-let ((depth (plist-get order :depth))) 
										 (list (format "--depth=%d" depth) "--no-single-branch")) 
						 ,(plist-get order 
							     :repo) ,repo)))) 
                 ((zerop (call-process "git" nil buffer t "checkout" (or (plist-get order 
                                                                                    :ref) "--")))) 
                 (emacs (concat invocation-directory invocation-name)) 
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch" "--eval" "(byte-recompile-directory \".\" 0 'force)"))) 
                 ((require 'elpaca)) 
                 ((elpaca-generate-autoloads "elpaca" repo))) 
            (progn (message "%s" (buffer-string)) 
                   (kill-buffer buffer)) 
          (error "%s" (with-current-buffer buffer (buffer-string)))) 
      ((error) 
       (warn "%s" err) 
       (delete-directory repo 'recursive)))) 
  (unless (require 'elpaca-autoloads nil t) 
    (require 'elpaca) 
    (elpaca-generate-autoloads "elpaca" repo) 
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package (elpaca-use-package-mode))

;; Declare jjba customizations
(defgroup jjba ()
  "JJBA customization group."
  :group 'tools)

(defcustom jjba-font-mono "Intel One Mono"
  "My personal choice for monospaced font family." 
  :type 'string)

(defcustom jjba-font-sans "Liberation Sans"
  "My personal choice for sans font family." 
  :type 'string)

(defcustom jjba-ews-music-directory "~/Muziek"
  "My personal main directory where to read music from."
  :type 'string)


;; Declare jjba packages

(use-package git-riddance 
  :ensure (:host github :repo "jjba23/git-riddance.el" :branch "trunk"))

(use-package modusregel
  :ensure (:host github :repo "jjba23/modusregel" :branch "trunk")
  :config
  (setq-default mode-line-format modusregel-format))

(use-package welkomscherm
  :ensure (:host github :repo "jjba23/welkomscherm.el" :branch "trunk")
  :bind (("C-c SPC SPC" . welkomscherm)
         )
  :init
  (setq welkomscherm-bookmarks-personal
        '((("sss" . "~/Ontwikkeling/Persoonlijk/sss/")
           ("notes" . "~/Ontwikkeling/Persoonlijk/private-notes/")
           ("emacs config" . "~/Ontwikkeling/Persoonlijk/sss/home/joe/emacs/init.el")
           ("stumpwm" . "~/Ontwikkeling/Persoonlijk/sss/home/joe/stumpwm/init.lisp")
           )
          (("wikimusic-api" . "~/Ontwikkeling/Persoonlijk/wikimusic-api/")
           ("wikimusic-ssr" . "~/Ontwikkeling/Persoonlijk/wikimusic-ssr/")
           ("byggsteg" . "~/Ontwikkeling/Persoonlijk/byggsteg/"))))
  (setq welkomscherm-bookmarks-work
        '((("Vandebron" . "~/Ontwikkeling/Werk/Vandebron/")
           ("hem-wiki" . "~/Ontwikkeling/Werk/hem-wiki/"))          
          ))

  (setq welkomscherm-buttons-actions
        '((("*scratch*" . (lambda(btn) (switch-to-buffer "*scratch*")))
           ("*Messages*" . (lambda(btn) (switch-to-buffer "*Messages*")))
           ("re-render me" . (lambda(btn) (welkomscherm)))
           )
          ))
  )


(use-package tekengrootte 
  :ensure (:host github :repo "jjba23/tekengrootte.el" :branch "trunk") 
  :bind (("C-c f c" . tekengrootte-set-scale-colossal) 
         ("C-c f j" . tekengrootte-set-scale-jumbo) 
         ("C-c f x" . tekengrootte-set-scale-larger) 
         ("C-c f l" . tekengrootte-set-scale-large) 
         ("C-c f r" . tekengrootte-set-scale-regular) 
         ("C-c f s" . tekengrootte-set-scale-small) 
         ("C-c f t" . tekengrootte-set-scale-tiny)
	 ("C-c f n" . tekengrootte-set-scale-nano)) 
  :hook ((tekengrootte-set-scale . (lambda () 
                                     (jjba-set-base-faces)))) 
  :config
  (defun jjba-set-base-faces ()
    "Adjust the base Emacs faces to my preferences.
According to size, color and font family"

    (set-face-attribute 'window-divider nil
                        :foreground (ef-themes-get-color-value 'bg-alt)
                        :background (ef-themes-get-color-value 'bg-alt))
    
    (set-face-attribute 'default nil 
		        :height (round (tekengrootte-mk-font-size 114)) 
		        :font jjba-font-mono)
    (set-face-attribute 'mode-line nil 
		        :height (tekengrootte-mk-font-size 0.7)
		        :font jjba-font-mono) 
    (set-face-attribute 'mode-line-active nil 
		        :height (tekengrootte-mk-font-size 0.7) 
		        :font jjba-font-mono) 
    (set-face-attribute 'mode-line-inactive nil 
		        :height (tekengrootte-mk-font-size 0.7) 
		        :font jjba-font-mono)
    (set-face-attribute 'variable-pitch nil 
		        :font jjba-font-sans
                        :height (tekengrootte-mk-font-size 1.05))

    (set-face-attribute 'org-default nil 
		        :height (tekengrootte-mk-font-size 1.05)
		        :font jjba-font-sans)
    (set-face-attribute 'button nil :background 'unspecified
                        :weight 'bold)
    (set-face-attribute 'org-block nil
		        :font jjba-font-mono
                        :height (tekengrootte-mk-font-size 1.05))
    (set-face-attribute 'font-lock-doc-face nil
                        :foreground (ef-themes-get-color-value 'yellow-faint))
    (set-face-attribute 'org-code nil                       
		        :font jjba-font-mono
                        :height (tekengrootte-mk-font-size 1.05))
    (set-face-attribute 'org-verbatim nil                       
		        :font jjba-font-mono
                        :height (tekengrootte-mk-font-size 1.05))
    (set-face-attribute 'org-level-1 nil 
		        :height (tekengrootte-mk-font-size 1.2))
    (set-face-attribute 'org-level-2 nil 
		        :height (tekengrootte-mk-font-size 1.2))
    (set-face-attribute 'org-level-3 nil 
		        :height (tekengrootte-mk-font-size 1.1))
    (set-face-attribute 'org-level-4 nil 
		        :height (tekengrootte-mk-font-size 1.1))
    (set-face-attribute 'org-level-5 nil 
		        :height (tekengrootte-mk-font-size 1.0)))
  
  (jjba-set-base-faces))

;; Dev

(use-package eglot
  :ensure nil
  :hook ((scala-ts-mode . eglot-ensure)
	 (sh-mode . eglot-ensure)
	 (haskell-mode . eglot-ensure)
	 (markdown-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (before-save . eglot-format-buffer))
  :bind (("C-c i i" . eglot-find-implementation)
	 ("C-c i e" . eglot)
	 ("C-c i k" . eglot-shutdown-all)
	 ("C-c i r" . eglot-rename)
	 ("C-c i x" . eglot-reconnect)
	 ("C-c i a" . eglot-code-actions)
	 ("C-c i m" . eglot-menu)
	 ("C-c i f" . eglot-format-buffer)
	 ("C-c i h" . eglot-inlay-hints-mode))
  :config  
  (add-to-list 'eglot-server-programs '(scala-ts-mode . ("metals")))
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))

  (setq-default eglot-workspace-configuration
                '(
                  :metals (
                           :autoImportBuild t
                           :superMethodLensesEnabled t
                           :showInferredType t
                           :enableSemanticHighlighting t
                           :inlayHints (:inferredTypes (:enable t )
                                                       :implicitArguments (:enable nil)
                                                       :implicitConversions (:enable nil )
                                                       :typeParameters (:enable t )
                                                       :hintsInPatternMatch (:enable nil )))
                  :haskell (:formattingProvider "ormolu")
                  :nil (:formatting (:command ["nixfmt"]))))

  (setq eglot-autoshutdown t
        eglot-confirm-server-edits nil
        eglot-report-progress t
        eglot-extend-to-xref t
        eglot-autoreconnect t)

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))


(use-package package-lint :ensure t)

(use-package markdown-mode 
  :ensure t 
  :mode "\\.md\\'"
  :hook ((markdown-mode . jjba-markdown-mode))
  :config
  (defun jjba-markdown-mode ()
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (set-face-attribute 'markdown-pre-face nil                       
		        :font jjba-font-mono)
    (set-face-attribute 'markdown-code-face nil                       
		        :font jjba-font-mono)
    (visual-line-mode 1)))

(use-package haskell-mode :ensure t :mode "\\.hs\\'")

(use-package scala-ts-mode :ensure t :mode "\\.scala\\'")

(use-package lua-mode :ensure t :mode "\\.lua\\'")

(use-package typescript-mode :ensure t :mode "\\.ts\\'")

(use-package nix-ts-mode 
  :ensure t 
  :mode "\\.nix\\'")

;; Dev ends here

;; Emacs UI/UX/DX

(use-package ef-themes
  :ensure t
  :config
  (setq ef-bio-palette-overrides '((variable fg-main)
                                   (string green-faint)))
  (setq ef-cyprus-palette-overrides '((variable fg-main)
                                      (bg-main bg-dim)
                                      (string green-faint)))
  (load-theme 'ef-bio t)
  ;;(load-theme 'ef-cyprus t)
  )




(use-package vertico 
  :ensure t 
  :init
  (setq vertico-cycle t
        vertico-resize t) 
  :config (vertico-mode))

(use-package marginalia 
  :ensure t 
  :after (vertico) 
  :config (marginalia-mode))

(use-package nerd-icons :ensure t)

(use-package nerd-icons-completion 
  :ensure t  
  :after (nerd-icons marginalia) 
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup)) 
  :config (nerd-icons-completion-mode))

(use-package flymake-collection 
  :ensure t 
  :hook ((after-init . flymake-collection-hook-setup) 
         (emacs-lisp-mode . flymake-mode)))

(use-package transient :ensure t)

(use-package magit :ensure t :after (transient))

(use-package ripgrep
  :ensure t
  :bind (("C-c z r" . ripgrep-regexp)))

(use-package toml-mode :ensure t :mode "\\.toml\\'")

(use-package yaml-mode :ensure t :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

(use-package org-contrib :ensure t)

(use-package org-present :ensure t)

(use-package org-auto-tangle
  :ensure t
  :after (org org-contrib)
  :hook ((org-mode . org-auto-tangle-mode)))

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-directory (file-truename "~/Ontwikkeling/Persoonlijk/private-notes/Roam")
        org-roam-v2-ack t
        org-roam-node-display-template (concat "$\{title:*} " (propertize "$\{tags:10}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(use-package org-roam-ui
  :ensure t
  :after (org-roam)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package super-save 
  :ensure t  
  :init (setq super-save-auto-save-when-idle t auto-save-default nil make-backup-files nil) 
  :config (super-save-mode +1) 
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package spacious-padding 
  :ensure t 
  :init (setq spacious-padding-widths '( :internal-border-width 18 
                                         :header-line-width 2 
                                         :mode-line-width 2 
                                         :tab-width 4 
                                         :right-divider-width 2 
                                         :scroll-bar-width 8 
                                         :left-fringe-width 16 
                                         :right-fringe-width 16)) 
  (setq spacious-padding-subtle-mode-line nil) 
  :config (spacious-padding-mode))

(use-package which-key 
  :ensure t
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha	
	which-key-max-description-length 35)
  (setq-default which-key-idle-delay 0.4) 
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package rainbow-mode :ensure t)

(use-package ob-http :ensure t)

(use-package ob-mermaid :ensure t)

(use-package mermaid-mode
  :ensure t
  :config
  (setq mermaid-mode-map
        (let ((map mermaid-mode-map))
          (define-key map (kbd "C-c C-c") nil)
          (define-key map (kbd "C-c C-f") nil)
          (define-key map (kbd "C-c C-b") nil)
          (define-key map (kbd "C-c C-r") nil)
          (define-key map (kbd "C-c C-o") nil)
          (define-key map (kbd "C-c C-d") nil)
          (define-key map (kbd "C-c C-d c") 'mermaid-compile)
          (define-key map (kbd "C-c C-d c") 'mermaid-compile)
          (define-key map (kbd "C-c C-d f") 'mermaid-compile-file)
          (define-key map (kbd "C-c C-d b") 'mermaid-compile-buffer)
          (define-key map (kbd "C-c C-d r") 'mermaid-compile-region)
          (define-key map (kbd "C-c C-d o") 'mermaid-open-browser)
          (define-key map (kbd "C-c C-d d") 'mermaid-open-doc)
          map)))

(use-package page-break-lines :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package bug-hunter :ensure t)

(use-package sly 
  :ensure t  
  :config (setq inferior-lisp-program "sbcl"))

(use-package f :ensure t)

(use-package speed-type :ensure t)

;; (use-package dape 
;;   :ensure t  
;;   :init (setq dape-buffer-window-arrangement 'gud))

(use-package dired-hacks-utils :ensure t)

(use-package dired-subtree 
  :ensure t  
  :bind (:map dired-mode-map (("<mouse-1>" . dired-subtree-toggle)
                              ("<TAB>" . dired-subtree-toggle) 
                              ("C-<tab>" . dired-subtree-toggle) 
                              ("C-<TAB>" . dired-subtree-toggle))))

(use-package dired-open-with :ensure t)

(use-package move-text 
  :ensure t  
  :config (move-text-default-bindings))

(use-package corfu
  :ensure t 
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.2
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-echo-documentation 0.25
        corfu-preview-current 'insert
        corfu-preselect-first nil
        corfu-popupinfo-delay '(0.8 . 0.8))
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode 1))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

(use-package consult 
  :ensure t  
  :bind (
         ("C-c m" . consult-mode-command) 
         ("C-c k" . consult-kmacro) 
         ("C-x M-:" . consult-complex-command) 
         ("C-x b" . consult-buffer) 
         ("C-x 4 b" . consult-buffer-other-window) 
         ("C-x 5 b" . consult-buffer-other-frame) 
         ("C-x r b" . consult-bookmark) 
         ("C-x p b" . consult-project-buffer) 
         ("M-#" . consult-register-load) 
         ("M-'" . consult-register-store) 
         ("C-M-#" . consult-register) 
         ("M-y" . consult-yank-pop) 
         ("M-g e" . consult-compile-error) 
         ("M-g f" . consult-flymake) 
         ("M-g g" . consult-goto-line) 
         ("M-g M-g" . consult-goto-line) 
         ("M-g o" . consult-outline) 
         ("M-g m" . consult-mark) 
         ("M-g k" . consult-global-mark) 
         ("M-g i" . consult-imenu) 
         ("M-g I" . consult-imenu-multi) 
         ("M-s d" . consult-find) 
         ("M-s D" . consult-locate) 
         ("M-s g" . consult-grep) 
         ("M-s G" . consult-git-grep) 
         ("M-s r" . consult-ripgrep) 
         ("M-s l" . consult-line) 
         ("M-s L" . consult-line-multi) 
         ("M-s k" . consult-keep-lines) 
         ("M-s u" . consult-focus-lines) 
         ("M-s e" . consult-isearch-history)) 
  :hook ((completion-list-mode . consult-preview-at-point-mode)) 
  :init (setq register-preview-delay 0.5 register-preview-function #'consult-register-format xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview 
              :override #'consult-register-window) 
  :config (consult-customize consult-theme 
                             :preview-key '(:debounce 0.2 any) 
                             consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register consult--source-recent-file consult--source-project-recent-file 
                             :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") 
  (global-set-key [f6] 'consult-recent-file)
  )

(use-package direnv
  :ensure t 
  :bind (("C-c d d" . direnv-mode)
         ("C-c d a" . direnv-allow)))

(use-package mu4e
  :ensure nil
  :defer 3
  :bind (("C-c C-m m" . mu4e)
         ("C-c C-m s" . mu4e-search)
         ("C-c C-m h" . mu4e-display-manual)
         ("C-c C-m f" . mu4e-headers-toggle-full-search))
  :config
  (setq mu4e-change-filenames-when-moving t
        mu4e-get-mail-command "true"
        mu4e-update-interval nil
        mu4e-maildir "~/Mail/jjbigorra@gmail.com/"
        mu4e-headers-result-limit 1000)    

  (setq mu4e-drafts-folder "/Concepten"
        mu4e-refile-folder "/Archive"
        mu4e-sent-folder   "/Archive"
        mu4e-trash-folder  "/Prullenbak")

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox"    :key ?i)
          (:maildir "/Archive" :key ?s)
          (:maildir "/Prullenbak"     :key ?t)
          (:maildir "/Concepten"    :key ?d)
          (:maildir "/Archive"  :key ?a))
        ))

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)))

(use-package org-modern
  :ensure t
  :config
  (setq org-modern-star 'replace))

(use-package org
  :ensure nil
  :after (org-modern)
  :hook ((org-mode . jjba-org-mode))
  :init
  (setq org-todo-keywords '((sequence "TODO" "WIP" "REVIEWING" "|" "DONE")))
  (setq org-log-done 'time)
  :config  
  (defun jjba-org-mode ()
    (variable-pitch-mode 1)
    (org-modern-mode)
    (org-indent-mode)
    (auto-fill-mode 0)))

(use-package pandoc-mode :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d c" . docker))

(use-package nerd-icons-dired 
  :ensure t  
  :hook ((dired-mode . nerd-icons-dired-mode)))

(use-package geiser
  :ensure t
  :init
  (setq geiser-active-implementations '(guile)))

(use-package geiser-guile
  :ensure t)

(use-package fancy-compilation
  :ensure t
  :commands (fancy-compilation-mode)
  :config
  (setq fancy-compilation-override-colors nil))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package helpful
  :ensure t
  :bind (
	 ("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function)
	 ("C-h C" . helpful-command)))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; first function returning a result wins
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(defun jjba-bookmark-emacs-config ()
  "Visit jjba bookmark: Emacs main init.el config file."
  (interactive) 
  (find-file "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/emacs/init.el"))

(defun jjba-bookmark-stumpwm-config ()
  "Visit jjba bookmark: StumpWM main config file."
  (interactive) 
  (find-file "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/stumpwm/init.lisp"))

(defun new-frame-setup (frame)
  (if (display-graphic-p frame)
      (progn
	(message "window system")
	(tekengrootte-set-scale-regular)

	)
    (message "not a window system")
    ))


(defun sss-joe-reconfigure ()
  "Rebuild GNU Guix Joe's configs."
  (interactive)
  (let ((default-directory "~/Ontwikkeling/Persoonlijk/sss"))
    (async-shell-command "make jr")))

(defun sss-full-reconfigure ()
  "Fully Rebuild GNU Guix Joe's configs and Joe's user."
  (interactive)
  (let ((default-directory "~/Ontwikkeling/Persoonlijk/sss"))
    (async-shell-command "make fr")))

(defun sss-sys-reconfigure ()
  "Rebuild GNU Guix Joe's configs."
  (interactive)
  (let ((default-directory "~/Ontwikkeling/Persoonlijk/sss"))
    (async-shell-command "make sr")))

(defun sss-sys-update ()
  "Update GNU Guix packages."
  (interactive)
  (let ((default-directory "~/Ontwikkeling/Persoonlijk/sss"))
    (async-shell-command "make update")))

(use-package flymake
  :ensure nil
  :bind(("C-c ! b" . flymake-show-buffer-diagnostics)
	("C-c ! n" . flymake-goto-next-error)
	("C-c ! p" . flymake-show-project-diagnostics)
	("C-c ! f" . flymake-mode)))

(use-package flymake-proselint
  :ensure t
  :hook ((text-mode . (lambda () (flymake-mode)(flymake-proselint-setup)))
         (markdown-mode . (lambda () (flymake-mode)(flymake-proselint-setup)))
         (org-mode . (lambda () (flymake-mode)(flymake-proselint-setup))))
  )


;; Emacs Multimedia System
(use-package emms
  :ensure t
  :init
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-source-file-default-directory jjba-ews-music-directory)
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :bind
  (("<f5>"   . emms-browser)
   ("M-<f5>" . emms)
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))


;; Configure Emacs native features

(use-package emacs 
  :ensure nil 
  :bind (("C-x C-b" . ibuffer) 
         ("C-c a h" . highlight-compare-buffers) 
         ("C-c b e" . jjba-bookmark-emacs-config)
         ("C-c b w" . jjba-bookmark-stumpwm-config)
         ("C-c l d" . toggle-debug-on-error)
         ("C-c l e" . eval-buffer)
         ("C-c s b" . flyspell-buffer)
         ("C-c s r" . flyspell-region)
         ("C-c # j" . sss-joe-reconfigure)
         ("C-c # s" . sss-sys-reconfigure)
         ("C-c # u" . sss-sys-update)
         ("C-c # f" . sss-full-reconfigure)
         ("C-c s p" . flyspell-prog-mode))
  :hook ((text-mode . visual-line-mode)
         (after-make-frame-functions . new-frame-setup)
         (org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (org-mode . flyspell-buffer)
         (markdown-mode . flyspell-mode))
  :config
  (setq-default user-personal-name "Joe"
		user-personal-full-name "Josep Jesus Bigorra Algaba"
		user-personal-email "jjbigorra@gmail.com"
		user-personal-initials "JJBA")
  
  (setq-default line-spacing 2
                pgtk-wait-for-event-timeout 0
                electric-indent-inhibit t)

  (setq read-extended-command-predicate #'command-completion-default-include-p
        backward-delete-char-untabify-method 'hungry)
  
  (setq treesit-font-lock-level 4
	ring-bell-function #'ignore
	frame-resize-pixelwise t
	inhibit-startup-message t
        completion-cycle-threshold 3
	tab-always-indent 'complete
	text-mode-ispell-word-completion nil
        vc-follow-symlinks t
	delete-by-moving-to-trash t
	tab-width 2)
  
  (savehist-mode 1) 
  (tool-bar-mode -1) 
  (scroll-bar-mode -1) 
  (menu-bar-mode -1) 
  (delete-selection-mode +1)

  ;; when in wayland this can be nice:
  ;; (set-frame-parameter nil 'alpha-background 90) 
  ;; (add-to-list 'default-frame-alist '(alpha-background . 90))
  
  (when (fboundp 'windmove-default-keybindings) 
    (windmove-default-keybindings))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq dired-listing-switches "-lAh --group-directories-first" dired-kill-when-opening-new-dired-buffer t)
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
  (setq-default indent-tabs-mode nil)
  (global-prettify-symbols-mode +1)

  (setq initial-buffer-choice
        (lambda () (welkomscherm) (get-buffer welkomscherm-buffer-name)))
  
  (recentf-mode 1)
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 100)
  )


;;; init.el ends here

