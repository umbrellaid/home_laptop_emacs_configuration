;;; init.el --- Emacs Home Configuration init -*- lexical-binding: t; -*-
;; This file is NOT part of GNU Emacs.

;; Uncomment line below to test if file loading successfully when opening Emacs.
;; (message "Init loaded from ~/.emacs.d/init.el")

;; Sources
;; https://github.com/goofansu/emacs-config/tree/main
;; https://github.com/goofansu

;; https://github.com/hwadii/home/tree/master
;; https://github.com/hwadii

;; Single Space after Period Sentence Setting from here:
;; https://github.com/hwadii/home/blob/master/config/emacs/init.el

;; Copied better default settings from here:
;; https://github.com/goofansu/emacs-config/blob/main/modules/init-better-defaults.el

;; Copied notmuch settings from here:
;; https://github.com/goofansu/emacs-config/blob/main/modules/init-mail.el

;; Copied ui settings from here:
;; https://github.com/goofansu/emacs-config/blob/main/modules/init-ui.el

;; Copied ux settings from here:
;; https://github.com/goofansu/emacs-config/blob/main/modules/init-ux.el

;; Copied denote config from here:
;; https://github.com/goofansu/emacs-config/blob/main/modules/init-writing.el

;; Bootstrap package.el and use-package
(use-package package
  :ensure nil
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (when (< emacs-major-version 29)
    (unless (package-installed-p 'use-package)
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install 'use-package))))

(use-package emacs
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 80)
  :custom
  (scroll-margin 2)
  (use-dialog-box nil)
  (use-short-answers t)
  (ring-bell-function 'ignore)
  (confirm-kill-emacs 'y-or-n-p)
  (truncate-string-ellipsis "…")
  (select-enable-clipboard t)
  ;; Editing
  (require-final-newline t)
  ;; File
  (delete-by-moving-to-trash t)
  ;; Keyboard
  (echo-keystrokes 0.25)
  ;; Password
  (password-cache-expiry nil)
  ;; Startup
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'text-mode)
  :config
  (save-place-mode 1)
  (blink-cursor-mode -1)
  (global-subword-mode 1)
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (recentf-mode t)
  (setq recentf-max-saved-items 65536)
  ;; Default to UTF-8 encoding
  (set-default-coding-systems 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  ;; Consider a period followed by a single space to be end of sentence.
  (setopt sentence-end-double-space nil)
  )

(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . whitespace-cleanup)))

(use-package display-line-numbers
  :ensure nil
  :bind ("<f7>" . display-line-numbers-mode)
  :custom
  (display-line-numbers-widen t))

(use-package visual-line-mode
  :ensure nil
  :hook text-mode
  :bind ("<f8>" . visual-line-mode))

(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-operandi :no-confirm)
  :bind ("<f9>" . modus-themes-toggle)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-completions '((t . (extrabold))))
  (modus-themes-prompts '(extrabold))
  (modus-themes-headings
   '((agenda-structure . (variable-pitch light 2.2))
     (agenda-date . (variable-pitch regular 1.3))
     (t . (regular 1.15)))))

;; (use-package standard-themes
;;   :ensure t
;;   :if (display-graphic-p)  ; Load only in GUI mode
;;   :demand t  ; Load immediately, like your fontaine setup
;;   :bind (("C-c T" . standard-themes-toggle)  ; Binding for toggling dark / light
;;          ("C-c R" . standard-themes-rotate)) ; Binding for rotation
;;   :custom
;;   ;; General customization options (from sample config)
;;   (standard-themes-bold-constructs t)         ; Bold text where appropriate
;;   (standard-themes-italic-constructs t)       ; Italic text where appropriate
;;   (standard-themes-disable-other-themes t)    ; Disable non-standard themes
;;   (standard-themes-mixed-fonts t)             ; Mix fixed/variable pitch fonts
;;   (standard-themes-variable-pitch-ui t)       ; Variable pitch for UI elements
;;   (standard-themes-prompts '(extrabold italic)) ; Stylize prompts
;;   ;; Define themes to toggle and rotate (inspired by your preset approach)
;;   (standard-themes-to-toggle '(standard-light standard-dark))
;;   (standard-themes-to-rotate '(standard-light standard-light-tinted standard-dark standard-dark-tinted))
;;   ;; Heading customizations (parallel to your fontaine-presets structure)
;;   (standard-themes-headings
;;    '((0 . (variable-pitch light 1.9))         ; Level 0 heading
;;      (1 . (variable-pitch light 1.8))         ; Level 1 heading
;;      (2 . (variable-pitch light 1.7))         ; Level 2 heading
;;      (3 . (variable-pitch semilight 1.6))     ; Level 3 heading
;;      (4 . (variable-pitch semilight 1.5))     ; Level 4 heading
;;      (5 . (variable-pitch 1.4))               ; Level 5 heading
;;      (6 . (variable-pitch 1.3))               ; Level 6 heading
;;      (7 . (variable-pitch 1.2))               ; Level 7 heading
;;      (agenda-date . (1.3))                    ; Agenda date size
;;      (agenda-structure . (variable-pitch light 1.8)) ; Agenda structure
;;      (t . (variable-pitch 1.1))))             ; Default for unspecified levels
;;   :config
;;   ;; Load the 'standard-light' theme by default (like your 'regular' preset)
;;   (standard-themes-load-theme 'standard-light))

(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :demand t
  :bind ("C-c F" . fontaine-set-preset)
  :custom
  (fontaine-presets
   '((small
      :default-height 120)
     (regular
      :default-height 160)
     (large
      :default-family "Aporetic Serif Mono"
      :default-weight semilight
      :default-height 180
      :fixed-pitch-family "Aporetic Serif Mono"
      :variable-pitch-family "Aporetic Sans"
      :bold-weight extrabold)
     (presentation
      :inherit large
      :default-height 260)
     (t
      :default-family "Aporetic Sans Mono"
      :default-weight regular
      :default-slant normal
      :default-width normal
      :default-height 100
      :fixed-pitch-family "Aporetic Sans Mono"
      :variable-pitch-family "Aporetic Serif")
     ))
  :config
  (fontaine-set-preset 'regular))

(use-package spacious-padding
  :if (display-graphic-p)
  :custom
  (spacious-padding-subtle-mode-line t)
  :config
  (spacious-padding-mode 1))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  )

;; (use-package ispell
;;   :ensure nil
;;   :custom
;;   (ispell-program-name "/opt/homebrew/bin/hunspell"))

;; (use-package flyspell
;;   :ensure nil
;;   :after ispell
;;   :hook ((markdown-mode org-mode) . flyspell-mode))

(use-package olivetti
  :ensure t
  :custom
  (olivetti-minimum-body-width 120)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package embrace
  :ensure t
  )

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package org
  :ensure nil ; do not try to install it as it is built-in
  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-directory "~/my_org_files/")
  (setq org-default-notes-file (concat org-directory "/my_main_notes_file.org"))
  (setq org-agenda-files (list org-directory))

  ;; Learn about the ! and more by reading the relevant section of the
  ;; Org manual.  Evaluate: (info "(org) Tracking TODO state changes")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  :custom
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (expand-file-name "todo.org" org-directory) "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("w" "Work" entry (file+headline (expand-file-name "work.org" org-directory) "Work")
           "* TODO %?\n  %i\n  %a")
          ("p" "Personal" entry (file+headline (expand-file-name "personal.org" org-directory) "Personal")
           "* TODO %?\n  %i\n  %a")))
  )

(use-package avy
  :ensure t
  )

(use-package magit
  :ensure t
  )

(use-package casual-suite
  :ensure t
  :config
  (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
  (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  (keymap-global-set "M-g" #'casual-avy-tmenu)
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
  (keymap-global-set "C-o" #'casual-editkit-main-tmenu))

(use-package which-key
  ;; :defer 5
  :ensure t
  :config
  (which-key-mode))

;; Custom functions (kept outside use-package as it's not package-specific)
(defun drr-load-emacs-init-config ()
  "Visit Emacs main init.el config file."
  (interactive) 
  (find-file "~/.emacs.d/init.el"))

(defun drr-my-reindent-file ()
  "Reindent the entire file and return to the original cursor position."
  (interactive)
  (let ((original-line (line-number-at-pos))
        (original-column (current-column)))
    (goto-char (point-min))
    (mark-whole-buffer)
    (indent-region (point-min) (point-max))
    (goto-line original-line)
    (move-to-column original-column nil)))

(defun drr-condense-blank-lines ()
  "Condense multiple blank lines into a single blank line in the entire buffer."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\n\n+" nil t)
    (replace-match "\n\n")))

(defun drr-insert-date-stamp-prefix ()
  "Inserts the current date in mm-dd-yyyy format, prefixed with 'Date: '."
  (interactive)
  (insert (format-time-string "Date: %m-%d-%Y")))

(defun drr-insert-date-stamp ()
  "Inserts the current date in mm-dd-yyyy format"
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))

;; guix install notmuch
;; setup notmuch on the command line
;; guix install emacs-notmuch
;; I could probably just have use-package install the
;; emacs notmuch but I installed both through GNU GUIX
;; to make sure they were the same version
;; I use GNU GUIX running on linux mint (ubuntu base)
(use-package notmuch
  :ensure nil
  :hook (notmuch-mua-send . notmuch-mua-attachment-check)
  :bind
  (("C-c m" . notmuch)
   ("C-x m" . notmuch-mua-new-mail))
  :custom
  ;; General UI
  (notmuch-show-logo nil)
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator "")
  (notmuch-show-all-tags-list t)
  (notmuch-hello-sections
   '(notmuch-hello-insert-saved-searches
     notmuch-hello-insert-recent-searches
     notmuch-hello-insert-tags-section))
  ;; Search
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20.20s  ")
     ("subject" . "%-80.80s  ")
     ("tags" . "(%s)")))
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20.20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-80.80s  ")
     ("tags" . "(%s)")))
  (notmuch-saved-searches
   `(( :name "inbox"
       :query "tag:inbox"
       :sort-order newest-first
       :key ,(kbd "i"))
     ( :name "unread (inbox)"
       :query "tag:unread and tag:inbox"
       :sort-order newest-first
       :key ,(kbd "u"))
     ))
  ;; Compose
  (notmuch-always-prompt-for-sender nil)
  (notmuch-fcc-dirs "sent +sent -unread")
  ;; Reading
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-wash-wrap-lines-length 120)
  (notmuch-unthreaded-show-out nil))

(use-package emacs
  :ensure nil
  :config
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-html2text-browser 'browse-url-generic)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-refile-folder "/gmail/[Gmail]/All Mail")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-maildir-shortcuts
        '((:maildir "/gmail/[Gmail]/All Mail"   :key ?a)
          (:maildir "/local_only_emails"   :key ?l)))
  (setq
   user-mail-address "daverecor@gmail.com"
   user-full-name  "David Recor"
   message-signature
   (concat
    "David Recor"))
  ;; make sure the gnutls command line utils are installed
  ;; package 'gnutls-bin' in Debian/Ubuntu
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "daverecor@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-change-filenames-when-moving t)
  )

(use-package org-make-toc
  :ensure t
  :hook (org-mode . org-make-toc-mode)  ; Auto-update on save
  :config
  (message "org-make-toc loaded"))
