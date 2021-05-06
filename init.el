(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (column-enforce-mode slime use-package ztree exec-path-from-shell guix magit-org-todos magit-gh-pulls magit-todos magithub multiple-cursors neotree paredit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-action-face ((t (:foreground "#8fbcbb"))))
 '(erc-error-face ((t (:foreground "#bf616a"))))
 '(erc-input-face ((t (:foreground "#ebcb8b"))))
 '(erc-notice-face ((t (:foreground "#ebcb8b"))))
 '(erc-timestamp-face ((t (:foreground "#a3be8c")))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; backup
(setq make-backup-files nil
      backup-inhibited t)

;;; Gui
(setq inhibit-startup-screen t)
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

(prefer-coding-system 'utf-8)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;-----------------------------------------------------------------------------
;;; Open files with sudo
;;;-----------------------------------------------------------------------------

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))
(global-set-key (kbd "C-x C-r") 'sudo-find-file)

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
        (when (yes-or-no-p "Are you sure you want to remove this file? ")
          (delete-file filename)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;;;-----------------------------------------------------------------------------
;;; Melpa
;;;-----------------------------------------------------------------------------

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(package-initialize)

;;;-----------------------------------------------------------------------------
;;; Load use-package
;;;-----------------------------------------------------------------------------

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t))

;;;------------------------------------------------------------------------------
;;; Load packages for specific features and modes
;;;------------------------------------------------------------------------------

(use-package exec-path-from-shell
             :ensure t)

(use-package epa
             :custom (epa-pinentry-mode 'loopback)
             :init (epa-file-enable))

(use-package image-dired)

(use-package ido
             :custom
             (ido-everywhere t)
             (ido-max-directory-size 100000)
             (ido-mode 'both)
             ;; Use the current window when visiting files and buffers with ido
             (ido-default-file-method 'selected-window)
             (ido-default-buffer-method 'selected-window)
             ;; Use the current window for indirect buffer display
             (org-indirect-buffer-display 'current-window))

(use-package erc
             :defer 3
             :delight "ε "
             :preface
             (require 'subr-x)
             (defun my/erc-notify (nickname message)
               "Displays a notification message for ERC."
               (let* ((channel (buffer-name))
                      (nick (erc-hl-nicks-trim-irc-nick nickname))
                      (title (if (string-match-p (concat "^" nickname) channel)
                                 nick
                                 (concat nick " (" channel ")")))
                      (msg (s-trim (s-collapse-whitespace message))))
                 (alert (concat nick ": " msg) :title title)))

             (defun my/erc-preprocess (string)
               "Avoids channel flooding."
               (setq str
                     (string-trim
                      (replace-regexp-in-string "\n+" " " str))))
             :hook ((ercn-notify . my/erc-notify)
                    (erc-send-pre . my/erc-preprocess))
             :custom-face
             (erc-action-face ((t (:foreground "#8fbcbb"))))
             (erc-error-face ((t (:foreground "#bf616a"))))
             (erc-input-face ((t (:foreground "#ebcb8b"))))
             (erc-notice-face ((t (:foreground "#ebcb8b"))))
             (erc-timestamp-face ((t (:foreground "#a3be8c"))))
             :custom
             (erc-autojoin-channels-alist '(("freenode.net" "#clim" "#next-browser" "#lisp" "#mezzano")))
             (erc-autojoin-timing 'ident)
             (erc-fill-function 'erc-fill-static)
             (erc-fill-static-center 13)
             (erc-header-line-format "%n on %t (%m)")
             (erc-hide-list '("JOIN" "PART" "QUIT"))
             (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
             (erc-lurker-threshold-time 43200)
             (erc-prompt-for-nickserv-password nil)
             (erc-server-reconnect-attempts 5)
             (erc-server-reconnect-timeout 3)
             (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                        "324" "329" "332" "333" "353" "477"))
             :config
             (add-to-list 'erc-modules 'notifications)
             (erc-services-mode 1)
             (erc-update-modules))

(use-package gnus
             :custom
             (user-mail-address "ebrasca@librepanther.com")
             (user-full-name "Bruno Cichon")
             ;; imaps
             (gnus-select-method
              '(nnimap "librepanther"
                (nnimap-address "imaps.librepanther.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)))
             ;; smtp
             (smtpmail-smtp-server "smtp.librepanther.com")
             (smtpmail-smtp-service 25)
             (send-mail-function 'smtpmail-send-it)
             ;; sort functions
             (gnus-thread-sort-functions
              '(gnus-thread-sort-by-most-recent-date
                (not gnus-thread-sort-by-number)))
             ;; gui
             (gnus-permanently-visible-groups ".*")
             (gnus-summary-display-arrow t)
             (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
             (gnus-thread-ignore-subject t)
             ;; Archived Messages
             (gnus-message-archive-method
              '(nnfolder "archive"
                (nnfolder-inhibit-expiry t)
                (nnfolder-active-file "~/News/sent-mail/active")
                (nnfolder-directory "~/News/sent-mail/")))
             ;; Tree view for groups.
             (gnus-summary-line-format  "%U%R%z %((%4,4k)  %&user-date;  %-32,32f %* %B%S%)\n")
             (gnus-user-date-format-alist '((t . "%d.%m.%y %H:%M")))
             (gnus-sum-thread-tree-indent          "  ")
             (gnus-sum-thread-tree-root            "● ")
             (gnus-sum-thread-tree-false-root      "◎ ")
             (gnus-sum-thread-tree-single-indent   "→ ")
             (gnus-sum-thread-tree-vertical        "│")
             (gnus-sum-thread-tree-leaf-with-other "├─► ")
             (gnus-sum-thread-tree-single-leaf     "└─► ")
             :config
             (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(use-package magit
             :ensure t
             :custom
             (magit-delete-by-moving-to-trash nil)
             (magit-no-confirm '(stage-all-changes unstage-all-changes))
             ;; Stop with these fucking annoying "'"style"'" conventions
             (git-commit-fill-column 9999)
             (git-commit-summary-max-length 9999)
             (git-commit-finish-query-functions nil)
             :config
             (global-set-key (kbd "C-x g") 'magit-status)
             (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package magit-todos
             :ensure t
             :config
             (magit-todos-mode))

(use-package magit-org-todos
             :ensure t
             :config
             (magit-org-todos-autoinsert))

(use-package org
             :preface
             (defun bh/verify-refile-target ()
               "Exclude todo keywords with a done state from refile targets"
               (not (member (nth 2 (org-heading-components)) org-done-keywords)))
             :custom
             (org-clock-persist 'history)
             ;; log
             (org-log-into-drawer t)
             (org-log-done 'time)
             (org-log-reschedule 'logreschedule)
             (org-deadline-warning-days 30)
             (org-enforce-todo-dependencies t)
             (org-agenda-todo-ignore-with-date nil) ; Keep tasks with dates on the global todo lists
             (org-agenda-todo-ignore-deadlines nil) ; Keep tasks with deadlines on the global todo lists
             (org-agenda-todo-ignore-scheduled nil) ; Keep tasks with scheduled dates on the global todo lists
             (org-agenda-todo-ignore-timestamp nil) ; Keep tasks with timestamps on the global todo lists
             (org-agenda-skip-deadline-if-done t) ; Remove completed deadline tasks from the agenda view
             (org-agenda-skip-scheduled-if-done t) ; Remove completed scheduled tasks from the agenda view
             (org-agenda-skip-timestamp-if-done t) ; Remove completed items from search results
             ;; org modules
             (org-modules '(org-gnus org-habit org-irc org-contacts))
             ;; Agenda Setup
             (org-directory "~/org")
             (org-default-notes-file "~/org/refile.org")
             (org-agenda-files '("~/org/todo.org"))
             ;; Task States
             (org-todo-keywords
              '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
             (org-todo-keyword-faces
              '(("PROJECT" :foreground "red" :weight bold)
                ("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)))
             ;; Capture templates
             (org-capture-templates
              '(("t" "todo" entry (file "~/org/refile.org")
                 "* TODO %?\n" :clock-in t :clock-resume t)
                ("r" "respond" entry (file "~/org/refile.org")
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
                 :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/org/refile.org")
                 "* %?" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+datetree "~/org/diary.org")
                 "* %?\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/org/refile.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/org/refile.org")
                 "* MEETING with %?" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file "~/org/refile.org")
                 "* PHONE %?" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/org/refile.org")
                 "* NEXT %?
  %U
  SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
  :LOGBOOK:
  :END:
  :PROPERTIES:
  :STYLE: habit
  :REPEAT_TO_STATE: NEXT
  :END:")
                ("c" "Contacts" entry (file "~/org/contacts.org")
                 "* Contact
  :PROPERTIES:
  :NAME:
  :EMAIL:
  :END:")))
             ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
             (org-refile-targets '((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
             ;; Refile settings
             ;; Exclude DONE state tasks from refile targets
             (org-refile-target-verify-function 'bh/verify-refile-target)
             ;; Use full outline paths for refile targets - we file directly with IDO
             (org-refile-use-outline-path t)
             ;; Targets complete directly with IDO
             (org-outline-path-complete-in-steps nil)
             ;; Allow refile to create parent tasks with confirmation
             (org-refile-allow-creating-parent-nodes 'confirm)
             ;; Use IDO for both buffer and file completion
             (org-completion-use-ido t)
             :config
             (org-clock-persistence-insinuate)
             ;; Standard key bindings
             (global-set-key "\C-cl" 'org-store-link)
             (global-set-key "\C-ca" 'org-agenda)
             (global-set-key "\C-cb" 'org-iswitchb)
             ;; I use C-c c to start capture mode
             (global-set-key (kbd "C-c c") 'org-capture))

(use-package paredit
             :ensure t
             :config
             (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
             (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
             (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
             (add-hook 'ielm-mode-hook #'enable-paredit-mode)
             (add-hook 'lisp-mode-hook #'enable-paredit-mode)
             (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
             (add-hook 'scheme-mode-hook #'enable-paredit-mode)
             (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
             (add-hook 'sly-mode-hook #'enable-paredit-mode)
             (defun override-slime-repl-bindings-with-paredit ()
               (define-key slime-repl-mode-map
                   (read-kbd-macro paredit-backward-delete-key) nil))
             (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit))

(use-package slime
             :init
             (load (expand-file-name "~/quicklisp/slime-helper.el"))
             :custom
             (inferior-lisp-program "sbcl --dynamic-space-size 4000")
             (indent-tabs-mode nil)
             (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
             (slime-startup-animation nil)
             (slime-auto-select-connection 'always)
             (slime-kill-without-query-p t)
             (slime-fuzzy-explanation "")
             (slime-asdf-collect-notes t)
             (slime-inhibit-pipelining nil)
             (slime-load-failed-fasl 'always)
             (slime-when-complete-filename-expand t)
             (slime-repl-history-remove-duplicates t)
             (slime-repl-history-trim-whitespaces t)
             (slime-export-symbol-representation-auto t)
             (lisp-indent-function 'common-lisp-indent-function)
             (lisp-loop-indent-subclauses nil)
             (lisp-loop-indent-forms-like-keywords t)
             (lisp-lambda-list-keyword-parameter-alignment t)
             :config
             (slime-setup '(slime-fancy slime-compiler-notes-tree slime-indentation slime-cl-indent slime-repl)))

(use-package neotree
             :ensure t
             :custom
             (neo-smart-open nil)
             (neo-autorefresh nil)
             (neo-window-width 40)
             :config
             (global-set-key [f8] 'neotree-toggle)
             (add-hook 'neo-after-create-hook (lambda (&optional dummy) (display-line-numbers-mode -1))))

(use-package multiple-cursors
             :ensure t
             :config
             (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
             (global-set-key (kbd "C-M-<next>") 'mc/mark-next-like-this)
             (global-set-key (kbd "C-M-<prior>") 'mc/mark-previous-like-this)
             (global-set-key (kbd "C-M-m <down>") 'mc/mark-next-like-this)
             (global-set-key (kbd "C-M-m <up>") 'mc/mark-previous-like-this)
             (global-set-key (kbd "C-M-m <right>") 'mc/unmark-next-like-this)
             (global-set-key (kbd "C-M-m <left>") 'mc/unmark-previous-like-this)
             (global-set-key (kbd "C-M-m a") 'mc/mark-all-like-this))
