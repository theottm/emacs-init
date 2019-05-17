(require 'package)
(setq package-archives
      '(
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("elpa" . "http://tromey.com/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ))
(setq package-enable-at-startup nil)
(require (quote org))
(package-initialize)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; emacs init file keybinding
(defun find-init ()
  (interactive)
  (find-file "~/code/emacs/org-init/init.org"))
(global-set-key "\C-ce" 'find-init)
;; don't open *Scratch* buffer on opening
(setq initial-scratch-message nil)
;; alternative to Alt-X
(global-set-key "\C-xm" 'execute-extended-command)
;; no menu and icons
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; recentf
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)
;; load-file
(global-set-key "\C-c\C-l" 'load-file)
;; slime (for lisp)
;; y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; change title of the frame to name of buffer
;;(set-frame-name "EMACS <3")
(setq frame-title-format "%b")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/handoff")
(require 'handoff)
(handoff-global-mode)

(require 'helm)
    (require 'helm-config)
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t)
    (defun spacemacs//helm-hide-minibuffer-maybe ()
      "Hide minibuffer in Helm session if we use the header line as input field."
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face
                       (let ((bg-color (face-background 'default nil)))
                         `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))
    (add-hook 'helm-minibuffer-set-up-hook
              'spacemacs//helm-hide-minibuffer-maybe)
    (global-set-key (kbd "M-x") #'helm-M-x)
    (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    (global-set-key (kbd "C-c C-f") #'helm-for-files)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)
    (global-set-key (kbd "M-!") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-b") 'helm-mini)
    (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t)
    (setq helm-autoresize-max-height 50)
    (setq helm-autoresize-min-height 0)
    (helm-autoresize-mode 1)
    (helm-mode 1)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))
;; (add-to-list '(helm-completing-read-handlers-alist) '(find-file))

(helm-flx-mode +1)

(add-to-list 'load-path              "~/.emacs.d/mystery-theme.el/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mystery-theme.el")

(setq dired-listing-switches "-aslh")

(require 'interaction-log)
(interaction-log-mode +1)

(global-set-key
 (kbd "C-h C-l")
 (lambda () (interactive) (display-buffer ilog-buffer-name)))

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner 'logo)

;; (setq dashboard-items '((projects . 5)
;;                         (recents  . 5)
;;                         (agenda . 5)
;;                         (registers . 5)))

;; (defun dashboard-insert-custom (list-size)
;;   (insert "Custom text"))
;; (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
;; (add-to-list 'dashboard-items '(custom) t)

;; (setq show-week-agenda-p t)

(defun external-term()
  "Open simple terminal at the current buffer working dir in a window out of emacs"
  (interactive)
  (shell-command "st &"))

;; Eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
(defun pcomplete/sudo ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((string= "sudo" prec)
           (while (pcomplete-here*
                   (funcall pcomplete-command-completion-function)
                   (pcomplete-arg 'last) t))))))

(add-hook 'term-mode-hook
 	      (function
 	       (lambda ()
 	             (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
 	             (setq-local mouse-yank-at-point t)
 	             (setq-local transient-mark-mode nil)
 	             (auto-fill-mode -1)
 	             (setq tab-width 8 )
				 )))

(defun kill-buffer-no-warning()
  (interactive)
  (with-simulated-input "y RET" (kill-buffer (current-buffer)))
  )

(defun kill-buffer-and-frame-no-warning ()
  (interactive)
  (kill-buffer-no-warning)
  (delete-frame)
  )

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-d") 'kill-buffer-and-frame-no-warning)
            (define-key term-mode-map (kbd "C-d") 'kill-buffer-and-frame-no-warning)
            (define-key term-raw-map (kbd "C-c <left>") 'windmove-left)
            ;; (define-key term-raw-map (kbd "C-c C-<left>") 'windmove-left)
            (define-key term-raw-map (kbd "C-c <right>") 'windmove-right)
            ;; (define-key term-raw-map (kbd "C-c C-<right>") 'windmove-right)
            ;; (define-key term-raw-map (kbd "C-x <left>") 'previous-buffer)
            ;; (define-key term-raw-map (kbd "C-x <right>") 'next-buffer)
            ;; (define-key term-raw-map (kbd "C-x C-<left>") 'previous-buffer)
            ;; (define-key term-raw-map (kbd "C-x C-<right>") 'next-buffer)
            (define-key term-mode-map (kbd "C-<up>") 'term-send-prior)
            (define-key term-mode-map (kbd "C-<down>") 'term-send-next)
            (define-key term-mode-map (kbd "M-<up>") 'backward-paragraph)
            (define-key term-mode-map (kbd "M-<down>") 'forward-paragraph)
            (define-key term-mode-map (kbd "<C-return>") 'term-send-input)
            (define-key term-mode-map (kbd "<RET>") 'newline)
	    (define-key term-raw-map (kbd "M-:") 'eval-expression)
	    (define-key term-raw-map (kbd "M-x") 'helm-M-x)
	    (define-key term-raw-map (kbd "<RET>") 'term-send-input)
            ))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(define-prefix-command 'teddd-map)

(global-set-key (kbd "C-ù") 'teddd-map)

(define-key teddd-map (kbd "d") 'server-start)

(define-key teddd-map (kbd "g") 'customize-group)

(define-key teddd-map (kbd "n") 'make-frame)

(define-key teddd-map (kbd "R") 'eval-region)

(define-key teddd-map (kbd "f") 'find-file)

(define-key teddd-map (kbd "p") 'list-packages)

(define-key teddd-map (kbd "=") 'describe-char)
(define-key teddd-map (kbd "<mouse-1>") 'describe-char)

(define-key teddd-map (kbd "s") 'ispell-region)

(define-key teddd-map (kbd "<tab>") 'show-two-children)

(defun read-mode()
  (interactive)
  (delete-other-windows)
  (multicolumn-split)
  (follow-mode 1)
  (next-line))

(define-key teddd-map (kbd "l") 'org-insert-link-global)

(define-key teddd-map (kbd "C") 'comment-or-uncomment-region)

(define-key teddd-map (kbd "k") 'kill-whole-line)

(define-key teddd-map (kbd "r") 'rectangle-mark-mode)

(defun capitalize-last-word()
  (interactive)
  (capitalize-word -1))

(define-key teddd-map (kbd "c") 'capitalize-last-word)

(defun flush-empty-lines()
  (interactive)
  (mark-whole-buffer)
  (flush-lines "^$"))
(define-key teddd-map (kbd "<backspace>") 'flush-empty-lines)

(define-key teddd-map (kbd "§") 'pair-last-word)

(define-key teddd-map (kbd "y") 'yas-describe-tables)

(define-key teddd-map (kbd "m") 'executable-chmod)

(defun revert-buffer-force()
  (interactive)
  (with-simulated-input "y RET" (revert-buffer))
  )
(define-key teddd-map (kbd "<f5>") 'revert-buffer-force)

(defun find-zshrc ()
  (interactive)
  (find-file "~/.zshrc"))
(define-key teddd-map (kbd "z") 'find-zshrc)


(defun facebook-events-open-browser()
  (interactive)
  (browse-url "https://www.facebook.com/events/calendar/"))
(define-key teddd-map (kbd "F") 'facebook-events-open-browser)

(defun new-term()
  (interactive)
  (make-frame)
  (sane-term-create)
  )

(define-key teddd-map (kbd "x") 'new-term)

(define-key teddd-map (kbd "<up>") 'org-babel-previous-src-block)
(define-key teddd-map (kbd "<down>") 'org-babel-next-src-block)

(defun ob-insert-ipython-block()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "ipython"))
  )

(defun ob-insert-elisp-block()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "elisp"))
  )

(defun ob-insert-ipython-session-block()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "obipythontemplate"))
  )

(define-key teddd-map (kbd "i") 'ob-insert-ipython-block)
(define-key teddd-map (kbd "I") 'ob-insert-ipython-session-block)
(define-key teddd-map (kbd "e") 'ob-insert-elisp-block)

;; (defun ob-comment-uncomment()
;;   (interactive)
;;   (org-edit-special)
;;   (comment-or-uncomment-region)
;;   (org-edit-src-exit)
;;   )

(fset 'ob-run-top-heading
   [?\C-c ?\C-u ?\C-c ?\C-u ?\C-c ?\C-u ?\C-c ?\C-u ?\C-c ?\C-u ?\C-c ?\C-u ?\C-c ?\C-u ?\C-c ?\C-u ?\C-c ?\C-v ?\C-s ?\C-u ?\C- ])
(define-key teddd-map (kbd "b") 'ob-run-top-heading)

(fset 'ob-toogle-comment
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("''" 0 "%d")) arg)))
(define-key teddd-map (kbd "C-c") 'ob-toogle-comment)

;; Comment / Uncomment Region
(global-set-key "\C-c\C-x\C-c" 'comment-or-uncomment-region)
;; comment line
(global-set-key "\C-c\C-x\C-c" 'comment-line)

;; Split windows fuzzy keystroke
(global-set-key (kbd "C-x C-<kp-2>") 'split-window-horizontally)
(global-set-key (kbd "C-x C-<kp-3>") 'split-window-vertically)
(global-set-key (kbd "C-x C-<kp-1>") 'delete-other-windows-vertically)

(defun kill-buffer-delete-frame()
  (interactive)
  (kill-this-buffer)
  (delete-frame)
  )

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-delete-frame)

(define-key key-translation-map (kbd "<C-mouse-4>") (kbd "<up>"))
(define-key key-translation-map (kbd "<C-mouse-5>") (kbd "<down>"))
(global-set-key (kbd "<C-mouse-6>") 'left-char)
(global-set-key (kbd "<C-mouse-7>") 'right-char)

(global-set-key (kbd "<C-S-mouse-6>") 'previous-buffer)
(global-set-key (kbd "<C-S-mouse-7>") 'next-buffer)

(global-set-key (kbd "M-z") 'undo-tree-redo)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(sml-modeline-mode 1)

(set-fringe-mode 3)

(set-face-background 'fringe "black")
(toggle-indicate-empty-lines)

(with-eval-after-load "ispell"
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB,fr_FR,de_DE")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,fr_FR,de_DE"))

;; package menu hook to highlight curent line
(add-hook 'package-menu-mode-hook 'highline-mode)

;; smartscan : look for next expression like the one under the cursor
(smartscan-mode 1)

;; beacon, highlight cursor when windows scroll
(beacon-mode 1)

;; isearch backwards with q
(global-unset-key "\C-q")
(global-unset-key "\C-r")
(global-set-key "\C-q" 'isearch-backward)
(define-key isearch-mode-map "\C-q" 'isearch-repeat-backward)

;; split windows
(global-set-key (kbd "C-x 3") 'split-window-below)
(global-set-key (kbd "C-x <kp-3>") 'split-window-below)
(global-set-key (kbd "C-x 2") 'split-window-right)
(global-set-key (kbd "C-x <kp-2>") 'split-window-right)

;; picture mode : stay in column
(setq scroll-conservatively most-positive-fixnum)

;; ACE jump : helm line
;; (require 'ace-jump-helm-line)
;; ;; enable idle execution for `helm-mini'
;; (ace-jump-helm-line-idle-exec-add 'helm-mini)
;; ;; enable hints preview
;; (ace-jump-helm-line-autoshow-mode +1)
;; ;; use `linum-mode' to show
;; (setq ace-jump-helm-line-autoshow-mode-use-linum t)

;helm-swoop
(global-set-key (kbd "C-S-s") 'helm-swoop)

;; access global mark ring
(global-set-key (kbd "C-x SPC") 'helm-all-mark-rings)

;; avy mode
(global-set-key (kbd "C-x <up>") 'avy-goto-line-above)
(global-set-key (kbd "C-x <down>") 'avy-goto-line-below)
(global-set-key (kbd "C-x C-<up>") 'avy-goto-line-above)
(global-set-key (kbd "C-x C-<down>") 'avy-goto-line-below)

;; multiframe window function
;; (global-set-key (kbd "C-c <left>") 'previous-multiframe-window)
;; (global-set-key (kbd "C-c <right>") 'next-multiframe-window)

;; navigate through windows : wind move
;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))
(defun set-windmove ()
  (interactive)
  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down)
  ;; force org-mode to let windmove work in org-mode
  ;; (add-hook 'org-mode-hook (local-unset-key (kbd "C-c <left>")))
  ;; (add-hook 'org-mode-hook (local-unset-key (kbd "C-c <right>")))
  ;; (add-hook 'org-mode-hook (local-unset-key (kbd "C-c <up>")))
  ;; (add-hook 'org-mode-hook (local-unset-key (kbd "C-c <down>")))
  ;; (add-hook 'org-mode-hook (local-set-key (kbd "C-c <left>") 'windmove-left))
  ;; (add-hook 'org-mode-hook (local-set-key (kbd "C-c <right>") 'windmove-right))
  ;; (add-hook 'org-mode-hook (local-set-key (kbd "C-c <up>") 'windmove-up))
  ;; (add-hook 'org-mode-hook (local-set-key (kbd "C-c <down>") 'windmove-down))
  (global-set-key (kbd "C-c <C-left>")  'windmove-left)
  (global-set-key (kbd "C-c <C-right>") 'windmove-right)
  (global-set-key (kbd "C-c <C-up>")    'windmove-up)
  (global-set-key (kbd "C-c <C-down>")  'windmove-down))
(set-windmove)

;; Make windmove work in org-mode:
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)

;; multicolumn mode
(require 'multicolumn)
(multicolumn-global-mode 1)
;; follow mode
(add-hook 'multicolumn-global-mode-hook 'follow-mode)

;; display keybindings
(which-key-mode 1)

(global-set-key (kbd "C-)") "[")

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))
(global-set-key (kbd "C-x x") 'swap-windows)

(defun step-5-lines()
  (interactive)
  (next-line 5))

(defun back-5-lines()
  (interactive)
  (previous-line 5))

(define-key teddd-map (kbd "<down>") 'step-5-lines)
(define-key teddd-map (kbd "<up>") 'back-5-lines)

(require 'indent-tools)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)
(add-hook 'python-mode-hook
 (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body))
)

;; ORG MODE
;;(load-file "~/elisp/org-mode/lisp/org.elc")
(add-to-list 'load-path "/home/teddd/elisp/org-mode/contrib/lisp" t)
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(require (quote org-install))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c .") 'org-time-stamp)
(setq org-log-done t)
(add-hook 'org-mode-hook 'org-indent-mode)
;; retour à la ligne
(add-hook 'org-mode-hook 'visual-line-mode)

;; Stuff found on John Wiegley blog http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/
(require 'org-agenda)

;; calendar mode navigation : show entries with TAB and jump to it with RET
(defun org-agenda-switch ()
  (define-key org-agenda-mode-map [(tab)] 'org-agenda-recenter)
  (define-key org-agenda-mode-map [(?\r)] 'org-agenda-goto))
(eval-after-load "org" '(org-agenda-switch))

;; end of org-mode configuration code

(require 'org-expiry)
(add-hook 'org-insert-heading-hook 'org-expiry-insert-created)

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp
                    (point-min)
                    (point)))
             (end (if globalp
                    (point-max)
                    (if (eq state 'children)
                      (save-excursion
                        (outline-next-heading)
                        (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0)))
                     (limit
                       (save-excursion
                         (outline-next-heading)
                           (point)))
                     (msg (format
                            (concat
                              "org-cycle-hide-drawers:  "
                              "`:END:`"
                              " line missing at position %s")
                            (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
                  (outline-flag-region start (point-at-eol) t)
                  (user-error msg))))))))))

(fset 'org-beginning-of-headline
   "\C-c\C-b\C-c\C-f")
(define-key org-mode-map (kbd "C-c ù") 'org-beginning-of-headline)

(require 'german-holidays)
(setq calendar-holidays holiday-german-BE-holidays)
'(org-agenda-include-diary t)

;; refile targets
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))

;; use outline path
(setq org-refile-use-outline-path 'file)
;; makes org-refile outline working with helm/ivy
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-image-actual-width 600)

;; show 2 children headings
(defun show-two-children ()
  "Sows 2 levels of descendents of the active heading"
  (interactive)
  (outline-show-children 2))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-todo-keywords
      '((sequence "TODO(t)" "ASK(a)" "|" "DONE(d)")))

(define-key org-read-date-minibuffer-local-map (kbd "<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<up>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<up>")  (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

(defun do-open-apostrophe ()
  "When called, insert insert a '."
  (interactive)
  (insert "'"))
(global-set-key (kbd "'") 'do-open-apostrophe)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("assignment"
                   "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")))

(add-to-list 'org-latex-classes '("ebook"
"\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
))

(add-to-list 'org-latex-classes
      '("org-article"
         "\\documentclass{org-article}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; kill region
(global-set-key (kbd "C-w") 'kill-region)
;; undo
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)
;; select word
(fset 'select-word
      [C-left ?\C-  C-right])
(global-set-key "\C-cw" 'select-word)
;; select line
(fset 'select-line
      [?\C-a ?\C-  ?\C-e])
(global-set-key "\C-cs" 'select-line)
;; select paragraph
(fset 'select-paragraph
      [C-down C-up down ?\C-  C-down left])
(global-set-key "\C-c\C-s" 'select-paragraph)

;; kill to next word but with parentheses
;; copy line
(defun copy-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank))
(global-set-key "\C-c\C-c" 'copy-line)
;; duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
    (global-set-key (kbd "C-c d") 'duplicate-line)
;; yank with !
(global-set-key (kbd "C-!") 'yank)
;; company mode
(add-hook 'after-init-hook 'global-company-mode)
;; (company-quickhelp-mode)
;; electricity
(electric-pair-mode 1)
(electric-quote-mode 1)
;; move line
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
;; undo tree
(global-undo-tree-mode)
;; indent
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c i") 'indent-region)
;; highlight parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
;; auto indent
(require 'auto-indent-mode)
;; expand region
(global-set-key (kbd "C-$") 'er/expand-region)
;; embrace : expanded regions editing
(global-set-key (kbd "M-$") #'embrace-add)
(add-hook 'org-mode-hook #'embrace-org-mode-hook)
(delete-selection-mode 1)

;; Git in Emacs
(require 'magit)
(global-set-key (kbd "C-x g") 'magit)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(setq global-magit-file-mode t)

(require 'smart-hungry-delete)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(yas-global-mode)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c <tab>") yas-maybe-expand)

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)
(defun backup-each-save-filter (filename)
  (let ((ignored-filenames
    	 '("^/tmp" "semantic.cache$" "\\.emacs-places$"
    	   "\\.recentf$" ".newsrc\\(\\.eld\\)?"))
    	(matched-ignored-filename nil))
    (mapc
     (lambda (x)
       (when (string-match x filename)
    	 (setq matched-ignored-filename t)))
     ignored-filenames)
    (not matched-ignored-filename)))
(setq backup-each-save-filter-function 'backup-each-save-filter)

(setq tab-stop-list (number-sequence 4 120 4))

(global-subword-mode 1)

(defmacro minibuffer-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun my-commit-on-save ()
  "commit the buffer"
  (message (buffer-name))
  (setq save-buffer (buffer-name))
  (if (vc-git-responsible-p (buffer-name))
	  (minibuffer-quit-and-run
	   (message save-buffer)
	   (with-current-buffer save-buffer
		 (call-interactively 'vc-next-action)
		 )
	   )
	)
  )

(add-hook 'after-save-hook 'my-commit-on-save)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c b") 'helm-projectile)
(setq projectile-project-search-path '("~"))

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(elpy-enable)
(setq elpy-rpc-python-command "python3"
      python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")
(define-key elpy-mode-map (kbd "C-c k") 'eply-shell-kill)
;; hooks
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode t)
            (setq-default tab-width 4)
            (setq-default py-indent-tabs-mode t)
            ))
;; python shell hook
(add-hook 'inferior-python-mode-hook (beacon-mode 0))

(defconst pyenv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v") 'pyenv-mode-set)
    (define-key map (kbd "C-c u") 'pyenv-mode-unset)
    map)
  "Keymap for pyenv-mode.")

(pyenv-mode)

(require 'pyenv-mode)
(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;;js2
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;;js2-refractor
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m") ;; eg. extract function with `C-c C-m ef`
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;;xref-js2 TODO : make these p5 projets VC projects (git, svn, etc.)
(require 'xref-js2)
(define-key js2-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(setq sgml-set-face t)
(setq sgml-auto-activate-dtd t)
(setq sgml-indent-data t)

;; Babel
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(defun toogle-src-fonts ()
  "Set native fonts for src blocks or leave it grey"
  (interactive)
  ((lambda
     (if (org-src-fontify-natively)
         (setq org-src-fontify-natively nil)
       (setq org-src-fontify-natively t)
       ))))

(defun org-babel-split-block-maybe (&optional arg)
  "Split the current source code block on the cursor."
  (interactive "p")
  ((lambda (info)
     (if info
         (let ((lang (nth 0 info))
               (indent (nth 6 info))
               (stars (make-string (org-current-level) ?*)))
           (insert (concat (if (looking-at "^") "" "\n")
                           (make-string indent ? ) "#+end_src\n"
                           (if arg stars (make-string indent ? )) "\n"
                           (make-string indent ? ) "#+begin_src " lang
                           (if (looking-at "[\n\r]") "" "\n  "))))
       (message "Not in src block.")))
   (org-babel-get-src-block-info)))

(require 'ob-async)

(require 'ob-ipython)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   ;; other languages..
   ))

;; add provided completion backend for company 
(add-to-list 'company-backends 'company-ob-ipython) 

;; display inline images after eval
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; for latex exports
;; (add-to-list 'org-latex-minted-langs '(ipython "python"))

;; to enable jupyter-console
(setq python-shell-completion-native-enable nil)

;; avoid warning messages to pop up

;; hack around json readtable error
;; (advice-add 'ob-ipython--collect-json :before
;;             (lambda (&rest args)
;;               (when (re-search-forward "{" nil t)
;;                 (backward-char))))

(require 'ox-ipynb)

(defun ox-ipynb-export()
  (interactive)
  (ox-ipynb-export-to-buffer)
  (save-buffer)
  (kill-buffer (buffer-name))
  )

;; To use the python lexer for ipython blocks, add this setting:

(add-to-list 'org-latex-minted-langs '(ipython "python"))

(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)))

(fset 'translate-org-header-in-src-block
   [?\C-  ?\C-a C-right C-left ?\M-w ?\C-e return ?< ?t ?r tab ?\C-! ?\C-c ?\C-v ?\C-e])
(define-key teddd-map (kbd "t") 'ob-translate-org-header-in-src-block)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

(setq org-latex-create-formula-image-program 'dvipng)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

(require 'ob-screen)
(defvar org-babel-default-header-args:screen
'((:results . "silent") (:session . "default") (:cmd . "zsh") (:terminal . "st"))
"Default arguments to use when running screen source blocks.")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

(setq woman-use-topic-at-point t)

(add-to-list 'Info-default-directory-list "~/code/info")

(add-hook 'doc-view-minor-mode-hook (lambda () (visible-mark-mode nil)))

(auto-image-file-mode 1)

(setq elfeed-feeds
      '("https://www.europeandataportal.eu/en/rss/articles.xml"
        "http://www.datatau.com/rss"
        "http://www.kdnuggets.com/feed"
        "https://dssg.uchicago.edu/feed"
        "http://blog.revolutionanalytics.com/atom.xml"
        "https://blog.mailchimp.com/tag/data-science/feed"
        "http://datascience.ibm.com/blog/rss"
        "http://blog.kaggle.com/category/data-science-news/feed"
        "https://blog.zhaw.ch/datascience/feed"
        "http://www.unofficialgoogledatascience.com/feeds/posts/default"
        "http://dataskeptic.com/feed.rss"
        "https://dataelixir.com/issues.rss"
        "http://www.polipsych.com/feed"
        ))

(require 'foxdot-mode)

(defvar foxdot-cli-path "/home/teddd/.pyenv/versions/music/lib/python3.5/site-packages/FoxDot/")

(load "auctex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-to-list 'load-path "~/.emacs.d/icicles")
(require 'icicles)

(add-to-list 'load-path "~/.emacs.d/bookmark-plus/bookmark-plus")
(require 'bookmark+)
