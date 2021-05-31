(require 'package)
(setq package-archives
      '(
        ;("marmalade" . "http://marmalade-repo.org/packages/")
        ;("elpa" . "http://tromey.com/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ))
(setq package-enable-at-startup nil)
(package-initialize)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq load-prefer-newer t)
(async-bytecomp-package-mode 1)

(define-prefix-command 'teddd-map)

(global-set-key (kbd "C-ù") 'teddd-map)

(define-key teddd-map (kbd "d") 'server-start)

(define-key teddd-map (kbd "g") 'customize-group)

(define-key teddd-map (kbd "n") 'make-frame)

(define-key teddd-map (kbd "R") 'eval-region)

(define-key teddd-map (kbd "f") 'customize-apropos-faces)

(define-key teddd-map (kbd "p") 'list-packages)

(define-key teddd-map (kbd "=") 'describe-char)
(define-key teddd-map (kbd "<mouse-1>") 'describe-char)

(define-key teddd-map (kbd "C-x C-c") 'save-buffers-kill-emacs)

(define-key teddd-map (kbd "s") 'ispell-region)

(define-key teddd-map (kbd "<tab>") 'show-two-children)

(defun read-mode()
  (interactive)
  (delete-other-windows)
  (multicolumn-split)
  (follow-mode 1)
  (next-line))

(put 'scroll-left 'disabled nil)
(define-key teddd-map (kbd "<left>") (lambda () (interactive) (scroll-right 3 0)))
(define-key teddd-map (kbd "<right>") (lambda () (interactive) (scroll-left 3 0)))

(define-key teddd-map (kbd "k") 'kill-whole-line)

(defun flush-empty-lines()
  (interactive)
  (mark-whole-buffer)
  (flush-lines "^$"))
(define-key teddd-map (kbd "<backspace>") 'flush-empty-lines)

(define-key teddd-map (kbd "y") 'yas-describe-tables)

;; (defun revert-buffer-force()
;;   (interactive)
;;   (condition-case nil
;; 	  (with-simulated-input "y RET" (revert-buffer))
;; 	(revert-buffer)
;; 	)
;;   )
(define-key teddd-map (kbd "<f5>") 'revert-buffer)

(defun find-zshrc ()
  (interactive)
  (find-file "~/.zshrc"))
(define-key teddd-map (kbd "z") 'find-zshrc)

(defun facebook-events-open-browser()
  (interactive)
  (browse-url "https://www.facebook.com/events/calendar/"))
(define-key teddd-map (kbd "F") 'facebook-events-open-browser)

;; (defun find-i3-config ()
;;   (interactive)
;;   (find-file "~/.config/i3/config"))
;;(define-key teddd-map (kbd "i") 'find-i3-config)

(defun find-bash-scripts ()
  (interactive)
  (helm-find-files-1 "~/code/bash/"))
(define-key teddd-map (kbd "b") 'find-bash-scripts)

(define-key teddd-map (kbd "x") 'external-term)

(define-key teddd-map (kbd "<up>") 'org-babel-previous-src-block)
(define-key teddd-map (kbd "<down>") 'org-babel-next-src-block)

;; (defun ob-comment-uncomment()
;;   (interactive)
;;   (org-edit-special)
;;   (comment-or-uncomment-region)
;;   (org-edit-src-exit)
;;   )

(fset 'ob-toogle-comment
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("''" 0 "%d")) arg)))
(define-key teddd-map (kbd "C-c") 'ob-toogle-comment)

;; Comment / Uncomment Region
(global-set-key "\C-c\C-x\C-c" 'comment-or-uncomment-region)
;; comment line
(global-set-key "\C-c\C-x\C-c" 'comment-line)

;; auto-revert-mode
(global-set-key (kbd "C-x <f5>") 'auto-revert-mode)

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

(global-set-key (kbd "<C-S-mouse-4>") 'previous-buffer)
(global-set-key (kbd "<C-S-mouse-5>") 'next-buffer)

(global-set-key (kbd "M-z") 'undo-tree-redo)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; emacs init file keybinding
(defun find-init ()
  (interactive)
  (find-file init-file))
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

(require 'epa-file)
(epa-file-enable)

(defun open-transfer-image-first()
  (interactive)
  (if-let* ((transfer-dir "~/Dropbox/Transfer/")
			(first-image (car (seq-filter (lambda (f) (image-type-available-p (image-type f)))
										  (cddr (directory-files transfer-dir))))))
	  (call-process "xdg-open" nil 0 nil (expand-file-name (concat transfer-dir first-image)))
	(call-process "xdg-open" nil 0 nil (expand-file-name transfer-dir))
	)
  )

(define-key teddd-map (kbd "t") 'open-transfer-image-first)

(defun caja-open-directory()
  "Open directory of current buffer's file-name with caja"
  (interactive)
  (if (eq major-mode 'dired-mode)
	  (call-process "caja" nil 0 nil (file-name-directory (dired-current-directory)))
	(call-process "caja" nil 0 nil (file-name-directory (buffer-file-name)))
	  )
  )

(define-key teddd-map (kbd "X") 'caja-open-directory)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

(add-to-list 'load-path              "~/.emacs.d/mystery-theme.el/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/mystery-theme.el")

(setq dired-listing-switches "-aslh")

(defun external-term()
  (interactive)
  (start-process "Terminal" nil "mate-terminal")
  )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;(sml-modeline-mode 1)

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

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-/")  'helm-select-action) ; list actions using C-z
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
     (define-key company-active-map (kbd "C-:") 'helm-company)
     ))
;; (add-to-list '(helm-completing-read-handlers-alist) '(find-file))

(helm-flx-mode +1)

(define-key helm-map (kbd "<left>") 'helm-previous-source)
(define-key helm-map (kbd "<right>") 'helm-next-source)
;; for helm-find-files
(customize-set-variable 'helm-ff-lynx-style-map t)
;; for helm-imenu
(customize-set-variable 'helm-imenu-lynx-style-map t)
;; for semantic
(customize-set-variable 'helm-semantic-lynx-style-map t)
;; for helm-occur
(customize-set-variable 'helm-occur-use-ioccur-style-keys t)
;; for helm-grep
(customize-set-variable 'helm-grep-use-ioccur-style-keys t)

(require 'helm-source)

(setq mouse-autoselect-window t)

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-;") 'xref-find-definitions-other-window)

;; package menu hook to highlight curent line
(add-hook 'package-menu-mode-hook 'hl-line-mode)
;; smartscan : look for next expression like the one under the cursor
(smartscan-mode 1)

;; beacon, highlight cursor when windows scroll
;(beacon-mode 1)

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

;; access global mark ring
(global-set-key (kbd "C-c SPC") 'helm-all-mark-rings)

;; Move point through buffer-undo-list positions
(global-set-key (kbd "C-c z") 'goto-last-change)

; C-s in a buffer: open helm-swoop with empty search field
(global-set-key (kbd "C-S-s") 'helm-swoop)
(with-eval-after-load 'helm-swoop
    (setq helm-swoop-pre-input-function
        (lambda () nil)))

;; C-s in helm-swoop with empty search field: activate previous search.
;; C-s in helm-swoop with non-empty search field: go to next match.
(with-eval-after-load 'helm-swoop
    (define-key helm-swoop-map (kbd "C-S-s") 'tl/helm-swoop-C-s))

(defun teddd/helm-swoop-C-S-s ()
    (interactive)
    (if (boundp 'helm-swoop-pattern)
            (if (equal helm-swoop-pattern "")
                    (previous-history-element 1)
                (helm-next-line))
    (helm-next-line)
    ))

(require 'highlight-symbol)
(global-set-key (kbd "C-c %") 'highlight-symbol)
(global-set-key (kbd "C-c n") 'highlight-symbol-next)
(global-set-key (kbd "C-c p") 'highlight-symbol-prev)
(global-set-key (kbd "M-%") 'highlight-symbol-query-replace)

;; multiframe window function
;; (global-set-key (kbd "C-c <left>") 'previous-multiframe-window)
;; (global-set-key (kbd "C-c <right>") 'next-multiframe-window)

;; navigate through windows : wind move
;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))
(defun windmove-set-teddd-keybindings ()
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
(windmove-set-teddd-keybindings)

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

;; ORG MODE
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c .") 'org-time-stamp)
(setq org-log-done nil)
(add-hook 'org-mode-hook 'org-indent-mode)
;; retour à la ligne
;(add-hook 'org-mode-hook 'visual-line-mode)

;; Stuff found on John Wiegley blog http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/
(require 'org-agenda)

;; calendar mode navigation : show entries with TAB and jump to it with RET
(defun org-agenda-switch ()
  (define-key org-agenda-mode-map [(tab)] 'org-agenda-recenter)
  (define-key org-agenda-mode-map [(?\r)] 'org-agenda-goto))
(eval-after-load "org" '(org-agenda-switch))

;; elispse appearance
(setq org-ellipsis " (+)")

;; adjust subtree level to current point when yanking
(setq org-yank-adjusted-subtrees t)

;; inset newline when at end of window
(setq org-startup-truncated nil)

;; remove line separator when cycling
(setq org-cycle-separator-lines 0)

;; startup visibility
(setq org-startup-folded t)

(setq org-todo-keywords
      '((sequence "NEXT(n)" "PLAN(p)" "ASK(a)" "REVIEW(p)" "WAITING(w)" "|"
                  "DELEGATED(D)" "CANCELED(C)" "COMPLETED(c)" "DONE(d)")))

(setq org-tag-alist '(
					  ("assignements" . ?A)
					  ("tasks" . ?t)
					  ("learning" . ?l)
					  ("later" . ?L)
					  ("exams" . ?e)
					  ("rituals" . ?r)
					  ("activiés" . ?a)
					  ("details" . ?d)
					  ("uni" . ?u)
					  ("nocal" . ?n)
					  ("hot" . ?h)
					  ))

(setq org-tags-exclude-from-inheritance '("hot"))

(defun org-align-all-tags-fit-window()
    (progn (setq org-tags-column (max -80 (- 5 (window-body-width)))))
  (org-align-all-tags)
    )
(add-hook 'focus-in-hook 'org-align-all-tags-fit-window)
(add-hook 'focus-out-hook 'org-align-all-tags-fit-window)

(require 'org-auto-tangle)
(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(set-face-attribute 'org-block nil :background "gray11")
(set-face-attribute 'org-block-begin-line nil :foreground "#05ABF8" :underline t :extend nil)
(set-face-attribute 'org-block-end-line nil :foreground "#05ABF8" :overline t :underline nil :extend nil)

(defface org-org-framed-purple
  '((t :box (:line-width 1 :color "MediumPurple1" :style nil)
       ))
  "Face for framing."
  :group 'org-teddd-markup)

(defface org-framed-red
  '((t :box (:line-width 1 :color "Red" :style nil)
       ))
  "Face for framing."
  :group 'org-teddd-markup)

(defface org-framed-blue
  '((t :box (:line-width 1 :color "#05ABF8" :style nil)
       ))
  "Face for framing."
  :group 'org-teddd-markup)

(defface org-inverted
  '((t :inverse-video t
       ))
  "Face for inverting colors."
  :group 'org-teddd-markup)

(defface org-underline-red
  '((t :underline (:color "red")
       ))
  "Face for red underline."
  :group 'org-teddd-markup)

(defface org-underline-blue
  '((t :underline (:color "#05ABF8")
       ))
  "Face for blue underline."
  :group 'org-teddd-markup)

(defface org-underline-green
  '((t :underline (:color "#64bf78")
       ))
  "Face for blue underline."
  :group 'org-teddd-markup)

(setq org-emphasis-alist
      '(
        ("_" org-underline-red)
        ("/" org-underline-blue)
        ;("=" org-inverted)
        ("~" org-underline-green)
        ("+" org-framed-blue)
        ("-" org-framed-purple)
        ("*" org-framed-red)
        ))

(require 'org-treeusage)
(setq org-treescope-overlay-header nil
	  org-treeusage-overlay-usecolorbands nil)

(setq org-file-apps (cons '(directory . "xdg-open file://%s") org-file-apps))
;(setq org-file-apps (cons '("\\.pdf\\'" . "xdg-open file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.ods\\'" . "xdg-open file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.png\\'" . "gimp file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.jpg\\'" . "gimp file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.jpeg\\'" . "gimp file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.odt\\'" . "xdg-open file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.ggb\\'" . "xdg-open file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.djvu\\'" . "xdg-open file://%s") org-file-apps))
(setq org-file-apps (cons '("\\.epub\\'" . "xdg-open file://%s") org-file-apps))
(setq org-file-apps (butlast org-file-apps 1))

(require 'org-protocol)

(setq org-link-keep-stored-after-insertion nil)

(defun org-teddd-link-make-description (link desc)
  (cond
   ((string-prefix-p "file:" link) (file-name-base (file-name-base link)))
    ; other rules here
   (t desc)
    )
  )

(setq org-link-make-description-function #'org-teddd-link-make-description)

(global-set-key (kbd "C-x !") 'org-cliplink)

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

(defun backup-each-save-only-org (filename)
  (let ((file (file-name-nondirectory filename)))
    (cond
     ((string-match ".*\.org$" file) t)
     (t nil)
     ))
  )

(setq backup-each-save-filter-function 'backup-each-save-only-org)

(setq org-image-actual-width nil)
;;(setq org-image-actual-width 800)

(defun org-resize-image()
  """If on a link to a file, run resize command for images on the file."""
  (interactive)
  (let* ((context (org-element-context))
         (element-type (org-element-type context))
         (link-type (org-element-property :type context))
         (path (org-element-property :path context))
         (width 800)
         size
         )
    (if (eq element-type 'link)
        (if (string-equal link-type "file")
            (progn
              (setq size (read-string "Size: " (format "%sx" width)))
              (call-process "convert" nil nil nil
                            path "-resize"  size path)
              (message "Image %s resized to %s" (file-name-nondirectory path) size)
              )
          (message "Link at point is not a file link.")
          )
      (message "Not on a link.")))
  )

(setq org-cycle-include-plain-lists 'integrate)

(setq org-startup-with-latex-preview nil)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;; (setq org-preview-latex-default-process 'imagemagick)
;; (setq org-preview-latex-default-process 'dvipng)

(setq org-preview-latex-image-directory "~/.emacs.d/ltximg/")

;; latex document header settings
(setq org-format-latex-header-file "~/.emacs.d/.org-format-latex-header.tex")
(defun org-format-latex-header-reload ()
    """Rereads the content of the file containing the latex header"""
    (interactive)
    (setq org-format-latex-header (get-string-from-file org-format-latex-header-file))
    )
(org-format-latex-header-reload)

;; org-preview-latex-process-alist
(setq org-preview-latex-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvipng -D %D -T tight -o %O %f"))
    (dvisvgm
     :programs ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("convert -quiet -density %D -trim -antialias %f -quality 100 %O")))
  )

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-startup-with-inline-images nil)

(defun org-display-images-and-latex(beg end)
  "Display inline images and embedded latex between beg and end"
  (let ((range (abs (- beg end)))
        store)
    (when (> beg end) (setq store end
                            end beg
                            beg store))
    (message "Display range: %s." range)
    (org-display-inline-images t t beg end)
    (message "Rendered images.")
    (org--latex-preview-region beg end)
    (message "Rendered latex.")
    (message "Display complete.")
    ))

(defun org-display-images-and-latex-subtree-or-region()
  "Display inline images and embedded latex in active region or current subtree"
  (interactive)
  ;; check if in an org buffer
  (if (not (eq major-mode 'org-mode))
      (message "Not in an org buffer.")
    ;; if region is active display in it
    (if mark-active
        (org-display-images-and-latex (point) (mark))
      ;; if point on item of list, display in item and children
      (if (and (org-in-item-p) (= (line-beginning-position) (org-in-item-p)))
          (save-excursion
            (let ((foldedp (invisible-p (point-at-eol))))
              (when (not foldedp)
                (org-cycle))
              (goto-char (org-in-item-p))
              (org-mark-element)
              (org-display-images-and-latex (point) (mark))
              (deactivate-mark)
              (when (not foldedp)
                (org-cycle))))
        ;; else display in current subtree
        (save-excursion 
          (org-mark-subtree)
          (org-display-images-and-latex (point) (mark))
          (deactivate-mark))))))

(define-key teddd-map (kbd "i") 'org-display-images-and-latex-subtree-or-region)

(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-appear-autoemphasis t
      org-appear-autolinks t
      org-appear-autosubmarkers t
      org-appear-autoentities t)

(setq org-content-folder "~/Uni/orgcontent/")

(fset 'org-content-week-programm
   [C-return M-right ?V ?L C-return ?U ?E C-return ?T ?u ?t C-return ?H ?A])
(define-key teddd-map (kbd "v") 'org-content-week-programm)

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(setq anki-folder "~/.local/share/Anki2/User 1/")
(load-file "~/.emacs.d/dev/anki/anki-brain.el")
(define-key teddd-map (kbd "a") 'anki-brain-push-entry-at-pt)

(if (listp org-use-property-inheritance)
    (push "ANKI_DECK" org-use-property-inheritance)
    (setq org-use-property-inheritance (list "ANKI_DECK")))

(require 'org-download)
(setq-default org-download-image-dir "./Images")
(setq-default org-download-heading-lvl 0)
;; remove annotation
(setq org-download-annotate-function (lambda(_link) ""))


;; implement async in org-download source code https://github.com/jwiegley/emacs-async
(defun org-download-screenshot-if-org()
  "Only takes a screenshot when current buffer has org-mode as major mode"
  (interactive)
  (if (eq 'org-mode major-mode)
	  (org-download-screenshot)
	(message "Not in an org file.")
	  )
  )

(require 'german-holidays)
(setq calendar-holidays holiday-german-BE-holidays)
'(org-agenda-include-diary t)

(setq org-agenda-files '("~/Dropbox/org-mode/track.org"
                         "/home/teddd/GeometryGroup/geometry-group.org"))

;; recursively add org content files under "~/Uni/Kurse" to agenda
(setq org-agenda-files (append org-agenda-files (directory-files-recursively "~/Uni/Kurse" "\\.org$")))

(load-file "~/.emacs.d/dev/anki/anki-editor/anki-editor.el")
(setq anki-editor-create-decks t)

(add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . nil))
(add-to-list 'helm-completing-read-handlers-alist '(org-match-sparse-tree . nil))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/+DONE|+CANCELED" 'file))

(define-key org-mode-map (kbd "C-c C-$") 'org-archive-done-tasks)

(setq org-log-into-drawer t)
(setq org-log-done 'time)
(setq org-agenda-log-mode-items '(closed))

(setq org-speed-commands-user
      '(("P" . helm-org-parent-headings)
        ("m" . org-match-sparse-tree)
        ("!" . org-move-top)
        ("." . org-move-bottom)
        ("1" . (org-move-from-top 1))
        ("2" . (org-move-from-top 2))
        ("3" . (org-move-from-top 3))
        ("4" . (org-move-from-top 4))
        ("5" . (org-move-from-top 5))
        )
      )

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

(defun org-move-top()
  "Move current heading as high as possible in its hieracy level"
  (save-excursion
	(condition-case nil
		(while t
		  (org-metaup)
		  )
	  (error nil)))
  )

(defun org-move-bottom()
  "Move current heading as low as possible in its hieracy level"
  (save-excursion
	(condition-case nil
		(while t
          (org-metadown)
          )
      (error nil)))
  )

(defun org-move-from-top(n)
  "Move current heading at the n-th position starting from the top"
  (let ((i n))
    (save-excursion
      (org-move-top)
      (while (> i 1)
        (org-metadown)
        (setq i (1- i)))
      )
    )
  )

(defun org-move-from-bottom(n)
  "Move current heading at the n-th position starting from the bottom"
  (let ((i n))
    (save-excursion
      (org-move-bottom)
      (while (> i 1)
        (org-metaup)
        (setq i (1- i)))
      )
    )
  )

(defun org-map-subtree-visible (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading t)
  (let ((level (funcall outline-level)))
    (save-excursion
      ;(funcall fun)
      (while (and (progn
		    (outline-next-visible-heading 1)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-schedule-subtree-visible()
  (interactive)
  (setq org-schedule-tree-count 0)
  (org-map-subtree-visible 'org-schedule-subtree--schedule)
  )

(defun org-schedule-subtree--schedule()
  (org-schedule 0 (format "+%sd" org-schedule-tree-count))
  (setq org-schedule-tree-count (+ 1 org-schedule-tree-count))
  )

(define-key teddd-map (kbd "C-s") 'org-schedule-subtree-visible)

;; show 2 children headings
(defun show-two-children ()
  "Sows 2 levels of descendents of the active heading"
  (interactive)
  (outline-show-children 2))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(define-key org-read-date-minibuffer-local-map (kbd "<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<up>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<up>")  (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
(define-key org-read-date-minibuffer-local-map (kbd "S-<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))

(setq org-html-head (get-string-from-file "/home/teddd/.emacs.d/org-html-head.html"))

(require 'ox-json)

(org-reload)

(add-hook 'org-mode-hook 'org-indent-mode)

(fset 'org-make-link-at-point
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 1 91 left 67108896 5 93 3 24 22 3 24 22] 0 "%d")) arg)))
(define-key teddd-map (kbd "l") 'org-make-link-at-point)

(define-key teddd-map (kbd "<print>") 'org-download-screenshot-if-org)

(define-key teddd-map (kbd "*") 'org-table-iterate)

(fset 'org-beginning-of-headline
   "\C-c\C-b\C-c\C-f")
(define-key org-mode-map (kbd "C-c ù") 'org-beginning-of-headline)

(define-key org-mode-map (kbd "C-c <left>") 'windmove-left)
(define-key org-mode-map (kbd "C-c <up>") 'windmove-up)
(define-key org-mode-map (kbd "C-c <down>") 'windmove-down)
(define-key org-mode-map (kbd "C-c <right>") 'windmove-right)
(define-key org-mode-map (kbd "C-c <C-right>") 'org-shiftright)
(define-key org-mode-map (kbd "C-c <C-left>") 'org-shiftleft)

(define-key org-mode-map (kbd "C-c SPC") 'helm-all-mark-rings)
(define-key org-mode-map (kbd "C-c %") ''highlight-symbol)

(defun do-open-apostrophe ()
  "When called, insert insert a '"
  (interactive)
  (insert "'"))
(global-unset-key (kbd "'"))
(global-set-key (kbd "'") 'do-open-apostrophe)

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
  (kill-ring-save (line-beginning-position) (line-end-position))
  )
(global-set-key (kbd "C-x M-w") 'copy-line)

;; duplicate line
(defun duplicate-line()
  (interactive)
  (copy-line)
  (yank)
  )

;; yank with !
(global-set-key (kbd "C-!") 'yank)
;; company mode
(require 'company) 
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook #'company-statistics-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq company-global-modes '(not org-mode latex-mode LaTeX-mode))
;; (company-quickhelp-mode)
;; electricity
(electric-pair-mode 1)
(electric-quote-mode 1)
(electric-indent-mode 0)
;; move line
(global-set-key (kbd "M-<up>") 'move-text-line-up)
(global-set-key (kbd "M-<down>") 'move-text-line-down)
;; undo tree
(global-undo-tree-mode)
;(setq undo-tree-auto-save-history t)
;; indent
(global-set-key (kbd "RET") 'newline-and-indent)
;;(global-set-key (kbd "C-c i") 'indent-region)
;; highlight parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
;; auto indent
(require 'auto-indent-mode)
(advice-remove 'beginning-of-visual-line #'ad-Advice-move-beginning-of-line)
;; expand region
(global-set-key (kbd "C-$") 'er/expand-region)
;; embrace : expanded regions editing
(global-set-key (kbd "M-$") #'embrace-add)
(add-hook 'org-mode-hook #'embrace-org-mode-hook)
(delete-selection-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun modify-case-word-line-or-region(case-modif)
  (let ((beg (mark))
        (end (point))
        tmp)
    ;; if no active region, region to capitalize becomes line or word
    (if (region-active-p)
        (when (> beg end) (setq tmp end
                                end beg
                                beg tmp))
      ;; region to capitalize becomes word if on a word else it becomes line
      (if (and (string-match "[[:alnum:]]" (string (following-char))) (not (eobp)))
          (save-excursion (beginning-of-thing 'word)
                          (setq beg (point))
                          (end-of-thing 'word)
                          (setq end (point)))
        (setq beg (line-beginning-position)
              end (line-end-position)))      
      )
    ;; modify case of selected region
    (cond ((equal case-modif 'upcase) (upcase-region beg end))
          ((equal case-modif 'downcase) (downcase-region beg end))
          ((equal case-modif 'capitalize) (capitalize-region beg end))
          )
    )
  )

(defun upcase-word-line-or-region()
  (interactive)
  (modify-case-word-line-or-region 'upcase)
  )

(defun downcase-word-line-or-region()
  (interactive)
  (modify-case-word-line-or-region 'downcase)
  )

(defun capitalize-word-line-or-region()
  (interactive)
  (modify-case-word-line-or-region 'capitalize)
  )

(define-key teddd-map (kbd "u") 'upcase-word-line-or-region)
(define-key teddd-map (kbd "d") 'downcase-word-line-or-region)
(define-key teddd-map (kbd "c") 'capitalize-word-line-or-region)

;;(defun capitalize-last-word()
;;  (interactive)
;;  (capitalize-word -1))

(setq make-pointer-invisible t)

(global-set-key (kbd "C-M-%") 'plur-query-replace)
(global-set-key (kbd "C-s") 'plur-isearch-forward)

(require 'smart-hungry-delete)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(yas-global-mode)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-c <tab>") yas-maybe-expand)

(load-file "/home/teddd/.emacs.d/elpa/hideshow-org-20120223.2250/hideshow-org.el")

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(define-key hs-minor-mode-map [C-M-tab] 'toggle-selective-display)

(setq tab-stop-list (number-sequence 4 120 4))

(indent-guide-global-mode)

(require 'indent-tools)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)

(add-hook 'python-mode-hook
 (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body))
)

(global-subword-mode 1)

(setq magithub-feature-autoinject t)

(setq typescript-indent-level 2)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;(flycheck-mode +1)
  ;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
;(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun find-library-file (library)  "Takes a single argument LIBRARY, being a library file to search for.Searches for LIBRARY directly (in case relative to current directory,or absolute) and then searches directories in load-path in order.  Itwill test LIBRARY with no added extension, then with .el, and finallywith .elc.  If a file is found in the search, it is visited.  If noneis found, an error is signaled.  Note that order of extension searchingis reversed from that of the load function."  (interactive "sFind library file: ")  (let ((path (cons "" load-path)) exact match elc test found)    (while (and (not match) path)      (setq test (concat (car path) "/" library)            match (if (condition-case nil                          (file-readable-p test)                        (error nil))                      test)            path (cdr path)))    (setq path (cons "" load-path))    (or match        (while (and (not elc) path)          (setq test (concat (car path) "/" library ".elc")                elc (if (condition-case nil                            (file-readable-p test)                          (error nil))                        test)                path (cdr path))))    (setq path (cons "" load-path))    (while (and (not match) path)      (setq test (concat (car path) "/" library ".el")            match (if (condition-case nil                          (file-readable-p test)                        (error nil))                      test)            path (cdr path)))    (setq found (or match elc))    (if found        (progn          (find-file found)          (and match elc               (message "(library file %s exists)" elc)               (sit-for 1))          (message "Found library file %s" found))      (error "Library file \"%s\" not found." library))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode t)
            (setq-default tab-width 4)
            (setq-default py-indent-tabs-mode t)
			(setq-default python-indent-offset 4)
            ))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))

(setq python-shell-interpreter "ipython3"
	  python-shell-interpreter-args "-i --simple-prompt --pprint")

(add-hook 'python-mode-hook 'pyenv-mode)

(require 'pyenv-mode-auto)

(require 'pyenv-mode)
(define-key pyenv-mode-map (kbd "C-c C-s") nil)
(define-key pyenv-mode-map (kbd "C-c v") 'pyenv-mode-set)

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

(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

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

(setq org-ditaa-jar-path "/usr/bin/ditaa")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates dot

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

(setq ob-async-no-async-languages-alist '("jupyter-python"))

(setq woman-use-topic-at-point t)

(add-to-list 'Info-default-directory-list "~/code/info")

(require 'sclang)

(defun LaTeX-indent-section()
  """Indent the current section."""
  (interactive)
  (save-excursion
    (LaTeX-mark-section)
    (indent-region (point) (mark)))
  )

(defun teddd-extend-latex-paren-left-right()
  """Replace parens in selection with their corresponding latex left-right pairs"""
  (interactive)
  (let ((match-replace-list '(("(" "\\left(")
                              (")" "\\right)")
                              ("[" "\\left[")
                              ("]" "\\right]")
                              ("<" "\\left<")
                              (">" "\\right>")
                              ("\\{" "\\left\\{")
                              ("\\}" "\\right\\}"))))
    (dolist (match-replace match-replace-list)
      (save-excursion
        (perform-replace
         (nth 0 match-replace) (nth 1 match-replace) nil nil nil nil nil (point) (mark))))
    ))

(require 'latex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;;(setq-default TeX-master nil)
;;(setq LaTeX-math-abbrev-prefix (kbd "§"))
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(setq auto-revert-interval 1)

;; use emacs' usual paren system
(defun cdlatex-set-teddd-keybindings()
  "use emacs' usual paren system"
  (interactive)
  (define-key cdlatex-mode-map (kbd "$") 'self-insert-command)
  (define-key cdlatex-mode-map (kbd "(") 'self-insert-command)
  (define-key cdlatex-mode-map (kbd "[") 'self-insert-command)
  (define-key cdlatex-mode-map (kbd "{") 'self-insert-command))

(add-hook 'cdlatex-mode-hook 'cdlatex-set-teddd-keybindings)

;; FIX: indent with tab
;; (add-hook 'cdlatex-tab-hook 'indent-region)

;; defined with customize :
;; (setq cdlatex-math-modify-prefix "µ") ;; S-*
;; (setq cdlatex-math-symbol-prefix "§") ;; S-!

(setq cdlatex-command-alist
      '(
        ("pr" "Insert \\,||\\,"
         "\\,||\\,"  cdlatex-position-cursor nil nil t)
        ("fk" "Insert \\faktor{}{}"
         "\\faktor{?}{}"  cdlatex-position-cursor nil nil t)
        ("eq" "Insert \\equiv"
         "\\equiv"  cdlatex-position-cursor nil nil t)
        ("ab" "Insert ||"
         "|?|"  cdlatex-position-cursor nil nil t)
        ("sum"       "Insert \\sum_{}^{}"
         "\\sum_{?}^{}"  cdlatex-position-cursor nil nil t)
        ("sumu"       "Insert \\sum_{}"
         "\\sum_{?}"  cdlatex-position-cursor nil nil t)
        ("pro" "Insert \\prod_{}^{}"
         "\\prod_{?}^{}"  cdlatex-position-cursor nil nil t)
        ("prou" "Insert \\prod_{}"
         "\\prod_{?}"  cdlatex-position-cursor nil nil t)
        ("bra" "Insert \\llbracket \\rrbracket"
         "\\llbracket ? \\rrbracket" cdlatex-position-cursor nil nil t)
        ("b(" "Insert \\big( \\big)"
         "\\big( ? \\big" cdlatex-position-cursor nil nil t)
        ("B(" "Insert \\Big( \\Big)"
         "\\Big( ? \\Big" cdlatex-position-cursor nil nil t)
        ("b{" "Insert \\big\\{ \\big\\}"
         "\\big\\{ ? \\big\\" cdlatex-position-cursor nil nil t)
        ("B{" "Insert \\Big\\{ \\Big\\}"
         "\\Big\\{ ? \\Big\\" cdlatex-position-cursor nil nil t)
        ;; correct bug with < pairs
        ("lra" "Insert a \\left\\langle \\right\\rangle pair"
         "\\left\\langle ? \\right\\rangle" cdlatex-position-cursor nil nil t)
        ;("lr[" "Insert a \\left\[ \\right\] pair"
        ; "\\left\[ ? \\right\]" cdlatex-position-cursor nil nil t)
        ("tx" "Insert a \\text{}"
         "\\text{?}" cdlatex-position-cursor nil nil t)
        ("nm" "Insert a \\| \\|"
         "\\left\\| ? \\right\\|" cdlatex-position-cursor nil nil t)
        ("ca" "Insert \\bigcap\\limits_{}^{}"
         "\\bigcap\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
        ("cau" "Insert \\bigcap\\limits_{}^{}"
         "\\bigcap\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("cu" "Insert \\bigcup\\limits_{}^{}"
         "\\bigcup\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
        ("cuu" "Insert \\bigcup\\limits_{}"
         "\\bigcup\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("cud" "Insert \\bigcupdot\\limits_{}^{}"
         "\\bigcupdot\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
        ("tim" "Insert \\bigtimes\\limits_{}^{}"
         "\\bigtimes\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
        ("timu" "Insert \\bigtimes\\limits_{}"
         "\\bigtimes\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("lim" "Insert \\lim\\limits_{}"
         "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("lif" "Insert \\liminf\\limits_{}"
         "\\liminf\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("lis" "Insert \\limsup\\limits_{}"
         "\\limsup\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("inf" "Insert \\inf\\limits_{}"
         "\\inf\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("sup" "Insert \\sup\\limits_{}"
         "\\sup\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("ma" "Insert \\max"
         "\\max" cdlatex-position-cursor nil nil t)
        ("mi" "Insert \\min"
         "\\min" cdlatex-position-cursor nil nil t)
        ("ti" "Insert \\to \\infty"
         "\\to \\infty" cdlatex-position-cursor nil nil t)
        ("tz" "Insert \\to 0"
         "\\to 0" cdlatex-position-cursor nil nil t)
        ("we" "Insert \\bigwedge\\limits_{}"
         "\\bigwedge\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("ve" "Insert \\bigvee\\limits_{}"
         "\\bigvee\\limits_{?}" cdlatex-position-cursor nil nil t)
        ("me" "Insert \\{\\}"
         "\\{?\\}" cdlatex-position-cursor nil nil t)
        ("bm" "Insert \\begin{bmatrix} ? \\end{bmatrix}"
         "\\begin{bmatrix} ? \\end{bmatrix}" cdlatex-position-cursor nil nil t)
        ("pm" "Insert \\begin{pmatrix} ? \\end{pmatrix}"
         "\\begin{pmatrix} ? \\end{pmatrix}" cdlatex-position-cursor nil nil t)
        ("cs" "Insert \\begin{cases} ? \\end{cases}"
         "\\begin{cases} ? \\end{cases}" cdlatex-position-cursor nil nil t)
        ("le" "Insert \\leq"
         "\\leq" cdlatex-position-cursor nil nil t)
        ("ge" "Insert \\geq"
         "\\geq" cdlatex-position-cursor nil nil t)
        ("int" "Insert \\int\\limits_{}^{} \\,d"
         "\\int\\limits_{?}^{} \\,d"  cdlatex-position-cursor nil nil t)
        ("intm" "Insert \\int \\,d\\mu"
         "\\int ? \\,d\\mu" cdlatex-position-cursor nil nil t)
        ("intl" "Insert \\int\\limits_{} \\,d\\lambda"
         "\\int\\limits_{?} \\,d\\lambda" cdlatex-position-cursor nil nil t)
        ("ints" "Insert \\int\\limits_{} \\,dS"
         "\\int\\limits_{?} \\,dS" cdlatex-position-cursor nil nil t)
        ("intss" "Insert \\int\\limits_{} \\cdot\\,ds"
         "\\int\\limits_{?} \\cdot\\,ds" cdlatex-position-cursor nil nil t)
        ("intu" "Insert \\int\\limits_{} \\,d"
         "\\int\\limits_{?} \\,d" cdlatex-position-cursor nil nil t)
        ("intz" "Insert \\int \\,d"
         "\\int ? \\,d" cdlatex-position-cursor nil nil t)
        ("inr" "Insert \\in \\mathbb{R}"
         "\\in \\mathbb{R}" cdlatex-position-cursor nil nil t)
        ("inq" "Insert \\in \\mathbb{Q}"
         "\\in \\mathbb{Q}" cdlatex-position-cursor nil nil t)
        ("inn" "Insert \\in \\mathbb{N}"
         "\\in \\mathbb{N}" cdlatex-position-cursor nil nil t)
        ("inc" "Insert \\in \\mathbb{C}"
         "\\in \\mathbb{C}" cdlatex-position-cursor nil nil t)
        ("inz" "Insert \\in \\mathbb{Z}"
         "\\in \\mathbb{Z}" cdlatex-position-cursor nil nil t)
        ("or" "Insert \\lor"
         "\\lor" cdlatex-position-cursor nil nil t)
        ("an" "Insert \\land"
         "\\land" cdlatex-position-cursor nil nil t)
        ("mo" "Insert \\models"
         "\\models" cdlatex-position-cursor nil nil t)
        ("vd" "Insert \\vdash"
         "\\vdash" cdlatex-position-cursor nil nil t)
        ("gr" "Insert grad\\,"
         "grad\\," cdlatex-position-cursor nil nil t)
        ("di" "Insert div\\,"
         "div\\," cdlatex-position-cursor nil nil t)
        ("ro" "Insert rot\\,"
         "rot\\," cdlatex-position-cursor nil nil t)
        ("fl" "Insert \\floor{}"
         "\\floor{?}" cdlatex-position-cursor nil nil t)
        ))

(setq cdlatex-math-symbol-alist
      '(
        (?~ ("\\cong" "\\sim" "\\approx"))
        (?L ("\\Lambda" "\\limits"))
        (?> ("\\rightarrow" "\\longrightarrow" "\\rightrightarrows"))
        (?< ("\\leftarrow" "\\longleftarrow" "\\leftleftarrows"))
        (?1 ("^{-1}"))
        (?* ("\\times" "\\otimes"))
        (?v ("\\vee" "\\bigvee"))
        (?\& ("\\wedge" "\\bigwedge"))
        (?{ ("\\subseteq" "\\subset" ))
        (?} ("\\supseteq" "\\supset" ))
        (?n ("\\cap" "\\nu" "\\ln"))
        (?u ("\\cup" "\\upsilon"))
        (?: ("\\;\\middle|\\;"))
        (?! ("\\neq"))
        (?_ ("\\neg"))
        (?. ("\\cdot"))
        (?\; ("\\ldots"))
        (?f ("\\varphi" "\\phi"))
        (?F ("\\Phi"))
        (?e ("\\varepsilon" "\\epsilon" "\\exp"))
        (?x ("\\upchi" "\\chi"))
        (?E ("\\exists\\ " "" "\\ln"))
        (?° ("\\circ"))
        (?T ("\\top" "\\bot"))
        (?r  ("\\rho" "\\varrho"))
        ))

(setq cdlatex-math-modify-alist
      '(
        (?> "\\overrightarrow" "\\overrightarrow" t nil ni)
        (?B "\\mathbf" "\\textbf" t nil ni)
        (?b "\\mathbb" nil t nil nil)
        (?u "\\underset{}" nil t nil nil)
        (?o "\\overset{}" nil t nil nil)
        (?t "\\text" nil t nil nil)
        (?\_ "\\underline" "\\underline" t nil nil)
        (?\- "\\overline" "\\overline" t nil nil)
        ))

(define-key LaTeX-mode-map (kbd "C-c !") 'TeX-next-error)

(require 'calfw)

(require 'calfw-org)
(setq cfw:org-overwrite-default-keybinding t)
(setq cfw:org-agenda-schedule-args '(:scheduled :deadline))
(setq cfw:render-line-breaker 'cfw:render-line-breaker-none)

;; Unicode characters
(setq cfw:fchar-junction ?╋
      cfw:fchar-vertical-line ?┃
      cfw:fchar-horizontal-line ?━
      cfw:fchar-left-junction ?┣
      cfw:fchar-right-junction ?┫
      cfw:fchar-top-junction ?┯
      cfw:fchar-top-left-corner ?┏
      cfw:fchar-top-right-corner ?┓)

(setq cfw:face-item-separator-color "black")

(setq org-capture-templates
      (list '("c" "calfw2org" entry (file "/home/teddd/Dropbox/org-mode/track.org")  "* %? %(cfw:org-capture-day)"))
      )

(defun cfw:my-open-org-calendar ()
  "Open an org schedule calendar in the new buffer."
  (interactive)
  (save-excursion
    (let* ((source1 (cfw:org-create-source))
           (curr-keymap (if cfw:org-overwrite-default-keybinding cfw:org-custom-map cfw:org-schedule-map))
           (cp (cfw:create-calendar-component-buffer
                :view 'two-weeks
                :contents-sources (list source1)
                :custom-map curr-keymap
                :sorter 'cfw:org-schedule-sorter)))
      (switch-to-buffer (cfw:cp-get-buffer cp)))))

(define-key teddd-map (kbd "C") 'cfw:my-open-org-calendar)

;; launch at startup
;; (setq initial-buffer-choice 'cfw:open-org-calendar)

(defun cfw:org-action(action &rest args)
  "Perform an action on the corresponding headline in the org file"
  (let ((marker (get-text-property (point) 'org-marker)))
    (org-with-point-at marker
      (apply action args))))

(defun cfw:org-todo()
  "Run org-todo command on headline"
  (cfw:org-action (org-todo))
  )

(defun cfw:org-done()
  "Set current heading to DONE"
  (interactive)
  (cfw:org-action 'org-set-property "TODO" "DONE")
  )

(defun cfw:org-schedule()
  "Schedule current heading"
  (interactive)
  (cfw:org-action 'org-schedule 0)
  )

(defun cfw:org-schedule-remove()
  "Remove schedule from current heading"
  (interactive)
  (cfw:org-action 'org-schedule '(4))
  )

(defun cfw:org-deadline()
  "Add deadline to current heading"
  (interactive)
  (cfw:org-action 'org-deadline 0)
  )

(defun cfw:org-deadline-remove()
  "Remove deadline from current heading"
  (interactive)
  (cfw:org-action 'org-deadline '(4))
  )

(defun cfw:org-refile()
  "Refile current heading"
  (interactive)
  (cfw:org-action 'org-refile)
  )

(defun cfw:org-tags()
  "Set current heading tags"
  (interactive)
  (cfw:org-action 'org-set-tags-command)
  )

(defun cfw:org-macro()
  "Run last recorded macro on heading"
  (interactive)
  (cfw:org-action 'kmacro-end-and-call-macro nil)
  )

(define-key cfw:org-text-keymap (kbd "t") 'cfw:org-todo)
(define-key cfw:org-text-keymap (kbd "d") 'cfw:org-done)
(define-key cfw:org-text-keymap (kbd "C-c C-s") 'cfw:org-schedule)
(define-key cfw:org-text-keymap (kbd "C-c C-d") 'cfw:org-deadline)
(define-key cfw:org-text-keymap (kbd "C-c C-q") 'cfw:org-tags)
(define-key cfw:org-text-keymap (kbd "C-u C-c C-s") 'cfw:org-schedule-remove)
(define-key cfw:org-text-keymap (kbd "C-u C-c C-d") 'cfw:org-deadline-remove)
(define-key cfw:org-text-keymap (kbd "C-x C-e") 'cfw:org-macro)
(define-key cfw:org-text-keymap (kbd "w") 'cfw:org-refile)

;(load-file "/home/teddd/sandboxes/org-mode/calfw/code.el")
(load-file "/home/teddd/sandboxes/org-mode/calfw/code_new.el")

(defun org-speed-command-press-key(key)
  "Take key as simultated input for a speed command"
  (let ((org-speed-command (org-speed-command-activate key)))
	  (cond
	   ((commandp org-speed-command)
		(setq this-command org-speed-command)
		(call-interactively org-speed-command))
	   ((functionp org-speed-command)
		(funcall org-speed-command))
	   ((and org-speed-command (listp org-speed-command))
		(eval org-speed-command))
	   (t (let (org-use-speed-commands)
			(call-interactively 'org-self-insert-command))))
	  ))

(defun cfw:org-speed()
  "Set current heading to DONE"
  (interactive)
  (cfw:org-action 'org-speed-command-press-key (char-to-string (read-char)))
  )

(define-key cfw:org-text-keymap (kbd "!") 'cfw:org-speed)

(pdf-loader-install)
;(setq pdf-view-display-size 1.75)
;(setq pdf-view-midnight-colors '("#eeeeee" . "#000000"))
(setq pdf-view-bounding-box-margin 0.1)

(require 'org-pdftools)
(add-hook 'org-mode-hook 'org-pdftools-setup-link)

(require 'pdfgrep)
(pdfgrep-mode)

(require 'saveplace-pdf-view)
(save-place-mode 1)

(require 'image)
(auto-image-file-mode 1)
(define-key image-mode-map (kbd "W") 'image-transform-fit-to-width)
(define-key image-mode-map (kbd "H") 'image-transform-fit-to-height)

(message "End of init.el reached.")

(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
