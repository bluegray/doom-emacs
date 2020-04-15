;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq
 doom-font
 (font-spec :family "Ubuntu Mono"
            :size (if (string= (getenv "COMPUTER") "mbp") 26 14))
 doom-variable-pitch-font
 (font-spec :family "Ubuntu Mono"
            :size (if (string= (getenv "COMPUTER") "mbp") 24 14)))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;; Custom doom configuration

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")

(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing lines-tail))
(global-whitespace-mode +1)
(setq-default fill-column 90)
(setq require-final-newline t)

(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-progressive-speed 10) ;; on a long mouse scroll keep scrolling by 1 line

(use-package! doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-city-lights t)
  ;;(load-theme 'doom-nord t)
  ;;(load-theme 'doom-spacegrey t)
  ;;(load-theme 'doom-tomorrow-night t)
  ;;(load-theme 'bluegray t)
  ;;(load-theme 'doom-vibrant t)
  (load-theme 'bluegray-city t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))


;; Global keybinds

(cua-mode t)  ;; C-c, C-v, C-z for copy, paste, undo
(map! :map undo-tree-map
      "C-y" #'undo-tree-redo)

(map! "C-S-t"         #'+treemacs/toggle
      "<f2>"          #'highlight-symbol-at-point
      "<f3>"          #'highlight-symbol-next
      "C-M-q"         #'clojure-align
      "C-<up>"        #'+fold/toggle
      "C-<down>"      #'+fold/open-all
      "C-S-M-<up>"    #'+fold/close-all
      "C-c C-<right>" #'tagedit-forward-slurp-tag
      "C-c C-<left>"  #'tagedit-forward-barf-tag
      "C-<f4>"        #'counsel-colors-web
      "C-S-M-w"       #'global-whitespace-mode
      "C-x w"         #'whitespace-cleanup)


;; Smartparens

(add-hook! scss-mode #'smartparens-strict-mode)
(add-hook! clojure-mode #'turn-off-smartparens-mode)
(add-hook! clojure-mode #'turn-off-smartparens-strict-mode)
(map! :after smartparens
      :map   smartparens-mode-map
      "C-<right>" #'sp-forward-slurp-sexp
      "C-<left>"  #'sp-forward-barf-sexp)


;;Paredit

(use-package! paredit
  :hook clojure-mode
  :config
  (map!
   "C-<right>" #'paredit-forward-slurp-sexp
   "C-<left>"  #'paredit-forward-barf-sexp))
(add-hook! clojure-mode #'paredit-mode)
(add-hook! emacs-lisp-mode #'paredit-mode)


;; Cider

(defun new-cider-local-repl  () (interactive) (cider-connect '(:host "localhost" :port 9991)))
(defun new-cider-tunnel-repl () (interactive) (cider-connect '(:host "localhost" :port 7888)))
(map! "<f9>"       #'new-cider-local-repl)
(map! "S-C-M-<f9>" #'new-cider-tunnel-repl)
(map! "<f10>"      #'cider-connect-clj&cljs)
(map! "C-S-<f9>"   #'cider-quit)


;; Misc editor keybinds

(map! "C-<f8>"
      (defun indent-buffer ()
        (interactive)
        (save-excursion
          (indent-region (point-min) (point-max)))))

(map! "C-SPC"
      (defun multi-line-just-one-space (&optional n)
        "Multi-line version of `just-one-space': Delete all
         spaces and tabs around point, leaving one space (or N
         spaces). When in clojure or emacs lisp mode, re-indents
         the s-expression."
        (interactive "*p")
        (let ((orig-pos (point)))
          (skip-chars-backward " \t\n")
          (constrain-to-field nil orig-pos)
          (dotimes (_ (or n 1))
            (if (= (following-char) ?\s)
                (forward-char 1)
              (insert ?\s)))
          (delete-region
           (point)
           (progn
             (skip-chars-forward " \t\n")
             (constrain-to-field nil orig-pos t))))
        (when (or (eq major-mode 'clojure-mode)
                  (eq major-mode 'emacs-lisp-mode))
          (indent-sexp))))

(map! "C-<f1>"
      (defun describe-char-disable-hl-line ()
        (interactive)
        (hl-line-mode -1)
        (describe-char (point))))


;; Misc hooks

(add-hook! after-save
  (defun clojure-maybe-compile-and-load-file ()
    "Call function 'cider-load-buffer' for clojure files.
     Meant to be used in `after-save-hook'."
    (when (and (or (eq major-mode 'clojurec-mode) (eq major-mode 'clojure-mode))
               ;;(not (string-match ".*\\(project\\|profiles\\)\.clj$" buffer-file-name))
               ;;(not (string-match "^.*\.cljs$" buffer-file-name))
               )
      (cider-load-buffer))))


;; Clojure mode

(after! clojure-mode
  ;; Custom clojure indentation
  (define-clojure-indent
    ;; compojure
    (context 'defun)
    (GET 'defun)
    (POST 'defun)
    ;; component
    (start 'defun)
    (stop 'defun)
    (init 'defun)
    (db 'defun)
    (conn 'defun)
    ;; datalog
    (and-join 'defun)
    (or-join 'defun)
    (not-join 'defun)
    ;; tufte
    (tufte/p 'defun)
    ;;re-frame
    (rf/reg-event-db 'defun)
    (rf/reg-event-fx 'defun)
    (rf/reg-sub 'defun)
    (rf/reg-fx 'defun))
  (setq clojure-align-forms-automatically t))

(after! clj-refactor
  (map! :map clj-refactor-map
        "<f8>" (defun clean-ns () (interactive) (cljr-clean-ns)))
  (setq cljr-favor-prefix-notation nil))

(add-hook! clojure-mode
  (defun indent-on-newline ()
    (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))
  (defun clj-refactor-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m")))


;; Flycheck

(use-package! flycheck-joker
  :after clojure-mode
  :config
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

  (dolist (checkers '((clj-kondo-clj . clojure-joker)
                      (clj-kondo-cljs . clojurescript-joker)
                      (clj-kondo-cljc . clojure-joker)
                      (clj-kondo-edn . edn-joker)))
    (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))

(add-hook! scss-mode
  (setq flycheck-checker 'scss-stylelint
        flycheck-stylelintrc "~/.stylelintrc.json"
        posframe-mouse-banish nil))


;; Web and css configuration

(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      css-indent-offset 2)

(after! web-mode
  (set-formatter! 'html-tidy
    '("tidy" "-q" "-indent"
      "-wrap" "100"
      "--indent-spaces" "2")
    :ok-statuses '(0 1)))


;; Misc package configuration

(after! doom-modeline (setq doom-modeline-minor-modes nil))

(after! emmet-mode
  (map! :map emmet-mode-keymap
        [tab] #'+web/indent-or-yas-or-emmet-expand))

(after! tagedit (tagedit-add-experimental-features))

(use-package! centaur-tabs
  :init
  (setq centaur-tabs-set-bar 'over)
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "⨯"
        centaur-tabs-modified-marker "⬤")
  (centaur-tabs-group-by-projectile-project)
  (map! "C-S-M-t" #'centaur-tabs-counsel-switch-group))


;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "#ff8888")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "#c4f2ff")
(setq djcb-normal-cursor-type    'bar)

(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color djcb-read-only-color)
    (setq cursor-type djcb-read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color djcb-overwrite-color)
    (setq cursor-type djcb-overwrite-cursor-type))
   (t
    (set-cursor-color djcb-normal-color)
    (setq cursor-type djcb-normal-cursor-type))))
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)
