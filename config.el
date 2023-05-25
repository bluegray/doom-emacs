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
 (font-spec :family (if (string= (getenv "COMPUTER") "mbp") "Ubuntu Mono" "ProggyCleanTTSZ")
            :size   (if (string= (getenv "COMPUTER") "mbp") 26 16))
 doom-variable-pitch-font
 (font-spec :family (if (string= (getenv "COMPUTER") "mbp") "Ubuntu Mono" "DejaVu Sans Mono")
            :size   (if (string= (getenv "COMPUTER") "mbp") 24 12))
 doom-big-font
 (font-spec :family (if (string= (getenv "COMPUTER") "mbp") "Ubuntu Mono" "DejaVu Sans Mono")
            :size   (if (string= (getenv "COMPUTER") "mbp") 30 16)))
(after! doom-themes
  (setq doom-themes-enable-bold t))


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

(setq whitespace-style
      '(face tabs tab-mark spaces space-mark trailing missing-newline-at-eof lines-tail empty))
(global-whitespace-mode +1)
(setq-default fill-column 100)
(setq require-final-newline t)

;;(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
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
      "<f2>"          #'highlight-symbol-at-point ;; #'hlt-highlight-symbol
      "<f3>"          #'highlight-symbol-next     ;; #'hlt-next-highlight
      "C-M-q"         #'clojure-align
      "C-<up>"        #'+fold/toggle
      "C-<down>"      #'+fold/open-all
      "C-S-M-<up>"    #'+fold/close-all
      "C-c C-<right>" #'tagedit-forward-slurp-tag
      "C-c C-<left>"  #'tagedit-forward-barf-tag
      "C-<f4>"        #'counsel-colors-web
      "C-S-M-w"       #'global-whitespace-mode
      "C-x w"         #'whitespace-cleanup
      "C-c C-c"       #'kill-ring-save)

(map! "C-<f1>"
      (defun describe-char-disable-hl-line ()
        (interactive)
        (hl-line-mode -1)
        (describe-char (point))))


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
  :config
  (map!
   "C-<right>" #'paredit-forward-slurp-sexp
   "C-<left>"  #'paredit-forward-barf-sexp))
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook! clojure-mode #'paredit-mode)
(add-hook! emacs-lisp-mode #'paredit-mode)
(add-hook! cider-repl-mode-hook #'paredit-mode)


;; Cider

(use-package! cider
  :init
  (setq
   ;;cider-print-fn 'pr
   cider-print-fn 'puget
   ;;cider-print-fn 'fipp
   ;;cider-print-fn 'pprint
   ;;cider-print-fn 'zprint
   ;;cider-font-lock-dynamically t
   ;;cider-overlays-use-font-lock t
   cider-result-overlay-position 'at-eol
   cider-repl-pop-to-buffer-on-connect 'display-only
   cider-repl-display-in-current-window nil
   ;;cider-repl-use-clojure-font-lock t
   cider-use-fringe-indicators t
   cider-print-options '(("length"       500) ("right-margin" 80)
                         ("print-length" 500) ("width"        80)
                         ("max-length"   500) ("max-depth"     4)
                         ("map-delimiter" ""))
   cider-known-endpoints
   '(("tunnel" "127.0.0.1" "7888")
     ("local"  "127.0.0.1" "9991"))))

(defun new-cider-local-mono-repl  () (interactive)
       (cider-connect '(:host "localhost" :port 9991)))
(defun new-cider-local-chat-repl  () (interactive)
       (cider-connect '(:host "localhost" :port 7999)))
(defun new-cider-tunnel-mono-repl () (interactive)
       (cider-connect '(:host "localhost" :port 7888)))
(defun new-shadow-mono-repl () (interactive)
       (cider-connect-clj&cljs '(:host "localhost" :port 9991 :cljs-repl-type shadow)))
(defun new-shadow-chat-repl () (interactive)
       (cider-connect-clj&cljs '(:host "localhost" :port 7999 :cljs-repl-type shadow)))
(map! "<f9>"       #'new-cider-local-mono-repl)
(map! "S-<f9>"     #'new-shadow-mono-repl)
(map! "S-C-M-<f9>" #'new-cider-tunnel-mono-repl)
(map! "<f10>"      #'new-cider-local-chat-repl)
(map! "S-<f10>"    #'new-shadow-chat-repl)
(map! "<f11>"      #'cider-connect-clj&cljs)
(map! "<f12>"      #'cider-quit)
(map! "C-c C-w"    #'cider-pprint-eval-last-sexp-to-comment)

(map! "C-<f8>"
      (defun indent-buffer ()
        (interactive)
        (save-excursion
          (indent-region (point-min) (point-max)))
        (when (or (eq major-mode 'clojure-mode)
                  (eq major-mode 'emacs-lisp-mode))
          (cider-format-buffer))))

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
          (cider-format-defun))))

;; Formatting wih cljfmt via cider
;; https://docs.cider.mx/cider/usage/misc_features.html#formatting-code-with-cljfmt
(map! "S-C-M-<f8>" #'lsp-format-buffer)

(defun clojure-maybe-save-and-format ()
  (when (and (or (eq major-mode 'clojurec-mode)
                 (eq major-mode 'clojure-mode))
             (not (string-match "^.*\.edn$" buffer-file-name)))
    (delete-trailing-whitespace)
    (lsp-format-buffer)))
(add-hook! before-save #'clojure-maybe-save-and-format)

(defun clojure-maybe-compile-and-load-file ()
    "Call function 'cider-load-buffer' for clojure files.
     Meant to be used in `after-save-hook'."
    (when (and (or (eq major-mode 'clojurec-mode)
                   (eq major-mode 'clojure-mode))
               (not (string-match "^.*\.edn$" buffer-file-name)))
      (cider-load-buffer)))
(add-hook! after-save #'clojure-maybe-compile-and-load-file)


;; Clojure mode

(after! clojure-mode
  (setq clojure-align-forms-automatically t)
  (map! "C-M-q" #'clojure-align))

(after! clj-refactor
  (map! :map clj-refactor-map
        "<f8>" (defun clean-ns () (interactive) (cljr-clean-ns)))
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-insert-newline-after-require nil))

(add-hook! clojure-mode
  'lsp
  (defun indent-on-newline ()
    (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))
  (defun clj-refactor-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (hl-line-mode -1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m")))

(setq lsp-enable-symbol-highlighting nil)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-lens-enable t)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-diagnostics nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-diagnostics-provider :none)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-completion-provider :none)
(setq lsp-completion-show-detail nil)
(setq lsp-completion-show-kind nil)

;; Flycheck

(use-package! flycheck-clj-kondo
  :after clojure-mode)

(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; Define new checker compatible with Stylelint 14.0.0
(flycheck-define-checker scss-stylelint-v14
  "A SCSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            ;; Stylelint 14.0.0 removed the syntax option
            ;; https://stylelint.io/migration-guide/to-14
            ;; "--syntax" "scss"
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :modes (scss-mode))

(add-hook! scss-mode
  (setq flycheck-checker 'scss-stylelint-v14
        ;;flycheck-stylelintrc "~/.stylelintrc.json"
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
    :ok-statuses '(0 1))
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq web-mode-enable-current-element-highlight t)
  (set-face-background 'web-mode-current-element-highlight-face "#666"))


;; js2-mode

(add-hook! js2-mode
  (setq js2-basic-offset 2))


;; Misc package configuration

(after! doom-modeline
  (setq doom-modeline-minor-modes t
        doom-modeline-bar-width 5
        doom-modeline-height 1)
  (set-face-attribute 'mode-line          nil
                      :family (if (string= (getenv "COMPUTER") "mbp") "Ubuntu Mono" "ProggyCleanTTSZ")
                      :height 90)
  (set-face-attribute 'mode-line-inactive nil
                      :family (if (string= (getenv "COMPUTER") "mbp") "Ubuntu Mono" "ProggyCleanTTSZ")
                      :height 90)
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  ")))

;; (after! emmet-mode
;;   (map! :map emmet-mode-keymap
;;         [tab] #'+web/indent-or-yas-or-emmet-expand))

(after! tagedit (tagedit-add-experimental-features))

(use-package! centaur-tabs
  :init
  (setq centaur-tabs-set-bar 'over)
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-style "bar"
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "⨯"
        centaur-tabs-modified-marker "⬤"
        centaur-tabs-adjust-buffer-order t
        centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-change-fonts "Ubuntu Mono" 110)
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

(use-package! minions
  :config (minions-mode 1))

;;TODO: Try this to increase performance
(setq inhibit-compacting-font-caches t)

(add-hook! json-mode
  (setq tab-width 2))

;; https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(add-to-list `auto-mode-alist '("\\.svg\\'" . xml-mode))

(setq enable-local-variables :safe)

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-q") 'clojure-align)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)
