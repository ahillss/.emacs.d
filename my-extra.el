
;;===common lisp with slime
(defun my-slime-switch-buffer ()
  (interactive)
  (let ((c (current-buffer)))
    (switch-to-buffer-other-window "*inferior-lisp*")
    (switch-to-buffer-other-window c)))

(defun my-slime-kill-buffer ()
  (interactive)
  (when (slime-connected-p) (slime-quit-lisp))
  (my-kill-buffers "*slime-events*" "*inferior-lisp*"))

(defun my-slime-after-load ()
  (setq slime-auto-connect 'always)
  (makunbound 'inferior-lisp-program)
  (defcustom inferior-lisp-program "lisp" "" :type 'string)
  (slime-setup '(slime-fancy)) )

(defun my-slime-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'slime-eval-buffer 'my-slime-switch-buffer)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'slime-eval-defun 'my-slime-switch-buffer)
  (my-local-set-key (kbd "<f7>") 'my-slime-kill-buffer) )

(defadvice my-slime-lisp-hook (around my-lisp-hook activate)
   ad-do-it
  (condition-case nil (slime-mode) (file-error nil)))

(add-hook 'slime-mode-hook 'my-slime-hook)
(autoload 'slime-mode "slime" "common lisp..." t)
(eval-after-load "slime" '(my-slime-after-load))

;;===lua
(defun my-lua-kill ()
  (interactive)
  (my-kill-buffers "*lua*"))

(defun my-lua-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'lua-send-buffer)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-region-line 'lua-send-region)
  (my-local-set-key (kbd "<f7>")
    'my-lua-kill))

(add-hook 'lua-mode-hook 'my-lua-hook)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;===java
(defun my-java-compile ()
  (interactive)
  (compile "ant -emacs compile -find" t))

(defun my-java-jar ()
  (interactive)
  (compile "ant -emacs jar -find" t))

(defun my-java-run ()
  (interactive)
  (compile "ant -emacs run -find" t))

(defun my-java-test ()
  (interactive)
  (compile "ant -emacs test -find" t))

(defun my-java-clean ()
  (interactive)
  (compile "ant -emacs clean -find" t))

(defun my-java-kill ()
  (interactive)
  (my-kill-buffers "*compilation*"))

(defun my-java-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-java-compile)
  (my-local-set-key (kbd "<f6>")
    'my-java-run)
  (my-local-set-key (kbd "<f7>")
    'my-java-kill)
  (my-local-set-key (kbd "<f8>")
    'my-java-clean))

(add-hook 'java-mode-hook 'my-java-hook)

;;===scala
(defun my-scala-start ()
  (interactive)
  (unless (get-buffer-process "*sbt*")
    (let ((c (current-buffer)))
      (async-shell-command "sbt" "*sbt*")
      (compilation-shell-minor-mode t)
      (switch-to-buffer-other-window "*sbt*")
      (switch-to-buffer-other-window c) )))

(defun my-scala-run ()
  (interactive)
  (comint-send-string "*sbt*" "run\n"))

(defun my-scala-compile ()
  (interactive)
  (comint-send-string "*sbt*" "compile\n"))

(defun my-scala-kill ()
  (interactive)
  (my-kill-buffers "*sbt*"))

(defun my-scala-hook ()
  (set (make-local-variable 'compilation-auto-jump-to-first-error) t)
  (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-scala-start 'my-scala-compile)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-scala-start 'my-scala-run)
  (my-local-set-key (kbd "<f7>")
    'my-scala-kill) )

(add-hook 'scala-mode-hook 'my-scala-hook)
(autoload 'scala-mode "scala-mode2" "scala..." t)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode2))

;;===haskell
(defun my-haskell-kill ()
  (interactive)
  (my-kill-buffers "*haskell*"))

(defun my-haskell-hook ()
  (turn-on-haskell-indentation)
  (local-unset-key  (kbd "C-x C-d"))
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'inferior-haskell-load-and-run
    'recenter-top-bottom 'other-window)
  (my-local-set-key (kbd "<f7>")
    'my-haskell-kill))

(add-hook 'haskell-mode-hook 'my-haskell-hook)
(autoload 'haskell-mode "inf-haskell" "haskell..." t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(eval-after-load "inf-haskell" '(require 'haskell-indentation nil t))
(eval-after-load "inf-haskell" '(require 'haskell-font-lock nil t))

;;===erlang
(let ((p (file-expand-wildcards "/usr/lib/erlang/lib/tools*")))
  (when p (add-to-list 'load-path (concat (car p) "/emacs"))))

(defun my-erlang-compile ()
  (interactive)
  (let ((c (current-buffer)))
    (call-interactively 'erlang-compile)
    (unless (eq c (current-buffer))
      (other-window))))

(defun my-erlang-run ()
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer)))
         (n (file-name-sans-extension (file-name-nondirectory fn))))
    ;; (inferior-erlang-send-command (concat n ":main()."))
    (comint-send-string "*erlang*" (concat n ":main().\n"))
    ))

(defun my-erlang-kill ()
  (interactive)
  (my-kill-buffers "*erlang*"))

(defun my-erlang-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-erlang-compile)
  (my-local-set-key (kbd "<f6>")
    'my-erlang-run)
  (my-local-set-key (kbd "<f7>")
    'my-erlang-kill))

(add-hook 'erlang-mode-hook 'my-erlang-hook)
(autoload 'erlang-mode "erlang-start" "erlang..." t)
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(setq inferior-erlang-machine-options '("-sname" "emacs"))

;;===ocaml
(defun my-ocaml-start ()
  (interactive)
  (save-excursion
    (tuareg-run-process-if-needed "ocaml")))

(defun my-ocaml-kill ()
  (interactive)
  (my-kill-buffers tuareg-interactive-buffer-name))

(defun my-ocaml-hook ()
  (auto-fill-mode 1)
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-ocaml-start 'tuareg-eval-buffer)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-ocaml-start 'my-region-line 'tuareg-eval-region)
  (my-local-set-key (kbd "<f7>")
    'tuareg-kill-ocaml 'my-ocaml-kill))

(add-hook 'tuareg-mode-hook 'my-ocaml-hook)
(setq tuareg-interactive-scroll-to-bottom-on-output t)
(setq tuareg-interactive-echo-phrase nil)
(setq tuareg-lazy-= t)
(setq tuareg-lazy-paren t)
(setq tuareg-in-indent 0)
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))

;;===ruby
(defun my-ruby-start ()
  (interactive)
  (let ((c (current-buffer)))
    (run-ruby ruby-program-name)
    (unless (eq (current-buffer) c)
      (other-window 1))))

(defun my-ruby-send-buffer ()
  (interactive)
  (ruby-send-region (point-min) (point-max)))

(defun my-ruby-kill ()
  (interactive)
  (my-kill-buffers "*ruby*"))

(defun my-ruby-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-ruby-start 'my-ruby-send-buffer)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-ruby-start 'my-region-line 'ruby-send-region)
  (my-local-set-key (kbd "<f7>")
    'my-ruby-kill))

(add-hook 'ruby-mode-hook 'my-ruby-hook)
(autoload 'ruby-mode "inf-ruby" "ruby..." t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

;;===cobol

(defun my-cobol-send-buffer ()
  (interactive)

  )

(defun my-cobol-kill ()
  (interactive)
  (my-kill-buffers "*cobol*"))

(defun my-cobol-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-cobol-send-buffer)
  (my-local-set-key (kbd "<f7>")
    'my-cobol-kill))

(add-hook 'cobol-mode-hook 'my-cobol-hook)
(autoload 'cobol-mode "cobol-mode" "Major mode for Tandem COBOL files." t nil)
(add-to-list 'auto-mode-alist '("\\.cbl$" . cobol-mode))
(add-to-list 'auto-mode-alist '("\\.cob$" . cobol-mode))

;;===php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;;===ragel
;;(defun my-ragel-hook ()
;;  (my-local-set-key (kbd "<f5>") 'save-buffer 'my-cpp-compile)
;;  (my-local-set-key (kbd "<f6>") 'save-buffer 'my-cpp-run)
;;  (my-local-set-key (kbd "<f7>") 'my-cpp-kill)
;;  (my-local-set-key (kbd "<f8>") 'my-cpp-clean))

;;(autoload 'ragel-mode "ragel-mode" "ragel..." t)
;;(add-hook 'ragel-mode-hook 'my-ragel-hook)
;;(add-to-list 'auto-mode-alist '("\\.rl$" . ragel-mode))

;;

(setq fci-rule-column 80)
(setq fci-rule-color "grey")
(setq fci-always-use-textual-rule t)
(autoload 'fci-mode "fill-column-indicator" "fci..." t)


;;===
(define-minor-mode my-extra "desc" nil nil nil )
(provide 'my-extra)
