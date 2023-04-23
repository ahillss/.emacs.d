
;;===
(add-to-list 'exec-path "/usr/local/bin")

;;===site-lisp location
(let ((default-directory (concat (file-name-directory load-file-name) "site-lisp")))
  (when (file-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;;===environments
(setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH")
(setenv "PYTHONPATH" "/usr/local/share/python:$PYTHONPATH")
(setenv "CPATH" "$CPATH:/usr/local/include")
(setenv "LIBRARY_PATH" "/usr/local/lib:/usr/local/share/python:$LIBRARY_PATH")
(setenv "NODE_PATH" "/usr/local/lib/node_modules:$NODE_PATH")
(setenv "NODE_NO_READLINE" "1")

;;===no query kill processes, buffers
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (mapc
   (lambda (p) (set-process-query-on-exit-flag p nil))
   (process-list))
  ad-do-it)

;;===
(defalias 'yes-or-no-p 'y-or-n-p)

;;===my-keys
(defun my-local-set-key (k &rest l)
  (local-set-key k
    `(lambda ()
       (interactive)
       (mapc 'call-interactively (quote (,@l))))))

(defun my-global-set-key (k &rest l)
  (global-set-key k
    `(lambda () (interactive) (mapc 'call-interactively (quote (,@l))))))

;;===
(defun my-kill-buffers (&rest n)
  (mapc
   (lambda (x)
     (let ((b (get-buffer x)))
       (when b (kill-buffer b))))
   n)
  (delete-other-windows))

;;===elisp
(defun my-emacs-lisp-hook ()
  (my-local-set-key (kbd "<f7>") 'delete-other-windows)
  (mapc
   (lambda (sym) (put sym 'lisp-indent-function 'defun))
   '(add-hook my-local-set-key my-global-set-key
      global-set-key local-set-key)))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-hook)

;;===fix some terminals keys
(unless (display-graphic-p)
  ;; (when (equal "xterm" (tty-type))
  ;;   (define-key input-decode-map "\e[1;2A" [S-up]))
  ;; (global-set-key "\M-[1~" 'beginning-of-line)
  (global-set-key [select] 'end-of-line))

;;===delete trailing space on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;===windows undo key
(global-set-key "\C-z"
  'undo-only)

;;===tabs to spaces indent
(my-global-set-key [C-iso-lefttab]
  'untabify 'indent-for-tab-command)

;;===ido
(setq ido-save-directory-list-file (concat (file-name-directory load-file-name) ".ido.last"))

(defun op-i:dired ()
  (interactive)
  (let ((dir (car (find-file-read-args "Dired: " nil))))
    (if (file-directory-p dir)
        (dired dir "-lhA")
      (find-file dir))))

(global-set-key (kbd "C-x C-d") 'op-i:dired)

;;===
(defun my-region-line ()
  (interactive)
  (unless (region-active-p)
    (move-beginning-of-line nil)
    (skip-chars-forward "\s\t")
    (set-mark-command nil)
    (move-end-of-line nil)
    (skip-chars-backward "\s\t")))

(defun my-region-expand ()
  (interactive)
  (when (region-active-p)
    (let ((a (save-excursion
               (goto-char (region-beginning))
               (line-beginning-position 1)))
          (b (save-excursion
               (goto-char (region-end)) (line-end-position 1))))
      (goto-char a)
      (set-mark-command nil)
      (goto-char b)
      (move-end-of-line nil))))

(defun my-region-indents ()
  (interactive)
  (unless (region-active-p)
    (save-match-data
      (let ((a (line-beginning-position 1)))
        (goto-char a)
        (let* ((ptn "^\\([\s\t]+\\)[^\r\n]*\\([\r\n]+\\1[\s\t]+[^\r\n]*\\)*")
               (r (re-search-forward ptn (point-max) t))
               (m (and r (match-end 0)))
               (b (or m (line-end-position 1))))
          (goto-char a)
          (set-mark-command nil)
          (goto-char b)
          (move-end-of-line nil))))))

;;===shift indent
(defun shift-indent (amount)
  (interactive "nShift indent by: ")
  (my-region-expand)
  (my-region-indents)
  (indent-rigidly (region-beginning) (region-end) amount))

(my-global-set-key [C-tab] 'shift-indent)

;;===mouse3 copy cut
(defun my-mouse3-kill ()
  (interactive)
  (let ((p (region-active-p)))
    (my-region-line)
    (call-interactively (if p 'kill-region 'copy-region-as-kill))))

(my-global-set-key [mouse-3] 'my-mouse3-kill)
(my-global-set-key [double-mouse-3] 'my-region-line 'kill-region)

;;===text zoom
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc)
  (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(defun global-text-scale-increase ()
  (interactive)
  (global-text-scale-adjust 1))

(defun global-text-scale-decrease ()
  (interactive)
  (global-text-scale-adjust -1))

(global-set-key [C-mouse-4] 'global-text-scale-increase)
(global-set-key [C-mouse-5] 'global-text-scale-decrease)
(global-set-key [C-wheel-up] 'global-text-scale-increase)
(global-set-key [C-wheel-down] 'global-text-scale-decrease)

;;===cut/copy
(my-global-set-key "\C-w"
  'my-region-line 'kill-region)

(my-global-set-key "\M-w"
  'my-region-line 'copy-region-as-kill)

;;===comment region
(my-global-set-key [f3]
  'my-region-expand 'my-region-line 'uncomment-region)

(my-global-set-key [f4]
  'my-region-expand 'my-region-line 'comment-region)

;;===comint history
(defun my-comint-read-history ()
  (let* ((buf (buffer-name (current-buffer)))
         (fn (replace-regexp-in-string "\*+" "" buf))
         (hist (concat user-emacs-directory "/hist"))
         (comint-input-ring-file-name (concat hist "/" fn)))
    (if (file-exists-p comint-input-ring-file-name)
        (comint-read-input-ring)
      (while (not (ring-empty-p comint-input-ring))
        (ring-remove comint-input-ring nil)))))

(defun my-comint-write-history ()
  (when (derived-mode-p 'comint-mode)
    (let* ((buf (buffer-name (current-buffer)))
           (fn (replace-regexp-in-string "\*+" "" buf))
           (hist (concat user-emacs-directory "/hist"))
           (comint-input-ring-file-name (concat hist "/" fn)))
      (unless (file-exists-p hist) (make-directory hist))
      (comint-write-input-ring))))

(defun my-comint-write-buffer-history (b)
  (with-current-buffer b
    (funcall 'my-comint-write-history)))

(defun my-comint-write-histories ()
  (mapc 'my-comint-write-buffer-history (buffer-list)))

(add-hook 'comint-mode-hook 'my-comint-read-history)
(add-hook 'kill-buffer-hook 'my-comint-write-history)
(add-hook 'kill-emacs-hook 'my-comint-write-histories)

;;===shell
(defun my-shell-sentinel (process event)
  (when (eq (current-buffer) (process-buffer process))
    (call-interactively 'other-window))
  (message (substring event 0 (- (length event) 1))))

(defun my-shell-run (cmd bufnm)
  (interactive)
  (let* ((c (current-buffer))
         (fn (buffer-file-name c))
         (default-directory (file-name-directory fn)))
    (unless (get-buffer-process bufnm)
      (async-shell-command cmd bufnm)
      (let ((p (get-buffer-process bufnm)))
        (set-process-query-on-exit-flag p nil)
        (set-process-sentinel p 'my-shell-sentinel)
        (switch-to-buffer-other-window bufnm)))))

;;===compile
(defadvice compile (before ad-compile-smart activate)
  (ad-set-arg 1 t))

(defadvice compile (around other-window activate)
  ad-do-it
  (other-window 1)
  (goto-char (point-max)))

(defadvice compilation-sentinel (around other-window activate)
  ad-do-it
  (other-window 1))

;;===scheme
(defun my-scheme-start ()
  (interactive)
  (let ((c (current-buffer)))
    (unless (get-buffer "*scheme*")
      (call-interactively 'run-scheme))
    (switch-to-buffer-other-window "*scheme*")
    (switch-to-buffer-other-window c)))

(defun my-scheme-file ()
  (interactive)
  (scheme-load-file (buffer-file-name (current-buffer))))

(defun my-scheme-send ()
  (interactive)
  (if (region-active-p)
      (scheme-send-region (region-beginning) (region-end))
    (scheme-send-last-sexp)))

(defun my-scheme-kill ()
  (interactive)
  (my-kill-buffers "*scheme*"))

(defun my-scheme-hook ()
  (put 'mymatch 'scheme-indent-function 1)
  (put 'match 'scheme-indent-function 1)
  (put 'qmatch 'scheme-indent-function 1)
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-scheme-start 'my-scheme-file)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-scheme-start 'my-scheme-send)
  (my-local-set-key (kbd "<f7>")
    'my-scheme-kill))

(add-hook 'scheme-mode-hook 'my-scheme-hook)
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))

;;===latex
(defun my-latex-file ()
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer)))
         (out (concat (file-name-directory fn) "bin"))
         (cmd (concat "pdflatex -halt-on-error -output-directory=" out " " fn))
         (default-directory (concat (file-name-directory fn))))
    (unless (file-exists-p out) (make-directory out))
    (if (= 0 (shell-command cmd "*tex-shell*"))
        (progn (message "success") (delete-windows-on "*tex-shell*"))
      (let ((c (current-buffer)))
        (switch-to-buffer-other-window "*tex-shell*")
        (end-of-buffer)
        (switch-to-buffer-other-window c)
        (message "failed") ))))

(defun my-latex-kill ()
  (interactive)
  (my-kill-buffers "*tex-shell*"))

(defun my-latex-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-latex-file)
  (my-local-set-key (kbd "<f7>")
    'my-latex-kill))

(add-hook 'latex-mode-hook 'my-latex-hook)

;;===prolog
(defun my-prolog-send-solve ()
  (interactive)
  (process-send-string "*prolog*" "solve(L), print(L), nl, false.\n"))

(defun my-prolog-create-rc-file ()
  (interactive)
  (let (fn (if (eq 'windows-nt system-type) "~/pl.ini" "~/.plrc"))
    (unless (file-exists-p fn)
      (with-temp-file fn
        (princ ":- set_prolog_flag(" (current-buffer))
        (princ "toplevel_print_options," (current-buffer))
        (princ "[quoted(true),portray(true)])." (current-buffer)) ))))

(defun my-prolog-kill ()
  (interactive)
  (my-kill-buffers "*prolog*"))

(defun my-prolog-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'run-prolog 'other-window
    'prolog-consult-file 'other-window)
  (my-local-set-key (kbd "S-<f5>")
    'save-buffer 'run-prolog 'other-window
    'prolog-consult-file 'other-window 'my-prolog-send-solve)
  (my-local-set-key (kbd "<f6>")
    'save-buffer'run-prolog 'other-window
    'my-region-line 'prolog-consult-region 'other-window)
  (my-local-set-key (kbd "<f7>")
    'my-prolog-kill)
  (my-local-set-key (kbd "<f8>")
    'my-prolog-send-solve)
  (my-local-set-key (kbd "<f9>")
    'my-prolog-create-rc-file))

(add-hook 'prolog-mode-hook 'my-prolog-hook)
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

;;===python

(defun my-python-start ()
  (interactive)
  (let ((c (current-buffer)))
    (pop-to-buffer
     (process-buffer (call-interactively 'run-python)))
    (switch-to-buffer-other-window c)))

(defun my-python-kill ()
  (interactive)
  (my-kill-buffers "*Python*"))

(defun my-python-send-buffer ()
  (interactive)
  (python-shell-send-buffer t))

(defun my-python-send-line ()
  (interactive)
  (my-region-line)
  (python-shell-send-region (region-beginning) (region-end) t)
  (deactivate-mark))

(defun my-python-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-python-start 'my-python-send-buffer)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-python-start 'my-python-send-line)
  (my-local-set-key (kbd "<f7>") 'my-python-kill))

(add-hook 'python-mode-hook 'my-python-hook)

;;===tcl
(defun my-tcl-file ()
  (interactive)
  (tcl-load-file (buffer-file-name (current-buffer))))

(defun my-tcl-send ()
  (interactive)
  (my-region-line)
  (tcl-eval-region (region-beginning) (region-end))
  (setq mark-active nil))

(defun my-tcl-start ()
  (interactive)
  (let ((c (current-buffer)))
    (if (get-buffer "*inferior-tcl*")
        (switch-to-buffer-other-window "*inferior-tcl*")
      (call-interactively 'inferior-tcl))
    (switch-to-buffer-other-window c)))

(defun my-tcl-kill ()
  (interactive)
  (my-kill-buffers "*inferior-tcl*"))

(defun my-tcl-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-tcl-start 'my-tcl-file)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-tcl-start 'my-tcl-send)
  (my-local-set-key (kbd "<f7>")
    'my-tcl-kill))

(add-hook 'tcl-mode-hook 'my-tcl-hook)

;;===cpp
(defun my-cpp-compile ()
  (interactive)
  (compile "make"))

(defun my-cpp-run ()
  (interactive)
  (compile "make test"))

(defun my-cpp-clean ()
  (interactive)
  (compile "make clean"))

(defun my-cpp-kill ()
  (interactive)
  (my-kill-buffers "*compilation*"))

(defun my-cpp-hook ()
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (my-local-set-key (kbd "<f5>") 'save-buffer 'my-cpp-compile)
  (my-local-set-key (kbd "<f6>") 'save-buffer 'my-cpp-run)
  (my-local-set-key (kbd "<f7>") 'my-cpp-kill)
  (my-local-set-key (kbd "<f8>") 'my-cpp-clean))

(add-to-list 'auto-mode-alist '("\\.[ch]p?p?\\'" . c++-mode))
(add-hook 'c++-mode-hook 'my-cpp-hook)
(add-to-list 'auto-mode-alist '("\\.rl$" . c++-mode))

;;===nodejs
(defun my-nodejs-start ()
  (interactive)
  (let ((c (current-buffer)))
    (if (get-buffer "*nodejs-repl*")
        (pop-to-buffer "*nodejs-repl*")
      (pop-to-buffer
       (make-comint "nodejs-repl" "node" nil "--interactive")))
    (switch-to-buffer-other-window c) ))

(defun my-nodejs-send-file ()
  (interactive)
  (let* ((fn (buffer-file-name))
         (str (format "eval(fs.readFileSync('%s')+'');\n" fn)))
    (comint-send-string "*nodejs-repl*" str) ))

(defun my-nodejs-send-region (start end)
  (interactive "r")
  (let ((str (concat (buffer-substring start end) "\n")))
    (save-excursion
      (my-region-line)
      (comint-send-string "*nodejs-repl*" str) ))
  (deactivate-mark))

(defun my-nodejs-kill ()
  (interactive)
  (my-kill-buffers "*nodejs-repl*"))

(defun my-nodejs-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-nodejs-start 'my-nodejs-send-file)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-nodejs-start 'my-nodejs-send-region)
  (my-local-set-key (kbd "<f7>")
    'my-nodejs-kill))

(add-hook 'js-mode-hook 'my-nodejs-hook)

;;===common lisp
(defun my-lisp-start ()
  (interactive)
  (unless (get-buffer "*inferior-lisp*")
    (let ((c (current-buffer)))
      (call-interactively 'run-lisp)
      (switch-to-buffer-other-window "*inferior-lisp*")
      (switch-to-buffer-other-window c))))

(defun my-lisp-file ()
  (interactive)
  (lisp-load-file (buffer-file-name (current-buffer)))
  (other-window 1))

(defun my-lisp-send ()
  (interactive)
  (if (region-active-p)
      (lisp-eval-region (region-beginning) (region-end))
    (lisp-eval-last-sexp)))

(defun my-lisp-kill ()
  (interactive)
  (my-kill-buffers "*inferior-lisp*"))

(defun my-lisp-hook ()
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'my-lisp-start 'my-lisp-file)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'my-lisp-start 'my-lisp-send)
  (my-local-set-key (kbd "<f7>")
    'my-lisp-kill) )

(add-hook 'lisp-mode-hook 'my-lisp-hook)

;;===rust
(defun my-rust-kill ()
  (interactive)
  (my-kill-buffers "*compilation*"))

(defun my-rust-hook ()
  (my-local-set-key (kbd "S-<f5>")
    'save-buffer 'rust-compile-release)
  (my-local-set-key (kbd "<f5>")
    'save-buffer 'rust-compile)
  (my-local-set-key (kbd "<f6>")
    'save-buffer 'rust-run)
  (my-local-set-key (kbd "<f7>")
    'my-rust-kill)
  (my-local-set-key (kbd "<f8>")
    'save-buffer 'rust-check)
  (my-local-set-key (kbd "<f9>")
    'save-buffer 'rust-test))

(add-hook 'rust-mode-hook 'my-rust-hook)
(autoload 'rust-mode "rust-mode" "rust..." t)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;;===extra
(require 'my-extra nil t)
(require 'simple-tabbar-mode nil t)

;;===
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action (quote accept))
 '(c-basic-offset 4)
 '(comint-scroll-to-bottom-on-input t)
 '(compilation-always-kill t)
 '(compilation-read-command nil)
 '(compile-command "make")
 '(cursor-type (cons (quote bar) 1))
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(electric-pair-mode t)
 '(global-auto-revert-mode t)
 '(hi-lock-auto-select-face t)
 '(horizontal-scroll-bar-mode nil)
 '(icomplete-mode t)
 '(icomplete-prospects-height 1)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 ;'(ido-save-directory-list-file "~/.emacs.d/.ido.last")
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "sbcl")
 '(js-indent-level 4)
 '(kill-buffer-query-functions nil t)
 '(linum-delay t)
 '(linum-eager nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-scroll-amount (list 1))
 '(prolog-indent-width 2)
 '(prolog-system (quote swi))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(python-shell-completion-native-enable nil)
 '(python-shell-interpreter "python")
 '(ring-bell-function (quote ignore))
 '(scheme-program-name "racket")
 '(scroll-bar-mode t)
 '(select-enable-clipboard t)
 '(show-paren-delay 0.0)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(split-width-threshold nil)
 '(tcl-application "tclsh")
 '(tcl-continued-indent-level 2)
 '(tcl-indent-level 2)
 '(tool-bar-mode nil)
 '(x-select-enable-clipboard-manager t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-black-b ((t (:background "DarkOrchid1"))))
 '(hi-black-hb ((t (:background "MediumPurple1"))))
 '(hi-blue ((t (:background "SteelBlue1"))))
 '(hi-blue-b ((t (:background "CadetBlue2"))))
 '(hi-green ((t (:background "IndianRed1"))))
 '(hi-green-b ((t (:background "gray64"))))
 '(hi-pink ((t (:background "SeaGreen2"))))
 '(hi-red-b ((t (:background "goldenrod2"))))
 '(hi-yellow ((t (:background "gold"))))
 '(region ((t (:background "CornflowerBlue"))))
 '(show-paren-match ((t (:background "blue" :bold t))))
 '(show-paren-mismatch ((t (:background "purple" :bold t)))))
