;;; Simple scheme interface

(require 'cmuscheme)
(setq auto-mode-alist (cons '("\\.ss$" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.scm$" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sss$" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.scm$" 'scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.sssp$" 'scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.sxml$" 'scheme-mode) auto-mode-alist))

(autoload 'run-scheme "cmuscheme" "Run a scheme" t)

(setq scheme-compile-exp-command "%s")

;;; Indentation hints for macros that don't begin with "def".
(put 'read-case 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-hook 1)
(put 'tag 'scheme-indent-hook 1)
(put '<> 'scheme-indent-hook 1)
(put 'public 'scheme-indent-hook 3)

(defun jscheme ()
  ;; To run jscheme from inside emacs, type:
  ;; <meta>-x jscheme
  (interactive)
  ;; Change the next two lines refer to the directory you want to be in
  ;; and the jscheme script you want to execute.
  (dired "/Users/tim/Research/Software/jscheme")
  (run-scheme "/Users/tim/Research/Software/jscheme/bin/jscheme")
  )

(defun scheme-here ()
    ;; Direct scheme expressions from other buffers to the buffer you are in.
  (interactive)
  (setq scheme-buffer (buffer-name)))

(defun scheme-send-and-advance ()
  ;; Send the define at pont to Scheme and advance to the next one.
  (interactive)
  (scheme-send-definition)
  (beginning-of-defun -1))
(define-key scheme-mode-map "\M-\C-x" 'scheme-send-and-advance)

;;; Info for scheme.
;(setq Info-directory-list
;        (append Info-directory-list
;		(list "d:/gnu/info/")))

;;; Indentation hints for macros that don'et begin with "def".
(put 'read-case 'scheme-indent-hook 1)
(put 'when 'scheme-indent-hook 1)
(put 'unless 'scheme-indent-hook 1)
(put 'dotimes 'scheme-indent-hook 1)
(put 'dolist 'scheme-indent-hook 1)
(put 'with-tag 'scheme-indent-hook 1)

