;;; -*- mode: lisp; package: editor -*-
;;;
;;; lisp-mode-extra.l -- lisp-mode に追加する雑多なものたち。

;;; Code:

(in-package :editor)


;;;; Utilities

(defun lisp-number-of-universal-arguments (&optional (n *prefix-value*))
  "C-u を押した回数"
  (cond ((null n) 0)
        (t (do ((n n (/ n 4))
                (i 0 (1+ i)))
               ((<= n 1) i)))))

(defun lisp-toplevel-paren-balanced-p ()
  "カーソル位置の式がすべて閉じられているか否か"
  ;; FIXME: Save goal column
  (save-excursion
    (while (backward-up-list 1 t))
    (ignore-errors (goto-matched-parenthesis))))

(defparameter +lisp-in-package-regexp+  ; FIXME: defconstant
  (compile-regexp "(in-package +[:'\"]\\(\\(?:\\sw\\|\\s_\\)+\\)\"?)"))

(defun lisp-point-package-name (&optional (point nil sv))
  "カーソル位置のパッケージ名（文字列）"
  (save-excursion
    (when sv
      (goto-char point))
    (cond ((scan-buffer +lisp-in-package-regexp+
                        :reverse t :regexp t)
           (match-string 1))
          (*buffer-package*)
          ((and (goto-char (point-min))
                (scan-buffer "package: \\(\\(?:\\sw\\|\\s_\\)+\\).*-\\*-[ \t]*$"
                             :regexp t :limit 3000)
                (eql (parse-point-syntax) :comment))
           (match-string 1))
          ("user"))))

(defun lisp-number-of-sexps-in (start &optional end)
  "START と END （END 省略時はカーソル位置と START）の間にある式の数"
  (unless end
    (setq end (point)))
  (unless (< start end)
    (rotatef start end))
  (save-excursion
    (goto-char start)
    (let ((n -1))
      (while (< (point) end)
        (skip-white-forward)
        (incf n)
        (or (forward-sexp 1 t)
            (return)))
      n)))


;;;; setup helper
(export '(lisp-mode-extra-setup
          ))

(defparameter *lisp-mode-extra-setup-functions* nil)

(defun lisp-mode-extra-setup ()
  (interactive)
  (mapc #'funcall *lisp-mode-extra-setup-functions*))

(defmacro defsetup (name ignare &body body)
  `(pushnew (si:*set-function-name (lambda () ,@body) ',name)
            *lisp-mode-extra-setup-functions*
            :key #'si:*function-name))



;;;; electric insert commands

(export '(lisp-electric-insert-open
          lisp-electric-insert-close
          lisp-electric-insert-double-quote
          ))

(defparameter *lisp-pair-chars-alist*
  '((#\( . #\))
    (#\{ . #\})
    (#\[ . #\])))

(defun lisp-electric-insert-open (&optional (arg 0))
  "開きカッコを挿入して、てきとーに閉じる"
  (interactive "*p")
  (setq arg (lisp-number-of-universal-arguments))
  (let ((open-char  *last-command-char*)
        (close-char (cdr (assoc *last-command-char* *lisp-pair-chars-alist*))))
    (insert open-char)
    (unless (and (null (parse-point-syntax))
                 close-char)
      (return-from lisp-electric-insert-open t))
    (save-excursion
      (let ((close-point (save-excursion
                           (when arg (forward-sexp arg t))
                           (point))))
        ;; 一番外の開きカッコから goto-matched-parenthesis * 2 すると、
        ;; カッコ/コッカのバランスが取れてれば同じ位置に戻ってくる。
        (while (up-list -1 t))
        (let ((p (point)))
          (unless (and (ignore-errors (goto-matched-parenthesis))
                       (ignore-errors (goto-matched-parenthesis))
                       (= (point) p))
            (goto-char close-point)
            (insert close-char)))
        t))))

(defun lisp-electric-insert-close (&optional (arg 1))
  "閉じコッカをてきとーに入力したりする"
  (interactive "*p")
  (case (lisp-number-of-universal-arguments)
    (1 (let ((beg (save-excursion
                    (while (up-list -1 t))
                    (point))))
         (while (save-excursion
                  (goto-char beg)
                  (not (ignore-errors (goto-matched-parenthesis))))
           (insert *last-command-char*))
         t))
    (t (if (and (eql (following-char) *last-command-char*)
                (lisp-toplevel-paren-balanced-p))
         (forward-char)
         (insert *last-command-char* arg)))))

(defun lisp-electric-insert-double-quote ()
  "ダブルクォート（文字列の開始/終了）をてきとーに入力したりしなかったり。

何もないところでは文字列を開始する \" を挿入し、必要であれば文字列を閉じる
\" も挿入します。

    abc|def  ->  abc\"|\"def

文字列中では、カーソルが文字列終了の \" の前であればその \" の後へカーソル
を移動し、そうでなければエスケープされたダブルクォート \\\" を挿入します。

    \"abc|def\" -> \"abc\\\"def\"

コメント内では単純に \" を挿入します。"
  (interactive "*")
  (case (parse-point-syntax)
    (:string
     (if (and (eql (following-char) *last-command-char*)
              (not (syntax-escape-p (preceding-char))))
       (forward-char)
       (progn
         (insert #\\)
         (insert *last-command-char*))))
    (:comment
     (insert *last-command-char*))
    (otherwise  ; 文字列の開始
     (insert *last-command-char*)
     (save-excursion
       (let ((opoint (point)))
         ;; FIXME: パラグラフ内にコメントだった " があると間違える。
         (when (and (forward-paragraph)
                    (eql (parse-point-syntax) :string))
           (goto-char opoint)
           (insert *last-command-char*)))))
    ))

(defsetup electric-commands ()
  (define-key ed::*lisp-mode-map* #\( 'lisp-electric-insert-open)
  (define-key ed::*lisp-mode-map* #\) 'lisp-electric-insert-close)
  (define-key ed::*lisp-mode-map* #\" 'lisp-electric-insert-double-quote))

;;;;


;;; lisp-mode-extra.l ends here.
