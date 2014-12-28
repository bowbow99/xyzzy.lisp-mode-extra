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
  ;; FIXME: パラグラフ内にコメントだった \" があると間違える。
  (interactive "*")
  (case (parse-point-syntax)
    (:string
      (cond ((and (eql (following-char) *last-command-char*)
                  (not (syntax-escape-p (preceding-char))))
              (forward-char))
            ((save-excursion
               (and (goto-char (point-max))
                    (eql (parse-point-syntax) :string)))
              (insert *last-command-char*))
            (t
              (insert #\\)
              (insert *last-command-char*))))
    (:comment
      (insert *last-command-char*))
    (otherwise  ; 文字列の開始
      (insert *last-command-char*)
      (save-excursion
        (let ((opoint (point)))
          (when (and (forward-paragraph)
                     (eql (parse-point-syntax) :string))
            (goto-char opoint)
            (insert *last-command-char*)))))
    ))

(defsetup electric-commands ()
  (define-key ed::*lisp-mode-map* #\( 'lisp-electric-insert-open)
  (define-key ed::*lisp-mode-map* #\) 'lisp-electric-insert-close)
  (define-key ed::*lisp-mode-map* #\" 'lisp-electric-insert-double-quote))


;;;; Indentation

(defun calc-lisp-indent+ (opoint)
  (protect-match-data
    (let ((begin-paren (and lisp-indent-close-paren
                            (looking-at "[ \t]*)"))))
      (goto-bol)
      (when (eql (parse-point-syntax) :string)
        (back-to-indentation)
        (return-from calc-lisp-indent+
          (current-column)))
      (when (and (looking-at "\\s(")
                 (forward-char -1))
        (skip-white-backward)
        (forward-char 1))
      (or (up-list -1 t)
          (return-from calc-lisp-indent+ 0))
      (cond
       (begin-paren
         (+ (current-column) lisp-paren-imaginary-offset))
       ((or (looking-at "#")
            (and (not (looking-back "#'"))
                 (looking-back "'")))
         (+ (current-column) 1))
       (t
         (let ((package (or (find-package (lisp-point-package-name))
                            (and (stringp *buffer-package*)
                                 (find-package *buffer-package*))
                            *package*)))
           ;; ed::lisp-indent-flet の対象
           (when (save-excursion
                   (when (and (up-list -1 t)
                              (looking-for "((")
                              (up-list -1 t))
                     (forward-char 1)
                     (multiple-value-bind (symbol found-p)
                         (calc-lisp-indent-current-symbol package)
                       (when found-p
                         (get symbol 'lisp-indent-flet)))))
             (return-from calc-lisp-indent+
               (+ (current-column) *lisp-body-indention*)))
           ;;
           (let ((column (progn
                           (forward-char 1)
                           (current-column))))
             (multiple-value-bind (symbol found package-marker-p)
                 (calc-lisp-indent-current-symbol package)
               ;; operator が keyword の場合
               #+nil
               (when package-marker-p
                 (return-from calc-lisp-indent+ column))
               ;; (get SYMBOL 'ed::lisp-indent-clause) = N
               ;; N 番目以降の sub forms は &body 扱い
               (save-excursion
                 (when (and (up-list -1 t)
                            (forward-sexp -1 t)
                            (up-list -1 t)
                            (forward-char 1))
                   (multiple-value-bind (symbol* found*)
                       (calc-lisp-indent-current-symbol package)
                     (when found*
                       (let ((x (get symbol* 'lisp-indent-clause)))
                         (when (and (numberp x)
                                    (>= (lisp-number-of-sexps-in opoint) (1- x)))
                           (return-from calc-lisp-indent+
                             (+ (1- column)
                                *lisp-body-indent*))))))))
               ;; ed:lisp-indent-hook やマクロの &body
               (let ((method (when found
                               (or (get symbol 'lisp-indent-hook)
                                   ;; マクロの &body に相当する場合
                                   (let* ((args   (cadr (macro-function symbol)))
                                          (before (when (find '&body args)
                                                    (subseq args 0 (position '&body args)))))
                                     (when before
                                       (while (find (car before) '(&whole &environment))
                                         (setq before (cddr before)))
                                       (length before)))))))
                 (cond ((numberp method)
                         (let ((count (lisp-number-of-sexps-in opoint)))
                           (+ (1- column)
                              (* (if (< count method) 2 1)
                                 *lisp-body-indent*))))
                       (method
                         (+ column -1 *lisp-body-indention*))
                       (t
                         (skip-chars-forward " \t")
                         (if (or (eolp)
                                 (looking-for ";"))
                           (+ column (or *lisp-indent-offset* 0))
                           (current-column)))))))))))))

(defconstant +calc-lisp-indent-original+ #'calc-lisp-indent)

(defsetup calc-lisp-indent+ ()
  (setf (symbol-function 'calc-lisp-indent) #'calc-lisp-indent+))


;;;; Compile and Load on Save

(export '(-lisp-load-on-save-
          -lisp-compile-on-save-
          lisp-compile/load-on-save
          ))

(defvar-local -lisp-load-on-save- :ask1
  "*lisp ファイルを保存したときにロードするかどうか。
:ask      -- 毎回確認する
:ask1     -- 初回のみ確認する
nil       -- しない
それ以外  -- する"
  ;; FIXME: :auto
  )

(defvar-local -lisp-compile-on-save- :auto
  "*lisp ファイルを保存したときにコンパイルするかどうか
:auto    -- FASL (*.lc) があればコンパイルする
:ask     -- 毎回確認する
:ask1    -- 初回のみ確認する
nil      -- しない
それ以外 -- する")

(defvar-local -lisp-action-on-save-confirm-method- 'yes-or-no-p
  "*lisp ファイルを保存時に何かするか確認する関数。
関数はフォーマット文字列とその引数を受け取って、何かするか否かを nil
か non-nil かで返すこと。")

(defun lisp-compile-on-save-p ()
  (let* ((src  (or (get-buffer-file-name)
                   (return-from lisp-compile-on-save-p nil)))
         (fasl
           ;; FIXME: Recent version of xyzzy supports "*.lisp" so adding "c" isn't safe
           (concat src "c")))
    (unless src
      (return-from lisp-compile-on-save-p nil))
    (labels ((ask ()
               (funcall -lisp-action-on-save-confirm-method- "コンパイルしますか？")))
      (case -lisp-compile-on-save-
        (:auto      (file-exist-p fasl))
        (:ask       (ask))
        (:ask1      (setf -lisp-compile-on-save- (ask)))
        (otherwise  -lisp-compile-on-save-)))))

(defun lisp-load-on-save-p ()
  (let ((src (get-buffer-file-name)))
    (unless src
      (return-from lisp-load-on-save-p nil))
    (labels ((ask ()
               (funcall -lisp-action-on-save-confirm-method- "ロードしますか？")))
      (case -lisp-load-on-save-
        ;(:auto)
        (:ask       (ask))
        (:ask1      (setf -lisp-load-on-save- (ask)))
        (otherwise  -lisp-load-on-save-)))))

(defun lisp-compile/load-on-save ()
  (when (member buffer-mode '(lisp-mode lisp-interaction-mode))
    (let* ((src  (or (get-buffer-file-name)
                     (return-from lisp-compile/load-on-save nil)))
           (fasl (concat src "c")))
      (when (lisp-compile-on-save-p)
        (compile-file src))
      (when (lisp-load-on-save-p)
        (load (if (file-exist-p fasl) fasl src))))))

(defsetup compile/load-on-save ()
  (add-hook '*after-save-buffer-hook* 'lisp-compile/load-on-save))


;;;; Literal Highligting

;; FIXME: ハイライトの色（ハイライトするかどうかも含めて）を設定できるように

(export (
         ))

(defparameter *lisp-regexp-keyword-list* nil)

(defparameter *lisp-compiled-regexp-keyword-list* nil)

(defun %lisp-update-compiled-regexp-keyword-list ()
  (setf *lisp-compiled-regexp-keyword-list*
        (compile-regexp-keyword-list (mapcar #'cdr *lisp-regexp-keyword-list*))))

(defun %lisp-add-regexp-keyword-list (name regexp case-fold color context begin end)
  (setf *lisp-regexp-keyword-list*
        (cons (list name regexp case-fold color context begin end)
              (delete name *lisp-regexp-keyword-list* :key #'car)))
  (%lisp-update-compiled-regexp-keyword-list))

(defmacro lisp-define-regexp-keyword (name color (&key (case-fold t) context begin end) &rest regexp-parts)
  `(%lisp-add-regexp-keyword-list
    ',name (concat ,@regexp-parts) ,case-fold ',color ,context ,begin ,end))
(setf (get 'lisp-define-regexp-keyword 'ed:lisp-indent-hook) '3)

(defun lisp-setup-regexp-keyword ()
  (interactive)
  (make-local-variable 'regexp-keyword-list)
  (setf regexp-keyword-list (%lisp-update-compiled-regexp-keyword-list)))

(defsetup literal-highlighting ()
  (dolist (hook '(*lisp-mode-hook* *lisp-interaction-mode-hook*))
    (add-hook hook 'lisp-setup-regexp-keyword)))


(lisp-define-regexp-keyword keyword-symbol (:color 8) ()
  "\\_<:\\(?:\\sw\\|\\s_\\)+\\_>")




#+develop
(progn
  (lisp-mode-extra-setup)
  (lisp-mode))

;;;; FIXME: lisp-make-tags+
;;;; FIXME: indentation for lambda-list
;;;; FIXME: indentation for extended loop


(provide "lisp-mode-extra")
;;; lisp-mode-extra.l ends here.
