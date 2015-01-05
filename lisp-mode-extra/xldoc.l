;;; -*- mode: lisp; package: editor -*-
;;;
;;; lisp-mode-extra/xldoc.l

;;; Commentary:

;; NetInstaller �ŃC���X�g�[�����Ă� ldoc ��������Ȃ��̂Ő����ō�����B

;;; Code:

(in-package :editor)

(export '(xldoc-turn-on xldoc-turn-off *xldoc-target-modes* *xldoc-exclude-commands*
          ))


(defparameter *xldoc-target-modes* '(lisp-mode lisp-interaction-mode)
  "*xldoc ��L���ɂ��郁�W���[���[�h")

(defparameter *xldoc-exclude-commands*
  '(eval-last-sexp eval-expression save-buffer describe-key-briefly
    execute-extended-command)
  "*�����ɓo�^���ꂽ�R�}���h�̌�� xldoc �\�������Ȃ��B")


(defun xldoc-%find-target-symbol ()
  (and (eql (following-char) #\()
       (forward-char 1)
       (looking-at "\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)")
       (let ((from (match-beginning 1))
             (to   (match-end 1)))
         (values (lookup-symbol from to (lisp-point-package-name))
                 from
                 to))))

(defun xldoc-%symbol-function-info (sym)
  (let ((def (symbol-function sym)))
    (cond ((get sym 'si::builtin-function-argument))
          ((si:*closurep def)
            (second (si:closure-body def)))
          ((and (listp def)
                (eql (car def) 'macro))
            (second def))
          (t
            (error 'undefined-function :name sym)))))

(defun xldoc-%get-operator-info ()
  (while (eql (parse-point-syntax) :string)
    (or (scan-buffer "\\s\"")
        (return-from xldoc-%get-operator-info nil)))
  (unless (up-list -1 t)
    (return-from xldoc-%get-operator-info nil))
  (handler-case
      (multiple-value-bind (sym from to)
          (xldoc-%find-target-symbol)
        (values sym (xldoc-%symbol-function-info sym) from to))
    (undefined-function (e)
      (if (up-list -1 t)
        (xldoc-%get-operator-info)
        (error e)))))

(defun xldoc-show-info (sym info)
  (cond ((stringp info)
          (message "(~S ~A)" sym info))
        ((listp info)
          (message "(~S~{ ~:@(~A~)~})" sym info))))

;; post-command-hook
(defun xldoc-maybe-display ()
  (when (and (member buffer-mode *xldoc-target-modes*)
             (not (member *this-command* *xldoc-exclude-commands*)))
    (ignore-errors
     (multiple-value-bind (sym info)
         (save-excursion
           (protect-match-data
             (xldoc-%get-operator-info)))
       (when sym
         (xldoc-show-info sym info))))))


(defun xldoc-turn-on ()
  (interactive)
  (add-hook '*post-command-hook* 'xldoc-maybe-display))

(defun xldoc-turn-off ()
  (interactive)
  (delete-hook '*post-command-hook* 'xldoc-maybe-display))

(provide "lisp-mode-extra/xldoc")
;;; lisp-mode-extra/xldoc.l ends here.