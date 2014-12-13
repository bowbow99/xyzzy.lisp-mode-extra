;;; -*- mode: lisp; package: editor -*-
;;;
;;; lisp-mode-extra.l -- lisp-mode �ɒǉ�����G���Ȃ��̂����B

;;; Code:

(in-package :editor)


;;;; Utilities

(defun lisp-number-of-universal-arguments (&optional (n *prefix-value*))
  "C-u ����������"
  (cond ((null n) 0)
        (t (do ((n n (/ n 4))
                (i 0 (1+ i)))
               ((<= n 1) i)))))

(defun lisp-toplevel-paren-balanced-p ()
  "�J�[�\���ʒu�̎������ׂĕ����Ă��邩�ۂ�"
  ;; FIXME: Save goal column
  (save-excursion
    (while (backward-up-list 1 t))
    (ignore-errors (goto-matched-parenthesis))))

(defparameter +lisp-in-package-regexp+  ; FIXME: defconstant
  (compile-regexp "(in-package +[:'\"]\\(\\(?:\\sw\\|\\s_\\)+\\)\"?)"))

(defun lisp-point-package-name (&optional (point nil sv))
  "�J�[�\���ʒu�̃p�b�P�[�W���i������j"
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
  "START �� END �iEND �ȗ����̓J�[�\���ʒu�� START�j�̊Ԃɂ��鎮�̐�"
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
  "�J���J�b�R��}�����āA�Ă��Ɓ[�ɕ���"
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
        ;; ��ԊO�̊J���J�b�R���� goto-matched-parenthesis * 2 ����ƁA
        ;; �J�b�R/�R�b�J�̃o�����X�����Ă�Γ����ʒu�ɖ߂��Ă���B
        (while (up-list -1 t))
        (let ((p (point)))
          (unless (and (ignore-errors (goto-matched-parenthesis))
                       (ignore-errors (goto-matched-parenthesis))
                       (= (point) p))
            (goto-char close-point)
            (insert close-char)))
        t))))

(defun lisp-electric-insert-close (&optional (arg 1))
  "���R�b�J���Ă��Ɓ[�ɓ��͂����肷��"
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
  "�_�u���N�H�[�g�i������̊J�n/�I���j���Ă��Ɓ[�ɓ��͂����肵�Ȃ�������B"
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
    (otherwise  ; ������̊J�n
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


;;;;


;;; lisp-mode-extra.l ends here.