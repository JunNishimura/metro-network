;; main.lisp
(in-package :cl-user)
(defpackage :cl-metro
  (:use cl)
  (:export :romaji-to-kanji
           :get-ekikan-kyori
	   :kyori-wo-hyouji
           :make-eki-list
	   ))
(in-package :cl-metro)

;; �ړI�F���[�}�����Ŏ󂯎�����w�̊����\�L��Ԃ��֐�
(defun romaji-to-kanji (romaji ekimei-list)
  (cond ((null ekimei-list) nil)
	((string= romaji (caddar ekimei-list))
	 (caar ekimei-list))
	(t (romaji-to-kanji romaji (cdr ekimei-list))) ))

;; �ړI�F�����\�L�̉w�����炻��2�w�Ԃ̋�����Ԃ��֐��B2�w���q�����Ă��Ȃ��ꍇ��nil��Ԃ�
(defun get-ekikan-kyori (eki1 eki2 ekikan-list)
  (cond ((null ekikan-list) nil)
	(t (let ((kiten (caar ekikan-list))
		 (shuten (cadar ekikan-list)) )
	     (cond ((or (and (string= eki1 kiten)
			     (string= eki2 shuten) )
			(and (string= eki1 shuten)
			     (string= eki2 kiten) ))
		    (cadddr (car ekikan-list)) )
		   (t (get-ekikan-kyori eki1 eki2 (cdr ekikan-list))) )))))

;; �ړI�F���[�}���̉w��2���󂯎��A���̊Ԃ̋����𒲂ׂĕ������Ԃ��֐�
(defun kyori-wo-hyouji (r-eki1 r-eki2 ekimei-list ekikan-list)
  (let ((k-eki1 (romaji-to-kanji r-eki1 ekimei-list))
	(k-eki2 (romaji-to-kanji r-eki2 ekimei-list)) )
    (cond ((null k-eki1) (format t "There is no station ~A.~%" r-eki1))
	  ((null k-eki2) (format t "There is no station ~A.~%" r-eki2))
	  (t (let ((kyori (get-ekikan-kyori k-eki1 k-eki2 ekikan-list)))
	       (cond ((null kyori) (format t "station ~A and station ~A is not connected~%" k-eki1 k-eki2))
		     (t (format t "the distance between ~A and ~A is ~F.~%" k-eki1 k-eki2 kyori)) ))))))

;; �ړI�F(namae, saitan-kyori, temae-list)���琬�郊�X�g�̍쐬
(defun make-eki-list (ekimei-list)
  (cond ((null ekimei-list) nil)
	(t (cons (list (caar ekimei-list) nil nil)
		 (make-eki-list (cdr ekimei-list)) ))))

;; �ړI�F�^����ꂽ�w�ŏ���������
(defun shokika (eki-list eki)
  (cond ((null eki-list) nil)
	((string= eki (caar eki-list))
	 (cons (list eki 0 (list eki))
	       (cdr eki-list) ))
	(t (cons (car eki-list)
		 (shokika (cdr eki-list) eki) ))))

;; �ړI�F�����ɕ���ł��郊�X�g�̐������ʒu�ɐV�����w��}������
;; ���́F�����\�[�g�ς݂̃��X�g, �V�����w
;; �o�́F�V�����w��ǉ������\�[�g�ς݂̃��X�g
(defun ekimei-insert (ekimei-list eki)
  (cond ((null ekimei-list) (list eki))
	((string= (cadar ekimei-list)(cadr eki)) ;; �����̉w�͏���
	 (ekimei-insert (cdr ekimei-list) eki) )
	((string< (cadar ekimei-list)(cadr eki))
	 (cons (car ekimei-list)
	       (ekimei-insert (cdr ekimei-list) eki) ))
	(t (cons eki ekimei-list)) )) ;; �K�؂Ȉʒu�𔭌�

;; �ړI�Fekimei-list���Ђ炪�ȏ��ŏ����\�[�g
;; ���́F������̃��X�g
;; �o�́F����ς݂̃��X�g
(defun seiretsu (ekimei-list)
  (cond ((null ekimei-list) nil)
	(t (ekimei-insert (seiretsu (cdr ekimei-list)) (car ekimei-list))) ))

;; �ړI�F�wp�Ɖwq���Ȃ����Ă��邩���m�F���A�K�v�ɉ����čX�V����
;; ���́F�wp, �wq, ekikan-list
;; �o�́F�wq(�X�V�̗L���Ɋ֌W���炸)
(defun koushin1 (p q ekikan-list)
  (let ((kyori (get-ekikan-kyori (car p) (car q) ekikan-list)))
    (cond ((null kyori) q) 		; �w���q�����Ă��Ȃ��ꍇ�͉wq�����̂܂ܕԂ�
	  ((null (cadr q))		; ���K��̎��͍X�V
	   (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
	  ((< (+ kyori (cadr p)) (cadr q)) ; �����̍ŒZ���������Z����
 	   (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
	  (t q) )))			; �X�V�̕K�v�̂Ȃ��ꍇ

(setq eki1 '("�r��" nil nil))
(setq eki2 '("�V���" 1.2 ("�V���" "䪉גJ")))
(setq eki3 '("䪉גJ" 0 ("䪉גJ")))
(setq eki4 '("��y��" nil nil))


