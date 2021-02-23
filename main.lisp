;; main.lisp
(in-package :cl-user)
(defpackage :cl-metro
  (:use cl)
  (:export :dijkstra))
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
  (mapcar #'(lambda (x) (list (car x) nil nil)) ekimei-list) )


;; �ړI�F�^����ꂽ�w�ŏ���������
;; ���́F�w���X�g�Əo���_�ƂȂ�w���i����)
;; �o�́F���������ꂽ�w���X�g
(defun shokika (eki-list eki)
  (mapcar #'(lambda (x)
	      (cond ((string= eki (car x))
		     (list eki 0 (list eki)) )
		    (t x)))
	  eki-list ))

;; �ړI�F(namae saitan-kyori temae-list)�̍\�����琬��w���X�g�̍쐬�A�y�я�����
;; ���́F�w�����X�g�Əo���_�ƂȂ�w���i�����j
;; �o�́F�������ς݂̉w���X�g
(defun make-initial-eki-list (ekimei-list eki)
  (mapcar #'(lambda (x)
	      (cond ((string= eki (car x))
		     (list eki 0 (list eki)) )
		    (t (list (car x) nil nil)) ))
	  ekimei-list ))


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

;; �ړI�Fv�Ɋ܂܂��w�ɂ��āA�wp�ƂȂ����Ă��邩���m�F���A�K�v�ɉ����čX�V����
;; ���́F���O�Ɋm�肵���wp�Ɩ��m��̉w�̃��X�gv�Ɖw�ԃ��X�g
;; �o�́F�K�؂ȏ������{������̃��X�gv
(defun koushin (p v ekikan-list)
  (mapcar #'(lambda (q)
	        (let ((kyori (get-ekikan-kyori (car p) (car q) ekikan-list)))
		  (cond ((null kyori) q) 		; �w���q�����Ă��Ȃ��ꍇ�͉wq�����̂܂ܕԂ�
			((null (cadr q))		; ���K��̎��͍X�V
			 (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
			((< (+ kyori (cadr p)) (cadr q)) ; �����̍ŒZ���������Z����
 			 (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
			(t q) )))	;�X�V�̕K�v�̂Ȃ��ꍇ
	  v ))


;; �ړI�Ffold_right�֐��̎���
;; ���́F�֐��A���X�g�A�����v�f
;; �o�́F���X�g�S�̂ɓn���Ċ֐���K�p��������
(defun fold-right (fn lst init)
  (cond ((null lst) init)
	(t (funcall fn (car lst) (fold-right fn (cdr lst) init))) ))

;; �ړI�F�w���X�g�̒�����ŒZ�̉w�Ƃ��̉w���������w���X�g��Ԃ��֐�
;; ���́F�w���X�g
;; �o�́F(�ŒZ�̉w�@�ŒZ�w���������w�̃��X�g)�̃��X�g
(defun saitan-wo-bunri (eki-list)
  (fold-right #'(lambda (x y)
		  (cond ((and (null (car y))
			      (null (cadr x)) )
			 (list nil (append (list x) (cadr y))) )
		   ((null (car y))
		    (list x (cadr y)) ) 
		   ((null (cadr x))
		    (list (car y) (append (list x) (cadr y))) )
		   ((< (cadr x) (cadar y)) ;�ŒZ���X�V
		    (list x (append (list (car y)) (cadr y))) )
		   (t (list (car y) (append (list x) (cadr y)))) ))
	      eki-list
	      (list nil nil) ))

;; �ړI�F�e�w�ɂ��čŒZ�����ƍŒZ�o�H�����������������X�g��Ԃ��֐�
;; ���́F���m��̉w�̃��X�g�Ɖw�ԃ��X�g
;; �o�́F�v�f�̍ŒZ�����ƍŒZ�o�H���������ݒ肳�ꂽ���X�g
(defun dijkstra-main (eki-list ekikan-list)
  (cond ((null eki-list) nil)
	(t (let ((result (saitan-wo-bunri eki-list)))
	     (cons (car result)
		   (dijkstra-main (koushin (car result) (cadr result) ekikan-list) ekikan-list)) ))))

;; �ړI�F�w���X�g����n���ꂽ�w��Ԃ��֐�
;; ���́F�w���X�g�A�w��
;; �o�́F�ړI�̉w
(defun find-eki (ekimei eki-list)
  (cond ((null eki-list) nil)
	((string= ekimei (caar eki-list))
	 (car eki-list))
	(t (find-eki ekimei (cdr eki-list))) ))

;; �ړI�F�_�C�N�X�g���@�̃��C���֐�
;; ���́F�n�_�ƏI�_�̉w�i���[�}���j
;; �o�́F�ŒZ�o�H���������X�g
(defun dijkstra (shiten shuten ekimei-list ekikan-list)
  (let ((k-shiten (romaji-to-kanji shiten ekimei-list))
	(k-shuten (romaji-to-kanji shuten ekimei-list)) )
    (let ((eki-list (make-initial-eki-list ekimei-list k-shiten)))
      (let ((result (dijkstra-main eki-list ekikan-list)))
	(find-eki k-shuten result) ))))
