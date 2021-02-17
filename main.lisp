;; main.lisp
(in-package :cl-user)
(defpackage :cl-metro
  (:use cl)
  (:export :romaji-to-kanji :get-ekikan-kyori) )
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
	((or (string= eki1 (caar ekikan-list))
	     (string= eki2 (caar ekikan-list)))
	 (cadddr (car ekikan-list)) ) ;; ������Ԃ�
	(t (get-ekikan-kyori eki1 eki2 (cdr ekikan-list))) ))
