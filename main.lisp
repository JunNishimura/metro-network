;; main.lisp
(in-package :cl-user)
(defpackage :cl-metro
  (:use cl)
  (:export :romaji-to-kanji :get-ekikan-kyori) )
(in-package :cl-metro)

;; 目的：ローマ字名で受け取った駅の漢字表記を返す関数
(defun romaji-to-kanji (romaji ekimei-list)
  (cond ((null ekimei-list) nil)
	((string= romaji (caddar ekimei-list))
	 (caar ekimei-list))
	(t (romaji-to-kanji romaji (cdr ekimei-list))) ))

;; 目的：漢字表記の駅名からその2駅間の距離を返す関数。2駅が繋がっていない場合はnilを返す
(defun get-ekikan-kyori (eki1 eki2 ekikan-list)
  (cond ((null ekikan-list) nil)
	((or (string= eki1 (caar ekikan-list))
	     (string= eki2 (caar ekikan-list)))
	 (cadddr (car ekikan-list)) ) ;; 距離を返す
	(t (get-ekikan-kyori eki1 eki2 (cdr ekikan-list))) ))
