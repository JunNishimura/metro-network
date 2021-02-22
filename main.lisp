;; main.lisp
(in-package :cl-user)
(defpackage :cl-metro
  (:use cl)
  (:export :romaji-to-kanji
           :get-ekikan-kyori
	   :kyori-wo-hyouji
           :make-eki-list
           :shokika
           :seiretsu
	   :koushin
	   :make-initial-eki-list))
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
	(t (let ((kiten (caar ekikan-list))
		 (shuten (cadar ekikan-list)) )
	     (cond ((or (and (string= eki1 kiten)
			     (string= eki2 shuten) )
			(and (string= eki1 shuten)
			     (string= eki2 kiten) ))
		    (cadddr (car ekikan-list)) )
		   (t (get-ekikan-kyori eki1 eki2 (cdr ekikan-list))) )))))

;; 目的：ローマ字の駅名2つを受け取り、その間の距離を調べて文字列を返す関数
(defun kyori-wo-hyouji (r-eki1 r-eki2 ekimei-list ekikan-list)
  (let ((k-eki1 (romaji-to-kanji r-eki1 ekimei-list))
	(k-eki2 (romaji-to-kanji r-eki2 ekimei-list)) )
    (cond ((null k-eki1) (format t "There is no station ~A.~%" r-eki1))
	  ((null k-eki2) (format t "There is no station ~A.~%" r-eki2))
	  (t (let ((kyori (get-ekikan-kyori k-eki1 k-eki2 ekikan-list)))
	       (cond ((null kyori) (format t "station ~A and station ~A is not connected~%" k-eki1 k-eki2))
		     (t (format t "the distance between ~A and ~A is ~F.~%" k-eki1 k-eki2 kyori)) ))))))

;; 目的：(namae, saitan-kyori, temae-list)から成るリストの作成
(defun make-eki-list (ekimei-list)
  (mapcar #'(lambda (x) (list (car x) nil nil)) ekimei-list) )


;; 目的：与えられた駅で初期化する
;; 入力：駅リストと出発点となる駅名（漢字)
;; 出力：初期化された駅リスト
(defun shokika (eki-list eki)
  (mapcar #'(lambda (x)
	      (cond ((string= eki (car x))
		     (list eki 0 (list eki)) )
		    (t x)))
	  eki-list ))

;; 目的：(namae saitan-kyori temae-list)の構造から成る駅リストの作成、及び初期化
;; 入力：駅名リストと出発点となる駅名（漢字）
;; 出力：初期化済みの駅リスト
(defun make-initial-eki-list (ekimei-list eki)
  (mapcar #'(lambda (x)
	      (cond ((string= eki (car x))
		     (list eki 0 (list eki)) )
		    (t (list (car x) nil nil)) ))
	  ekimei-list ))


;; 目的：昇順に並んでいるリストの正しい位置に新しい駅を挿入する
;; 入力：昇順ソート済みのリスト, 新しい駅
;; 出力：新しい駅を追加したソート済みのリスト
(defun ekimei-insert (ekimei-list eki)
  (cond ((null ekimei-list) (list eki))
	((string= (cadar ekimei-list)(cadr eki)) ;; 同名の駅は除く
	 (ekimei-insert (cdr ekimei-list) eki) )
	((string< (cadar ekimei-list)(cadr eki))
	 (cons (car ekimei-list)
	       (ekimei-insert (cdr ekimei-list) eki) ))
	(t (cons eki ekimei-list)) )) ;; 適切な位置を発見

;; 目的：ekimei-listをひらがな順で昇順ソート
;; 入力：未整列のリスト
;; 出力：整列済みのリスト
(defun seiretsu (ekimei-list)
  (cond ((null ekimei-list) nil)
	(t (ekimei-insert (seiretsu (cdr ekimei-list)) (car ekimei-list))) ))

;; 目的：駅pと駅qがつながっているかを確認し、必要に応じて更新する
;; 入力：駅p, 駅q, ekikan-list
;; 出力：駅q(更新の有無に関係あらず)
(defun koushin1 (p q ekikan-list)
  (let ((kyori (get-ekikan-kyori (car p) (car q) ekikan-list)))
    (cond ((null kyori) q) 		; 駅が繋がっていない場合は駅qをそのまま返す
	  ((null (cadr q))		; 初訪問の時は更新
	   (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
	  ((< (+ kyori (cadr p)) (cadr q)) ; 既存の最短距離よりも短い時
 	   (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
	  (t q) )))			; 更新の必要のない場合

;; 目的：リストの各要素にkoushin1を適用する
;; 入力：直前に確定した駅pと未確定の駅のリストv
;; 出力：適切な処理を施した後のリストv
(defun koushin (p v ekikan-list)
  (mapcar #'(lambda (q)
	        (let ((kyori (get-ekikan-kyori (car p) (car q) ekikan-list)))
		  (cond ((null kyori) q) 		; 駅が繋がっていない場合は駅qをそのまま返す
			((null (cadr q))		; 初訪問の時は更新
			 (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
			((< (+ kyori (cadr p)) (cadr q)) ; 既存の最短距離よりも短い時
 			 (list (car q) (+ kyori (cadr p)) (append (list (car q)) (caddr p))) )
			(t q) )))	;更新の必要のない場合
	  v ))


;; 目的：fold_right関数の実装
;; 入力：関数、リスト、初期要素
;; 出力：リスト全体に渡って関数を適用した結果
(defun fold-right (fn lst init)
  (cond ((null lst) init)
	(t (funcall fn (car lst) (fold-right fn (cdr lst) init))) ))

;; 目的：駅リストの中から最短の駅とその駅を除いた駅リストを返す関数
;; 入力：駅リスト
;; 出力：(最短の駅　最短駅を除いた駅のリスト)のリスト
;; (defun saitan-wo-bunri (eki-list)
;;   (cond ((null eki-list) '(nil nil))
;; 	(t (let ((result (saitan-wo-bunri (cdr eki-list))))
;; 	     (cond ((and (null (car result))
;; 			 (null (cadar eki-list)) )
;; 		    (list nil (append (list (car eki-list)) (cadr result))) )
;; 		   ((null (car result))
;; 		    (list (car eki-list) (cadr result)) )
;; 		   ((null (cadar eki-list))
;; 		    (list (car result) (append (list (car eki-list)) (cadr result))) )
;; 		   ((< (cadar eki-list) (cadar result)) ;最短を更新
;; 		    (list (car eki-list) (append (list (car result)) (cadr result))) )
;; 		   (t (list (car result) (append (list (car eki-list)) (cadr result)))) )))))
;; fold-rightを使用した場合
(defun saitan-wo-bunri (eki-list)
  (fold-right #'(lambda (x y)
		  (cond ((and (null (car y))
			      (null (cadr x)) )
			 (list nil (append (list x) (cadr y))) )
		   ((null (car y))
		    (list x (cadr y)) ) 
		   ((null (cadr x))
		    (list (car y) (append (list x) (cadr y))) )
		   ((< (cadr x) (cadar y)) ;最短を更新
		    (list x (append (list (car y)) (cadr y))) )
		   (t (list (car y) (append (list x) (cadr y)))) ))
	      eki-list
	      (list nil nil) ))
					
(setq eki1 '("池袋" nil nil))
(setq eki2 '("新大塚" 1.2 ("新大塚" "茗荷谷")))
(setq eki3 '("茗荷谷" 0 ("茗荷谷")))
(setq eki4 '("後楽園" nil nil))
(setq eki-list (list eki1 eki2 eki3 eki4))
