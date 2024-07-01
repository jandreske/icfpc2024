
;; Number in Base 9
;; V99 = lowest digit 9^0
;; V11 = highest digit 9^80

;; 11 12 13 14 15 16 17 18
;; 20, 30, 40, 50, 60, 70, 80, 90 missing

(defun same-vars-p (a b)
  (or (and (eq (cadr a) (cadr b))
	   (eq (caddr a) (caddr b)))
      (and (eq (cadr a) (caddr b))
	   (eq (caddr a) (cadr b)))))

(defun mentions-var-p (v term)
  (or (eq v (cadr term))
      (eq v (caddr term))))

(defun var-to-index (v)
  (let ((n (read-from-string (subseq (symbol-name v) 1))))
    (cond ((< n 20) (- n 11))
	  ((< n 30) (- n (- 21 9)))
	  ((< n 40) (- n (- 31 (* 9 2))))
	  ((< n 50) (- n (- 41 (* 9 3))))
	  ((< n 60) (- n (- 51 (* 9 4))))
	  ((< n 70) (- n (- 61 (* 9 5))))
	  ((< n 80) (- n (- 71 (* 9 6))))
	  ((< n 90) (- n (- 81 (* 9 7))))
	  ((< n 100) (- n (- 91 (* 9 8))))
	  (t (error "var-to-index")))))

(defparameter sat-expr
  '(AND
    (NOT (= V11 V12)) (NOT (= V11 V13)) (NOT (= V11 V14)) (NOT (= V11 V15))
    (NOT (= V11 V16)) (NOT (= V11 V17)) (NOT (= V11 V18)) (NOT (= V11 V19))
    (NOT (= V11 V21)) (NOT (= V11 V22)) (NOT (= V11 V23)) (NOT (= V11 V31))
    (NOT (= V11 V32)) (NOT (= V11 V33)) (NOT (= V11 V41)) (NOT (= V11 V51))
    (NOT (= V11 V61)) (NOT (= V11 V71)) (NOT (= V11 V81)) (NOT (= V11 V91))
    (NOT (= V12 V13)) (NOT (= V12 V14)) (NOT (= V12 V15)) (NOT (= V12 V16))
    (NOT (= V12 V17)) (NOT (= V12 V18)) (NOT (= V12 V19)) (NOT (= V12 V21))
    (NOT (= V12 V22)) (NOT (= V12 V23)) (NOT (= V12 V31)) (NOT (= V12 V32))
    (NOT (= V12 V33)) (NOT (= V12 V42)) (NOT (= V12 V52)) (NOT (= V12 V62))
    (NOT (= V12 V72)) (NOT (= V12 V82)) (NOT (= V12 V92)) (NOT (= V13 V14))
    (NOT (= V13 V15)) (NOT (= V13 V16)) (NOT (= V13 V17)) (NOT (= V13 V18))
    (NOT (= V13 V19)) (NOT (= V13 V21)) (NOT (= V13 V22)) (NOT (= V13 V23))
    (NOT (= V13 V31)) (NOT (= V13 V32)) (NOT (= V13 V33)) (NOT (= V13 V43))
    (NOT (= V13 V53)) (NOT (= V13 V63)) (NOT (= V13 V73)) (NOT (= V13 V83))
    (NOT (= V13 V93)) (NOT (= V14 V15)) (NOT (= V14 V16)) (NOT (= V14 V17))
    (NOT (= V14 V18)) (NOT (= V14 V19)) (NOT (= V14 V24)) (NOT (= V14 V25))
    (NOT (= V14 V26)) (NOT (= V14 V34)) (NOT (= V14 V35)) (NOT (= V14 V36))
    (NOT (= V14 V44)) (NOT (= V14 V54)) (NOT (= V14 V64)) (NOT (= V14 V74))
    (NOT (= V14 V84)) (NOT (= V14 V94)) (NOT (= V15 V16)) (NOT (= V15 V17))
    (NOT (= V15 V18)) (NOT (= V15 V19)) (NOT (= V15 V24)) (NOT (= V15 V25))
    (NOT (= V15 V26)) (NOT (= V15 V34)) (NOT (= V15 V35)) (NOT (= V15 V36))
    (NOT (= V15 V45)) (NOT (= V15 V55)) (NOT (= V15 V65)) (NOT (= V15 V75))
    (NOT (= V15 V85)) (NOT (= V15 V95)) (NOT (= V16 V17)) (NOT (= V16 V18))
    (NOT (= V16 V19)) (NOT (= V16 V24)) (NOT (= V16 V25)) (NOT (= V16 V26))
    (NOT (= V16 V34)) (NOT (= V16 V35)) (NOT (= V16 V36)) (NOT (= V16 V46))
    (NOT (= V16 V56)) (NOT (= V16 V66)) (NOT (= V16 V76)) (NOT (= V16 V86))
    (NOT (= V16 V96)) (NOT (= V17 V18)) (NOT (= V17 V19)) (NOT (= V17 V27))
    (NOT (= V17 V28)) (NOT (= V17 V29)) (NOT (= V17 V37)) (NOT (= V17 V38))
    (NOT (= V17 V39)) (NOT (= V17 V47)) (NOT (= V17 V57)) (NOT (= V17 V67))
    (NOT (= V17 V77)) (NOT (= V17 V87)) (NOT (= V17 V97)) (NOT (= V18 V19))
    (NOT (= V18 V27)) (NOT (= V18 V28)) (NOT (= V18 V29)) (NOT (= V18 V37))
    (NOT (= V18 V38)) (NOT (= V18 V39)) (NOT (= V18 V48)) (NOT (= V18 V58))
    (NOT (= V18 V68)) (NOT (= V18 V78)) (NOT (= V18 V88)) (NOT (= V18 V98))
    (NOT (= V19 V27)) (NOT (= V19 V28)) (NOT (= V19 V29)) (NOT (= V19 V37))
    (NOT (= V19 V38)) (NOT (= V19 V39)) (NOT (= V19 V49)) (NOT (= V19 V59))
    (NOT (= V19 V69)) (NOT (= V19 V79)) (NOT (= V19 V89)) (NOT (= V19 V99))
    (NOT (= V21 V22)) (NOT (= V21 V23)) (NOT (= V21 V24)) (NOT (= V21 V25))
    (NOT (= V21 V26)) (NOT (= V21 V27)) (NOT (= V21 V28)) (NOT (= V21 V29))
    (NOT (= V21 V31)) (NOT (= V21 V32)) (NOT (= V21 V33)) (NOT (= V21 V41))
    (NOT (= V21 V51)) (NOT (= V21 V61)) (NOT (= V21 V71)) (NOT (= V21 V81))
    (NOT (= V21 V91)) (NOT (= V22 V23)) (NOT (= V22 V24)) (NOT (= V22 V25))
    (NOT (= V22 V26)) (NOT (= V22 V27)) (NOT (= V22 V28)) (NOT (= V22 V29))
    (NOT (= V22 V31)) (NOT (= V22 V32)) (NOT (= V22 V33)) (NOT (= V22 V42))
    (NOT (= V22 V52)) (NOT (= V22 V62)) (NOT (= V22 V72)) (NOT (= V22 V82))
    (NOT (= V22 V92)) (NOT (= V23 V24)) (NOT (= V23 V25)) (NOT (= V23 V26))
    (NOT (= V23 V27)) (NOT (= V23 V28)) (NOT (= V23 V29)) (NOT (= V23 V31))
    (NOT (= V23 V32)) (NOT (= V23 V33)) (NOT (= V23 V43)) (NOT (= V23 V53))
    (NOT (= V23 V63)) (NOT (= V23 V73)) (NOT (= V23 V83)) (NOT (= V23 V93))
    (NOT (= V24 V25)) (NOT (= V24 V26)) (NOT (= V24 V27)) (NOT (= V24 V28))
    (NOT (= V24 V29)) (NOT (= V24 V34)) (NOT (= V24 V35)) (NOT (= V24 V36))
    (NOT (= V24 V44)) (NOT (= V24 V54)) (NOT (= V24 V64)) (NOT (= V24 V74))
    (NOT (= V24 V84)) (NOT (= V24 V94)) (NOT (= V25 V26)) (NOT (= V25 V27))
    (NOT (= V25 V28)) (NOT (= V25 V29)) (NOT (= V25 V34)) (NOT (= V25 V35))
    (NOT (= V25 V36)) (NOT (= V25 V45)) (NOT (= V25 V55)) (NOT (= V25 V65))
    (NOT (= V25 V75)) (NOT (= V25 V85)) (NOT (= V25 V95)) (NOT (= V26 V27))
    (NOT (= V26 V28)) (NOT (= V26 V29)) (NOT (= V26 V34)) (NOT (= V26 V35))
    (NOT (= V26 V36)) (NOT (= V26 V46)) (NOT (= V26 V56)) (NOT (= V26 V66))
    (NOT (= V26 V76)) (NOT (= V26 V86)) (NOT (= V26 V96)) (NOT (= V27 V28))
    (NOT (= V27 V29)) (NOT (= V27 V37)) (NOT (= V27 V38)) (NOT (= V27 V39))
    (NOT (= V27 V47)) (NOT (= V27 V57)) (NOT (= V27 V67)) (NOT (= V27 V77))
    (NOT (= V27 V87)) (NOT (= V27 V97)) (NOT (= V28 V29)) (NOT (= V28 V37))
    (NOT (= V28 V38)) (NOT (= V28 V39)) (NOT (= V28 V48)) (NOT (= V28 V58))
    (NOT (= V28 V68)) (NOT (= V28 V78)) (NOT (= V28 V88)) (NOT (= V28 V98))
    (NOT (= V29 V37)) (NOT (= V29 V38)) (NOT (= V29 V39)) (NOT (= V29 V49))
    (NOT (= V29 V59)) (NOT (= V29 V69)) (NOT (= V29 V79)) (NOT (= V29 V89))
    (NOT (= V29 V99)) (NOT (= V31 V32)) (NOT (= V31 V33)) (NOT (= V31 V34))
    (NOT (= V31 V35)) (NOT (= V31 V36)) (NOT (= V31 V37)) (NOT (= V31 V38))
    (NOT (= V31 V39)) (NOT (= V31 V41)) (NOT (= V31 V51)) (NOT (= V31 V61))
    (NOT (= V31 V71)) (NOT (= V31 V81)) (NOT (= V31 V91)) (NOT (= V32 V33))
    (NOT (= V32 V34)) (NOT (= V32 V35)) (NOT (= V32 V36)) (NOT (= V32 V37))
    (NOT (= V32 V38)) (NOT (= V32 V39)) (NOT (= V32 V42)) (NOT (= V32 V52))
    (NOT (= V32 V62)) (NOT (= V32 V72)) (NOT (= V32 V82)) (NOT (= V32 V92))
    (NOT (= V33 V34)) (NOT (= V33 V35)) (NOT (= V33 V36)) (NOT (= V33 V37))
    (NOT (= V33 V38)) (NOT (= V33 V39)) (NOT (= V33 V43)) (NOT (= V33 V53))
    (NOT (= V33 V63)) (NOT (= V33 V73)) (NOT (= V33 V83)) (NOT (= V33 V93))
    (NOT (= V34 V35)) (NOT (= V34 V36)) (NOT (= V34 V37)) (NOT (= V34 V38))
    (NOT (= V34 V39)) (NOT (= V34 V44)) (NOT (= V34 V54)) (NOT (= V34 V64))
    (NOT (= V34 V74)) (NOT (= V34 V84)) (NOT (= V34 V94)) (NOT (= V35 V36))
    (NOT (= V35 V37)) (NOT (= V35 V38)) (NOT (= V35 V39)) (NOT (= V35 V45))
    (NOT (= V35 V55)) (NOT (= V35 V65)) (NOT (= V35 V75)) (NOT (= V35 V85))
    (NOT (= V35 V95)) (NOT (= V36 V37)) (NOT (= V36 V38)) (NOT (= V36 V39))
    (NOT (= V36 V46)) (NOT (= V36 V56)) (NOT (= V36 V66)) (NOT (= V36 V76))
    (NOT (= V36 V86)) (NOT (= V36 V96)) (NOT (= V37 V38)) (NOT (= V37 V39))
    (NOT (= V37 V47)) (NOT (= V37 V57)) (NOT (= V37 V67)) (NOT (= V37 V77))
    (NOT (= V37 V87)) (NOT (= V37 V97)) (NOT (= V38 V39)) (NOT (= V38 V48))
    (NOT (= V38 V58)) (NOT (= V38 V68)) (NOT (= V38 V78)) (NOT (= V38 V88))
    (NOT (= V38 V98)) (NOT (= V39 V49)) (NOT (= V39 V59)) (NOT (= V39 V69))
    (NOT (= V39 V79)) (NOT (= V39 V89)) (NOT (= V39 V99)) (NOT (= V41 V42))
    (NOT (= V41 V43)) (NOT (= V41 V44)) (NOT (= V41 V45)) (NOT (= V41 V46))
    (NOT (= V41 V47)) (NOT (= V41 V48)) (NOT (= V41 V49)) (NOT (= V41 V51))
    (NOT (= V41 V52)) (NOT (= V41 V53)) (NOT (= V41 V61)) (NOT (= V41 V62))
    (NOT (= V41 V63)) (NOT (= V41 V71)) (NOT (= V41 V81)) (NOT (= V41 V91))
    (NOT (= V42 V43)) (NOT (= V42 V44)) (NOT (= V42 V45)) (NOT (= V42 V46))
    (NOT (= V42 V47)) (NOT (= V42 V48)) (NOT (= V42 V49)) (NOT (= V42 V51))
    (NOT (= V42 V52)) (NOT (= V42 V53)) (NOT (= V42 V61)) (NOT (= V42 V62))
    (NOT (= V42 V63)) (NOT (= V42 V72)) (NOT (= V42 V82)) (NOT (= V42 V92))
    (NOT (= V43 V44)) (NOT (= V43 V45)) (NOT (= V43 V46)) (NOT (= V43 V47))
    (NOT (= V43 V48)) (NOT (= V43 V49)) (NOT (= V43 V51)) (NOT (= V43 V52))
    (NOT (= V43 V53)) (NOT (= V43 V61)) (NOT (= V43 V62)) (NOT (= V43 V63))
    (NOT (= V43 V73)) (NOT (= V43 V83)) (NOT (= V43 V93)) (NOT (= V44 V45))
    (NOT (= V44 V46)) (NOT (= V44 V47)) (NOT (= V44 V48)) (NOT (= V44 V49))
    (NOT (= V44 V54)) (NOT (= V44 V55)) (NOT (= V44 V56)) (NOT (= V44 V64))
    (NOT (= V44 V65)) (NOT (= V44 V66)) (NOT (= V44 V74)) (NOT (= V44 V84))
    (NOT (= V44 V94)) (NOT (= V45 V46)) (NOT (= V45 V47)) (NOT (= V45 V48))
    (NOT (= V45 V49)) (NOT (= V45 V54)) (NOT (= V45 V55)) (NOT (= V45 V56))
    (NOT (= V45 V64)) (NOT (= V45 V65)) (NOT (= V45 V66)) (NOT (= V45 V75))
    (NOT (= V45 V85)) (NOT (= V45 V95)) (NOT (= V46 V47)) (NOT (= V46 V48))
    (NOT (= V46 V49)) (NOT (= V46 V54)) (NOT (= V46 V55)) (NOT (= V46 V56))
    (NOT (= V46 V64)) (NOT (= V46 V65)) (NOT (= V46 V66)) (NOT (= V46 V76))
    (NOT (= V46 V86)) (NOT (= V46 V96)) (NOT (= V47 V48)) (NOT (= V47 V49))
    (NOT (= V47 V57)) (NOT (= V47 V58)) (NOT (= V47 V59)) (NOT (= V47 V67))
    (NOT (= V47 V68)) (NOT (= V47 V69)) (NOT (= V47 V77)) (NOT (= V47 V87))
    (NOT (= V47 V97)) (NOT (= V48 V49)) (NOT (= V48 V57)) (NOT (= V48 V58))
    (NOT (= V48 V59)) (NOT (= V48 V67)) (NOT (= V48 V68)) (NOT (= V48 V69))
    (NOT (= V48 V78)) (NOT (= V48 V88)) (NOT (= V48 V98)) (NOT (= V49 V57))
    (NOT (= V49 V58)) (NOT (= V49 V59)) (NOT (= V49 V67)) (NOT (= V49 V68))
    (NOT (= V49 V69)) (NOT (= V49 V79)) (NOT (= V49 V89)) (NOT (= V49 V99))
    (NOT (= V51 V52)) (NOT (= V51 V53)) (NOT (= V51 V54)) (NOT (= V51 V55))
    (NOT (= V51 V56)) (NOT (= V51 V57)) (NOT (= V51 V58)) (NOT (= V51 V59))
    (NOT (= V51 V61)) (NOT (= V51 V62)) (NOT (= V51 V63)) (NOT (= V51 V71))
    (NOT (= V51 V81)) (NOT (= V51 V91)) (NOT (= V52 V53)) (NOT (= V52 V54))
    (NOT (= V52 V55)) (NOT (= V52 V56)) (NOT (= V52 V57)) (NOT (= V52 V58))
    (NOT (= V52 V59)) (NOT (= V52 V61)) (NOT (= V52 V62)) (NOT (= V52 V63))
    (NOT (= V52 V72)) (NOT (= V52 V82)) (NOT (= V52 V92)) (NOT (= V53 V54))
    (NOT (= V53 V55)) (NOT (= V53 V56)) (NOT (= V53 V57)) (NOT (= V53 V58))
    (NOT (= V53 V59)) (NOT (= V53 V61)) (NOT (= V53 V62)) (NOT (= V53 V63))
    (NOT (= V53 V73)) (NOT (= V53 V83)) (NOT (= V53 V93)) (NOT (= V54 V55))
    (NOT (= V54 V56)) (NOT (= V54 V57)) (NOT (= V54 V58)) (NOT (= V54 V59))
    (NOT (= V54 V64)) (NOT (= V54 V65)) (NOT (= V54 V66)) (NOT (= V54 V74))
    (NOT (= V54 V84)) (NOT (= V54 V94)) (NOT (= V55 V56)) (NOT (= V55 V57))
    (NOT (= V55 V58)) (NOT (= V55 V59)) (NOT (= V55 V64)) (NOT (= V55 V65))
    (NOT (= V55 V66)) (NOT (= V55 V75)) (NOT (= V55 V85)) (NOT (= V55 V95))
    (NOT (= V56 V57)) (NOT (= V56 V58)) (NOT (= V56 V59)) (NOT (= V56 V64))
    (NOT (= V56 V65)) (NOT (= V56 V66)) (NOT (= V56 V76)) (NOT (= V56 V86))
    (NOT (= V56 V96)) (NOT (= V57 V58)) (NOT (= V57 V59)) (NOT (= V57 V67))
    (NOT (= V57 V68)) (NOT (= V57 V69)) (NOT (= V57 V77)) (NOT (= V57 V87))
    (NOT (= V57 V97)) (NOT (= V58 V59)) (NOT (= V58 V67)) (NOT (= V58 V68))
    (NOT (= V58 V69)) (NOT (= V58 V78)) (NOT (= V58 V88)) (NOT (= V58 V98))
    (NOT (= V59 V67)) (NOT (= V59 V68)) (NOT (= V59 V69)) (NOT (= V59 V79))
    (NOT (= V59 V89)) (NOT (= V59 V99)) (NOT (= V61 V62)) (NOT (= V61 V63))
    (NOT (= V61 V64)) (NOT (= V61 V65)) (NOT (= V61 V66)) (NOT (= V61 V67))
    (NOT (= V61 V68)) (NOT (= V61 V69)) (NOT (= V61 V71)) (NOT (= V61 V81))
    (NOT (= V61 V91)) (NOT (= V62 V63)) (NOT (= V62 V64)) (NOT (= V62 V65))
    (NOT (= V62 V66)) (NOT (= V62 V67)) (NOT (= V62 V68)) (NOT (= V62 V69))
    (NOT (= V62 V72)) (NOT (= V62 V82)) (NOT (= V62 V92)) (NOT (= V63 V64))
    (NOT (= V63 V65)) (NOT (= V63 V66)) (NOT (= V63 V67)) (NOT (= V63 V68))
    (NOT (= V63 V69)) (NOT (= V63 V73)) (NOT (= V63 V83)) (NOT (= V63 V93))
    (NOT (= V64 V65)) (NOT (= V64 V66)) (NOT (= V64 V67)) (NOT (= V64 V68))
    (NOT (= V64 V69)) (NOT (= V64 V74)) (NOT (= V64 V84)) (NOT (= V64 V94))
    (NOT (= V65 V66)) (NOT (= V65 V67)) (NOT (= V65 V68)) (NOT (= V65 V69))
    (NOT (= V65 V75)) (NOT (= V65 V85)) (NOT (= V65 V95)) (NOT (= V66 V67))
    (NOT (= V66 V68)) (NOT (= V66 V69)) (NOT (= V66 V76)) (NOT (= V66 V86))
    (NOT (= V66 V96)) (NOT (= V67 V68)) (NOT (= V67 V69)) (NOT (= V67 V77))
    (NOT (= V67 V87)) (NOT (= V67 V97)) (NOT (= V68 V69)) (NOT (= V68 V78))
    (NOT (= V68 V88)) (NOT (= V68 V98)) (NOT (= V69 V79)) (NOT (= V69 V89))
    (NOT (= V69 V99)) (NOT (= V71 V72)) (NOT (= V71 V73)) (NOT (= V71 V74))
    (NOT (= V71 V75)) (NOT (= V71 V76)) (NOT (= V71 V77)) (NOT (= V71 V78))
    (NOT (= V71 V79)) (NOT (= V71 V81)) (NOT (= V71 V82)) (NOT (= V71 V83))
    (NOT (= V71 V91)) (NOT (= V71 V92)) (NOT (= V71 V93)) (NOT (= V72 V73))
    (NOT (= V72 V74)) (NOT (= V72 V75)) (NOT (= V72 V76)) (NOT (= V72 V77))
    (NOT (= V72 V78)) (NOT (= V72 V79)) (NOT (= V72 V81)) (NOT (= V72 V82))
    (NOT (= V72 V83)) (NOT (= V72 V91)) (NOT (= V72 V92)) (NOT (= V72 V93))
    (NOT (= V73 V74)) (NOT (= V73 V75)) (NOT (= V73 V76)) (NOT (= V73 V77))
    (NOT (= V73 V78)) (NOT (= V73 V79)) (NOT (= V73 V81)) (NOT (= V73 V82))
    (NOT (= V73 V83)) (NOT (= V73 V91)) (NOT (= V73 V92)) (NOT (= V73 V93))
    (NOT (= V74 V75)) (NOT (= V74 V76)) (NOT (= V74 V77)) (NOT (= V74 V78))
    (NOT (= V74 V79)) (NOT (= V74 V84)) (NOT (= V74 V85)) (NOT (= V74 V86))
    (NOT (= V74 V94)) (NOT (= V74 V95)) (NOT (= V74 V96)) (NOT (= V75 V76))
    (NOT (= V75 V77)) (NOT (= V75 V78)) (NOT (= V75 V79)) (NOT (= V75 V84))
    (NOT (= V75 V85)) (NOT (= V75 V86)) (NOT (= V75 V94)) (NOT (= V75 V95))
    (NOT (= V75 V96)) (NOT (= V76 V77)) (NOT (= V76 V78)) (NOT (= V76 V79))
    (NOT (= V76 V84)) (NOT (= V76 V85)) (NOT (= V76 V86)) (NOT (= V76 V94))
    (NOT (= V76 V95)) (NOT (= V76 V96)) (NOT (= V77 V78)) (NOT (= V77 V79))
    (NOT (= V77 V87)) (NOT (= V77 V88)) (NOT (= V77 V89)) (NOT (= V77 V97))
    (NOT (= V77 V98)) (NOT (= V77 V99)) (NOT (= V78 V79)) (NOT (= V78 V87))
    (NOT (= V78 V88)) (NOT (= V78 V89)) (NOT (= V78 V97)) (NOT (= V78 V98))
    (NOT (= V78 V99)) (NOT (= V79 V87)) (NOT (= V79 V88)) (NOT (= V79 V89))
    (NOT (= V79 V97)) (NOT (= V79 V98)) (NOT (= V79 V99)) (NOT (= V81 V82))
    (NOT (= V81 V83)) (NOT (= V81 V84)) (NOT (= V81 V85)) (NOT (= V81 V86))
    (NOT (= V81 V87)) (NOT (= V81 V88)) (NOT (= V81 V89)) (NOT (= V81 V91))
    (NOT (= V81 V92)) (NOT (= V81 V93)) (NOT (= V82 V83)) (NOT (= V82 V84))
    (NOT (= V82 V85)) (NOT (= V82 V86)) (NOT (= V82 V87)) (NOT (= V82 V88))
    (NOT (= V82 V89)) (NOT (= V82 V91)) (NOT (= V82 V92)) (NOT (= V82 V93))
    (NOT (= V83 V84)) (NOT (= V83 V85)) (NOT (= V83 V86)) (NOT (= V83 V87))
    (NOT (= V83 V88)) (NOT (= V83 V89)) (NOT (= V83 V91)) (NOT (= V83 V92))
    (NOT (= V83 V93)) (NOT (= V84 V85)) (NOT (= V84 V86)) (NOT (= V84 V87))
    (NOT (= V84 V88)) (NOT (= V84 V89)) (NOT (= V84 V94)) (NOT (= V84 V95))
    (NOT (= V84 V96)) (NOT (= V85 V86)) (NOT (= V85 V87)) (NOT (= V85 V88))
    (NOT (= V85 V89)) (NOT (= V85 V94)) (NOT (= V85 V95)) (NOT (= V85 V96))
    (NOT (= V86 V87)) (NOT (= V86 V88)) (NOT (= V86 V89)) (NOT (= V86 V94))
    (NOT (= V86 V95)) (NOT (= V86 V96)) (NOT (= V87 V88)) (NOT (= V87 V89))
    (NOT (= V87 V97)) (NOT (= V87 V98)) (NOT (= V87 V99)) (NOT (= V88 V89))
    (NOT (= V88 V97)) (NOT (= V88 V98)) (NOT (= V88 V99)) (NOT (= V89 V97))
    (NOT (= V89 V98)) (NOT (= V89 V99)) (NOT (= V91 V92)) (NOT (= V91 V93))
    (NOT (= V91 V94)) (NOT (= V91 V95)) (NOT (= V91 V96)) (NOT (= V91 V97))
    (NOT (= V91 V98)) (NOT (= V91 V99)) (NOT (= V92 V93)) (NOT (= V92 V94))
    (NOT (= V92 V95)) (NOT (= V92 V96)) (NOT (= V92 V97)) (NOT (= V92 V98))
    (NOT (= V92 V99)) (NOT (= V93 V94)) (NOT (= V93 V95)) (NOT (= V93 V96))
    (NOT (= V93 V97)) (NOT (= V93 V98)) (NOT (= V93 V99)) (NOT (= V94 V95))
    (NOT (= V94 V96)) (NOT (= V94 V97)) (NOT (= V94 V98)) (NOT (= V94 V99))
    (NOT (= V95 V96)) (NOT (= V95 V97)) (NOT (= V95 V98)) (NOT (= V95 V99))
    (NOT (= V96 V97)) (NOT (= V96 V98)) (NOT (= V96 V99)) (NOT (= V97 V98))
    (NOT (= V97 V99)) (NOT (= V98 V99)))
  )

(defun make-excl-table ()
  (let ((table (make-array '(81 81) :element-type 'bit :initial-element 0)))
    (dolist (cl (cdr sat-expr))
      (let ((v1 (var-to-index (cadr (cadr cl))))
	    (v2 (var-to-index (caddr (cadr cl)))))
	(setf (aref table v1 v2) 1)
	(setf (aref table v2 v1) 1)))
    table))

(declaim (ftype (function (unsigned-byte
			   (integer 0 8)
			   (simple-array (integer 0 8) (81))
			   (simple-array bit (81 81)))
			  t)
		can-fill))
(defun can-fill (i digit digits exclusions)
  (dotimes (k i)
    (when (and (> (aref exclusions i k) 0)
	       (= digit (aref digits k)))
      (return-from can-fill nil)))
  t)

(defun get-number (digits)
  (let ((n 0))
    (dotimes (i 81)
      (setf n (+ (* n 9) (aref digits i))))
    n))
    

(declaim (ftype (function ((simple-array bit (81 81))) list) build-eff9))	
(defun build-eff9 (exclusions)
  (let ((digits (make-array 81 :element-type '(integer 0 8))))
    (labels ((try-fill (i)
	       (block try-fill
		 (if (> i 80)
		     t
		     (progn
		       (dotimes (d 9)
			 (when (can-fill i d digits exclusions)
			   (setf (aref digits i) d)
			   (let ((r (try-fill (+ i 1))))
			     (when r
			       (return-from try-fill t)))))
		       nil)))))
      (if (try-fill 0)
	  (let ((n1 (get-number digits)))
	    (setf digits (nreverse digits))
	    (list n1 (get-number digits)))
	  nil))))

					

(defmacro make-eff-test ()
  (labels ((make-bindings ()
	     (make-bindings-1 0 99 'n nil))
	   (make-bindings-1 (ex vi var bs)
	     (if (> ex 80)
		 bs
		 (make-bindings-1
		  (+ ex 1)
		  (prev-var vi)
		  var
		  (cons `(,(make-var vi) (+ 1 (rem (truncate ,var ,(expt 9 ex)) 9)))
			bs))))
	   (make-vars (vi)
	     (if (< vi 11)
		 nil
		 (cons (make-var vi) (make-vars (prev-var vi)))))
	   (make-var (i)
	     (read-from-string (format nil "V~A" i)))
	   (prev-var (i)
	     (if (= 1 (rem i 10))
		 (- i 2)
		 (- i 1)))
	   )
    `(defun sat-eff9 (n)
       (declare (optimize speed))
       (let ,(make-bindings)
	 (declare (type (integer 1 9) ,@(make-vars 99)))
	 ,sat-expr))))

(declaim (ftype (function ((integer 1 *)) t) sat-eff9))
(make-eff-test)

(defun solve-eff9 ()
  (do ((i 1 (+ i 1)))
      ((sat-eff9 i) i)))

(defvar simplified
  `(APPLY
    (APPLY Y
     (LAMBDA (2 1)
       (APPLY
	(LAMBDA 11
	  (APPLY
	   (LAMBDA 12
	     (APPLY
	      (LAMBDA 13
		(APPLY
		 (LAMBDA 14
		   (APPLY
		    (LAMBDA 15
		      (APPLY
		       (LAMBDA 16
			 (APPLY
			  (LAMBDA 17
			    (APPLY
			     (LAMBDA 18
			       (APPLY
				(LAMBDA 19
				  (APPLY
				   (LAMBDA 21
				     (APPLY
				      (LAMBDA 22
					(APPLY
					 (LAMBDA 23
					   (APPLY
					    (LAMBDA 24
					      (APPLY
					       (LAMBDA 25
						 (APPLY
						  (LAMBDA 26
						    (APPLY
						     (LAMBDA 27
						       (APPLY
							(LAMBDA
							    28
							  (APPLY
							   (LAMBDA
							       29
							     (APPLY
							      (LAMBDA
								  31
								(APPLY
								 (LAMBDA
								     32
								   (APPLY
								    (LAMBDA
									33
								      (APPLY
								       (LAMBDA
									   34
									 (APPLY
									  (LAMBDA
									      35
									    (APPLY
									     (LAMBDA
										 36
									       (APPLY
										(LAMBDA
										    37
										  (APPLY
										   (LAMBDA
										       38
										     (APPLY
										      (LAMBDA
											  39
											(APPLY
											 (LAMBDA
											     41
											   (APPLY
											    (LAMBDA
												42
											      (APPLY
											       (LAMBDA
												   43
												 (APPLY
												  (LAMBDA
												      44
												    (APPLY
												     (LAMBDA
													 45
												       (APPLY
													(LAMBDA
													    46
													  (APPLY
													   (LAMBDA
													       47
													     (APPLY
													      (LAMBDA
														  48
														(APPLY
														 (LAMBDA
														     49
														   (APPLY
														    (LAMBDA
															51
														      (APPLY
														       (LAMBDA
															   52
															 (APPLY
															  (LAMBDA
															      53
															    (APPLY
															     (LAMBDA
																 54
															       (APPLY
																(LAMBDA
																    55
																  (APPLY
																   (LAMBDA
																       56
																     (APPLY
																      (LAMBDA
																	  57
																	(APPLY
																	 (LAMBDA
																	     58
																	   (APPLY
																	    (LAMBDA
																		59
																	      (APPLY
																	       (LAMBDA
																		   61
																		 (APPLY
																		  (LAMBDA
																		      62
																		    (APPLY
																		     (LAMBDA
																			 63
																		       (APPLY
																		       (LAMBDA
																		       64
																		       (APPLY
																		       (LAMBDA
																		       65
																		       (APPLY
																		       (LAMBDA
																		       66
																		       (APPLY
																		       (LAMBDA
																		       67
																		       (APPLY
																		       (LAMBDA
																		       68
																		       (APPLY
																		       (LAMBDA
																		       69
																		       (APPLY
																		       (LAMBDA
																		       71
																		       (APPLY
																		       (LAMBDA
																		       72
																		       (APPLY
																		       (LAMBDA
																		       73
																		       (APPLY
																		       (LAMBDA
																		       74
																		       (APPLY
																		       (LAMBDA
																		       75
																		       (APPLY
																		       (LAMBDA
																		       76
																		       (APPLY
																		       (LAMBDA
																		       77
																		       (APPLY
																		       (LAMBDA
																		       78
																		       (APPLY
																		       (LAMBDA
																		       79
																		       (APPLY
																		       (LAMBDA
																		       81
																		       (APPLY
																		       (LAMBDA
																		       82
																		       (APPLY
																		       (LAMBDA
																		       83
																		       (APPLY
																		       (LAMBDA
																		       84
																		       (APPLY
																		       (LAMBDA
																		       85
																		       (APPLY
																		       (LAMBDA
																		       86
																		       (APPLY
																		       (LAMBDA
																		       87
																		       (APPLY
																		       (LAMBDA
																		       88
																		       (APPLY
																		       (LAMBDA
																		       89
																		       (APPLY
																		       (LAMBDA
																		       91
																		       (APPLY
																		       (LAMBDA
																		       92
																		       (APPLY
																		       (LAMBDA
																		       93
																		       (APPLY
																		       (LAMBDA
																		       94
																		       (APPLY
																		       (LAMBDA
																		       95
																		       (APPLY
																		       (LAMBDA
																		       96
																		       (APPLY
																		       (LAMBDA
																		       97
																		       (APPLY
																		       (LAMBDA
																		       98
																		       (APPLY
																		       (LAMBDA
																		       99
																		       (IF ,sat-expr
																			   V1
																			   (APPLY
																			    V2
																			    (+
																			     V1
																			     1))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  1)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  9)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  81)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  729)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  6561)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  59049)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  531441)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  4782969)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  43046721)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  387420489)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  3486784401)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  31381059609)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  282429536481)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  2541865828329)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  22876792454961)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  205891132094649)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  1853020188851841)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  16677181699666569)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  150094635296999121)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  1350851717672992089)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  12157665459056928801)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  109418989131512359209)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  984770902183611232881)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  8862938119652501095929)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  79766443076872509863361)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  717897987691852588770249)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  6461081889226673298932241)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  58149737003040059690390169)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  523347633027360537213511521)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  4710128697246244834921603689)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  42391158275216203514294433201)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  381520424476945831628649898809)
																			 9))))
																		       (+
																			1
																			(REM
																			 (TRUNCATE
																			  V1
																			  3433683820292512484657849089281)
																			 9))))
																		     (+
																		      1
																		      (REM
																		       (TRUNCATE
																			V1
																			30903154382632612361920641803529)
																		       9))))
																		  (+
																		   1
																		   (REM
																		    (TRUNCATE
																		     V1
																		     278128389443693511257285776231761)
																		    9))))
																	       (+
																		1
																		(REM
																		 (TRUNCATE
																		  V1
																		  2503155504993241601315571986085849)
																		 9))))
																	    (+
																	     1
																	     (REM
																	      (TRUNCATE
																	       V1
																	       22528399544939174411840147874772641)
																	      9))))
																	 (+
																	  1
																	  (REM
																	   (TRUNCATE
																	    V1
																	    202755595904452569706561330872953769)
																	   9))))
																      (+
																       1
																       (REM
																	(TRUNCATE
																	 V1
																	 1824800363140073127359051977856583921)
																	9))))
																   (+
																    1
																    (REM
																     (TRUNCATE
																      V1
																      16423203268260658146231467800709255289)
																     9))))
																(+
																 1
																 (REM
																  (TRUNCATE
																   V1
																   147808829414345923316083210206383297601)
																  9))))
															     (+
															      1
															      (REM
															       (TRUNCATE
																V1
																1330279464729113309844748891857449678409)
															       9))))
															  (+
															   1
															   (REM
															    (TRUNCATE
															     V1
															     11972515182562019788602740026717047105681)
															    9))))
														       (+
															1
															(REM
															 (TRUNCATE
															  V1
															  107752636643058178097424660240453423951129)
															 9))))
														    (+
														     1
														     (REM
														      (TRUNCATE
														       V1
														       969773729787523602876821942164080815560161)
														      9))))
														 (+
														  1
														  (REM
														   (TRUNCATE
														    V1
														    8727963568087712425891397479476727340041449)
														   9))))
													      (+
													       1
													       (REM
														(TRUNCATE
														 V1
														 78551672112789411833022577315290546060373041)
														9))))
													   (+
													    1
													    (REM
													     (TRUNCATE
													      V1
													      706965049015104706497203195837614914543357369)
													     9))))
													(+
													 1
													 (REM
													  (TRUNCATE
													   V1
													   6362685441135942358474828762538534230890216321)
													  9))))
												     (+
												      1
												      (REM
												       (TRUNCATE
													V1
													57264168970223481226273458862846808078011946889)
												       9))))
												  (+
												   1
												   (REM
												    (TRUNCATE
												     V1
												     515377520732011331036461129765621272702107522001)
												    9))))
											       (+
												1
												(REM
												 (TRUNCATE
												  V1
												  4638397686588101979328150167890591454318967698009)
												 9))))
											    (+
											     1
											     (REM
											      (TRUNCATE
											       V1
											       41745579179292917813953351511015323088870709282081)
											      9))))
											 (+
											  1
											  (REM
											   (TRUNCATE
											    V1
											    375710212613636260325580163599137907799836383538729)
											   9))))
										      (+
										       1
										       (REM
											(TRUNCATE
											 V1
											 3381391913522726342930221472392241170198527451848561)
											9))))
										   (+
										    1
										    (REM
										     (TRUNCATE
										      V1
										      30432527221704537086371993251530170531786747066637049)
										     9))))
										(+
										 1
										 (REM
										  (TRUNCATE
										   V1
										   273892744995340833777347939263771534786080723599733441)
										  9))))
									     (+
									      1
									      (REM
									       (TRUNCATE
										V1
										2465034704958067503996131453373943813074726512397600969)
									       9))))
									  (+
									   1
									   (REM
									    (TRUNCATE
									     V1
									     22185312344622607535965183080365494317672538611578408721)
									    9))))
								       (+
									1
									(REM
									 (TRUNCATE
									  V1
									  199667811101603467823686647723289448859052847504205678489)
									 9))))
								    (+
								     1
								     (REM
								      (TRUNCATE
								       V1
								       1797010299914431210413179829509605039731475627537851106401)
								      9))))
								 (+
								  1
								  (REM
								   (TRUNCATE
								    V1
								    16173092699229880893718618465586445357583280647840659957609)
								   9))))
							      (+
							       1
							       (REM
								(TRUNCATE
								 V1
								 145557834293068928043467566190278008218249525830565939618481)
								9))))
							   (+ 1
							      (REM
							       (TRUNCATE
								V1
								1310020508637620352391208095712502073964245732475093456566329)
							       9))))
							(+ 1
							   (REM
							    (TRUNCATE
							     V1
							     11790184577738583171520872861412518665678211592275841109096961)
							    9))))
						     (+ 1
							(REM
							 (TRUNCATE
							  V1
							  106111661199647248543687855752712667991103904330482569981872649)
							 9))))
						  (+ 1
						     (REM
						      (TRUNCATE
						       V1
						       955004950796825236893190701774414011919935138974343129836853841)
						      9))))
					       (+ 1
						  (REM
						   (TRUNCATE V1
							     8595044557171427132038716315969726107279416250769088168531684569)
						   9))))
					    (+ 1
					       (REM
						(TRUNCATE V1
							  77355401014542844188348446843727534965514746256921793516785161121)
						9))))
					 (+ 1
					    (REM
					     (TRUNCATE V1
						       696198609130885597695136021593547814689632716312296141651066450089)
					     9))))
				      (+ 1
					 (REM
					  (TRUNCATE V1
						    6265787482177970379256224194341930332206694446810665274859598050801)
					  9))))
				   (+ 1
				      (REM
				       (TRUNCATE V1
						 56392087339601733413306017749077372989860250021295987473736382457209)
				       9))))
				(+ 1
				   (REM
				    (TRUNCATE V1
					      507528786056415600719754159741696356908742250191663887263627442114881)
				    9))))
			     (+ 1
				(REM
				 (TRUNCATE V1
					   4567759074507740406477787437675267212178680251724974985372646979033929)
				 9))))
			  (+ 1
			     (REM
			      (TRUNCATE V1
					41109831670569663658300086939077404909608122265524774868353822811305361)
			      9))))
		       (+ 1
			  (REM
			   (TRUNCATE V1
				     369988485035126972924700782451696644186473100389722973815184405301748249)
			   9))))
		    (+ 1
		       (REM
			(TRUNCATE V1
				  3329896365316142756322307042065269797678257903507506764336659647715734241)
			9))))
		 (+ 1
		    (REM
		     (TRUNCATE V1
			       29969067287845284806900763378587428179104321131567560879029936829441608169)
		     9))))
	      (+ 1
		 (REM
		  (TRUNCATE V1
			    269721605590607563262106870407286853611938890184108047911269431464974473521)
		  9))))
	   (+ 1
	      (REM
	       (TRUNCATE V1
			 2427494450315468069358961833665581682507450011656972431201424883184770261689)
	       9))))
	(+ 1
	   (REM
	    (TRUNCATE V1
		      21847450052839212624230656502990235142567050104912751880812823948662932355201)
	    9)))))
    1))

