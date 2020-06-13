#|
    Assignment 05: More recursion
    Program that works with lists to compute binary numbers, reverse and rotate lists.

    Samantha Barco Mejia A01196844
    17/04/2020
|#

; notas: 
; Llave y n fuera del documento p/ambos LONG ALPH
; Eliminar sobra de caracteres 
; NO Globales

#lang racket

(define alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.  #\?  #\,  #\-  #\_))

(define (decipher-document input-path output-path)
	; DUDA Define
	; DUDA global key n
	(define content (file->lines input-path))
	(define n (string->number (car content)))
	(define key (cadr content))

	(if (empty? content)
		(list->file '("Invalid document") output-path) ; Document was empty
		(if (or (false? n) (equal? 0 n))
			(list->file '("Invalid n size") output-path) ; Document was empty
			(if (equal? key "")
				(list->file '("Invalid key") output-path)
				(if (equal? (determinant (set-key key n) n) 0)
					(list->file '("Invalid key") output-path)
					(let loop
						([to-cipher (cddr content)]
						[result null]
						)
						(if (empty? to-cipher)
							(list->file (append (list (number->string n)) (append (list key) result)) output-path)
							(loop (cdr to-cipher) (append result (list (decipher (car to-cipher) key n))))
						)
					)
				)
			)
		)
	)
)

(define (cipher-document input-path output-path)
	; DUDA Define
	; DUDA Catch error on empty doc
	; DUDA ELIMINAR LAS Xs del final
	(define content (file->lines input-path))
	(define n (string->number (car content)))
	(define key (cadr content))

	(if (empty? content)
		(list->file '("Invalid document") output-path) ; Document was empty
		(if (or (false? n) (equal? 0 n))
			(list->file '("Invalid n size") output-path) ; Document was empty
			(if (equal? key "")
				(list->file '("Invalid key") output-path)
				(if (equal? (determinant (set-key key n) n) 0)
					(list->file '("Invalid key") output-path)
					(let loop
						([to-cipher (cddr content)]
						[result null]
						)
						(if (empty? to-cipher)
							(list->file (append (list (number->string n)) (append (list key) result)) output-path)
							(loop (cdr to-cipher) (append result (list (cipher (car to-cipher) key n))))
						)
					)
				)
			)
		)
	)
)

(define (decipher word key n)
	; DUDA define bien planteado
	(define matrix-key (set-key key n))
	(define det (modulo (determinant matrix-key n) (length alphabet)))
	(if (equal? 0 det)
		0
		(list->string (number-to-word (join-matrix(matrix-modulo (multiply-matrices (get-decrypt-key matrix-key n (determinant-inverse det)) (set-word word n))))))
	)
)

(define (get-decrypt-key matrix n det-inv)
	(matrix-modulo (multiply-matrix-element (matrix-modulo (get-adjugate (cofactor matrix n) n)) det-inv))
)

(define (get-adjugate matrix n)
	(let loopRows
		([indexRow 0]
		[matrix matrix]
		[result null]
		[n n]
		)
		(if (equal? indexRow n)
			result
			(let loopColumns
				([indexColumn 0]
				[indexRow indexRow]
				[matrix matrix]
				[innerResult null]
				[n n]
				)
				(if (equal? indexColumn n)
					(loopRows (+ 1 indexRow) matrix (append result (list innerResult)) n)
					(loopColumns (+ 1 indexColumn) indexRow matrix (append innerResult (list (get-matrix-element matrix indexColumn indexRow))) n)
				)
			)
		)
	)
)

(define (get-matrix-element matrix row column)
	(let loopRows
		([indexRow 0]
		[column column]
		[row row]
		[matrix matrix]
		)
		(if (equal? indexRow row)
			(get-list-element (car matrix) column)
			(if (> indexRow row)
				0
				(loopRows (+ 1 indexRow) column row (cdr matrix))
			)
		)
	)
)

(define (cofactor matrix n)
	(let loopRows
		([matrix matrix]
		[indexRow 0]
		[n n]
		[result null]
		[symbolOut 1]
		)
		(if (< indexRow n)
			(let loopColumns
				([matrix matrix]
				[indexColumn 0]
				[n n]
				[innerResult null]
				[symbolIn symbolOut ]
				)
				(if (< indexColumn n)
					(loopColumns
						matrix
						(+ 1 indexColumn)
						n
						(append innerResult (list (* symbolIn (determinant (delete-column (delete-row matrix indexRow) indexColumn) (- n 1)))))
						(- 0 symbolIn)
					)
					(loopRows
						matrix
						(+ 1 indexRow)
						n
						(append result (list innerResult))
						(- 0 symbolOut)
					)
				)
			)
			result
		)
	)
)


(define (determinant-inverse determinant)
	(let loop
		([base determinant]
		[inverse 1]
		[mod (length alphabet)]
		)
		(if (< inverse mod)
			(if (equal? (modulo (* inverse base) mod) 1) 
				inverse
				(loop base (+ 1 inverse) mod)
			)
			-1
		)
	)
)

(define (determinant matrix n)	
	(define sub-matrices 
		(let outerLoop
			([arrays (cdr matrix)]
			[outerIndex 0]
			[outerResult null]
			)
			(if (equal? outerIndex n) 
				outerResult; Limit has been reached must stop
				(let innerLoop ; loop para armar sub arreglos
					([arrays arrays]
					[innerArrays arrays]
					[innerIndex outerIndex]
					[innerResult null]
					)
					(if (empty? innerArrays)
						(outerLoop arrays (+ 1 innerIndex) (append outerResult (list innerResult)))
						(innerLoop arrays (cdr innerArrays) innerIndex (append innerResult (list (delete-row (car innerArrays) innerIndex))))
					)
				)
			)
		)
	)
	
	(if (equal? n 1)
		(caar matrix)
		(let loop
			([length n]
			[length-multiplier (length (car sub-matrices))]
			[multiplicand (car matrix)]
			[multiplier sub-matrices]
			[symbol 1]
			[index 0]
			[result 0]
			)
			(if (< index length)
				(if (eqv? 1 length-multiplier)
					(loop length length-multiplier (cdr multiplicand) (cdr multiplier) (- 0 symbol) (+ 1 index) (+ result (* symbol (* (car multiplicand) (caaar multiplier)))))
					(loop length length-multiplier (cdr multiplicand) (cdr multiplier) (- 0 symbol) (+ 1 index) (+ result (* symbol (* (car multiplicand) (determinant (car multiplier) (- n 1))))))
				)
				result
			)
		)
	)
)


(define (delete-column matrix n)
	(let loop
		([matrix matrix]
		[column n]
		[result null]
		)
		(if (empty? matrix)
			result
			(let innerLoop
				([row (car matrix)]
				[index 0]
				[column column]
				[innerResult null]
				)
				(if (empty? row)
					(loop (cdr matrix) n (append result (list innerResult)))
					(if (equal? index column)
						(innerLoop (cdr row) (+ 1 index) column innerResult)
						(innerLoop (cdr row) (+ 1 index) column (append innerResult (list (car row))))
					)
				)
			)
		)
	)
)

(define (delete-row array n)
	(let loop
		([index 0]
		[length (length array)]
		[n n]
		[array array]
		[result null]
		)
		(if (equal? index n)
			(loop (+ 1 index) length n (cdr array) result)
			(if (< index length)
				(loop (+ 1 index) length n (cdr array) (append result (list (car array))))
				result
			)
		)
	)
)

(define (get-list-element list n)
	(let loop
		([index 0]
		[n n]
		[list list]
		)
		(if (equal? index n)
			(car list)
			(if (empty? list)
				#\x
				(loop (+ 1 index) n (cdr list))
			)
		)
	)
)

(define (cipher word key n)
	(if (equal? 0 (modulo (determinant (set-key key n) n) (length alphabet)))
		0
		(list->string (number-to-word (join-matrix (matrix-modulo (multiply-matrices (set-key key n) (set-word word n))))))
	)
)

(define (number-to-word m)
	(let loop
		([original m]
		[result null]
		)
		(if (empty? original)
			result
			(loop (cdr original) (append result (list (get-letter (car original)))))
		)
	)
)

(define (join-matrix m)
	(let loop
		([original m]
		[result null]
		)
		(if (empty? original)
			result
			(loop (cdr original) (append result (car original)))
		)
	)
)

(define (matrix-modulo m)
	(let loop
		([outerList m]
		[outerResult null]
		)
		(if (null? outerList)
			outerResult
			(let loop2
				([innerList (car outerList)]
				[innerResult null]
				[n (length alphabet)]
				)
				(if (empty? innerList)
					(loop (cdr outerList) (append outerResult (list innerResult)))
					(loop2 (cdr innerList) (append innerResult (list (modulo (car innerList) n))) n)
				)
			)
		)
	)
)

(define (set-key key n)
	(key-to-matrix3 (word-to-number key) n)
)

(define (set-word word n)
	(word-to-matrix2 (word-to-number word) n)
)

(define (word-to-number word)
	(let loop
		([word (string->list word)]
		[result null]
		)
		(if (empty? word)
			result
			(loop
				(cdr word)
				(append result (list (get-number (car word)))))
		)
	)
)

(define (get-letter number)
	(let loop
		([count 0]
		[newAlphabet alphabet]
		[number number]
		)
		(if (and (equal? count number) (< count (length alphabet)))
			(car newAlphabet)
			(loop (+ count 1) (cdr newAlphabet) number)
		)
	)
)

(define (get-number letter)
	(let loop
		([count 0]
		[newAlphabet alphabet]
		[letter letter]
		)
		(if (and (char=? letter (car newAlphabet)) (< count (length alphabet)))
			count
			(loop (+ count 1) (cdr newAlphabet) letter)
		)
	)
)

(define (key-to-matrix3 word n)
	(let loop
		([word word]
		[count 0]
		[n n]
		[result null]
		)
		(if (< count n) ; loop para renglones
			(if (empty? word)
				(let loop3
					(
					[n n]
					[countIn 0]
					[countOut count]
					[sub null]
					)
					(if (< countIn n)
						(loop3 n (+ countIn 1) countOut (append sub '(23)))
						(loop word (+ countOut 1) n (append result (list sub)))
					)
				)
				(let loop2
					([word word]
					[n n]
					[countIn 0]
					[countOut count]
					[sub null]
					)
					(if (< countIn n)
						(if (false? (empty? word))
							(loop2 (cdr word) n (+ countIn 1) countOut (append sub (list (car word))))
							(loop2 word n (+ countIn 1) countOut (append sub '(23)))
						)
						(loop word (+ countOut 1) n (append result (list sub)))
					)
				)
			)
			result
		)
	)
)

(define (key-to-matrix word n)
	(let loop
		([word word]
		[count 0]
		[n n]
		[result null]
		)
		(if (< count n) ; loop para renglones
			(if (string=? word "")
				(let loop3
					(
					[n n]
					[countIn 0]
					[countOut count]
					[sub null]
					)
					(if (< countIn n)
						(loop3 n (+ countIn 1) countOut (append sub '(23)))
						(loop word (+ countOut 1) n (append result (list sub)))
					)
				)
				(let loop2
					([word word]
					[n n]
					[countIn 0]
					[countOut count]
					[sub null]
					)
					(if (< countIn n)
						(if (false? (string=? word ""))
							(loop2 (substring word 1) n (+ countIn 1) countOut (append sub (list (substring word 0 1))))
							(loop2 word n (+ countIn 1) countOut (append sub '(23)))
						)
						(loop word (+ countOut 1) n (append result (list sub)))
					)
				)
			)
			(display result)
		)
	)
)

(define (word-to-matrix2 word n)
	(let loop
		([word word]
		[n n]
		[result null]
		)
		(if (empty? word)
			result
			(let loop2
				([word word]
				[n n]
				[count 0]
				[sub null]
				)
				(if (< count n)
					(if (false? (empty? word))
						(loop2 (cdr word) n (+ count 1) (append sub (list (car word))))
						(loop2 word n (+ count 1) (append sub '(23)))
					)
					(loop word n (append result (list sub)))
				)
			)
		)
	)
)

(define (word-to-matrix word n)
	(let loop
		([word word]
		[n n]
		[result null]
		)
		(if (string=? word "")
			(display result)
			(let loop2
				([word word]
				[n n]
				[count 0]
				[sub null]
				)
				(if (< count n)
					(if (false? (string=? word ""))
						(loop2 (substring word 1) n (+ count 1) (append sub (list (substring word 0 1))))
						(loop2 word n (+ count 1) (append sub '(x)))
					)
					(loop word n (append result (list sub)))
				)
			)
		)
	)
)

(define (multiply-matrices m1 m2) ; m1 is key m2 is word
	(let loop
		([m1 m1]
		[m2 m2]
		[res '()]
		)
		(if (empty? m2)
			res ; If the lists are empty, returns the result
			(loop m1 (cdr m2) (append res (list (multiply-matrix-list m1 (car m2))))) ; If the lists aren't empty increases the multiplication
		)
	)
)

(define (multiply-matrix-list m1 l1)
	; Lists have same lenth
	(let loop
		([m1 m1]
		[l1 l1]
		[res '()]
		)
		(if (empty? m1)
			res ; If the lists are empty, returns the result
			(loop (cdr m1) l1 (append res (list (multiply-lists (car m1) l1)))) ; If the lists aren't empty increases the multiplication
		)
	)
)

(define (multiply-lists l1 l2)
	(if (equal? (length l1) (length l2))
		; Lists have same lenth
		(let loop
			([l1 l1]
			[l2 l2]
			[res 0]
			)
			(if (empty? l1)
				res ; If the lists are empty, returns the result
				(loop (cdr l1) (cdr l2) (+ res (* (car l1) (car l2)))) ; If the lists aren't empty increases the multiplication
			)
		)
		; Lists don't have same length
		(- 0 1)		
	)
)

(define (multiply-matrix-element matrix multiplier)
	(let loopRows
		([matrix matrix]
		[result null]
		[multiplier multiplier]
		)
		(if (empty? matrix)
			result
			(let loopColumns
				([innerMatrix (car matrix)]
				[innerResult null]
				[multiplier multiplier]
				)
				(if (empty? innerMatrix)
					(loopRows (cdr matrix) (append result (list innerResult)) multiplier)
					(loopColumns (cdr innerMatrix) (append innerResult (list (* multiplier (car innerMatrix)))) multiplier)
				)
			)
		)
	)
)

; https://stackoverflow.com/questions/30729513/write-list-to-file-using-display-lines-to-file
(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'text))