#|

    Samantha Barco Mejia   A01196844
    Emilio Hernandez Lopez A013364189
    16/06/2020

    

    Final Project Hill Cipher
|#

; Eliminar sobra de caracteres 

#lang racket

(define alphabet '(
#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j 
#\k #\l #\m #\n #\o #\p #\q #\r #\s #\t 
#\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 
#\4 #\5 #\6 #\7 #\8 #\9 #\. #\? #\, #\- 
#\  #\! #\_ #\@ #\$ #\" #\' #\A #\B #\C
#\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W
#\X #\Y #\Z ))


(define (decipher-document input-path output-path n key)
	;Function to decipher a word
	(define (decipher word key n)

		;Transforms encrypting key to decrypting key
		(define (get-decrypt-key matrix n det-inv)

			;Returns adjugate matrix from a given matrix
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

			;Returns cofactor from a matrix
			(define (cofactor matrix n)

				;Deletes a column n from a matrix
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


			;Multiplies all matrix elements by a constant multiplier
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

			(matrix-modulo (multiply-matrix-element (matrix-modulo (get-adjugate (cofactor matrix n) n)) det-inv))
		)

		;Returns an inverse from a determinant
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

		(define matrix-key (set-key key n))
		(define det (modulo (determinant matrix-key n) (length alphabet)))
		(if (equal? 0 det)
			0
			(list->string (number-to-word (join-matrix(matrix-modulo (multiply-matrices (get-decrypt-key matrix-key n (determinant-inverse det)) (set-word word n))))))
		)
	)
	(define content (file->lines input-path))

	; Document was empty
	(if (empty? content)
		(list->file '("Invalid document") output-path) 

		;Matrix size is not valid
		(if (or (false? n) (< n 1))
			(list->file '("Invalid n size") output-path) 

			;Key is empty			
			(if (equal? key "")
				(list->file '("Invalid key") output-path)

				;Invalid key- determinant 0
				(if (equal? (determinant (set-key key n) n) 0)
					(list->file '("Invalid key: Determinant 0") output-path)

					;Deciphers document
					(let loop
						([to-cipher content]
						[result null]
						)

						(if (empty? to-cipher)
							(list->file result output-path)
							(loop (cdr to-cipher) (append result (list (decipher (car to-cipher) key n))))
						)
					)
				)
			)
		)
	)
)

;Ciphers a given text document, given a path to the document
(define (cipher-document input-path output-path n key)

	;Ciphers a word using matrices
	(define (cipher word key n)
		(if (equal? 0 (modulo (determinant (set-key key n) n) (length alphabet)))
			0
			(list->string (number-to-word (join-matrix (matrix-modulo (multiply-matrices (set-key key n) (set-word word n))))))
		)
	)

	(define content (file->lines input-path))

	; Document was empty
	(if (empty? content)
		(list->file '("Invalid document") output-path) 

		;Matrix size is not valid
		(if (or (false? n) (< n 1))
			(list->file '("Invalid n size") output-path) 

			;Key is empty
			(if (equal? key "")
				(error-display "Error key: Empty key" output-path)

				;Invalid key- determinant 0
				(if (equal? (determinant (set-key key n) n) 0)
					(error-display "Error Determinant: Determinant 0" output-path)
					;Ciphers document
					(let loop
						([to-cipher content]
						[result null]
						)
						(if (empty? to-cipher)
							(list->file result output-path)
							(loop (cdr to-cipher) (append result (list (cipher (car to-cipher) key n))))
						)
					)
				)
			)
		)
	)
)

(define (error-display error-msg output-path)
	(display error-msg )
	(display "\n")
	(list->file (list error-msg) output-path)
)

;Gets an n,m element from a matrix
(define (get-matrix-element matrix row column)
	;Returns a list element given the index
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

;Returns the determinant from a matrix
(define (determinant matrix n)	
	
	;creates sub matrices to get determinant
	(define sub-matrices 
		(let outerLoop
			([arrays (cdr matrix)]
			[outerIndex 0]
			[outerResult null]
			)
			(if (equal? outerIndex n) 
				outerResult; Limit has been reached must stop
				(let innerLoop 
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

		;Multiplies sub matrices to get determinant
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

;Deletes a row n from a matrix
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

;Builds a word from list of numbers
(define (number-to-word m)
	;Returns the value of a letter from a position
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

;Converts a matrix into a list joining every row
(define (join-matrix m)	(let loop
		([original m]
		[result null]
		)
		(if (empty? original)
			result
			(loop (cdr original) (append result (car original)))
		)
	)
)

;Obtains the module of a matrix
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

;Prepares a key to cipher words
(define (set-key key n)
	;Converts a key to matrix form
	(define (key-to-matrix word n)

		;Goes through the word
		(let loop
			([word word]
			[count 0]
			[n n]
			[result null]
			)
			(if (< count n) 
				(if (empty? word)

					;Creates rows
					(let loopR
						(
						[n n]
						[countIn 0]
						[countOut count]
						[sub null]
						)
						(if (< countIn n)
							(loopR n (+ countIn 1) countOut (append sub '(23)))
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
	(key-to-matrix (word-to-number key) n)
)

;Prepares a word to be ciphered
(define (set-word word n)
	;Converts an array of word (in numbers) into matrix form
	(define (word-to-matrix word n)
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
							(loop2 word n (+ count 1) (append sub '(40)))
						)
						(loop word n (append result (list sub)))
					)
				)
			)
		)
	)
	(word-to-matrix (word-to-number word) n)
)

;Receives a word and returns a list of numbers, based on alphabet array
(define (word-to-number word)
	;Returns a position from a letter on alphabet array
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

;multiplies two matrices
(define (multiply-matrices m1 m2) 

	;Multiplies matrix by list
	(define (multiply-matrix-list m1 l1)

		;Mulitplies two lists
		(define (multiply-lists l1 l2)
			(if (equal? (length l1) (length l2))

				; If lists have the same length, multiplies
				(let loop
					([l1 l1]
					[l2 l2]
					[res 0]
					)
					(if (empty? l1)
						; If the lists are empty, returns the result
						res 

						; If the lists aren't empty increases the multiplication 
						(loop (cdr l1) (cdr l2) (+ res (* (car l1) (car l2)))) 
					)
				)

				; If lists don't have same length returns -1
				(- 0 1)		
			)
		)

		; Lists have same lenth
		(let loop
			([m1 m1]
			[l1 l1]
			[res '()]
			)
			(if (empty? m1)

				; If the lists are empty, returns the result
				res 

				; If the lists aren't empty increases the multiplication
				(loop (cdr m1) l1 (append res (list (multiply-lists (car m1) l1)))) 
			)
		)
	)

	; m1 is key to cipher m2 is a word in its matrix form
	(let loop
		([m1 m1]
		[m2 m2]
		[res '()]
		)
		(if (empty? m2)
			; If the lists are empty, returns the result
			res 

			; If the lists aren't empty increases the multiplication
			(loop m1 (cdr m2) (append res (list (multiply-matrix-list m1 (car m2))))) 
		)
	)
)

;List to file function
(define (list->file lst file)
  (display-lines-to-file lst
                         file
                         #:exists 'replace
                         #:mode 'text))



