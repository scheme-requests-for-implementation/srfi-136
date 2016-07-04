;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax define-record-type
  (syntax-rules ()
    ((_ type-spec constructor-spec
	#f field-spec ...)
     (define-record-type type-spec constructor-spec
	predicate-spec field-spec ...))
    ((_ type-spec #f
	predicate-spec field-spec ...)
     (define-record-type/136 type-spec #f
       predicate-spec field-spec ...))
    ((_ type-spec (constructor-name field-name ...)
	predicate-spec (field accessor . mutator*) ...)
     (begin
       (define-record-type/136 type-spec constructor-name/136
	 predicate-spec (field accessor . mutator*) ...)
       (define constructor-name
	 (make-constructor/131 predicate-spec
			       '(field-name ...)))))
    ((_ type-spec constructor-name
	predicate-spec (field accessor . mutator*) ...)
     (begin
       (define-record-type/136 type-spec constructor-name/136
	 predicate-spec (field accessor . mutator*) ...)
       (define constructor-name
	 (make-constructor/131 predicate-spec #f))))))

(define (make-constructor/131 predicate field-names)
  (if field-names
      (let loop ((p predicate) (fields '()))
	(if p
	    (loop (record-type-parent p)
		  (append (map car (record-type-fields p)) fields))
	    (let ((indices
		   (map (lambda (field-name)
			  (let loop ((fields fields) (index 0))
			    (cond
			     ((null? fields)
			      (error "not a record field" field-name))
			     ((eq? (car fields) field-name)
			      index)
			     (else
			      (loop (cdr fields) (+ 1 index))))))
			field-names)))
	      (lambda args
		(unless (= (length args) (length indices))
		  (error "unsupported number of arguments in constructor call"))
		(let ((field-vector (make-vector (length fields))))
		  (for-each (lambda (arg index)
			      (vector-set! field-vector index arg))
			    args indices)
		  (make-record predicate field-vector))))))
      (lambda args
	(make-record predicate (list->vector args)))))
