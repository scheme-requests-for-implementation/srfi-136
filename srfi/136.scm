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

;;; Sentinel location

(define <secret> #(#f))

;;; Syntax

(define-syntax define-record-type-descriptor
  (syntax-rules (root)
    ((_ type-name rtd size indices root (field-spec ...))
     (define-record-type-descriptor type-name rtd size indices #f (field-spec ...)))    
    ((_ type-name rtd size indices parent (field-spec ...))
     (define-syntax type-name
       (syntax-rules (<secret>)
	 ((_ (keyword datum (... ...)))
	  (keyword datum (... ...) parent field-spec ...))
	 ((_ <secret> (k (... ...)))
	  (k (... ...) rtd size indices))
	 ((_ . _)
	  (syntax-error "invalid use of record type descriptor")))))))

(define-record-type-descriptor root
  #f 0 () #f ())

(define-syntax define-record-type
  (syntax-rules ()
    ((_ (type-name #f) . rest)
     (define-record-type (type-name root) . rest))
    ((_ (type-name parent) spec ...)
     (parent <secret> (define-record-type-helper1 type-name spec ... parent)))
    ((_ type-name . rest)
     (define-record-type (type-name root) . rest))))

(define-syntax define-record-type-helper1
  (syntax-rules ()
    ((_ type-name #f . rest)
     (define-record-type-helper1
       type-name constructor-name . rest))
    ((_ type-name constructor #f . rest)
     (define-record-type-helper1
       type-name constructor predicate-name . rest))
    ((_ type-name (constructor-name . args) predicate-name field-spec ...
	parent parent-rtd parent-size parent-indices)
     (define-record-type-helper2 parent-indices
       type-name (constructor-name . args) predicate-name field-spec ...
       parent parent-rtd parent-size parent-indices))
    ((_ type-name constructor-name . rest)
     (define-record-type-helper3 type-name (constructor-name) . rest))))

(define-syntax define-record-type-helper2
  (syntax-rules ()
    ((_ () type-name (constructor-name . args) . rest)
     (define-record-type-helper3
       type-name (constructor-name . args) . rest))
    ((_ (index . index*)
	type-name (constructor-name arg . args) . rest)
     (define-record-type-helper2
       index* type-name (constructor-name . args) . rest))))

(define-syntax define-record-type-helper3
  (syntax-rules ()
    ((_ type-name constructor predicate-name field-spec ...
	parent parent-rtd parent-size parent-indices)
     (define-record-type-helper4 () parent-size
       type-name constructor predicate-name field-spec ...
       parent parent-rtd parent-size parent-indices (field-spec ...)))))

(define-syntax define-record-type-helper4
  (syntax-rules ()
    ((_ (field ...) index
	type-name constructor predicate-name
	parent parent-rtd parent-size parent-indices original-fields)
     (define-record-type-helper5 ()
       type-name index constructor predicate-name field ...
       parent parent-rtd parent-size parent-indices original-fields))
    ((_ (field ...) index
	type-name constructor predicate-name
	(field-name accessor-name) . rest)
     (define-record-type-helper4
       (field ... (field-name accessor-name mutator-name index)) (+ 1 index)
       type-name constructor predicate-name . rest))
    ((_ (field ...) index
	type-name constructor predicate-name
	(field-name accessor-name mutator-name) . rest)
     (define-record-type-helper4
       (field ... (field-name accessor-name mutator-name index)) (+ 1 index)
       type-name constructor predicate-name . rest))
    ((_ . _)
     (syntax-error "bad field specs"))))

(define-syntax define-record-type-helper5
  (syntax-rules ()
    ((_ indices
	type-name size (constructor-name) predicate-name (field-name accessor mutator index) ...
	parent parent-rtd parent-size (parent-index ...) original-fields)
     (begin
       (define accessor
	 (make-accessor index))
       ...
       (define mutator
	 (make-mutator index))
       ...
       (define-values (rtd predicate-name)
	 (let*
	     ((rtd (make-rtd 'type-name 'original-fields (list accessor ...) (list mutator ...)))
	      (predicate-name (make-predicate rtd)))
	   (rtd-set-ancestors! rtd (cons rtd (if parent-rtd
						 (rtd-ancestors parent-rtd)
						 '())))
	   (rtd-set-predicate! rtd predicate-name)
	   (values rtd predicate-name)))
       (define constructor-name
	 (make-constructor rtd size (list parent-index ... . indices)))
       (define-record-type-descriptor type-name rtd size (parent-index ... . indices)
	 parent original-fields)))
    ((_ indices
	type-name size (constructor-name . args) predicate-name
	(field-name accessor-name mutator-name index) ...
	parent parent-rtd parent-size parent-indices original-fields)
     (define-record-type-helper6
       ((field-name accessor-name mutator-name index) ...
       (accessor-name field-name mutator-name index) ...)
       indices type-name size (constructor-name . args) predicate-name
       (field-name accessor-name mutator-name index) ...
       parent parent-rtd parent-size parent-indices original-fields))))

(define-syntax define-record-type-helper6
  (syntax-rules ()
    ((_ ((field-name accessor mutator field-index) . fields) (index ...)
	type-name size (constructor-name arg . args) . rest)
     (begin
       (define-syntax m
	 (syntax-rules ()
	   ((m . params)
	    (begin
	      (define-syntax field-name
		(syntax-rules *... ()
		  ((_ *field-index *fields (*index *...)
		       *type-name *size (*constructor-name . *args) . *rest)
		   (define-record-type-helper6 *fields (*index *...)
		     *type-name *size (*constructor-name . *args) . *rest))))
	      (define-syntax ok
		(syntax-rules *... ()		 
		  ((_ *field-index *fields (*index *...)
		       *type-name *size (*constructor-name *arg . *args) . *rest)
		   (define-record-type-helper5
		     (*index *... *field-index)
		     *type-name *size (*constructor-name . *args) . *rest))))
	      (define-syntax test
		(syntax-rules ()
		  ((_ arg . c) (field-name . c))))
	      (test ok . params)))))
       (m field-index fields (index ...) type-name size
	  (constructor-name arg . args) . rest)))
    ((_ fields indices
	type-name size (constructor-name arg . args) . rest)
     (syntax-error "invalid field name in constructor" arg))))

;;; Foundation

(scheme-define-record-type <rtd>
  (make-rtd name fieldspecs accessors mutators)
  rtd?
  (name rtd-name)
  (fieldspecs rtd-fieldspecs)
  (accessors rtd-accessors)
  (mutators rtd-mutators)
  (predicate rtd-predicate rtd-set-predicate!)
  (ancestors rtd-ancestors rtd-set-ancestors!))

(scheme-define-record-type <record>
  (%make-record rtd fields)
  record?
  (rtd record-rtd)
  (fields record-fields))
  
(define (make-constructor rtd size indices)
  (lambda args
    (let* ((fields (make-vector size))
	   (record (%make-record rtd fields)))
      (for-each
       (lambda (index arg)
	 (vector-set! fields index arg))
       indices args)
      record)))

(define (make-predicate rtd)
  (lambda (obj)
    (if (eq? <secret> obj)
	rtd
	(and (record? obj)
	     (memq rtd (rtd-ancestors (record-rtd obj)))
	     #t))))

(define (make-accessor index)
  (lambda (record)
    (vector-ref (record-fields record) index)))

(define (make-mutator index)
  (lambda (record value)
    (vector-set! (record-fields record) index value)))

;;; Procedural interface

(define (record-type-predicate record)
  (rtd-predicate (record-rtd record)))

(define (record-type-name predicate)
  (rtd-name (predicate <secret>)))

(define (record-type-parent predicate)
  (let ((ancestors (rtd-ancestors (predicate <secret>))))
    (and (not (null? (cdr ancestors)))
	 (rtd-predicate (cadr ancestors)))))

(define (record-type-fields predicate)
  (let ((rtd (predicate <secret>)))
    (let loop ((fieldspecs (rtd-fieldspecs rtd))
	       (accessors (rtd-accessors rtd))
	       (mutators (rtd-mutators rtd)))
      (cond
       ((null? fieldspecs)
	'())
       ((= (length (car fieldspecs)) 3)
	(cons (list (caar fieldspecs) (car accessors) (car mutators))
	      (loop (cdr fieldspecs) (cdr accessors) (cdr mutators))))
       (else
	(cons (list (caar fieldspecs) (car accessors) #f)
	      (loop (cdr fieldspecs) (cdr accessors) (cdr mutators))))))))

(define (make-record-type-predicate name fieldspecs . parent*)
  (let*
      ((parent-rtd
	(and (not (null? parent*)) ((car parent*) <secret>)))
       (parent-size
	(if parent-rtd (length (rtd-fieldspecs parent-rtd)) 0))
       (accessors 
	(let loop ((fieldspecs fieldspecs) (index parent-size))
	  (if (null? fieldspecs)
	      '()
	      (cons (make-accessor index)
		    (loop (cdr fieldspecs) (+ 1 index))))))
       (mutators
	(let loop ((fieldspecs fieldspecs) (index parent-size))
	  (if (null? fieldspecs)
	      '()
	      (cons (make-accessor index)
		    (loop (cdr fieldspecs) (+ 1 index))))))
       (rtd (make-rtd name fieldspecs accessors mutators))
       (predicate (make-predicate rtd)))
    (rtd-set-predicate! rtd predicate)
    (rtd-set-ancestors! rtd (cons rtd (if parent-rtd
					  (rtd-ancestors parent-rtd)
					  '())))
    predicate))

(define (make-record predicate field-vector)
  (%make-record (predicate <secret>) field-vector))
