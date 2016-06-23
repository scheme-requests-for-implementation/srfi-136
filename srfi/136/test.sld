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

(define-library (srfi 136 test)
  (export run-tests)
  (import (except (scheme base) define-record-type)
	  (srfi 64)
	  (srfi 136))
  (begin
    (define (run-tests)
      (test-begin "Extensible record types")

      (test-assert "Predicate"
		   (let ()
		     (define-record-type <record>
		       (make-record)
		       record?)
		     (record? (make-record))))

      (test-assert "Disjoint type"
		   (let ()
		     (define-record-type <record>
		       (make-record)
		       record?)
		     (not (vector? (make-record)))))

      (test-equal "Record fields"
		  '(1 2 3)
		  (let ()
		    (define-record-type <record>
		      (make-record foo baz)
		      record?
		      (foo foo)
		      (bar bar set-bar!)
		      (baz baz))
		    (define record (make-record 1 3))
		    (set-bar! record 2)
		    (list (foo record) (bar record) (baz record))))

      (test-equal "Subtypes"
		  '(#t #f)
		  (let ()
		    (define-record-type <parent>
		      (make-parent)
		      parent?)
		    (define-record-type (<child> <parent>)
		      (make-child)
		      child?)
		    (list (parent? (make-child)) (child? (make-parent)))))

      (test-equal "Inheritance of constructor"
		  '(1 2)
		  (let ()
		    (define-record-type <parent>
		      (make-parent foo)
		      parent?
		      (foo foo))
		    (define-record-type (<child> <parent>)
		      (make-child dummy bar)
		      child?
		      (bar bar))
		    (define child (make-child 1 2))
		    (list (foo child) (bar child))))

      (test-equal "Default constructors"
		  1
		  (let ()
		    (define-record-type <parent>
		      (make-parent foo)
		      #f
		      (foo foo))
		    (define-record-type (<child> <parent>)
		      make-child
		      child?)
		    (define child (make-child 1))
		    (foo child)))

      (test-equal "Unnamed fields"
		  1
		  (let ()
		    (define-record-type <record>
		      (make-record)
		      record?
		      (#f foo set-foo!))
		    (define record (make-record))
		    (set-foo! record 1)
		    (foo record)))

      (test-assert "Missing parent"
		   (let ()
		     (define-record-type (<record> #f)
		       (make-record)
		       record?)
		     (record? (make-record))))
		    
      (test-equal "Accessor names in constructor"
		  '(1 2 3)
		  (let ()
		    (define-record-type <record>
		      (make-record bar foo baz)
		      record?
		      (bar foo)
		      (foo bar)
		      (quux baz))
		    (define record (make-record 1 2 3))
		    (list (foo record)
			  (bar record)
			  (baz record))))

      (test-equal "Introspection"
		  '(foo 1)
		  (let ()
		    (define-record-type <record>
		      (make-record foo)
		      record?
		      (foo foo))
		    (define record (make-record 1))
		    (define-syntax k
		      (syntax-rules ()
			((k parent (field-name accessor-name . mutator-name*) ...)
			 (values (quote field-name) ... accessor-name ...))))
		    (let-values (((field-symbol accessor)
				  (<record> (k))))
		      (list field-symbol (accessor record)))))

      (test-equal "Predicate for records"
		  '(#t #f)
		  (let ()
		    (define-record-type <record>
		      (make-foo)
		      foo?)
		    (list (record? (make-foo))
			  (record? (vector)))))

      (test-assert "Record-type predicate"
		   (let ()
		     (define-record-type <record>
		       (make-record)
		       record?)
		     (eq? (record-type-predicate (make-record)) record?)))
		   
      (test-equal "Introspection of record-type name"
		  '<record>
		  (let ()
		    (define-record-type <record>
		      (make-record)
		      record?)
		    (record-type-name record?)))

      (test-assert "Introspection of record-type parent"
		   (let ()
		     (define-record-type <parent>
		       #f
		       parent?)
		     (define-record-type (<child> <parent>)
		       #f
		       child?)
		     (eq? (record-type-parent child?) parent?)))

      (test-equal "Introspection of record-type fields"
		  '(foo 2)
		  (let ()
		    (define-record-type <record>
		      (make-record foo)
		      record?
		      (foo foo)
		      (bar bar set-bar!))
		    (define-values (field-foo field-bar)
		      (apply values (record-type-fields record?)))
		    (define record (make-record 1))
		    ((list-ref field-bar 2) record 2)
		    (list (list-ref field-foo 0) (bar record))))

      (test-equal "Procedural generation of record types"
		  '(#t 1 2)
		  (let ()
		    (define-record-type <parent>
		      #f
		      parent?
		      (bar bar))
		    (define child? (make-record-type-predicate '<child>
							       '((foo foo))
							       parent?))
		    (define child (make-record child? #(1 2)))
		    (define foo (list-ref (car (record-type-fields child?)) 1))
		    (list (parent? child)
			  (bar child)
			  (foo child))))

		  
      (test-end "Extensible record types"))))
