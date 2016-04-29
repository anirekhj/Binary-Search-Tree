;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require "definitions.rkt")

;; Required data definitions ==>

;; A binary search tree (BST) is one of:
;; * empty
;; * a Node

;; (define-struct node (key val left right))
;; A Node is a (make-node Num Str BST BST)
;; requires: key > every key in left BST
;; key < every key in right BST


;; Required constant definitions ==>

(define bst-1 (make-node 100 "100"
                         (make-node 27 "27" (make-node 20 "20"
                                                       empty
                                                       empty)
                                    empty)
                         (make-node 110 "110"
                                    (make-node 105 "105" empty
                                               (make-node 107 "107" empty empty))
                                    (make-node 125 "125" empty empty))))
(define bst-2 (make-node 50 "hi"
                         (make-node 36 "hi" (make-node 20 "hi"
                                                       empty
                                                       empty)
                                    (make-node 37 "hi" empty empty))
                         (make-node 77 "hi" empty (make-node 90 "hi" empty empty))))

;;************************************************************************************

;; (bst-internal-count bst) consumes a BST bst and produces
;; the total number of internal nodes in the BST.

;; bst-internal-count: BST -> Nat

;; Examples:
(check-expect (bst-internal-count bst-2) 3)
(check-expect (bst-internal-count (make-node 5 "5"
                                             (make-node 2 "2" empty empty)
                                             (make-node 6 "6" empty empty))) 1)

;; Function:
(define (bst-internal-count bst)
  (cond [(empty? bst) 0]
        [(and (empty? (node-left bst)) (empty? (node-right bst))) 0]
        [(empty? (node-left bst)) (add1 (bst-internal-count (node-right bst)))]
        [(empty? (node-right bst)) (add1 (bst-internal-count (node-left bst)))]
        [else (+ (add1 (bst-internal-count (node-left bst)))
                 (bst-internal-count (node-right bst)))]))

;; Tests:
(check-expect (bst-internal-count empty) 0)
(check-expect (bst-internal-count (make-node 5 "5"
                                             (make-node 2 "2" empty empty)
                                             empty)) 1)

(check-expect (bst-internal-count bst-1) 4)

;;************************************************************************************

;; (bst-bounded? bst lb ub) consumes a BST bst, a lower bound
;; integer lb and an upper bound interger ub and checks if 
;; all keys in the BST are greater than or  equal to the 
;; lower bound and less than or equal to the upper bound.

;; bst-bounded?: BST Int Int -> Bool

;; Examples:
(check-expect (bst-bounded? (make-node 5 "5"
                                       (make-node 2 "2" empty empty)
                                       empty) 1 10) true)
(check-expect (bst-bounded? bst-1 0 10) false)

;; Function:
(define (bst-bounded? bst lb ub)
  (cond [(empty? bst) true]
        [(and (>= (node-key bst) lb) (<= (node-key bst) ub))
         (and (bst-bounded? (node-left bst) lb ub)
              (bst-bounded? (node-right bst) lb ub))]
        [else false]))

;; Tests:
(check-expect (bst-bounded? bst-1 20 125) true)
(check-expect (bst-bounded? bst-1 3 3) false)
(check-expect (bst-bounded? empty 13 13) true)

;;************************************************************************************

;; (bst-add bst k v) consumes a BST bst, a key k and a val
;; v and overwrites the val if the key is already present
;; otherwise adds the new node in the appropriate place.

;; bst-add: BST Num Str -> BST

;; Examples:
(check-expect (bst-add (make-node 5 "5" (make-node 2 "2" empty empty) empty) 7 "7")
              (make-node 5 "5" (make-node 2 "2" empty empty)
                         (make-node 7 "7" empty empty)))
(check-expect (bst-add bst-1 36 "bye")
              (make-node
               100
               "100"
               (make-node 27 "27" (make-node 20 "20" empty empty) (make-node 36 "bye" empty empty))
               (make-node
                110
                "110"
                (make-node 105 "105" empty (make-node 107 "107" empty empty))
                (make-node 125 "125" empty empty))))

;; Function:
(define (bst-add bst k v)
  (cond [(empty? bst) (make-node k v empty empty)]
        [(= (node-key bst) k) (make-node k v (node-left bst) (node-right bst))]
        [(> (node-key bst) k) (make-node (node-key bst) (node-val bst)
                                         (bst-add (node-left bst) k v)
                                         (node-right bst))]
        [else (make-node (node-key bst) (node-val bst)
                         (node-left bst)
                         (bst-add (node-right bst) k v))]))

;; Tests:
(check-expect (bst-add bst-1 107 "hello")
              (make-node
               100
               "100"
               (make-node 27 "27" (make-node 20 "20" empty empty) empty)
               (make-node
                110
                "110"
                (make-node 105 "105" empty (make-node 107 "hello" empty empty))
                (make-node 125 "125" empty empty))))

(check-expect (bst-add bst-1 109 "109")
              (make-node
               100
               "100"
               (make-node 27 "27" (make-node 20 "20" empty empty) empty)
               (make-node
                110
                "110"
                (make-node 105 "105" empty
                           (make-node 107 "107" empty
                                      (make-node 109 "109" empty empty)))
                (make-node 125 "125" empty empty))))

(check-expect (bst-add empty 107 "hello")
              (make-node 107 "hello" empty empty))

;;************************************************************************************

;; (bst->al bst) consumes a BST bst and produces an
;; association list that contains all entries in the
;; tree, sorted by their keys in ascending order.

;; bst->al: BST -> (listof (list Num String)

;; Examples:
(check-expect (bst->al (make-node 5 "5" (make-node 2 "2" empty empty) empty))
              '((2 "2") (5 "5")))

(check-expect (bst->al bst-2) (list
                               (list 20 "hi")
                               (list 36 "hi")
                               (list 37 "hi")
                               (list 50 "hi")
                               (list 77 "hi")
                               (list 90 "hi")))

;; Function:
(define (bst->al bst)
  (local [;; (bst-remove bst k) consumes a BST bst and a key k and 
          ;; keeps the right side of the node with key equal to k.
          ;; bst-remove: BST Num -> BST
          ;; requires: k has to be one of the keys in bst
          (define (bst-remove bst k)
            (cond [(= (node-key bst) k) (node-right bst)]
                  [(> (node-key bst) k) (make-node (node-key bst) (node-val bst)
                                                   (bst-remove (node-left bst) k)
                                                   (node-right bst))]))
          ;; (smallest bst) consumes a BST bst and produces the 
          ;; list of smallest key and its corresponding value.
          ;; smallest: BST -> (list Num Str)
          (define (smallest bst)
            (cond [(empty? (node-left bst)) (list (node-key bst) (node-val bst))]
                  [else (smallest (node-left bst))]))]
    (cond [(empty? bst) empty]
          [else (cons (smallest bst)
                      (bst->al (bst-remove bst (first (smallest bst)))))])))

;; Tests:
(check-expect (bst->al bst-1)(list
                              (list 20 "20")
                              (list 27 "27")
                              (list 100 "100")
                              (list 105 "105")
                              (list 107 "107")
                              (list 110 "110")
                              (list 125 "125")))
(check-expect (bst->al empty) empty)
(check-expect (bst->al (make-node 3 "3" empty empty)) (list (list 3 "3")))

;;************************************************************************************

;; (bst-value-list bst) consumes a BST bst and produces
;; a list of strings containing all the values stored in
;; bst with no duplicates. The strings in the list should
;; be in decreasing order of their associated keys.

;; bst-value-list: BST -> (listof Str)

;; Examples:
(check-expect (bst-value-list (make-node 5 "5" (make-node 2 "2" empty empty) empty))
              (list "5" "2"))
(check-expect (bst-value-list (make-node 5 "5"
                                         (make-node 4 "4"
                                                    (make-node 3 "3"
                                                               (make-node 2 "2"
                                                                          (make-node 1 "1" empty empty) empty) empty) empty) empty))
              (list "5" "4" "3" "2" "1"))

;; Function:
(define (bst-value-list bst)
  (local [;; (bst-remove bst k) consumes a BST bst and a key k and 
          ;; keeps the left side of the node with key equal to k.
          ;; bst-remove: BST Num -> BST
          ;; requires: k has to be one of the keys in bst
          (define (remove bst k)
            (cond [(= (node-key bst) k) (node-left bst)]
                  [(< (node-key bst) k) (make-node (node-key bst) (node-val bst)
                                                   (node-left bst) 
                                                   (remove (node-right bst) k))]))
          ;; (smallest bst) consumes a BST bst and produces the 
          ;; list of greatest key and its corresponding value.
          ;; smallest: BST -> (list Num Str)
          (define (small bst)
            (cond [(empty? (node-right bst)) (list (node-key bst) (node-val bst))]
                  [else (small (node-right bst))]))]
    (cond [(empty? bst) empty]
          [else (cond [(empty? (bst-value-list (remove bst (first (small bst)))))
                       (cons (second (small bst))
                             (bst-value-list (remove bst (first (small bst)))))]
                      [(string=? (second (small bst))
                                 (first (bst-value-list (remove bst (first (small bst))))))
                       (bst-value-list (remove bst (first (small bst))))]
                      [else (cons (second (small bst))
                                  (bst-value-list (remove bst (first (small bst)))))])])))

;; Tests:
(check-expect (bst-value-list bst-1) (list "125" "110" "107" "105" "100" "27" "20"))
(check-expect (bst-value-list empty) empty)
(check-expect (bst-value-list bst-2) (list "hi"))

;; Required constant definitions ==>

(define bst-9 (make-node 100 "100"
                         (make-node 27 "27" (make-node 20 "20"
                                                       empty
                                                       empty)
                                    empty)
                         (make-node 110 "110"
                                    (make-node 105 "105" empty
                                               (make-node 107 "107" empty empty))
                                    (make-node 125 "125" empty empty))))

(define bst-10
  (make-node 5 "5" (make-node 3 "3" (make-node 0 "0" empty empty)
                              (make-node 4 "4" empty empty))
             (make-node 8 "8" empty empty)))

(define bst-11
  (make-node 5 "5" (make-node 3 "3" (make-node 0 "0" empty empty)
                              (make-node 4 "4" empty empty))
             empty))

;;*****************************************************************************************

;; (root-at-smallest bst) consumes a BST bst and makes
;; the element with the smallest key as the root of the
;; original BST while keeping the rest of the bst same.

;; root-at-smallest: BST -> BST

;; Examples:
(check-expect (root-at-smallest bst-9)
              (make-node
               20
               "20"
               empty
               (make-node
                100
                "100"
                (make-node 27 "27" empty empty)
                (make-node
                 110
                 "110"
                 (make-node 105 "105" empty (make-node 107 "107" empty empty))
                 (make-node 125 "125" empty empty)))))

(check-expect
 (root-at-smallest (make-node 5 "5" (make-node 3 "3" empty empty) empty))
 (make-node 3 "3" empty (make-node 5 "5" empty empty)))

;; Function:
(define (root-at-smallest bst)
  (local [;; (new bst) consumes a BST bst and removes
          ;; the node with the smallest key in the BST.
          ;; new: BST -> BST
          (define (new bst)
            (cond [(empty? (node-left bst)) (node-right bst)]
                  [else (make-node (node-key bst) (node-val bst)
                                   (new (node-left bst))
                                   (node-right bst))]))
          ;; (smallest bst) consumes a BST bst and produces
          ;; the node with the smallest key in the BST.
          ;; smallest: BST -> BST
          (define (smallest bst)
            (cond [(empty? (node-left bst))
                   (list (node-key bst) (node-val bst))]
                  [else (smallest (node-left bst))]))]
    (cond [(or (empty? bst) (empty? (node-left bst))) bst]
          [else (make-node (first (smallest bst))
                           (second (smallest bst))
                           empty
                           (new bst))])))

;; Tests:
(check-expect (root-at-smallest empty) empty)

(check-expect (root-at-smallest bst-10)
              (make-node
               0
               "0"
               empty
               (make-node 5 "5"
                          (make-node 3 "3" empty (make-node 4 "4" empty empty))
                          (make-node 8 "8" empty empty))))

(check-expect (root-at-smallest bst-11)
              (make-node 0 "0" empty
                         (make-node 5 "5"
                                    (make-node 3 "3" empty
                                               (make-node 4 "4" empty empty))
                                    empty)))

;;*****************************************************************************************

;; (bst-remove k bst) consumes a BST bst, a key k and removes
;; the node with the key equal to k from the original BST.

;; bst-remove: Nat BST -> BST

;; Examples:
(check-expect (bst-remove 20 bst-9)
              (make-node
               100
               "100"
               (make-node 27 "27" empty empty)
               (make-node
                110
                "110"
                (make-node 105 "105" empty
                           (make-node 107 "107" empty empty))
                (make-node 125 "125" empty empty))))

(check-expect (bst-remove 125 bst-9)
              (make-node
               100
               "100"
               (make-node 27 "27"
                          (make-node 20 "20" empty empty) empty)
               (make-node 110 "110"
                          (make-node 105 "105" empty
                                     (make-node 107 "107" empty empty))
                          empty)))

;; Function:
(define (bst-remove k bst)
  (local [;; (tricky-case bst) consumes a BST bst and removes the node
          ;; with the smallest key and properly arranges the remaining tree.
          ;; tricky-case: BST -> BST
          (define (tricky-case bst)
            (cond [(empty? (node-right bst)) (node-left bst)]
                  [(empty? (node-left bst)) (node-right bst)]
                  [else (make-node (node-key (root-at-smallest (node-right bst)))
                                   (node-val (root-at-smallest (node-right bst)))
                                   (node-left bst)
                                   (node-right (root-at-smallest (node-right bst))))]))]
    (cond [(empty? bst) bst]
          [(= k (node-key bst)) (tricky-case bst)]
          [(< k (node-key bst)) (make-node (node-key bst) (node-val bst)
                                           (bst-remove k (node-left bst))
                                           (node-right bst))]
          [else (make-node (node-key bst) (node-val bst)
                           (node-left bst)
                           (bst-remove k (node-right bst)))])))

;; Tests:
(check-expect (bst-remove 30 bst-9) bst-9)

(check-expect (bst-remove 5 bst-11)
              (make-node 3 "3"
                         (make-node 0 "0" empty empty)
                         (make-node 4 "4" empty empty)))

(check-expect (bst-remove 8 bst-10)
              (make-node 5 "5"
                         (make-node 3 "3"
                                    (make-node 0 "0" empty empty)
                                    (make-node 4 "4" empty empty)) empty))

(check-expect (bst-remove 105 bst-9)
              (make-node
               100
               "100"
               (make-node 27 "27" (make-node 20 "20" empty empty) empty)
               (make-node 110 "110" (make-node 107 "107" empty empty)
                          (make-node 125 "125" empty empty))))

(check-expect (bst-remove 110 bst-9)
              (make-node
               100
               "100"
               (make-node 27 "27" (make-node 20 "20" empty empty) empty)
               (make-node 125 "125" (make-node 105 "105" empty
                                               (make-node 107 "107" empty empty))
                          empty)))

;;************************************************************************************