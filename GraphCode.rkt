#lang racket/base

#|
A Graphviz UML free code generator

Generate Graphviz UML models from your code
Heavily Work-In-Progress


CONCEPT
---
Convert a code expression into a UML diagram that
can be followed as if it were laid out manually,
removing the need to design UML first, code later.
You can now code first, then have UML documentation
later.


The program should be able to mimic these basic flows, as well
as conditionals like `when` and `unless`, which are easy
to implement from the same flow as `if`

Multiple sections of code (aka a sequence of code) like a `begin`
sequence should be connected properly.
If a `when` happens before an `if`, then the result of the `when`
should connect to the following `if` body, and so on and so forth,
exactly as it would with Racket code.


TODO LIST:
* different styles of edges
* different shapes
|#


(require racket/match
         (only-in racket/list empty? remove-duplicates)
         (only-in racket/string string-join)
         (only-in racket/contract -> and/c or/c define/contract)
         )

(provide define/diagram Render-Diagrams)

(define *things-to-graph*
  (make-parameter '()))


;; parameter to control the output of the program
(define *diagram-verbosity*
  (make-parameter #f))


;; a generic printf wrapper contained underneath a parameter
(define (puts . args)
  (when (*diagram-verbosity*)
    (apply printf args)))


;; Macro to re-out to define, but store code in a parameter
;; The parameter is later flushed with Render-Diagrams
(define-syntax-rule (define/diagram code ...)
  (begin
    (*things-to-graph* (cons `(define code ...)
                             (*things-to-graph*)))
    (define code ...)))


;; The basic two structures for describing our graph
;; TODO: add support for more shapes to support Graphviz fully
(struct Node  (id contents) #:transparent)
(struct Graph (entrynode nodes edges) #:transparent)
(struct Edge  (id1 id2 style text))


;; Graph-init - given an ID, create an empty graph
;; The ID is used as the start so we know where to start from
;; The ID however, is left not included
(define (Graph-init firstid)
  (Graph firstid
         (make-immutable-hash '())
         '()))


;; Singular add edge function
(define (Graph-add-edge G node-id other-id)
  (Graph (Graph-entrynode G)
         (Graph-nodes G)
         (cons `(,node-id . ,other-id)
               (Graph-edges G))))


;; a multi-add edge function, which acts based around
;; the IDs received on the right side.
(define (Graph-add-edges G node-id [other-ids '()])
  (cond
    ([empty?   other-ids] G)
    ([symbol?  other-ids] G)
    ([boolean? other-ids] G)
    (else
     (foldl (λ (pair acc)
              (Graph-add-edge acc (car pair) (cdr pair)))
            G
            (map (λ (gid) (cons node-id gid)) other-ids)))))
  

;; Add a node to a given graph G
;; the #:connects keyword will help bind edges
;; to avoid multiple calls to Graph-add-edge
;; creating awkward code
(define (Graph-add-node G node #:connects [uids #f])
  (let ([nodes (Graph-nodes G)])
    (if (hash-has-key? nodes (Node-id node))
        (if (not (eqv? #f uids))
            G
            (Graph-add-edges G (Node-id node) uids))
        (Graph-add-edges
         (Graph (Graph-entrynode G)
                (hash-set nodes (Node-id node) node)
                (Graph-edges G))
         (Node-id node)
         uids))))


;; steps to converting to a graph
;; 1. differentiate between a (define x y) and a (define (f ...) c ...)
;; 2. separate the two into different categories (binding vs function)
;; 3. create a generalized code parser that parses all types of conditionals
;; 4. if we reach a `begin` block, we start over and treat it like a sequence
;;
;; This is the main entrypoint to defer to the correct branches
(define (code->graph code)
  (match code
    ([list 'define (list f args ...) datum ...]
     (let ([id       (gensym)]
           [first-id (gensym)])
       (parse-sequence datum first-id 
                       (Graph-add-node
                        (Graph-init id)
                        (Node id (format "~a ~a" f args))
                        #:connects (list first-id)))))
    ([list 'define varname result]
     (let ([id (gensym)])
       (parse-code result id (Graph-init id))))
    (else (error "wtf is this?"))))


;; parse-code handles singular pieces of code
;; These code instructions can then be translated into graph
;; structures and connecting the branches downards like a
;; "flow chart" diagram
;;
;; if - 2 branches
;; cond - many branches
;; case - many branches
;;
;; blocks can be encountered and must be sent to parse-sequence
;; instead, as those deal with groups of code
;;
;; begin
;; when
;; unless
;; let
(define (parse-code code uid gcc #:next-section [ns #f])
  (puts "--Code--\n")
  (puts "--Code--\n")
  (puts "uid: ~a\n" uid)
  (puts "C: ~a\n" code)
  (match code
    ([list 'let (list binds ...) seq ...]
     (puts "Got a Let") ; a `let` block isn't necessarily diagram flow, it's bindings
     (parse-sequence seq uid gcc))
    ([list 'if C T F]
     (let ([leftid  (gensym)]
           [rightid (gensym)])
       (let ([rightcc (parse-code F rightid gcc #:next-section ns)])
         (let ([leftcc (parse-code T leftid rightcc #:next-section ns)])
           (Graph-add-node leftcc (Node uid (format "if ~a" C))
                           #:connects (list leftid rightid))))))
    ([list 'cond conds ...]
     (puts "Got a cond\n")
     (let ([pairs (map (λ (c) (cons (gensym) c)) conds)])
       (foldl
        (λ (con-p acc)
          (let ([datum   (cdr con-p)]
                [left-id (car con-p)])
            (let ([left-side  (car datum)]
                  [right-side (car (cdr datum))]
                  [right-id   (gensym)])
              (parse-code right-side right-id
               (Graph-add-node acc
                               (Node left-id (format "~a" left-side))
                               #:connects (list right-id))
               #:next-section ns))))
        (Graph-add-node gcc (Node uid (format "cond"))
                        #:connects (map car pairs))
        pairs)))
    ([list 'case V seq ...]
     (puts "Got a case\n")
     (let ([pairs (map (λ (c) (cons (gensym) c)) seq)])
       (foldl
        (λ (con-p acc)
          (let ([datum   (cdr con-p)]
                [left-id (car con-p)])
            (let ([left-side  (car datum)]
                  [right-side (car (cdr datum))]
                  [right-id   (gensym)])
              (parse-code right-side right-id
               (Graph-add-node acc
                               (Node left-id (format "~a" left-side))
                               #:connects (list right-id))
               #:next-section ns))))
        (Graph-add-node gcc (Node uid (format "match ~a" V))
                        #:connects (map car pairs))
        pairs)))
    ([list 'when C seq ...]
     (puts "Got a when\n")
     (let ([when-id (gensym)])
       (parse-sequence seq when-id
                       (Graph-add-node gcc (Node uid (format "when ~a" C))
                                       #:connects (list when-id)))))
    ([list 'unless C seq ...]
     (puts "Got an unless")
     (let ([unless-id (gensym)])
       (parse-sequence seq unless-id
                       (Graph-add-node gcc (Node uid (format "unless ~a" C))
                                       #:connects (list unless-id)))))
    ([list 'begin seq ...]
     (puts "Got a begin\n")
     (let ([begin-id (gensym)])
       (parse-sequence seq begin-id
                       (Graph-add-node gcc (Node uid "begin")
                                       #:connects (list begin-id)))))
    ([list 'match V branches ...]
     (puts "Got a match")
     (let ([pairs (map (λ (c) (cons (gensym) c)) branches)])
       (foldl
        (λ (con-p acc)
          (let ([datum   (cdr con-p)]
                [left-id (car con-p)])
            (let ([left-side  (car datum)]
                  [right-side (car (cdr datum))]
                  [right-id   (gensym)])
              (parse-code right-side right-id
               (Graph-add-node acc
                               (Node left-id (format "~a" left-side))
                               #:connects (list right-id))
               #:next-section ns))))
        (Graph-add-node gcc (Node uid (format "match ~a" V))
                        #:connects (map car pairs))
        pairs)))
    ([list fn args ...]
     (if (empty? args)
         (parse-code fn uid gcc #:next-section ns)
         (let ([sub-id (gensym)])
           (parse-code args sub-id
                       (Graph-add-node gcc
                                       (Node uid (format "~a" fn))
                                       #:connects (list sub-id))
                       #:next-section ns))))
    (datum
     (if (empty? datum)
         gcc
         (Graph-add-node gcc
                         (Node uid (format "~a" datum))
                         #:connects
                         (if (not (eqv? #f ns))
                             (list ns)
                             '()))))))


;; A sequence of code is different.
;; A sequence is a linear list of actions to take, which may not have
;; any impact on other functions in the sequence, but are still executed
;; in a linear first-in-first-out fashion.
;; A sequence is like `begin`, where results are stored, but discarded
;; per each execution, if not bound locally via let.
;;
;; Example:
;; ```
;; (begin
;;  (displayln hello)
;;  (+ 1 2 3))
;; ```
;; While displayln returns void, it's result is not connected to
;; the + call, but displayln occurs before +, so in that sense,
;; the "flow" is displayln first, then +, meaning displayln
;; connects to +
(define (parse-sequence codes sid gcc)
  (puts "----Section----\n")
  (puts "sid: ~a\n" sid)
  (puts "c: ~a\n" codes)
  (match codes
    ([list lastcode]
     (puts "GOT ONE ITEM")
     (parse-code lastcode sid gcc))
    ([list code1 code2]
     (puts "GOT TWO ITEMS")
     (let ([id1 (gensym)]
           [id2 (gensym)])
       (parse-code code2 id2
                   (parse-code code1 sid gcc
                               #:next-section id2))))
    ([list codes ...]
     (puts "GOT N ITEMS")
     (let ([id1 (gensym)]
           [id2 (gensym)])
       (parse-sequence (cdr codes)
                       id2
                       (parse-code (car codes) sid gcc
                                   #:next-section id2))))
    (else gcc)))
     

;; Find all edges in the graph that correspond with k
(define (find-edges k edges)
  (remove-duplicates
   (map cdr
        (filter (λ (edge) (eqv? k (car edge)))
                edges))))


(define (write-graph-dot G)
  (displayln "digraph {")
  (for ([key-val (hash->list (Graph-nodes G))])
    (define k (car key-val))
    (define node (cdr key-val))
    (printf "~a [label=\"~a\"]\n"
            (Node-id node)
            (Node-contents node)))
  (for ([edge-p (remove-duplicates (Graph-edges G))])
    (printf "~a -> ~a\n"
            (car edge-p)
            (cdr edge-p)))
  (displayln "}"))
  



(define (Render-Diagrams fname)
  (define graphs (map code->graph (*things-to-graph*)))
  (for ([g graphs])
    (displayln g))
  
  (call-with-output-file fname
    #:exists 'replace
    (λ (out)
      (parameterize ([current-output-port out])
        (for ([g graphs])
          (write-graph-dot g)))))) 



(module+ test
  (require rackunit)
  (*diagram-verbosity* #t)

  (define (test-function x)
    (begin
      (case x
        ((1 2 3 4 5) "odd")
        ((2 4 5 6 10) "even")
        (else "What?"))
      (displayln "kinda done")))

  (Render-Diagrams "Test.dot")
  )

; end
