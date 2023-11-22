#|

This is a simple translator to command line language (scoff!).  In the
tree, each node is a list, the first element of which is a type tag.

The intention is merely to sketch out the concept for what the corresponding
Rust code will implement.

Admittedly, this is as bare bones as possible. In the Rust
implementation, it is far easier to provide default
arguments. Moreover, strong typing enables one to enforce that the
constructed argument tree is self-consistent (e.g. no options for
`optimize' show up in `sample') at compile time.

|#

(define (flat-map f elements)
  (fold-right append '() (map f elements)))

(define (translate node)
  (cond ((null? node) '())
        ((kv? node)
         (translate-kv node))
        ((product? node)
         (translate-product node))
        ((sum? node)
         (translate-sum node))
        ((arg-tree? node)
         (flat-map translate (cdr node)))
        (else (error "unknown type: " (car node)))))

(define (translate-kv node)
  (list (string (caadr node) '= (cdadr node))))

(define (translate-product node)
  (append (list (string (cadr node)))
          (flat-map translate (cddr node))))

(define (translate-sum node)
  (append (list (string (caadr node) '= (cdadr node)))
          (flat-map translate (cddr node))))

(define (kv? node)
  (eq? (car node) 'kv))

(define (product? node)
  (eq? (car node) 'product))

(define (sum? node)
  (eq? (car node) 'sum))

(define (arg-tree? node)
  (eq? (car node) 'arg-tree))

(define (make-kv key value)
  (list 'kv (cons key value)))

(define (make-product type . elements)
  (cons 'product (cons type elements)))

(define (make-sum type variant . elements)
  (cons 'sum (cons (cons type variant) elements)))

(define (make-arg-tree . elements)
  (cons 'arg-tree elements))

(define (translate-to-string node)
  (let ((elems (translate node)))
    (if (null? elems)
        ""
        (fold-left (lambda (x y) (string x " " y)) (car elems) (cdr elems))
        )))

(define tree (make-arg-tree
              (make-sum
               'method
               'sample
               (make-kv 'num_samples 1000)
               (make-kv 'num_warmup 1000)
               (make-kv 'save_warmup 0)
               (make-kv 'thin 1)
               (make-kv 'num_chains 1)
               (make-product 'adapt
                             (make-kv 'engaged 1)
                             (make-kv 'gamma 0.05)
                             (make-kv 'delta 0.8)
                             (make-kv 'kappa 0.75)
                             (make-kv 't0 10)
                             (make-kv 'init_buffer 75)
                             (make-kv 'term_buffer 50)
                             (make-kv 'window 25)
                        )
               (make-sum 'algorithm
                         'hmc
                         (make-sum 'engine
                                   'nuts
                                   (make-kv 'max_depth 10)
                                   )
                         (make-sum 'metric 'diag_e)
                         (make-kv 'stepsize 1)
                         (make-kv 'stepsize_jitter 0)
                         )
               )
              (make-kv 'id 1)
              (make-product 'data
                            (make-kv 'file "bernoulli.data.json")
                            )
              (make-kv 'init "2")
              (make-product 'random
                            (make-kv 'seed -1)
                            )
              (make-product 'output
                            (make-kv 'file "output.csv")
                            (make-kv 'diagnostic_file "diagnostic.csv")
                            (make-kv 'refresh 100)
                            (make-kv 'sig_figs -1)
                            (make-kv 'profile_file "profile.csv")
                            )
              (make-kv 'num_threads 1)
              ))

(translate-to-string tree)
