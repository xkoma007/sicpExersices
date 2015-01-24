(define (factorial n)
  (facter 1 1 n))

(define (facter product counter max-count)
    (if (> counter max-count)
        product
        (facter (* counter product) (+ counter 1)
                max-count)))