

	(require (planet soegaard/sicp:2:1/sicp))

(paint (number->painter 0))
   (paint diagonal-shading)
   (paint-hires  (below (beside diagonal-shading
                          (rotate90 diagonal-shading))
                  (beside (rotate270 diagonal-shading)
                          (rotate180 diagonal-shading))))
   (paint einstein)

(- 1 0.9)
