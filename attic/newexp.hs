{-# LANGUAGE UnicodeSyntax #-}

-- The syntax is inspired by that of halide, and will allow expression of partial differential equations with much more ease.

main :: IO ()
main = do
  әt(v(r)[i]) := ә[j](σ(r)[i,j]+f[i])
  әt(σ(r)[i,j]) := μ * ә[j](v(r)[j]))
                  + λ * (δ[i,j] * ә[k]v(r)[k])


