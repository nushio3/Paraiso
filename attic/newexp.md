
New program
====

a program that looks like this:

~~~ {.haskell}
program :: Writer [String] ()
program = do
  let
      i = "i" :: Axis
      j = "j" :: Axis
      k = "k" :: Axis

      μ = "\\mu"
      λ = "\\lambda"

  әt(v[i])   ≔ ә[j](σ[i,j]+f[i])
  әt(σ[i,j]) ≔ μ * ә[j](v[j])
             + λ * (δ[i,j] * ә[k](v[k]))

~~~

Would produce equations like this:

\begin{eqnarray}\frac{\partial {v_{i}}}{\partial t} &=& \partial_{j}\left(\sigma_{i,j}+f_{i}\right)\\\frac{\partial {\sigma_{i,j}}}{\partial t} &=& \mu \partial_{j}\left(v_{j}\right)+\lambda \delta_{i,j} \partial_{k}\left(v_{k}\right)\end{eqnarray}

Conclusions
===

Bravo!