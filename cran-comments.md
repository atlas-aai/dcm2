## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

This is a resubmission.

Based on comments during the initial submission, we have
explained the DCM acronym (diagnostic classification model), and we no longer
use the acronym. We left M2 in the description because it is the name of a 
model fit statistic and is not an acronym.

We have also added `\value` to the `.Rd` files and described the output of the
exported functions. We have done this for the `calc_m2.Rd`, `pipe.Rd`, and
`tidyeval.Rd` files. We have also removed the `Mord.Rd` file since this function
is no longer exported.

We no longer reference non-exported functions with `:::`.

We now include Dr. Wenchao Ma as a contributor to the package, with specific
recognition for his work in producing the `Mord.cpp` code.
