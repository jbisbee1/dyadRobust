# dyadRobust

This package estimates cluster-robust standard errors for dyadic data using multiway decomposition as decsribed in Aronow, Peter M., Cyrus Samii, and Valentina A. Assenova. "Cluster-robust variance estimation for dyadic data." Political Analysis 23.4 (2015): 564-577. It was developed by [James Bisbee](http://www.jamesbisbee.com/) and [Pedro Rodriguez](http://prodriguezsosa.com/)

It is built on the replication materials for the same, available at https://doi.org/10.7910/DVN/OMJYE5. The value-add of this package is to dramatically reduce the computation time required to calculate these standard errors, as illustrated in the figure below. 

![Sample image](https://raw.githubusercontent.com/jbisbee1/dyadRobust/master/timing.png)

---

#### Installation

`dyadRobust` can be installed from this GitHub repository with the following lines of code: 

```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jbisbee1/dyadRobust")

library(dyadRobust)
```

---

#### Example

Below is a short demonstration. 

```{r}
data("dyad.sim")
m <- lm(dY ~ dX,dyad.sim)
out <- dyadRobust(fit = m,
           dat = dyad.sim,
           dyadid = "dyads",
           egoid = "dyad1",
           alterid = "dyad2")
}
```

---

#### Speed improvements with `feols()` and `lfe()` functions

Speed can be further improved if estimating a model with fixed effects using either the `feols()` function from the `fixest` package, or the `felm()` function from the `lfe` package. Speed improvements relative to the replication code from Aronow, Samii, and Assenova (2015) are consistently ~94% across models, as demonstrated using the replication code from Fisman, Raymond, Sheena S Iyengar, Emik Kamenica and Itamar Simonson. 2006. "Gender differences in mate selection: Evidence from a speed dating experiment." Quarterly Journal of Economics 121:673–697.

![Sample image](https://raw.githubusercontent.com/jbisbee1/dyadRobust/master/timing.png)

To take advantage of these improvements, make sure to set the following inputs:

	- `feols()`: `lean = FALSE` and `demeaned = TRUE`
	- `felm()`: `keepCX = TRUE`
	
Examples are provided below.

```{r}
require(fixest)
m <- feols(dec ~ amb + attr + intel | iid,
           sdat,weights = sdat$wts,lean = F,demeaned = T)
out <- dyadRobust(fit = m,
                  dat = sdat,
                  dyadid = "dyadid",
                  egoid = "fid",
                  alterid = "mid")

require(lfe)
m <- felm(dec ~ amb + attr + intel | iid,
          sdat,weights = sdat$wts,keepCX=T)
out <- dyadRobust(fit = m,
                  dat = sdat,
                  dyadid = "dyadid",
                  egoid = "fid",
                  alterid = "mid")
}
```

---

#### Maintainer(s)

[James Bisbee](http://www.jamesbisbee.com/) - <jhb362@nyu.edu>