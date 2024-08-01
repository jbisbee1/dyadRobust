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

#### Under construction

Updates are forthcoming (circa August 2024) which will allow users to use the package with:
- `feols` model objects from the `fixest` package
- `felm` model objects from the `lfe` package

In addition, there is an issue where using the package on a high performance computing cluster might cause a drain on the CPU due to the function ignoring any tasks-per-node parameters. Please email the maintainer for help with this issue if it arises.

---

#### Maintainer(s)

[James Bisbee](http://www.jamesbisbee.com/) - <jhb362@nyu.edu>