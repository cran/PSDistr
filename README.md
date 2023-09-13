
# PSDistr - Distributions Derived from Normal Distribution

**author: Piotr Sulewski, Pomeranian University**

<!-- badges: start -->
<!-- badges: end -->

Distributions derived from normal distribution are: two-piece power
normal (TPPN), plasticizing component (PC), DS normal (DSN), expnormal
(EN), Sulewski plasticizing component (SPC), easily changeable kurtosis
(ECK) distributions. Density, distribution function, quantile function
and random generation are presented. To read more about the package
please see (and cite :)) papers:

1)  Sulewski P. (2021) DS Normal Distribution: properties and
    applications, Lobachevskii Journal of Mathematics, 42(12),
    2980-2999.
2)  Sulewski P. (2022) Easily Changeable Kurtosis Distribution. Austrian
    Journal of Statistics, 52, 1-24.
3)  Sulewski , P. (2022). New Members of The Johnson Family of
    Probability Distributions: Properties and Application, Accepted:
    February 2022. REVSTAT-Statistical Journal.
4)  Sulewski P. (2020) Normal Distribution with Plasticizing Component,
    Communications in Statistics ? Theory and Method, 51(11), 3806-3835.
5)  Sulewski P., Volodin A. (2022) Sulewski Plasticizing Component
    Distribution: properties and applications. Lobachtetavskii Journal
    of Mathtetamatics, 43(8), 2286-2300.
6)  Sulewski P. (2021) Two-Piece Power Normal Distribution,
    Communications in Statistics ? Theory and Method, 50(11), 2619-2639.

## Installation

You can install the released version of **PSDistr** from CRAN with:

``` r
install.packages("PSDistr")
```

You can install the development version of **PSDistr** from
[GitHub](https://github.com/) with:

``` r
library("remotes")
install_github("PiotrSule/PSDistr")
```

### Functions

**ddsn, pdsn, qdsn, rdsn**

Density, distribution function, quantile function and random generation
for the DS Normal Distribution are calculated

``` r
library(PSDistr)
ddsn(-0.5,2,2,2,0)
#> [1] 1.053981
pdsn(-0.5,2,2,2,0)
#> [1] 0.7733726
qdsn(0.5,2,2,2,0)
#> [1] -0.6823278
rdsn(10,2,2,2,0)
#>  [1] -0.9174543 -0.9531677 -0.9434789 -0.9387052 -0.7463924 -0.3198462
#>  [7] -0.5119604 -0.7520390 -0.5192255 -0.1585803
```

**deck, peck, qeck, reck**

Density, distribution function, quantile function and random generation
for the Easily Changeable Kurtosis Distribution are calculated

``` r
deck(1,2,3)
#> [1] 0.2307129
peck(1,2,3)
#> [1] 0.9294434
qeck(0.5,2,3)
#> [1] 0
reck(10,2,3)
#>  [1]  0.19273161  0.29262681  0.26832902 -1.04825437 -1.55783427 -1.19081611
#>  [7]  0.03379742 -0.44629456 -0.59413517  0.72587502
```

**den, pen, qen, ren**

Density, distribution function, quantile function and random generation
for the Expnormal Distribution are calculated

``` r
den(1,1,2,2,2,1)
#> [1] 0.2666153
pen(1,1,2,2,2,1)
#> [1] 0.7279188
qen(0.5,1,2,2,2,1)
#> [1] 0.2909696
ren(10,1,2,2,2,1)
#>  [1] -0.565035585  0.371245691  0.007892049  0.035879908  0.507669393
#>  [6] -0.242076982 -1.066860331  0.801121683 -0.353035247 -0.332387666
```

**dpc, ppc, qpc, rpc**

Density, distribution function, quantile function and random generation
for the Plasticizing Component are calculated

``` r
dpc(0,1,2,2)
#> [1] 0.1933341
ppc(0,1,2,2)
#> [1] 0.4012937
qpc(0.5,1,2,2)
#> [1] 1
rpc(10,1,2,2)
#>  [1] -0.5623307  3.2750871 -0.3884369 -1.4182320 -1.1450447  3.8902870
#>  [7] -0.4963983 -0.4527041  0.3387212 -1.0823312
```

**dspc, pspc, qspc, rspc**

Density, distribution function, quantile function and random generation
for the Sulewski Plasticizing Component Distribution are calculated

``` r
dspc(0,1,1,1,1,0)
#> [1] 0.2419707
pspc(0,1,1,1,1,0)
#> [1] 0.8413447
qspc(0.5,1,1,1,1,0)
#> [1] -0.6823278
rspc(10,1,1,1,1,0)
#>  [1] -0.79434037  0.01560102 -0.82784459  0.09298656 -1.18855183 -0.72773510
#>  [7] -0.43819889 -0.68688316 -0.61121603 -0.49309748
```

**dspc, pspc, qspc, rspc**

Density, distribution function, quantile function and random generation
for the Two-piece Power Normal distribution are calculated

``` r
dtppn(2,1,1,1,2)
#> [1] 0.4839414
ptppn(2,1,1,1,2)
#> [1] 0.8413447
qtppn(0.5,1,1,1,2)
#> [1] 1
rtppn(10,1,1,1,2)
#>  [1]  1.8747816  0.2496032  0.1118770  0.5042015  0.9287008  0.3041738
#>  [7]  1.6097886 -0.2114342  1.9926648  0.6559251
```
