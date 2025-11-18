# Methods Comparison

## Introduction

There are two families of UPL methods included in the `UPLforOAR`
package: those replicating the functionality of the Excel workbook in
the frequentist framework,
([`distribution_type()`](https://luddaludwig.github.io/UPLforOAR/reference/distribution_type.md),
[`Normal_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Normal_UPL.md),
[`Lognormal_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Lognormal_UPL.md),
and
[`Skewed_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Skewed_UPL.md))
and those using fitted likelihood distributions in the Bayesian
framework
([`Bayesian_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Bayesian_UPL.md)).
The UPL is the same MACT floor limit in principle, with these two
families of methods simply being different ways of arriving at the same
answer. However, there are differences worth explaining, pros and cons,
and circumstances where the frequentist and Bayesian methods will get
different UPL estimates.

### Frequentist

The UPL for Normal data is well-suited to the frequentist framework
because it can be calculated analytically using the mean, standard
deviation, and *t*-statistic of the emissions data. However, there are
no equivalent analytical solutions for Skewed or Lognormal UPL
calculations. Instead,
[`Lognormal_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Lognormal_UPL.md)
uses a Gram-Charlier Type A Series expansion to approximate the density
distribution and determine the arithmetic mean of future samples, and
[`Skewed_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Skewed_UPL.md)
uses the skewness and kurtosis moments to iteratively adjust the
*t*-statistic until the desired significance is achieved. All three UPL
methods assume the distribution explicitly and without uncertainty, and
assume the emissions are observed perfectly and independently. This
means that the parameters defining the probability distributions from
which the UPL is derived are fixed point estimates. For example, any
uncertainty around the mean of emissions in normally distributed data
does *not* carry through to the UPL estimate. Furthermore, if there are
circumstances where we don’t want to assume all test runs are
independent, we want to incorporate additional sources of variance, or
perhaps use individual test run uncertainty, then it will be very
difficult if not impossible to accommodate in these methods. However,
these formulas are very quick to use, have no stochastic components, and
have been in use a long time.

### Bayesian

In contrast, the UPL is estimated the same way regardless of the
distribution in the Bayesian approach. In this method we use Bayesian
MCMC to fit a likelihood distribution to the emissions data. The results
of this include posterior distributions, which contain a full
probability around your result rather than a fixed point estimate. That
means the UPL for a normal distribution is based on full posterior
distribution of the mean and standard deviation, rather than fixed point
estimates of the mean and standard deviation. This gives us much better
quantification of uncertainty in how well the distribution represents
the data. While we could still use the ratios of skewness and kurtosis
implemented in
[`distribution_type()`](https://luddaludwig.github.io/UPLforOAR/reference/distribution_type.md)
to pick the distribution to use, we can do better in the Bayesian
framework. Those ratios only tell us if the data are approximately
Normal or Lognormal, and are assigned as Skewed for any other outcome.
But can use many more types of distributions with Bayes that we will
want to pick between. With the
[`Bayesian_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Bayesian_UPL.md)
function, we have several ways to quantitatively and qualitatively asses
the best fit distribution. Rather than assuming the distribution is
correct implicitly, we can evaluate how well the parameters converged,
which is another method by which we can reject bad choices for
distributions.

The biggest advantage in using a Bayesian approach are the expanded
options for distributions, such as Beta and Gamma which are already
implemented in
[`Bayesian_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Bayesian_UPL.md).
Most emissions data are bounded by zero, and in some circumstances have
upper bounds as well. Bounded data can be accommodated both by picking
an appropriate distribution and by explicitly truncating the likelihood
distribution. More nuanced situations, such as non-independent test run
data, additional sources of variance, or individual test run
uncertainty, can easily be incorporated into the Bayesian approach
without changing the underlying methods of calculating the UPL. The main
draw back is in the difficulty of implementing the method. The Markov
Chain Monte Carlo (MCMC) Gibbs sampler takes longer than a simple
analytical solution. Since there are fewer assumptions, there are more
steps to ensure the likelihood model worked well and is appropriate to
the data. There is a stochastic component due to the way the MCMC
sampler searches for the parameter solutions. While this shouldn’t have
a meaningful effect on the outcome and can be controlled to create
reproducible results, it is a factor influencing different outcomes that
might be undesirable. Lastly, the Bayesian method uses prior
information. While in many situations this can be a strength in the
method, here we generally want completely uninformative priors for UPL
calculations. We very much want to avoid a situation where we think the
prior is uninformative, but in fact it is not.

For more details on
[`Bayesian_UPL()`](https://luddaludwig.github.io/UPLforOAR/reference/Bayesian_UPL.md)
see the example and details at [Bayesian
UPL](https://luddaludwig.github.io/UPLforOAR/articles/Bayesian-UPL.html).

## Example Comparison

We are going to calculate the UPL from emissions data using multiple
methods and compare the outcomes. This example uses Hg emissions data
from the recent [EPA
rule-making](https://www.regulations.gov/document/EPA-HQ-OAR-2009-0234-20132)
NESHAP for Coal- and Oil-fired Electric Utility Steam Generating Units.
First we load and organize the data into `emissions` and `sources`, then
select the top performers to use for the UPL calculations. We are only
going to be considering the existing source data set since it does not
include multiple runs per source.

``` r
dat_emiss = read_csv("./../man/data_example/MATS_Hg.csv", col_names = TRUE)
dat_emiss$sources = paste0(dat_emiss$`Plant Name`, "_", dat_emiss$`Unit Number`,
                         "_", dat_emiss$boiler_id)
dat_emiss$emissions = dat_emiss$Mercury_min_lb_MMBtu
dat_emiss = subset(dat_emiss, select = c(sources, emissions))

dat_exist = MACT_existing(data = dat_emiss, emissions = 'emissions',
                          sources = 'sources', CAA_section = 112)
dat_exist_avg = dat_exist %>% group_by(sources) %>% 
  summarize(avg = mean(emissions), counts = n())
dat_exist_avg = arrange(dat_exist_avg, avg)
distribution_result_exist = distribution_type(dat_exist, 
                                              emissions = 'emissions')
```

### Zero-Boundary

The skewness and kurtosis ratio tests tell us the distribution is
Normal. However, looking at plot of the data it is clear that the Normal
(in orange) doesn’t actually fit well (Fig. 1). This is because those
ratio tests do not account for zero as a lower boundary.

![Observation density of Hg for the overall population. The obseration
data are indicated in black as points and a rug along the axis, with the
observation density distribution as a black line. The fitted normal
distribution that is the basis of the UPL estimate is colored orange,
with a zero lower boundary. The average of the Hg emissions is the
vertical black line.](Methods-Comparison_files/figure-html/plot1-1.png)

Observation density of Hg for the overall population. The obseration
data are indicated in black as points and a rug along the axis, with the
observation density distribution as a black line. The fitted normal
distribution that is the basis of the UPL estimate is colored orange,
with a zero lower boundary. The average of the Hg emissions is the
vertical black line.

We can recreate this plot without the zero boundary for the density.
Notice that the Normal distribution a lot looks more reasonable when we
allow the left tails to extend a bit below zero, and things become
symmetric (Fig. 2). This is unrealistic, since we know we cannot have
negative emissions. However, we cannot account for boundaries in the
analytical normal UPL calculation. In the Bayesian framework, we can
explore both bounded and unbounded normal distributions.

![Observation density of Hg for the overall population. The obseration
data are indicated in black as points and a rug along the axis, with the
observation density distribution as a black line. The fitted normal
distribution that is the basis of the UPL estimate is colored orange,
without boundaries. The average of the Hg emissions is the vertical
black line.](Methods-Comparison_files/figure-html/plot2-1.png)

Observation density of Hg for the overall population. The obseration
data are indicated in black as points and a rug along the axis, with the
observation density distribution as a black line. The fitted normal
distribution that is the basis of the UPL estimate is colored orange,
without boundaries. The average of the Hg emissions is the vertical
black line.

### Comparing UPL Methods

Ignoring the best distribution for the moment, we are going to calculate
the UPL using the frequentist methods, the Bayesian methods unbounded,
and the Bayesian methods bounded, for the Normal, Lognormal, and Skewed
distributions. We are using 1 for the number of future runs, since the
data here are already averages of 3 runs.

``` r
UPL_freq_N = Normal_UPL(data = dat_exist,
                        emissions = 'emissions',
                        future_runs = 1,
                        significance = 0.99)
UPL_freq_LN = Lognormal_UPL(data = dat_exist,
                            emissions = 'emissions',
                            future_runs = 1,
                            significance = 0.99)
UPL_freq_SN = Skewed_UPL(data = dat_exist,
                         emissions = 'emissions',
                         future_runs = 1,
                         significance = 0.99)
Bayes_0bounded = Bayesian_UPL(distr_list = c("Normal", "Lognormal", "Skewed"),
                              data = dat_exist,  emissions = 'emissions', 
                              minY = 0, future_runs = 1, significance = 0.99)
Bayes_unbounded = Bayesian_UPL(distr_list = c("Normal", "Lognormal", "Skewed"),
                               data = dat_exist, emissions = 'emissions',
                               minY = -mean(dat_exist$emissions), 
                               future_runs = 1, significance = 0.99)
```

### Bayesian Results

While the zero bounded and unbounded Bayesian results look very similar,
there are slight differences caused by the zero hard boundary. As a
result the bounded distributions look slightly better, and are more
realistic.

![Fitted likelihood distributions for zero bounded (right) and unbounded
(left) Bayes distributions. The fitted distributions are colored blue
for lognormal, orange for normal, and green for skewed. Vertical lines
indicate the UPL results for each distribution and
method.](Methods-Comparison_files/figure-html/plot3-1.png)

Fitted likelihood distributions for zero bounded (right) and unbounded
(left) Bayes distributions. The fitted distributions are colored blue
for lognormal, orange for normal, and green for skewed. Vertical lines
indicate the UPL results for each distribution and method.

All six of the Bayesian distributions converged. When it comes to the
goodness-of-fit metrics, the best distribution was Skewed for both Bayes
bounded and unbounded, with the zero bounded being the best overall. The
Normal distribution did have the most points within 95% CI, but had the
worst fit when zero-bounded and did not integrate to 1.

| Distribution |      SSE | No. Obs. in 95% CI |  integral | Type         |
|:-------------|---------:|-------------------:|----------:|:-------------|
| Normal       | 1.63e+15 |                 25 | 0.9881275 | Unbounded    |
| Skewed       | 2.60e+15 |                 18 | 0.9931168 | Unbounded    |
| Lognormal    | 5.04e+15 |                 15 | 0.9836213 | Unbounded    |
| Skewed       | 5.70e+14 |                 26 | 0.9931192 | Zero bounded |
| Lognormal    | 2.39e+15 |                 22 | 0.9836326 | Zero bounded |
| Normal       | 2.86e+15 |                 30 | 0.7600093 | Zero bounded |

Goodness of fit results for Hg emissions

### UPL Results

But we aren’t interested in finding the best distribution in this
exercise (it is probably a Gamma distribution anyways), just comparing
UPL methods. The Lognormal doesn’t change between bounded and unbounded
Bayesian methods. That’s because it already is strictly positive. The
frequentist Lognormal result is similar but not the same, but it uses an
approximation that is not replicated in the Bayesian method that isn’t
unexpected. All Lognormal results are similarly larger than all others,
which is typical for the long-tailed distribution. The Skewed changes a
little with the zero boundary, since it isn’t strictly positive but is
asymmetric. It changes a similar amount with the frequentist method,
which again is an approximation, and not actually the 99$^{th}$
percentile of a skew-normal distribution. The unbounded Bayesian and
frequentist UPL’s for the Normal distribution are very similar to one
another. Since the frequentist method has an exact analytical solution,
we should be aiming to arrive at the same values with the Bayesian
method, excepting the stochastic component and added accountability to
uncertainty. The zero-bounded Bayesian Normal UPL is quite different,
since we are essentially fitting a partial Normal.

| Distribution | Method       |      UPL |
|:-------------|:-------------|---------:|
| Lognormal    | Frequentist  | 8.48e-08 |
| Lognormal    | Unbounded    | 9.05e-08 |
| Lognormal    | Zero bounded | 9.05e-08 |
| Normal       | Frequentist  | 4.86e-08 |
| Normal       | Unbounded    | 4.97e-08 |
| Normal       | Zero bounded | 5.58e-08 |
| Skewed       | Frequentist  | 4.82e-08 |
| Skewed       | Unbounded    | 4.35e-08 |
| Skewed       | Zero bounded | 4.02e-08 |

UPL results for fitting 3 distributions with frequentist, unbounded
Bayesian, and zero bounded Bayesian.

### Stochastic Effects

The next logical question is how much does the UPL change if we use
different random seeds when starting the MCMC iterations? We can explore
this be re-running distribution fits with `random = TRUE`. These results
will be reproducible, as long as you save the RNG state. We will fit
three sets of unbounded Normal distributions with identical inputs, but
allowing the random seeds to change.

``` r
Bayes_random1 = Bayesian_UPL(distr_list = c("Normal"),
                             data = dat_exist, emissions = 'emissions',
                             random = TRUE, 
                             minY = -mean(dat_exist$emissions), 
                             future_runs = 1, significance = 0.99)
Bayes_random2 = Bayesian_UPL(distr_list = c("Normal"),
                             data = dat_exist, emissions = 'emissions',
                             random = TRUE, 
                             minY = -mean(dat_exist$emissions), 
                             future_runs = 1, significance = 0.99)
Bayes_random3 = Bayesian_UPL(distr_list = c("Normal"),
                             data = dat_exist, emissions = 'emissions',
                             random = TRUE, 
                             minY = -mean(dat_exist$emissions), 
                             future_runs = 1, significance = 0.99)
```

Looking at the UPL results, we do see some variance in the UPL, but it
is much smaller than the difference between approximation methods and
distributions. The standard deviation in these 4 repeated UPL results is
3.57^{-10} which is similar to the difference between the Bayesian and
frequentist Normal UPL’s. It is worth pointing out that the more
uncertainty in the fit, the more the stochastic component might affect
the UPL results. This likely only a concern when the model struggles to
converge because it is a poor fit or because there are not enough data.

| Type     |       UPL |
|:---------|----------:|
| Fixed    | 4.970e-08 |
| Random 1 | 4.921e-08 |
| Random 2 | 4.944e-08 |
| Random 3 | 4.885e-08 |

UPL results for fitting unbounded Normal distributions with fixed and
then 3 different random seeds.

The exact random number generator and state used in each of these runs
is saved in `Bayes_random1$state` (for example) and should be included
in any report for record keeping. Each time you re-run a model with
`random = TRUE` a new set of random states is generated for use. In
order to recreate these results, you will need to pass the list
containing `.RNG.name` and `.RNG.state` from this output as the argument
`RNG.state` when rerunning (while leaving `random = FALSE`).

| MCMC Chain | Name                   | State                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:-----------|:-----------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Chain 1    | base::Super-Duper      | 1426460837, -10455191                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Chain 2    | base::Mersenne-Twister | 1, -956293786, -527640336, 1308154899, -2056941135, 1182813280, 1898539518, -857543287, -995349829, 740587146, -145284756, -266946833, -1041485531, -1076916260, 1277975986, 97922221, 1557744743, -1702014066, -325176216, 635465835, -1727792631, 368854168, -901769274, -332535007, 1567057843, 2076692258, 143024404, -1529867881, -1161784947, 1140922532, 742421130, 1469393141, 1040293711, 256673878, 1966846720, 1220257347, 1834335713, 2060339536, -1474183250, -1009863335, -1410706581, 1971704538, 713835420, -1168359521, -1101576715, -1402869620, -1563170462, -533748419, 948970935, 508195422, -777006184, -1725662469, -953699687, 759167784, -1120146986, 1803327057, 1616876803, -464320366, -2098840028, -1696571161, -1674595011, -1943187020, 660925978, -177795739, -950193153, 1549439622, 774001168, 342632627, -75308207, 857509632, 1673764318, 1870931497, -1679651365, -165866774, -1899937652, 915206095, -1126373499, -1730608068, -1537415918, 359494157, 2065549639, -1391617234, -569379192, -1507180661, -324063383, -942874312, -590029850, -145306751, 934077203, -1430942526, 126461812, -2030285961, 1070779501, -125050108, 783153706, 1332385685, -273540049, -467421898, -1729233568, -1594344541, 495967425, 1653038256, -2055603250, -1639850183, -245678261, 503401722, -839330308, 1486472767, -475874539, -1500964820, 696649986, 444752349, 85246167, 1872137150, -2008220168, 1893181915, -975591367, 47714696, -1916388042, -845461647, 630830499, 1218275570, 153769028, 1961496327, -626770211, -1625423916, 1579740922, 1653813829, -437771617, 676089894, 1840559280, -173943469, -1705111439, -784765536, 1099915582, -2033971127, -230992645, -662143798, 1674150444, -60459473, 1476572901, 628095900, -1568109070, 1494440045, 1815654823, -1376992306, -535369432, -279504085, -534922935, -1766347176, 1519886086, 67291873, 341032563, 713877218, -831137196, -355654825, 1869449037, 919018212, 1569841866, 1003977525, -206589553, -579385706, -1312299456, -771203325, 894097185, 1672118288, 1506007022, -557213799, -1958989781, 1171014938, -167589924, -1439626785, -1746104907, -1144022196, 1748842914, 1123809533, -1993169417, 1883480862, 524983768, -291391685, -1675845415, -1362187160, 229265814, 1393856785, 2070987459, 511654610, -340341916, 1586101927, -1780324355, -1020288524, -16724262, -1321897947, -783647425, -1722510650, -945307824, -1588322445, 1649729937, 171193280, 1535382942, 1759487849, 1976869787, 572132010, 611755212, -647793, 579353029, 1324236156, -1201093166, -804814515, -1706163193, -116411410, -796124472, 2012763595, -1855434711, 1195996792, 1158058918, 250607041, -2113564077, 780032258, -1446866892, 297265591, -1844576851, 301405892, 125811434, -1810481835, -775818513, 355356918, 1412034592, -1244130845, 130645952, -1428951492, -836546160, -137903070, -1067384568, 1557173388, 359326492, -1415544686, -824340096, -287732716, 1251346216, -1662800326, 158515664, 2108494188, -560902220, 665675282, 1864831968, -1711573556, -1796856608, -1596303326, -1906639064, -853326324, 1918671676, 829217778, -117869840, 7979588, 1854436952, -1820844230, -1949746192, 1065340604, -330044140, 1854345410, -1227086752, 1158361468, -1639456816, 333392162, -796066136, -1276279924, 125691068, -1862826190, -1014369728, 416074932, 2001413192, -633036358, 1069387280, -213423892, 1495982420, 767598354, -494529120, -989746484, 622372448, -1483137086, -1595000024, -1870539348, -35997380, 754706290, 380361008, -2029465820, 1350249784, -1300976934, 173778832, 1339159292, 1141534612, -1875306430, -125911808, 1309602876, -1556490928, -782399326, -1287391416, 1792102156, -48895268, -1971086510, 1081647744, -892794796, -1062661144, -1644484870, 1392103760, -1368659156, -999796876, 196062098, 1152900704, -715686004, 586965856, -988184606, 1734664104, -2016434996, 2013105660, 635020210, 1332793840, 342077892, 868213080, 1554089338, 1721622960, 463613884, -1216446380, -1338063934, 909138976, -1160618820, 871907920, 1508050914, -689918360, -822068660, 345825532, -1204203534, -1286646528, -1851191436, -1312976888, -1590325830, 135071184, 1678965740, -1610592364, 1538384850, -1500022944, -737964724, -1932165856, -1856691326, -124796184, -1024121620, 176566844, 496210802, -1686148496, 1383546916, -636448968, -790023014, 1969789648, 726862012, -21662060, -656109950, -985355072, 434225468, 1482315920, -1482624478, 819834376, 1260546316, 31505052, -1298745838, 53298560, 1564526100, 905428520, 1152185658, -1464145968, -2125290900, -103100620, 437013138, 1619503456, -1603675828, 2112716768, -418322526, 2110713768, 87724940, -288721220, 1773968242, -773392144, -188338108, 2093002072, 1700389946, -1749542032, 1622453308, -939035884, -1170202174, 721977568, -73545860, 874006224, 86726306, -1987482584, 328954124, 932723644, -1846321870, -1317447744, -770162124, 1959508936, -1101765446, 389042064, 1125121516, -199335852, -580956526, -1256869088, 966350284, 1467825120, -1860670782, 1123166760, 899614380, -2059212100, 297859570, -1708758736, -1377018716, -1887368520, -1516817574, 1305433488, -902250116, 778581268, -150212286, 1128332032, -1986126916, 237609808, 942954786, -759464632, 2074904588, -540049316, 881306834, -1384101888, 1476868948, 1834035688, 1753948154, -1001169200, -274565844, 1078300916, -2045380718, 882023520, -457888500, -543523104, -917070494, -1237060952, 1601268300, -733395076, -1998783694, -727538064, -511734460, -1075831848, -1571413894, -917227984, 463442620, 620504916, -372288958, -1901423712, 197783740, 1272236496, 654440483, 1149569316, -152322606, 938081831, -1549416335, -1450090826, 965767764, 1038536421, -1929024785, 1663419944, 2036068998, 587107507, 2121912853, -754879374, -586534720, -153869015, 1872788379, 902048876, -1966681990, 2025544111, -1096003399, 2016281294, 807133212, -939895923, -607482377, -890445568, 1740762974, 1338669099, -1599342803, -384484454, -1184179880, -1387455295, -122107021, -1979062124, -928887518, -605459401, -1489214207, 18352134, 375176772, 1303594869, -1072719137, -816536008, 673429014, 1271261731, 742049253, 610769922, 2093776496, -1708233703, -517173749, 1765297404, 365224234, 1776056159, -713410839, 864794110, -1799419412, -1261801155, 518152455, 127545584, 1977306254, 57042619, 940684957, -1880944054, 1044074600, 278765649, -1400941757, -605129148, -1340914446, 474538183, -874417519, -753677290, -951839116, 162066693, 456118095, 279234888, -797993754, 1309690387, 874859893, 1507373394, -428942944, -110849911, 29604027, 571847436, 1957270170, 1845999631, 1348033625, 19001582, 494809980, -964878355, 1268373655, -1072250016, 11494206, -1198216181, 575507597, -1625682502, -406150152, 9060257, 1046206803, 1518950516, 1874569730, -40598377, -1094310303, 1975840422, 996447908, 1600176981, -1328062465, 1827634840, -441564234, 291637315, 1679425925, -2132704734, -1796790000, -960718663, -1457013397, 433349724, -1528002166, 1411521919, -1561276215, 1714660702, 579573260, -1393964579, -14509849, -1671684848, -757763026, 109161179, 342457277, -1707428566, -1374115384, 1325643505, -316073885, -1505649564, 1854394258, 2042053735, 1024789553, -1909123722, 778072724, 2010026149, 431556015, -1805599128, 2123089478, -526629901, -856136875, 920243506, -1464876800, -1179780631, 850160731, 1915389612, 735654074, 1096632943, 1948932089, -572090738, -727378084, -1514279987, -971863241, 1638289856, 1865180830, -84971669, -581798163, -346315942, -536018920, 671690113, 1037892403, 1826007380, 163306338, -1801799689, -23218111, -193745722, 466536964, 359007669, -706651489, -1315419001 |
| Chain 3    | base::Wichmann-Hill    | 6382, 19737, 11206                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

Random number generator (RNG) names and states used in MCMC chains
