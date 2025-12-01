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
| Skewed       | Unbounded    | 2.91e-08 |
| Skewed       | Zero bounded | 2.91e-08 |

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
2.29^{-10} which is similar to the difference between the Bayesian and
frequentist Normal UPL’s. It is worth pointing out that the more
uncertainty in the fit, the more the stochastic component might affect
the UPL results. This likely only a concern when the model struggles to
converge because it is a poor fit or because there are not enough data.

| Type     |       UPL |
|:---------|----------:|
| Fixed    | 4.970e-08 |
| Random 1 | 4.918e-08 |
| Random 2 | 4.927e-08 |
| Random 3 | 4.931e-08 |

UPL results for fitting unbounded Normal distributions with fixed and
then 3 different random seeds.

The exact random number generator and state used in each of these runs
is saved in `Bayes_random1$state` (for example) and should be included
in any report for record keeping. Each time you re-run a model with
`random = TRUE` a new set of random states is generated for use. In
order to recreate these results, you will need to pass the list
containing `.RNG.name` and `.RNG.state` from this output as the argument
`RNG.state` when rerunning (while leaving `random = FALSE`).

| MCMC Chain | Name                   | State                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|:-----------|:-----------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Chain 1    | base::Super-Duper      | -331855013, 2090978051                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Chain 2    | base::Mersenne-Twister | 1, 608246984, 1334815990, 688753969, 735849827, -1193955534, -1115846524, 112374983, 1946449821, -96492780, 1481707962, -1156569083, 1970142687, -2124386202, -342638608, -1292052205, -1937922383, -1885875872, 1183074046, 2010178185, 1024867771, -1494161526, -1589612948, 1917136879, -106026971, 1143867612, 672100018, 57917869, -2020072089, 1563210382, -488891544, 1476633451, 1697938697, -1649831016, 1706025670, -1031690207, 411892915, 657292834, -142286828, 1050002583, 380350093, 1886412708, 952394122, -939573771, -157183921, -432593066, -729600, 1672875843, 1221265121, 1761723472, -1558049618, -1117570983, 332183659, -1475371558, -329431396, -1177151329, 1686360821, 1892437900, -451043742, 641598013, 1734692023, 330437470, 1838839448, 176272891, 949299097, 2022314024, -1308026666, 1913363793, -745395197, -2101693550, 525443364, -1906681881, -1898615747, -2138170700, -1300203238, -207365531, 771641087, -769600634, -2030013168, 1773088179, 562646609, 1102097920, -1868977442, 894708009, 589624539, -1446161430, -1338457204, 776996047, -409391995, 232302396, 1734137362, 1306244365, 1859460679, -187505618, 1094846856, 460517515, -913978775, -1117333960, -1071894810, 1178978945, -243984877, 1462927810, 582068852, 1463520375, 1311946093, -113538556, -985667286, 999866005, -942090961, 961179702, 395289696, -705656157, -2086085183, -506070608, 1983654094, -1136617927, 410093643, 1924045306, 639489276, 1801140543, 1228412437, 499393836, 1994540546, 818592989, -2142959657, 1198756542, -799759112, 296483547, 1735983417, -1341143928, 89269302, -1260830607, -1969876317, 1532504050, -1378546876, 431244295, -244358691, 618413780, 167141882, -821047995, 617773471, -527766746, -1432153168, 335011923, -967845007, -778024288, 194420286, 993088841, -2004382725, 1917710282, -828473044, -1250845393, -675855387, 187628188, 897842930, -25145491, 554477735, 1783147214, -1352550360, 1266426923, 955483721, 762064728, 1967458822, 112374753, -1694854797, -1914281502, -1083239084, 715156567, -677621683, -27929628, 217256394, -1915185099, 655488655, -74487914, -860599488, -1240685053, 394664993, -1260712176, 1111778542, 379432089, -1615992533, 1803878426, 708393692, 9513695, 509572277, 1106879052, 43667106, -1631119875, 1304248055, 705673246, -1305265448, -1235030469, 574503897, 2046224744, 1871921302, 1278320145, -1570842685, 1139493842, -2125556124, -2004908633, 1997773565, -881754380, 604912090, -917725915, 1262275647, 969317318, -465954736, -889777037, -746735471, 748832960, -983145826, -742375831, 1720250011, 862471082, -1013782580, 738245263, -875097403, -1939293572, 1582503634, 538927693, 846999303, 937112302, -1906899000, -109967157, -1026172631, -1236106888, 557385382, 1039159489, -1102640920, 1502951148, -683702980, 1978080370, 1177041008, -1534283740, -1592755144, 2058891034, 384806096, 120717116, 425285524, 803570562, 1954294208, -1384112580, 357533072, -5972446, 1581737224, 393535116, -2097049828, 1508589714, 1148787072, -1490938348, -29552856, -886896070, -1694999088, 754067820, -1188191308, -1885767662, -596654624, -1218938932, -1606926112, 1637742626, 403501352, 956255756, 614696764, 1200468978, -1971975440, -611135932, -68136360, 1657634618, -372517392, 483268284, 89307412, -846130494, 1614357600, -1667320964, -872790064, 1709715746, -1765606744, -700294260, 501156028, 1938071346, 438774336, -602628940, 629215816, 10565050, -188366320, -499096852, -1751989932, 2116165906, -959214176, 1479103180, -299278752, 1640314818, -942794968, -89387092, -824323268, -1230919310, 1993055024, 818634020, 783625016, -516976934, 366135184, -1515376900, 1577548692, 371839042, 912184576, -1297324996, -1200398000, 1041285282, -1315647160, -543308020, -1045626148, 1235230034, 1159074944, -432025516, -1529057304, -619616006, -1110942896, 1448271660, -1826584204, 638028690, 1976080480, 155237772, -1020781728, -783484446, 539571112, -1579158324, -916541444, -1272396366, -1380411920, 341445572, -576367272, 1928739194, 1353409456, -87393860, -503158700, -850327614, -482201568, 860563644, 899662416, -1147100190, 1938456680, 632616012, 285542652, 1666075634, -652277504, 1125904756, -1459061752, 53555130, -3374640, 584788972, -1555951212, -2014588462, -1012956832, -1089749172, -672786144, 1167716226, 1644093160, 218812652, 89445436, -1868944526, -1227194768, -408873948, -481817800, -849302374, -1903267120, 846807228, 1100172948, 1065444994, 1086535872, -1539509444, -1106854768, -814490078, 2113961992, 1175403788, -1229267812, -662487534, -307306624, -1909617132, 1511533096, -1756986054, -575481392, 747623532, 1880021300, 1843693202, -46486688, -338304180, -1711155232, 93374370, -1059379288, 1196780940, -1940592452, -651155086, 1480635632, -674275260, -1864937128, -491456966, 790969200, 836048956, 993877780, 1328444866, 1431558880, -1952561284, 1149423312, 461781154, 799628328, 531014924, -69556292, -202233550, -1760908864, 1749084724, -443970616, 634558650, -1992238704, 1059615212, 1938686548, 406298770, -1686634720, -455259700, 1152677344, 908203714, -720690136, 1157984940, -130808644, -881092110, -51715792, -2070793052, 432350904, 1648921946, 1135977872, 1776841596, 228464916, 979776322, -996484352, -1133176900, 1492459344, -866295006, -2122187448, -1099539956, 1571174492, 1062472914, -439936512, -95122092, -185787928, -234963974, 257142992, 134528300, -1158057228, 383650194, -693046176, -458332916, 369537760, -100505246, 1202506408, 1924092492, -2107440772, -1998523255, -936738658, -1683280564, -454607587, 702224551, 582432720, -303325330, -265882085, 710819709, -1403435670, 413383688, -915626831, 857080611, 1268543524, 612467410, 959465255, 1984919921, -636117066, -284863660, -1681916443, 1734993903, 299681576, -1290436730, 1440918451, -1715299563, -1391151758, -1024685632, 1171991081, -15855973, 913373036, -2071687302, -373139281, 1188180921, -1423714866, 2039939356, -1921383283, -196425481, 1813169152, -731740578, -1701062357, -8066515, -1504183654, 1061889624, 1719585217, 261174899, 16937876, -1355650526, -1334536905, -1162898431, 1604573446, 159264068, -209923979, 1487454687, 46787896, 131178262, -962232541, -1264206619, 166054146, -308081808, -865328359, -1219103989, -85131268, -2053309398, -482922401, -1650739223, 1943550206, -1057625876, 850947133, 1766350855, -2106909712, 1851930510, 222954939, 1167246237, 2065791306, 1922060648, -2112268463, 519586883, 1067543876, -1260794382, -1693841465, -217314927, 919919894, -1532344972, -1648933883, 1340367951, 2073737288, -1893700122, 82805523, -858319755, -211407278, -1642608480, -1704705655, 1389973947, -367931892, -821766246, 1527160591, 1202824537, 1931173870, -1568315780, 1016835821, -1263773289, -1120054688, -1135482818, -268349173, 304382861, 1477730490, -648157448, 540429473, -898394029, -2138627212, 230572290, 1266368919, 1067244385, -684515418, 1436961188, -1205718955, -845445889, -1387665000, -10571082, -1057445053, -1890601339, -2115749086, -1291878384, 945183673, -1370477461, 237055836, -1473751414, -42323329, 718109641, -388200354, 2112879372, 401122013, -1595847705, 974139408, -1453343442, 1798959579, -976968003, -2024582102, 2144175304, -1442618895, 1322676067, 1226389348, -169996654, 971623271, 242316593, -1763062154, -430190700, 1152396197, -264463185, 1235030376, 2039798086, 501723379, -1037373867, -893627854, 717119488, -420231959, -390692005, 1969186220, -1941439046, -869790865, -1888883463, -275959410, -513375140, 1844451021, 1879278135, 279324864, -1766107234, 472071275, 1633278957, 603297368 |
| Chain 3    | base::Wichmann-Hill    | 11690, 10915, 455                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

Random number generator (RNG) names and states used in MCMC chains
