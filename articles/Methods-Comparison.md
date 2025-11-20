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
4.51^{-10} which is similar to the difference between the Bayesian and
frequentist Normal UPL’s. It is worth pointing out that the more
uncertainty in the fit, the more the stochastic component might affect
the UPL results. This likely only a concern when the model struggles to
converge because it is a poor fit or because there are not enough data.

| Type     |       UPL |
|:---------|----------:|
| Fixed    | 4.970e-08 |
| Random 1 | 4.873e-08 |
| Random 2 | 4.947e-08 |
| Random 3 | 4.893e-08 |

UPL results for fitting unbounded Normal distributions with fixed and
then 3 different random seeds.

The exact random number generator and state used in each of these runs
is saved in `Bayes_random1$state` (for example) and should be included
in any report for record keeping. Each time you re-run a model with
`random = TRUE` a new set of random states is generated for use. In
order to recreate these results, you will need to pass the list
containing `.RNG.name` and `.RNG.state` from this output as the argument
`RNG.state` when rerunning (while leaving `random = FALSE`).

| MCMC Chain | Name                   | State                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:-----------|:-----------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Chain 1    | base::Super-Duper      | -885537271, -1085173675                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| Chain 2    | base::Mersenne-Twister | 1, 715102974, 1659715768, 263178907, 762407161, -1266197944, 793642358, -1949297487, 170088675, 649484210, -1593702396, 2124307271, -1176735203, 543541396, -2090481606, -1350915451, -532030625, -720751642, -319275408, -308055661, 282277425, -403723040, -27624066, -1595887607, 1790058555, 731098634, -1129251348, 1650455151, 1448295845, 1634041180, 2111668530, -669999059, -1612210457, 1102165006, -1925988888, -2103547925, 1574746249, -1701536744, -1359217850, -259389023, -683427789, 1533900194, 1950896276, 1832285975, -764280563, 239298596, -565909494, -1524179595, 41389263, 250470358, -961552512, 697783235, 890525025, 675097552, 175491886, 507360985, 923690219, 1943479642, -1143515364, 1701220895, -826494347, -2118484212, -595103006, -388601923, 1453711415, 1859267294, 1536366104, 1088436603, -139382503, 228744616, -461012650, 1929413073, 712032643, -1250344942, -1905413212, -738081945, 1173903549, 18260020, 2062758810, -1703624731, 1390283647, -1126401274, 214889616, 1899808051, 213983953, -243881856, -763801250, 547317417, 1002459483, 479247210, -29838836, -355610033, 897154821, -935200836, -386518382, -575112819, 1922064839, -988282194, -775648504, 622180619, -979859479, 1435328696, -1359198362, -288922623, 1573409427, 1902212418, 1470356212, 1124411127, -597727507, 1755804036, -1927217238, 520586517, -14766161, -1365516362, 777438688, 2061720867, 1169322561, -1318838992, -1896171698, -1273766471, -862158901, 1126151290, 454403452, 2099363263, -1590625643, -103683924, 791147138, -1221150115, -128996009, -2078027714, 514729080, -170168485, -1406231879, -1045514488, 1856998838, 611767025, -1892608733, -583643278, -1572692540, 1911104903, 943908701, -1001838252, -1449219206, 1833241285, 528325663, 739716006, 1604758832, 1874628563, 1333689841, -802764000, 1297475006, -486351415, 1735917947, 502122314, -996747348, -1585914193, 2128436325, -708801508, -1878744462, 1822823661, -174912217, -174016178, 248797608, 1479335339, 85685961, -1158385960, -1388448890, -1107368351, -2118721549, 2062897762, -1156340524, 1487979991, -1462685747, -1958466716, 665284426, 2036283061, -963858929, -1838907626, -659412544, 1556593027, 2006859681, 1154805648, 1619776622, 2113839641, 2040195755, -1835176294, 247130716, -1049315489, 1654933813, 1453435084, -1990962654, -366490499, -2013776521, -901481826, 392108376, -199827269, -2060050855, 1056566760, -1692977130, 1728676753, 171715651, 1618903890, 2000914404, 130971175, -1095446915, 18640756, 736887642, 288421285, 810430911, 892615238, -631252784, 1417425651, 594453521, 83800640, 992912926, 2068705513, 1519718939, 735113770, 149132620, 788461327, -995444155, 822438908, 1966535250, 732057549, -903853689, -2136130450, 2129396808, 1458871883, -98451744, 57225932, -705367904, 1604926338, 729608936, 774279916, 450272316, 783454834, -987270032, -1382889948, -1625668552, -454543334, -305800496, -1098755012, 119022740, -1772446590, 66144192, -2133403332, 1580323472, 345222178, 2001093640, -2118154100, 2004696092, 1422939794, -653032320, -1707110380, -1405098712, 549080634, -1090362160, 1955255404, 1052455604, -12511214, -107968544, -1607815220, 783650272, 773701154, 1174939944, -1354854132, 1917281852, -1102114574, -1374672144, 861983556, -1177691560, -204635590, -1804138768, -1823798084, 1428360724, 1946124482, -1988222880, -226513540, -1558102832, 1410812962, -2144126296, 811188108, -1961715268, 1772061746, -288716992, -766584652, -679103672, -46970182, 168365840, -715015956, 33444948, 1699526418, -172608608, 80846796, -1426805920, -912170814, -843168216, -383445588, -1943417028, 1797480306, 25719600, -823287260, 905506872, 1701579994, -345390704, 1692579580, 1044909972, 897966914, 1920326144, 1735533116, -1677797808, 731058850, 835173192, -125953524, -1469338404, 1339241042, -686897024, -1277084844, 1598099944, 639593210, 479582032, -170679252, 315594100, 1698782866, 772586080, -987213172, -1641747616, -32853278, -648750936, -262516532, 638665212, 742064050, -1758882320, 1822469572, -66160808, -1734222982, -1667580496, -449281604, -1642782636, -1971031102, 1362256672, 1186387900, -1923960240, 910929378, 1199239528, -86454964, 1423220988, -353467662, -1002524928, 732888692, -2138685432, -1249431110, 1467437008, 706234860, 96839572, -718957870, 536411744, 1068115276, 1235961120, -1875105150, 1719867880, 1781432300, -624310724, -901462414, 1784195952, -1923339996, -683171272, 1476942234, 990935504, -1848744516, -172615532, 523072642, -1339513408, 629181244, -1875694448, 112745250, 1129756168, 1585440012, 22541724, 1145351698, -15942784, -516041964, 630747432, 621560890, 1908698064, -899310996, -226504140, -1370269038, 2038916448, 1117804876, -588240928, 1883682210, 563099048, -908208500, 715095484, -1914712462, -193301520, -265392572, -269684648, -1055557574, 283402352, 1447106620, -2126307052, -1224301886, 1905485536, -1144440708, -18827568, 1713191330, -287266776, 2000816908, -929576516, 1684443954, 1164376768, 950955060, 131526344, 1348056250, -1216719216, -1008574740, 739494484, -2010664302, 76003616, 1098367436, 1196860128, -2046201662, -1493881816, -528771156, -189916484, 1466190834, -1649002960, 1150238628, -2041836104, -2147268774, -872279920, 1709800316, -241606380, 152860994, -240467712, -996864580, -941891248, 2056444194, -2092377528, 327200780, -488778148, 1252739538, -88476160, 1770026324, -1736053528, -836714246, -1680660784, 794610476, -1210871564, -1225549422, 518405472, 232950796, -399590176, -576581845, -1675219812, -869834038, -1176228929, 1068705545, -117075938, 1507975628, -1786827875, -1487460825, 787295312, -2067557138, 1346212251, -529512451, 924697322, 1528386952, 2119422769, 336579491, 1770265508, 481523282, -2077829721, 247992817, 2145797430, 712519124, -1654226075, -1580135569, 641484200, -812229882, -764322253, -1532275563, -1011462926, 1890204992, 1462010537, -261747941, 1974516972, 1232421114, -604143313, 1397978425, 1875949390, 1001225628, 1211670285, -1876256393, -1446052480, -1186833442, 1421980843, -1095299155, 1339917338, 338291416, -489381055, -282389517, 1377623572, -1798129758, -91060041, -1622453631, -1852870010, -1619623228, -1248844811, -1757889953, 2054352056, -2128270954, 91009955, 560113509, 693231234, 165821424, -598688359, -1915604085, -1583841668, -2011892566, 586819295, -1975993239, 740365950, -2105975188, -1390509379, 1626005383, 1013287792, 44726798, -1271408837, -1189852131, -1045592886, 396050408, -996324143, 2008656067, -952868924, -363717518, -447204025, 1011820817, 2035424406, -1064873484, -586005627, -1486140721, 31446984, 641457766, -328748909, 1840443381, 1043769810, 1792225824, -158553079, 143962427, 932140172, -1057316582, 725735311, -1064671015, 1609099118, 717725436, -447619987, -702699497, -297184800, -1388274242, 1219273611, -1872718835, 1042232378, -1236044680, 894394401, 1275210451, -1025381900, -1559702910, -111202537, 305145313, 1301306150, 1725896228, 291786965, -531173249, -1588943592, -387834826, 566526659, 546352901, -1728175454, 1865945232, 173531193, 1906725099, -107265572, 214812682, 474003199, 1718445897, -1456710434, -316620148, 214550109, 312724583, 1492132240, -487128658, -615025573, 324117565, 976644778, -1310304440, -1642079375, -916185885, -1133366300, -1835310318, -1545359641, 220043441, -180643082, 2088484116, -639573467, -79007953, 1644253416, 1880175046, -2077761421, 561892053, 1639258290, -1848357760, 292212841, 906267867, -762101716, -892010438, -269549841, 1456623993, 1422867982, 493649884, 1942009933, -1099015241, -446586053 |
| Chain 3    | base::Wichmann-Hill    | 14279, 15840, 6675                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |

Random number generator (RNG) names and states used in MCMC chains
