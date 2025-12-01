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
3.98^{-10} which is similar to the difference between the Bayesian and
frequentist Normal UPL’s. It is worth pointing out that the more
uncertainty in the fit, the more the stochastic component might affect
the UPL results. This likely only a concern when the model struggles to
converge because it is a poor fit or because there are not enough data.

| Type     |       UPL |
|:---------|----------:|
| Fixed    | 4.970e-08 |
| Random 1 | 4.946e-08 |
| Random 2 | 4.978e-08 |
| Random 3 | 4.889e-08 |

UPL results for fitting unbounded Normal distributions with fixed and
then 3 different random seeds.

The exact random number generator and state used in each of these runs
is saved in `Bayes_random1$state` (for example) and should be included
in any report for record keeping. Each time you re-run a model with
`random = TRUE` a new set of random states is generated for use. In
order to recreate these results, you will need to pass the list
containing `.RNG.name` and `.RNG.state` from this output as the argument
`RNG.state` when rerunning (while leaving `random = FALSE`).

| MCMC Chain | Name                   | State                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|:-----------|:-----------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Chain 1    | base::Super-Duper      | 87989349, -1223382215                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| Chain 2    | base::Mersenne-Twister | 1, -918131586, 153732152, -200596709, 1451640953, -889384760, -1925072650, 1950927153, 2120974691, 1221268274, 992468100, -281876281, 918442909, -925444332, 780307898, -1196626939, -1377828385, -1617951642, -2104278032, -1826816749, -1316068687, -1359884960, -191085826, 2122546825, -602560069, -1256811638, 1049573996, 202208239, -1252131803, 105964764, 508295858, 788569517, 1815880039, -2105033074, -226826392, -1121001621, 1441007881, -1461477480, -2045647162, 463713313, 1584144563, 403536418, -316490732, 354889879, 1877262989, 2111034276, 1923083658, 1435834869, -31132593, 500773718, -89413120, -552271037, -41794847, -1558687664, -156471122, -745429927, 1833391211, 241768922, 1735369372, -1257472865, -671011083, -770059380, -162241950, -439560643, 656718007, -499071138, -1571681640, 1778014715, 1829798809, 110910504, 685640918, -1618003631, -1629644797, -859357294, 9694500, -217455641, 1351301181, -108829004, -295441126, 2118909541, -1810312449, 514623366, 773667088, 219072947, 159158865, -1225883136, -1089167650, 1450333481, 1515968731, -1413144598, -511836276, -1769230129, 902377605, -336145604, -1961334254, 336814349, -484545977, -1126404050, 1431618952, 1852578955, 601601641, 678947384, -234669338, -835742079, 1191819795, 295304642, 1019770484, -193197961, 433826157, -1083044348, 1433173290, 1382472341, -538218193, -1143487434, -1412112288, -1147238749, 1468694977, -7258704, -580373298, 720347705, -509725621, 458642938, -642068228, -997178049, -167413227, -356200148, 683196930, -264269603, -780999721, 1292558014, -854791944, -638576933, 1681222969, -1602291576, 1502806070, -1610697615, -2001404253, -1937525774, -825833660, 1343342599, 1066182109, -2004884780, -1133711878, -27557563, -1990081121, -1707968730, 1991923632, -421792685, -889704591, -1756764512, 1586858558, -1660116663, -1840537605, -1325284406, -1361289940, 1232482607, -702570523, 660651676, 699943666, -1694012563, -1344777049, -244019506, 1702585384, 953317419, 1668692553, -1908927656, -271966714, -132814879, -907184781, 1698586082, -1831087788, 1777311831, 1139518541, -1961189404, 988392906, -1828899787, -24571249, 1112394646, 1713444672, 281082371, -175071199, 1829112592, -370251538, -1230337895, 1339833643, -925322214, -1894258980, -1849717025, -719427403, -1068114356, -582892894, 1759267325, 404751095, -2054511586, 1261661912, -1855843781, -1172574247, -1302317720, 220611734, 774914577, -1857864765, -1426102318, -452570524, 1105424807, 551702269, -1195328780, -442249766, 31146277, 1610897471, -1026451514, 328559696, -847511437, 385930385, -1255988032, -1400215906, -1078846871, 1848384155, 2026248106, 1847559116, 1871978127, 377224901, -225020292, 833788626, 1673948749, -1452473593, -452712722, -951729208, -164891445, 323316192, 1962687692, 938683552, 1566807426, 1118183656, -1825866004, -521564868, 2033302642, -458334096, 609613860, 1759063096, 194279194, 1392475344, 1685854012, 937744276, -906949758, 1326559680, -1612100036, 287843728, -125393374, 90873096, -1760065908, -204220644, -2130042734, 427739520, 1808883220, -651342040, -1561324998, -433711664, 1796082028, -598957132, 248563730, 847900128, -1514192948, 1244963040, 2098190370, 1045135656, -1662566900, -69411012, 1753906162, -983586064, -1636850108, -1790205352, 1662934842, -659360272, 1629605564, -1763944172, -1373898046, -2145825696, -1376947332, 1348388816, -927284958, 983927464, -948046964, 1091152060, 1979148082, -60028352, 1762944180, 1146190408, -2065130054, 1939864080, -680998164, 1718896980, 1713127698, 599408032, -1279747380, -493928864, 619241410, -344359128, 780095404, 93172540, -268236430, 851557168, 856950052, 2023594808, 1967101658, 1844649872, -610996484, -492469356, -344655806, -182262528, 249775164, -2125004464, 1692944546, 1168283976, -1101068532, -179227940, 1013525842, 1342323840, -1626144684, 775673832, 238821626, 2003684176, -1801789652, -1727620748, -1009497198, 1119692896, 207938956, -901153952, 185387490, -1796641880, -652919604, 842676220, 80176562, -1523315216, 1313274820, 712891736, 461369722, 676334512, 767115708, -1744484268, -2142294078, 1820358688, -916973380, 787759696, 157502434, 1275340904, -1680982964, -1174243076, -1666319374, -1315153664, -1960918668, 240452616, -182878278, 480580048, 721214444, 921762196, -471957038, -1559506592, -1651609780, 1642490144, 23091074, 176213736, 831660268, 2124047420, -654853262, 1672513136, -1240740828, -130251976, 2056862874, 355207888, -974643012, 2086264468, -1382912382, 584562880, 1331960636, 1386294416, -1761903070, 1700593672, 1377387788, 1182622876, -915055086, 1795846016, -2141542892, -932216280, -256570054, -1937514032, 404003948, 652491060, -1036783982, 1343353696, 901161804, -828999712, 642496418, 2002401192, -591098484, 313127100, -1121275534, -1864635152, -696995772, -1182164648, 935321146, 1911855984, 1373116476, 1492629268, -135324222, -544517408, 486502268, -1581965616, -1472343902, -1223556056, -1706719988, 1711394748, 201584946, 310161856, -1533781452, 1296643016, -705507142, 598362512, -156685844, -1726899628, 397682834, 917505824, 1674490316, 1591281120, -834169150, 841407528, 449583788, -1668770628, -961846798, 941777200, -1347953500, -1632317768, 491695450, 1868615056, -1297859716, -681639660, 1828967234, 1195649792, 2095847356, 2131461968, 364180258, 466711880, -567999988, 94001244, 781120722, -686480896, 1998181716, -980573720, 72715258, 200921296, 882298156, -1268874508, -1277382254, 620627040, -1699017460, 1802289888, 1463326123, -1760994532, 263433290, -1753860033, 1687627913, 1362259102, -1780082356, -1135967971, -792425305, -1457464368, 887792494, -1870929381, 656106365, -1585282710, 545226760, 848144561, 15910691, 2079540260, 1198200530, 287470375, 612283761, 632601526, -810999980, 717527525, 1506083823, -657566936, -1096395898, 1819307955, -25802987, -313659022, -1225771584, -188471767, 535296667, 1935836012, 25713530, -1632557905, 744902585, 894650830, -541479652, -1884632947, -1803213577, 874905600, -89447842, -950387413, -927646163, -1084488038, -1289640, 4500929, -266464653, 1193643924, 290299426, 598745399, 1075507201, 1632472326, 1346681156, -1167643531, -460538401, -1124480712, 387466006, 2131401507, 1692659941, 1807240450, 1717010288, -486351079, 792917771, 2057294844, -1031475158, -321414049, -1089219607, -947593986, 713200876, 1928579133, 126254087, -53221392, 631722894, -2133929541, 9277341, 690707786, -226093720, 542833489, -959891389, -1129029308, -358819342, -1187099705, 1821539729, 2076766486, -38272652, -882608123, 485163087, 1840284744, 816937446, -87652589, 439160949, 1856451154, -1676135264, 216272265, 1683439035, -1224291828, -762215526, -1319474417, -6633127, 231554030, 323125884, -105970963, -959801961, -834007456, 1863562302, -597830389, 235169677, -739822406, -1234914568, 26994849, 2128383059, 1897676660, 1273818370, -191466089, 32526177, 2117893030, -1330031196, 53316693, -994246401, 89159064, 992902838, 2144026435, 672125573, -401942750, 1617560592, 206997433, -2046416789, 1583752028, 1312166538, -1014215041, 290750409, -1343548322, 9731852, 35798749, 721127399, 1857588240, -409214674, -1735711269, 1303997117, 2031112234, 1737418952, 1542450673, 183573347, -695207068, 50655890, 1676226407, 1976238385, -327800202, 844340116, 1004556197, 618619055, 1472437608, -1842864826, 610073843, -2109332907, -473137614, 1633688576, 1154625769, 119637851, -1756298836, -1173872198, -54214801, 121889017, 1849537934, 798140508, 1087081165, -1499271625, -1294498181 |
| Chain 3    | base::Wichmann-Hill    | 1985, 9143, 6909                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

Random number generator (RNG) names and states used in MCMC chains
