# R scripts to generate control chart limits used by IHI's PowerBI application
Notes by [Kevin Little](mailto:klittle@iecodesign.com?subject=[GitHub]IHI_Covid_display_Nov2020), Ph.D., Informing Ecological Design, LLC 

This project implements a method based on control charts to view phases in daily reported deaths from COVID-19. The method was developed by Lloyd Provost, Shannon Provost, Rocco Perla, Gareth Parry, and Kevin Little, with an initial focus on death series and is described [here](https://academic.oup.com/intqhc/advance-article/doi/10.1093/intqhc/mzaa069/5863166).

Gareth Parry used SPSS to develop the initial IHI presentation in the spring and summer of 2020.  His SPSS code created data tables for countries and U.S. states and territories.  Gareth created a PowerBI script to read these tables and created the data displays.  The IHI display is [here](http://www.ihi.org/Topics/COVID-19/Pages/COVID-19-Data-Dashboard.aspx). 

In September, Kevin Little advised by Lloyd Provost replaced the SPSS code with R code that requires less daily human intervention.   This document describes the R code function and limitations.  The PowerBI visualization remains essentially the same.

## The foundation of our control chart modeling:  Epochs and phases
Epidemiologists use the term 'phases' to describe the structure of a pandemic.  The [core WHO pandemic reference](https://www.ncbi.nlm.nih.gov/books/NBK143061/) for example says: "The WHO pandemic phases were developed in 1999 and revised in 2005. The phases are applicable to the entire world and provide a global framework to aid countries in pandemic preparedness and response planning." 

We use epochs and phases to describe the patterns in Covid death series. We first observed the patterns in the death series for locations like China and New York in the United States in late winter and early spring 2020.  As our use of the term phase is potentially confusing to users familiar with WHO terminology, let's explain.

### Table of Epochs
| Epoch | Description | Control Chart structure |
| ----- | ----------- | ----------------------- |
|   1   | pre-exponential growth | c-chart on original scale |
|   2   | exponential growth | individuals chart fitted to log10 of the death series, back transformed to original scale |
|   3   | post-exponential growth:  flat trajectory or exponential decline | individuals chart fitted to log10 of the death series, back transformed to the original scale |
|   4   | stability after descent | c-chart on original scale |

Within any Epoch, we require at least one phase.  For example, within Epoch 1, if the algorithm does not detect exponential growth but shows an increase in average deaths, additional phases will display c-charts with means higher than the first phase.  Here's a display of the state of Arkansas death series showing multiple phases within Epoch 1.   The red dot represents a 'ghosted value', likely associated with an administrative action to report an unusually large number of deaths in one day. Note that in the IHI implementation, red dots are reserved for the start of a new phase; ghosted values are presented as light blue. See below for further discussion of ghosting.

![phases within Epoch 1](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/images/ARkansas%202%20Nov%202020.jpg)

In other words, in our application, a phase is a distinct time period described by a distinct control chart.

A location always starts in Epoch 1.  How do we handle a series with rare deaths associated with Covid after exponential growth and decline?  The algorithm for fitting in Epoch 4 is identical to the logic in Epoch 1.  The only distinction is that Epoch 1 characterizes the start of the death series.

Check the detailed table [here](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/Phase%20and%20Epoch%20logic%209%20November%202020%20public%20version.pdf) to see the rules for transitions from phase to phase within and between epochs.


## Who can use this project?

People who have a basic understanding of Shewhart control charts and want to apply control chart methods to characterize how reported events from COVID-19 change over time.  People who have skills in R can modify the code in order to load data sources to replace the built-in sources and to consider other measures, like hospitalizations or ICU cases.

## Getting Started

This document describes the R code and what you need to run it yourself.  It provides sample output for you to check for successful use **not yet linked/finished**

### Prerequisites

You need a current version of R (we developed this using R version 4.0.2 and RStudio version 1.3.959).   You need to be connected to the internet to enable update of the input data tables.

The code looks for current data in a local folder **data**; if data are not current, the code will attempt to connect to web sites to obtain current data:

```
data_file_country <- paste0('data/country_data_', as.character(Sys.Date()), '.csv')
data_file_state   <- paste0('data/us_state_data_', as.character(Sys.Date()), '.csv')


if (!file.exists(data_file_country)) {
  covid_data <- httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                          authenticate(":", ":", type="ntlm"),
                          write_disk(data_file_country, overwrite=TRUE))
}

if (!file.exists(data_file_state)) {
  download.file(url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
                destfile = data_file_state)
}
```
### Copying the code for local use

[Click here to download the latest version](https://github.com/klittle314/IHI_Covid_display_Nov2020/archive/master.zip) **link not yet active**

Alternatively, if you understand how repositories work, you can fork the master branch for your use.

Make sure you have installed the following libraries and dependencies referenced in the generate-data-files.R script  

```
library(ggplot2)
library(httr)

```
As our code is running on Amazon Web Services, you will also see that we load `library(aws.s3)`.

### Structure of the scripts
The core files are
1. generate-data-files.R.  This file:
    - loads the data from external websites for country and U.S. state/territory COVID daily data.  
    - does minimal editing of the data frames to assure common names.  
    - for the U.S. state/territory file, it converts cumulative deaths or cases into deaths reported daily.  
    - when running in interactive mode, it creates plots of the input country and U.S. state and territory files and saves the plots as pdf file in a folder named samples.
    - creates five comma-separated value files used as input to PowerBI, writing the files to a folder named output
      - Dates by Phase II.csv, a file used to show the count of U.S. states and territories in one of the four Epochs.
      - NYT Daily Multiphase.csv, a file with the control chart parameters for the U.S. raw death series
      - NYT Daily Multiphase ADJ.csv, a file with the control chart parameters for the U.S. adjusted death series
      - Country Daily MultiPhase.csv, a file with the control chart parameters for the country raw death series
      - Country Daily MultiPhase ADJ.csv, a file with the control chart parameters for the country adjusted death series
    
2. functions.R This file contains the core functions.   In addition to several small auxiliary functions, we created four functions.  See the notes section below for further explanation of our design choices.
    - detect_outlier_dates, a function that identifies dates with records deemed to be unusually large that will be excluded from creating the control charts. 
      - Inputs:  input data frame; threshold to declare an outlier
      - Output:  a column appended to the data frame with value = TRUE if the deaths value for a given day is assessed as an outlier 
    - force_monotonicity, a function forces U.S. data series to be monotone non-decreasing. 
      - Input:  vector of deaths by state or territory from the New York Times source
      - Ouput:  vector of deaths for the specific state or territory with negative values accounted for (see below)
    - model_phase_change, a function that detects whether the series indicates the start of a new phase in Epochs 2 or 3.
      - Inputs:  a data fame, subsetted to records such that the date > date_phase_end & date <= min(date_phase_end + 21, date_max, na.rm = TRUE); the name of the series to model, in our case the death series.
      - Output: a list that contains the linear model fit to the raw data values; the average log10 deaths; a logical value indicating the sign (plus or minus) of the slope of the log10 linear model; a logical value indicating whether or not the slope of the log10 linear model is statistically significant (p < .05); the median moving range of the residuals from the linear model fit.
    - find_phase_dates, a function that does the 'heavy lifting'; this function checks for beginning and end of phases and generates the control chart parameters for each phase.  It also adjusts for within-week seasonality in Epochs 2 and 3.
      - Inputs:  a data frame with the death series and indicators of ghosted data, by location; a logical variable to adjust the data for within week seasonality; a logical variable to look for ghosted values.
      - Output:  a data frame that appends new columns to the input data frame:  indicators of epochs and phases within epochs, start dates for phases; control chart parameters (midline and upper and lower control imits) for each phase.

If you run the generate_data_files.R in interactive mode, the code will create pdf files containing plots of the raw and adjusted death series for countries and U.S. states and territories.   The code will also print out the a table of phases and Epochs for each location.
      
### Key parameters
These parameters are currently 'hard-coded' but should be expressed as parameters for generalization and sensitivity testing.

minimum control-chart length:  at least five records in Epochs 1 and 4 to estimate parameters.  In Epochs 2 and 3, we require 21 records to estimate parameters.  If the exponential fit in Epochs 2 or 3 is being tested with the most current records, we require a minimum of five non-zero records for a preliminary fit.    

maximum length of series to establish control chart limits:  21 records, corresponding to three calendar weeks.

starting number of deaths:  at least eight deaths are required in the first phase of Epochs 1 and 4 to estimate control chart parameters.  This parameter accounts for the potentially large number of days with zero deaths the first phase of Epochs 1 and 4.

shift length:  eight consecutive values above or below the midline of a phase signal a special cause and the start of a new phase.

## Additional Notes

### flagging and setting aside unusually large values:  ghosting
The detect_outlier function examines each daily record to assess if the record is unusually large compared to days preceding and following the record.  During spring 2020, we followed news reports of 'data dumps' and flagged these events manually.  Such data dumps are a simple and clear example of a special cause of variation in the data series.  If you adapt the R code for your own use, we recommend that you allow users to identify records that should be excluded from calculations using your knowledge of the reported death series.  In our development team, we refer to records flagged by the detect_outlier function as 'ghosted' because the original PowerBI script plotted such values with a pale dot.

### monotonicity
The New York Times data table provides cumulative death counts for each U.S. state or territory.  The code differences the cumulative death series to get daily deaths.  The cumulative death count series shows adjustments for 27 states and territories as of 8 November that make the series non-monotone increasing--52 records are less than previous records, within state or territory.  This means that the differenced series will have negative values.   To eliminate negative deaths in the differenced series, the function allocates the negative values to previous records so that the revised series has only non-negative values.

### adjusting
By June, the development team saw that some locations reported deaths in a way that two days a week tended to be lower than the other five days in each calendar week.  As a result, the IHI application adjusted for a day-of-week effect.  We developed the R code to mimic the IHI application:  the R code generates adjusted data for each location.  The adjusted data series is then used as input to the epochs and phases algorithm.

For example, Illinois shows a strong pattern of two days a week lower than the other five.  The pattern is easy to see starting in Epoch 3, phase 1:

![Illinois series](https://raw.githubusercontent.com/klittle314/IHI_Covid_display_Nov2020/main/images/Illinois%20Raw%20Deaths%20Seasonality%202020-11-08_13-43-19.jpg)

Illinois raw deaths epoch and phase start dates:
| Epoch | overall phase | phase within epoch: start date|
| ----- | ------------- | ----------------------------- |
| Epoch 1 | phase 1 overall | phase 1 within epoch: 2020-03-17|
| Epoch 2 | phase 2 overall | phase 1 within epoch: 2020-03-27|
| Epoch 3 | phase 3 overall | phase 1 within epoch: 2020-04-25|
| Epoch 3 | phase 4 overall | phase 2 within epoch: 2020-06-13|
| Epoch 3 | phase 5 overall | phase 3 within epoch: 2020-07-05|
| Epoch 3 | phase 6 overall | phase 4 within epoch: 2020-10-06|
 
The excerpt of the Illinois data records shows that deaths reported on Sunday and Monday are systematically lower than the other days of the week.  This appears to be an administrative source of variation in the death series, a special cause of variation, which will affect the control limits. Two low values each week will tend to inflate the variation and widen the control limits.  

Observing this pattern in multiple data series, we sought to eliminate the special cause of variation.  We limited the adjustment to data within Epochs 2 and 3 as the control limits are derived from the range of the day to day differences.  In Epochs 1 and 4, we did not apply the adjustment; the c-charts are defined solely by the average value of the series and may be dominated by many days with zero deaths.

Here's the logic for adjustment:

1.  Fit the original ("raw") series to obstain phases and epochs.
2.  Within each fitted phase with at least 21 records for Epochs 2 and 3 and using the linear fit to the log10 deaths:
(a) compute the residuals as (observed - midline) on the log10 scale.  
(b) by day of week, get the median of the residuals.   This is the adjustment for day of week.
(c) compute the log10 adjusted death:   adjusted log10 death = log10 observed death - adjustment for day of week.
(d) compute the adjusted death as 10^adjusted log10 death
(e) normalize the adjusted deaths so that the total adjusted deaths in the phase matches the total deaths in the phase:
               norm_adjusted death <- adjusted death * (total raw deaths/total adjusted deaths)
(e) report the adjusted death as round(norm_adjusted death)
3. Stitch together the adjusted data, phase by phase.

Once we have the adjusted data series, apply the algorithm to get the epochs and phases.

The adjustment logic will fail to adjust some days of the week if the state or country reports zero deaths consistently on those days.  The failure stems from our fitting of log10 deaths:  before fitting, we set the zero deaths to missing.  This means that the day with zero deaths never enters the calculation for adjustment.  See below for discussion of this limitation to our approach. 

Louisiana provides a clear example of the situation, with reported deaths on Saturdays identically zero for three months starting in July:

![Saturday pattern](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/images/Louisiana%20Raw%20Deaths%20Seasonality%202020-11-08_15-25-54.jpg)

In the Louisiana case, our adjustment actually seems to work well: from the data series, it appears that Louisiana is reporting only six days each week for weeks starting in mid-summer and model fits accommodate this structure. 

### computations related to the c-chart
The function find_phase_dates calculates the c-chart center line and upper control limit in Epochs 1 and 4.  As described above, the c-chart calculations are based on several other parameters.  The c-chart calculations require at least 8 non-zero deaths; the maximum number of records used for the c-chart calculations is 21.  As the find_phase_dates function iterates through the records, the calculation stops as soon as a special cause signal is detected.  We designed the c-chart calculations to identify the tentative starting point of exponential growth and recognize this approach might not reproduce the c-chart designed by an analyst to look at a sequence of events.  An analyst might require a minimum number of records (e.g. 15 or 20) and iteratively remove points that generate special cause signal(s).  See the additional discussion below on the difference between the rules used in the first phase of Epoch 1 or 4 and subsequent phases within those epochs.  The detailed table [here](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/Phase%20and%20Epoch%20logic%209%20November%202020%20public%20version.pdf) summarizes our rules for transitions from phase to phase within and between epochs.

### computations related to the fit of the regression line

#### Calculation of the control chart limits using residuals from linear regression on log10 deaths
The code uses the median moving range to estimate 'sigma-hat' in the calculation of the individuals control chart parameters.  That's why we use the multiplier 3.14 to compute the upper and lower control limits rather than the customary 2.66 value.  The median moving range is more robust to one or two large point-to-point ranges relative to the average moving range.  Usually, use of the average moving range requires two stages:  examine moving ranges to determine if there are any that are unusually large on a chart of moving ranges; discard any ranges that exceed the upper control limit on the range chart, and recalculate the limits on the individuals chart.  We chose to use the median approach to simplify the derivation of the individuals control chart limits.

#### Use of test of significance for the slope of the regression fit on log10 deaths
In model_phase_change, we use the test of significance (p-value) and the sign of the serial day coefficient to determine exponential growth, no evidence of growth or decay, or exponential decline. 

| Epoch | calculation |
| ----- | ----------- |
|   2   | p < .05 and slope positive:  exponential growth |
|   3   | p < .05 and slope negative:  exponential decay  |
|   3   | p >= .05:  neither growth nor decay |


### Details and limitations of the current method
**Limit anomaly** In Epoch 3, the log transformation stretches the scale of the control limits when there are multiple days close to zero.   For example, in the Wisconsin raw data (upper limit increase in phase 4 ia above the upper limit in phase 3 despite the average value in phase 4 below the average value in phase 3. Our method implies we could expect occaisionaly  much higher values in phase 4 relative to phase 3 and not declare a change in phase.

![Wisconsin limit anomaly](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/images/Wisconsin%20limit%20anomaly%202020-11-08_15-56-51.jpg)

**Modification of 'Shewhart criterion 1':  points beyond the control limits and overdispersion**  We modified the Shewhart criterion.  Except for the initial phase of Epoch 1 or Epoch 4, we require two points above the control limits in Epochs 2 and 3 to signal the start of a new phase.  We expect to see more than 'usual' variation in the death series.  We dampen the trigger of a new phase by requiring a stronger signal.  For example, a single large value sometimes reflects a 'data dump' by the reporting entity that is not screened by our ghosting function.

Similarly, we require two points below the lower limit in Epochs 2 and 3.   We turn this rule 'off' in Epochs 1 and 4 as we found the combination of within week seasonality (pattern of two days low each week) and presence of zero values tended to generate numerous phase changes that did not appear to reflect the system performance.

In locations with small counts, the variation sometimes appears more than expected in the Poisson model underlying the c-chart.  More complicated charts based on a distribution like the negative binomial can handle extra dispersion but we elected to stay with c-charts and modify the signal rule.   Idaho in phase 4 illustrates the over-disperson and the consequence of requiring two points above the control limits to indicate a start of a new phase.

![Idaho overdispersion](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/images/Idaho%20overdispersion%202020-11-08_16-23-37.jpg)

**Requirement of 21 records to fit the control chart in Epochs 2 and 3 can lead to problematic fits**  Our requirement of 21 records before calculating control limits in Epochs 2 and 3 can lead to special cause signals within the 21 record span. We imposed the 21 record rule to align with typical control chart advice to have 20 records to calculate chart parameters and to allow three weeks of data for the adjustment algorithm.  

Our method does not react to the signals within the initial 21 records even though such signals undermine the basis for the control limit calculations.  For example, Turkey shows a signal in the beginning of phase 7, with eight consecutive points above the mid line in records six to sixteen of the phase.  An analyst working by hand might identify the signal of special cause in 11 deaths above the midline and decide not to calculate limits or annotate the phase to indicate the poor fit.

![Turkey signal in baseline](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/images/Turkey%20signal%20in%20baseline%202020-11-08_16-35-49.jpg)

Similarly, there are two points below the lower limit in the sixth phase of the United States raw death series, at records 16 and 17 in the phase. Our algorithm ignores this signal of special cause in calculating the parameters of for fitting the phase.

![US signal in baseline](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/images/United%20States%20signal%20in%20baseline%202020-11-08_16-47-50.jpg)

**Bias induced by the adjustment method**  In Epochs 2 and 3, we set zero values to missing before calculating the model fit on the log10 scale.   Eliminating the zero values has the effect of biasing the fit upwards.   We have not characterized the size of the bias.  An alternative to a linear model fitted to log10 deaths:  fit a Poisson regression, possibly allowing for over-dispersion. Zero values will be handled directly as observed values.  As this is not the approach used in the initial IHI application, we did not pursue this alternative in the current R development.

Also, the adjustment procedure can produce values in the adjusted series that are larger than those in the observed series.  Consider Florida's raw death series, phase 5. ![Florida induced large value](https://github.com/klittle314/IHI_Covid_display_Nov2020/blob/main/images/Florida%20adjusted%202020-11-08_19-50-02.jpg)  Here is the table of relevant values for the Sundays in the series.   

|date     | weekday| New_Deaths| Deaths_adj| log10_Deaths|        adjustment| log10_residuals|
|:----------|-------:|----------:|----------:|------------:|----------:|---------------:|
|2020-09-06 |       1|         38|  100.30020|    1.5797836| -0.4215182|      -0.2888926|
|2020-09-13 |       1|          8|   21.11583|    0.9030900| -0.4215182|      -0.9655863|
|2020-09-20 |       1|          9|   23.75531|    0.9542425| -0.4215182|      -0.9144337|
|2020-09-27 |       1|         10|   26.39479|    1.0000000| -0.4215182|      -0.8686762|
|2020-10-04 |       1|         43|  113.49760|    1.6334685| -0.4215182|      -0.2352078|
|2020-10-11 |       1|        178|  469.82726|    2.2504200| -0.4215182|       0.3817438|
|2020-10-18 |       1|         50|  131.97395|    1.6989700| -0.4215182|      -0.1697062|
|2020-10-25 |       1|         12|   31.67375|    1.0791812| -0.4215182|      -0.7894950|
|2020-11-01 |       1|         28|   73.90541|    1.4471580| -0.4215182|      -0.4215182|

All of the log10 residuals are negative except for the record on 11 October.  Hence the median residual is negative, -0.4215182.  The adjustment rule sets the adjusted deaths as 10^(log10_Deaths - adjustment).   For 11 October, this leads to a raw adjusted value of 469.8272 = 10^(2.2504200 + 0.4215182).  The adjustment algorithm then normalizes the adjusted death series in the phase to have the same total number of deaths as the raw total deaths, which increases the value to 523.   This value is almost twice the value of the maximum observed deaths.

Thus, we have problems with both the raw series and the adjusted series.  The raw data series can show a systematic pattern day-of-week reporting; that is, the series can have a special cause of variation arising from measurement reporting that may affect the control limits. On the other hand, the adjusted data has an upward bias in the model fit and may induce records larger than any observed in the raw data.  Here's my current view:  The message in the charts should be an interpolation between the ‘raw’ and the ‘adjusted’ displays.   A display that incorporates adjusted data should allow the user also to see the raw data to make a considered interpretation.  "Presentation of results, to be optimally useful, and to be good science, must conform to Shewhart’s rule: viz., preserve, for the uses intended, all the evidence in the original data.” (W.E. Deming, “On probability as a basis for action”, *American Statistician*, **29**, No. 4., 148)


