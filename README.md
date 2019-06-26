# Is there a gender bias in Chemical Sciences scholarly communication? #
Here is example code and example data sets (with sensitive information removed) to interrogate publishing data to highlight gender biases.

##Files##
* *License.txt* has details about license that this code and data is available under.
* *Examples.Rmd* is an example R Markdown file which shows examples of:
  * Example 1: Running gender assignment code (using example file *data/fileWithNameInIt.csv*)
  * Example 2: Basic bar chart with significance bars, and baseline (using example file *data/s1_authorrevieweraverages.csv*)
  * Example 3: Running code for determining if difference between weighted averages is significant (using example file *data/s2_CorrespondingAuthorVsNoSubmissions.csv*)
  * Example 4: Binomial GLM model with one variable (using example file *data/RSCCitedToCitingFile.csv*)
  * Example 5: Binomial GLM model with two variables (using example file *data/RSCCitedToCitingFile.csv*)
  * Example 6: Multinomial GLM model with two variables (using example file *data/s1_OneRowPerReview.csv*)
* *genderDiversitySharedFunctions.R* has shared functions that are called by *Examples.Rmd* and our other code
* *binom.Calc.R* has shared functions for performing binomial test (main useful function is *calculateBinomialProportions*)
* *genderAssignmentByName.R* has shared functions for assigning gender to a dataset based on first names (main useful function is *assignGenderToVector*)
* *authorPositionPlot.R* has code for generating plot of distribution of female authors by author position and number of authors
*assignGenderToVector* is the function which maps first name to gender using the file *data/genderNames.csv*
* *data* directory contains example data sets for running examples as above and additionally *genderNames.csv* is the data set of first names and gender mappings for  running *assignGenderToVector* function *assignGenderToVector* function (to cross-reference against to output deduced gender)
##Dependencies##
This code was run in RStudio and libraries required to run it are:

* *library(ggplot2)*
* *library(data.table)*
* *library(forcats)*
* *library(reshape2)*
* *library(plyr)*
* *library(knitr)*
* *library(weights)*
* *library(effects)*
* *library(vcd)*
* *library(nnet)*
* *library(dplyr)*
* *library(binom)*
* *library(rjson)*

To install any packages which are not currently installed run *install.packages("nnet")*.