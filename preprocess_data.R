library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


# Read dataset
cleveland <- read_file('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/cleveland.data')
hungarian <- read_file('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/hungarian.data')
long_beach <- read_file('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/long-beach-va.data')
switzerland <- read_file('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/switzerland.data')

# variables names
HD_colnames <- c("id"
                 ,"ccf"
                 ,"age"
                 ,"sex"
                 ,"painloc"
                 ,"painexer"
                 ,"relrest"
                 ,"pncaden"
                 ,"cp"
                 ,"trestbps"
                 ,"htn"
                 ,"chol"
                 ,"smoke"
                 ,"cigs"
                 ,"years"
                 ,"fbs"
                 ,"dm"
                 ,"familyHx"
                 ,"restecg"
                 ,"ekgmo"
                 ,"ekgday"
                 ,"ekgyr"
                 ,"dig"
                 ,"prop"
                 ,"nitr"
                 ,"pro"
                 ,"diuretic"
                 ,"proto"
                 ,"thaldur"
                 ,"thaltime"
                 ,"met"
                 ,"thalach"
                 ,"thalrest"
                 ,"tpeakbps"
                 ,"tpeakbpd"
                 ,"dummy"
                 ,"trestbpd"
                 ,"exang"
                 ,"xhypo"
                 ,"oldpeak"
                 ,"slope"
                 ,"rldv5"
                 ,"rldv5e"
                 ,"ca"
                 ,"restckm"
                 ,"exerckm"
                 ,"restef"
                 ,"restwm"
                 ,"exeref"
                 ,"exerwm"
                 ,"thal"
                 ,"thalsev"
                 ,"thalpul"
                 ,"earlobe"
                 ,"cmo"
                 ,"cday"
                 ,"cyr"
                 ,"num"
                 ,"lmt"
                 ,"ladprox"
                 ,"laddist"
                 ,"diag"
                 ,"cxmain"
                 ,"ramus"
                 ,"om1"
                 ,"om2"
                 ,"rcaprox"
                 ,"rcadist"
                 ,"lvx1"
                 ,"lvx2"
                 ,"lvx3"
                 ,"lvx4"
                 ,"lvf"
                 ,"cathef"
                 ,"junk"
                 ,"name")

# use gsub to remove pattern from string, out line breaks that follow numbers
cleveland_df <- read.table(text = gsub('(\\d)\\n', '\\1 ', cleveland), fill = TRUE, na.strings = '-9', col.names = HD_colnames)
hungarian_df <- read.table(text = gsub('(\\d)\\n', '\\1 ', hungarian), na.strings = '-9', col.names = HD_colnames)
long_beach_df <- read.table(text = gsub('(\\d)\\n', '\\1 ', long_beach), na.strings = '-9', col.names = HD_colnames)
switzerland_df <- read.table(text = gsub('(\\d)\\n', '\\1 ', switzerland), na.strings = '-9', col.names = HD_colnames)

# combine obs from four datasets and drop variables "not used" and "irrelevant"
HD_datasets <- bind_rows(cleveland_df, hungarian_df, long_beach_df, switzerland_df) %>%
  select(1:58) %>%
  select(- one_of('id', 'ccf', 'dummy', 'restckm', 'exerckm', 'thalsev', 'thalpul', 'earlobe', 'restckm', 'exerckm'))

HD_datasets <- as_tibble(HD_datasets)
 
# exploring raw data
summary(HD_datasets)

# variables type conversion to factor
HD_datasets %>% mutate(sex = factor(sex, levels = 0:1, labels = c("Female", "Male")))-> HD_datasets
HD_datasets %>% mutate(cp = factor(cp, levels = 1:4, labels = c("typical angina", "atypical angina", "nonanginal pain", "asymptomatic")))-> HD_datasets
HD_datasets %>% mutate(restecg = factor(restecg, levels = 0:2, labels = c("normal", "ST-T abn", "LVH")))-> HD_datasets
HD_datasets %>% mutate(slope = factor(slope, levels = 1:3, labels = c("upsloping", "flat", "downsloping")))-> HD_datasets
HD_datasets %>% mutate(restwm = factor(restwm, levels = 0:3, labels = c("none", "mild or moderate", "moderate or severe", "akinesis or dyskmem")))-> HD_datasets
HD_datasets %>% mutate(thal = factor(thal, levels = c(3,6,7), labels = c("normal", "fixed defect", "reversable defect")))-> HD_datasets

# variables with a yes and no labels 
var_list <- c("dig", "prop", "nitr", "pro", "diuretic","exang", "xhypo")
HD_datasets <- HD_datasets %>% mutate_at(vars(var_list), .funs = list(~factor(.)))

# unite cmo, cday and cyr then convert to date data type
HD_datasets <- HD_datasets %>% mutate(datesp = ymd(paste(cyr, cmo, cday, sep="-")))

# count the missing values and select variables with less than 25% missing values
HD_datasets_sub <- HD_datasets %>% select_if(~sum(is.na(.)) < 200)

# select only numeric columns
# gather - Convert to key-value pairs (column name-value)
# Plot the values
# in separate panels, according to key
# as histogram to detect any outliers
HD_datasets_sub %>%
  select_if(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


