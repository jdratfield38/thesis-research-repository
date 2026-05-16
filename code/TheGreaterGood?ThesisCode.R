library(readxl)
library(tidyverse)
library(psych)
library(ez)
library(corrplot)

ros <- list()

# Raw data
ros$df <- read_excel("Study1ROSData.xlsx", col_names = FALSE)

# Conversion to numeric data
ros$df <- as.data.frame(lapply(ros$df, as.numeric))

# Imputation of row means
ros$df <- as.data.frame(
  t(apply(ros$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)

# Calculation of reliability
ros$reliability <- psych::alpha(ros$df)
ros$alpha <- ros$reliability$total$raw_alpha

print(ros$alpha)

# Calculation of participant-level scores
ros$scores <- rowMeans(ros$df, na.rm = TRUE)

# Descriptive statistics of data set
ros$mean <- mean(ros$scores)
print(ros$mean)
ros$sd <- sd(ros$scores)
print(ros$sd)
ros$range <- range(ros$scores)
print(ros$range)

# One-sample t test
ros$ttest <- t.test(ros$scores, mu = 0.5)
print(ros$ttest)

# Cohen's d
ros$cohens_d <- (ros$mean - 0.5) / ros$sd
print(ros$cohens_d)

############################
# CORS
############################

cors <- list()

# Raw data
cors$df <- read_excel("Study1CORData.xlsx", col_names = FALSE)

# Conversion to numeric data
cors$df <- as.data.frame(lapply(cors$df, as.numeric))

# Imputation of row means
cors$df <- as.data.frame(
  t(apply(cors$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)

# Calculation of reliability
cors$reliability <- psych::alpha(cors$df)
cors$alpha <- cors$reliability$total$raw_alpha
print(cors$alpha)

# Calculation of participant-level scores
cors$scores <- rowMeans(cors$df, na.rm = TRUE)

# Scaling constant
cors$max_possible_score <- 5

# Descriptive statistics of data set
cors$mean <- mean(cors$scores) / cors$max_possible_score
print(cors$mean)
cors$sd <- sd(cors$scores) / cors$max_possible_score
print(cors$sd)
cors$range <- range(cors$scores) / cors$max_possible_score
print(cors$range)


############################
# UCLA
############################
ucla <- list()
# Raw data
ucla$df <- read_excel("Study1UCLAdata.xlsx", col_names = FALSE)
# Conversion to numeric data
ucla$df <- as.data.frame(lapply(ucla$df, as.numeric))
# Imputation of row means
ucla$df <- as.data.frame(
  t(apply(ucla$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
ucla$reliability <- psych::alpha(ucla$df)
ucla$alpha <- ucla$reliability$total$raw_alpha
print(ucla$alpha)
# Calculation of participant-level scores
ucla$scores <- rowMeans(ucla$df, na.rm = TRUE)
# Scaling constant
ucla$max_possible_score <- 4
# Descriptive statistics of data set
ucla$mean <- mean(ucla$scores) / ucla$max_possible_score
print(ucla$mean)
ucla$sd <- sd(ucla$scores) / ucla$max_possible_score
print(ucla$sd)
ucla$range <- range(ucla$scores) / ucla$max_possible_score
print(ucla$range)

############################
# RSE
############################
rse <- list()
# Raw data
rse$df <- read_excel("Study1RSEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
rse$df <- as.data.frame(lapply(rse$df, as.numeric))
# Imputation of row means
rse$df <- as.data.frame(
  t(apply(rse$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
rse$reliability <- psych::alpha(rse$df)
rse$alpha <- rse$reliability$total$raw_alpha
print(rse$alpha)
# Calculation of participant-level scores
rse$scores <- rowMeans(rse$df, na.rm = TRUE)
# Scaling constant
rse$max_possible_score <- 4
# Descriptive statistics of data set
rse$mean <- mean(rse$scores) / rse$max_possible_score
print(rse$mean)
rse$sd <- sd(rse$scores) / rse$max_possible_score
print(rse$sd)
rse$range <- range(rse$scores) / rse$max_possible_score
print(rse$range)

############################
# SMUS
############################
smus <- list()
# Raw data
smus$df <- read_excel("Study1SMUSdata.xlsx", col_names = FALSE)
# Conversion to numeric data
smus$df <- as.data.frame(lapply(smus$df, as.numeric))
# Imputation of row means
smus$df <- as.data.frame(
  t(apply(smus$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
smus$reliability <- psych::alpha(smus$df)
smus$alpha <- smus$reliability$total$raw_alpha
print(smus$alpha)
# Calculation of participant-level scores
smus$scores <- rowMeans(smus$df, na.rm = TRUE)
# Scaling constant
smus$max_possible_score <- 9
# Descriptive statistics of data set
smus$mean <- mean(smus$scores) / smus$max_possible_score
print(smus$mean)
smus$sd <- sd(smus$scores) / smus$max_possible_score
print(smus$sd)
smus$range <- range(smus$scores) / smus$max_possible_score
print(smus$range)

############################
# OPEN
############################
open <- list()
# Raw data
open$df <- read_excel("Study1OPENdata.xlsx", col_names = FALSE)
# Conversion to numeric data
open$df <- as.data.frame(lapply(open$df, as.numeric))
# Imputation of row means
open$df <- as.data.frame(
  t(apply(open$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
open$reliability <- psych::alpha(open$df)
open$alpha <- open$reliability$total$raw_alpha
print(open$alpha)
# Calculation of participant-level scores
open$scores <- rowMeans(open$df, na.rm = TRUE)
# Scaling constant
open$max_possible_score <- 5
# Descriptive statistics of data set
open$mean <- mean(open$scores) / open$max_possible_score
print(open$mean)
open$sd <- sd(open$scores) / open$max_possible_score
print(open$sd)
open$range <- range(open$scores) / open$max_possible_score
print(open$range)

############################
# CON
############################
con <- list()
# Raw data
con$df <- read_excel("Study1CONdata.xlsx", col_names = FALSE)
# Conversion to numeric data
con$df <- as.data.frame(lapply(con$df, as.numeric))
# Imputation of row means
con$df <- as.data.frame(
  t(apply(con$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
con$reliability <- psych::alpha(con$df)
con$alpha <- con$reliability$total$raw_alpha
print(con$alpha)
# Calculation of participant-level scores
con$scores <- rowMeans(con$df, na.rm = TRUE)
# Scaling constant
con$max_possible_score <- 5
# Descriptive statistics of data set
con$mean <- mean(con$scores) / con$max_possible_score
print(con$mean)
con$sd <- sd(con$scores) / con$max_possible_score
print(con$sd)
con$range <- range(con$scores) / con$max_possible_score
print(con$range)

############################
# EXTRA
############################
extra <- list()
# Raw data
extra$df <- read_excel("Study1EXTRAdata.xlsx", col_names = FALSE)
# Conversion to numeric data
extra$df <- as.data.frame(lapply(extra$df, as.numeric))
# Imputation of row means
extra$df <- as.data.frame(
  t(apply(extra$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
extra$reliability <- psych::alpha(extra$df)
extra$alpha <- extra$reliability$total$raw_alpha
print(extra$alpha)
# Calculation of participant-level scores
extra$scores <- rowMeans(extra$df, na.rm = TRUE)
# Scaling constant
extra$max_possible_score <- 5
# Descriptive statistics of data set
extra$mean <- mean(extra$scores) / extra$max_possible_score
print(extra$mean)
extra$sd <- sd(extra$scores) / extra$max_possible_score
print(extra$sd)
extra$range <- range(extra$scores) / extra$max_possible_score
print(extra$range)

############################
# AGREE
############################
agree <- list()
# Raw data
agree$df <- read_excel("Study1AGREEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
agree$df <- as.data.frame(lapply(agree$df, as.numeric))
# Imputation of row means
agree$df <- as.data.frame(
  t(apply(agree$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
agree$reliability <- psych::alpha(agree$df)
agree$alpha <- agree$reliability$total$raw_alpha
print(agree$alpha)
# Calculation of participant-level scores
agree$scores <- rowMeans(agree$df, na.rm = TRUE)
# Scaling constant
agree$max_possible_score <- 5
# Descriptive statistics of data set
agree$mean <- mean(agree$scores) / agree$max_possible_score
print(agree$mean)
agree$sd <- sd(agree$scores) / agree$max_possible_score
print(agree$sd)
agree$range <- range(agree$scores) / agree$max_possible_score
print(agree$range)

############################
# NEURO
############################
neuro <- list()
# Raw data
neuro$df <- read_excel("Study1NEUROdata.xlsx", col_names = FALSE)
# Conversion to numeric data
neuro$df <- as.data.frame(lapply(neuro$df, as.numeric))
# Imputation of row means
neuro$df <- as.data.frame(
  t(apply(neuro$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
neuro$reliability <- psych::alpha(neuro$df)
neuro$alpha <- neuro$reliability$total$raw_alpha
print(neuro$alpha)
# Calculation of participant-level scores
neuro$scores <- rowMeans(neuro$df, na.rm = TRUE)
# Scaling constant
neuro$max_possible_score <- 5
# Descriptive statistics of data set
neuro$mean <- mean(neuro$scores) / neuro$max_possible_score
print(neuro$mean)
neuro$sd <- sd(neuro$scores) / neuro$max_possible_score
print(neuro$sd)
neuro$range <- range(neuro$scores) / neuro$max_possible_score
print(neuro$range)

############################
# PANPOSONE
############################
panposone <- list()
# Raw data
panposone$df <- read_excel("Study1PANPOSONEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
panposone$df <- as.data.frame(lapply(panposone$df, as.numeric))
# Imputation of row means
panposone$df <- as.data.frame(
  t(apply(panposone$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
panposone$reliability <- psych::alpha(panposone$df)
panposone$alpha <- panposone$reliability$total$raw_alpha
print(panposone$alpha)
# Calculation of participant-level scores
panposone$scores <- rowMeans(panposone$df, na.rm = TRUE)
# Scaling constant
panposone$max_possible_score <- 5
# Descriptive statistics of data set
panposone$mean <- mean(panposone$scores) / panposone$max_possible_score
print(panposone$mean)
panposone$sd <- sd(panposone$scores) / panposone$max_possible_score
print(panposone$sd)
panposone$range <- range(panposone$scores) / panposone$max_possible_score
print(panposone$range)

############################
# PANNEGONE
############################
pannegone <- list()
# Raw data
pannegone$df <- read_excel("Study1PANNEGONEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
pannegone$df <- as.data.frame(lapply(pannegone$df, as.numeric))
# Imputation of row means
pannegone$df <- as.data.frame(
  t(apply(pannegone$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
pannegone$reliability <- psych::alpha(pannegone$df)
pannegone$alpha <- pannegone$reliability$total$raw_alpha
print(pannegone$alpha)
# Calculation of participant-level scores
pannegone$scores <- rowMeans(pannegone$df, na.rm = TRUE)
# Scaling constant
pannegone$max_possible_score <- 5
# Descriptive statistics of data set
pannegone$mean <- mean(pannegone$scores) / pannegone$max_possible_score
print(pannegone$mean)
pannegone$sd <- sd(pannegone$scores) / pannegone$max_possible_score
print(pannegone$sd)
pannegone$range <- range(pannegone$scores) / pannegone$max_possible_score
print(pannegone$range)

############################
# PANPOSTWO
############################
panpostwo <- list()
# Raw data
panpostwo$df <- read_excel("Study1PANPOSTWOdata.xlsx", col_names = FALSE)
# Conversion to numeric data
panpostwo$df <- as.data.frame(lapply(panpostwo$df, as.numeric))
# Imputation of row means
panpostwo$df <- as.data.frame(
  t(apply(panpostwo$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
panpostwo$reliability <- psych::alpha(panpostwo$df)
panpostwo$alpha <- panpostwo$reliability$total$raw_alpha
print(panpostwo$alpha)
# Calculation of participant-level scores
panpostwo$scores <- rowMeans(panpostwo$df, na.rm = TRUE)
# Scaling constant
panpostwo$max_possible_score <- 5
# Descriptive statistics of data set
panpostwo$mean <- mean(panpostwo$scores) / panpostwo$max_possible_score
print(panpostwo$mean)
panpostwo$sd <- sd(panpostwo$scores) / panpostwo$max_possible_score
print(panpostwo$sd)
panpostwo$range <- range(panpostwo$scores) / panpostwo$max_possible_score
print(panpostwo$range)

############################
# PANNEGTWO
############################
pannegtwo <- list()
# Raw data
pannegtwo$df <- read_excel("Study1PANNEGTWOdata.xlsx", col_names = FALSE)
# Conversion to numeric data
pannegtwo$df <- as.data.frame(lapply(pannegtwo$df, as.numeric))
# Imputation of row means
pannegtwo$df <- as.data.frame(
  t(apply(pannegtwo$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
pannegtwo$reliability <- psych::alpha(pannegtwo$df)
pannegtwo$alpha <- pannegtwo$reliability$total$raw_alpha
print(pannegtwo$alpha)
# Calculation of participant-level scores
pannegtwo$scores <- rowMeans(pannegtwo$df, na.rm = TRUE)
# Scaling constant
pannegtwo$max_possible_score <- 5
# Descriptive statistics of data set
pannegtwo$mean <- mean(pannegtwo$scores) / pannegtwo$max_possible_score
print(pannegtwo$mean)
pannegtwo$sd <- sd(pannegtwo$scores) / pannegtwo$max_possible_score
print(pannegtwo$sd)
pannegtwo$range <- range(pannegtwo$scores) / pannegtwo$max_possible_score
print(pannegtwo$range)

############################
# PANPOSTHREE
############################
panposthree <- list()
# Raw data
panposthree$df <- read_excel("Study1PANPOSTHREEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
panposthree$df <- as.data.frame(lapply(panposthree$df, as.numeric))
# Imputation of row means
panposthree$df <- as.data.frame(
  t(apply(panposthree$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
panposthree$reliability <- psych::alpha(panposthree$df)
panposthree$alpha <- panposthree$reliability$total$raw_alpha
print(panposthree$alpha)
# Calculation of participant-level scores
panposthree$scores <- rowMeans(panposthree$df, na.rm = TRUE)
# Scaling constant
panposthree$max_possible_score <- 5
# Descriptive statistics of data set
panposthree$mean <- mean(panposthree$scores) / panposthree$max_possible_score
print(panposthree$mean)
panposthree$sd <- sd(panposthree$scores) / panposthree$max_possible_score
print(panposthree$sd)
panposthree$range <- range(panposthree$scores) / panposthree$max_possible_score
print(panposthree$range)

############################
# PANNEGTHREE
############################
pannegthree <- list()
# Raw data
pannegthree$df <- read_excel("Study1PANNEGTHREEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
pannegthree$df <- as.data.frame(lapply(pannegthree$df, as.numeric))
# Imputation of row means
pannegthree$df <- as.data.frame(
  t(apply(pannegthree$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
pannegthree$reliability <- psych::alpha(pannegthree$df)
pannegthree$alpha <- pannegthree$reliability$total$raw_alpha
print(pannegthree$alpha)
# Calculation of participant-level scores
pannegthree$scores <- rowMeans(pannegthree$df, na.rm = TRUE)
# Scaling constant
pannegthree$max_possible_score <- 5
# Descriptive statistics of data set
pannegthree$mean <- mean(pannegthree$scores) / pannegthree$max_possible_score
print(pannegthree$mean)
pannegthree$sd <- sd(pannegthree$scores) / pannegthree$max_possible_score
print(pannegthree$sd)
pannegthree$range <- range(pannegthree$scores) / pannegthree$max_possible_score
print(pannegthree$range)

############################
# QD
############################
qd <- list()
# Raw data
qd$df <- read_excel("Study1QDdata.xlsx", col_names = FALSE)
# Conversion to numeric data
qd$df <- as.data.frame(lapply(qd$df, as.numeric))
# Calculation of participant-level scores
qd$scores <- qd$df[, 1]
# Descriptive statistics of data set
qd$mean <- mean(qd$scores)
print(qd$mean)
qd$sd <- sd(qd$scores)
print(qd$sd)
qd$range <- range(qd$scores)
print(qd$range)

############################
# MAC
############################
mac <- list()
# Raw data
mac$df <- read_excel("Study1MACdata.xlsx", col_names = FALSE)
# Conversion to numeric data
mac$df <- as.data.frame(lapply(mac$df, as.numeric))
# Calculation of participant-level scores
mac$scores <- mac$df[, 1]
# Descriptive statistics of data set
mac$mean <- mean(mac$scores,na.rm = TRUE)
print(mac$mean)
mac$sd <- sd(mac$scores,na.rm = TRUE)
print(mac$sd)
mac$range <- range(mac$scores,na.rm = TRUE)
print(mac$range)

############################
# AGE
############################
age <- list()
# Raw data
age$df <- read_excel("Study1AGEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
age$df <- as.data.frame(lapply(age$df, as.numeric))
# Calculation of participant-level scores
age$scores <- age$df[, 1]
# Descriptive statistics of data set
age$mean <- mean(age$scores,na.rm = TRUE)
print(age$mean)
age$sd <- sd(age$scores,na.rm = TRUE)
print(age$sd)
age$range <- range(age$scores,na.rm = TRUE)
print(age$range)

############################
# FULL MODEL REGRESSION
############################
fullmodel <- list()
# Full model regression with 18 variables
fullmodel$result <- lm(ros$scores ~ cors$scores + qd$scores + panposone$scores +
                         pannegone$scores + panpostwo$scores + pannegtwo$scores +
                         panposthree$scores + pannegthree$scores + mac$scores +
                         smus$scores + ucla$scores + rse$scores + extra$scores +
                         agree$scores + con$scores + neuro$scores + open$scores +
                         age$scores, na.action = na.omit)
print(summary(fullmodel$result))

############################
# STEPWISE REGRESSION
############################
stepwise <- list()
# Combining all variables into one data frame
stepwise$df <- na.omit(data.frame(
  ros = ros$scores,
  cors = cors$scores,
  qd = qd$scores,
  panposone = panposone$scores,
  pannegone = pannegone$scores,
  panpostwo = panpostwo$scores,
  pannegtwo = pannegtwo$scores,
  panposthree = panposthree$scores,
  pannegthree = pannegthree$scores,
  mac = mac$scores,
  smus = smus$scores,
  ucla = ucla$scores,
  rse = rse$scores,
  extra = extra$scores,
  agree = agree$scores,
  con = con$scores,
  neuro = neuro$scores,
  open = open$scores,
  age = age$scores
))
# Forward stepwise regression with 18 variables
stepwise$null <- lm(ros ~ 1, data = stepwise$df)
stepwise$result <- step(stepwise$null, scope = list(lower = ~ 1, upper = ~ cors + qd + panposone +
                                                      pannegone + panpostwo + pannegtwo +
                                                      panposthree + pannegthree + mac +
                                                      smus + ucla + rse + extra +
                                                      agree + con + neuro + open +
                                                      age), direction = "forward")
print(summary(stepwise$result))




############################
# Failed ANOVA
############################
rosanova <- list()
# Raw data
rosanova$df <- read_excel("Study1IVdata.xlsx", col_names = FALSE)
# Conversion to numeric data
rosanova$df <- as.data.frame(lapply(rosanova$df, as.numeric))
# One-way ANOVA
rosanova$result <- aov(rosanova$df[, 2] ~ as.factor(rosanova$df[, 1]), data = rosanova$df)
print(summary(rosanova$result))

############################
# PANAS Repeated Measures ANOVA
############################
panan <- list()
# Raw data
panan$df <- read_excel("Study1PANANdata.xlsx", col_names = FALSE)
# Conversion to numeric data
panan$df <- as.data.frame(lapply(panan$df, as.numeric))
# Naming columns
colnames(panan$df) <- c("id", "condition", "panpos1", "panneg1", "panpos2", "panneg2", "panpos3", "panneg3")
# Converting to long format for repeated measures
panan$long_pos <- reshape(panan$df, varying = c("panpos1", "panpos2", "panpos3"),
                          v.names = "score", timevar = "time", times = c(1, 2, 3),
                          idvar = "id", direction = "long")
panan$long_neg <- reshape(panan$df, varying = c("panneg1", "panneg2", "panneg3"),
                          v.names = "score", timevar = "time", times = c(1, 2, 3),
                          idvar = "id", direction = "long")
# Converting condition and time to factors
panan$long_pos$condition <- as.factor(panan$long_pos$condition)
panan$long_pos$time <- as.factor(panan$long_pos$time)
panan$long_neg$condition <- as.factor(panan$long_neg$condition)
panan$long_neg$time <- as.factor(panan$long_neg$time)
# Repeated measures ANOVA for PANAS positive scores
panan$result_pos <- ezANOVA(data = panan$long_pos, dv = score, wid = id,
                            within = time, between = condition, type = 3)
print(panan$result_pos)
# Descriptive statistics for PANAS positive scores by time
panan$pos_means <- tapply(panan$long_pos$score, panan$long_pos$time, mean, na.rm = TRUE)
panan$pos_sds <- tapply(panan$long_pos$score, panan$long_pos$time, sd, na.rm = TRUE)
print(panan$pos_means)
print(panan$pos_sds)
# Repeated measures ANOVA for PANAS negative scores
panan$result_neg <- ezANOVA(data = panan$long_neg, dv = score, wid = id,
                            within = time, between = condition, type = 3)
print(panan$result_neg)
# Descriptive statistics for PANAS negative scores by time
panan$neg_means <- tapply(panan$long_neg$score, panan$long_neg$time, mean, na.rm = TRUE)
panan$neg_sds <- tapply(panan$long_neg$score, panan$long_neg$time, sd, na.rm = TRUE)
print(panan$neg_means)
print(panan$neg_sds)

############################
# ZERO-ORDER CORRELATIONS - STUDY 1
############################
zeroorder_study1 <- list()
zeroorder_study1$result <- cor(data.frame(
  ros = ros$scores,
  ucla = ucla$scores / ucla$max_possible_score,
  rse = rse$scores / rse$max_possible_score,
  mac = mac$scores,
  cors = cors$scores / cors$max_possible_score,
  smus = smus$scores / smus$max_possible_score,
  open = open$scores / open$max_possible_score,
  con = con$scores / con$max_possible_score,
  extra = extra$scores / extra$max_possible_score,
  agree = agree$scores / agree$max_possible_score,
  neuro = neuro$scores / neuro$max_possible_score,
  age = age$scores,
  panpos1 = panposone$scores / panposone$max_possible_score,
  panneg1 = pannegone$scores / pannegone$max_possible_score,
  panpos2 = panpostwo$scores / panpostwo$max_possible_score,
  panneg2 = pannegtwo$scores / pannegtwo$max_possible_score,
  panpos3 = panposthree$scores / panposthree$max_possible_score,
  panneg3 = pannegthree$scores / pannegthree$max_possible_score,
  qd = qd$scores
), use = "complete.obs")
print(round(zeroorder_study1$result, 2))

colnames(zeroorder_study1$result) <- c("ROS", "UCLA Loneliness", "Self-Esteem",
                                       "MacArthur SSS", "Religiosity", "Social Media",
                                       "Openness", "Conscientiousness", "Extraversion",
                                       "Agreeableness", "Neuroticism", "Age",
                                       "PANAS Pos 1", "PANAS Neg 1",
                                       "PANAS Pos 2", "PANAS Neg 2",
                                       "PANAS Pos 3", "PANAS Neg 3",
                                       "Q Difficulty")
rownames(zeroorder_study1$result) <- colnames(zeroorder_study1$result)

png("study1_correlation_matrix.png", width = 2400, height = 2400, res = 150)

par(mar = c(2, 2, 4, 2))

corrplot(zeroorder_study1$result,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 1.0,
         tl.cex = 1.1,
         col = colorRampPalette(c("steelblue", "white", "firebrick"))(200),
         title = "Zero-Order Correlation Matrix - Study 1",
         mar = c(0, 0, 2, 0))

dev.off()

############################
# SECONDROS
############################
secondros <- list()
# Raw data
secondros$df <- read_excel("Study2SECONDROSdata.xlsx", col_names = FALSE)
# Conversion to numeric data
secondros$df <- as.data.frame(lapply(secondros$df, as.numeric))
# Recoding to 0s and 1s (1 = relationship choice, 0 = status choice)
odd_questions <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21)
even_questions <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)
secondros$df[, odd_questions] <- ifelse(secondros$df[, odd_questions] == 2, 1, 0)
secondros$df[, even_questions] <- ifelse(secondros$df[, even_questions] == 1, 1, 0)
# Quick check
print(head(secondros$df))
# Imputation of row means
secondros$df <- as.data.frame(
  t(apply(secondros$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
secondros$reliability <- psych::alpha(secondros$df)
secondros$alpha <- secondros$reliability$total$raw_alpha
print(secondros$alpha)
# Calculation of participant-level scores
secondros$scores <- rowMeans(secondros$df, na.rm = TRUE)
# Descriptive statistics of data set
secondros$mean <- mean(secondros$scores)
print(secondros$mean)
secondros$sd <- sd(secondros$scores)
print(secondros$sd)
secondros$range <- range(secondros$scores)
print(secondros$range)
# One-sample t test
secondros$ttest <- t.test(secondros$scores, mu = 0.5)
print(secondros$ttest)
# Cohen's d
secondros$cohens_d <- (secondros$mean - 0.5) / secondros$sd
print(secondros$cohens_d)

############################
# FS
############################
fs <- list()
# Raw data
fs$df <- read_excel("study2FSdata.xlsx", col_names = FALSE)
# Conversion to numeric data
fs$df <- as.data.frame(lapply(fs$df, as.numeric))
# Quick check
print(head(fs$df))
# Imputation of row means
fs$df <- as.data.frame(
  t(apply(fs$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
fs$reliability <- psych::alpha(fs$df)
fs$alpha <- fs$reliability$total$raw_alpha
print(fs$alpha)
# Calculation of participant-level scores
fs$scores <- rowMeans(fs$df, na.rm = TRUE)
# Scaling constant
fs$max_possible_score <- 7
# Descriptive statistics of data set
fs$mean <- mean(fs$scores) / fs$max_possible_score
print(fs$mean)
fs$sd <- sd(fs$scores) / fs$max_possible_score
print(fs$sd)
fs$range <- range(fs$scores) / fs$max_possible_score
print(fs$range)

############################
# INCOS
############################
incos <- list()
# Raw data
incos$df <- read_excel("study2INCOSdata.xlsx", col_names = FALSE)
# Conversion to numeric data
incos$df <- as.data.frame(lapply(incos$df, as.numeric))
# Correcting Qualtrics quirk (0s converted to 1s)
incos$df[incos$df == 0] <- 1
# Quick check
print(head(incos$df))
# Imputation of row means
incos$df <- as.data.frame(
  t(apply(incos$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
incos$reliability <- psych::alpha(incos$df)
incos$alpha <- incos$reliability$total$raw_alpha
print(incos$alpha)
# Calculation of participant-level scores
incos$scores <- rowMeans(incos$df, na.rm = TRUE)
# Scaling constant
incos$max_possible_score <- 5
# Descriptive statistics of data set
incos$mean <- mean(incos$scores) / incos$max_possible_score
print(incos$mean)
incos$sd <- sd(incos$scores) / incos$max_possible_score
print(incos$sd)
incos$range <- range(incos$scores) / incos$max_possible_score
print(incos$range)

############################
# SECONDSMUS
############################
secondsmus <- list()
# Raw data
secondsmus$df <- read_excel("Study2SECONDSMUSdata.xlsx", col_names = FALSE)
# Conversion to numeric data
secondsmus$df <- as.data.frame(lapply(secondsmus$df, as.numeric))
# Correcting Qualtrics quirk (0s and NAs converted to 1s)
secondsmus$df[secondsmus$df == 0 | is.na(secondsmus$df)] <- 1
# Imputation of row means
secondsmus$df <- as.data.frame(
  t(apply(secondsmus$df, 1, function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  }))
)
# Calculation of reliability
secondsmus$reliability <- psych::alpha(secondsmus$df)
secondsmus$alpha <- secondsmus$reliability$total$raw_alpha
print(secondsmus$alpha)
# Calculation of participant-level scores
secondsmus$scores <- rowMeans(secondsmus$df, na.rm = TRUE)
# Scaling constant
secondsmus$max_possible_score <- 9
# Descriptive statistics of data set
secondsmus$mean <- mean(secondsmus$scores) / secondsmus$max_possible_score
print(secondsmus$mean)
secondsmus$sd <- sd(secondsmus$scores) / secondsmus$max_possible_score
print(secondsmus$sd)
secondsmus$range <- range(secondsmus$scores) / secondsmus$max_possible_score
print(secondsmus$range)


############################
# FULLIPIP
############################
fullipip <- list()
# Raw data
fullipip$df <- read_excel("Study2FULLIPIPdata.xlsx", col_names = FALSE)
# Conversion to numeric data via likert scale
fullipip$df <- as.data.frame(lapply(fullipip$df, as.numeric))
# Reverse scoring
reverse_items <- c(
  9, 19, 24, 30,
  39, 40, 48, 49, 51, 53, 54, 60,
  62, 67, 68, 69, 70, 73, 74, 75, 78, 79, 80, 81, 83, 84, 85,
  88, 89, 90, 92, 94, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
  106, 107, 108, 109, 110, 111, 113, 114, 115, 116, 118, 119, 120
)
fullipip$df[, reverse_items] <- 6 - fullipip$df[, reverse_items]
# Quick check
print(head(fullipip$df))
# Item groupings by personality trait
neuroticism_items <- c(
  1,31,61,91,
  6,36,66,96,
  11,41,71,101,
  16,46,76,106,
  21,51,81,111,
  26,56,86,116
)
extraversion_items <- c(
  2,32,62,92,
  7,37,67,97,
  12,42,72,102,
  17,47,77,107,
  22,52,82,112,
  27,57,87,117
)
openness_items <- c(
  3,33,63,93,
  8,38,68,98,
  13,43,73,103,
  18,48,78,108,
  23,53,83,113,
  28,58,88,118
)
agreeableness_items <- c(
  4,34,64,94,
  9,39,69,99,
  14,44,74,104,
  19,49,79,109,
  24,54,84,114,
  29,59,89,119
)
conscientiousness_items <- c(
  5,35,65,95,
  10,40,70,100,
  15,45,75,105,
  20,50,80,110,
  25,55,85,115,
  30,60,90,120
)
# Subsetting by personality trait
fullipip$neuro <- fullipip$df[, neuroticism_items]
fullipip$extra <- fullipip$df[, extraversion_items]
fullipip$open <- fullipip$df[, openness_items]
fullipip$agree <- fullipip$df[, agreeableness_items]
fullipip$con <- fullipip$df[, conscientiousness_items]
# Scaling constant
fullipip$max_possible_score <- 5
# Neuroticism
fullipip$neuro_reliability <- psych::alpha(fullipip$neuro)
fullipip$neuro_alpha <- fullipip$neuro_reliability$total$raw_alpha
print(fullipip$neuro_alpha)
fullipip$neuro_scores <- rowMeans(fullipip$neuro, na.rm = TRUE)
fullipip$neuro_mean <- mean(fullipip$neuro_scores) / fullipip$max_possible_score
print(fullipip$neuro_mean)
fullipip$neuro_sd <- sd(fullipip$neuro_scores) / fullipip$max_possible_score
print(fullipip$neuro_sd)
fullipip$neuro_range <- range(fullipip$neuro_scores) / fullipip$max_possible_score
print(fullipip$neuro_range)
# Extraversion
fullipip$extra_reliability <- psych::alpha(fullipip$extra)
fullipip$extra_alpha <- fullipip$extra_reliability$total$raw_alpha
print(fullipip$extra_alpha)
fullipip$extra_scores <- rowMeans(fullipip$extra, na.rm = TRUE)
fullipip$extra_mean <- mean(fullipip$extra_scores) / fullipip$max_possible_score
print(fullipip$extra_mean)
fullipip$extra_sd <- sd(fullipip$extra_scores) / fullipip$max_possible_score
print(fullipip$extra_sd)
fullipip$extra_range <- range(fullipip$extra_scores) / fullipip$max_possible_score
print(fullipip$extra_range)
# Openness
fullipip$open_reliability <- psych::alpha(fullipip$open)
fullipip$open_alpha <- fullipip$open_reliability$total$raw_alpha
print(fullipip$open_alpha)
fullipip$open_scores <- rowMeans(fullipip$open, na.rm = TRUE)
fullipip$open_mean <- mean(fullipip$open_scores) / fullipip$max_possible_score
print(fullipip$open_mean)
fullipip$open_sd <- sd(fullipip$open_scores) / fullipip$max_possible_score
print(fullipip$open_sd)
fullipip$open_range <- range(fullipip$open_scores) / fullipip$max_possible_score
print(fullipip$open_range)
# Agreeableness
fullipip$agree_reliability <- psych::alpha(fullipip$agree)
fullipip$agree_alpha <- fullipip$agree_reliability$total$raw_alpha
print(fullipip$agree_alpha)
fullipip$agree_scores <- rowMeans(fullipip$agree, na.rm = TRUE)
fullipip$agree_mean <- mean(fullipip$agree_scores) / fullipip$max_possible_score
print(fullipip$agree_mean)
fullipip$agree_sd <- sd(fullipip$agree_scores) / fullipip$max_possible_score
print(fullipip$agree_sd)
fullipip$agree_range <- range(fullipip$agree_scores) / fullipip$max_possible_score
print(fullipip$agree_range)
# Conscientiousness
fullipip$con_reliability <- psych::alpha(fullipip$con)
fullipip$con_alpha <- fullipip$con_reliability$total$raw_alpha
print(fullipip$con_alpha)
fullipip$con_scores <- rowMeans(fullipip$con, na.rm = TRUE)
fullipip$con_mean <- mean(fullipip$con_scores) / fullipip$max_possible_score
print(fullipip$con_mean)
fullipip$con_sd <- sd(fullipip$con_scores) / fullipip$max_possible_score
print(fullipip$con_sd)
fullipip$con_range <- range(fullipip$con_scores) / fullipip$max_possible_score
print(fullipip$con_range)

############################
# NEWESTSELF
############################
newestself <- list()
# Raw data
newestself$df <- read_excel("Study2NEWESTSELFdata.xlsx", col_names = FALSE)
# Conversion to numeric data
newestself$df <- as.data.frame(lapply(newestself$df, as.numeric))
# Calculation of participant-level scores
newestself$scores <- newestself$df[, 1]
# Scaling constant
newestself$max_possible_score <- 100
# Descriptive statistics of data set
newestself$mean <- mean(newestself$scores, na.rm = TRUE) / newestself$max_possible_score
print(newestself$mean)
newestself$sd <- sd(newestself$scores, na.rm = TRUE) / newestself$max_possible_score
print(newestself$sd)
newestself$range <- range(newestself$scores, na.rm = TRUE) / newestself$max_possible_score
print(newestself$range)

############################
# NEWESTOTHERS
############################
newestothers <- list()
# Raw data
newestothers$df <- read_excel("Study2NEWESTOTHERSdata.xlsx", col_names = FALSE)
# Conversion to numeric data
newestothers$df <- as.data.frame(lapply(newestothers$df, as.numeric))
# Calculation of participant-level scores
newestothers$scores <- newestothers$df[, 1]
# Scaling constant
newestothers$max_possible_score <- 100
# Descriptive statistics of data set
newestothers$mean <- mean(newestothers$scores, na.rm = TRUE) / newestothers$max_possible_score
print(newestothers$mean)
newestothers$sd <- sd(newestothers$scores, na.rm = TRUE) / newestothers$max_possible_score
print(newestothers$sd)
newestothers$range <- range(newestothers$scores, na.rm = TRUE) / newestothers$max_possible_score
print(newestothers$range)

############################
# ACTIONINFLUENCE
############################
actioninfluence <- list()
# Raw data (each response option on its own row)
actioninfluence$df <- read_excel("Study2ACTIONINFLUENCEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
actioninfluence$df <- as.data.frame(lapply(actioninfluence$df, as.numeric))
# Count occurrences of each response option
actioninfluence$counts <- table(actioninfluence$df[, 1])
print(actioninfluence$counts)

############################
# ACTIONREFLECTION
############################
actionreflection <- list()
# Raw data
actionreflection$df <- read_excel("Study2ACTIONREFLECTIONdata.xlsx", col_names = FALSE)
# Conversion to numeric data via likert scale
actionreflection$df <- as.data.frame(lapply(actionreflection$df, as.numeric))
# Calculation of participant-level scores
actionreflection$scores <- actionreflection$df[, 1]
# Scaling constant
actionreflection$max_possible_score <- 6
# Descriptive statistics of data set
actionreflection$mean <- mean(actionreflection$scores, na.rm = TRUE) / actionreflection$max_possible_score
print(actionreflection$mean)
actionreflection$sd <- sd(actionreflection$scores, na.rm = TRUE) / actionreflection$max_possible_score
print(actionreflection$sd)
actionreflection$range <- range(actionreflection$scores, na.rm = TRUE) / actionreflection$max_possible_score
print(actionreflection$range)


############################
# SECONDAGE
############################
secondage <- list()
# Raw data
secondage$df <- read_excel("Study2SECONDAGEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
secondage$df <- as.data.frame(lapply(secondage$df, as.numeric))
# Calculation of participant-level scores
secondage$scores <- secondage$df[, 1]
# Descriptive statistics of data set
secondage$mean <- mean(secondage$scores, na.rm = TRUE)
print(secondage$mean)
secondage$sd <- sd(secondage$scores, na.rm = TRUE)
print(secondage$sd)
secondage$range <- range(secondage$scores, na.rm = TRUE)
print(secondage$range)

############################
# SECOND FULL MODEL REGRESSION
############################
secondfullmodel <- list()
# Note: This is a draft. actioninfluence was excluded due to its multiple response nature
# Full model regression
secondfullmodel$result <- lm(secondros$scores ~ fs$scores + incos$scores +
                               secondsmus$scores + fullipip$neuro_scores +
                               fullipip$extra_scores + fullipip$open_scores +
                               fullipip$agree_scores + fullipip$con_scores+secondage$scores,
                             na.action = na.omit)
print(summary(secondfullmodel$result))

# Full model regression table
fullmodel_table <- data.frame(
  Predictor = c("Intercept", "Flourishing Scale", "INCOS", "Social Media Use",
                "Neuroticism", "Extraversion", "Openness", "Agreeableness",
                "Conscientiousness", "Age"),
  B = round(coef(summary(secondfullmodel$result))[, 1], 3),
  SE = round(coef(summary(secondfullmodel$result))[, 2], 3),
  t = round(coef(summary(secondfullmodel$result))[, 3], 3),
  p = round(coef(summary(secondfullmodel$result))[, 4], 3)
)
print(fullmodel_table, row.names = FALSE)

install.packages("QuantPsyc")
library(QuantPsyc)

# Get standardized betas
betas <- lm.beta(secondfullmodel$result)

# Build the table
fullmodel_table <- data.frame(
  Predictor = c("Intercept", "Flourishing Scale", "INCOS", "Social Media Use",
                "Neuroticism", "Extraversion", "Openness", "Agreeableness",
                "Conscientiousness", "Age"),
  B = round(coef(summary(secondfullmodel$result))[, 1], 2),
  SE = round(coef(summary(secondfullmodel$result))[, 2], 2),
  Beta = round(c(NA, betas), 2),  # NA for intercept, which has no beta
  t = round(coef(summary(secondfullmodel$result))[, 3], 2),
  p = round(coef(summary(secondfullmodel$result))[, 4], 3)
)

print(fullmodel_table, row.names = FALSE)


############################
# FLOURISHING SCALE: FULL MODEL REGRESSION
############################
fsmodel <- list()
# Note: flourishing scale as DV
# Full model regression
fsmodel$result <- lm(fs$scores ~ secondros$scores + incos$scores +
                       secondsmus$scores + fullipip$neuro_scores +
                       fullipip$extra_scores + fullipip$open_scores +
                       fullipip$agree_scores + fullipip$con_scores +
                       secondage$scores,
                     na.action = na.omit)
print(summary(fsmodel$result))

# Flourishing scale model regression table
fsmodel_table <- data.frame(
  Predictor = c("Intercept", "ROS", "INCOS", "Social Media Use",
                "Neuroticism", "Extraversion", "Openness", "Agreeableness",
                "Conscientiousness", "Age"),
  B = round(coef(summary(fsmodel$result))[, 1], 3),
  SE = round(coef(summary(fsmodel$result))[, 2], 3),
  t = round(coef(summary(fsmodel$result))[, 3], 3),
  p = round(coef(summary(fsmodel$result))[, 4], 3)
)

print(fsmodel_table, row.names = FALSE)

betas_fs <- lm.beta(fsmodel$result)

fsmodel_table <- data.frame(
  Predictor = c("Intercept", "ROS", "INCOS", "Social Media Use",
                "Neuroticism", "Extraversion", "Openness", "Agreeableness",
                "Conscientiousness", "Age"),
  B = round(coef(summary(fsmodel$result))[, 1], 2),
  SE = round(coef(summary(fsmodel$result))[, 2], 2),
  Beta = round(c(NA, betas_fs), 2),
  t = round(coef(summary(fsmodel$result))[, 3], 2),
  p = round(coef(summary(fsmodel$result))[, 4], 3)
)

print(fsmodel_table, row.names = FALSE)


############################
# Gender Counts
############################

gender <- list()
# Raw data
gender$df <- read_excel("Study2GENDERdata.xlsx", col_names = FALSE)
# Conversion to numeric data
gender$df <- as.data.frame(lapply(gender$df, as.numeric))
# Frequency count of each gender category
gender$counts <- table(gender$df[, 1])
print(gender$counts)


############################
# ESTIMATION CORRELATIONS (WITH MISSING DATA)
############################
estcor_missing <- list()
# Correlation between newestself and newestothers
estcor_missing$selfothers <- cor.test(as.numeric(newestself$df[, 1]), as.numeric(newestothers$df[, 1]), use = "complete.obs")
print(estcor_missing$selfothers)
# Correlation between newestself and secondros
estcor_missing$selfros <- cor.test(as.numeric(newestself$df[, 1]), secondros$scores, use = "complete.obs")
print(estcor_missing$selfros)
# Correlation between newestothers and secondros
estcor_missing$othersros <- cor.test(as.numeric(newestothers$df[, 1]), secondros$scores, use = "complete.obs")
print(estcor_missing$othersros)

############################
# ACCURACY SCORES
############################
# Difference between estimated and actual ROS scores
newestself$accuracy <- newestself$scores - secondros$scores * 100
newestothers$accuracy <- newestothers$scores - secondros$scores * 100
# Descriptive statistics for self accuracy
newestself$accuracy_mean <- mean(newestself$accuracy, na.rm = TRUE)
print(newestself$accuracy_mean)
newestself$accuracy_sd <- sd(newestself$accuracy, na.rm = TRUE)
print(newestself$accuracy_sd)
newestself$accuracy_range <- range(newestself$accuracy, na.rm = TRUE)
print(newestself$accuracy_range)
# Descriptive statistics for others accuracy
newestothers$accuracy_mean <- mean(newestothers$accuracy, na.rm = TRUE)
print(newestothers$accuracy_mean)
newestothers$accuracy_sd <- sd(newestothers$accuracy, na.rm = TRUE)
print(newestothers$accuracy_sd)
newestothers$accuracy_range <- range(newestothers$accuracy, na.rm = TRUE)
print(newestothers$accuracy_range)

############################
# ONE-SAMPLE T-TESTS - ACCURACY SCORES
############################
# Self-estimation accuracy vs. 0
newestself$accuracy_ttest <- t.test(newestself$accuracy, mu = 0)
print(newestself$accuracy_ttest)
# Other-estimation accuracy vs. 0
newestothers$accuracy_ttest <- t.test(newestothers$accuracy, mu = 0)
print(newestothers$accuracy_ttest)

############################
# NORMALITY TESTS - ACCURACY SCORES
############################
# Shapiro-Wilk test for self-estimation accuracy
accuracynorm <- list()
accuracynorm$self <- shapiro.test(newestself$accuracy)
print(accuracynorm$self)
# Shapiro-Wilk test for other-estimation accuracy
accuracynorm$others <- shapiro.test(newestothers$accuracy)
print(accuracynorm$others)


############################
# SCATTERPLOT - ACTUAL ROS VS SELF ESTIMATE
############################
plot(secondros$scores * 100, newestself$scores,
     xlab = "Actual ROS Score",
     ylab = "Self Estimate",
     main = "Actual ROS Score vs. Self Estimate",
     pch = 16,
     col = "steelblue")
abline(lm(newestself$scores ~ I(secondros$scores * 100)), col = "red")

############################
# SCATTERPLOT - ACTUAL ROS VS OTHER ESTIMATION ACCURACY
############################
plot(secondros$scores * 100, newestothers$accuracy,
     xlab = "Actual ROS Score",
     ylab = "Other Estimation Accuracy",
     main = "Actual ROS Score vs. Other Estimation Accuracy",
     pch = 16,
     col = "steelblue")
abline(lm(newestothers$accuracy ~ I(secondros$scores * 100)), col = "red")
abline(h = 0, col = "gray", lty = 2)

############################
# SCATTERPLOT - ACTUAL ROS VS SELF ESTIMATION ACCURACY
############################
plot(secondros$scores * 100, newestself$accuracy,
     xlab = "Actual ROS Score",
     ylab = "Self Estimation Accuracy",
     main = "Actual ROS Score vs. Self Estimation Accuracy",
     pch = 16,
     col = "steelblue")
abline(lm(newestself$accuracy ~ I(secondros$scores * 100)), col = "red")
abline(h = 0, col = "gray", lty = 2)

############################
# ACTIONREFLECTION BAR CHART
############################
# Count frequency of each response (1-6)

actionreflection$counts <- table(factor(actionreflection$scores, levels = 1:6))

# Labels for x-axis
response_labels <- c("Never", "Rarely", "Occasionally", "Often", "Very Often", "Always")

# Bar chart
barplot(actionreflection$counts,
        names.arg = response_labels,
        main = "Perceived Alignment Between Past Actions\nand Indicated Relationship Orientation",
        xlab = "Self-Described Frequency",
        ylab = "Number of Participants",
        col = "steelblue",
        ylim = c(0, max(actionreflection$counts) + 5))

# Add count labels on top of each bar
text(x = barplot(actionreflection$counts, plot = FALSE),
     y = actionreflection$counts + 1,
     labels = as.numeric(actionreflection$counts),
     cex = 0.9)

############################
# ACTIONINFLUENCE BAR CHART
############################
actioninfluence <- list()
# Raw data (each response option on its own row)
actioninfluence$df <- read_excel("Study2ACTIONINFLUENCEdata.xlsx", col_names = FALSE)
# Conversion to numeric data
actioninfluence$df <- as.data.frame(lapply(actioninfluence$df, as.numeric))
# Count occurrences of each response option
actioninfluence$counts <- table(factor(actioninfluence$df[, 1], levels = 1:7))
print(actioninfluence$counts)

# Labels for x-axis
influence_labels <- c("Economic\nPressure",
                      "Social\nPressure",
                      "Cultural or\nReligious Pressure",
                      "Family\nPressure",
                      "Anxiety or\nIndecision",
                      "Always on\nPreferences",
                      "Not\nSure")

# Margins and axis label placement
par(mar = c(10, 6, 4, 2), mgp = c(3, 1, 0))

# Bar chart
bp <- barplot(actioninfluence$counts,
              names.arg = influence_labels,
              main = "Perceived Reasons for Disparities Between Past Actions\nand Indicated Relationship Orientation",
              xlab = "",
              ylab = "Number of Participants",
              col = "steelblue",
              ylim = c(0, max(actioninfluence$counts) + 5),
              cex.names = 0.7,
              las = 2)

# Manual x-axis label
mtext("Self-Described Reason", side = 1, line = 8)

# Add count labels on top of each bar
text(x = bp,
     y = actioninfluence$counts + 2,
     labels = as.numeric(actioninfluence$counts),
     cex = 0.9)

############################
# SOCIAL MEDIA SITES BAR CHART
############################
socialmedia <- list()
# Raw data
socialmedia$df <- read_excel("Study2SITESSOCIALMEDIAdata.xlsx", col_names = FALSE)
# Conversion to numeric data
socialmedia$df <- as.data.frame(lapply(socialmedia$df, as.numeric))
# Count occurrences of each response option across all columns
socialmedia$counts <- table(factor(unlist(socialmedia$df), levels = 1:9))
print(socialmedia$counts)

# Labels for x-axis
sites_labels <- c("Bluesky", "Discord", "Facebook", "Instagram",
                  "Snapchat", "TikTok", "X", "Other", "None")

# Margins and axis label placement
par(mar = c(10, 6, 4, 2), mgp = c(3, 1, 0))

# Bar chart
bp <- barplot(socialmedia$counts,
              names.arg = sites_labels,
              main = "Social Media Sites Used by Participants",
              xlab = "",
              ylab = "Number of Participants",
              col = "steelblue",
              ylim = c(0, max(socialmedia$counts) + 5),
              cex.names = 0.8,
              las = 2)

# Manual x-axis label
mtext("Platform", side = 1, line = 8)

# Add count labels on top of each bar
text(x = bp,
     y = socialmedia$counts + 2,
     labels = as.numeric(socialmedia$counts),
     cex = 0.9)

############################
# ZERO-ORDER CORRELATIONS - STUDY 2
############################
zeroorder_ros <- list()
zeroorder_ros$result <- cor(data.frame(
  secondros = secondros$scores,
  fs = fs$scores,
  incos = incos$scores,
  secondsmus = secondsmus$scores,
  neuro = fullipip$neuro_scores,
  extra = fullipip$extra_scores,
  open = fullipip$open_scores,
  agree = fullipip$agree_scores,
  con = fullipip$con_scores,
  secondage = secondage$scores
), use = "complete.obs")
print(round(zeroorder_ros$result, 2))

colnames(zeroorder_ros$result) <- c("ROS", "Flourishing", "INCOS", "Social Media",
                                    "Neuroticism", "Extraversion", "Openness",
                                    "Agreeableness", "Conscientiousness", "Age")
rownames(zeroorder_ros$result) <- colnames(zeroorder_ros$result)

png("study2_correlation_matrix.png", width = 2400, height = 2400, res = 150)

par(mar = c(2, 2, 4, 2))

corrplot(zeroorder_ros$result,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 1.0,
         tl.cex = 1.1,
         col = colorRampPalette(c("steelblue", "white", "firebrick"))(200),
         title = "Zero-Order Correlation Matrix - Study 2",
         mar = c(0, 0, 2, 0))

dev.off()



