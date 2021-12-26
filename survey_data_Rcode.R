
# In this work, I am using a survey that would ask customers to rate various aspects of 
# their experience, such as comfort of seats, delay handling, friendliness of flight attendants,
# overall customer satisfaction, and etc. I want to know the key drivers of overall customer 
# satisfaction to suggest airline how they can improve customer satisfaction.


# load package
library(ggstatsplot)

# Reading the data set.
airline = read.csv("survey_data.csv")
# Looking at data.
str(airline)
# Checking for missing values.
summary(airline)
colSums(is.na(airline))
# We can see values of 'delay minutes' and 'delay handling' columns are "NA" for ontime flights.
# Thus, I seperate this dataset into two 'ontime' and 'delayed' datasets for further exploring.
dataset1 = airline[!is.na(airline$delay_minutes), ]
dataset2 = airline[is.na(airline$delay_minutes), ]
data_delay = dataset1[, - which(colnames(dataset1) == "was_flight_delayed")]
data_ontime = dataset2[, colSums(is.na(dataset2)) == 0]
data_ontime = data_ontime[, - which(colnames(data_ontime) == "was_flight_delayed")]

# Correlogram for delayed flights to see which coefficients are
# statistically significant.

ggstatsplot::ggcorrmat(
  data = data_delay,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

# Using backward elimination method for finding drivers of overall satisfaction.

modelFit1 <- lm(overall_customer_satisfaction ~ ., data = data_delay)
summary(modelFit1)

modelFit2 <- update(modelFit1, .~.-seat_cleanliness)
summary(modelFit2)

modelFit3 <- update(modelFit2, .~.-flight_attendant_attentiveness)
summary(modelFit3)

# Using F test to compare three model and make sure modelFit3 is the best model.
anova(modelFit3, modelFit2, modelFit1)
# We can see in the ANOVA table that p-value is much higher than 0.05 and
# we can say there is no statistically significant evidence to reject the null hypothesis.
# Thus, we accept that the coefficients of "flight_attendant_attentiveness" and 
# "seat_cleanliness" are zero.


# We can check the residual values to see if they have a normal distribution with zero mean
# and a fixed variance.

plot(fitted(modelFit3), modelFit3$residuals)
abline(h = 0)
qqnorm(modelFit3$residuals)
qqline(modelFit3$residuals)



# Correlogram for ontime flights to see which coefficients are statistically significant.

ggstatsplot::ggcorrmat(
  data = data_ontime,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

# Using backward elimination method for finding drivers of overall satisfaction for ontime flights.

modelFit11 <- lm((overall_customer_satisfaction) ~ ., data = data_ontime)
summary(modelFit11)

modelFit22 <- update(modelFit11, .~.-seat_cleanliness)
summary(modelFit22)

modelFit33 <- update(modelFit22, .~.-flight_attendant_attentiveness)
summary(modelFit33)

# Using F test to compare three model and make sure modelFit33 is the best.

anova(modelFit33, modelFit22, modelFit11)
# We can see in the ANOVA table that p-value is far away from 0.05 (close to 1) and
# we can say there is no statistically significant evidence to reject the null hypothesis.
# Thus, we accept that the coefficients of "flight_attendant_attentiveness" and 
# "seat_cleanliness" are zero.


# We can check the residual graph to see if they have a normal distribution with zero mean
# and a fixed variance.

plot(fitted(modelFit33), modelFit33$residuals)
abline(h = 0)
qqnorm(modelFit33$residuals)
qqline(modelFit33$residuals)




