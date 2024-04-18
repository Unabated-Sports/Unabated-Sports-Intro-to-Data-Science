
# load packages
require(data.table)
require(ggplot2)







#################
### ARTICLE 5 ###
#################

# load data from after article 4.
dat <- data.table(read.csv("data_after_article_4.csv"))

# Build models with the variables we created

# simple linear regression using hh stuff and rapg

# Let's predict home score using both home hh pg and away rpg
fit1 <- lm(data = dat, home_score ~ home_hh_pg_before + away_rapg_before)

# this command will show you info about the model, including the coefficients
summary(fit1)

# add predictions back to the data set for analysis later
dat$home_score_pred1 <- predict(fit1, data=dat)

# plot qq plot
plot(fit1, which=2)

# plot density of residuals
ggplot(dat, aes(x=home_score_pred1 - home_score)) + 
    geom_density() +
    xlab("Model Error") + 
    ylab("Density") +
    ggtitle("Distribution of Residuals") +
    theme_bw()


# Now let's say we believe teams can be on "hot" or "cold" streaks
# We can add in the last_10 vars we created. 
# WE DIDNT CREATE A RAPG_LAST_10 var
fit2 <- lm(
    data = dat, 
    home_score ~ home_hh_pg_before + home_hh_last_10 + away_rapg_before
)

# look at summary of model
summary(fit2)

# add predictions back to the data set
dat$home_score_pred2 <- predict(fit2, data=dat)


# Now we'll do the same for away team (no comments this time)
fit1 <- lm(data = dat, away_score ~ away_hh_pg_before + home_rapg_before)
summary(fit1)
dat$away_score_pred1 <- predict(fit1, data=dat)

fit2 <- lm(
    data = dat, 
    away_score ~ away_hh_pg_before + away_hh_last_10 + home_rapg_before
)

summary(fit2)
dat$away_score_pred2 <- predict(fit2, data=dat)



# Now we'll do some regressions on the game total
fit3 <- lm(
    data = dat, 
    actual_total ~ home_hh_pg_before + away_hh_pg_before + home_hh_last_10 + away_hh_last_10 + home_rapg_before + away_rapg_before
)

summary(fit3)
dat$total_pred <- predict(fit3, data=dat)


# Now we can do some Logistic regression on whether the total went over
# this is a binary outcome, the Logistic regression will allow us to predict 
# a probability that the game goes over the total

# build logistic model on whether the total was over
# we'll just a few vars that come up as significant
fit4 <- glm(
    data = dat,
    family = "binomial",
    tot_over ~ home_hh_last_10 + home_rapg_before + away_rapg_before
)

summary(fit4)
dat$over_prob_pred <- predict(fit4, data=dat, type="response")


# Now we can convert these probs back to odds to compare to no-vig market
dat[, o_nv_odds := sapply(o_nv_prob, conv_prob_to_odds)]
dat[, o_pred_odds := sapply(over_prob_pred, conv_prob_to_odds)]

# Ok we've got some models built, in the next article, we'll assess how well they perform.
