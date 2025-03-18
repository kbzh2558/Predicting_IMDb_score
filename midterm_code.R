library(tidyverse)
library(here)
library(gridExtra)
library(cowplot)
library(MPV)
library(car)
library(corrplot)
library(ggfortify)
library(kableExtra)
library(car)
library(doBy)
library(faraway)
library(leaps)
library(broom)
library(boot)
library(caret)
library(glmnet)
library(GGally)
library(ggfortify)
library(rsample)



setwd("D:/PPT&WORD/McGill/MGSC401/R_workspace")

## read data
movies <- read.csv("IMDB_data_Winter_2025.csv")
dict <- read.csv("data_dictionary_IMDB_Winter_2025.csv")

## check
movies %>% str
view(dict)

## data cleaning
# check na -> no nan
movies %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "NA_Count") %>%
  filter(NA_Count > 0) %>%
  arrange(desc(NA_Count)) %>% nrow

# drop unrelevent cols
movies <- movies %>% dplyr::select(-c(movie_title,movie_id,imdb_link,actor1,actor2,actor3,genres))

# change month -> numtidyr::# change month -> num
movies$release_month %>% unique

month_mapping <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6,
                   "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)
movies <- movies %>%
  mutate(release_month = as.numeric(replace(release_month, release_month %in% names(month_mapping), month_mapping[release_month])))

## eda
# hist: left-skewed
ggplot(movies, aes(x = imdb_score))+
  geom_histogram(aes(y = ..density..), bins = 25, col = "black", fill='lightblue', position = "dodge")

# summary stats
summary(movies %>% dplyr::select(-dict$variableName[27:39]) %>%
          select_if(is.numeric))

# tbl of unique levels and their cnt
movies %>% select_if(is.character) %>% dplyr::select(c(-cinematographer, 
                                                -country, -director, -distributor, 
                                                -language)) %>% 
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Level") %>%
  group_by(Column, Level) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Column, desc(Count))

# anova for each chr var with multiple lvl -> fooling results: all significant
lapply(dict$variableName[c(10,11,14,16,41,42)], function(var) {
  formula <- as.formula(paste("imdb_score ~", var))
  summary(aov(formula, data = movies))
})

# group less cnt lvls
group_rare_categories <- function(data, column, min_occurrences = 10) {
  category_counts <- table(data[[column]])
  
  rare_categories <- names(category_counts[category_counts < min_occurrences])
  
  data[[column]] <- ifelse(data[[column]] %in% rare_categories, "Other", data[[column]])
  
  return(data)
}

movies <- movies %>%
  group_rare_categories("language", min_occurrences = 10) %>%
  group_rare_categories("country", min_occurrences = 10) %>%
  group_rare_categories("director", min_occurrences = 10) %>%
  group_rare_categories("cinematographer", min_occurrences = 10) %>%
  group_rare_categories("production_company", min_occurrences = 10) %>%
  group_rare_categories("maturity_rating", min_occurrences = 10) %>%
  group_rare_categories("distributor", min_occurrences = 10)
  

# Check how many levels remain after grouping
table(movies$language) 
table(movies$country)  
table(movies$distributor)  
table(movies$director)
table(movies$cinematographer) 
table(movies$production_company)
table(movies$maturity_rating)

# drop cinematographer &. production_company &. director: too much variation little size across each field

# anova again -> drop cinematographer: > 1%
lapply(dict$variableName[c(10,11,12,14,16,41,42)], function(var) {
  formula <- as.formula(paste("imdb_score ~", var))
  summary(aov(formula, data = movies))
})

# boxplot
ggplot(movies, aes(x = reorder(colour_film, imdb_score, median), y = imdb_score, fill = colour_film)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "IMDb Score by colour_film", x = "colour_film", y = "IMDb Score") +
  theme_minimal()

ggplot(movies, aes(x = reorder(language, imdb_score, median), y = imdb_score, fill = language)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "IMDb Score by language", x = "Language", y = "IMDb Score") +
  theme_minimal()

ggplot(movies, aes(x = reorder(country, imdb_score, median), y = imdb_score, fill = country)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "IMDb Score by Country", x = "Country", y = "IMDb Score") +
  theme_minimal()

ggplot(movies, aes(x = reorder(director, imdb_score, median), y = imdb_score, fill = director)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "IMDb Score by Director", x = "Director", y = "IMDb Score") +
  theme_minimal()

ggplot(movies, aes(x = reorder(maturity_rating, imdb_score, median), y = imdb_score, fill = maturity_rating)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "IMDb Score by maturity_rating", x = "maturity_rating", y = "IMDb Score") +
  theme_minimal()

# group maturity_rating
movies <- movies %>%
  mutate(maturity_rating = case_when(
    maturity_rating %in% c("G", "Other") ~ "G_Other",
    maturity_rating == "R" ~ "R",
    maturity_rating == "PG" ~ "PG",
    maturity_rating == "PG-13" ~ "PG-13",
    maturity_rating == "Approved" ~ "Approved"
  ))

# Check the new categories
table(movies$maturity_rating)

ggplot(movies, aes(x = reorder(maturity_rating, imdb_score, median), y = imdb_score, fill = maturity_rating)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "IMDb Score by maturity_rating", x = "maturity_rating", y = "IMDb Score") +
  theme_minimal()
str(movies)
# drop cols: genre is a better rep for plot_keywords
movies <- movies %>% dplyr::select(-c(cinematographer, production_company, distributor, plot_keywords, director))

# to factor
movies <- movies %>%
  mutate(across(where(is.character), as.factor))
str(movies)


# matrix plot
cor(movies %>% select_if(is.numeric))[1,]

## model-building

# 1. train-test split
set.seed(66)

# 80-20
split <- initial_split(movies, prop = 0.8)
movies_train <- training(split)
movies_test <- testing(split)

# check prop -> good
table(movies_train$language) 
table(movies_test$language) 
table(movies_train$country) 
table(movies_test$country) 
table(movies_train$maturity_rating) 
table(movies_test$maturity_rating) 
table(movies_train$colour_film) 
table(movies_test$colour_film) 

# full
modelfull <- lm(imdb_score ~ movie_budget+release_day+release_month+release_year+duration+
                  language+country+nb_news_articles+actor1_star_meter+actor2_star_meter+
                  actor3_star_meter+colour_film+nb_faces+action+adventure+scifi+
                  thriller+musical+romance+western+sport+horror+drama+war+animation+
                  crime+movie_meter_IMDBpro, data=movies_train)
summary(modelfull)

# 2.step-wise: forward
# bic
model_f <- step(lm(imdb_score ~ 1, data = movies_train), 
     scope = list(lower = imdb_score ~ 1, upper = as.formula(paste("imdb_score ~", paste(names(movies_train)[-which(names(movies_train) == "imdb_score")], collapse = " + ")))),
     k = log(nrow(movies_train)), 
     direction = "forward")

# 2.step-wise: backward
# bic
model_b <- step(lm(imdb_score ~., data = movies_train), 
     scope = list(lower = imdb_score ~ 1, upper = as.formula(paste("imdb_score ~", paste(names(movies_train)[-which(names(movies_train) == "imdb_score")], collapse = " + ")))),
     k = log(nrow(movies_train)), 
     direction = "backward")

# 3.step-wise: both
# bic
model_fb <- step(lm(imdb_score ~., data = movies_train), 
                scope = list(lower = imdb_score ~ 1, upper = as.formula(paste("imdb_score ~", paste(names(movies_train)[-which(names(movies_train) == "imdb_score")], collapse = " + ")))),
                k = log(nrow(movies_train)), 
                direction = "both")
# same models for three approach
summary(model_f)
summary(model_b)
summary(model_fb)

formula(model_f)
formula(model_b)
formula(model_fb)


## vis
# Extract model summaries
summary_full <- summary(modelfull)
summary_fb <- summary(model_fb)

# Extract key metrics
comparison_table <- data.frame(
  Metric = c("R-squared", "Adj. R-squared", "AIC", "BIC", "RMSE", "Num Predictors"),
  Full_Model = c(summary_full$r.squared, summary_full$adj.r.squared, AIC(modelfull), BIC(modelfull), 
                 sqrt(mean(summary_full$residuals^2)), length(coef(modelfull))),
  Stepwise_Model = c(summary_fb$r.squared, summary_fb$adj.r.squared, AIC(model_fb), BIC(model_fb),
                     sqrt(mean(summary_fb$residuals^2)), length(coef(model_fb)))
)

# Extract coefficients and p-values
coeff_table <- merge(
  data.frame(Variable = names(coef(modelfull)), Coef_Full = coef(modelfull), P_Full = coef(summary_full)[,4], row.names = NULL),
  data.frame(Variable = names(coef(model_fb)), Coef_FB = coef(model_fb), P_FB = coef(summary_fb)[,4], row.names = NULL),
  by = "Variable", all = TRUE
)

names(comparison_table) <- c("Metric","Full Model","Stepwise Model")

# Print tables
library(knitr)
library(kableExtra)
library(webshot)

# Display Model Metrics Comparison
kable(comparison_table, caption = "Regression Model Comparison", digits = 4) %>%
  kable_styling()

kable(comparison_table, caption = "Regression Model Comparison", digits = 4) %>%
  kable_styling() %>%
  save_kable("table.html")

# Convert the HTML table to an image (PNG)
webshot("table.html", "table_image.png", zoom = 2)



## colinearity
eigen_val <- eigen(cor(scale(model.matrix(model_fb)[, -1] , center = T, scale = T)))$values
eigen_vec <- eigen(cor(scale(model.matrix(model_fb)[, -1] , center = T, scale = T)))$vector
rownames(eigen_vec) <- colnames(model.matrix(model_fb)[, -1])


# eigen val
eigen_val
# vif
vif_val <- vif(model_fb)

# vif > 5 -> merge PG groups together
vif_val[vif_val>5]

# summary stats by m-ratings
movies_train %>% group_by(maturity_rating) %>%
  summarise(Avg = mean(imdb_score),
            Med = median(imdb_score),
            Q25 = quantile(imdb_score,0.25),
            Q75 = quantile(imdb_score,0.75),
            StD = sd(imdb_score),
            IQR = IQR(imdb_score))

# lamda < 1 -> high colineariy between vars with high entry within same vec
eigen_val[eigen_val<1]
low_eigen_vec <- eigen_vec[, which(eigen_val < 1)]
low_eigen_vec[apply(abs(low_eigen_vec) > 0.3, 1, any), , drop = FALSE]

## potential interactions
# e1: nb_news_articles, movie_budget                   
# e2: language, colorfilm
# e3: language, movie_meter
# e4: budget, duration, action, horror
# e5: animation, Gother
# e6: action, horror, drama
# e7: budget, release year, duration, drama
# e8: rating: pg, pg-13, r

# cor matrix plot -> confirmed conclusions from diag analysis 
corr_matrix <- cor(model.matrix(model_fb)[, -1])
round(corr_matrix, 2) 
colors <- colorRampPalette(c("red","lightgrey","darkblue"))(10)
corrplot(corr_matrix, method="number", col = colors,tl.col = "darkred",tl.cex = 0.6, number.cex = 0.6)

## model advancement
# 1. merge PG and PG-13
movies_train <- training(split)
movies_test <- testing(split)

movies_train <- movies_train %>%
  mutate(maturity_rating = factor(ifelse(maturity_rating %in% c("PG", "PG-13"), 
                                         "PG", 
                                         as.character(maturity_rating))))

movies_test <- movies_test %>%
  mutate(maturity_rating = factor(ifelse(maturity_rating %in% c("PG", "PG-13"), 
                                         "PG", 
                                         as.character(maturity_rating))))

model1 <- lm(formula(model_fb), data=movies_train)
summary(model1)
vif(model1)

# not good
# merge all PG and R
movies_train <- training(split)
movies_test <- testing(split)

movies_train <- movies_train %>%
  mutate(maturity_rating = factor(ifelse(maturity_rating %in% c("PG", "PG-13",'R'), 
                                         "PG_R", 
                                         as.character(maturity_rating))))

model2 <- lm(formula(model_fb), data=movies_train)
summary(model2)
# vif improved
vif(model2)

# keep or no? -> keep
model3 <- update(model2, . ~ . - maturity_rating)
anova(model2, model3)

# change ref -> better interpretation
table(movies_train$maturity_rating)
movies_train$maturity_rating <- relevel(movies_train$maturity_rating, ref = "PG_R")
model2 <- lm(formula(model_fb), data=movies_train)
summary(model2)

## interactions
# e1: nb_news_articles, movie_budget 
# e2: language, colorfilm
# e3: language, movie_meter
# e4: budget, duration, action, horror
# e5: animation, Gother
# e6: action, horror, drama
# e7: budget, release year, duration, drama


# nb_news_articles*movie_budget -> keep                
model4 <- update(model2, .~.+nb_news_articles*movie_budget)
anova(model2, model4)
summary(model4)

# e2: language, colorfilm -> nope
model5 <- update(model4, .~.+language*colour_film)
anova(model4, model5)
summary(model5)

# e3: language, movie_meter
model5 <- update(model4, .~.+language*movie_meter_IMDBpro)
anova(model4, model5)
summary(model5)

# e7: budget, release year, duration, drama
# duration*release_year
model6 <- update(model5, .~.+duration*release_year)
anova(model5, model6)
summary(model6)
# budget*release_year
model7 <- update(model6, .~.+movie_budget*release_year)
anova(model6, model7)
summary(model7)
# budget*release_year
model7 <- update(model6, .~.+movie_budget*release_year)
anova(model6, model7)
summary(model7)
# drama*duration
model8 <- update(model7, .~.+drama*duration)
anova(model7, model8)
summary(model8)

# e5: animation, Gother -> nope
model9 <- update(model8, .~.+animation*maturity_rating)
anova(model8, model9)
summary(model9)

# e6: action, horror, drama
# action*drama -> nope
model9 <- update(model8, .~.+action*drama)
anova(model8, model9)
summary(model9)

# drama*horror
model9 <- update(model8, .~.+horror*drama)
anova(model8, model9)
summary(model9)

# e4: budget, duration, action, horror
# budget*action
model10 <- update(model9, .~.+movie_budget*action)
anova(model9, model10)
summary(model9)
# duration*budget -> nope
model11 <- update(model10, .~.+movie_budget*duration)
anova(model10, model11)
summary(model11)
# duration*horror
model11 <- update(model10, .~.+horror*duration)
anova(model10, model11)
summary(model11)

## transformation + residual
library(MASS)
library(AID)
# nonlinearity
autoplot(model11, which=1:6)
avPlots(model11)

summary(model11)

continuous_vars <- movies_train %>%
  dplyr::select(imdb_score, movie_budget, release_year, duration, 
                nb_news_articles, nb_faces, movie_meter_IMDBpro)

colnames(continuous_vars) <- make.names(c("IMDB_Score", "Budget", "Year", "Duration", 
                                          "News_Articles", "Faces", "Movie_Meter"))

#  scatter plot matrix 
plot_list <- lapply(names(continuous_vars)[-1], function(var) {
  ggplot(continuous_vars, aes_string(x = var, y = "IMDB_Score")) +  # Use valid column name
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = TRUE) +
    theme_minimal() +
    ggtitle(paste("IMDB Score vs", var))
})

grid.arrange(grobs = plot_list, ncol = 2)


# transformation on y -> ^2.5
boxcox(model11, lambda = seq(-2, 2, 0.1))
# zoom in 
boxcox(model11, lambda=seq(1,3,length.out=100))
# square
model12 <- update(model11, I(imdb_score^2.5) ~ .)
summary(model12)
# better normality -> influence analysis needed
autoplot(model12)

# log no like
model13 <- update(model11, log(imdb_score) ~ .)
summary(model13)

# suggests -> 4.5 -> better!
boxcoxlm(model.matrix(model11),movies_train$imdb_score,method="mle",lambda = seq(-5, 5, 0.1))
model13 <- update(model11, I(imdb_score^4.5) ~ .)
summary(model13)

# more transformations in design matrix
avPlots(model13)

## transformations on x
vif(lm(I(imdb_score^4.5) ~ movie_budget + release_year + duration + nb_news_articles + nb_faces + movie_meter_IMDBpro + language + maturity_rating + colour_film + action + horror + drama + animation, data = movies_train))


# exploded vif 
vif(model13)
# scaling key contributors
movies_train$movie_budget <- scale(movies_train$movie_budget)
movies_train$release_year <- scale(movies_train$release_year)
movies_train$duration <- scale(movies_train$duration)



# fit with new data
model14 <- lm(formula(model13), data=movies_train)
vif(model14)
summary(model14)

# check nan or neg values in cols
sapply(movies_train, function(x) sum(is.na(x)))  # Check for NA values
sapply(movies_train, function(x) sum(is.infinite(x)))  # Check for Inf values
sapply(movies_train, function(x) sum(is.nan(x)))  # Check for NaN values


library(ggAVplots)
ggAVplot(model14,"movie_meter_IMDBpro")

model15 <- update(model14, . ~ . - movie_meter_IMDBpro + I(1 / movie_meter_IMDBpro))
model16 <- update(model14, . ~ . - movie_meter_IMDBpro + I(1 / movie_meter_IMDBpro^2))
model17 <- update(model14, . ~ . - movie_meter_IMDBpro + I(movie_meter_IMDBpro^-4))
# poly up to 9 still significant -> problem
model18 <- update(model14, . ~ . - movie_meter_IMDBpro - horror:drama - release_year:duration - duration:drama -language:movie_meter_IMDBpro + poly(movie_meter_IMDBpro,9))

summary(model18)
summary(model17)
summary(model16)
# ^-1 best
summary(model15)
summary(model14)
avPlots(model16)

# check hist -> highly skewed
ggplot(movies_train, aes(x = movie_meter_IMDBpro)) + geom_histogram(bins = 100, col = "black", fill = "lightblue")

ggplot(movies_train, aes(x = log(movie_meter_IMDBpro))) + geom_histogram(bins = 100, col = "black", fill = "lightblue")

# try log -> works
model19 <- update(model14, . ~ . - movie_meter_IMDBpro - language*movie_meter_IMDBpro + 
                    I(log(movie_meter_IMDBpro)) + 
                        language*I(log(movie_meter_IMDBpro)))
summary(model19)
# remove interaction
model19 <- update(model14, . ~ . - movie_meter_IMDBpro - language*movie_meter_IMDBpro + 
                    I(log(movie_meter_IMDBpro)))
summary(model19)

library(e1071)
# log best
skewness(movies_train$movie_meter_IMDBpro)
skewness(log(movies_train$movie_meter_IMDBpro))
skewness(1 / (movies_train$movie_meter_IMDBpro))

# remove some insignificant coeffs
model19 <- update(model19, . ~ . - duration:drama)
summary(model19)
model19 <- update(model19, . ~ . - duration:horror)
summary(model19)
model19 <- update(model19, . ~ . - horror:drama)
summary(model19)

# check nonlinearity again -> influence analysis
# news articles
avPlots(model19)



## check residuals -> transformations needed
autoplot(model19,which=1:6)
model20 <- update(model19, imdb_score ~ .)
# transformation -> 3
boxcox(model20, lambda = seq(-2, 2, 0.1))
boxcox(model20, lambda = seq(1, 4, 0.1))
boxcoxlm(model.matrix(model20),movies_train$imdb_score,method="mle",lambda = seq(-5, 5, 0.1))

# better than 19
model20 <- update(model19, I(imdb_score^3) ~ .)
summary(model20)
autoplot(model20,which=1:6)

autoplot(model19,which=1)
autoplot(model20,which=1)



## influence analysis
# need influence analysis, non-linearity
resid_tibble<-tibble(stud=rstandard(model20), rstud=rstudent(model20),diff=stud-rstud, avg=(stud+rstud)/2)
ggplot(resid_tibble,aes(x=avg,y=diff)) +geom_point()
resid_tibble %>%mutate(id=1:nrow(resid_tibble)) %>%arrange(-abs(avg)) %>%head(10)


infl_model20 <- influence.measures(model20)
infl_values <- influence.measures(model20)
infl_df <- as.data.frame(infl_values$infmat)
infl_selected <- infl_df[, c("hat", "cook.d", "cov.r", "dffit")]

summary(infl_selected)

infl_selected[order(-infl_selected$cook.d), ][1:10, ] 

# graph
augmented_model20 <- augment(model20)
augmented_model20$y <- model20$model[[1]]  
augmented_model20$y <- as.numeric(augmented_model20$y)

augmented_model20 <- augmented_model20 %>%
  dplyr::select(.resid, .hat, .sigma, .cooksd, .std.resid, .fitted, y) %>%
  mutate(isinfl = apply(infl_model20$is.inf, 1, any))  # Identify influential points

ggpairs(
  augmented_model20 %>% dplyr::select(isinfl, .hat, .cooksd, .fitted, .std.resid, y),
  aes(col = factor(isinfl))
)

#
p <- length(coef(model20)) 
n <- nrow(infl_selected)   
hii_threshold <- 3 * p / n
dffit_threshold <- 3 * sqrt(p / (n - p))
covr_threshold <- 3 * p / (n - p)

infl_selected <- infl_selected %>%
  mutate(isinfl = abs(dffit) > dffit_threshold | 
           abs(1 - cov.r) > covr_threshold | 
           hat > hii_threshold | 
           cook.d > (4 / n))

# top 5 on cooks
top10_infl <- infl_selected %>% 
  arrange(desc(cook.d)) %>% 
  slice(1:5)
library(ggrepel)  
# suggest maybe spline?
ggplot(infl_selected, aes(x = hat, y = cook.d, color = factor(isinfl))) +
  geom_point(alpha = 0.6, size = 2) +  # Adjust transparency and point size
  geom_text_repel(data = top10_infl, aes(label = rownames(top10_infl)), 
                  size = 5, color = "black", fontface = "bold",
                  nudge_y = 0.2, # Move labels slightly to avoid overlap
                  segment.color = "gray", segment.size = 0.5) +  # Connecting lines
  theme_minimal() +
  labs(title = "Influence Plot with Top 5 Points Labeled",
       x = "Leverage (Hat values)", 
       y = "Cook's Distance",
       color = "Influence Status") + 
  theme(legend.position = "bottom", 
        text = element_text(size = 14))
# display
infl_selected %>% dplyr::filter(isinfl==T) %>% arrange(desc(cook.d)) %>% slice(1:10)



# fit without
top50_indices <- infl_selected %>% dplyr::filter(isinfl==T) %>% arrange(desc(cook.d)) %>% slice(1:70) %>% rownames %>% as.numeric()
model21 <- update(model20, data = movies_train[-top50_indices, ])

# Compare summaries
summary(model20)
summary(model21)

# Extract model summaries
summary_all_points <- summary(model20)  # Model with all data points
summary_filtered <- summary(model21)  # Model after removing 70 points

# Extract key metrics
comparison_table <- data.frame(
  Metric = c("R-squared", "Adj. R-squared", "AIC", "BIC", "RMSE", "Num Predictors"),
  All_Points = c(summary_all_points$r.squared, summary_all_points$adj.r.squared, 
                 AIC(model20), BIC(model20), sqrt(mean(summary_all_points$residuals^2)), 
                 length(coef(model20))),
  After_Removing_70 = c(summary_filtered$r.squared, summary_filtered$adj.r.squared, 
                        AIC(model21), BIC(model21), sqrt(mean(summary_filtered$residuals^2)), 
                        length(coef(model21)))
)

# Extract coefficients and p-values for both models
coeff_table <- merge(
  data.frame(Variable = names(coef(model20)), Coef_All = coef(model20), P_All = coef(summary_all_points)[, 4], row.names = NULL),
  data.frame(Variable = names(coef(model21)), Coef_Filtered = coef(model21), P_Filtered = coef(summary_filtered)[, 4], row.names = NULL),
  by = "Variable", all = TRUE
)


kable(comparison_table, caption = "Regression Model Comparison Before and After Removing Influential Points", digits = 4) %>%
  kable_styling()

# Display Coefficient Comparison
kable(coeff_table, caption = "Coefficient Comparison Before and After Removing Influential Points", digits = 4) %>%
  kable_styling()

library(stargazer)
stargazer(model20, model21, type = "latex", 
          title = "Regression Model Comparison Before and After Removing Influential Points",
          column.labels = c("Model Before Removal", "Model After Removal"),
          dep.var.labels = c("IMDb Score$^3$"),
          covariate.labels = c(
            "Movie Budget",
            "Release Year",
            "Duration",
            "Approved Maturity Rating",
            "G and Other Maturity Rating",
            "Number of Articles",
            "Colour Film",
            "Number of Faces",
            "Action Genre",
            "Horror Genre",
            "Drama Genre",
            "Animation Genre",
            "log(Movie Rank 2023 by IMDbPro)",
            "Movie Budget $\\times$ Number of Articles",
            "Release Year $\\times$ Duration",
            "Movie Budget $\\times$ Release Year",
            "Movie Budget $\\times$ Action Genre"
          ),
          align = TRUE,
          digits = 2,
          single.row = TRUE,
          no.space = TRUE)


# check -> improved but still
resid_tibble<-tibble(stud=rstandard(model21), rstud=rstudent(model21),diff=stud-rstud, avg=(stud+rstud)/2)
ggplot(resid_tibble,aes(x=avg,y=diff)) +geom_point()

# more nonlinear analysis
movies_train_sub <- movies_train[-top50_indices, ] %>%
  mutate(log_movie_meter = log(movie_meter_IMDBpro),
         imdb_score_cubed = imdb_score^3)


continuous_vars <- movies_train_sub %>%
  dplyr::select(imdb_score_cubed, movie_budget, release_year, duration, 
         nb_news_articles, nb_faces, log_movie_meter)

colnames(continuous_vars) <- make.names(c("IMDB_Score^3", "Budget", "Year", "Duration", 
                                          "News_Articles", "Faces", "Log_Movie_Meter"))

#  scatter plot matrix 
# year, duration, news articles, log movie meters
plot_list <- lapply(names(continuous_vars)[-1], function(var) {
  ggplot(continuous_vars, aes_string(x = var, y = "IMDB_Score.3")) +  # Use valid column name
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = TRUE) +
    theme_minimal() +
    ggtitle(paste("IMDB Score³ vs", var))
})

grid.arrange(grobs = plot_list, ncol = 2)

# year spline -> yes
library(splines)
summary(movies_train_sub$release_year)
model22 <- update(model21, . ~ . - release_year - duration:release_year - movie_budget:release_year + 
                    bs(release_year, 
                       knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), 
                       degree = 1) +
                    duration*bs(release_year, 
                                                knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), 
                                                degree = 1) + movie_budget*bs(release_year, 
                                                                              knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), 
                                                                              degree = 1) )

summary(model22)
summary(model21)

# poly log movie meter -> yes
model23 <- update(model22, . ~ . - I(log(movie_meter_IMDBpro)) + poly(log_movie_meter,2), data=movies_train_sub)
summary(model23)

# ploy articles 
model24 <- update(model23, . ~ . - nb_news_articles - movie_budget:nb_news_articles + poly(nb_news_articles,3))
summary(model24)
summary(model23)

# poly duration -> yes
model25 <- update(model24, . ~ . - duration -duration:bs(release_year, knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), degree = 1) + poly(duration,2)+ poly(duration,2)*bs(release_year, knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), degree = 1))
summary(model25)
# rmv interac
model25 <- update(model25, . ~ . - poly(duration,2):bs(release_year, knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), degree = 1))
summary(model25)

# spline duration -> not good 
duration_knots <- quantile(movies_train_sub$duration, probs = c(0.5, 0.8))
model26 <- update(model24, . ~ . - duration -release_year:duration + bs(duration, knots = duration_knots, degree=1) + release_year*bs(duration, knots = duration_knots, degree=1))
summary(model26)


# rm insignificant vars
model26 <- update(model25, .~. -maturity_rating - movie_budget:bs(release_year, knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), degree = 1))
summary(model26)

summary(model21)

vif(model26)
# check

# try to fit without the points identified
infl_values26 <- influence.measures(model26)
infl_df26 <- as.data.frame(infl_values26$infmat)
infl_selected26 <- infl_df[, c("hat", "cook.d", "cov.r", "dffit")]

# not really useful
top10_indices <- infl_selected26[order(-infl_selected26$cook.d), ][1:10, ] %>% rownames %>% as.numeric
model27 <- update(model26, data = movies_train_sub[-top10_indices, ])
summary(model27)


## go with model26
summary(model26)

# eigen val
eigen_val26<-eigen(cor(scale(model.matrix(model26)[, -1] , center = T, scale = T)))$values
eigen_vec26<-eigen(cor(scale(model.matrix(model26)[, -1] , center = T, scale = T)))$vectors
rownames(eigen_vec26) <- colnames(model.matrix(model26)[, -1])

# near zero: 17
eigen_val26
eigen_vec26[,17]

# add interac 
model27 <- update(model26, .~. +movie_budget*bs(release_year, knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), degree = 1))
summary(model27)

# try only sp2
release_year_spline <- bs(movies_train_sub$release_year, knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), degree = 1)
movies_train_sub$spline_release_year2 <- release_year_spline[,2]
model27 <- update(model26, . ~ . - movie_budget:bs(release_year, knots = c(mean(movies_train_sub$release_year, na.rm = TRUE)), degree = 1) + 
                    movie_budget:spline_release_year2)
summary(model27)
# check
eigen_val27<-eigen(cor(scale(model.matrix(model27)[, -1] , center = T, scale = T)))$values
eigen_vec27<-eigen(cor(scale(model.matrix(model27)[, -1] , center = T, scale = T)))$vectors
rownames(eigen_vec27) <- colnames(model.matrix(model27)[, -1])
vif(model27)
eigen_vec27[,17]
autoplot(model27, which=1:6)

# CV
# final model

knot_mean <- mean(movies_train_sub$release_year, na.rm = TRUE)
spline_basis <- bs(
  movies_train_sub$release_year,
  knots = knot_mean,
  degree = 1
)

# Add named columns to your dataset
movies_train_sub <- movies_train_sub %>%
  mutate(
    spline_B1 = spline_basis[,1],  
    spline_B2 = spline_basis[,2]   
  )

model28 <- update(model26, 
                  . ~ . - + bs(release_year, knots = c(mean(movies_train_sub$release_year, 
                                                            na.rm = TRUE)), degree = 1) + 
                    spline_B1 + spline_B2 +
                    movie_budget * spline_B2)
summary(model28)

library(Metrics)   

set.seed(123)

n <- nrow(movies_train_sub)
num_fold <- c(5,10,'LOOCV')

for (k in num_fold) {
  if (k == "LOOCV") {
    k <- n  
  }

  folds <- sample(rep(1:k, length.out = n))
  
  mse_values <- numeric(k)
  rmse_values <- numeric(k)
  mape_values <- numeric(k)
  
  for(i in 1:k){
    # Split data into training and test sets
    train_data <- movies_train_sub[folds != i, ]
    test_data  <- movies_train_sub[folds == i, ]
    
    ## --- Polynomial Transformations ---
    # For log_movie_meter (degree 2)
    p_log_movie_meter <- poly(train_data$log_movie_meter, 2)
    train_poly_log_movie_meter <- as.matrix(p_log_movie_meter)
    train_data$poly_log_movie_meter1 <- train_poly_log_movie_meter[,1]
    train_data$poly_log_movie_meter2 <- train_poly_log_movie_meter[,2]
    
    test_poly_log_movie_meter <- predict(p_log_movie_meter, 
                                         newdata = as.numeric(test_data$log_movie_meter))
    test_data$poly_log_movie_meter1 <- test_poly_log_movie_meter[,1]
    test_data$poly_log_movie_meter2 <- test_poly_log_movie_meter[,2]
    
    # For nb_news_articles (degree 3)
    p_nb_news_articles <- poly(train_data$nb_news_articles, 3)
    train_poly_nb_news_articles <- as.matrix(p_nb_news_articles)
    train_data$poly_nb_news_articles1 <- train_poly_nb_news_articles[,1]
    train_data$poly_nb_news_articles2 <- train_poly_nb_news_articles[,2]
    train_data$poly_nb_news_articles3 <- train_poly_nb_news_articles[,3]
    
    test_poly_nb_news_articles <- predict(p_nb_news_articles, 
                                          newdata = as.numeric(test_data$nb_news_articles))
    test_data$poly_nb_news_articles1 <- test_poly_nb_news_articles[,1]
    test_data$poly_nb_news_articles2 <- test_poly_nb_news_articles[,2]
    test_data$poly_nb_news_articles3 <- test_poly_nb_news_articles[,3]
    
    # For duration (degree 2)
    p_duration <- poly(train_data$duration, 2)
    train_poly_duration <- as.matrix(p_duration)
    train_data$poly_duration1 <- train_poly_duration[,1]
    train_data$poly_duration2 <- train_poly_duration[,2]
    
    test_poly_duration <- predict(p_duration, 
                                  newdata = as.numeric(test_data$duration))
    test_data$poly_duration1 <- test_poly_duration[,1]
    test_data$poly_duration2 <- test_poly_duration[,2]
    
    ## --- Spline Basis for release_year ---
    # Compute an internal knot using the training data (using the mean here)
    knot_train <- mean(train_data$release_year)
    spline_train <- bs(train_data$release_year, knots = knot_train, degree = 1)
    knots <- attr(spline_train, "knots")
    Boundary.knots <- attr(spline_train, "Boundary.knots")
    
    train_data$spline_B1 <- spline_train[,1]
    train_data$spline_B2 <- spline_train[,2]
    
    spline_test <- bs(test_data$release_year, degree = 1, knots = knots, 
                      Boundary.knots = Boundary.knots)
    test_data$spline_B1 <- spline_test[,1]
    test_data$spline_B2 <- spline_test[,2]
    
    ## fit
    fold_formula <- I(imdb_score^3) ~ movie_budget + colour_film + nb_faces + action +
      horror + drama + animation +
      poly_log_movie_meter1 + poly_log_movie_meter2 +
      poly_nb_news_articles1 + poly_nb_news_articles2 + poly_nb_news_articles3 +
      poly_duration1 + poly_duration2 +
      spline_B1 + spline_B2 +
      movie_budget:spline_B2 + movie_budget:action
    
    model <- lm(fold_formula, data = train_data)
    
    predictions <- predict(model, newdata = test_data)
    actuals <- test_data$imdb_score
    
    mse_values[i] <- mean((predictions^(1/3) - actuals)^2)
    rmse_values[i] <- sqrt(mse_values[i])
    mape_values[i] <- mean(abs((actuals - predictions^(1/3))/actuals)) * 100
  }
  
  avg_mse <- mean(mse_values)
  avg_rmse <- mean(rmse_values)
  avg_mape <- mean(mape_values)
  
  cat(paste(k, "-Fold Cross-Validation Results:\n"))
  cat("MSE:", avg_mse, "\n")
  cat("RMSE:", avg_rmse, "\n")
  cat("MAPE:", avg_mape, "\n")
}


# operation on test set
movies_test <- movies_test %>%
  mutate(maturity_rating = factor(ifelse(maturity_rating %in% c("PG", "PG-13",'R'), 
                                         "PG_R", 
                                         as.character(maturity_rating))))
movies_test$movie_budget <- scale(movies_test$movie_budget)
movies_test$release_year <- scale(movies_test$release_year)
movies_test$duration <- scale(movies_test$duration)

movies_test <- movies_test %>%
  mutate(log_movie_meter = log(movie_meter_IMDBpro))

knots <- attr(spline_basis, "knots")
Boundary.knots <- attr(spline_basis, "Boundary.knots")

spline_basis_test <- bs(
  movies_test$release_year,  # Apply to test set
  knots = knots,                 # Use training set knots
  Boundary.knots = Boundary.knots,  # Ensure boundary consistency
  degree = 1
)

movies_test <- movies_test %>%
  mutate(
    spline_B1 = spline_basis_test[,1],  
    spline_B2 = spline_basis_test[,2]
  )


p_log_movie_meter <- poly(movies_train_sub$log_movie_meter, 2)
movies_train_sub <- movies_train_sub %>%
  mutate(
    poly_log_movie_meter1 = p_log_movie_meter[, 1],
    poly_log_movie_meter2 = p_log_movie_meter[, 2]
  )

p_nb_news_articles <- poly(movies_train_sub$nb_news_articles, 3)
movies_train_sub <- movies_train_sub %>%
  mutate(
    poly_nb_news_articles1 = p_nb_news_articles[, 1],
    poly_nb_news_articles2 = p_nb_news_articles[, 2],
    poly_nb_news_articles3 = p_nb_news_articles[, 3]
  )

# For duration (degree 2)
p_duration <- poly(movies_train_sub$duration, 2)
movies_train_sub <- movies_train_sub %>%
  mutate(
    poly_duration1 = p_duration[, 1],
    poly_duration2 = p_duration[, 2]
  )


model_fit <- lm(I(imdb_score^3) ~ movie_budget + colour_film + nb_faces + action +
                  horror + drama + animation +
                  poly_log_movie_meter1 + poly_log_movie_meter2 +
                  poly_nb_news_articles1 + poly_nb_news_articles2 + poly_nb_news_articles3 +
                  poly_duration1 + poly_duration2 +
                  spline_B1 + spline_B2 +
                  movie_budget:action + movie_budget:spline_B2,
                data = movies_train_sub)


test_poly_log_movie_meter <- predict(p_log_movie_meter, 
                                     newdata = as.numeric(movies_test$log_movie_meter))
movies_test <- movies_test %>%
  mutate(
    poly_log_movie_meter1 = test_poly_log_movie_meter[, 1],
    poly_log_movie_meter2 = test_poly_log_movie_meter[, 2]
  )

# For nb_news_articles (degree 3)
test_poly_nb_news_articles <- predict(p_nb_news_articles, 
                                      newdata = as.numeric(movies_test$nb_news_articles))
movies_test <- movies_test %>%
  mutate(
    poly_nb_news_articles1 = test_poly_nb_news_articles[, 1],
    poly_nb_news_articles2 = test_poly_nb_news_articles[, 2],
    poly_nb_news_articles3 = test_poly_nb_news_articles[, 3]
  )

# For duration (degree 2)
test_poly_duration <- predict(p_duration, 
                              newdata = as.numeric(movies_test$duration))
movies_test <- movies_test %>%
  mutate(
    poly_duration1 = test_poly_duration[, 1],
    poly_duration2 = test_poly_duration[, 2]
  )

# pred
predictions <- predict(model_fit, newdata = movies_test)

predictions_original <- predictions^(1/3)

actuals <- movies_test$imdb_score

# MSE RMSE MAPE
mse_test <- mean((predictions_original - actuals)^2)
rmse_test <- sqrt(mse_test)
mape_test <- mean(abs((actuals - predictions_original)/actuals)) * 100

# Out-of-Sample R2
sse <- sum((actuals - predictions_original)^2)
sst <- sum((actuals - mean(actuals))^2)
r2_test <- 1 - sse/sst


cat("Test Set Performance:\n")
cat("MSE: ", mse_test, "\n")
cat("RMSE: ", rmse_test, "\n")
cat("MAPE: ", mape_test, "\n")
cat("Out-of-Sample R^2: ", r2_test, "\n")



# tbl vis

library(stargazer)


stargazer(model28, type = "latex", 
          title = "Regression Results",
          dep.var.labels = c("IMDb Score$^3$"),
          covariate.labels = c(
            "Movie Budget",
            "Colour Film",
            "Number of Faces",
            "Action Genre",
            "Horror Genre",
            "Drama Genre",
            "Animation Genre",
            "log(Movie Rank 2023 by IMDbPro)",
            "log(Movie Rank 2023 by IMDbPro)$^2$",
            "Number of Articles",
            "Number of Articles$^2$",
            "Number of Articles$^3$",
            "Duration",
            "Duration$^2$",
            "Release Year before Mean",
            "Release Year after Mean",
            "Movie Budget $\\times$ Action Genre",
            "Movie Budget $\\times$ Release Year after Mean"
          ),
          align = TRUE,
          digits = 2,
          single.row = TRUE,
          no.space = TRUE)

stargazer(model_fb, type = "latex", 
          title = "Stepwise Baseline Model Summary",
          dep.var.labels = c("IMDb Score"),
          covariate.labels = c(
            "Movie Budget",
            "Release Year",
            "Duration",
            "Other Language",
            "G and Other Maturity Rating",
            "PG Maturity Rating",
            "PG-13 Maturity Rating",
            "R Maturity Rating",
            "Number of Articles",
            "Colour Film",
            "Number of Faces",
            "Action Genre",
            "Horror Genre",
            "Drama Genre",
            "Animation Genre",
            "Movie Rank 2023 by IMDbPro"
          ),
          align = TRUE,
          digits = 2,
          single.row = TRUE,
          no.space = TRUE)


# pred on 12 new movies
library(readxl)

df <- read_excel("12 New Movies.xlsx") %>% 
  mutate(colour_film = as.factor(colour_film))

shared_attrs <- c("colour_film", "nb_faces", "action", "horror", "drama", "animation")

# fill na missing values by looking up movies with matching attributes and movie_meter_IMDBpro within ±500
df_filled <- df %>%
  rowwise() %>%
  mutate(
    movie_budget = if_else(is.na(movie_budget),
      mean(movies$movie_budget[
        as.character(movies$colour_film) == as.character(colour_film) &
        movies$nb_faces == nb_faces &
        movies$action == action &
        movies$horror == horror &
        movies$drama == drama &
        movies$animation == animation &
        abs(movies$movie_meter_IMDBpro - movie_meter_IMDBpro) <= 500
      ], na.rm = TRUE),
      movie_budget
    ),
    duration = if_else(is.na(duration),
      mean(movies$duration[
        as.character(movies$colour_film) == as.character(colour_film) &
        movies$nb_faces == nb_faces &
        movies$action == action &
        movies$horror == horror &
        movies$drama == drama &
        movies$animation == animation &
        abs(movies$movie_meter_IMDBpro - movie_meter_IMDBpro) <= 500
      ], na.rm = TRUE),
      duration
    ),
    nb_news_articles = if_else(is.na(nb_news_articles),
      mean(movies$nb_news_articles[
        as.character(movies$colour_film) == as.character(colour_film) &
        movies$nb_faces == nb_faces &
        movies$action == action &
        movies$horror == horror &
        movies$drama == drama &
        movies$animation == animation &
        abs(movies$movie_meter_IMDBpro - movie_meter_IMDBpro) <= 500
      ], na.rm = TRUE),
      nb_news_articles
    )
  ) %>%
  ungroup()
df_filled

# 
df_filled$movie_budget <- scale(df_filled$movie_budget)
df_filled$release_year <- scale(df_filled$release_year)
df_filled$duration <- scale(df_filled$duration)

df_filled <- df_filled %>%
  mutate(log_movie_meter = log(movie_meter_IMDBpro))

knots <- attr(spline_basis, "knots")
Boundary.knots <- attr(spline_basis, "Boundary.knots")

spline_basis_test <- bs(
  df_filled$release_year,  
  knots = knots,                 # Use training set knots
  Boundary.knots = Boundary.knots,  # Ensure boundary consistency
  degree = 1
)

df_filled <- df_filled %>%
  mutate(
    spline_B1 = spline_basis_test[,1],  
    spline_B2 = spline_basis_test[,2]
  )

test_poly_log_movie_meter <- predict(p_log_movie_meter, 
                                     newdata = as.numeric(df_filled$log_movie_meter))
df_filled <- df_filled %>%
  mutate(
    poly_log_movie_meter1 = test_poly_log_movie_meter[, 1],
    poly_log_movie_meter2 = test_poly_log_movie_meter[, 2]
  )

# For nb_news_articles (degree 3)
test_poly_nb_news_articles <- predict(p_nb_news_articles, 
                                      newdata = as.numeric(df_filled$nb_news_articles))
df_filled <- df_filled %>%
  mutate(
    poly_nb_news_articles1 = test_poly_nb_news_articles[, 1],
    poly_nb_news_articles2 = test_poly_nb_news_articles[, 2],
    poly_nb_news_articles3 = test_poly_nb_news_articles[, 3]
  )

# For duration (degree 2)
test_poly_duration <- predict(p_duration, 
                              newdata = as.numeric(df_filled$duration))
df_filled <- df_filled %>%
  mutate(
    poly_duration1 = test_poly_duration[, 1],
    poly_duration2 = test_poly_duration[, 2]
  )

# pred
predictions <- predict(model_fit, newdata = df_filled)

predictions_original <- predictions^(1/3)
names(predictions_original) <- df_filled$name
predictions_original

# store as xlsx
library(writexl)

df_predictions <- data.frame(
  name = df_filled$name,  
  IMDb_score = predictions_original
)

write_xlsx(df_predictions, "predictions.xlsx")


# real test data
df_filled <- read_csv("test_data_IMDB_Winter_2025.csv") %>% 
  mutate(colour_film = as.factor(colour_film))


# 
movies_train <- training(split)

train_means <- apply(movies_train[, c("movie_budget", "release_year", "duration")], 2, mean, na.rm = TRUE)
train_sds <- apply(movies_train[, c("movie_budget", "release_year", "duration")], 2, sd, na.rm = TRUE)

df_filled$movie_budget <- as.numeric(df_filled$movie_budget)

df_filled$movie_budget <- as.matrix((df_filled$movie_budget - train_means["movie_budget"]) / train_sds["movie_budget"])
df_filled$release_year <- (df_filled$release_year - train_means["release_year"]) / train_sds["release_year"]
df_filled$duration <- (df_filled$duration - train_means["duration"]) / train_sds["duration"]


df_filled <- df_filled %>%
  mutate(log_movie_meter = log(movie_meter_IMDBpro))

knots <- attr(spline_basis, "knots")
Boundary.knots <- attr(spline_basis, "Boundary.knots")

spline_basis_test <- bs(
  df_filled$release_year,  
  knots = knots,                 # Use training set knots
  Boundary.knots = Boundary.knots,  # Ensure boundary consistency
  degree = 1
)

df_filled <- df_filled %>%
  mutate(
    spline_B1 = spline_basis_test[,1],  
    spline_B2 = spline_basis_test[,2]
  )

test_poly_log_movie_meter <- predict(p_log_movie_meter, 
                                     newdata = as.numeric(df_filled$log_movie_meter))
df_filled <- df_filled %>%
  mutate(
    poly_log_movie_meter1 = test_poly_log_movie_meter[, 1],
    poly_log_movie_meter2 = test_poly_log_movie_meter[, 2]
  )

# For nb_news_articles (degree 3)
test_poly_nb_news_articles <- predict(p_nb_news_articles, 
                                      newdata = as.numeric(df_filled$nb_news_articles))
df_filled <- df_filled %>%
  mutate(
    poly_nb_news_articles1 = test_poly_nb_news_articles[, 1],
    poly_nb_news_articles2 = test_poly_nb_news_articles[, 2],
    poly_nb_news_articles3 = test_poly_nb_news_articles[, 3]
  )

# For duration (degree 2)
test_poly_duration <- predict(p_duration, 
                              newdata = as.numeric(df_filled$duration))
df_filled <- df_filled %>%
  mutate(
    poly_duration1 = test_poly_duration[, 1],
    poly_duration2 = test_poly_duration[, 2]
  )

# pred
predictions <- predict(model_fit, newdata = df_filled)

predictions_original <- predictions^(1/3)
names(predictions_original) <- df_filled$movie_title    
predictions_original

df_predictions <- data.frame(
  name = df_filled$movie_title,  
  IMDb_score = predictions_original
)

write_xlsx(df_predictions, "2025_movies_predictions.xlsx")

