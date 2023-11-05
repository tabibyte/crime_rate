library(tidyverse)
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes)
library(caret)
library(ourrr)
library(graphics)
library(Hmisc)
library(glue)
library(h20)
library(patchwork)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

raw <- fread("crimes.csv")

raw  %>%  glimpse()

raw  %>%
  inspect_na()

target <- 'ViolentCrimesPerPop'
features <- raw  %>% select(-ViolentCrimesPop)  %>%  names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = raw)

glm  %>%  summary()

while(glm  %>%  faraway::vif()  %>%  sort(decreasing = TRUE)  %>%  .[1] >= 2){
    afterVIF <- glm  %>%  faraway::vif()  %>%  sort(decreasing = TRUE)  %>%
    .[-1]  %>%  names()
    f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
    glm <- glm(f, data = raw)
}

glm  %>%  faraway::vif()  %>%  sort(decreasing = TRUE)  %>%  names() -> features

df <- raw  %>%  
select(ViolentCrimesPerPop,features)  %>%
  as.data.frame()

df  %>%  glimpse()

df[, -1] <- df[, -1]  %>%  scale()  %>%  as.data.frame()

h20.init()

h20_data <- df %>%  as.h20()

h20_data <- h20_data  %>%  h20.splitFrame(ratios = 0.8, seed = 123)
train <- h20_data[[1]]
test <- h20_data[[2]]
features <- df  %>%  select(-ViolentCrimesPerPop)  %>%  names()

model <- h20.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = TRUE)

model@model$coefficients_table  %>%
  as.data.frame()  %>%
  dplyr::select(names,p_value)  %>%
  mutate(p_value = round(p_value,3))  %>%
  .[-1]  %>%
  arrange(desc(p_value))

while(model@model$coefficients_table  %>%
      as.data.frame()  %>%
      dplyr::select(names,p_value)  %>%
      mutate(p_value = round(p_value,3))  %>%
      .[-1]  %>%
      arrange(desc(p_value))  %>%
      .[1,2] > 0.05) {
  model@model$coefficients_table  %>%
    as.data.frame()  %>%
    dplyr::select(names,p_value)  %>%
    mutate(p_value = round(p_value,3))  %>%
    filter(!is.nan(p_value))  %>%
    .[-1]  %>%
    arrange(desc(p_value))  %>%
    .[1,1] -> v
  features <- features[features != v]

  train <- train  %>%  as.data.frame()  %>%
    select(target,features) %>%  as.h20()

  test <- test %>%  as.data.frame() %>%
    select(target,features) %>%  as.h20()


  model <- h20.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = TRUE)
  }

model@model$coefficients_table  %>%
  as.data.frame()  %>%
  dplyr::select(names,p_value)  %>%
  mutate(p_value = round(p_value,3))


y_pred <- model %>%  h20.predict(newdata = test) %>% as.data.frame()
y_pred$predict

test_set <-  test  %>%  as.data.frame()
residuals = test_set$ViolentCrimesPerPop - y_pred$predict

RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test_set$ViolentCrimesPerPop)

tss = sum((test_set$ViolentCrimesPerPop - y_test_mean)^2)
rss = sum(residuals^2)

R2 = 1 - (rss/tss); R2

N <- test_set  %>%  nrow()
k <- features  %>%  length()
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),R2,Adjusted_R2)

my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$ViolentCrimesPerPop)  %>%  as.data.frame()

g_train <- my_data_train  %>%
  ggplot(aes(predicted, observed)) +
  geom_point(color = "red")
  geom_smooth(method = lm)
  labs(x = "Predecited PWRTPT",
       y = "Observed PWRTPT",
       title = glue('Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color ="green",size = 16, hjust =0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

g_train  %>%  ggplotly()

g_train + g
tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),

       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)