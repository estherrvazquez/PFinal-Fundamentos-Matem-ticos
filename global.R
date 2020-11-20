rm(list = ls())
library(ggplot2)
library(dplyr)
library(class)
library(caret)
library(shiny)
library(Metrics)
library(tidyverse)
library(shinyAce)

showfiles <-  list(
  aceEditor("server",
            value = paste(readLines("server.R"), collapse="\n"),
            mode="r",  height="380px",
            readOnly=TRUE),
  aceEditor("ui",
            value = paste(readLines("ui.R"), collapse="\n"),
            mode="r",  height="270px",
            readOnly=TRUE)
)

ds <- read.csv("datos/heart.csv",  header = F)

df <- read.csv("datos/heart.csv",  header = F)

names(ds) <- c( "age", "sex", "cp",
                "trestbps", "chol",
                "fbs", "restecg",
                "thalach","exang",
                "oldpeak","slope",
                "ca","thal","num")

names(df) <- c( "age", "sex", "cp",
                "trestbps", "chol",
                "fbs", "restecg",
                "thalach","exang",
                "oldpeak","slope",
                "ca","thal","num")

feature.list <- list("age" = "age", "sex" ="sex",
                     "cp"= "cp","trestbps" = "trestbps",
                     "chol"="chol","fbs"="fbs",
                     "restecg"="restecg","thalach"="thalach",
                     "exang"="exang","oldpeak"="oldpeak",
                     "slope"="slope","ca"="ca","thal"="thal")

# change the class of all columns to numeric
ds <- as.data.frame(apply(ds, 2, as.numeric))
df <- as.data.frame(apply(df, 2, as.numeric))
# remove na/missing values (original data shows as ?)
ds <- na.omit(ds)
df <- na.omit(df)

# df_1 <- df %>%
#   filter(num == 1) %>%
#   group_by(thal) %>%
#   count()
# 
# df_2 <- df %>%
#   filter(num == 0) %>%
#   group_by(thal) %>%
#   count()


ds$num[ds$num > 0] <- 1
df$num[df$num > 0] <- 1

ds$thal[ds$thal == 0] <- 1
df$thal[df$thal == 0] <- 1

df$num <- as.factor(df$num)
theme_set(theme_bw()) # The current theme is automatically applied to every plot we draw

df_s <- df
df_s <- df %>%
  mutate(num = fct_recode(num, absent = "0", present = "1"))

chart <-
  df_s %>%
  count(num) %>%
  mutate(pct = round(n / sum(n) * 100), num = reorder(num, n)) %>%
  ggplot(aes(x = num, y = n)) +
  geom_segment(aes(xend = num, yend = 0), size = 1.5, lineend = "square") +
  geom_point(size = 10, color = c("#17B3BF", "#FAFA77")) +
  theme_bw() +
  labs(x = "Heart disease", y = "Number of patients") +
  geom_text(aes(label = str_c(pct, " %")), vjust = -0, size = 2.5, colour = "black") +
  theme(axis.title.y = element_text(size = 12, face = "bold"), axis.title.x = element_text(size = 12, face = "bold"), axis.text.x = element_text(vjust = 0.3))



df$sex <- as.factor(df$sex)
df$cp <- as.factor(df$cp)
df$restecg <- as.factor(df$restecg)
df$exang <- as.factor(df$exang)
df$slope <- as.factor(df$slope)
df$thal <- as.factor(df$thal)


# standardize/normalize the data
standardized.X <- scale(ds[,-14])
set.seed(55)

training.index <- createDataPartition(ds$num, p = .5,list = F)
train.X <- standardized.X[training.index,]
test.X  <- standardized.X[-training.index,]
train.Y <- ds$num[training.index]
test.Y <- ds$num[-training.index]
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)


table.settings <- list(searching = F, pageLength = 5, bLengthChange = F,
                       bPaginate = F, bInfo = F )


