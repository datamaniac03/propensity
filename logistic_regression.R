require(plyr)
require(RPostgreSQL)
require(ROCR)
require(stringr)
require(ggplot2)

source('D:/Users/philipp.deutsch/occ_analytics/funcs.R')

print_contingency <- function(actual, score, threshold) {
# Example data for actual and score:
# head(df)
#   actual     score
# 1      1 0.4925375
# 2      1 0.8900711
# 3      0 0.3160326
# Usage: print_contingency(df$actual, df$score, 0.5)
  
  prediction <- rep(1, length(actual))
  prediction[score <  threshold] <- 0
  
  cont <- data.frame(
    "Predict 0" = table(actual[prediction==0]),
    "Predict 1" = table(actual[prediction==1])
  )
  rownames(cont) <- cont$Predict.0.Var1
  cont$Predict.1.Var1 <- NULL
  cont$Predict.0.Var1 <- NULL
  names(cont) <- c("Predict 0", "Predict 1")
  cont
}

print_woe <- function(df, target_name, var_name) {
  data <- df[,c(target_name, var_name)]
  names(data) <- c("target", "var")
  ret <- with(data, data.frame(
    "Obs" = tapply(target, var, length),
    "Good" = tapply(target, var, sum)))
  ret$Bad <- ret$Obs - ret$Good
  ret$PGood <- round(ret$Good / (ret$Good + ret$Bad),1)
  ret$PBad <- round(ret$Bad / (ret$Good + ret$Bad),1)
  ret$WoE <- round(with(ret, log(PGood / PBad)), 1)
  ret$DGood <- round(ret$Good / sum(ret$Good), 1)
  ret$DBad <-  round(ret$Bad / sum(ret$Bad), 1)
  ret$IV <- with(ret, WoE * (DGood - DBad))
  ret <- ret[order(ret$PGood),]
  print(ret)
  print(paste("Information value:", sum(ret$IV)))
  .e <- environment()
  p <- ggplot(data=ret, environment=.e) +
    geom_bar(aes(x=reorder(rownames(ret), PGood), y=PGood), stat="identity", fill="#002C54") +
    xlab(var_name) + 
    ylab("Percentage of Goods, IV") + 
    guides(fill=FALSE) + theme_bw() + theme(text = element_text(size = 15)) + 
    geom_line(aes(x=as.numeric(reorder(rownames(ret), PGood)), y=IV), colour="#FF9933", lwd=1)
  print(p)
}

clean_variable <- function(x) {
# x a character vector
  x <- str_trim(x)
  x[is.na(x)] <- "Unknown"
  x[x==""] <- "Unknown"
  x <- as.factor(x)
  x
}

bin_variable <- function(x, bins) {
  x <- as.character(cut(x, bins))
  x[is.na(x)] <- "Unknown"
  x <- as.factor(x)
  x
}

if (!exists("df.vars")) {
  print("loading data")
  df.targets <- load_from_db("cac0007", "targets")
  df.vars <- load_from_db("cac0007", "po_vars")
  df <- join(df.vars, df.targets, by="urn")
  
  model_date <- as.Date('2011-07-31')
  df$days_not_cuised <- bin_variable(as.numeric(model_date - df$last_cruise), c(0,90,540,Inf))
  df$avg_olbd_per_cruise <- bin_variable(df$nights_onboard/df$max_cruise,  c(0,3,7,14,21,Inf))
  df$avg_obr_per_cruise <- bin_variable(df$obr/df$max_cruise,  c(0,50,100,Inf))
  
  df$attitude <- clean_variable(df$First_Attitude)
  df$lifestage <- clean_variable(df$First_Lifestage)
  df$segment <- clean_variable(df$First_Segment)∟
  df$meta <- clean_variable(df$First_Meta)
  
  df$first_lead <- bin_variable(df$First_Lead, c(0,30,90,360,Inf))
  df$first_age <- bin_variable(df$First_Age, c(0,50,60,70,80,Inf))
  df$compensation <- bin_variable(df$compensation, c(-Inf,-1,Inf))
  df$max_cruise <- bin_variable(df$max_cruise, c(0,2,4,6,Inf))
  df$cltv <- bin_variable(df$cltv, c(-Inf,0,2500,500,1000,Inf))
}

nas <- data.frame("NAs"=sapply(df, function(x) sum(is.na(x))))
print("The following variables contain missing values:")
print(nas[nas$NAs > 0,,drop=FALSE])

target <- "tg_cruise_again"

# print_woe(df, target, "attitude") 
# print_woe(df, target, "bob_ever")
# print_woe(df, target, "max_cruise") 
# print_woe(df, target, "cltv") 
# print_woe(df, target, "days_not_cuised")
# print_woe(df, target, "avg_olbd_per_cruise") 

# print_woe(df, target, "lifestage") 
# print_woe(df, target, "segment") 
# print_woe(df, target, "first_age") 
# print_woe(df, target, "compensation") 
# print_woe(df, target, "meta") 
# print_woe(df, target, "first_lead") 
# print_woe(df, target, "avg_obr_per_cruise")

vars <- c("attitude",
          "bob_ever",
          "max_cruise",
          "cltv",
          "days_not_cuised",
          "avg_olbd_per_cruise")
str_vars <- paste0(vars[1], paste(" +", vars[2:length(vars)], collapse=""))

fit <- glm(as.formula(paste(target,"~", str_vars)), data = df, family = "binomial")
print(summary(fit))

df.check <- df[,c(target, vars)]
df.check <- cbind(df.check, "score"=predict(fit, df.check, type="response"))
df.check <- df.check[, c(target, "score")]
names(df.check) <- c("actual", "score")

cont <- print_contingency(df.check$actual, df.check$score, 0.5)
print(round(100*cont/sum(cont), digits=1))

pred <- prediction(df.check$score, df.check$actual)
perf <- performance(pred,"tpr","fpr")
plot(perf, lwd=4, downsampling=0.2)
lines(0:1,0:1, col="red")

auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
gini <- 2 * auc -1
print(paste("AUC:  ",round(100*auc,  digits=1),"%", sep=""))
print(paste("Gini: ",round(100*gini, digits=1),"%", sep=""))
