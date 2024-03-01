library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(ROCR)
library(knitr)
library(tibble)
library(plotly)
library(lime)
library(rpart)
library(rpart.plot)
library(dendextend)
library(vtreat)
library(class)
library(grDevices)
library(factoextra)
library(fpc)

#import data
YT <- read.csv("./youtube_UTF_8.csv", header = TRUE, encoding = "UTF-8")

# data cleaning 
YT[YT == ""] <- NaN
YT <- YT %>%
  mutate(
    allunemployment = (Unemployment.rate / 100) * Population,
    unemployment_urban = (Unemployment.rate / 100) * Urban_population,
    educationall = (Gross.tertiary.education.enrollment.... / 100) * Population,
    educationurban = (Gross.tertiary.education.enrollment.... / 100) * Urban_population,
    averagemonthearning = log((lowest_yearly_earnings+ highest_yearly_earnings) / 2),
    averageyearearning = log((lowest_monthly_earnings+ highest_monthly_earnings) / 2))
YT$created_year <- as.character(YT$created_year)
YT$created_date <- as.character(YT$created_date)
YT[] <- lapply(YT, function(x) {
  if (is.character(x)) {
    x[x == "nan"] <- "missing"
  }
  return(x)
})

YT <- YT[!(rowSums(is.na(YT[, -c(1, 2)])) == ncol(YT) - 2), ]

median_re <- function(YT) {
  nucol <- setdiff(names(YT), c("created_year", "created_date", "rank"))
  
  for (col in nucol) {
    if (is.numeric(YT[[col]])) {
      YT[[col]][is.na(YT[[col]])] <- median(YT[[col]], na.rm = TRUE)
    }
  }
  return(YT)
}
YT <-  median_re(YT)
YT <- subset(YT, select = -c(Youtuber, Longitude, Latitude, Title, country_rank, Abbreviation, channel_type_rank,Gross.tertiary.education.enrollment....,Unemployment.rate,video_views_rank,highest_yearly_earnings,lowest_monthly_earnings,highest_monthly_earnings,lowest_yearly_earnings))

## Target Variable
YT <- YT %>%
  mutate(subscribersup = cut(subscribers, breaks = c(0, 17700000, Inf), include.lowest=T, right=F)) %>% 
  filter(!is.na(subscribersup))
table(YT$subscribersup)
YT$subscribersupbool <- YT$subscribers >= median(YT$subscribers)
target <- 'subscribersup'
pos.label <- "[1.77e+07,Inf]"
YT <- YT %>%
  select(-subscribers)

## Feature Variable
# Identify categorical and numerical variables
YTclass <- setdiff(colnames(YT), c(target,'subscribersup'))

YTCV <- YT %>%
  select(all_of(YTclass)) %>%
  select_if(~ is.factor(.) || is.character(.)) %>%
  names()

YTNV <- YT %>%
  select(all_of(YTclass)) %>%
  select_if(~ is.numeric(.)) %>%
  names()

#convert to numerical
for (cV in YTCV) {
  catcount <- table(YT[, cV])
  countcv <- paste("Numerical", cV, sep = '_')
  YT[countcv] <- catcount[match(YT[, cV], names(catcount))]
}

YTnewclass <- setdiff(colnames(YT), target)
YTCV <- YT %>%
  select(all_of(YTnewclass)) %>%
  select_if(~ is.factor(.) || is.character(.)) %>%
  names()

YTNV <- YT %>%
  select(all_of(YTnewclass)) %>%
  select_if(~ is.numeric(.)) %>%
  names()

YTNVdata <- YT[sapply(YT, is.numeric) | sapply(YT, is.integer)]
YTCVdata <- YT[sapply(YT, is.factor) | sapply(YT, is.character)]

## Training and testing model
YTsaperate <- runif(nrow(YT))
YTtrain <- YT[YTsaperate <= 0.6, ]
YTCal <- YT[YTsaperate > 0.6 & YTsaperate <= 0.8, ]
YTtest <- YT[YTsaperate > 0.8, ]


## Null model 
YTNullm <- sum(YT$subscribersupbool) / nrow(YTtrain)

### AUC and Log Likelyhood
YTAUC <- function(predcol, outcol, pos=pos.label) {
  perf <- performance(prediction(predcol, outcol==pos),'auc')
  as.numeric(perf@y.values)
}

YTlog <- function(outCol, predCol, pos=pos.label) {
  sum(ifelse(outCol==pos, log(predCol), log(1-predCol)))
}

baseRateCheck <- YTlog(YTCal[,target],  sum(YTCal[,target]==pos.label)/length(YTCal[,target]) )

## Single Variable
mkPredC <- function(outCol,varCol,appCol, pos=pos.label) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T)))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  mkPredC(outCol, varC, appC)
}

# list all the variables
AUCYTNV <- data.frame(Pred = character(), Type = character(), TrainAUC = numeric(), CalAUC = numeric())
for (v in YTNV) {
  predii <- paste('pred', v, sep = '.')
  YTtrain[, predii] <- mkPredN(YTtrain[, target], YTtrain[, v], YTtrain[, v])
  YTtest[, predii] <- mkPredN(YTtrain[, target], YTtrain[, v], YTtest[, v])
  YTCal[, predii] <- mkPredN(YTtrain[, target], YTtrain[, v], YTCal[, v])
  trainsetAUC <- YTAUC(YTtrain[, predii], YTtrain[, target])
  
  if (trainsetAUC >= 0.5) {
    CalsetAUC <- YTAUC(YTCal[, predii], YTCal[, target])
    AUCYTNV <- rbind(AUCYTNV, data.frame(Predictor = predii, Type = 'univariate', TrainAUC = trainsetAUC, CalAUC = CalsetAUC))
  }
}

# Remove rows where Predictor is "pred of subscribers"
AUCYTNV <- AUCYTNV %>%
  filter(!grepl("pred of subscribers", Predictor)) %>%
  mutate(Differ = abs(TrainAUC - CalAUC)) %>%
  arrange(Differ) %>%
  select(-Differ) 

# select feature variables for log and coefficient 
selPredVars <- character(0)
selVars <- character(0)
minStep <- baseRateCheck
YTlogNV <- data.frame(Pred = character(0), 'Log likelihood' = numeric(0))
for (v in YTNV) {
  predii <- paste('pred', v, sep = '.')
  likehoodlog <- sum(ifelse(YTCal[, target] == pos.label, log(YTCal[, predii]), log(1 - YTCal[, predii])))
  if (likehoodlog > minStep) {
    selPredVars <- c(selPredVars, predii)
    selVars <- c(selVars, v)
    YTlogNV <- rbind(YTlogNV, data.frame(Pred = predii, loglike = likehoodlog))
  }
}

cormap <- YTNVdata %>%
  cbind(subscriberupbool = YT$subscribersupbool) %>%
  cor() %>%
  as.table() %>%
  as.data.frame() %>%
  filter(Var1 == "subscriberupbool") %>%
  filter(Var2 != "subscriberupbool") %>%
  filter(Var2 != "subscribers") %>%
  distinct(Var2, .keep_all = TRUE) %>%
  filter(Freq >= 0.05)


select_variable_corelation <- as.character(cormap$Var2)
select_variable_ODDcorelation <- as.character(cormap$Var2)[seq(1, length(as.character(cormap$Var2)), by = 2)]
select_variable_EVENcorelation <- as.character(cormap$Var2)[seq(2, length(as.character(cormap$Var2)), by = 2)]
select_variable_log <- as.character(YTlogNV$Pred)
select_variable_log <- as.character(YTlogNV$Pred)

# models
YTlogicregressionEVENCC <- glm(paste(target, ' ~ ', paste(select_variable_EVENcorelation, collapse=' + '), sep=''), data=YTtrain, family = binomial)
YTlogicregressionlog <- glm(paste(target, ' ~ ', paste(select_variable_log, collapse=' + '), sep=''), data=YTtrain, family = binomial)
YTlogicregressionODDCC <- glm(paste(target, ' ~ ', paste(select_variable_ODDcorelation, collapse=' + '), sep=''), data=YTtrain, family = binomial)
YTEVENCCDT <- rpart(paste(target, ' ~ ', paste(select_variable_EVENcorelation, collapse=' + '), sep=''),data=YTtrain)
YTlogDT <- rpart(paste(target, ' ~ ', paste(select_variable_log, collapse=' + '), sep=''),data=YTtrain)
YTODDCCDT <- rpart(paste(target, ' ~ ', paste(select_variable_ODDcorelation, collapse=' + '), sep=''),data=YTtrain)
YTknntrainevenCC <- knn(YTtrain[select_variable_EVENcorelation], YTtrain[select_variable_EVENcorelation], YTtrain[,target], k=3, prob=T)
YTknnCalevenCC <- knn(YTtrain[select_variable_EVENcorelation], YTCal[select_variable_EVENcorelation], YTtrain[,target], k=3, prob=T)
YTtrainknnProbevenCC <- attributes(YTknntrainevenCC)$prob
YTcalknnProbevenCC <- attributes(YTknnCalevenCC)$prob
YTknntrainlog <- knn(YTtrain[select_variable_log], YTtrain[select_variable_log], YTtrain[,target], k=3, prob=T)
YTknnCal <- knn(YTtrain[select_variable_log], YTCal[select_variable_log], YTtrain[,target], k=3, prob=T)
YTtrainknnProblog <- attributes(YTknntrainlog)$prob
YTcalknnProblog <- attributes(YTknnCal)$prob
YTknntrainODDcc <- knn(YTtrain[select_variable_ODDcorelation], YTtrain[select_variable_ODDcorelation], YTtrain[,target], k=3, prob=T)
YTknnCalODdcc <- knn(YTtrain[select_variable_ODDcorelation], YTCal[select_variable_ODDcorelation], YTtrain[,target], k=3, prob=T)
YTtrainknnProbodCC <- attributes(YTknntrainODDcc)$prob
YTcalknnProbodCC <- attributes(YTknnCalODdcc)$prob

# multivariable model
YTdiffModel <- list(
  list(predmodel = predict(YTlogicregressionEVENCC, newdata = YTtrain, type = 'response'), trueV = YTtrain[, target], namea = "LogisticRegression for evencc"),
  list(predmodel = predict(YTlogicregressionODDCC, newdata = YTtrain, type = 'response'), trueV = YTtrain[, target], namea = "LogisticRegression for ODDcc"),
  list(predmodel = predict(YTlogicregressionEVENCC, newdata = YTtrain, type = 'response'), trueV = YTtrain[, target], namea = "LogisticRegression for evencc"),
  list(predmodel = predict(YTEVENCCDT, newdata = YTtrain)[, pos.label], trueV = YTtrain[, target], namea = "DecisionTree for even CC"),
  list(predmodel = predict(YTlogDT, newdata = YTtrain)[, pos.label], trueV = YTtrain[, target], namea = "DecisionTree for log"),
  list(predmodel = predict(YTODDCCDT, newdata = YTtrain)[, pos.label], trueV = YTtrain[, target], namea = "DecisionTree for odd CC"),
  list(predmodel = YTcalknnProbevenCC, trueV = YTCal[, target], namea = "KNN for even cc"),
  list(predmodel = YTcalknnProblog, trueV = YTCal[, target], namea = "KNN for log"),
  list(predmodel = YTcalknnProbodCC, trueV = YTCal[, target], namea = "KNN for odd cc")
)

YTmodels <- do.call(rbind, lapply(YTdiffModel, as.data.frame))
YTrocG <- list()
for (model_name in unique(YTmodels$namea)) {
  model_subset <- YTmodels[YTmodels$namea == model_name, ]
  perf <- performance(prediction(model_subset$predmodel, model_subset$trueV), "tpr", "fpr")
  YTROCMulti <- data.frame(
    FPR = unlist(perf@x.values),
    TPR = unlist(perf@y.values),
    threshold = unlist(perf@alpha.values),
    namea = model_name
  )
  YTrocG[[model_name]] <- YTROCMulti
}

combined_roc <- do.call(rbind, YTrocG)
# cluster data
YT_cluster <- read.csv("./youtube_UTF_8.csv", header = TRUE, encoding = "UTF-8")
YTNV_cluster <- YT_cluster %>%
  mutate(
    allunemployment = (Unemployment.rate / 100) * Population,
    unemployment_urban = (Unemployment.rate / 100) * Urban_population,
    educationall = (Gross.tertiary.education.enrollment.... / 100) * Population,
    educationurban = (Gross.tertiary.education.enrollment.... / 100) * Urban_population,
    averagemonthearning = (lowest_yearly_earnings+ highest_yearly_earnings) / 2,
    averageyearearning = (lowest_monthly_earnings+ highest_monthly_earnings) / 2
  ) %>%
  select(-highest_monthly_earnings, -lowest_monthly_earnings, -lowest_yearly_earnings, -highest_yearly_earnings, -Gross.tertiary.education.enrollment...., -Unemployment.rate, -video_views_rank, -country_rank, -channel_type_rank, -created_year, -created_date) %>%
  filter(rowSums(is.na(select(., (ncol(.)-9):(ncol(.)-2)))) == 0) %>%
  select_if(is.numeric)  %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
vars.to.use <- colnames(YTNV_cluster)[-2]
scaled_df <- scale(YTNV_cluster[,vars.to.use])
hc <- hclust(dist(scale(YTNV_cluster[,colnames(YTNV_cluster)[-2]]), method = "euclidean"), method = "ward.D2")
wss <- function(cluster) {
  c0 <- colMeans(cluster)
  sum(apply(cluster, 1, function(row) sum((row - c0) ^ 2)))
}
Totalwss <- function(df, labels) {
  sumwss <- 0
  for (i in 1:length(unique(labels))) {
    sumwss <- sumwss + wss(subset(df, labels == i))
  }
  sumwss
}
indexch <- function(df, kmax, data) {
  npts <- nrow(df)
  wss.value <- numeric(kmax)
  wss.value[1] <- wss(df)
  for (k in 2:kmax) {
    labels <- cutree(data, k)
    wss.value[k] <- Totalwss(df, labels)
  }
  B <- (wss(df) - wss.value)/ (0:(kmax - 1))
  w <- wss.value / (npts - 1:kmax)
  data.frame(k = 1:kmax, CH_index = B/w, WSS = wss.value)}
ch_criterion <- indexch(scaled_df, 10, hc)
princ <- prcomp(scaled_df)
project2D <- as.data.frame(predict(princ, newdata = scaled_df)[, 1:2])

convexHullcheck <- function(proj2ddata, groups) {
  do.call(rbind, lapply(unique(groups), function(c) {
    f <- subset(proj2ddata, cluster == c)
    f[chull(f), ]
  }))
}

plot_clusters <- function(number) {
  groups <- cutree(hc, number)
  convexHulldf <<- cbind(
    project2D,
    cluster = as.factor(groups),
    subscriber = YTNV_cluster$subscribers
  )
  convex_hull <<- convexHullcheck(convexHulldf, groups)
  ggplot(convexHulldf, aes(x = PC1, y = PC2)) +
    geom_point(aes(shape = cluster, color = cluster, alpha = 0.1)) +
    geom_polygon(data = convex_hull, aes(group = cluster, fill = cluster), alpha = 0.4, linetype = 0) +
    labs(title = sprintf("k = %d", number)) +
    theme(legend.position = "none") +
    theme_minimal()
}

# create Shiny app dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Machine Learning study"),
  dashboardSidebar(
    
    # create content for the shiny app
    sidebarMenu(id = "tabs",
                menuItem("Single variable Plot", tabName = "plot1", icon = icon("line-chart")),
                menuItem("Multivariable Model", tabName = "plot2", icon = icon("area-chart")),
                menuItem("Clustering", tabName = "plot3", icon = icon("sitemap"))
    )
  ),
  dashboardBody(
    tabItems(
      # create content name in plot 1
      tabItem(tabName = "plot1",
              fluidRow(
                box(title = "Feature Variable Selection", width = 4, 
                    selectInput("Singleselect", "Select a Feature Variable:",
                                choices = unique(AUCYTNV$Pred))),
                box(title = "Distribution Plot", width = 8, plotOutput("plot1_output"))
              )),
      
      # create content name in plot 2
      tabItem(tabName = "plot2",
              fluidRow(
                box(title = "Multivariable Model", width = 4, 
                    checkboxGroupInput("table_area", "Select Earning Periods:",
                                       choices = c("LogisticRegression for evencc", "LogisticRegression for ODDcc", 
                                                   "DecisionTree for even CC", "DecisionTree for log", 
                                                   "DecisionTree for odd CC", "KNN for even cc", 
                                                   "KNN for log", "KNN for odd cc"))),
                box(title = "ROC Curve", width = 8, plotOutput("plot2_output"))
              )),
      
      # create content name in plot 3
      tabItem(tabName = "plot3",
              fluidRow(
                box(title = "Clustering Method", width = 4,
                    selectInput("clustering_method", "Choose a clustering method", 
                                choices = c('Hierarchical Clustering','Kmean Clustering')),
                    conditionalPanel(
                      condition = "input.clustering_method == 'Hierarchical Clustering'",
                      sliderInput("hclustplot", "Hierarchical Clustering", min = 1, max = 10, value = 5)
                    ),
                    conditionalPanel(
                      condition = "input.clustering_method == 'Kmean Clustering'",
                      sliderInput("kmeanplot", "Kmean Clustering", min = 1, max = 10, value = 5)
                    )),
                box(title = "Cluster Plot", width = 8, plotOutput("plot3_output"))
              ))
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$main_tabs, {
    updateTabsetPanel(session, "sidebar_tabs", input$main_tabs)
  })
  
  output$plot1_output <- renderPlot({
    channeltype_selection <- input$Singleselect
    ggplot(YTCal) +
      geom_density(aes(x=.data[[channeltype_selection]], color=as.factor(subscribersup))) +
      labs(color = 'subscribers') +
      theme_minimal()
  })
  
  output$plot2_output <- renderPlot({
    multivariablemodelname <- input$table_area
    multivariablemodel <- combined_roc[combined_roc$namea %in% multivariablemodelname, ]
    ggplot(multivariablemodel, aes(x = FPR, y = TPR, color = namea)) +
      geom_line() +
      geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = "red") + 
      labs(x = "FPR", y = "TPR", title = "ROC Curves for Multiple Models") +
      labs(color = "Model") +
      theme_minimal()
  })
  
  output$plot3_output <- renderPlot({
    if (input$clustering_method == "Hierarchical Clustering") {
      return(plot_clusters(input$hclustplot))
    } else if (input$clustering_method == "Kmean Clustering") {
      fviz_cluster(kmeans(scaled_df, input$kmeanplot), scaled_df)
    }
  })
  
}

shinyApp(ui, server)
