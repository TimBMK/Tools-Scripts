## Mass Granger Testing Function (with lag auto-selection) ##
#############################################################

# includes a function to auto-select optimal lag based on different metrics through VARselect(), 
# as suggested here: https://www.researchgate.net/post/optimal_order_in_Granger_test_in_R


#### Function ####

mass_granger <- function(data, group, var1, var2, select = c("AIC", "HQ", "SC", "FPE", NULL), 
                         type = c("const", "trend", "both", "none"), order = NULL, lag.max = 10,
                         significance = c(NULL, 0.05, 0.01, 0.001), output = c("print", "dataframe"),
                         verbose = T){
  
  # var1 = Predictor
  # var2 = Null Hypothesis / Dependant Variable
  
  require(lmtest)
  require(vars)
  require(data.table)
  require(dplyr)
  
  
  # dataframe for output (if needed)
  if (output == "dataframe") {
    return <- data.frame()
  }
  
  # loop through tests
  for (i in 1:nrow(unique(dplyr::select(data, !!as.name(group))))) {
    
    value <- unique(dplyr::select(data, !!as.name(group)))[[1]][i]
    
    sub <- data %>% filter(!!as.name(group) == value) %>% ungroup() %>% dplyr::select(!!as.name(var1), !!as.name(var2))
    
    if (verbose == T) {
      cat(paste("Running Analysis for value:", value, "\n"))
    }
    
    # auto-select optimal lag order
    if (is.null(select) && is.null(order)){
      stop("Specify either a select condition or a lag order.\n")
    }
    
    if (!is.null(select) && is.null(order)) {
      
      selector <- VARselect(sub, lag.max = lag.max, type = type, season = NULL, exogen = NULL)
      
      if (select == "AIC") {
        lag <- selector$selection[[1]]
      }
      if (select == "HQ") {
        lag <- selector$selection[[2]]
      }
      if (select == "SC") {
        lag <- selector$selection[[3]]
      }
      if (select == "FPE") {
        lag <- selector$selection[[4]]
      }
      
      if (verbose == T) {
        cat(paste("Selecting lag of", lag, "based on", select, ".\n"))
      }
      
      if (is.null(select)) {
        lag <- order
        if (verbose == T) {
          cat(paste("Lag of", order, "selected.\n"))}
      }
      
    } else {
      lag <- order
      }
    
    
    # granger test
    granger <- grangertest(sub[,var1], sub[,var2], order = lag)
    
    # filter by significance (if needed)
    if (!is.null(significance)) {
      
      if (granger$`Pr(>F)`[2] <= significance) {
        
        if (output == "print") {
          print(granger)
        } else {
          
          if (output == "dataframe") {
            result <- as.data.frame(granger[2,])
            result <- result %>% mutate(!!group := value, lag = lag)
            return <- rbindlist(list(return, result))
          }
        }
      } else { if (verbose == T) {cat("Not signifcant.\n")}}
      
    } else {
      
      if (output == "print") {
        print(granger)
      } else {
        
        if (output == "dataframe") {
          result <- as.data.frame(granger[2,])
          result <- result %>% mutate(!!group := value, lag = lag)
          return <- rbindlist(list(return, result))
        }
      }
    }
    if (verbose == T) {cat("\n")}
  }
  
  
  if (output == "dataframe") {
    return(return)
  }
}


#### Example ####

# # Example data on the (weighted) indegree of trending words on twitter and news articles
# load("D:/academicCloud/R/EPINetz/DVPW '21/dvpw_twitter_macds_tf.RDa")
# load("D:/academicCloud/R/EPINetz/DVPW '21/dvpw_news_macds_tf.RDa")
# 
# # data is assumed to be in one dataframe and ordered by date/day/timeslot
# data <- left_join(macds_tf[, c("node", "degree_tf", "day")], 
#                   macds_tf_news[, c("node", "degree_tf", "day")], by = c("node", "day"))
# 
# data <- data %>% rename(degree_twitter = degree_tf.x, degree_news = degree_tf.y)
# 
# 
# # make sure observations exist for all grouping values in both dataframes (drop all missing)
# data <- data[complete.cases(data),]
# 
# 
# # run function
# granger_results_twitter <- mass_granger(data = data, group = "node", var1 = "degree_twitter", var2 = "degree_news", 
#                                         select = "AIC", type = "const", lag.max = 10, significance = NULL, output = "dataframe", verbose = F)
# 
# # filter for significance (can also be down by picking a significance value in the function)
# significant_twitter <- granger_results_twitter %>% filter(`Pr(>F)` <= 0.05)
#   # significant predictions for twitter through news
# 
# 
# # you might want to run this with the variables the other way round to outrule reverse causality
# granger_results_news <- mass_granger(data = data, group = "node", var1 = "degree_news", var2 = "degree_twitter", 
#                                      select = "AIC", type = "const", lag.max = 10, significance = NULL, output = "dataframe", verbose = F)
# 
# significant_news <- granger_results_news %>% filter(`Pr(>F)` <= 0.05)
# # significant predictions for news through twitter
# 
# 
# # overlap (reverse casuality)
# significant_news[significant_news$node %in% significant_twitter$node,]
# 
# # no reverse causality
# no_reverse <- significant_news[!(significant_news$node %in% significant_twitter$node),]
# 
# # lags are equal for both samples
# check <- left_join(granger_results_news, granger_results_twitter, by = "node")
# check[check$lag.x != check$lag.y,]




