# Clean the Global Environment
rm(list = ls())
gc()
cat("\014")

# Load packages ####
library(rtweet)
library(ggplot2)
library(quanteda)
library(dplyr)
library(plotly)
library(wordcloud)
library(Polychrome)
library(lubridate)
library(fBasics)
library(tictoc)

# Path for input ####
pathTwitter <- file.path("W:\\Twitter\\data\\")

# Path for output ####
pathFigures <- file.path("W:\\Detecting_emerging_OpRisks\\FiguresTwitter\\")

# Log file ####
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
sink(file = file.path(pathFigures, paste0("Log_ReportTwitterAnalysis_", date, ".Rout")))

# Produce pdf output ####
plot_pdf <- FALSE
ggplot_pdf <- FALSE # TRUE
plotly_pdf <- FALSE # TRUE
features_pdf <- TRUE
plotly_output <- TRUE

# Include titles or not
TitleChart <- FALSE # TRUE # 


# Dates for tweets analysis ####
begin_date_analysis <- "05/05/2023"
end_date_analysis <- "12/07/2023"

# Saving dates for the output files to be considered
# begin_date_calculation <- "2023-11-15 12:50:00"
# end_date_calculation <- "2023-11-27 23:40:00"
begin_date_calculation <- "2024-01-26 10:00:00"
end_date_calculation <- "2024-01-26 23:00:00"

begin_date_analysis <- as.Date(begin_date_analysis, "%d/%m/%Y")
end_date_analysis <- as.Date(end_date_analysis, "%d/%m/%Y")

begin_date_calculation <- as.POSIXct(strptime(begin_date_calculation, "%Y-%m-%d %H:%M:%S"))
end_date_calculation <- as.POSIXct(strptime(end_date_calculation, "%Y-%m-%d %H:%M:%S"))

all_date_analysis <- seq(begin_date_analysis, end_date_analysis, by = 1)
all_date_analysis <- as.character(all_date_analysis)


# Plots of daily tweets by cluster ####
print("Plots of daily tweets by cluster")
tic()

# Selection of relevant files
FileNames_Plot <- list.files(path = pathTwitter, pattern = "TweetsAccounts.Filter")
TweetsDates_Plot_analysis <- substr(FileNames_Plot, start = 23, stop = 32) 
TweetsDates_Plot_calculation <- substr(FileNames_Plot, start = 34, stop = 49) 

TweetsDates_Plot_calculation <- paste0(substr(TweetsDates_Plot_calculation,1,10), " ", substr(TweetsDates_Plot_calculation,12,13),
                                        ":", substr(TweetsDates_Plot_calculation,15,16), ":00 CET")

Plot_Selected_analysis <- (TweetsDates_Plot_analysis >= begin_date_analysis) & (TweetsDates_Plot_analysis <= end_date_analysis)
Plot_Selected_calculation <- (TweetsDates_Plot_calculation >= begin_date_calculation) & (TweetsDates_Plot_calculation <= end_date_calculation)
FileNames_Plot_Selected <- FileNames_Plot[Plot_Selected_analysis & Plot_Selected_calculation]

load(file = file.path(pathTwitter, FileNames_Plot_Selected[1]))
TweetsAccounts.Filter.Plot <- TweetsAccounts.Filter
if(length(FileNames_Plot_Selected)>1){
  for(i in 2:length(FileNames_Plot_Selected)){
    load(file = file.path(pathTwitter, FileNames_Plot_Selected[i]))
    TweetsAccounts.Filter.Plot <- rbind(TweetsAccounts.Filter.Plot, TweetsAccounts.Filter)
  }
}
rm(TweetsAccounts.Filter)

TweetsAccounts.Filter.Plot <- TweetsAccounts.Filter.Plot[which((as.Date(TweetsAccounts.Filter.Plot$created_at) >= begin_date_analysis) &
                                                  (as.Date(TweetsAccounts.Filter.Plot$created_at) <= end_date_analysis)),]

TweetsAccounts.Filter.Plot <- as.data.frame(TweetsAccounts.Filter.Plot)

# Set English dates
# https://stackoverflow.com/questions/39340185/how-to-set-the-default-language-of-date-in-r
Sys.setlocale("LC_TIME", "English")


# Plot of tweets time series
if(begin_date_analysis != end_date_analysis){
  ts_plot(TweetsAccounts.Filter.Plot, "days") +
    labs(title = "Number of tweets per day", x = "Day", y = "Number of tweets") +
    theme_minimal()
  if(plot_pdf || ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    ts_plot(TweetsAccounts.Filter.Plot, "days", color = "blue") +
      labs(title = "Number of tweets per day", x = "Day", y = "Number of tweets") +
      theme_minimal()    
    ggsave(paste0(pathFigures, "Tweets_", begin_date_analysis, 
                  "_", end_date_analysis, "_", date, ".pdf"), 
           width = 16, height = 9, units = "cm")
  }
}

if(TitleChart){
  if(begin_date_analysis != end_date_analysis){
    title_plot <- paste0("Number of tweets from ", begin_date_analysis, " to ", end_date_analysis)
  }else{
    title_plot <- paste0("Number of tweets as of ", begin_date_analysis)
  }
}else{
  title_plot <- NULL
}
  
# Plot_ly of daily tweets
if(begin_date_analysis != end_date_analysis){
  TweetsAccounts.Filter.ByDay <- TweetsAccounts.Filter.Plot %>% 
    group_by(date=as.Date(created_at)) %>% 
    summarize(n = length(id_str))
  plot_tweets <- TweetsAccounts.Filter.ByDay %>% 
    plot_ly(x = ~ date) %>% 
    add_lines(y = ~ n) %>%
    layout(
      xaxis = list(
        title = "Day"),
      yaxis = list(
        title = "Number of tweets"),
      title = title_plot)
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    save_image(plot_tweets, paste0(pathFigures, "Tweets_", begin_date_analysis, 
                                "_", end_date_analysis, "_", date, "_plotly.pdf"))
  }
}
if(plotly_output){
  plot_tweets
}

# Plot of daily tweets by cluster
if(begin_date_analysis != end_date_analysis){
  TweetsAccounts.Filter.Plot %>% 
    group_by(cluster) %>% 
    ts_plot("days") +
    labs(title = "Number of tweets per day", x = "Day", y = "Number of tweets") +
    theme_minimal()
  if(plot_pdf || ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    set.seed(935234)
    Pk = createPalette(length(unique(TweetsAccounts.Filter.Plot$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
                       range = c(10, 45))
    names(Pk) <- NULL
    TweetsAccounts.Filter.Plot %>% 
      group_by(cluster) %>% 
      ts_plot("days") +
      labs(title = "Number of tweets per day", x = "Day", y = "Number of tweets") +
      labs(colour = "") + 
      theme_minimal()  + 
      theme(legend.text = element_text(size = 6)) + 
      scale_colour_manual(values = Pk)   
    ggsave(paste0(pathFigures, "Tweets_cluster_", begin_date_analysis, 
                  "_", end_date_analysis, "_", date, ".pdf"), 
           width = 16, height = 9, units = "cm")
  }
}

if(TitleChart){
  if(begin_date_analysis != end_date_analysis){
    title_plot <- paste0("Number of tweets by cluster from ", begin_date_analysis, " to ", end_date_analysis)
  }else{
    title_plot <- paste0("Number of tweets by cluster as of ", begin_date_analysis)
  }
}else{
  title_plot <- NULL
}

# Plot_ly of daily tweets by cluster
if(begin_date_analysis != end_date_analysis){
  TweetsAccounts.Filter.ByDayCluster <- TweetsAccounts.Filter.Plot %>% 
    group_by(cluster, date=as.Date(created_at)) %>% 
    summarize(n = length(id_str))
  plot_tweets_cluster <- TweetsAccounts.Filter.ByDayCluster %>% 
    plot_ly(x = ~ date) %>% 
    add_lines(y = ~ n, 
              color = ~ factor(cluster)) %>%
    layout(
      xaxis = list(
        title = "Day"),
      yaxis = list(
        title = "Number of tweets"),
      title = title_plot)
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    save_image(plot_tweets_cluster, paste0(pathFigures, "Tweets_cluster_", begin_date_analysis, 
                                   "_", end_date_analysis, "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}
if(plotly_output){
  plot_tweets_cluster
}

toc()


# Plots of daily tweets for each cluster with peaks detection ####
print("Plot_ly of daily tweets for each cluster with peaks detection")
tic()

# Detect peaks 
TweetsAccounts.Filter.Plot$date <- ymd(substr(TweetsAccounts.Filter.Plot$created_at,1,10))

TweetsAccounts.Filter.Day.Cluster <- TweetsAccounts.Filter.Plot %>%
  dplyr::group_by(date, cluster) %>% 
  summarise(AMOUNT = length(id_str))

dim(TweetsAccounts.Filter.Day.Cluster)

# Exclude outliers
outlier.days <- c("2023-05-05", "2023-05-21", "2023-05-27", "2023-06-07", "2023-06-18", "2023-07-09")
TweetsAccounts.Filter.Day.Cluster.NoOutlier <- TweetsAccounts.Filter.Day.Cluster %>%
  dplyr::filter(!date %in% outlier.days)

dim(TweetsAccounts.Filter.Day.Cluster.NoOutlier)


ClusterList <- unique(TweetsAccounts.Filter.Day.Cluster$cluster)

ProbLevel <- 0.95
QuantileLevel <- rep(0, length(ClusterList))
names(QuantileLevel) <- ClusterList

for(i in 1:length(ClusterList)){
  TweetsAccounts.Filter.Day.Cluster.NoOutlier.filter <- TweetsAccounts.Filter.Day.Cluster.NoOutlier %>%
    dplyr::filter(cluster %in% ClusterList[i])
  mu <- mean(TweetsAccounts.Filter.Day.Cluster.NoOutlier.filter$AMOUNT)
  sigma <- sd(TweetsAccounts.Filter.Day.Cluster.NoOutlier.filter$AMOUNT)
  QuantileLevel[i] <- qnorm(ProbLevel, mu, sigma)
  hist(TweetsAccounts.Filter.Day.Cluster.NoOutlier.filter$AMOUNT, main=ClusterList[i], xlab="Number of daily tweets", freq = FALSE)
  title(ClusterList[i])
  ind <- seq(min(TweetsAccounts.Filter.Day.Cluster.NoOutlier.filter$AMOUNT), max(TweetsAccounts.Filter.Day.Cluster.NoOutlier.filter$AMOUNT), len = 1000)
  lines(ind, dnorm(ind, mu, sigma))
  print(ClusterList[i])
  print(fBasics::normalTest(TweetsAccounts.Filter.Day.Cluster.NoOutlier.filter$AMOUNT, method=c("sw"), na.rm=T)) 
}


if(begin_date_analysis != end_date_analysis){
  cluster_set <- sort(unique(TweetsAccounts.Filter.Plot$cluster))
  plot_tweets_cluster <- list()
  for(cluster_ind in 1:length(cluster_set)){
    if(TitleChart){
      title_plot <- paste0("Number of tweets of ", cluster_set[cluster_ind], 
                           " from ", begin_date_analysis, " to ", end_date_analysis)
    }else{
      title_plot <- NULL
    }
    TweetsAccounts.Filter.ByDayCluster <- TweetsAccounts.Filter.Plot %>% 
      group_by(date=as.Date(created_at)) %>% 
      filter(cluster==cluster_set[cluster_ind]) %>%
      summarize(n = length(id_str))
    plot_tweets_cluster[[cluster_ind]] <- TweetsAccounts.Filter.ByDayCluster %>% 
      plot_ly(x = ~ date) %>% 
      add_lines(y = ~ n) %>%
      add_lines(y = QuantileLevel[cluster_ind]) %>%
      layout(
        xaxis = list(
          title = "Day"),
        yaxis = list(
          title = "Number of tweets"),
        title = title_plot,
        showlegend = F) 
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      save_image(plot_tweets_cluster[[cluster_ind]], paste0(pathFigures, "Tweets_cluster_", 
                                                            cluster_set[cluster_ind], "_", 
                                                            date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
  }
  if(plotly_output){
    plot_tweets_cluster
  }
}

toc()



# Daily PCA ####
print("Daily PCA")
tic()

# Selection of relevant files
FileNames_PCA_NoCluster <- list.files(path = pathTwitter, pattern = "df_pca_NoCluster")
TweetsDates_PCA_NoCluster_analysis <- substr(FileNames_PCA_NoCluster, start = 18, stop = 27) 
TweetsDates_PCA_NoCluster_calculation <- substr(FileNames_PCA_NoCluster, start = 29, stop = 44) 

TweetsDates_PCA_NoCluster_calculation <- paste0(substr(TweetsDates_PCA_NoCluster_calculation,1,10), " ", substr(TweetsDates_PCA_NoCluster_calculation,12,13),
                                                ":", substr(TweetsDates_PCA_NoCluster_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_PCA_NoCluster_analysis >= begin_date_analysis) & (TweetsDates_PCA_NoCluster_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_PCA_NoCluster_calculation >= begin_date_calculation) & (TweetsDates_PCA_NoCluster_calculation <= end_date_calculation)
FileNames_PCA_NoCluster_Selected <- FileNames_PCA_NoCluster[Selected_analysis & Selected_calculation]

plot_pca <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_PCA_NoCluster_Selected)){
  load(file = file.path(pathTwitter, FileNames_PCA_NoCluster_Selected[i]))
  if(TitleChart){
    title_plot_pca <- paste0("PCA of tweets as of ", all_date_analysis[i])
  }else{
    title_plot_pca <- NULL
  }
  plot_pca[[i]] <- plot_ly(df_pca, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
    layout(title = title_plot_pca, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "PCA_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    plot(df_pca$x, df_pca$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    title(title_plot_pca)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    ggplot(df_pca, aes(x, y, colour = 6)) +
      geom_point() +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_pca) + 
      theme(legend.position = "none") +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
    ggsave(paste0(pathFigures, "PCA_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf") , 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_pca[[i]], paste0(pathFigures, "PCA_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_pca
}

toc()



# Daily PCA features ####
print("Daily PCA features")
tic()

# Selection of relevant files
FileNames_PCA_features <- list.files(path = pathTwitter, pattern = "df_pca_features")
TweetsDates_PCA_features_analysis <- substr(FileNames_PCA_features, start = 17, stop = 26) 
TweetsDates_PCA_features_calculation <- substr(FileNames_PCA_features, start = 28, stop = 43) 

TweetsDates_PCA_features_calculation <- paste0(substr(TweetsDates_PCA_features_calculation,1,10), " ", substr(TweetsDates_PCA_features_calculation,12,13),
                                               ":", substr(TweetsDates_PCA_features_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_PCA_features_analysis >= begin_date_analysis) & (TweetsDates_PCA_features_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_PCA_features_calculation >= begin_date_calculation) & (TweetsDates_PCA_features_calculation <= end_date_calculation)
FileNames_PCA_features_Selected <- FileNames_PCA_features[Selected_analysis & Selected_calculation]

plot_pca_features <- list()

if(plotly_pdf){
  scope <- kaleido()
}

# For static text in plotly: https://plotly.com/r/text-and-annotations/

for(i in 1:length(FileNames_PCA_features_Selected)){
  load(file = file.path(pathTwitter, FileNames_PCA_features_Selected[i]))
  if(TitleChart){
    title_plot_pca_features <- paste0("PCA terms of tweets as of ", all_date_analysis[i])
  }else{
    title_plot_pca_features <- NULL
  }
  if(features_pdf){
    df_pca_features$id_features_2 <- df_pca_features$id_features
    range_x <- max(df_pca_features$x) - min(df_pca_features$x)
    range_y <- max(df_pca_features$y) - min(df_pca_features$y)
    range_coef <- 0.05
    higher_x <- range_x * range_coef
    lower_x <- -higher_x
    higher_y <- range_y * range_coef
    lower_y <- -higher_y
    df_pca_features$id_features_2[((df_pca_features$x < higher_x) & (df_pca_features$x > lower_x)) &
                                    ((df_pca_features$y < higher_y) & (df_pca_features$y > lower_y))] <- ""
    # Text and markers
    # https://stackoverflow.com/questions/62643187/add-text-labels-to-on-top-of-markers-multiple-traces-in-r-plotly
    plot_pca_features[[i]] <- plot_ly(df_pca_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
      add_text(text = ~id_features_2, textposition = "top center", showlegend = F) %>%
      layout(title = title_plot_pca_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  }else{
    # title_plot_pca_features <- paste0("PCA terms of tweets as of ", all_date_analysis[i])
    plot_pca_features[[i]] <- plot_ly(df_pca_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
      layout(title = title_plot_pca_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  }
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "PCA_features_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    plot(df_pca_features$x, df_pca_features$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    title(title_plot_pca_features)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    df_pca_features$id_features_2 <- df_pca_features$id_features
    range_x <- max(df_pca_features$x) - min(df_pca_features$x)
    range_y <- max(df_pca_features$y) - min(df_pca_features$y)
    range_coef <- 0.05
    higher_x <- range_x * range_coef
    lower_x <- -higher_x
    higher_y <- range_y * range_coef
    lower_y <- -higher_y
    df_pca_features$id_features_2[((df_pca_features$x < higher_x) & (df_pca_features$x > lower_x)) &
                                    ((df_pca_features$y < higher_y) & (df_pca_features$y > lower_y))] <- ""
    ggplot(df_pca_features, aes(x, y, colour = 6)) +
      geom_point() +
      geom_text(label = df_pca_features$id_features_2, color = "black") +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_pca_features) + 
      theme(legend.position = "none") +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
    ggsave(paste0(pathFigures, "PCA_features_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_pca_features[[i]], paste0(pathFigures, "PCA_features_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_pca_features
}

toc()
  


# Daily PCA by cluster ####
print("Daily PCA by cluster")
tic()

# Selection of relevant files
FileNames_PCA_Cluster <- list.files(path = pathTwitter, pattern = "df_pca_2")
TweetsDates_PCA_Cluster_analysis <- substr(FileNames_PCA_Cluster, start = 8, stop = 17) 
TweetsDates_PCA_Cluster_calculation <- substr(FileNames_PCA_Cluster, start = 19, stop = 34) 

TweetsDates_PCA_Cluster_calculation <- paste0(substr(TweetsDates_PCA_Cluster_calculation,1,10), " ", substr(TweetsDates_PCA_Cluster_calculation,12,13),
                                              ":", substr(TweetsDates_PCA_Cluster_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_PCA_Cluster_analysis >= begin_date_analysis) & (TweetsDates_PCA_Cluster_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_PCA_Cluster_calculation >= begin_date_calculation) & (TweetsDates_PCA_Cluster_calculation <= end_date_calculation)
FileNames_PCA_Cluster_Selected <- FileNames_PCA_Cluster[Selected_analysis & Selected_calculation]

plot_pca <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_PCA_Cluster_Selected)){
  load(file = file.path(pathTwitter, FileNames_PCA_Cluster_Selected[i]))
  if(TitleChart){
    title_plot_pca <- paste0("PCA of tweets as of ", all_date_analysis[i])
  }else{
    title_plot_pca <- NULL
  }
  plot_pca[[i]] <- plot_ly(df_pca, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers",
                           color = ~cluster) %>%
    layout(title = title_plot_pca, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "PCA_cluster_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    col_plot <- as.numeric(as.factor(df_pca$cluster))+1
    plot(df_pca$x, df_pca$y, xlab = "V1", ylab = "V2", pch = 16, 
         col = col_plot)
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    legend_pos <- c("topright")
    legend(legend_pos,
           legend=sort(unique(df_pca$cluster,)),
           col=sort(unique(col_plot)), pch=16, cex=0.8)
    title(title_plot_pca)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    set.seed(935234)
    Pk = createPalette(length(unique(df_pca$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
                       range = c(10, 45))
    names(Pk) <- NULL
    ggplot(df_pca, aes(x, y, colour = cluster)) +
      geom_point() +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_pca) + 
      theme(legend.text = element_text(size = 6)) +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5) + 
      scale_colour_manual(values = Pk)
    ggsave(paste0(pathFigures, "PCA_cluster_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_pca[[i]], paste0(pathFigures, "PCA_cluster_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_pca
}

toc()



# Daily LSA ####
print("Daily LSA")
tic()

# Selection of relevant files
FileNames_LSA_NoCluster <- list.files(path = pathTwitter, pattern = "df_lsa_NoCluster")
TweetsDates_LSA_NoCluster_analysis <- substr(FileNames_LSA_NoCluster, start = 18, stop = 27) 
TweetsDates_LSA_NoCluster_calculation <- substr(FileNames_LSA_NoCluster, start = 29, stop = 44) 

TweetsDates_LSA_NoCluster_calculation <- paste0(substr(TweetsDates_LSA_NoCluster_calculation,1,10), " ", substr(TweetsDates_LSA_NoCluster_calculation,12,13),
                                                ":", substr(TweetsDates_LSA_NoCluster_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_LSA_NoCluster_analysis >= begin_date_analysis) & (TweetsDates_LSA_NoCluster_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_LSA_NoCluster_calculation >= begin_date_calculation) & (TweetsDates_LSA_NoCluster_calculation <= end_date_calculation)
FileNames_LSA_NoCluster_Selected <- FileNames_LSA_NoCluster[Selected_analysis & Selected_calculation]

plot_lsa <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_NoCluster_Selected)){
  load(file = file.path(pathTwitter, FileNames_LSA_NoCluster_Selected[i]))
  if(TitleChart){
    title_plot_lsa <- paste0("LSA of tweets as of ", all_date_analysis[i])
  }else{
    title_plot_lsa <- NULL
  }
  plot_lsa[[i]] <- plot_ly(df_lsa, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
    layout(title = title_plot_lsa, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "PCA_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    plot(df_lsa$x, df_lsa$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    title(title_plot_lsa)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    ggplot(df_lsa, aes(x, y, colour = 6)) +
      geom_point() +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_pca) + 
      theme(legend.position = "none") +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
    ggsave(paste0(pathFigures, "LSA_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_lsa[[i]], paste0(pathFigures, "LSA_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_lsa
}

toc()



# Daily LSA features ####
print("Daily LSA features")
tic()

# Selection of relevant files
FileNames_LSA_features <- list.files(path = pathTwitter, pattern = "df_lsa_features")
TweetsDates_LSA_features_analysis <- substr(FileNames_LSA_features, start = 17, stop = 26) 
TweetsDates_LSA_features_calculation <- substr(FileNames_LSA_features, start = 28, stop = 43) 

TweetsDates_LSA_features_calculation <- paste0(substr(TweetsDates_LSA_features_calculation,1,10), " ", substr(TweetsDates_LSA_features_calculation,12,13),
                                                ":", substr(TweetsDates_LSA_features_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_LSA_features_analysis >= begin_date_analysis) & (TweetsDates_LSA_features_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_LSA_features_calculation >= begin_date_calculation) & (TweetsDates_LSA_features_calculation <= end_date_calculation)
FileNames_LSA_features_Selected <- FileNames_LSA_features[Selected_analysis & Selected_calculation]

plot_lsa_features <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_features_Selected)){
  load(file = file.path(pathTwitter, FileNames_LSA_features_Selected[i]))
  if(TitleChart){
    title_plot_lsa_features <- paste0("LSA terms of tweets as of ", all_date_analysis[i])  
  }else{
    title_plot_lsa_features <- NULL
  }
  plot_lsa_features[[i]] <- plot_ly(df_lsa_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
    layout(title = title_plot_lsa_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "LSA_features_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    plot(df_lsa_features$x, df_lsa_features$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    title(title_plot_lsa_features)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    df_lsa_features$id_features_2 <- df_lsa_features$id_features
    range_x <- max(df_lsa_features$x) - min(df_lsa_features$x)
    range_y <- max(df_lsa_features$y) - min(df_lsa_features$y)
    range_coef <- 0.05
    higher_x <- range_x * range_coef
    lower_x <- -higher_x
    higher_y <- range_y * range_coef
    lower_y <- -higher_y
    df_lsa_features$id_features_2[((df_lsa_features$x < higher_x) & (df_lsa_features$x > lower_x)) &
                                    ((df_lsa_features$y < higher_y) & (df_lsa_features$y > lower_y))] <- ""
    ggplot(df_lsa_features, aes(x, y, colour = 6)) +
      geom_point() +
      geom_text(label = df_lsa_features$id_features_2, color = "black") +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_lsa_features) + 
      theme(legend.position = "none") +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
    ggsave(paste0(pathFigures, "LSA_features_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_lsa_features[[i]], paste0(pathFigures, "LSA_features_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_lsa_features
}

toc()



# Daily LSA by cluster ####
print("Daily LSA by cluster")
tic()

# Selection of relevant files
FileNames_LSA_Cluster <- list.files(path = pathTwitter, pattern = "df_lsa_2")
TweetsDates_LSA_Cluster_analysis <- substr(FileNames_LSA_Cluster, start = 8, stop = 17) 
TweetsDates_LSA_Cluster_calculation <- substr(FileNames_LSA_Cluster, start = 19, stop = 34) 

TweetsDates_LSA_Cluster_calculation <- paste0(substr(TweetsDates_LSA_Cluster_calculation,1,10), " ", substr(TweetsDates_LSA_Cluster_calculation,12,13),
                                              ":", substr(TweetsDates_LSA_Cluster_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_LSA_Cluster_analysis >= begin_date_analysis) & (TweetsDates_LSA_Cluster_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_LSA_Cluster_calculation >= begin_date_calculation) & (TweetsDates_LSA_Cluster_calculation <= end_date_calculation)
FileNames_LSA_Cluster_Selected <- FileNames_LSA_Cluster[Selected_analysis & Selected_calculation]

plot_lsa <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_Cluster_Selected)){
  load(file = file.path(pathTwitter, FileNames_LSA_Cluster_Selected[i]))
  if(TitleChart){
    title_plot_lsa <- paste0("LSA of tweets as of ", all_date_analysis[i])
  }else{
    title_plot_lsa <- NULL
  }
  plot_lsa[[i]] <- plot_ly(df_lsa, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers",
                           color = ~cluster) %>%
    layout(title = title_plot_lsa, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "LSA_cluster_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    col_plot <- as.numeric(as.factor(df_lsa$cluster))+1
    plot(df_lsa$x, df_lsa$y, xlab = "V1", ylab = "V2", pch = 16, 
         col = col_plot)
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    legend_pos <- c("topright")
    legend(legend_pos,
           legend=sort(unique(df_lsa$cluster,)),
           col=sort(unique(col_plot)), pch=16, cex=0.8)
    title(title_plot_lsa)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    set.seed(935234)
    Pk = createPalette(length(unique(df_lsa$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
                       range = c(10, 45))
    names(Pk) <- NULL
    ggplot(df_lsa, aes(x, y, colour = cluster)) +
      geom_point() +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_lsa) + 
      theme(legend.text = element_text(size = 6)) +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5) + 
      scale_colour_manual(values = Pk)
    ggsave(paste0(pathFigures, "LSA_cluster_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_lsa[[i]], paste0(pathFigures, "LSA_cluster_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_lsa
}

toc()



# Daily UMAP ####
print("Daily UMAP")
tic()

# Selection of relevant files
FileNames_UMAP_NoCluster <- list.files(path = pathTwitter, pattern = "df_umap_NoCluster")
TweetsDates_UMAP_NoCluster_analysis <- substr(FileNames_UMAP_NoCluster, start = 19, stop = 28) 
TweetsDates_UMAP_NoCluster_calculation <- substr(FileNames_UMAP_NoCluster, start = 30, stop = 45) 

TweetsDates_UMAP_NoCluster_calculation <- paste0(substr(TweetsDates_UMAP_NoCluster_calculation,1,10), " ", substr(TweetsDates_UMAP_NoCluster_calculation,12,13),
                                               ":", substr(TweetsDates_UMAP_NoCluster_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_UMAP_NoCluster_analysis >= begin_date_analysis) & (TweetsDates_UMAP_NoCluster_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_UMAP_NoCluster_calculation >= begin_date_calculation) & (TweetsDates_UMAP_NoCluster_calculation <= end_date_calculation)
FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster[Selected_analysis & Selected_calculation]

plot_umap <- list()

if(plotly_pdf){
  scope <- kaleido()
}

# Problem to save plotly in pdf: error given by "\\U" in the path, and sorted out including "pUMAP"
# https://stackoverflow.com/questions/37400974/error-unicode-error-unicodeescape-codec-cant-decode-bytes-in-position-2-3

for(i in 1:length(FileNames_UMAP_NoCluster_Selected)){
  load(file = file.path(pathTwitter, FileNames_UMAP_NoCluster_Selected[i]))
  if(TitleChart){
    title_plot_umap <- paste0("UMAP of tweets as of ", all_date_analysis[i])  
  }else{
    title_plot_umap <- NULL
  }
  plot_umap[[i]] <- plot_ly(df_umap, x = ~X1, y = ~X2, 
                            text = ~id_desc, 
                            #color = ~cluster, 
                            type = 'scatter', mode = 'markers') %>%
    layout(
      plot_bgcolor = "#e5ecf6",
      xaxis = list(
        title = "V1"),
      yaxis = list(
        title = "V2"),
      title = title_plot_umap)
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "UMAP_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    plot(df_umap$X1, df_umap$X2, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    title(title_plot_umap)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    ggplot(df_umap, aes(X1, X2, colour = 6)) +
      geom_point() +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_umap) + 
      theme(legend.position = "none") +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
    ggsave(paste0(pathFigures, "UMAP_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_umap[[i]], paste0(pathFigures, "pUMAP_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_umap
}

toc()



# Daily UMAP by cluster ####
print("Daily UMAP by cluster")
tic()

# Selection of relevant files
FileNames_UMAP_Cluster <- list.files(path = pathTwitter, pattern = "df_umap_2")
TweetsDates_UMAP_Cluster_analysis <- substr(FileNames_UMAP_Cluster, start = 9, stop = 18) 
TweetsDates_UMAP_Cluster_calculation <- substr(FileNames_UMAP_Cluster, start = 20, stop = 35) 

TweetsDates_UMAP_Cluster_calculation <- paste0(substr(TweetsDates_UMAP_Cluster_calculation,1,10), " ", substr(TweetsDates_UMAP_Cluster_calculation,12,13),
                                       ":", substr(TweetsDates_UMAP_Cluster_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_UMAP_Cluster_analysis >= begin_date_analysis) & (TweetsDates_UMAP_Cluster_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_UMAP_Cluster_calculation >= begin_date_calculation) & (TweetsDates_UMAP_Cluster_calculation <= end_date_calculation)
FileNames_UMAP_Cluster_Selected <- FileNames_UMAP_Cluster[Selected_analysis & Selected_calculation]

plot_umap <- list()

if(plotly_pdf){
  scope <- kaleido()
}

# for(i in 1:length(FileNames_UMAP_Cluster_Selected)){
for(i in 1:length(FileNames_UMAP_Cluster_Selected)){
  load(file = file.path(pathTwitter, FileNames_UMAP_Cluster_Selected[i]))
  if(TitleChart){
    title_plot_umap <- paste0("UMAP of tweets as of ", all_date_analysis[i])  
  }else{
    title_plot_umap <- NULL
  }
  plot_umap[[i]] <- plot_ly(df_umap, x = ~X1, y = ~X2, 
                       text = ~id_desc, 
                       color = ~cluster, 
                       type = 'scatter', mode = 'markers') %>%
    layout(
      plot_bgcolor = "#e5ecf6",
      xaxis = list(
        title = "V1"),
      yaxis = list(
        title = "V2"),
      title = title_plot_umap)
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "UMAP_cluster_Tweets_", all_date_analysis[i], "_", date, ".pdf"), width=11)
    col_plot <- as.numeric(as.factor(df_umap$cluster))+1
    plot(df_umap$X1, df_umap$X2, xlab = "V1", ylab = "V2", pch = 16, 
         col = col_plot)
    grid()
    abline(h=0, lty=3)
    abline(v=0, lty=3)
    legend_pos <- c("topright")
    legend(legend_pos, 
           legend=sort(unique(df_umap$cluster,)),
           col=sort(unique(col_plot)), pch=16, cex=0.8)
    title(title_plot_umap)
    dev.off()
  }
  if(ggplot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    set.seed(935234)
    Pk = createPalette(length(unique(df_umap$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
                       range = c(10, 45))
    names(Pk) <- NULL
    ggplot(df_umap, aes(X1, X2, colour = cluster)) +
      geom_point() +
      labs(colour = "") + 
      labs(x = "V1", y = "V2", title = title_plot_umap) + 
      theme(legend.text = element_text(size = 6)) +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5) + 
      scale_colour_manual(values = Pk)
    ggsave(paste0(pathFigures, "UMAP_cluster_Tweets_", all_date_analysis[i], "_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_umap[[i]], paste0(pathFigures, "pUMAP_cluster_Tweets_", all_date_analysis[i], "_", date, "_plotly.pdf"), 
               width = 800, height = 450, scale = 1)
  }
}

if(plotly_pdf){
  rm(scope)
  gc()
}

if(plotly_output){
  plot_umap
}

toc()
  


# Daily wordcloud by topic ####
print("Daily wordcloud by topic")
tic()

# Selection of relevant files
FileNames_wordcloud_topic <- list.files(path = pathTwitter, pattern = "ProbWords")
TweetsDates_wordcloud_topic_analysis <- substr(FileNames_wordcloud_topic, start = 11, stop = 20) 
TweetsDates_wordcloud_topic_calculation <- substr(FileNames_wordcloud_topic, start = 22, stop = 37) 

TweetsDates_wordcloud_topic_calculation <- paste0(substr(TweetsDates_wordcloud_topic_calculation,1,10), " ", substr(TweetsDates_wordcloud_topic_calculation,12,13),
                                                  ":", substr(TweetsDates_wordcloud_topic_calculation,15,16), ":00 CET")

Selected_analysis <- (TweetsDates_wordcloud_topic_analysis >= begin_date_analysis) & (TweetsDates_wordcloud_topic_analysis <= end_date_analysis)
Selected_calculation <- (TweetsDates_wordcloud_topic_calculation >= begin_date_calculation) & (TweetsDates_wordcloud_topic_calculation <= end_date_calculation)
FileNames_wordcloud_topic_Selected <- FileNames_wordcloud_topic[Selected_analysis & Selected_calculation]

# Load seeded words
load(file = file.path(pathTwitter, "seeded_words_2023-11-12_20-19.RData"))

# Number of residual topics
extra_topics <- 5

# Total number of topics (seeded and non-seeded)
k <- length(unique(seeded_words$topic)) + extra_topics

# Seeded topics
topic_vector <- as.character(unique(seeded_words$topic))
# Seeded and unseeded topics
if(extra_topics > 0){
  topic_vector <- c(topic_vector, paste0("Other_",formatC(1:(k - length(topic_vector)) , width=2, flag="0") ))
}

def.par <- par(no.readonly = TRUE)
for(i in 1:length(FileNames_wordcloud_topic_Selected)){
  load(file = file.path(pathTwitter, FileNames_wordcloud_topic_Selected[i]))
  for (topic in 1:k) {
    df <- data.frame(term = row.names(ProbWords), p = ProbWords[, topic])
    head(df[order(-df$p), ])
    if(TitleChart){
      layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
      par(mar = rep(0, 4))
      plot.new()
      text(x = 0.5,
           y = 0.5,
           paste0(topic_vector[topic], " as of ", all_date_analysis[i])) 
    }
    wordcloud(
      words = df$term,
      freq = df$p,
      scale = c(2, .5),
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8, "Dark2")
    )
    if(plot_pdf || ggplot_pdf || plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "Wordcloud_Tweets_", all_date_analysis[i], "_", paste0(topic_vector[topic]), 
                 "_", date, ".pdf"), width=11)
      if(TitleChart){
        layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
        par(mar = rep(0, 4))
        plot.new()
        text(x = 0.5,
             y = 0.5,
             paste0(topic_vector[topic], " as of ", all_date_analysis[i])) 
      }
      wordcloud(
        words = df$term,
        freq = df$p,
        scale = c(2, .5),
        max.words = 200,
        random.order = FALSE,
        rot.per = 0.35,
        colors = brewer.pal(8, "Dark2")
      )
      dev.off()
    }
  }
}
par(def.par) 

toc()


print("End of calculation")

sink()

