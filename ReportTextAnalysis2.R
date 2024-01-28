# Clean the Global Environment
rm(list = ls())
gc()
cat("\014")

# Load packages ####
library(ggplot2)
library(quanteda)
library(dplyr)
library(plotly)
library(wordcloud)
library(Polychrome)
library(factoextra)
library(tictoc)

# Path for input ####
path <- file.path("W:\\TextAnalysis\\data\\")

# Path for output ####
pathFigures <- file.path("W:\\Detecting_emerging_OpRisks\\Figures\\")

# Log file ####
date <- Sys.time()
date <- as.character(date)
date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
sink(file = file.path(pathFigures, paste0("Log_ReportTextAnalysis2_", date, ".Rout")))

# Produce pdf output ####
plot_pdf <- FALSE
ggplot_pdf <- FALSE
plotly_pdf <- TRUE
features_pdf <- FALSE
plotly_output <- TRUE

# Include titles or not
TitleChart <- FALSE


# Dates for text analysis ####
begin_date_calculation <- "2024-01-24 12:00:00"
end_date_calculation <- "2024-01-25 15:00:00"

begin_date_calculation <- as.POSIXct(strptime(begin_date_calculation, "%Y-%m-%d %H:%M:%S"))
end_date_calculation <- as.POSIXct(strptime(end_date_calculation, "%Y-%m-%d %H:%M:%S"))


# OpData.Filter ####

print("Read OpData.Filter")
tic()
FileNames_OpData.Filter <- list.files(path = path, pattern = "OpData.Filter")
Dates_OpData.Filter <- substr(FileNames_OpData.Filter, start = 15, stop = 30)
Dates_OpData.Filter <- paste0(substr(Dates_OpData.Filter,1,10), " ", substr(Dates_OpData.Filter,12,13),
                                          ":", substr(Dates_OpData.Filter,15,16), ":00 CET")
FileNames_OpData.Filter <- FileNames_OpData.Filter[(Dates_OpData.Filter >= begin_date_calculation) & (Dates_OpData.Filter <= end_date_calculation)]
for(i in 1:length(FileNames_OpData.Filter)){
  load(file = file.path(path, FileNames_OpData.Filter[i]))
}
toc()



# PCA of Group ####

print("PCA of Group")
tic()
# Selection of relevant files
FileNames_PCA_NoCluster <- list.files(path = path, pattern = "df_pca_NoCluster_2")
Dates_PCA_NoCluster_calculation <- substr(FileNames_PCA_NoCluster, start = 18, stop = 33) 

Dates_PCA_NoCluster_calculation <- paste0(substr(Dates_PCA_NoCluster_calculation,1,10), " ", substr(Dates_PCA_NoCluster_calculation,12,13),
                                                ":", substr(Dates_PCA_NoCluster_calculation,15,16), ":00 CET")

FileNames_PCA_NoCluster_Selected <- FileNames_PCA_NoCluster[(Dates_PCA_NoCluster_calculation >= begin_date_calculation) & (Dates_PCA_NoCluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_PCA_NoCluster_Selected <- FileNames_PCA_NoCluster_Selected[length(FileNames_PCA_NoCluster_Selected)]

plot_pca <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_PCA_NoCluster_Selected)){
  load(file = file.path(path, FileNames_PCA_NoCluster_Selected[i]))
  if(TitleChart){
    title_plot_pca <- paste0("PCA of Group")
  }else{
    title_plot_pca <- NULL
  }
  plot_pca[[i]] <- plot_ly(df_pca, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
    layout(title = title_plot_pca, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "PCA_Group_", date, ".pdf"), width=11)
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
    ggsave(paste0(pathFigures, "PCA_Group_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_pca[[i]], paste0(pathFigures, "PCA_Group_", date, "_plotly.pdf"), 
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



# PCA of ETs ####

print("PCA of ETs")
tic()
ET <- sort(unique(OpData.Filter$'Event Type'))

# Selection of relevant files
FileNames_PCA_NoCluster <- list.files(path = path, pattern = "df_pca_NoCluster_ET_")
Dates_PCA_NoCluster_calculation <- substr(FileNames_PCA_NoCluster, start = 21, stop = 36) 

Dates_PCA_NoCluster_calculation <- paste0(substr(Dates_PCA_NoCluster_calculation,1,10), " ", substr(Dates_PCA_NoCluster_calculation,12,13),
                                                ":", substr(Dates_PCA_NoCluster_calculation,15,16), ":00 CET")

FileNames_PCA_NoCluster_Selected <- FileNames_PCA_NoCluster[(Dates_PCA_NoCluster_calculation >= begin_date_calculation) & (Dates_PCA_NoCluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_PCA_NoCluster_Selected <- FileNames_PCA_NoCluster_Selected[length(FileNames_PCA_NoCluster_Selected)]

plot_pca <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_PCA_NoCluster_Selected)){
  load(file = file.path(path, FileNames_PCA_NoCluster_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_pca <- paste0("PCA of ", ET[[j]])  
    }else{
      title_plot_pca <- NULL
    }
    plot_pca[[j]] <- plot_ly(df_pca_ET[[j]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
      layout(title = title_plot_pca, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
    if(plot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "PCA_ET", j, "_", date, ".pdf"), width=11)
      plot(df_pca_ET[[j]]$x, df_pca_ET[[j]]$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
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
      ggplot(df_pca_ET[[j]], aes(x, y, colour = 6)) +
        geom_point() +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_pca) + 
        theme(legend.position = "none") +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
      ggsave(paste0(pathFigures, "PCA_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      scope$transform(plot_pca[[j]], paste0(pathFigures, "PCA_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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



# PCA features of Group ####

print("PCA features of Group")
tic()
# Selection of relevant files
FileNames_PCA_features <- list.files(path = path, pattern = "df_pca_features_2")
Dates_PCA_features_calculation <- substr(FileNames_PCA_features, start = 17, stop = 32) 

Dates_PCA_features_calculation <- paste0(substr(Dates_PCA_features_calculation,1,10), " ", substr(Dates_PCA_features_calculation,12,13),
                                               ":", substr(Dates_PCA_features_calculation,15,16), ":00 CET")

FileNames_PCA_features_Selected <- FileNames_PCA_features[(Dates_PCA_features_calculation >= begin_date_calculation) & (Dates_PCA_features_calculation <= end_date_calculation)]

# Select the last one
FileNames_PCA_features_Selected <- FileNames_PCA_features_Selected[length(FileNames_PCA_features_Selected)]

plot_pca_features <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_PCA_features_Selected)){
  load(file = file.path(path, FileNames_PCA_features_Selected[i]))
  if(TitleChart){
    title_plot_pca_features <- paste0("PCA terms of Group")
  }else{
    title_plot_pca_features <- NULL
  }
  if(features_pdf){
    df_pca_features$id_features_2 <- df_pca_features$id_features
    range_x <- max(df_pca_features$x) - min(df_pca_features$x)
    range_y <- max(df_pca_features$y) - min(df_pca_features$y)
    range_coef <- 0.25
    higher_x <- range_x * range_coef
    lower_x <- -higher_x
    higher_y <- range_y * range_coef
    lower_y <- -higher_y
    df_pca_features$id_features_2[((df_pca_features$x < higher_x) & (df_pca_features$x > lower_x)) &
                                    ((df_pca_features$y < higher_y) & (df_pca_features$y > lower_y))] <- ""
    plot_pca_features[[i]] <- plot_ly(df_pca_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
      add_text(text = ~id_features_2, textposition = "top center", showlegend = F) %>%
      layout(title = title_plot_pca_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  }else{
    plot_pca_features[[i]] <- plot_ly(df_pca_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
    layout(title = title_plot_pca_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  }
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "PCA_features_Group_", date, ".pdf"), width=11)
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
    range_coef <- 0.15
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
    ggsave(paste0(pathFigures, "PCA_features_Group_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_pca_features[[i]], paste0(pathFigures, "PCA_features_Group_", date, "_plotly.pdf"), 
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



# PCA features of ETs ####

print("PCA features of ETs")
tic()
# Selection of relevant files
FileNames_PCA_features <- list.files(path = path, pattern = "df_pca_features_ET_")
Dates_PCA_features_calculation <- substr(FileNames_PCA_features, start = 20, stop = 35) 

Dates_PCA_features_calculation <- paste0(substr(Dates_PCA_features_calculation,1,10), " ", substr(Dates_PCA_features_calculation,12,13),
                                         ":", substr(Dates_PCA_features_calculation,15,16), ":00 CET")

FileNames_PCA_features_Selected <- FileNames_PCA_features[(Dates_PCA_features_calculation >= begin_date_calculation) & (Dates_PCA_features_calculation <= end_date_calculation)]

# Select the last one
FileNames_PCA_features_Selected <- FileNames_PCA_features_Selected[length(FileNames_PCA_features_Selected)]

plot_pca_features <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_PCA_features_Selected)){
  load(file = file.path(path, FileNames_PCA_features_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_pca_features <- paste0("PCA terms of ", ET[[j]])
    }else{
      title_plot_pca_features <- NULL
    }
    if(features_pdf){
      df_pca_features_ET[[j]]$id_features_2 <- df_pca_features_ET[[j]]$id_features
      range_x <- max(df_pca_features_ET[[j]]$x) - min(df_pca_features_ET[[j]]$x)
      range_y <- max(df_pca_features_ET[[j]]$y) - min(df_pca_features_ET[[j]]$y)
      range_coef <- 0.25
      higher_x <- range_x * range_coef
      lower_x <- -higher_x
      higher_y <- range_y * range_coef
      lower_y <- -higher_y
      df_pca_features_ET[[j]]$id_features_2[((df_pca_features_ET[[j]]$x < higher_x) & (df_pca_features_ET[[j]]$x > lower_x)) &
                                      ((df_pca_features_ET[[j]]$y < higher_y) & (df_pca_features_ET[[j]]$y > lower_y))] <- ""
      plot_pca_features[[j]] <- plot_ly(df_pca_features_ET[[j]], x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
        add_text(text = ~id_features_2, textposition = "top center", showlegend = F) %>%
        layout(title = title_plot_pca_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
    }else{
    plot_pca_features[[j]] <- plot_ly(df_pca_features_ET[[j]], x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
      layout(title = title_plot_pca_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
    }
    if(plot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "PCA_features_ET", j, "_", date, ".pdf"), width=11)
      plot(df_pca_features_ET[[j]]$x, df_pca_features_ET[[j]]$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
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
      df_pca_features_ET[[j]]$id_features_2 <- df_pca_features_ET[[j]]$id_features
      range_x <- max(df_pca_features_ET[[j]]$x) - min(df_pca_features_ET[[j]]$x)
      range_y <- max(df_pca_features_ET[[j]]$y) - min(df_pca_features_ET[[j]]$y)
      range_coef <- 0.15
      higher_x <- range_x * range_coef
      lower_x <- -higher_x
      higher_y <- range_y * range_coef
      lower_y <- -higher_y
      df_pca_features_ET[[j]]$id_features_2[((df_pca_features_ET[[j]]$x < higher_x) & (df_pca_features_ET[[j]]$x > lower_x)) &
                                      ((df_pca_features_ET[[j]]$y < higher_y) & (df_pca_features_ET[[j]]$y > lower_y))] <- ""
      ggplot(df_pca_features_ET[[j]], aes(x, y, colour = 6)) +
        geom_point() +
        geom_text(label = df_pca_features_ET[[j]]$id_features_2, color = "black") +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_pca_features) + 
        theme(legend.position = "none") +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
      ggsave(paste0(pathFigures, "PCA_features_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      scope$transform(plot_pca_features[[j]], paste0(pathFigures, "PCA_features_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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



# PCA of ETs by cluster ####

print("PCA of ETs by cluster")
tic()
# Selection of relevant files
FileNames_PCA_Cluster <- list.files(path = path, pattern = "df_pca_ET_cluster_")
Dates_PCA_Cluster_calculation <- substr(FileNames_PCA_Cluster, start = 19, stop = 34) 

Dates_PCA_Cluster_calculation <- paste0(substr(Dates_PCA_Cluster_calculation,1,10), " ", substr(Dates_PCA_Cluster_calculation,12,13),
                                              ":", substr(Dates_PCA_Cluster_calculation,15,16), ":00 CET")

FileNames_PCA_Cluster_Selected <- FileNames_PCA_Cluster[(Dates_PCA_Cluster_calculation >= begin_date_calculation) & (Dates_PCA_Cluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_PCA_Cluster_Selected <- FileNames_PCA_Cluster_Selected[length(FileNames_PCA_Cluster_Selected)]

plot_pca <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_PCA_Cluster_Selected)){
  load(file = file.path(path, FileNames_PCA_Cluster_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_pca <- paste0("PCA of ", ET[[j]])  
    }else{
      title_plot_pca <- NULL
    }
    plot_pca[[j]] <- plot_ly(df_pca_ET_cluster[[j]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers", 
                             color = ~cluster) %>%
      layout(title = title_plot_pca, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
    if(plot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "PCA_cluster_ET", j, "_", date, ".pdf"), width=11)
      col_plot <- as.numeric(as.factor(df_pca_ET_cluster[[j]]$cluster))+1
      # par(mar = c(5, 4, 4, 12),                                  # Specify par parameters
      #     xpd = TRUE)
      plot(df_pca_ET_cluster[[j]]$x, df_pca_ET_cluster[[j]]$y, xlab = "V1", ylab = "V2", pch = 16, 
           col = col_plot)
      grid()
      abline(h=0, lty=3)
      abline(v=0, lty=3)
      legend_pos <- c("topleft", "bottomright", "bottomleft", "topleft", "bottomright", "bottomright", "bottomleft")
      legend(legend_pos[j], # inset = c(-0.5, 0),
             legend=sort(unique(df_pca_ET_cluster[[j]]$cluster,)),
             col=sort(unique(col_plot)), pch=16, cex=0.8)
             # , box.lty=0,
             # bg="transparent")
      title(title_plot_pca)
      dev.off()
    }
    if(ggplot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      set.seed(935234)
      Pk = createPalette(length(unique(df_pca_ET_cluster[[j]]$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
                         range = c(10, 45))
      names(Pk) <- NULL
      ggplot(df_pca_ET_cluster[[j]], aes(x, y, colour = cluster)) +
        geom_point() +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_pca) + 
        theme(legend.text = element_text(size = 6)) +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5) + 
        scale_colour_manual(values = Pk)
      ggsave(paste0(pathFigures, "PCA_cluster_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      scope$transform(plot_pca[[j]], paste0(pathFigures, "PCA_cluster_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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



# Explained var for PCA of ETs ####

print("Explained var for PCA of ETs")
tic()
ET <- sort(unique(OpData.Filter$'Event Type'))

# Selection of relevant files
FileNames_PCA <- list.files(path = path, pattern = "Semantic_pca_ET_")
Dates_PCA_calculation <- substr(FileNames_PCA, start = 17, stop = 32) 

Dates_PCA_calculation <- paste0(substr(Dates_PCA_calculation,1,10), " ", substr(Dates_PCA_calculation,12,13),
                                          ":", substr(Dates_PCA_calculation,15,16), ":00 CET")

FileNames_PCA_Selected <- FileNames_PCA[(Dates_PCA_calculation >= begin_date_calculation) & (Dates_PCA_calculation <= end_date_calculation)]

# Select the last one
FileNames_PCA_Selected <- FileNames_PCA_Selected[length(FileNames_PCA_Selected)]

l_pca_scree <- list()
pca.dim <- 50

for(i in 1:length(FileNames_PCA_Selected)){
  load(file = file.path(path, FileNames_PCA_Selected[i]))
  for(j in 1:length(ET)){
    
    if(TitleChart){
      main_scree <- paste0("Scree plot of ", ET[j])
    }else{
      main_scree <- c("")
    }

    print(paste0("Summary PCA of ", ET[j]))
    print(summary(Semantic_pca_ET[[j]]))
    # https://stackoverflow.com/questions/69490130/in-r-factoextra-when-i-use-function-fviz-eig-how-to-adjust-the-column-width
    l_pca_scree[[j]] <- fviz_eig(Semantic_pca_ET[[j]], ncp = pca.dim, addlabels = FALSE, main = main_scree, 
                                 bar_width = 1) + 
      geom_text(label = round(Semantic_pca_ET[[j]]$sdev^2 / sum(Semantic_pca_ET[[j]]$sdev^2) * 100, 1), vjust = -1, hjust = 0, size = 1.8) +
      theme(text = element_text(size = 5),
            axis.title = element_text(size = 5),
            axis.text = element_text(size = 5))
    
    if(plot_pdf || ggplot_pdf || plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      ggsave(paste0(pathFigures, "PCA_scree_ET", j, "_", date, ".pdf"), 
             width = 16, height = 9, units = "cm")
    }
  }
}

if(plotly_output){
  l_pca_scree
}
toc()



# LSA of Group ####

print("LSA of Group")
tic()
# Selection of relevant files
FileNames_LSA_NoCluster <- list.files(path = path, pattern = "df_lsa_NoCluster_2")
Dates_LSA_NoCluster_calculation <- substr(FileNames_LSA_NoCluster, start = 18, stop = 33) 

Dates_LSA_NoCluster_calculation <- paste0(substr(Dates_LSA_NoCluster_calculation,1,10), " ", substr(Dates_LSA_NoCluster_calculation,12,13),
                                          ":", substr(Dates_LSA_NoCluster_calculation,15,16), ":00 CET")

FileNames_LSA_NoCluster_Selected <- FileNames_LSA_NoCluster[(Dates_LSA_NoCluster_calculation >= begin_date_calculation) & (Dates_LSA_NoCluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_LSA_NoCluster_Selected <- FileNames_LSA_NoCluster_Selected[length(FileNames_LSA_NoCluster_Selected)]

plot_lsa <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_NoCluster_Selected)){
  load(file = file.path(path, FileNames_LSA_NoCluster_Selected[i]))
  if(TitleChart){
    title_plot_lsa <- paste0("LSA of Group")
  }else{
    title_plot_lsa <- NULL
  }
  plot_lsa[[i]] <- plot_ly(df_lsa, x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
    layout(title = title_plot_lsa, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "LSA_Group_", date, ".pdf"), width=11)
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
      labs(x = "V1", y = "V2", title = title_plot_lsa) + 
      theme(legend.position = "none") +
      geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
      geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
    ggsave(paste0(pathFigures, "LSA_Group_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_lsa[[i]], paste0(pathFigures, "LSA_Group_", date, "_plotly.pdf"), 
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



# LSA of ETs ####

print("LSA of ETs")
tic()
ET <- sort(unique(OpData.Filter$'Event Type'))

# Selection of relevant files
FileNames_LSA_NoCluster <- list.files(path = path, pattern = "df_lsa_NoCluster_ET_")
Dates_LSA_NoCluster_calculation <- substr(FileNames_LSA_NoCluster, start = 21, stop = 36) 

Dates_LSA_NoCluster_calculation <- paste0(substr(Dates_LSA_NoCluster_calculation,1,10), " ", substr(Dates_LSA_NoCluster_calculation,12,13),
                                          ":", substr(Dates_LSA_NoCluster_calculation,15,16), ":00 CET")

FileNames_LSA_NoCluster_Selected <- FileNames_LSA_NoCluster[(Dates_LSA_NoCluster_calculation >= begin_date_calculation) & (Dates_LSA_NoCluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_LSA_NoCluster_Selected <- FileNames_LSA_NoCluster_Selected[length(FileNames_LSA_NoCluster_Selected)]

plot_lsa <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_NoCluster_Selected)){
  load(file = file.path(path, FileNames_LSA_NoCluster_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_lsa <- paste0("LSA of ", ET[[j]])  
    }else{
      title_plot_lsa <- NULL
    }
    plot_lsa[[j]] <- plot_ly(df_lsa_ET[[j]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers") %>%
      layout(title = title_plot_lsa, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
    if(plot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "LSA_ET", j, "_", date, ".pdf"), width=11)
      plot(df_lsa_ET[[j]]$x, df_lsa_ET[[j]]$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
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
      ggplot(df_lsa_ET[[j]], aes(x, y, colour = 6)) +
        geom_point() +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_lsa) + 
        theme(legend.position = "none") +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
      ggsave(paste0(pathFigures, "LSA_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      scope$transform(plot_lsa[[j]], paste0(pathFigures, "LSA_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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



# LSA features of Group ####

print("LSA features of Group")
tic()
# Selection of relevant files
FileNames_LSA_features <- list.files(path = path, pattern = "df_lsa_features_2")
Dates_LSA_features_calculation <- substr(FileNames_LSA_features, start = 17, stop = 32) 

Dates_LSA_features_calculation <- paste0(substr(Dates_LSA_features_calculation,1,10), " ", substr(Dates_LSA_features_calculation,12,13),
                                         ":", substr(Dates_LSA_features_calculation,15,16), ":00 CET")

FileNames_LSA_features_Selected <- FileNames_LSA_features[(Dates_LSA_features_calculation >= begin_date_calculation) & (Dates_LSA_features_calculation <= end_date_calculation)]

# Select the last one
FileNames_LSA_features_Selected <- FileNames_LSA_features_Selected[length(FileNames_LSA_features_Selected)]

plot_lsa_features <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_features_Selected)){
  load(file = file.path(path, FileNames_LSA_features_Selected[i]))
  if(TitleChart){
    title_plot_lsa_features <- paste0("LSA terms of Group")
  }else{
    title_plot_lsa_features <- NULL
  }
  if(features_pdf){
    df_lsa_features$id_features_2 <- df_lsa_features$id_features
    range_x <- max(df_lsa_features$x) - min(df_lsa_features$x)
    range_y <- max(df_lsa_features$y) - min(df_lsa_features$y)
    range_coef <- 0.25
    higher_x <- range_x * range_coef
    lower_x <- -higher_x
    higher_y <- range_y * range_coef
    lower_y <- -higher_y
    df_lsa_features$id_features_2[((df_lsa_features$x < higher_x) & (df_lsa_features$x > lower_x)) &
                                    ((df_lsa_features$y < higher_y) & (df_lsa_features$y > lower_y))] <- ""
    plot_lsa_features[[i]] <- plot_ly(df_lsa_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
      add_text(text = ~id_features_2, textposition = "top center", showlegend = F) %>%
      layout(title = title_plot_lsa_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  }else{
    plot_lsa_features[[i]] <- plot_ly(df_lsa_features, x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
      layout(title = title_plot_lsa_features, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
  }
  if(plot_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    pdf(paste0(pathFigures, "LSA_features_Group_", date, ".pdf"), width=11)
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
    range_coef <- 0.15
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
    ggsave(paste0(pathFigures, "LSA_features_Group_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_lsa_features[[i]], paste0(pathFigures, "LSA_features_Group_", date, "_plotly.pdf"), 
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



# LSA features of ETs ####

print("LSA features of ETs")
tic()
# Selection of relevant files
FileNames_LSA_features <- list.files(path = path, pattern = "df_lsa_features_ET_")
Dates_LSA_features_calculation <- substr(FileNames_LSA_features, start = 20, stop = 35) 

Dates_LSA_features_calculation <- paste0(substr(Dates_LSA_features_calculation,1,10), " ", substr(Dates_LSA_features_calculation,12,13),
                                         ":", substr(Dates_LSA_features_calculation,15,16), ":00 CET")

FileNames_LSA_features_Selected <- FileNames_LSA_features[(Dates_LSA_features_calculation >= begin_date_calculation) & (Dates_LSA_features_calculation <= end_date_calculation)]

# Select the last one
FileNames_LSA_features_Selected <- FileNames_LSA_features_Selected[length(FileNames_LSA_features_Selected)]

plot_lsa_features <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_features_Selected)){
  load(file = file.path(path, FileNames_LSA_features_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_lsa_features <- paste0("LSA terms of ", ET[[j]])
    }else{
      title_plot_lsa_features <- NULL
    }
    plot_lsa_features[[j]] <- plot_ly(df_lsa_features_ET[[j]], x = ~x, y = ~y, text = ~id_features, type = "scatter", mode = "markers") %>%
      layout(title = title_plot_lsa, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
    if(plot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "LSA_features_ET", j, "_", date, ".pdf"), width=11)
      plot(df_lsa_features_ET[[j]]$x, df_lsa_features_ET[[j]]$y, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
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
      df_lsa_features_ET[[j]]$id_features_2 <- df_lsa_features_ET[[j]]$id_features
      range_x <- max(df_lsa_features_ET[[j]]$x) - min(df_lsa_features_ET[[j]]$x)
      range_y <- max(df_lsa_features_ET[[j]]$y) - min(df_lsa_features_ET[[j]]$y)
      range_coef <- 0.15
      higher_x <- range_x * range_coef
      lower_x <- -higher_x
      higher_y <- range_y * range_coef
      lower_y <- -higher_y
      df_lsa_features_ET[[j]]$id_features_2[((df_lsa_features_ET[[j]]$x < higher_x) & (df_lsa_features_ET[[j]]$x > lower_x)) &
                                              ((df_lsa_features_ET[[j]]$y < higher_y) & (df_lsa_features_ET[[j]]$y > lower_y))] <- ""
      ggplot(df_lsa_features_ET[[j]], aes(x, y, colour = 6)) +
        geom_point() +
        geom_text(label = df_lsa_features_ET[[j]]$id_features_2, color = "black") +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_lsa_features) + 
        theme(legend.position = "none") +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
      ggsave(paste0(pathFigures, "LSA_features_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      scope$transform(plot_lsa_features[[j]], paste0(pathFigures, "LSA_features_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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



# LSA of ETs by cluster ####

print("LSA of ETs by cluster")
tic()
# Selection of relevant files
FileNames_LSA_Cluster <- list.files(path = path, pattern = "df_lsa_ET_cluster_")
Dates_LSA_Cluster_calculation <- substr(FileNames_LSA_Cluster, start = 19, stop = 34) 

Dates_LSA_Cluster_calculation <- paste0(substr(Dates_LSA_Cluster_calculation,1,10), " ", substr(Dates_LSA_Cluster_calculation,12,13),
                                        ":", substr(Dates_LSA_Cluster_calculation,15,16), ":00 CET")

FileNames_LSA_Cluster_Selected <- FileNames_LSA_Cluster[(Dates_LSA_Cluster_calculation >= begin_date_calculation) & (Dates_LSA_Cluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_LSA_Cluster_Selected <- FileNames_LSA_Cluster_Selected[length(FileNames_LSA_Cluster_Selected)]

plot_lsa <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_LSA_Cluster_Selected)){
  load(file = file.path(path, FileNames_LSA_Cluster_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_lsa <- paste0("LSA of ", ET[[j]])
    }else{
      title_plot_lsa <- NULL
    }
    plot_lsa[[j]] <- plot_ly(df_lsa_ET_cluster[[j]], x = ~x, y = ~y, text = ~id_desc, type = "scatter", mode = "markers", 
                             color = ~cluster) %>%
      layout(title = title_plot_lsa, xaxis = list(title = "V1"), yaxis = list(title = "V2"))
    if(plot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "LSA_cluster_ET", j, "_", date, ".pdf"), width=11)
      col_plot <- as.numeric(as.factor(df_lsa_ET_cluster[[j]]$cluster))+1
      plot(df_lsa_ET_cluster[[j]]$x, df_lsa_ET_cluster[[j]]$y, xlab = "V1", ylab = "V2", pch = 16, 
           col = col_plot)
      grid()
      abline(h=0, lty=3)
      abline(v=0, lty=3)
      legend_pos <- c("topleft", "bottomright", "bottomleft", "topleft", "bottomright", "bottomright", "bottomleft")
      legend(legend_pos[j], 
             legend=sort(unique(df_lsa_ET_cluster[[j]]$cluster,)),
             col=sort(unique(col_plot)), pch=16, cex=0.8)
      title(title_plot_lsa)
      dev.off()
    }
    if(ggplot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      set.seed(935234)
      Pk = createPalette(length(unique(df_lsa_ET_cluster[[j]]$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
                         range = c(10, 45))
      names(Pk) <- NULL
      ggplot(df_lsa_ET_cluster[[j]], aes(x, y, colour = cluster )) +
        geom_point() +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_lsa) + 
        theme(legend.text = element_text(size = 6)) +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5) + 
        scale_colour_manual(values = Pk)
      ggsave(paste0(pathFigures, "LSA_cluster_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      scope$transform(plot_lsa[[j]], paste0(pathFigures, "LSA_cluster_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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



# UMAP of Group ####

print("UMAP of Group")
tic()
# Selection of relevant files
FileNames_UMAP_NoCluster <- list.files(path = path, pattern = "df_umap_NoCluster_2")
Dates_UMAP_NoCluster_calculation <- substr(FileNames_UMAP_NoCluster, start = 19, stop = 34) 

Dates_UMAP_NoCluster_calculation <- paste0(substr(Dates_UMAP_NoCluster_calculation,1,10), " ", substr(Dates_UMAP_NoCluster_calculation,12,13),
                                               ":", substr(Dates_UMAP_NoCluster_calculation,15,16), ":00 CET")

FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster[(Dates_UMAP_NoCluster_calculation >= begin_date_calculation) & (Dates_UMAP_NoCluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster_Selected[length(FileNames_UMAP_NoCluster_Selected)]

plot_umap <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_UMAP_NoCluster_Selected)){
  load(file = file.path(path, FileNames_UMAP_NoCluster_Selected[i]))
  if(TitleChart){
    title_plot_umap <- paste0("UMAP of Group")
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
    pdf(paste0(pathFigures, "UMAP_Group_", date, ".pdf"), width=11)
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
    ggsave(paste0(pathFigures, "UMAP_Group_", date, "_ggplot.pdf"), 
           width = 16, height = 9, units = "cm")
  }
  if(plotly_pdf){
    date <- Sys.time()
    date <- as.character(date)
    date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
    scope$transform(plot_umap[[i]], paste0(pathFigures, "pUMAP_Group_", date, "_plotly.pdf"), 
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



# UMAP of ETs ####

print("UMAP of ETs")
tic()
# Selection of relevant files
FileNames_UMAP_NoCluster <- list.files(path = path, pattern = "df_umap_NoCluster_ET_")
Dates_UMAP_NoCluster_calculation <- substr(FileNames_UMAP_NoCluster, start = 22, stop = 37) 

Dates_UMAP_NoCluster_calculation <- paste0(substr(Dates_UMAP_NoCluster_calculation,1,10), " ", substr(Dates_UMAP_NoCluster_calculation,12,13),
                                           ":", substr(Dates_UMAP_NoCluster_calculation,15,16), ":00 CET")

FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster[(Dates_UMAP_NoCluster_calculation >= begin_date_calculation) & (Dates_UMAP_NoCluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster_Selected[length(FileNames_UMAP_NoCluster_Selected)]

plot_umap <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_UMAP_NoCluster_Selected)){
  load(file = file.path(path, FileNames_UMAP_NoCluster_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_umap <- paste0("UMAP of ", ET[[j]])
    }else{
      title_plot_umap <- NULL
    }
    plot_umap[[j]] <- plot_ly(df_umap_ET[[j]], x = ~X1, y = ~X2, 
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
      pdf(paste0(pathFigures, "UMAP_ET", j, "_", date, ".pdf"), width=11)
      plot(df_umap_ET[[j]]$X1, df_umap_ET[[j]]$X2, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
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
      ggplot(df_umap_ET[[j]], aes(X1, X2, colour = 6)) +
        geom_point() +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_umap) + 
        theme(legend.position = "none") +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
      ggsave(paste0(pathFigures, "UMAP_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
     scope$transform(plot_umap[[j]], paste0(pathFigures, "pUMAP_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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



# UMAP of ETs - Sensitivity ####

print("UMAP of ETs - Sensitivity")
tic()
ET <- sort(unique(OpData.Filter$'Event Type'))

# Selection of relevant files
FileNames_UMAP_NoCluster <- list.files(path = path, pattern = "Semantic_tfidf.Filter.umap.ET_")
FileNames_UMAP_NoCluster_nChar <- nchar(FileNames_UMAP_NoCluster)
FileNames_UMAP_NoCluster <- FileNames_UMAP_NoCluster[substr(FileNames_UMAP_NoCluster, 
                                                            FileNames_UMAP_NoCluster_nChar-13, FileNames_UMAP_NoCluster_nChar-6)=="20230925"]
# Dates_UMAP_NoCluster_calculation <- substr(FileNames_UMAP_NoCluster, start = 33, stop = 41) 
# 
# Dates_UMAP_NoCluster_calculation <- paste0(substr(Dates_UMAP_NoCluster_calculation,1,10), " ", substr(Dates_UMAP_NoCluster_calculation,12,13),
#                                            ":", substr(Dates_UMAP_NoCluster_calculation,15,16), ":00 CET")
# 
# FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster[(Dates_UMAP_NoCluster_calculation >= begin_date_calculation) & (Dates_UMAP_NoCluster_calculation <= end_date_calculation)]

# Select the last one
# FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster_Selected[length(FileNames_UMAP_NoCluster_Selected)]
FileNames_UMAP_NoCluster_Selected <- FileNames_UMAP_NoCluster[which(FileNames_UMAP_NoCluster %in% 
                                                                      c("Semantic_tfidf.Filter.umap.ET_5_0.01_20230925.Rdata",
                                                                        "Semantic_tfidf.Filter.umap.ET_100_0.01_20230925.Rdata",
                                                                        "Semantic_tfidf.Filter.umap.ET_5_0.2_20230925.Rdata",
                                                                        "Semantic_tfidf.Filter.umap.ET_100_0.2_20230925.Rdata",
                                                                        "Semantic_tfidf.Filter.umap.ET_5_1_20230925.Rdata",
                                                                        "Semantic_tfidf.Filter.umap.ET_100_1_20230925.Rdata"
                                                                        ))]
FileNames_UMAP_NoCluster_Selected_nChar <- nchar(FileNames_UMAP_NoCluster_Selected)
FileNames_UMAP_NoCluster_Names <- substr(FileNames_UMAP_NoCluster_Selected, 31, FileNames_UMAP_NoCluster_Selected_nChar-15)


plot_umap <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_UMAP_NoCluster_Selected)){
  load(file = file.path(path, FileNames_UMAP_NoCluster_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_umap <- paste0("UMAP of ", ET[[j]], " ", FileNames_UMAP_NoCluster_Names[i])
    }else{
      title_plot_umap <- NULL
    }
    plot_umap[[j]] <- plot_ly(as.data.frame(Semantic_tfidf.Filter.umap.ET[[j]]), x = ~V1, y = ~V2, 
                              #text = ~id_desc, 
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
      pdf(paste0(pathFigures, "UMAP_ET", j, "_", FileNames_UMAP_NoCluster_Names[i], "_",  date, ".pdf"), width=11)
      plot(df_umap_ET[[j]]$X1, df_umap_ET[[j]]$X2, xlab = "V1", ylab = "V2", pch = 16, col = "blue")
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
      ggplot(df_umap_ET[[j]], aes(X1, X2, colour = 6)) +
        geom_point() +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_umap) + 
        theme(legend.position = "none") +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5)
      ggsave(paste0(pathFigures, "UMAP_ET", j, "_", FileNames_UMAP_NoCluster_Names[i], "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      scope$transform(plot_umap[[j]], paste0(pathFigures, "pUMAP_ET", j, "_", FileNames_UMAP_NoCluster_Names[i], "_", date, "_plotly.pdf"), 
                      width = 800, height = 450, scale = 1)
    }
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



# UMAP of ETs by cluster ####

print("UMAP of ETs by cluster")
tic()
# Selection of relevant files
FileNames_UMAP_Cluster <- list.files(path = path, pattern = "df_umap_ET_cluster_")
Dates_UMAP_Cluster_calculation <- substr(FileNames_UMAP_Cluster, start = 20, stop = 35) 

Dates_UMAP_Cluster_calculation <- paste0(substr(Dates_UMAP_Cluster_calculation,1,10), " ", substr(Dates_UMAP_Cluster_calculation,12,13),
                                           ":", substr(Dates_UMAP_Cluster_calculation,15,16), ":00 CET")

FileNames_UMAP_Cluster_Selected <- FileNames_UMAP_Cluster[(Dates_UMAP_Cluster_calculation >= begin_date_calculation) & (Dates_UMAP_Cluster_calculation <= end_date_calculation)]

# Select the last one
FileNames_UMAP_Cluster_Selected <- FileNames_UMAP_Cluster_Selected[length(FileNames_UMAP_Cluster_Selected)]

plot_umap <- list()

if(plotly_pdf){
  scope <- kaleido()
}

for(i in 1:length(FileNames_UMAP_Cluster_Selected)){
  load(file = file.path(path, FileNames_UMAP_Cluster_Selected[i]))
  for(j in 1:length(ET)){
    if(TitleChart){
      title_plot_umap <- paste0("UMAP of ", ET[[j]])
    }else{
      title_plot_umap <- NULL
    }
    plot_umap[[j]] <- plot_ly(df_umap_ET_cluster[[j]], x = ~X1, y = ~X2, 
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
      pdf(paste0(pathFigures, "UMAP_cluster_ET", j, "_", date, ".pdf"), width=11)
      col_plot <- as.numeric(as.factor(df_umap_ET_cluster[[j]]$cluster))+1
      plot(df_umap_ET_cluster[[j]]$X1, df_umap_ET_cluster[[j]]$X2, xlab = "V1", ylab = "V2", pch = 16, 
           col = col_plot)
      grid()
      abline(h=0, lty=3)
      abline(v=0, lty=3)
      legend_pos <- c("topleft", "bottomright", "bottomleft", "topleft", "bottomright", "bottomright", "bottomleft")
      legend(legend_pos[j], 
             legend=sort(unique(df_umap_ET_cluster[[j]]$cluster,)),
             col=sort(unique(col_plot)), pch=16, cex=0.8)
      title(title_plot_umap)
      dev.off()
    }
    if(ggplot_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      set.seed(935234)
      Pk = createPalette(length(unique(df_umap_ET_cluster[[j]]$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
                         range = c(10, 45))
      names(Pk) <- NULL
      ggplot(df_umap_ET_cluster[[j]], aes(X1, X2, colour = cluster )) +
        geom_point() +
        labs(colour = "") + 
        labs(x = "V1", y = "V2", title = title_plot_umap) + 
        theme(legend.text = element_text(size = 6)) +
        geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
        geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5) + 
        scale_colour_manual(values = Pk)
      ggsave(paste0(pathFigures, "UMAP_cluster_ET", j, "_", date, "_ggplot.pdf"), 
             width = 16, height = 9, units = "cm")
    }
    if(plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
     scope$transform(plot_umap[[j]], paste0(pathFigures, "pUMAP_cluster_ET", j, "_", date, "_plotly.pdf"), 
                 width = 800, height = 450, scale = 1)
    }
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
  


# # UMAP of ETs by cluster (basic LDA) ####
# 
# print("UMAP of ETs by cluster (basic LDA)")
# tic()
# ET <- sort(unique(OpData.Filter$'Event Type'))
# 
# begin_date_calculation <- "2023-12-28 15:30:00"
# end_date_calculation <- "2023-12-28 15:40:00"
# 
# begin_date_calculation <- as.POSIXct(strptime(begin_date_calculation, "%Y-%m-%d %H:%M:%S"))
# end_date_calculation <- as.POSIXct(strptime(end_date_calculation, "%Y-%m-%d %H:%M:%S"))
# 
# # Selection of relevant files
# FileNames_UMAP_Cluster <- list.files(path = path, pattern = "df_umap_ET_cluster_")
# Dates_UMAP_Cluster_calculation <- substr(FileNames_UMAP_Cluster, start = 20, stop = 35) 
# 
# Dates_UMAP_Cluster_calculation <- paste0(substr(Dates_UMAP_Cluster_calculation,1,10), " ", substr(Dates_UMAP_Cluster_calculation,12,13),
#                                          ":", substr(Dates_UMAP_Cluster_calculation,15,16), ":00 CET")
# 
# FileNames_UMAP_Cluster_Selected <- FileNames_UMAP_Cluster[(Dates_UMAP_Cluster_calculation >= begin_date_calculation) & (Dates_UMAP_Cluster_calculation <= end_date_calculation)]
# 
# # Select the last one
# FileNames_UMAP_Cluster_Selected <- FileNames_UMAP_Cluster_Selected[length(FileNames_UMAP_Cluster_Selected)]
# 
# plot_umap <- list()
# 
# if(plotly_pdf){
#   scope <- kaleido()
# }
# 
# for(i in 1:length(FileNames_UMAP_Cluster_Selected)){
#   load(file = file.path(path, FileNames_UMAP_Cluster_Selected[i]))
#   for(j in 1:length(ET)){
#     if(TitleChart){
#       title_plot_umap <- paste0("UMAP of ", ET[[j]])
#     }else{
#       title_plot_umap <- NULL
#     }
#     plot_umap[[j]] <- plot_ly(df_umap_ET_cluster[[j]], x = ~X1, y = ~X2, 
#                               text = ~id_desc, 
#                               color = ~cluster, 
#                               type = 'scatter', mode = 'markers') %>%
#       layout(
#         plot_bgcolor = "#e5ecf6",
#         xaxis = list(
#           title = "V1"),
#         yaxis = list(
#           title = "V2"),
#         title = title_plot_umap)
#     if(plot_pdf){
#       date <- Sys.time()
#       date <- as.character(date)
#       date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
#       pdf(paste0(pathFigures, "UMAP_cluster_ET", j, "_", date, ".pdf"), width=11)
#       col_plot <- as.numeric(as.factor(df_umap_ET_cluster[[j]]$cluster))+1
#       plot(df_umap_ET_cluster[[j]]$X1, df_umap_ET_cluster[[j]]$X2, xlab = "V1", ylab = "V2", pch = 16, 
#            col = col_plot)
#       grid()
#       abline(h=0, lty=3)
#       abline(v=0, lty=3)
#       legend_pos <- c("topleft", "bottomright", "bottomleft", "topleft", "bottomright", "bottomright", "bottomleft")
#       legend(legend_pos[j], 
#              legend=sort(unique(df_umap_ET_cluster[[j]]$cluster,)),
#              col=sort(unique(col_plot)), pch=16, cex=0.8)
#       title(title_plot_umap)
#       dev.off()
#     }
#     if(ggplot_pdf){
#       date <- Sys.time()
#       date <- as.character(date)
#       date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
#       set.seed(935234)
#       Pk = createPalette(length(unique(df_umap_ET_cluster[[j]]$cluster)),  c("#ff0000", "#00ff00", "#0000ff"),
#                          range = c(10, 45))
#       names(Pk) <- NULL
#       ggplot(df_umap_ET_cluster[[j]], aes(X1, X2, colour = cluster )) +
#         geom_point() +
#         labs(colour = "") + 
#         labs(x = "V1", y = "V2", title = title_plot_umap) + 
#         theme(legend.text = element_text(size = 6)) +
#         geom_hline(yintercept=0, linetype="dotted", color="black", linewidth=0.5) +
#         geom_vline(xintercept=0, linetype="dotted", color="black", linewidth=0.5) + 
#         scale_colour_manual(values = Pk)
#       ggsave(paste0(pathFigures, "UMAP_cluster_ET", j, "_", date, "_ggplot.pdf"), 
#              width = 16, height = 9, units = "cm")
#     }
#     if(plotly_pdf){
#       date <- Sys.time()
#       date <- as.character(date)
#       date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
#       scope$transform(plot_umap[[j]], paste0(pathFigures, "pUMAP_basic_cluster_ET", j, "_", date, "_plotly.pdf"), 
#                       width = 800, height = 450, scale = 1)
#     }
#   }
# }
# 
# if(plotly_pdf){
#   rm(scope)
#   gc()
# }
# 
# if(plotly_output){
#   plot_umap
# }
# toc()



# Wordcloud of ETs by topic ####

print("Wordcloud of ETs by topic")
tic()
# Selection of relevant files
FileNames_wordcloud_topic <- list.files(path = path, pattern = "ProbWords_ET_")
Dates_wordcloud_topic_calculation <- substr(FileNames_wordcloud_topic, start = 14, stop = 29) 

Dates_wordcloud_topic_calculation <- paste0(substr(Dates_wordcloud_topic_calculation,1,10), " ", substr(Dates_wordcloud_topic_calculation,12,13),
                                                  ":", substr(Dates_wordcloud_topic_calculation,15,16), ":00 CET")

FileNames_wordcloud_topic_Selected <- FileNames_wordcloud_topic[(Dates_wordcloud_topic_calculation >= begin_date_calculation) & (Dates_wordcloud_topic_calculation <= end_date_calculation)]

# Select the last one
FileNames_wordcloud_topic_Selected <- FileNames_wordcloud_topic_Selected[length(FileNames_wordcloud_topic_Selected)]

# Load seeded words
FileNames_seeded_words_complete <- list.files(path = path, pattern = "seeded_words_complete_2")
# load(file = file.path(path, "seeded_words_complete_2023-11-15_09-59.Rdata"))
Dates_seeded_words_complete_calculation <- substr(FileNames_seeded_words_complete, start = 23, stop = 38) 

Dates_seeded_words_complete_calculation <- paste0(substr(Dates_seeded_words_complete_calculation,1,10), " ", substr(Dates_seeded_words_complete_calculation,12,13),
                                            ":", substr(Dates_seeded_words_complete_calculation,15,16), ":00 CET")

FileNames_seeded_words_complete_Selected <- FileNames_seeded_words_complete[(Dates_seeded_words_complete_calculation >= begin_date_calculation) & (Dates_seeded_words_complete_calculation <= end_date_calculation)]

# Select the last one
FileNames_seeded_words_complete_Selected <- FileNames_seeded_words_complete_Selected[length(FileNames_seeded_words_complete_Selected)]
load(file = file.path(path, FileNames_seeded_words_complete_Selected))

def.par <- par(no.readonly = TRUE)
for(i in 1:length(FileNames_wordcloud_topic_Selected)){
  load(file = file.path(path, FileNames_wordcloud_topic_Selected[i]))
  for(j in 1:length(ET)){
    seeded_words <- seeded_words_complete[substr(seeded_words_complete$topic,1,4)==substr(ET[[j]],1,4),]
    # Total number of topics (seeded and non-seeded)
    k <- length(unique(seeded_words$topic))
    # Topics (seeded and non-seeded)
    topic_vector <- as.character(unique(seeded_words$topic))
    ProbWords_ET <- ProbWords[[j]]
    for (topic in 1:k) {
      df <- data.frame(term = row.names(ProbWords_ET), p = ProbWords_ET[, topic])
      head(df[order(-df$p), ])
      
      layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
      par(mar = rep(0, 4))
      plot.new()
      text(x = 0.5,
           y = 0.5,
           paste0(topic_vector[topic])) 
      
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
        pdf(paste0(pathFigures, "Wordcloud_", paste0(topic_vector[topic]), "_", date, ".pdf"), width=11)
        layout(matrix(c(1, 2), nrow = 2), heights = c(1, 4))
        par(mar = rep(0, 4))
        plot.new()
        text(x = 0.5,
             y = 0.5,
             paste0(topic_vector[topic])) 
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
}
par(def.par) 
toc()



# Traceplots of ETs ####

print("Traceplots of ETs")
tic()
# Selection of relevant files
FileNames_LDA <- list.files(path = path, pattern = "model_LDA_ET_")
Dates_LDA_calculation <- substr(FileNames_LDA, start = 14, stop = 29) 

Dates_LDA_calculation <- paste0(substr(Dates_LDA_calculation,1,10), " ", substr(Dates_LDA_calculation,12,13),
                                            ":", substr(Dates_LDA_calculation,15,16), ":00 CET")

FileNames_LDA_Selected <- FileNames_LDA[(Dates_LDA_calculation >= begin_date_calculation) & (Dates_LDA_calculation <= end_date_calculation)]

# Select the last one
FileNames_LDA_Selected <- FileNames_LDA_Selected[length(FileNames_LDA_Selected)]

num.iterations_par <- 10^3 
burn_par <- num.iterations_par / 2 
LDA_verbose <- round(num.iterations_par / 100)
LDA_keep <- max(LDA_verbose / 100, 1)

for(i in 1:length(FileNames_LDA_Selected)){
  load(file = file.path(path, FileNames_LDA_Selected[i]))
  for(j in 1:length(ET)){
    
    LDA_logLiks <- model[[j]]@logLiks

    ylim_ET <- c(min(LDA_logLiks), max(LDA_logLiks))
    
    LDA_logliks_ind <- seq(LDA_keep, burn_par[i]+num.iterations_par[i], by = LDA_keep)
    plot(LDA_logliks_ind, LDA_logLiks, xlab = "Iteration", ylab = "Log-likelihood", 
         pch = 16, 
         lty = 1,
         col = "blue", type = "l", # "b",
         ylim = ylim_ET)
    if(TitleChart){
      title(paste0("Traceplot of ", ET[j]))  
    }
    abline(v = burn_par, lty = 2)
    
    if(plot_pdf || ggplot_pdf || plotly_pdf){
      date <- Sys.time()
      date <- as.character(date)
      date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
      pdf(paste0(pathFigures, "Traceplot_ET", j, "_", date, ".pdf"), width=11)
      plot(LDA_logliks_ind, LDA_logLiks, xlab = "Iteration", ylab = "Log-likelihood", 
           pch = 16, 
           lty = 1,
           col = "blue", type = "l", # "b",
           ylim = ylim_ET)
      if(TitleChart){
        title(paste0("Traceplot of ", ET[j]))
      }
      abline(v = burn_par[i], lty = 2)
      dev.off()
    }
  }
}
toc()

print("End of calculation")

sink()


