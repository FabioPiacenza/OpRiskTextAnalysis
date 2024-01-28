# Clean the Global Environment
rm(list = ls())
gc()
cat("\014")

# Path code to be sourced (where TwitterAnalysis.R is saved)
pathSource <- file.path("C:\\Users\\ui40232\\OneDrive - Unicredit\\Desktop\\OpRiskTextAnalysis\\")

# Path for output
pathTwitter <- file.path("W:\\Twitter\\data\\")

# Dates for tweets analysis
begin_date_analysis <- "15/06/2023"
end_date_analysis <- "16/06/2023"
# begin_date_analysis <- "07/07/2023"
# end_date_analysis <- "12/07/2023"

begin_date_analysis <- as.Date(begin_date_analysis, "%d/%m/%Y")
end_date_analysis <- as.Date(end_date_analysis, "%d/%m/%Y")

all_date_analysis <- seq(begin_date_analysis, end_date_analysis, by = 1)
all_date_analysis <- as.character(all_date_analysis)

for(ind_date in all_date_analysis){
  print(ind_date)
  begin_date <- end_date <- ind_date
  date <- Sys.time()
  date <- as.character(date)
  date <- paste0(substr(date,1,10), "_", substr(date,12,13), "-", substr(date,15,16))
  sink(file = file.path(pathTwitter,paste0("Log_TwitterAnalysis_", begin_date, "_" , date, ".Rout")))
  source(file = file.path(pathSource, "TwitterAnalysis.R"))
  sink()
  gc()
}



