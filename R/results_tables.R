# === Create tables -------------------------------------
# Julie Turner
# started July 1 2024

#### Packages ####
libs <- c('data.table', 'flextable', 'tidyr')
lapply(libs, require, character.only = TRUE)

## Input data ----
results.path <-  file.path('Data', 'results')


## load results ----
files <- list.files(path = results.path)
results <- data.table(files = files[files %like% 'results'])
results[,names := stringr::str_extract(files, "(?<=results_).*(?=_20240630.csv)")]

for (i in 1:length(results$files)) { # for each file in the list
  fileName <- results$files[[i]] # save filename of element i
  dataName <- results$names[[i]] # save data name of element i
  tempData <- fread (file.path(results.path, fileName)) # read csv file
  assign (dataName, tempData, envir=.GlobalEnv)  # assign the results of file to the data named
  
}


## make table ----
dictionary <- data.table(cols = c("herd", "N.2024", "N.2024.uncertainty",
                         "lambda.2024", "lambda.2024.uncertainty",
                         "N.2027", "N.2027.uncertainty", 
                         "lambda.2027", "lambda.2027.uncertainty","time2recovery.upper50"),
                         labs = c("Herd", "# adult cows 2024", "% uncertainty in # cows 2024",
                                  "Lambda 2024", "% uncertainty lambda 2024",
                                  "# adult cows 2027", "% uncertainty in # cows 2027", 
                                  "Lambda 2027", "% uncertainty lambda 2027","Yrs to recovery since 2025"))
tab <- remove3in2025
mk_table <- function(tab, title){
  tmp <- tab[,.(herd, N.2024 = N.pre_removal.median, N.2024.uncertainty = round((N.pre_removal.median - N.pre_removal.lower95)/N.pre_removal.median*100),
         lambda.2024 = round(exp(r.pre_removal.median), 2), lambda.2024.uncertainty = round(abs(((exp(r.pre_removal.median) - exp(r.pre_removal.lower95))/exp(r.pre_removal.median))*100)),
         N.2027 = N.post_removal.median, N.2027.uncertainty = round((N.post_removal.median - N.post_removal.lower95)/N.post_removal.median*100),
         lambda.2027 = round(exp(r.post_removal.median), 2), lambda.2027.uncertainty = round(abs(((exp(r.post_removal.median) - exp(r.post_removal.lower95))/exp(r.post_removal.median))*100)),
         time2recovery.upper50)]
  setnames(tmp, old = dictionary$cols, new = dictionary$labs)
  ft.tmp <- flextable(tmp) %>%
    add_header_lines(values = title) %>%
    align(align = 'center', part = 'header')
  return(ft.tmp)
}


rm3 <- mk_table(remove3in2025, title = 'Remove 3 cows in 2025')
rm3

rm6 <- mk_table(remove6in2025, title = 'Remove 6 cows in 2025')
rm6

rm3n3 <- mk_table(remove3in2025_remove3in2026, title = 'Remove 3 cows in 2025 and 3 in 2026')
rm3n3
