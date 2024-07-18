# === Create tables -------------------------------------
# Julie Turner
# started July 1 2024

#### Packages ####
libs <- c('data.table', 'flextable', 'tidyr')
lapply(libs, require, character.only = TRUE)

## Input data ----
results.path <-  file.path('Data', 'results')

census_yr <- fread(file.path('Data', 'herds_last_census.csv'))

## load results ----
files <- list.files(path = results.path)
results <- data.table(files = files[files %like% 'results'])
results[,names := stringr::str_extract(files, "(?<=results_).*(?=_202407)")]

for (i in 1:length(results$files)) { # for each file in the list
  fileName <- results$files[[i]] # save filename of element i
  dataName <- results$names[[i]] # save data name of element i
  tempData <- fread (file.path(results.path, fileName)) # read csv file
  assign (dataName, tempData, envir=.GlobalEnv)  # assign the results of file to the data named
  
}


## make table ----
colnames(noDD_remove0in2025_remove0in2026)
dictionary <- data.table(cols = c("herd", 'last_census',
                                  "N.2024", "N.2024.uncertainty",
                         "lambda.2024", "lambda.2024.uncertainty",
                         "N.2027", "N.2027.uncertainty", 
                         "lambda.2027", "lambda.2027.uncertainty",
                         #"recovery5yrs", 
                         "time2recovery", 
                         "lambda.5yr", "lambda.5yr.uncertainty",
                         "cow.prod"),
                         labs = c("Subpopulation", 'Yr of last census',
                                  "# adult cows 2024", "% uncertainty in # cows 2024",
                                  "Lambda 2024", "% uncertainty lambda 2024",
                                  "# adult cows 2027", "% uncertainty in # cows 2027", 
                                  "Lambda 2027", "% uncertainty lambda 2027",
                                 # "% recovered in 5 yrs",
                                  "Yrs to recovery since 2025", 
                                 "5-yr lambda", "% uncertainty 5-yr lambda",
                                 "Predicted cow growth"))
#tab <- remove3in2025

### make meta table ----
meta.tab <- merge(noDD_remove0in2025_remove0in2026, census_yr, by = 'herd')
simp.meta.tab <- meta.tab[,.(herd, last_census = as.character(last_census),
                  N.2024 = N.pre_removal.median, N.2024.uncertainty = round((N.pre_removal.median - N.pre_removal.lower95)/N.pre_removal.median*100),
       lambda.2024 = round(exp(r.pre_removal.median), 2), 
       lambda.2024.uncertainty = round(abs(((exp(r.pre_removal.median) - exp(r.pre_removal.lower95))/exp(r.pre_removal.median))*100)),
       cow.prod = round(N.pre_removal.median*exp(r.pre_removal.median))-N.pre_removal.median)]
simp.meta.tab[herd == 'A La Peche', herd:= 'À la Pêche']
simp.meta.order <- simp.meta.tab[order(-cow.prod, -N.2024, -lambda.2024, lambda.2024.uncertainty, -last_census)]
setnames(simp.meta.order, old = dictionary$cols, new = dictionary$labs, skip_absent = T)

meta.ft <- flextable(simp.meta.order) %>%
  add_header_lines(values = 'Business as usual 2024') %>%
  align(align = 'center', part = 'header') 
meta.ft


DT::datatable(simp.meta.order, options = list(pageLength = 20))

### make forecasted tables ----
mk_table <- function(tab, title){
  tmp <- tab[,.(herd,          
         N.2027 = N.post_removal.median, N.2027.uncertainty = round((N.post_removal.median - N.post_removal.lower95)/N.post_removal.median*100),
         #lambda.2027 = round(exp(r.post_removal.median), 2), lambda.2027.uncertainty = round(abs(((exp(r.post_removal.median) - exp(r.post_removal.lower95))/exp(r.post_removal.median))*100)),
         lambda.5yr = round(exp(r.5yr.median), 2), lambda.5yr.uncertainty = round(abs(((exp(r.5yr.median) - exp(r.5yr.lower95))/exp(r.5yr.median))*100)),
         #recovery5yrs = round((N.2029.median/N.pre_removal.median)*100), 
         time2recovery.upper50 
         )]
  tmp[herd == 'A La Peche', herd:= 'À la Pêche']
  tmp[,time2recovery:=as.character(time2recovery.upper50)]
  tmp[is.na(time2recovery.upper50), time2recovery := '>100']
  tmp[time2recovery.upper50 == 2, time2recovery := '\u22642']
  tmp.order <- tmp[order(time2recovery.upper50, -lambda.5yr, lambda.5yr.uncertainty, -N.2027)]
  setnames(tmp.order, old = dictionary$cols, new = dictionary$labs, skip_absent = T)
  ft.tmp <- flextable(tmp.order[,-'time2recovery.upper50']) %>%
    add_header_lines(values = title) %>%
    align(align = 'center', part = 'header') %>%
    align(align = c('left', 'center', 'center', 'center', 'center', 'center'), part = 'body')
  return(ft.tmp)
}


rm3 <- mk_table(noDD_remove3in2025_remove0in2026, title = 'Remove 3 cows in 2025')
rm3

rm6 <- mk_table(noDD_remove6in2025_remove0in2026, title = 'Remove 6 cows in 2025')
rm6

rm3n3 <- mk_table(noDD_remove3in2025_remove3in2026, title = 'Remove 3 cows in 2025 and 3 in 2026')
rm3n3

rm12 <- mk_table(noDD_remove12in2025_remove0in2026, title = 'Remove 12 cows in 2025')
rm12

rm6n6 <- mk_table(noDD_remove6in2025_remove6in2026, title = 'Remove 6 cows in 2025 and 6 in 2026')
rm6n6

rm24 <- mk_table(noDD_remove24in2025_remove0in2026, title = 'Remove 24 cows in 2025')
rm24

rm12n12 <- mk_table(noDD_remove12in2025_remove12in2026, title = 'Remove 12 cows in 2025 and 12 in 2026')
rm12n12

### years to recovery ----
# remove duplicate 2024 vals
dups <- c("N.pre_removal.lower95", "N.pre_removal.median", "N.pre_removal.upper95", 
          "r.pre_removal.lower95", "r.pre_removal.median", "r.pre_removal.upper95" )
predResults <- rbind(noDD_remove3in2025_remove0in2026[,!(names(noDD_remove3in2025_remove0in2026) %in% dups), with = F][,removal := '3'], 
                    noDD_remove6in2025_remove0in2026[,!(names(noDD_remove6in2025_remove0in2026) %in% dups), with = F][,removal := '6'],
                     noDD_remove3in2025_remove3in2026[,!(names(noDD_remove3in2025_remove3in2026) %in% dups), with = F][,removal := '3/3'], 
                    noDD_remove12in2025_remove0in2026[,!(names(noDD_remove12in2025_remove0in2026) %in% dups), with = F][,removal := '12'],
                     noDD_remove6in2025_remove6in2026[,!(names(noDD_remove6in2025_remove6in2026) %in% dups), with = F][,removal := '6/6'], 
                    noDD_remove24in2025_remove0in2026[,!(names(noDD_remove24in2025_remove0in2026) %in% dups), with = F][,removal := '24'],
                     noDD_remove12in2025_remove12in2026[,!(names(noDD_remove12in2025_remove12in2026) %in% dups), with = F][,removal := '12/12'])
allResults <- merge(meta.tab[,.(herd, last_census = as.character(last_census),
                                N.2024 = N.pre_removal.median,
                                cow.prod = round(N.pre_removal.median*exp(r.pre_removal.median))-N.pre_removal.median)], 
                    predResults, 
                    by = 'herd')

yrs2recover <- allResults[,.(herd, last_census, N.2024, removal,
                             cow.prod, time2recovery.upper50)]
yrs2recover[,time2recovery := as.character(time2recovery.upper50)]
yrs2recover[is.na(time2recovery.upper50), time2recovery := '>100']
yrs2recover[time2recovery.upper50 == 2, time2recovery := '\u22642']
yrs2recover.wide <- dcast(yrs2recover, herd + last_census + N.2024 + cow.prod ~ removal, value.var = 'time2recovery')

yrs2recover.wide <- yrs2recover.wide[,.(herd, last_census, N.2024, cow.prod,
                                        `3`, `6`, `3/3`, `12`, `6/6`, `24`, `12/12`)]
yrs2recover.wide[herd == 'A La Peche', herd:= 'À la Pêche']

yrs2recover.order <- yrs2recover.wide[order(-cow.prod, -N.2024, -last_census)]
setnames(yrs2recover.order, old = dictionary$cols, new = dictionary$labs, skip_absent = T)

yrs2recover.ft <- flextable(yrs2recover.order) %>%
  add_header_lines(values = 'Years to recovery under different removal scenarios') %>%
  align(align = 'center', part = 'header') %>%
  align(align = c('left', 'center', 'center', 'center', 'center', 'center', 'center', 'center', 'center', 'center', 'center'))
yrs2recover.ft

## FOR APPENDICES ----
### extirpation herds ----
tooSmallHerds <- data.table(herd = c('Brazeau', 'Frisby-Boulder', ' Purcells South', 'South Selkirks', 'Narrow Lake'))

ext.herds <- SmallHerds[,.(decline.probability.2023, FE.probability.10yrs, FE.probability.30yrs, FE.probability.100yrs)]

### DD results tables ----
DDrm3 <- mk_table(DD_remove3in2025_remove0in2026, title = 'Remove 3 cows in 2025 modelled with density-dependence')
DDrm3

DDrm6 <- mk_table(DD_remove6in2025_remove0in2026, title = 'Remove 6 cows in 2025 modelled with density-dependence')
DDrm6

DDrm12 <- mk_table(DD_remove12in2025_remove0in2026, title = 'Remove 12 cows in 2025 modelled with density-dependence')
DDrm12

DDrm24 <- mk_table(DD_remove24in2025_remove0in2026, title = 'Remove 24 cows in 2025 modelled with density-dependence')
DDrm24

