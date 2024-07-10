
# clean env
rm(list = ls())
gc()

# Libraries
library(dplyr)
library(mgcv)
library(mgcViz)

# the functional extirpation threshold
FET <- 10

# the number of posterior samples
n <- 1000

# # Load full posterior chains and thin to n samples per herd-year
# IPM_posteriors <- read.csv("Data/demog.draws_forTal.csv",
#                            header = T, stringsAsFactors = T) %>%
#   select(-c(1:4, 6:11, 13:15, 19))
# draws <- sort(unique(IPM_posteriors$.draw))
# draws <- draws[seq(from = 1, to = length(draws), length.out = n)]
# IPM_posteriors <- IPM_posteriors %>%
#   filter(.draw %in% draws) %>%
#   arrange(herd, yrs, .draw)
# 
# # retain only high quality data
# PosteriorModes <- read.csv("Data/SMC_IPM_Demography_posteriors.csv",
#                            header = T, stringsAsFactors = T)
# for (i in 1:nrow(PosteriorModes)) {
#   if (PosteriorModes[i, "quality"] == "low") {
#     invalid.indices <- which((as.character(IPM_posteriors$herd) ==
#                                 as.character(PosteriorModes[i, "herd"])) &
#                                (IPM_posteriors$yrs ==
#                                   PosteriorModes[i, "year"]))
#     IPM_posteriors$totAdults[invalid.indices] <- NA
#   }
# }
# 
# # Calculate adult-cow-only r
# IPM_posteriors$totAdults.minus1 <- NA
# IPM_posteriors$totAdults.minus2 <- NA
# IPM_posteriors$totAdults.minus3 <- NA
# IPM_posteriors$trt.minus1 <- NA
# IPM_posteriors$trt.minus1 <- factor(IPM_posteriors$trt.minus1)
# levels(IPM_posteriors$trt.minus1) <- levels(IPM_posteriors$trt)
# IPM_posteriors$trt.minus2 <- IPM_posteriors$trt.minus1
# for (d in unique(IPM_posteriors$.draw)) {
#   for (h in unique(IPM_posteriors$herd)) {
#     indices <- which((IPM_posteriors$.draw == d) & (IPM_posteriors$herd == h))
#     temp <- IPM_posteriors[indices, ]
#     temp$totAdults.minus1[2:length(indices)] <-
#       temp$totAdults[1:(length(indices) - 1)]
#     temp$totAdults.minus2[3:length(indices)] <-
#       temp$totAdults[1:(length(indices) - 2)]
#     temp$totAdults.minus3[4:length(indices)] <-
#       temp$totAdults[1:(length(indices) - 3)]
#     temp$trt.minus1[2:length(indices)] <-
#       temp$trt[1:(length(indices) - 1)]
#     temp$trt.minus2[3:length(indices)] <-
#       temp$trt[1:(length(indices) - 2)]
#     IPM_posteriors[indices, ] <- temp
#   }
# }
# dat <- IPM_posteriors %>% filter(!is.na(totAdults), !is.na(totAdults.minus1))
# write.csv(dat, 'Data/dat_240617.csv')
dat <- read.csv("Data/dat_240617.csv", header = T, stringsAsFactors = T) %>%
  mutate(FE = totAdults < FET)

for (h in levels(dat$herd)) {
  FE.yrs <- dat$yrs[dat$FE & (dat$herd == h)]
  if (length(FE.yrs) > 0) {
    dat$totAdults[(dat$yrs >= min(FE.yrs)) & (dat$herd == h)] <- NA
  }
}

dat <- dat %>% filter(!is.na(totAdults) & !is.na(totAdults.minus1))

dat$is.WolfReduce <- as.numeric(dat$trt %in%
                                  c("feed-reducewolves",
                                    "pen-reducemoose-reducewolves",
                                    "pen-reducewolves",
                                    "reducemoose-reducewolves",
                                    "reducewolves",
                                    "reducewolves-sterilizewolves",
                                    "reducewolves-transplant"))
dat$is.WolfReduce.minus1 <- as.numeric(dat$trt.minus1 %in%
                                         c("feed-reducewolves",
                                           "pen-reducemoose-reducewolves",
                                           "pen-reducewolves",
                                           "reducemoose-reducewolves",
                                           "reducewolves",
                                           "reducewolves-sterilizewolves",
                                           "reducewolves-transplant"))
dat$is.MooseReduce <- as.numeric(dat$trt %in%
                                   c("pen-reducemoose",
                                     "pen-reducemoose-reducewolves",
                                     "reducemoose",
                                     "reducemoose-reducewolves"))
dat$is.MooseReduce.minus1 <- as.numeric(dat$trt.minus1 %in%
                                          c("pen-reducemoose",
                                            "pen-reducemoose-reducewolves",
                                            "reducemoose",
                                            "reducemoose-reducewolves"))
dat$is.Pen <- as.numeric(dat$trt %in%
                           c("pen-reducemoose",
                             "pen-reducemoose-reducewolves",
                             "pen-reducewolves"))
dat$is.Pen.minus1 <- as.numeric(dat$trt.minus1 %in%
                                  c("pen-reducemoose",
                                    "pen-reducemoose-reducewolves",
                                    "pen-reducewolves"))
dat$is.Feed <- as.numeric(dat$trt %in%
                            c("feed",
                              "feed-reducewolves"))
dat$is.Feed.minus1 <- as.numeric(dat$trt.minus1 %in%
                                   c("feed",
                                     "feed-reducewolves"))

RangeAreas <- read.csv("Data/ipm_herds.csv", header = T)
dat$area <- NA
for (h in levels(dat$herd)) {
  indices <- which(dat$herd == h)
  if (sum(h == RangeAreas$herd) == 1) {
    dat$area[indices] <- RangeAreas$area[h == RangeAreas$herd]
  }
}

dat <- dat %>% mutate(
  density = case_when(
    (totAdults.minus1 >= FET) ~ (totAdults.minus1 / area),
    (totAdults.minus1 < FET) ~ 0),
  r = log(totAdults / totAdults.minus1),
  r.minus1 = log(totAdults.minus1 / totAdults.minus2),
  r.minus2 = log(totAdults.minus2 / totAdults.minus3),
  pen.effect = log(1 + (is.Pen.minus1 / totAdults.minus2)),
  year = factor(yrs))


################################################################################
###                          model                                           ###
################################################################################

summary(gam_1 <- gam(r ~ s(herd, bs = "re") +
                       is.WolfReduce.minus1 + 
                       s(herd, is.WolfReduce.minus1, bs = "re") + 
                       is.MooseReduce.minus1 +
                       s(herd, is.MooseReduce.minus1, bs = "re") + 
                       is.Feed.minus1 + pen.effect +
                       s(I(density * (1 - is.WolfReduce.minus1)), k = 4) + 
                       s(I(density * is.WolfReduce.minus1), k = 4) + 
                       s(herd, density, bs = 'sz', k = 4) +
                       s(r.minus1, r.minus2, k = 4),
                     data = dat))
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   r ~ s(herd, bs = "re") + is.WolfReduce.minus1 + s(herd, is.WolfReduce.minus1, 
#                                                     bs = "re") + is.MooseReduce.minus1 + s(herd, is.MooseReduce.minus1, 
#                                                                                            bs = "re") + is.Feed.minus1 + pen.effect + s(I(density * 
#                                                                                                                                             (1 - is.WolfReduce.minus1)), k = 4) + s(I(density * is.WolfReduce.minus1), 
#                                                                                                                                                                                     k = 4) + s(herd, density, bs = "sz", k = 4) + s(r.minus1, 
#                                                                                                                                                                                                                                     r.minus2, k = 4)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -0.161798   0.004932 -32.804   <2e-16 ***
#   is.WolfReduce.minus1   0.085949   0.037274   2.306   0.0211 *  
#   is.MooseReduce.minus1 -0.013451   0.040980  -0.328   0.7427    
# is.Feed.minus1         0.246393   0.003572  68.984   <2e-16 ***
#   pen.effect             3.207775   0.081714  39.256   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(herd)                                    -1.135e-12 32.000    0.0  <2e-16 ***
#   s(herd,is.WolfReduce.minus1)                1.200e+01 12.000 1216.3  <2e-16 ***
#   s(herd,is.MooseReduce.minus1)               1.998e+00  2.000  780.4  <2e-16 ***
#   s(I(density * (1 - is.WolfReduce.minus1)))  3.000e+00  3.000 1184.9  <2e-16 ***
#   s(I(density * is.WolfReduce.minus1))        2.991e+00  2.999  282.6  <2e-16 ***
#   s(herd,density)                             9.195e+01 93.489  627.9  <2e-16 ***
#   s(r.minus1,r.minus2)                        3.000e+00  3.000 1441.5  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.202   Deviance explained = 20.2%
# GCV = 0.0097732  Scale est. = 0.0097716  n = 735000

# plot(getViz(gam_1))
# plot(gam_1)

dat.last10years <- dat %>% filter(yrs > 2013,
                            !is.na(r.minus2),
                            !is.na(r.minus1),
                            !is.na(r))
dat.last10years$gam_1.pred <- predict(gam_1, newdata = dat.last10years)
summary(gam_2 <- gam(r ~ 0 + s(gam_1.pred, k = 4) +
                       s(herd, gam_1.pred, bs = 'sz', k = 4) +
                       s(herd, year, gam_1.pred, bs = 'sz', k = 4) + 
                       s(r.minus1, r.minus2, k = 4),
                     data = dat.last10years))
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   r ~ 0 + s(gam_1.pred, k = 4) + s(herd, gam_1.pred, bs = "sz", 
#                                    k = 4) + s(herd, year, gam_1.pred, bs = "sz", k = 4) + s(r.minus1, 
#                                                                                             r.minus2, k = 4)
# 
# Approximate significance of smooth terms:
#   edf  Ref.df      F p-value    
# s(gam_1.pred)             2.980   2.994 532.65  <2e-16 ***
#   s(herd,gam_1.pred)       63.197  66.598  51.24  <2e-16 ***
#   s(herd,year,gam_1.pred) 373.418 379.667 342.02  <2e-16 ***
#   s(r.minus1,r.minus2)      2.963   2.998 754.09  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Rank: 840/886
# R-sq.(adj) =  0.636   Deviance explained = 63.9%
# GCV = 0.0057069  Scale est. = 0.0056941  n = 198000

# plot(getViz(gam_2))
# plot(gam_2)



################################################################################
###                          simulate                                        ###
################################################################################

mydat <- dat %>% filter(yrs > 2020,
                        !is.na(r.minus2),
                        !is.na(r.minus1),
                        !is.na(r)) %>%
  mutate(gam_1.pred = NA)
r.min <- min(dat$r)
r.max <- max(dat$r)
x.2025 <- 3
x.2026 <- 3
for (y in 2024:2124) {
  
  x <- 0
  if (y == 2025) {x <- x.2025}
  if (y == 2026) {x <- x.2026}
  
  mydat.y <- mydat %>%
    filter(yrs == (y - 1)) %>%
    mutate(yrs = y,
           r.minus2 = r.minus1,
           r.minus1 = r,
           r = 0, 
           totAdults.minus3 = totAdults.minus2,
           totAdults.minus2 = totAdults.minus1,
           totAdults.minus1 = totAdults - x,
           totAdults = 0,
           density = case_when(
             (totAdults.minus1 >= FET) ~ (totAdults.minus1 / area),
             (totAdults.minus1 < FET) ~ 0),
           is.WolfReduce.minus1 = is.WolfReduce,
           # is.WolfReduce = 0,
           is.MooseReduce.minus1 = is.MooseReduce,
           # is.MooseReduce = 0,
           is.Pen.minus1 = is.Pen,
           # is.Pen = 0,
           is.Feed.minus1 = is.Feed,
           # is.Feed = 0,
           pen.effect = log(1 + (is.Pen.minus1 / totAdults.minus2)))
  
  valid.indices <- which(mydat.y$totAdults.minus1 >= FET &
                           mydat.y$totAdults.minus2 >= FET) 
  mydat.y$gam_1.pred[valid.indices] <-
    predict(gam_1, newdata = mydat.y[valid.indices, ])
  r.expected <- predict(gam_2, newdata = mydat.y[valid.indices, ], se.fit = TRUE)
  r <- rnorm(n = length(valid.indices),
             mean = r.expected$fit,
             sd = r.expected$se.fit * sqrt(n))
  invalid.valid.indices <- which((r > r.max) | (r < r.min))
  while (length(invalid.valid.indices) > 0) {
    r[invalid.valid.indices] <- rnorm(n = length(invalid.valid.indices),
                                      mean = r.expected$fit[invalid.valid.indices],
                                      sd = r.expected$se.fit[invalid.valid.indices] * sqrt(n))
    invalid.valid.indices <- which((r > r.max) | (r < r.min))
  }
  
  mydat.y$totAdults[valid.indices] <-
    round(exp(r) * mydat.y$totAdults.minus1[valid.indices])
  # mydat.y$totAdults[valid.indices] <-
  #   rpois(n = length(valid.indices), lambda =
  #           exp(r) * mydat.y$totAdults.minus1[valid.indices])
  mydat.y$r[valid.indices] <- log(mydat.y$totAdults[valid.indices] /
                                    mydat.y$totAdults.minus1[valid.indices])
  mydat <- rbind(mydat, mydat.y)
  
}
# boxplot(totAdults ~ yrs, data = mydat, subset = herd == "Columbia North",
#         ylim = c(0, 1000), ylab = "Adult Females", xlab = "Year")
# boxplot(totAdults ~ yrs, data = mydat, subset = herd == "Hart North",
#         ylim = c(0, 1000), ylab = "Adult Females", xlab = "Year")
# boxplot(totAdults ~ yrs, data = mydat, subset = herd == "A La Peche",
#         ylim = c(0, 1000), ylab = "Adult Females", xlab = "Year")
# boxplot(totAdults ~ yrs, data = mydat, subset = herd == "Hart South",
#         ylim = c(0, 1000), ylab = "Adult Females", xlab = "Year")
# boxplot(totAdults ~ yrs, data = mydat, subset = herd == "Itcha-Ilgachuz",
#         ylim = c(0, 1000), ylab = "Adult Females", xlab = "Year")
# boxplot(totAdults ~ yrs, data = mydat, subset = herd == "Takla",
#         ylim = c(0, 100), ylab = "Adult Females", xlab = "Year")


FE.prob.30yr <- aggregate(mydat$totAdults[mydat$yrs == 2053] < FET,
                          FUN = mean, by = list(mydat$herd[mydat$yrs == 2053]))
viable.herds <- FE.prob.30yr$Group.1[FE.prob.30yr$x < 0.5]
results <- data.frame("herd" = viable.herds,
                      "N.pre_removal.lower95" = rep(NA, length(viable.herds)),
                      "N.pre_removal.median" = rep(NA, length(viable.herds)),
                      "N.pre_removal.upper95" = rep(NA, length(viable.herds)),
                      "r.pre_removal.lower95" = rep(NA, length(viable.herds)),
                      "r.pre_removal.median" = rep(NA, length(viable.herds)),
                      "r.pre_removal.upper95" = rep(NA, length(viable.herds)),
                      "N.post_removal.lower95" = rep(NA, length(viable.herds)),
                      "N.post_removal.median" = rep(NA, length(viable.herds)),
                      "N.post_removal.upper95" = rep(NA, length(viable.herds)),
                      "r.post_removal.lower95" = rep(NA, length(viable.herds)),
                      "r.post_removal.median" = rep(NA, length(viable.herds)),
                      "r.post_removal.upper95" = rep(NA, length(viable.herds)),
                      "r.5yr.lower95" = rep(NA, length(viable.herds)),
                      "r.5yr.median" = rep(NA, length(viable.herds)),
                      "r.5yr.upper95" = rep(NA, length(viable.herds)),
                      "time2recovery.mean" = rep(NA, length(viable.herds)),
                      "time2recovery.upper50" = rep(NA, length(viable.herds)),
                      "time2recovery.upper95" = rep(NA, length(viable.herds)))
for (h in 1:nrow(results)) {
  
  mydat.h.2024 <- mydat %>% filter(herd == results[h, "herd"], yrs == 2024)
  results[h, c("N.pre_removal.lower95",
               "N.pre_removal.median",
               "N.pre_removal.upper95")] <-
    round(quantile(mydat.h.2024$totAdults, probs = c(0.025, 0.5, 0.975)))
  results[h, c("r.pre_removal.lower95",
               "r.pre_removal.median",
               "r.pre_removal.upper95")] <-
    quantile(mydat.h.2024$r, probs = c(0.025, 0.5, 0.975))
  
  mydat.h.2027 <- mydat %>% filter(herd == results[h, "herd"], yrs == 2027)
  results[h, c("N.post_removal.lower95",
               "N.post_removal.median",
               "N.post_removal.upper95")] <-
    round(quantile(mydat.h.2027$totAdults, probs = c(0.025, 0.5, 0.975)))
  results[h, c("r.post_removal.lower95",
               "r.post_removal.median",
               "r.post_removal.upper95")] <-
    quantile(mydat.h.2027$r, probs = c(0.025, 0.5, 0.975))
  
  mydat.h.2023 <- mydat %>% filter(herd == results[h, "herd"], yrs == 2023)
  mydat.h.2028 <- mydat %>% filter(herd == results[h, "herd"], yrs == 2028)
  r.5yr <- log(mydat.h.2028$totAdults / mydat.h.2023$totAdults) / 5
  results[h, c("r.5yr.lower95",
               "r.5yr.median",
               "r.5yr.upper95")] <-
    quantile(r.5yr, probs = c(0.025, 0.5, 0.975))
  
  mydat.h <- mydat %>% filter(herd == results[h, "herd"], yrs > 2026)
  recoverd <- data.frame(year = 2027:2124, n = 0)
  for (y in 2027:2124) {
    mydat.h.y <-  mydat.h %>% filter(yrs == y) %>%
      mutate(totAdults.diff = totAdults - mydat.h.2024$totAdults)
    recoverd.draws <- mydat.h.y$.draw[mydat.h.y$totAdults.diff >=
                                        (x.2025 + x.2026)]
    recoverd$n[recoverd$year == y] <- length(recoverd.draws)
    mydat.h <- mydat.h %>% filter(!(.draw %in% recoverd.draws))
    mydat.h.2024 <- mydat.h.2024 %>% filter(!(.draw %in% recoverd.draws))
  }
  results[h, "time2recovery.mean"] <-
    sum((recoverd$year - 2024) * recoverd$n, na.rm = T) /
    sum(recoverd$n, na.rm = T)
  results[h, "time2recovery.upper50"] <- which(cumsum(recoverd$n) >= (0.5 * n))[1] + 1
  results[h, "time2recovery.upper95"] <- which(cumsum(recoverd$n) >= (0.95 * n))[1] + 1
  
}
write.csv(results, 'Data/results_DD_remove3in2025_remove3in2026_20240709.csv')


