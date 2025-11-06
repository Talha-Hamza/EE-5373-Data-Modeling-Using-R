# Read the data from the csv file.
processors <- read.csv(file.choose())

################################################################
#
# This function returns the data from the desired column.
# Example:  clock<-get_column("Fp2000","Processor.Clock..MHz.")

get_column <- function(x,y) {
  
  # x = string with the name of the desired benchmark
  # y = desired column
  #
  # Find the indices of all rows that have an entry for the  
  # indicated benchmark
  benchmark <- paste(paste("Spec",x,sep=""),"..average.base.",
                     sep="")
  ix <- !is.na(processors[,benchmark])
  return(processors[ix,y])
}
################################################################

################################################################
# This function extracts the interesting data columns for the given benchmark
# program and returns a dataframe with these columns.

extract_data <- function(benchmark) {
  
  temp <- paste(paste("Spec",benchmark,sep=""),"..average.base.", sep="")
  
  # perf = the performance reported in the database
  perf <- get_column(benchmark,temp)
  
  #nperf = performance normalized to the overall range
  max_perf <- max(perf)
  min_perf <- min(perf)
  range <- max_perf - min_perf
  nperf <- 100 * (perf - min_perf) / range
  
  clock <- get_column(benchmark,"Processor.Clock..MHz.")
  threads <- get_column(benchmark,"Threads.core")
  cores <- get_column(benchmark,"Cores")
  TDP <- get_column(benchmark,"TDP")
  transistors <- get_column(benchmark,"Transistors..millions.")
  dieSize <- get_column(benchmark,"Die.size..mm.2.")
  voltage <- get_column(benchmark,"Voltage..low.")
  featureSize <- get_column(benchmark,"Feature.Size..microns.")
  channel <- get_column(benchmark,"Channel.length..microns.")
  FO4delay <- get_column(benchmark,"FO4.Delay..ps.")
  L1icache <- get_column(benchmark,"L1..instruction...on.chip.")
  L1dcache <- get_column(benchmark,"L1..data...on.chip.")
  L2cache <- get_column(benchmark,"L2..on.chip.")
  L3cache <- get_column(benchmark,"L3..on.chip.")
  
  return(data.frame(nperf,perf,clock,threads,cores,TDP,transistors,dieSize,voltage,featureSize,channel,FO4delay,L1icache,L1dcache,L2cache,L3cache))
  
}
################################################################
# Extract a new data frame for each of the benchmark programs available in the data set.

int92.dat <- extract_data("Int1992")
fp92.dat <- extract_data("Fp1992")
int95.dat <- extract_data("Int1995")
fp95.dat <- extract_data("Fp1995")
int00.dat <- extract_data("Int2000")
fp00.dat <- extract_data("Fp2000")
int06.dat <- extract_data("Int2006")
fp06.dat <- extract_data("Fp2006")

# ----------- Example given:--------------------------
# # set.seed(1234) # suppress as needed 
# rows <- nrow(int00.dat)
# f <- 0.5 # fraction going towards training
# 
# # Splitting data set
# upper_bound <- floor(f * rows)
# permuted_int00.dat <- int00.dat[sample(rows), ]
# train.dat <- permuted_int00.dat[1:upper_bound, ]
# test.dat <- permuted_int00.dat[(upper_bound+1):rows, ]
# 
# # Built dataset
# int00_new.lm <- lm(nperf ~ clock + cores + voltage + channel +
#                      L1icache + sqrt(L1icache) + L1dcache + sqrt(L1dcache) +
#                      L2cache + sqrt(L2cache), data = train.dat)
# 
# predicted.dat <- predict(int00_new.lm, newdata=test.dat)
# delta <- predicted.dat- test.dat$nperf
# t.test(delta, conf.level = 0.95)
# plot(delta)

#--------------------------------- PART 1 --------------------------------------
# int95 model testing
# f_values <- seq(0.1, 0.9, by = 0.1)
# mean_vals <- numeric(length(f_values))
# lower_vals <- numeric(length(f_values))
# upper_vals <- numeric(length(f_values))
# k <- 100
# 
# for (i in seq_along(f_values)) {
#   f <- f_values[i]
#   rows <- nrow(int95.dat)
#   N <- floor((1 - f) * rows)
#   D_f <- c()
# 
#   for (j in 1:k) {
#     perm <- sample(rows)
#     upper_bound <- floor(f * rows)
#     train <- int95.dat[perm[1:upper_bound], ] # change the data set here
#     test  <- int95.dat[perm[(upper_bound+1):rows], ] # change the data set here
# 
#     # Change the model as appropriate
#     model <- lm(nperf~clock+
#                   dieSize+voltage+featureSize+
#                   FO4delay+L1icache+sqrt(L1icache), data = train)
# 
#     pred <- predict(model, newdata = test)
#     delta <- test$nperf - pred
#     D_f <- c(D_f, delta)
#   }
# 
#   ci <- t.test(D_f, conf.level = 0.95)
#   mean_vals[i]  <- ci$estimate
#   lower_vals[i] <- ci$conf.int[1]
#   upper_vals[i] <- ci$conf.int[2]
# }
# 
# # Plot mean ± CI vs. f for dataset chosen
# plot(f_values, mean_vals, ylim = range(c(lower_vals, upper_vals)),
#      xlab = "Training fraction (f)", ylab = "Mean prediction error",
#      main = "int95 benchmark")
# arrows(f_values, lower_vals, f_values, upper_vals, length = 0.05, angle = 90, code = 3)
# abline(h = 0, col = "lightgreen", lty = 2, lwd = 2)

#-------------------------------- int06 data -----------------------------------
# int06 model testing
# f_values <- seq(0.1, 0.9, by = 0.1)
# mean_vals <- numeric(length(f_values))
# lower_vals <- numeric(length(f_values))
# upper_vals <- numeric(length(f_values))
# k <- 100
# 
# for (i in seq_along(f_values)) {
#   f <- f_values[i]
#   rows <- nrow(int06.dat)
#   N <- floor((1 - f) * rows)
#   D_f <- c()
#   
#   for (j in 1:k) {
#     perm <- sample(rows)
#     upper_bound <- floor(f * rows)
#     train <- int06.dat[perm[1:upper_bound], ] # change the data set here
#     test  <- int06.dat[perm[(upper_bound+1):rows], ] # change the data set here
#     
#     # Change the model as appropriate 
#     model <- lm(nperf~clock+
#                   transistors +voltage+featureSize+channel+
#                   FO4delay+L1icache+L2cache+sqrt(L2cache)+L3cache+sqrt(L3cache), data = train)
#     
#     pred <- predict(model, newdata = test)
#     delta <- test$nperf - pred
#     D_f <- c(D_f, delta)
#   }
#   
#   ci <- t.test(D_f, conf.level = 0.95)
#   mean_vals[i]  <- ci$estimate
#   lower_vals[i] <- ci$conf.int[1]
#   upper_vals[i] <- ci$conf.int[2]
# }
# 
# # Plot mean ± CI vs. f for dataset chosen
# plot(f_values, mean_vals, ylim = range(c(lower_vals, upper_vals)),
#      xlab = "Training fraction (f)", ylab = "Mean prediction error",
#      main = "int06 benchmark")
# arrows(f_values, lower_vals, f_values, upper_vals, length = 0.05, angle = 90, code = 3)
# abline(h = 0, col = "lightgreen", lty = 2, lwd = 2)

#-------------------------------- fp95 data -----------------------------------
# # fp95 model testing
# f_values <- seq(0.1, 0.9, by = 0.1)
# mean_vals <- numeric(length(f_values))
# lower_vals <- numeric(length(f_values))
# upper_vals <- numeric(length(f_values))
# k <- 100
# 
# for (i in seq_along(f_values)) {
#   f <- f_values[i]
#   rows <- nrow(fp95.dat)
#   N <- floor((1 - f) * rows)
#   D_f <- c()
# 
#   for (j in 1:k) {
#     perm <- sample(rows)
#     upper_bound <- floor(f * rows)
#     train <- fp95.dat[perm[1:upper_bound], ] # change the data set here
#     test  <- fp95.dat[perm[(upper_bound+1):rows], ] # change the data set here
# 
#     # Change the model as appropriate
#     model <- lm(nperf~clock+
#                   transistors +dieSize+voltage+
#                   sqrt(L1icache)+L1dcache, data = train)
# 
#     pred <- predict(model, newdata = test)
#     delta <- test$nperf - pred
#     D_f <- c(D_f, delta)
#   }
# 
#   ci <- t.test(D_f, conf.level = 0.95)
#   mean_vals[i]  <- ci$estimate
#   lower_vals[i] <- ci$conf.int[1]
#   upper_vals[i] <- ci$conf.int[2]
# }
# 
# # Plot mean ± CI vs. f for dataset chosen
# plot(f_values, mean_vals, ylim = range(c(lower_vals, upper_vals)),
#      xlab = "Training fraction (f)", ylab = "Mean prediction error",
#      main = "fp95 benchmark")
# arrows(f_values, lower_vals, f_values, upper_vals, length = 0.05, angle = 90, code = 3)
# abline(h = 0, col = "lightgreen", lty = 2, lwd = 2)

#-------------------------------- fp06 data -----------------------------------
# f_values <- seq(0.1, 0.9, by = 0.1)
# mean_vals <- numeric(length(f_values))
# lower_vals <- numeric(length(f_values))
# upper_vals <- numeric(length(f_values))
# k <- 100
# 
# for (i in seq_along(f_values)) {
#   f <- f_values[i]
#   rows <- nrow(fp06.dat)
#   N <- floor((1 - f) * rows)
#   D_f <- c()
#   
#   for (j in 1:k) {
#     perm <- sample(rows)
#     upper_bound <- floor(f * rows)
#     train <- fp06.dat[perm[1:upper_bound], ] # change the data set here
#     test  <- fp06.dat[perm[(upper_bound+1):rows], ] # change the data set here
#     
#     # Change the model as appropriate
#     model <- lm(nperf~clock+
#                   transistors+featureSize+channel+
#                   FO4delay+L1icache+L2cache+sqrt(L2cache)+L3cache+sqrt(L3cache), data = train)
#     
#     pred <- predict(model, newdata = test)
#     delta <- test$nperf - pred
#     D_f <- c(D_f, delta)
#   }
#   
#   ci <- t.test(D_f, conf.level = 0.95)
#   mean_vals[i]  <- ci$estimate
#   lower_vals[i] <- ci$conf.int[1]
#   upper_vals[i] <- ci$conf.int[2]
# }
# 
# # Plot mean ± CI vs. f for dataset chosen
# plot(f_values, mean_vals, ylim = range(c(lower_vals, upper_vals)),
#      xlab = "Training fraction (f)", ylab = "Mean prediction error",
#      main = "fp06 benchmark")
# arrows(f_values, lower_vals, f_values, upper_vals, length = 0.05, angle = 90, code = 3)
# abline(h = 0, col = "lightgreen", lty = 2, lwd = 2)

# Using INT2006 model to predict FP2006

# int95.lm<-lm(nperf~clock+
#                     dieSize+voltage+featureSize+
#                     FO4delay+L1icache+sqrt(L1icache),data=int95.dat)
# 
# 
# predicted.dat <- predict(int95.lm, newdata=fp95.dat)
# 
# delta <- predicted.dat- fp95.dat$nperf
# 
# t.test(delta, conf.level = 0.95)
# 
# plot(delta, main = "INT95 lm used to predict from FP95 data")
# 
# mean(delta, na.rm = TRUE)
