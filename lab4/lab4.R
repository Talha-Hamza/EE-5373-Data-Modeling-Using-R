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
# Pair-wise visulaization:
pairs(int00.dat,
      gap=0.5)
# Step 1
int00.lm.full<-lm(nperf~clock+threads+cores+
                    transistors +dieSize+voltage+featureSize+channel+
                    FO4delay+L1icache+sqrt(L1icache)+L1dcache+
                    sqrt(L1dcache)+L2cache+sqrt(L2cache),data=int00.dat)

summary(int00.lm.full)
# Step 2 - Backward elimination
int00.lm.2<-lm(nperf~clock+cores+
                    voltage+channel+
                    L1icache+sqrt(L1icache)+L1dcache+
                    sqrt(L1dcache)+L2cache+sqrt(L2cache),data=int00.dat)

summary(int00.lm.2)
par(mfrow=c(2,2))
plot(int00.lm.2)


# ----------- Model 1: int95--------------------------
# Pair-wise visualization:
int95.dat["L3cache"] <- NULL
pairs(int95.dat,
      gap=0.5)
# Step 1
int95.lm.full<-lm(nperf~clock+
                    dieSize+voltage+featureSize+
                    FO4delay+L1icache+sqrt(L1icache),data=int95.dat)

summary(int95.lm.full)
par(mfrow=c(2,2))
plot(int95.lm.full)

# ----------- Model 2: int06--------------------------
# Pair-wise visualization:
pairs(int06.dat,
      gap=0.5)

int06.lm.full<-lm(nperf~clock+
                    transistors +voltage+featureSize+channel+
                    FO4delay+L1icache+L2cache+sqrt(L2cache)+L3cache+sqrt(L3cache),data=int06.dat)
summary(int06.lm.full)

par(mfrow=c(2,2))
plot(int06.lm.full)

# ----------- Model 3: fp95--------------------------

fp95.dat["L3cache"] <- NULL

pairs(fp95.dat,
      gap=0.5)

fp95.lm.full<-lm(nperf~clock+
                   transistors +dieSize+voltage+
                   sqrt(L1icache)+L1dcache,data=fp95.dat)
summary(fp95.lm.full)

par(mfrow=c(2,2))
plot(fp95.lm.full)

# ----------- Model 4: fp06--------------------------
pairs(fp06.dat,
      gap=0.5)

fp06.lm.full<-lm(nperf~clock+
                    transistors+featureSize+channel+
                    FO4delay+L1icache+L2cache+sqrt(L2cache)+L3cache+sqrt(L3cache),data=fp06.dat)
summary(fp06.lm.full)

par(mfrow=c(2,2))
plot(fp06.lm.full)

