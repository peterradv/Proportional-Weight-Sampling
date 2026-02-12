#### LOADING REQUIRED PACKAGES ####

packages <- c("dplyr", "tibble", "ggplot2", "purrr", "mgcv", "tidymv",
              "SamplingBigData", "BalancedSampling", "RColorBrewer",
              "animation", "gridExtra", "ggpubr", "sp", "metR")
lapply(packages, library, character.only = TRUE)

#### SCENARIO SETTINGS ####

# total sample size
samps <- paste0(240, "samps\\") # number of samples to take over the whole period

# sampling frequency
spe <- 48 # samples per event
noe <- 5 # number of events
freq <- paste0(spe, 'x', noe, "\\") # frequency

#### SAVING/LOADING PATHS ####

# saving path
path0 <- "results\\"
# subfolder based on sampling frequency - should be created before running
path <- paste0(path0, samps, noe, '\\')

# loading plume data
load("data//complex_plume.RData")

# loading well coordinates
load("data//rand_24.RData")

#### TRIMMING DATA (OPTIONAL) ####
# to work with the convex hull only (area enclosed by wells) instead of the whole spatial domain, enable next part:

# well coordinates without well.id
ps <- cbind(well_coords$X1, well_coords$X2)

# creating polygon of wells and checking which data points are inside
true.data.inpoly <- point.in.polygon(true.data$X1, true.data$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])

# attaching indicator (in poly or not) to true.data
true.data.inpoly <- as.data.frame(cbind(true.data, true.data.inpoly))

# filtering data for points that are within the poly
true.data <- true.data.inpoly %>% filter(true.data.inpoly > 0)

# removing indicator column
true.data <- true.data %>% select(-true.data.inpoly)

# checking plot of convex hull
# ggplot(data = true.data %>% filter(Time == 3467.5), aes(x=X1, y=X2)) +
#   geom_point(aes(color=log(y)))

#### DATA PREPARATION ####

# time slices
ts <- unique(true.data$Time)

# splitting time slices into past and future sampling events
ts1 <- ts[1:10]
td2 <- ts[11:20]

# selecting future time slices based on number of desired sampling events
if (noe == 10){
  
  ts2 <- ts[11:20]
  
} else if (noe == 8) {
  
  ts2 <- c(ts[11], ts[13], ts[14], ts[15], ts[16], ts[17], ts[18], ts[20])
  
} else if (noe == 6) {
  
  ts2 <- c(ts[11], ts[13], ts[15], ts[17], ts[19], ts[20])
  
} else if (noe == 5) {
  
  ts2 <- c(ts[11], ts[13], ts[15], ts[17], ts[20])
  
} else if (noe == 4) {
  
  ts2 <- c(ts[11], ts[14], ts[17], ts[20])
  
} else if (noe == 3) {
  
  ts2 <- c(ts[11], ts[15], ts[20])
  
} else if (noe == 2) {
  
  ts2 <- c(ts[11], ts[20])
  
} else {break}

# total number of wells
W = nrow(well_coords)  # 48 wells, 1 sampling time

# number of wells to be sampled per event
div <- W/spe # number of wells / samples per event
w = W/div 

# checking total number of samples
length(ts2)*w

#### GENERATING OBSERVATIONS FROM TRUE DATA ####

# splitting data into past and future
true.data1 <- true.data %>% filter(Time %in% ts1)
true.data2 <- true.data %>% filter(Time %in% td2)

# filtering data by wells to get past observations
obs <- left_join(well_coords, true.data1)

# store original observations without noise
obs.og <- obs

# all potential future observations
obs.fut <- left_join(well_coords, true.data2)

# storing original values without noise
obs.fut.og <- obs.fut

# adding measurement noise
snr <- 0.15 # amount: 15% 

# past observations
obs$y <- obs$y * rnorm(nrow(obs), 1, snr)

# future observations
obs.fut$y <- obs.fut$y * rnorm(nrow(obs.fut), 1, snr)

#### SHORTEST DISTANCE COMPUTING FUNCTION ####

# calculates euclidiean distance between two sets of x,y coordinates
euclidean_dist <- function(P1, P2) sqrt(sum((P1 - P2)^2))

# uses the euclidean_dist function to find the closest point from a list of coordinates
closest <- function(data, point) {
  
  distances <- double()
  for (i in 1:nrow(data)) {
    dist <- euclidean_dist(P1=c(data[i,1], data[i,2]), P2=point)
    distances <- c(dist, distances)
  }
  
  min <- which.min(distances)
  
  closest.p <- cbind(data[min,1:2], min(distances))
  colnames(closest.p) <- c("X1", "X2", "D")
  
  return(closest.p)
}

# calculates the shortest distance between the plume and the well based on a concentration limit
plumedist <- function(data, point, plume.limit) {
  
  distances <- double()
  
  for (i in 1:length(ts1)) {
    
    sub.data <- data %>% 
      filter(Time == ts1[i]) %>% 
      filter(y >= plume.limit)
    
    time <- ts1[i]
    dist <- closest(data = sub.data, point = point)
    dist <- cbind(time, dist)
    distances <- rbind(dist, distances)
  }
  return(distances)
}

#### MODEL OF PAST OBSERVATIONS ####

# loading smst modeling functions
source("data//sm-st.R",
       chdir = TRUE)

# model settings
nseg = c(6,6,6) # number of splines in 3 dimensions
bdeg = 2 # type of spline: quadtratic (2), cubic (3)
pord = 1 # penalty type: first order derivatives (1), second order derivatives (2)

# model of past observations
hist.mod <- sm.st(
  x = obs[,2:3],
  t = obs[,4],
  y = log(obs[,5]),
  lambda = log.post,
  nseg = nseg,
  bdeg = bdeg,
  pord = pord
)

# Creating B matrices 
# - this way is faster than using the predict function every time

hist.xrange <- rbind(c(1, 100), 
                c(1, 35), 
                c(0, 1642.5))

fut.xrange <- rbind(c(1, 100),
                    c(1, 35),
                    c(1825, 3467.5))

rownames(hist.xrange) <- c("X1", "X2", "Time")

rownames(fut.xrange) <- c("X1", "X2", "Time")

# B matrix of past observations
hist.B <- st.matrices(true.data1[,1:3], 
                    xrange = hist.xrange, 
                    nseg = nseg,
                    bdeg = bdeg,
                    pord = pord)

# B matrix of future observations
fut.B <- st.matrices(true.data2[,1:3],
                     xrange = fut.xrange,
                     nseg = nseg,
                     bdeg = bdeg,
                     pord = pord)

# model predictions of past observations
hist.preds <- hist.B$B %*% hist.mod$alpha

# binding predictions to original data and removing true values
hist.preds <- cbind(true.data1, hist.preds) %>%
  subset(select = -y)
colnames(hist.preds)[4] <- 'y'

#### CALCULATING PAST WELL-PLUME DISTANCES ####

# plume delineation limit:
  # in data: 0-100 - assume it means % of max solubility
  # Benzene: max solubility at 9C is 1810 mg/L
  # allowed limit in GW by WHO = 0.01 mg/L -> that is at 0.002% of max
limit = 0.002

# empty list to collect distances within loop
all_dist <- list()

# loop through past observations and wells to compute distances
for (t in 1:nrow(well_coords)) {
  
  P <- c(well_coords[t,2], well_coords[t,3])
  
  all.dist <- plumedist(data = hist.preds, point = P, plume.limit = log(limit))
  
  all_dist[[t]] <- all.dist
  
}

# looking at distribution of distances
Ds <- double()

for (r in 1:length(all_dist)) {
  Ds.temp <- as.data.frame(all_dist[[r]]$D)
  Ds <- rbind(Ds, Ds.temp)
}

colnames(Ds) <- "D"

# sometimes there are NAs when plume cannot be deliniated at time = 0
Ds <- na.omit(Ds)

# mean distance
mD <- mean(Ds$D)

# median distance
meD <- median(Ds$D)

# maximum distance
max.dist <- max(Ds)

#### FORECASTING FUTURE WELL PLUME DISTANCES ####

# future sampling times
new.data <- as.data.frame(ts2)
colnames(new.data) <- "time"

# creating empty lists for collecting distance predictions and standard errors within the loop
all.d.preds <- list()
all.se <- list()

# forecasting distances based on past time-series using GLMs
for (p in 1:length(all_dist)) {
  
  # creating time series data
  data <- as.data.frame(cbind(all_dist[[p]]$time, all_dist[[p]]$D)) %>%
    map_df(rev)
  colnames(data) <- c("time", "D")
  
  # GLM model
  model <- glm(D+1 ~ time, data = data, family = gaussian(link = "log"))
  
  # computing predictions
  preds <- predict.glm(model, newdata = new.data, type = "response", se.fit = TRUE)
  # removing 1 added in the model
  preds$fit <- preds$fit-1
  # negative distances should just be 0s
  preds$fit[preds$fit < 0] <- 0
  
  # saving preds
  all.d.preds[[p]] <- preds$fit

  # saving standard errors
  all.se[[p]] <- preds$se.fit
  
} 

#### CREATING AND SCALING KERNEL FUNCTION ####

# standard errors for tuning sigma
unlist.se <- unlist(all.se)
# median or max
median.se <- median(unlist.se)
max.se <- max(unlist.se)

# tuning sigma - controls how quickly the function drops to 1
s <- median.se

# tuning y-scalar - controls the height of the function, for half-normal distribution its pi
ep <- rep(w/W, W)
y_scaler <- ep[1] # controls the height of the function, for half-normal distribution its pi

# half-normal kernel function
k_function <- function(x){(1/y_scaler) * (sqrt(2)/(sqrt(pi)))*exp(-((x^2)/(2*s^2)))}

# saving png of kernel function to save path
png(paste0(path, "kernel.png"), width = 620, height = 400)

# plotting kernel
ggplot(data = data.frame(x=c(0,max.dist)), aes(x=x)) +
  stat_function(fun = k_function)

dev.off()

#### TUNING PARAMETERS FROM KERNEL FUNCTION ####

# empty list for collecting tuning parameters in the loop
all.t.pars <- list()

# calculating tuning parameters
for (o in 1:length(all.d.preds)) {
  
  t.pars <- double()
  
  for (w1 in 1:length(all.d.preds[[o]])) {
   
    # tuning params from kernel function; add 1 to avoid numbers <1
    
    t.par <- 1 + k_function(all.d.preds[[o]][w1])
    
    t.pars <- rbind(t.pars, t.par)
     
  }
  
  all.t.pars[[o]] <- t.pars
  
}

#### TUNING THE INCLUSION PROBABILITIES ####

# empty list for collecting inclusion probs
ups <- list()

# computing inclusion probs
for (m in 1:length(ts2)) {
  
  tps <- double()
  
  for (n in 1:length(all.t.pars)) {
    tp <- all.t.pars[[n]][m,]
    tps <- rbind(tps, tp)
  }
  
  # scaling probs by tuning params
  sp <- ep * tps
  
  # constant for re-scaling
  c <- sum(ep)/sum(sp)
  
  # re-scaling weigths so they sum to required number of samples
  up <- sp*c
  
  # adding to list
  ups[[m]] <- up
  
}

#### SAMPLING SIMULATION LOOP ####

# the number of wells to be sampled in each slice  
samp.well.nr <- w

# list of wells for sampling
well.list <- cbind(well_coords$well.id, well_coords$X1, well_coords$X2)

# results will be collected here in the loop
# rmse 
results <- data.frame(double())
# rmse only in plume area
results.plume <- data.frame(double())
# rmse outside plume area
results.out <- data.frame(double())
# ratio of samples from plume
in.ratios <- data.frame(double())
# spatial balance values
sbs <- data.frame(double())
# plume mass (sum of concentrations)
pmass <- data.frame(double())

# collecting samples
# eLPM
lpm.app2.samples <- list()
# SRS
rand.app2.samples <- list()
# pLPM
lpm.up.samples <- list()

# runnning simulations
for (i in 1:100) {
  
  # all potential samples ####################################################
  
  # spatial balance when taking all samples (for comparison)
  all.sb <- BalancedSampling::sb(p=rep(nrow(well_coords)/nrow(well_coords), nrow(well_coords)), x=cbind(well_coords$X1, well_coords$X2), s=c(well_coords$well.id))
  # all sb values
  all.sbs <- rep(all.sb, length(ts2))

  # model of future observations
  all.model <- sm.st(
    x = obs.fut[,2:3],
    t = obs.fut[,4],
    y = log(obs.fut[,5]),
    lambda = log.post,
    nseg = nseg,
    bdeg = bdeg,
    pord = pord
  )

  # calculating predictions
  all.preds <- fut.B$B %*% all.model$alpha

  # calculating rmse for entire surface
  all.rmspe <- sqrt(sum((log(true.data2$y) - all.preds)^2)/nrow(all.preds))
  
  # calculating rmse for only the observations
  all.obs.preds <- predict.smst(all.model)
  all.rmse <- sqrt(sum((log(obs.fut$y) - all.obs.preds)^2)/length(all.obs.preds))
  
  # Plume area only #
  
  # filtering predictions for plume only
  all.plume.preds <- cbind(true.data2, all.preds) %>% filter(true.data2$y >= limit)

  # calculating rmse for plume only
  all.plume.rmspe <- sqrt(sum((log(all.plume.preds$y) - all.plume.preds$all.preds)^2)/nrow(all.plume.preds))
  
  # calculating plume mass
  all.pred.plume <- cbind(true.data2, all.preds) %>% filter(all.preds >= log(limit))
  all.pmass <- sum(exp(1)^all.pred.plume$all.preds)/3500
  
  # Area outside plume only #
  
  # filtering predictions
  all.out.preds <- cbind(true.data2, all.preds) %>% filter(true.data2$y < limit)  
  
  #calculating rmspe 
  all.out.rmspe <- sqrt(sum((log(all.out.preds$y) - all.out.preds$all.preds)^2)/nrow(all.out.preds))
  
  # eLPM ##################################################
  
  # equal inclusion probs
  group.p <- ep
  
  # samples will be collected in this df
  lpm.app2.sample <- data.frame()
  
  # spatial balance values will be collected in this df
  lpm.app2.sbs <- data.frame(double())
  
  # sampling different wells at each event using equal prob LPM
  for (k in 1:length(ts2)) {

    # randomly selecting wells to sample
    samp.wells <- as.data.frame(lpm2_kdtree(x=well.list, prob=group.p))
    colnames(samp.wells) <- "well.id"

    # at each time slice
    samp.time <- as.data.frame(ts2[k])
    colnames(samp.time) <- "Time"

    # selecting observations from the selected wells and at the selected time slice
    temp.samples <- left_join(samp.time, obs.fut)
    temp.samples <- left_join(samp.wells, temp.samples)

    # collecting samples from the different time slices
    lpm.app2.sample <- rbind(lpm.app2.sample, temp.samples)

    # calculating spatial balance
    lpm.app2.sb <- BalancedSampling::sb(p=ep, x=cbind(well_coords$X1, well_coords$X2), s=c(temp.samples$well.id))
    lpm.app2.sbs <- rbind(lpm.app2.sbs, lpm.app2.sb)

  }
  
  # collecting samples
  lpm.app2.samples[[i]] <- lpm.app2.sample
  colnames(lpm.app2.sbs) <- c("lpm.app2.sbs")
  
  # modelling samples
  lpm.app2.model <- sm.st(
    x = lpm.app2.sample[,3:4],
    t = lpm.app2.sample[,2],
    y = log(lpm.app2.sample[,5]),
    lambda = log.post,
    nseg = nseg,
    bdeg = bdeg,
    pord = pord
  )
  
  # calculating model predictions for surface
  lpm.app2.preds <- fut.B$B %*% lpm.app2.model$alpha
  
  # calculating rmse for entire surface
  lpm.app2.rmspe <- sqrt(sum((log(true.data2$y) - lpm.app2.preds)^2)/nrow(lpm.app2.preds))
  
  # calculating rmse for observations only
  lpm.app2.obs.preds <- predict.smst(lpm.app2.model)
  lpm.app2.rmse <- sqrt(sum((log(lpm.app2.sample$y) - lpm.app2.obs.preds)^2)/length(lpm.app2.obs.preds))
  
  # Plume area only #
  
  # filtering predictions for plume only
  lpm.app2.plume.preds <- cbind(true.data2, lpm.app2.preds) %>% filter(true.data2$y >= limit)
  
  # calculating rmse for plume only
  lpm.app2.plume.rmspe <- sqrt(sum((log(lpm.app2.plume.preds$y) - lpm.app2.plume.preds$lpm.app2.preds)^2)/nrow(lpm.app2.plume.preds))
  
  # plume mass
  lpm.app2.pred.plume <- cbind(true.data2, lpm.app2.preds) %>% filter(lpm.app2.preds >= log(limit))
  lpm.app2.pmass <- sum(exp(1)^lpm.app2.pred.plume$lpm.app2.preds)/3500
  
  # Area outside plume only #
  
  # filtering predictions
  lpm.app2.out.preds <- cbind(true.data2, lpm.app2.preds) %>% filter(true.data2$y < limit)
  
  #calculating rmse 
  lpm.app2.out.rmspe <- sqrt(sum((log(lpm.app2.out.preds$y) - lpm.app2.out.preds$lpm.app2.preds)^2)/nrow(lpm.app2.out.preds))
  
  # pLPM ################################################
  
  # samples will be collected in this data frame
  lpm.up.sample <- data.frame()
  
  # spatial balance values will be collected in this df
  lpm.up.sbs <- data.frame()
  
  # sampling different wells at each upcoming event using proportional prob LPM
  for (j in 1:length(ts2)) {

    # inclusion weights
    group.p <- c(ups[[j]])

    # randomly selecting wells to sample
    samp.wells <- as.data.frame(lpm2_kdtree(x=well.list, prob=group.p))
    colnames(samp.wells) <- "well.id"

    # at each time slice
    samp.time <- as.data.frame(ts2[j])
    colnames(samp.time) <- "Time"

    # selecting observations from the selected wells and at the selected time slice
    temp.samples <- left_join(samp.time, obs.fut)
    temp.samples <- left_join(samp.wells, temp.samples)

    # collecting samples from the different time slices
    lpm.up.sample <- rbind(lpm.up.sample, temp.samples)

    # calculating spatial balance
    lpm.up.sb <- sb(p=ep, x=cbind(well_coords$X1, well_coords$X2), s=c(temp.samples$well.id))
    lpm.up.sbs <- rbind(lpm.up.sbs, lpm.up.sb)

  }
  
  # collecting samples
  lpm.up.samples[[i]] <- lpm.up.sample
  colnames(lpm.up.sbs) <- c("lpm.up.sbs")
  
  # modelling samples
  lpm.up.model <- sm.st(
    x = lpm.up.sample[,3:4],
    t = lpm.up.sample[,2],
    y = log(lpm.up.sample[,5]),
    lambda = log.post,
    nseg = nseg,
    bdeg = bdeg,
    pord = pord
  )
  
  # calculating model predictions for surface
  lpm.up.preds <- fut.B$B %*% lpm.up.model$alpha
  
  # calculating rmse of entire surface
  lpm.up.rmspe <- sqrt(sum((log(true.data2$y) - lpm.up.preds)^2)/nrow(lpm.up.preds))
  
  # calculating rmse of observations only
  lpm.up.obs.preds <- predict.smst(lpm.up.model)
  lpm.up.rmse <- sqrt(sum((log(lpm.up.sample$y) - lpm.up.obs.preds)^2)/length(lpm.up.obs.preds))
  
  # Plume area only #
  
  # filtering predictions for plume only
  lpm.up.plume.preds <- cbind(true.data2, lpm.up.preds) %>% filter(true.data2$y >= limit)
  
  # calculating rmse for plume only
  lpm.up.plume.rmspe <- sqrt(sum((log(lpm.up.plume.preds$y) - lpm.up.plume.preds$lpm.up.preds)^2)/nrow(lpm.up.plume.preds))
  
  # plume mass
  lpm.up.pred.plume <- cbind(true.data2, lpm.up.preds) %>% filter(lpm.up.preds >= log(limit))
  
  lpm.up.pmass <- sum(exp(1)^lpm.up.pred.plume$lpm.up.preds)/3500
  
  # Area outside plume only #
  
  # filtering predictions
  lpm.up.out.preds <- cbind(true.data2, lpm.up.preds) %>% filter(true.data2$y < limit)
  
  #calculating rmspe 
  lpm.up.out.rmspe <- sqrt(sum((log(lpm.up.out.preds$y) - lpm.up.out.preds$lpm.up.preds)^2)/nrow(lpm.up.out.preds))
  
  # SRS ###############################################
  
  # samples will be collected in this data frame
  rand.app2.sample <- data.frame()

  # spatial balance values will be collected in this df
  rand.app2.sbs <- data.frame()
  
  # sampling different wells at each upcoming event using SRS
  for (u in 1:length(ts2)) {

    # randomly selecting wells to sample
    samp.wells <- as.data.frame(sample(well.list[,1], samp.well.nr))
    colnames(samp.wells) <- "well.id"

    # at each time slice
    samp.time <- as.data.frame(ts2[u])
    colnames(samp.time) <- "Time"

    # selecting observations from the selected wells and at the selected time slice
    temp.samples <- left_join(samp.time, obs.fut)
    temp.samples <- left_join(samp.wells, temp.samples)

    # collecting samples from the different time slices
    rand.app2.sample <- rbind(rand.app2.sample, temp.samples)

    # calculating spatial balance
    rand.app2.sb <- sb(p=ep, x=cbind(well_coords$X1, well_coords$X2), s=c(temp.samples$well.id))
    rand.app2.sbs <- rbind(rand.app2.sbs, rand.app2.sb)
  }
  
  # collecting samples
  rand.app2.samples[[i]] <- rand.app2.sample
  colnames(rand.app2.sbs) <- c("rand.app2.sbs")
  
  # modelling samples
  rand.app2.model <- sm.st(
    x = rand.app2.sample[,3:4],
    t = rand.app2.sample[,2],
    y = log(rand.app2.sample[,5]),
    lambda = log.post,
    nseg = nseg,
    bdeg = bdeg,
    pord = pord
  )
  
  # calculating predictions for surface
  rand.app2.preds <- fut.B$B %*% rand.app2.model$alpha
  
  # calculating rmse for entire surface
  rand.app2.rmspe <- sqrt(sum((log(true.data2$y) - rand.app2.preds)^2)/nrow(rand.app2.preds))
  
  # calculating rmse for observations only
  rand.app2.obs.preds <- predict.smst(rand.app2.model)
  rand.app2.rmse <- sqrt(sum((log(rand.app2.sample$y) - rand.app2.obs.preds)^2)/length(rand.app2.obs.preds))
  
  # Plume area only #
  
  # filtering predictions for plume only
  rand.app2.plume.preds <- cbind(true.data2, rand.app2.preds) %>% filter(true.data2$y >= limit)
  
  # calculating rmse for plume only
  rand.app2.plume.rmspe <- sqrt(sum((log(rand.app2.plume.preds$y) - rand.app2.plume.preds$rand.app2.preds)^2)/nrow(rand.app2.plume.preds))
  
  # plume mass
  rand.app2.pred.plume <- cbind(true.data2, rand.app2.preds) %>% filter(rand.app2.preds >= log(limit))
  rand.app2.pmass <- sum(exp(1)^rand.app2.pred.plume$rand.app2.preds)/3500
  
  # Area outside plume only #

  # filtering predictions
  rand.app2.out.preds <- cbind(true.data2, rand.app2.preds) %>% filter(true.data2$y < limit)
  
  # calculating rmse 
  rand.app2.out.rmspe <- sqrt(sum((log(rand.app2.out.preds$y) - rand.app2.out.preds$rand.app2.preds)^2)/nrow(rand.app2.out.preds))
  

  # Collecting results #############################################
  
  # surface rmse results
  results <- rbind(results, cbind(all.rmspe, all.rmse, rand.app2.rmspe, rand.app2.rmse, lpm.app2.rmspe, lpm.app2.rmse, lpm.up.rmspe, lpm.up.rmse))
  # plume rmse results
  results.plume <- rbind(results.plume, cbind(all.plume.rmspe, rand.app2.plume.rmspe, lpm.app2.plume.rmspe, lpm.up.plume.rmspe))
  # outside plume rmse results
  results.out <- rbind(results.out, cbind(all.out.rmspe, rand.app2.out.rmspe, lpm.app2.out.rmspe, lpm.up.out.rmspe))
  
  # spatial balance resutls
  sbs <- rbind(sbs, cbind(all.sbs, rand.app2.sbs, lpm.app2.sbs, lpm.up.sbs))
  
  # plume mass results
  pmass <- rbind(pmass, cbind(all.pmass, rand.app2.pmass, lpm.app2.pmass, lpm.up.pmass))
  
  # Ratio of samples from plume #######################################
  
  # all possoble samples from plume
  all.in.plume <- obs.fut %>%
    filter(y >= limit)
  all.ratio <- nrow(all.in.plume) / nrow(obs.fut)
  
  # SRS samples from plume
  rand.in.plume <- rand.app2.sample %>%
    filter(y >= limit)
  rand.ratio <- nrow(rand.in.plume) / nrow(rand.app2.sample)
  
  # eLPM samples from plume
  ep.in.plume <- lpm.app2.sample %>%
    filter(y >= limit)
  ep.ratio <- nrow(ep.in.plume) / nrow(lpm.app2.sample)
  
  # pLPM samples from plume
  up.in.plume <- lpm.up.sample %>%
    filter(y >= limit)
  up.ratio <- nrow(up.in.plume) / nrow(lpm.up.sample)
  
  # collecting ratio results
  in.ratios <- rbind(in.ratios, cbind(all.ratio, rand.ratio, ep.ratio, up.ratio))
}

#### SAVING RESULTS ####

results <- rowid_to_column(results)
save(results, file = paste0(path, 'rmspe_total.RData'))
sink(paste0(path, "summary_total.txt"))
print(summary(results))
sink()  # returns output to the console

results.plume <- rowid_to_column(results.plume)
save(results.plume, file = paste0(path, 'rmspe_plume.RData'))
sink(paste0(path, "summary_plume.txt"))
print(summary(results.plume))
sink()  # returns output to the console

results.out <- rowid_to_column(results.out)
save(results.out, file = paste0(path, 'rmspe_out.RData'))
sink(paste0(path, "summary_out.txt"))
print(summary(results.out))
sink()  # returns output to the console

in.ratios <- rowid_to_column(in.ratios)
save(in.ratios, file = paste0(path, 'in_ratios.RData'))

sbs <- rowid_to_column(sbs)
save(sbs, file = paste0(path, 'sbs.RData'))

pmass <- rowid_to_column(pmass)
save(pmass, file = paste0(path, 'pmass.RData'))

# saving samples
save(lpm.app2.samples, file = paste0(path, 'LPM_samples.RData'))
save(rand.app2.samples, file = paste0(path, 'SRS_samples.RData'))
save(lpm.up.samples, file = paste0(path, 'pLPM_samples.RData'))

#### OPTIONAL: PLOTTING SOME RESULTS ####

##### RMSE of entire surface ####

# lower bound
min <- min(
  c(
    min(results$all.rmspe), min(results$rand.app2.rmspe), min(results$lpm.app2.rmspe), min(results$lpm.up.rmspe)
  )
)
# upper bound
max <- max(
  c(
    max(results$all.rmspe), max(results$rand.app2.rmspe), max(results$lpm.app2.rmspe), max(results$lpm.up.rmspe)
  )
)
# eLPM box
ew_plot <- ggplot(data = results, mapping = aes(x = '', y = lpm.app2.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("equal weights") +
  theme(axis.title.y=element_blank()) 
# pLPM box
uw_plot <- ggplot(data = results, mapping = aes(x = '', y = lpm.up.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("prop. weights") +
  theme(axis.title.y=element_blank())
# SRS box
rand_plot <- ggplot(data = results, mapping = aes(x = '', y = rand.app2.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("simple random") +
  theme(axis.title.y=element_blank())
# all possible observations for reference
all_plot <- ggplot(data = results, mapping = aes(x = '', y = all.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("all pot. samples") +
  theme(axis.title.y=element_blank())
# grid layout
layout <- rbind(c(1,2,3,4))

# saving plot as png
png(paste0(path, "rmspe_total.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout, top = text_grob("RMSPE: total", size = 18))

dev.off()

#### RMSE of observations ####

min <- min(
  c(
    min(results$all.rmse), min(results$rand.app2.rmse), min(results$lpm.app2.rmse), min(results$lpm.up.rmse)
  )
)

max <- max(
  c(
    max(results$all.rmse), max(results$rand.app2.rmse), max(results$lpm.app2.rmse), max(results$lpm.up.rmse)
  )
)

ew_plot <- ggplot(data = results, mapping = aes(x = '', y = lpm.app2.rmse)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("equal weights") +
  theme(axis.title.y=element_blank()) 

uw_plot <- ggplot(data = results, mapping = aes(x = '', y = lpm.up.rmse)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("prop. weights") +
  theme(axis.title.y=element_blank())

rand_plot <- ggplot(data = results, mapping = aes(x = '', y = rand.app2.rmse)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("simple random") +
  theme(axis.title.y=element_blank())

all_plot <- ggplot(data = results, mapping = aes(x = '', y = all.rmse)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("all pot. samples") +
  theme(axis.title.y=element_blank())

layout <- rbind(c(1,2,3,4))

# filename
png(paste0(path, "rmse_total.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout, top = text_grob("RMSE: total", size = 18))

dev.off()

#### RMSE: Plume only ####

min <- min(
  c(
    min(results.plume$all.plume.rmspe), min(results.plume$rand.app2.plume.rmspe), min(results.plume$lpm.app2.plume.rmspe), min(results.plume$lpm.up.plume.rmspe)
  )
)

max <- max(
  c(
    max(results.plume$all.plume.rmspe), max(results.plume$rand.app2.plume.rmspe), max(results.plume$lpm.app2.plume.rmspe), max(results.plume$lpm.up.plume.rmspe)
  )
)

ew_plot <- ggplot(data = results.plume, mapping = aes(x = '', y = lpm.app2.plume.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("equal weights") +
  theme(axis.title.y=element_blank())

uw_plot <- ggplot(data = results.plume, mapping = aes(x = '', y = lpm.up.plume.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("prop. weights") +
  theme(axis.title.y=element_blank())

rand_plot <- ggplot(data = results.plume, mapping = aes(x = '', y = rand.app2.plume.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("simple random") +
  theme(axis.title.y=element_blank())

all_plot <- ggplot(data = results.plume, mapping = aes(x = '', y = all.plume.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("all pot. samples") +
  theme(axis.title.y=element_blank())

layout <- rbind(c(1,2,3,4))

# filename
png(paste0(path, "rmspe_plume.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout, top = text_grob("RMSPE: plume only", size = 18))

dev.off()

#### RMSE outside plume only ####

min <- min(
  c(
    min(results.out$all.out.rmspe), min(results.out$rand.app2.out.rmspe), min(results.out$lpm.app2.out.rmspe), min(results.out$lpm.up.out.rmspe)
  )
)

max <- max(
  c(
    max(results.out$all.out.rmspe), max(results.out$rand.app2.out.rmspe), max(results.out$lpm.app2.out.rmspe), max(results.out$lpm.up.out.rmspe)
  )
)

ew_plot <- ggplot(data = results.out, mapping = aes(x = '', y = lpm.app2.out.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("equal weights") +
  theme(axis.title.y=element_blank())

uw_plot <- ggplot(data = results.out, mapping = aes(x = '', y = lpm.up.out.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("prop. weights") +
  theme(axis.title.y=element_blank())

rand_plot <- ggplot(data = results.out, mapping = aes(x = '', y = rand.app2.out.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("simple random") +
  theme(axis.title.y=element_blank())

all_plot <- ggplot(data = results.out, mapping = aes(x = '', y = all.out.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("all pot. samples") +
  theme(axis.title.y=element_blank())

layout <- rbind(c(1,2,3,4))

# filename
png(paste0(path, "rmspe_out.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout, top = text_grob("RMSPE: without plume", size = 18))

dev.off()

#### Ratio of samples from plume ####

ew_plot <- ggplot(data = in.ratios, mapping = aes(x = '', y = ep.ratio)) +
  geom_boxplot() +
  ylim(0, 1) +
  ggtitle("eLPM") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

uw_plot <- ggplot(data = in.ratios, mapping = aes(x = '', y = up.ratio)) +
  geom_boxplot() +
  ylim(0, 1) +
  ggtitle("pLPM") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

rand_plot <- ggplot(data = in.ratios, mapping = aes(x = '', y = rand.ratio)) +
  geom_boxplot() +
  ylim(0, 1) +
  ggtitle("SRS") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

all_plot <- ggplot(data = in.ratios, mapping = aes(x = '', y = all.ratio)) +
  geom_boxplot() +
  ylim(0, 1) +
  ggtitle("All") +
  ylab('Ratio of samples from plume')+
  theme(axis.title.y=element_text(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

layout <- rbind(c(1,2,3,4))

# filename
png(paste0(path, "plume_ratio.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout)

dev.off()

#### Spatial balance ####

ew_plot <- ggplot(data = sbs, mapping = aes(x = '', y = lpm.app2.sbs)) +
  geom_boxplot() +
  ylim(0, 1.5) +
  ggtitle("eLPM") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

uw_plot <- ggplot(data = sbs, mapping = aes(x = '', y = lpm.up.sbs)) +
  geom_boxplot() +
  ylim(0, 1.5) +
  ggtitle("pLPM") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

rand_plot <- ggplot(data = sbs, mapping = aes(x = '', y = rand.app2.sbs)) +
  geom_boxplot() +
  ylim(0, 1.5) +
  ggtitle("SRS") +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

all_plot <- ggplot(data = sbs, mapping = aes(x = '', y = all.sbs)) +
  geom_boxplot() +
  ylim(0, 1.5) +
  ggtitle("All sample") +
  ylab('Spatial balance') +
  theme(axis.title.y=element_text(),
        axis.title.x=element_blank(),
        text = element_text(size = 20),
        axis.text.x = element_text())

layout <- rbind(c(1,2,3,4))

# filename
png(paste0(path, "spatial_balance.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout) #top = text_grob("spatial balance", size = 18))

dev.off()

#### Plume mass ####

# calculating real plume mass
real.plume <- true.data2 %>% filter(y >= limit)
real.pmass <- sum(real.plume$y)/3500

# different plot depending on under/over prediction
if (max(pmass[,2:5]) > real.pmass) {
  ew_plot <- ggplot(data = pmass, mapping = aes(x = '', y = lpm.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), max(pmass[,2:5])) +
    ggtitle("equal weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
  
  uw_plot <- ggplot(data = pmass, mapping = aes(x = '', y = lpm.up.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), max(pmass[,2:5])) +
    ggtitle("prop. weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
  
  rand_plot <- ggplot(data = pmass, mapping = aes(x = '', y = rand.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), max(pmass[,2:5])) +
    ggtitle("simple random") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
  
  all_plot <- ggplot(data = pmass, mapping = aes(x = '', y = all.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), max(pmass[,2:5])) +
    ggtitle("all pot. samples") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
} else {
  ew_plot <- ggplot(data = pmass, mapping = aes(x = '', y = lpm.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), real.pmass) +
    ggtitle("equal weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
  
  uw_plot <- ggplot(data = pmass, mapping = aes(x = '', y = lpm.up.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), real.pmass) +
    ggtitle("prop. weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
  
  rand_plot <- ggplot(data = pmass, mapping = aes(x = '', y = rand.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), real.pmass) +
    ggtitle("simple random") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
  
  all_plot <- ggplot(data = pmass, mapping = aes(x = '', y = all.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]), real.pmass) +
    ggtitle("all pot. samples") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = real.pmass, color='red')
}

layout <- rbind(c(1,2,3,4))

# filename
png(paste0(path, "plume_mass.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout, top = text_grob("plume mass", size = 18))

dev.off()

#### Plotting monitoring network ####

# plume outline at final time slice
plume <- true.data2 %>% filter(Time == 3467.5) %>% filter(y >= limit)
ps <- cbind(plume$X1, plume$X2)

# creating polygon of plume and checking data points position in relation to poly
plume.inpoly <- point.in.polygon(plume$X1, plume$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])

# attaching indicator (in poly or not) to true.data
plume.inpoly <- as.data.frame(cbind(plume, plume.inpoly))

# filtering data for points that are on the poly borders
plume.delin <- plume.inpoly %>% filter(plume.inpoly > 1)

# removing indicator column
plume.delin <- plume.delin %>% select(-plume.inpoly)

# filename
png(paste0(path, "network.png"), width = 620, height = 400)

# plot of well coordinates and true data (last time slice)
ggplot(data=true.data %>% filter(Time == 3467.5),
       mapping = aes(x=X1, y=X2)) +
  geom_contour_fill(aes(z=log(y)), bins=60) +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
  geom_point(data = well_coords, size=3) +
  geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1, size=5) +
  # geom_point(data = plume.delin, aes(x=X1, y=X2), color = 'red') +
  xlab("Easting") +
  ylab("Northing") + 
  labs(fill="log conc") +
  xlim(1, 100) +
  ylim(1, 35)

dev.off()

#### Plotting example GIFs of sampling designs and corresponding model estimates through time ####

#### all possible observations ####

# creating df of results for plotting
all.preds.df <- cbind(true.data2, all.preds)

# looping through sampling events
for (h in 1:length(td2)) {

  samp_coords <- obs.fut %>%
    filter(Time == td2[h]) %>%
    select(X1, X2)

  # plume_delin <- lpm.up.preds %>%
  #   filter(y >= limit) %>%
  #   filter(Time == ts2[h])

  # for spatiotemporal model
  # data = cbind(true.data2, all.preds

  # plume outline
  # plume coordinates
  # plume <- all.preds.df %>% filter(Time == ts[h]) %>% filter(all.preds >= log(limit))
  # ps <- cbind(plume$X1, plume$X2)
  # 
  # # creating polygon of plume and checking data points position in relation to poly
  # plume.inpoly <- point.in.polygon(plume$X1, plume$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])
  # 
  # # attaching indicator (in poly or not) to true.data
  # plume.inpoly <- as.data.frame(cbind(plume, plume.inpoly))
  # 
  # # filtering data for points that are on the poly borders
  # plume.delin <- plume.inpoly %>% filter(plume.inpoly > 1)
  # 
  # # removing indicator column
  # plume.delin <- plume.delin %>% select(-plume.inpoly)

  assign(
    paste0("up.ts", h),
    ggplot(data = all.preds.df %>% filter(Time == td2[h]), #instead of the cbind part jsut put it like "rand.app2.plume.preds" if you want to display the plume only part
           mapping = aes(x = X1, y = X2)) +
      # geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
      # geom_point(data = plume_delin, color = "red", size=3) +
      geom_contour_fill(aes(z=all.preds), bins=60) +
      geom_point(data = well_coords) +
      geom_point(data = samp_coords, color = "green", size=3) +
      geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(min(all.preds.df$all.preds),max(all.preds.df$all.preds))) +
      # geom_point(data = plume.delin, aes(x=X1, y=X2), color = "red") +
      # scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
      xlab("Easting") +
      ylab("Northing") +
      labs(fill="Conc.(mg/L)") +
      ylim(0,35) +
      xlim(0, 100)
  )
}

saveGIF(expr = {
  print(up.ts1)
  print(up.ts2)
  print(up.ts3)
  print(up.ts4)
  print(up.ts5)
  print(up.ts6)
  print(up.ts7)
  print(up.ts8)
  # print(up.ts9)
  # print(up.ts10)
}, interval = 1, ani.height=600, ani.width=800,
movie.name = paste0(path, "all_example.gif"))


#### SRS ####

# creating df for plotting results
rand.app2.preds.df <- cbind(true.data2, rand.app2.preds)

for (h in 1:length(td2)) {

  samp_coords <- rand.app2.sample %>%
    filter(Time == td2[h]) %>%
    select(X1, X2)

  # plume_delin <- lpm.up.preds %>%
  #   filter(y >= limit) %>%
  #   filter(Time == ts2[h])

  # plume outline
  # plume coordinates
  # plume <- rand.app2.preds.df %>% filter(Time == ts[h]) %>% filter(rand.app2.preds >= log(limit))
  # ps <- cbind(plume$X1, plume$X2)
  # 
  # # creating polygon of plume and checking data points position in relation to poly
  # plume.inpoly <- point.in.polygon(plume$X1, plume$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])
  # 
  # # attaching indicator (in poly or not) to true.data
  # plume.inpoly <- as.data.frame(cbind(plume, plume.inpoly))
  # 
  # # filtering data for points that are on the poly borders
  # plume.delin <- plume.inpoly %>% filter(plume.inpoly > 1)
  # 
  # # removing indicator column
  # plume.delin <- plume.delin %>% select(-plume.inpoly)

  assign(
    paste0("up.ts", h),
    ggplot(data = rand.app2.preds.df %>% filter(Time == td2[h]), #instead of the cbind part jsut put it like "rand.app2.plume.preds" if you want to display the plume only part
           mapping = aes(x = X1, y = X2)) +
      # geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
      # geom_point(data = plume_delin, color = "red", size=3) +
      geom_contour_fill(aes(z=rand.app2.preds), bins=60) +
      geom_point(data = well_coords) +
      geom_point(data = samp_coords, color = "green", size=3) +
      geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(min(rand.app2.preds.df$rand.app2.preds), max(rand.app2.preds.df$rand.app2.preds))) +
      # geom_point(data = plume.delin, aes(x=X1, y=X2), color = "red") +
      # scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
      xlab("Easting") +
      ylab("Northing") +
      labs(fill="Conc.(mg/L)") +
      ylim(0,35) +
      xlim(0, 100)
  )
}

saveGIF(expr = {
  print(up.ts1)
  print(up.ts2)
  print(up.ts3)
  print(up.ts4)
  print(up.ts5)
  print(up.ts6)
  print(up.ts7)
  print(up.ts8)
  print(up.ts9)
  print(up.ts10)
}, interval = 1, ani.height=600, ani.width=800,
movie.name = paste0(path, "rand_example.gif"))

#### eLPM ####

# creating df for plotting results
lpm.app2.preds.df <- cbind(true.data2, lpm.app2.preds)

for (h in 1:length(td2)) {

  samp_coords <- lpm.app2.sample %>%
    filter(Time == td2[h]) %>%
    select(X1, X2)

  # plume_delin <- lpm.up.preds %>%
  #   filter(y >= limit) %>%
  #   filter(Time == ts2[h])

  # plume outline
  # plume coordinates
  # plume <- lpm.app2.preds.df %>% filter(Time == ts[h]) %>% filter(lpm.app2.preds >= log(limit))
  # ps <- cbind(plume$X1, plume$X2)
  # 
  # # creating polygon of plume and checking data points position in relation to poly
  # plume.inpoly <- point.in.polygon(plume$X1, plume$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])
  # 
  # # attaching indicator (in poly or not) to true.data
  # plume.inpoly <- as.data.frame(cbind(plume, plume.inpoly))
  # 
  # # filtering data for points that are on the poly borders
  # plume.delin <- plume.inpoly %>% filter(plume.inpoly > 1)
  # 
  # # removing indicator column
  # plume.delin <- plume.delin %>% select(-plume.inpoly)

  assign(
    paste0("up.ts", h),
    ggplot(data = lpm.app2.preds.df %>% filter(Time == td2[h]), #instead of the cbind part jsut put it like "rand.app2.plume.preds" if you want to display the plume only part
           mapping = aes(x = X1, y = X2)) +
      # geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
      # geom_point(data = plume_delin, color = "red", size=3) +
      geom_contour_fill(aes(z=lpm.app2.preds), bins=60) +
      geom_point(data = well_coords) +
      geom_point(data = samp_coords, color = "green", size=3) +
      geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(min(lpm.app2.preds.df$lpm.app2.preds), max(lpm.app2.preds.df$lpm.app2.preds))) +
      # geom_point(data = plume.delin, aes(x=X1, y=X2), color = "red") +
      # scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
      xlab("Easting") +
      ylab("Northing") +
      labs(fill="Conc.(mg/L)") +
      ylim(0,35) +
      xlim(0, 100)
  )
}

saveGIF(expr = {
  print(up.ts1)
  print(up.ts2)
  print(up.ts3)
  print(up.ts4)
  print(up.ts5)
  print(up.ts6)
  print(up.ts7)
  print(up.ts8)
  print(up.ts9)
  print(up.ts10)
}, interval = 1, ani.height=600, ani.width=800,
movie.name = paste0(path, "equal_example.gif"))

#### pLPM ####

# creating df for plotting results
lpm.up.preds.df <- cbind(true.data2, lpm.up.preds)

for (h in 1:length(td2)) {

  samp_coords <- lpm.up.sample %>%
    filter(Time == td2[h]) %>%
    select(X1, X2)

  # plume_delin <- lpm.up.preds %>%
  #   filter(y >= limit) %>%
  #   filter(Time == ts2[h])

  # plume outline
  # plume coordinates
  # plume <- lpm.up.preds.df %>% filter(Time == ts[h]) %>% filter(lpm.up.preds >= log(limit))
  # ps <- cbind(plume$X1, plume$X2)
  # 
  # # creating polygon of plume and checking data points position in relation to poly
  # plume.inpoly <- point.in.polygon(plume$X1, plume$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])
  # 
  # # attaching indicator (in poly or not) to true.data
  # plume.inpoly <- as.data.frame(cbind(plume, plume.inpoly))
  # 
  # # filtering data for points that are on the poly borders
  # plume.delin <- plume.inpoly %>% filter(plume.inpoly > 1)
  # 
  # # removing indicator column
  # plume.delin <- plume.delin %>% select(-plume.inpoly)

  assign(
    paste0("up.ts", h),
    ggplot(data = lpm.up.preds.df %>% filter(Time == td2[h]), #instead of the cbind part jsut put it like "rand.app2.plume.preds" if you want to display the plume only part
           mapping = aes(x = X1, y = X2)) +
      # geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
      # geom_point(data = plume_delin, color = "red", size=3) +
      geom_contour_fill(aes(z=lpm.up.preds), bins=60) +
      geom_point(data = well_coords) +
      geom_point(data = samp_coords, color = "green", size=3) +
      geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(min(lpm.up.preds.df$lpm.up.preds), max(lpm.up.preds.df$lpm.up.preds))) +
      # geom_point(data = plume.delin, aes(x=X1, y=X2), color = "red") +
      # scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
      xlab("Easting") +
      ylab("Northing") +
      labs(fill="Conc.(mg/L)") +
      ylim(0,35) +
      xlim(0, 100)
  )
}

saveGIF(expr = {
  print(up.ts1)
  print(up.ts2)
  print(up.ts3)
  print(up.ts4)
  print(up.ts5)
  print(up.ts6)
  print(up.ts7)
  print(up.ts8)
  print(up.ts9)
  print(up.ts10)
}, interval = 1, ani.height=600, ani.width=800,
movie.name = paste0(path, "prop_example.gif"))


#### Plotting GIF of true plume data ####

for (h in 1:length(td2)) {


  # plume outline
  # plume coordinates
  # plume <- true.data2 %>% filter(Time == ts[h]) %>% filter(y >= limit)
  # ps <- cbind(plume$X1, plume$X2)
  # 
  # # creating polygon of plume and checking data points position in relation to poly
  # plume.inpoly <- point.in.polygon(plume$X1, plume$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])
  # 
  # # attaching indicator (in poly or not) to true.data
  # plume.inpoly <- as.data.frame(cbind(plume, plume.inpoly))
  # 
  # # filtering data for points that are on the poly borders
  # plume.delin <- plume.inpoly %>% filter(plume.inpoly > 1)
  # 
  # # removing indicator column
  # plume.delin <- plume.delin %>% select(-plume.inpoly)

  assign(
    paste0("up.ts", h),
    ggplot(data = true.data2 %>% filter(Time == td2[h]), #instead of the cbind part jsut put it like "rand.app2.plume.preds" if you want to display the plume only part
           mapping = aes(x = X1, y = X2)) +
      # geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
      # geom_point(data = plume_delin, color = "red", size=3) +
      geom_contour_fill(aes(z=log(y)), bins=60) +
      geom_point(data = well_coords) +
      geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) + #, limits = c(log(min(true.data2$y)), log(max(true.data2$y)))
      # geom_point(data = plume.delin, aes(x=X1, y=X2), color = "red") +
      # scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
      xlab("Easting") +
      ylab("Northing") +
      labs(fill="Conc.(mg/L)") +
      ylim(0,35) +
      xlim(0, 100)
  )
}

saveGIF(expr = {
  print(up.ts1)
  print(up.ts2)
  print(up.ts3)
  print(up.ts4)
  print(up.ts5)
  print(up.ts6)
  print(up.ts7)
  print(up.ts8)
  print(up.ts9)
  print(up.ts10)
}, interval = 1, ani.height=600, ani.width=800,
movie.name = paste0(path, "real.gif"))
