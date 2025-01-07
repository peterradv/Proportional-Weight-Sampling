# packages
# packages <- c("dplyr", "tibble", "ggplot2", "purrr", "mgcv", "tidymv",
#               "SamplingBigData", "BalancedSampling", "RColorBrewer",
#               "animation", "gridExtra", "ggpubr", "sp", "metR")
# lapply(packages, library, character.only = TRUE)

# library(SamplingBigData)
# library(BalancedSampling)
# library(RColorBrewer)
# library(animation)
# library(metR)

samps <- paste0(240, "samps\\") # number of samples to take over the whole period

spe <- 48 # samples per event
noe <- 5 # number of events
freq <- paste0(spe, 'x', noe, "\\") # frequency

# path to save images to
path0 <- "C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\"
# path <- paste0(path0, samps, freq)
path <- paste0(path0, samps, noe, '\\')

# plume data
load("C://Users//2608904R//OneDrive - University of Glasgow//PhD//well_influence_analysis_sim_study//data//complex_plume.RData")

# well coordinates
load("C://Users//2608904R//OneDrive - University of Glasgow//PhD//Balanced Sampling for Groundwater Monitoring//Comparison of BSMs//well_placement//rand_3.RData")


######## this bit trims true.data based on well coordinates so we only work with the convex hull ##########

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

# checking resultin plot of convex hull
# ggplot(data = true.data %>% filter(Time == 3467.5), aes(x=X1, y=X2)) +
#   geom_point(aes(color=log(y)))

# counting number of sampling events (time slices) ------------------------

# time slices
ts <- unique(true.data$Time)
ts1 <- ts[1:10]
td2 <- ts[11:20]

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

# divide total possible samples by this number
div <- W/spe

# number of wells to be sampled per time slice
w = W/div

# checking total number of samples
length(ts2)*w

# filtering simulated data at well coordinates ----------------------------

# first 10 sampling times
true.data1 <- true.data %>% filter(Time %in% ts1)
true.data2 <- true.data %>% filter(Time %in% td2)

# filtering data to mimic observations - these are all the potential sampling points and times
obs <- left_join(well_coords, true.data1)
obs.og <- obs # this will be obs without noise
# possible future observations
obs.fut <- left_join(well_coords, true.data2)
obs.fut.og <- obs.fut


# adding measurement noise to observations --------------------------------

# adding noise 
snr <- 0.15 
# set.seed(1)
obs$y <- obs$y * rnorm(nrow(obs), 1, snr)
obs.fut$y <- obs.fut$y * rnorm(nrow(obs.fut), 1, snr)


##### FUNCTIONS ###############################################################

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

# calculates the shortest distance between the plume and the well
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


##### TEST PLOTTING ###########################################################

# # coordinates of well 1
# P1 <- c(well_coords[10,2], well_coords[10,3])
# # 
# # # plume delineation limit
# limit = 0.002
# # 
# # # distance of plume to well 1 time series
# dist_to_plume <- plumedist(data = hist.preds, point = P1, plume.limit = limit)
# # 
# # # plotting results
# ggplot(data = dist_to_plume, mapping = aes(x=time, y=D)) +
#   geom_point()
#   #geom_smooth()

###############################################################################


# fitting model to historical observations

# loading smst functions
source("C://Users//2608904R//OneDrive - University of Glasgow//PhD//well_influence_analysis_sim_study//sm-st.R",
       chdir = TRUE)

# model settings
nseg = c(6,6,6)
bdeg = 2
pord = 1

hist.mod <- sm.st(
  x = obs[,2:3],
  t = obs[,4],
  y = log(obs[,5]),
  lambda = log.post,
  nseg = nseg,
  bdeg = bdeg,
  pord = pord
)

hist.xrange <- rbind(c(1, 100), 
                c(1, 35), 
                c(0, 1642.5))

fut.xrange <- rbind(c(1, 100),
                    c(1, 35),
                    c(1825, 3467.5))

rownames(hist.xrange) <- c("X1", "X2", "Time")
rownames(fut.xrange) <- c("X1", "X2", "Time")

hist.B <- st.matrices(true.data1[,1:3], 
                    xrange = hist.xrange, 
                    nseg = nseg,
                    bdeg = bdeg,
                    pord = pord)

# disable this part for testing spatial models
fut.B <- st.matrices(true.data2[,1:3],
                     xrange = fut.xrange,
                     nseg = nseg,
                     bdeg = bdeg,
                     pord = pord)

hist.preds <- hist.B$B %*% hist.mod$alpha
hist.preds <- cbind(true.data1, hist.preds) %>%
  subset(select = -y)
colnames(hist.preds)[4] <- 'y'


# calculating plume distance for all wells through time

# plume delineation limit - base it on realistic limits 
# (e.g. Benzene: max in GWSDAT examples = 500 mg/L (max solubility at 9C is 1810 mg/L)
# allowed limit in GW by WHO = 0.01 mg/L -> that is at 0.002% of max)
limit = 0.002

all_dist <- list()

for (t in 1:nrow(well_coords)) {
  
  P <- c(well_coords[t,2], well_coords[t,3])
  
  all.dist <- plumedist(data = hist.preds, point = P, plume.limit = log(limit))
  
  all_dist[[t]] <- all.dist
  
}

# kernel function for tuning parameters #####################################

# looking at D distribution to help scale kernel
Ds <- double()

for (r in 1:length(all_dist)) {
  Ds.temp <- as.data.frame(all_dist[[r]]$D)
  Ds <- rbind(Ds, Ds.temp)
}

colnames(Ds) <- "D"

# sometimes there are NAs when the plume limit cannot deliniate the plume at time = 0
Ds <- na.omit(Ds)

# mean distance
mD <- mean(Ds$D)
# median distance
meD <- median(Ds$D)
# maximum distance
max.dist <- max(Ds)

# histogram of plume distances with mean and median
# hist(Ds$D)
# abline(v = mD, col="red")
# abline(v = meD, col="blue")


################### OLD KERNEL STUFF ##########################
# h <- 1 # to avoid 0s, 1 is added to the minimum y value
# s <- 5 # sigma - controls how quickly the function drops to 1
# y_scaler <- .3 # controls the height of the function, for half-normal distribution its pi
# 
# # half-normal kernel function
# k_function <- function(x){h+(sqrt(2)/(sqrt(y_scaler)))*exp(-((x^2)/(2*s^2)))}
# 
# # plotting kernel
# ggplot(data = data.frame(x=c(0,max.dist)), aes(x=x)) +
#   stat_function(fun = k_function)
# 
# # checking kurtosis
# v1 <- seq(1,max.dist, by = 1)
# v2 <- k_function(v1)
# 
# PerformanceAnalytics::kurtosis(v2, method = "excess")

# set distance where half of max weight should be (mD, meD or a distance)
# p.halfweight <- meD
# height <- 10
#
# vector <- seq(from = 0.0001, to = 1, by = 0.001)
# 
# # automatic x-axis scaling
# for (q in 1:length(vector)) {
#   
#   # variables for x-axis scaling
#   r <- vector[q]
#   
#   ### half-normal function ###
#   
#   k_function <- function(x){h+(sqrt(2)/(s*sqrt(pi/height)))*exp(-((r*x^2)/(2*s^2)))}
#   
#   # break if function of median (or mean) distance is half of max weight
#   if (k_function(p.halfweight)>((k_function(0)-1)/2+1)-.1 & k_function(p.halfweight)<((k_function(0)-1)/2+1)+.1) break
#   
#   ### linear function ###
#   
#   # k_function <- function(x){1.8*h-r*x}
#   
#   # break linear function when k_function(max.dist)=0
#   # if (k_function(max.dist)>= 1 & k_function(max.dist)<1.06) break
# }

######### PLOTTING AND TESTING KERNEL FUNCTION #################

# testing function on median distance
# k_function(meD)
# 
# # plot of final kernel function
# ggplot(data = data.frame(x=c(0,max.dist)), aes(x=x)) +
#   stat_function(fun = k_function)
# 
# alltpars <- do.call('rbind', all.t.pars)
# alldists <- do.call('rbind', all.d.preds)
# 
# alltest <- cbind(alldists, alltpars)
# 
# plot(alltest)

##############################################################################

#### TESTING MODELLING ########################################################

# # can select any well to plot plume distance time series for
# ggplot(data = all_dist[[5]], mapping = aes(x=time, y=D)) +
#   geom_point() +
#   geom_smooth(method = "glm", method.args = list(family = gaussian(link = "log")), fullrange=TRUE) +
#   xlim(0, 3467.5) +
#   ylim(0, 25)
# #
# # # pulling distance time series for selected well
# data <- as.data.frame(cbind(all_dist[[5]]$time, all_dist[[5]]$D)) %>%
#   map_df(rev)
# colnames(data) <- c("time", "D")
# #
# # # preparing new data for prediction
# new.data <- as.data.frame(ts2)
# colnames(new.data) <- "time"
# #
# # # fitting model
# model <- glm(D+1 ~ time, data = all_dist[[5]], family = gaussian(link = "log"))
# 
# # plot fitted values
# plot(x=model$model$`time`, y=model$fitted.values)
# 
# # splines package, might need degrees of freedom - try to see if there is a constrained spline package
# # # predicting for new data (second half of true.data sampling campaigns)
# 
# new.data$D <- predict.glm(model, newdata = new.data, type="response")
# new.data <- new.data %>% mutate(D=D-1)
# #
# # new.data2 <- cbind(new.data, preds$fit)
# # colnames(new.data2) <- c("time", "D")
# 
# # # # concatenating data
# predss <- rbind(data, new.data)
# # #
# # # # plotting predicted change in distance
# ggplot(data = predss, mapping = aes(x=time, y=D)) +
#   geom_point() +
#   geom_smooth(method = 'glm', method.args = list(family = gaussian(link = "log"))) +
#   xlim(0, 3467.5) +
#   ylim(0, 25)

###############################################################################

# calculating tuning parameters for each well for each time slice

new.data <- as.data.frame(ts2)
colnames(new.data) <- "time"

# all.t.pars <- list()
all.d.preds <- list()
all.se <- list()

for (p in 1:length(all_dist)) {
  
  data <- as.data.frame(cbind(all_dist[[p]]$time, all_dist[[p]]$D)) %>%
    map_df(rev)
  colnames(data) <- c("time", "D")
  
  # replace NAs with something - look into this 
  # all_dist[[i]][is.na(all_dist[[i]])] <- max(all_dist[[i]]$D, na.rm = TRUE)
  
  model <- glm(D+1 ~ time, data = data, family = gaussian(link = "log"))
  
  # # predicting for new data (second half of true.data sampling campaigns)
  # new.data$D <- predict.glm(model, newdata = new.data, type = "response")
  # new.data <- new.data %>% mutate(D = D-1)
  # new.data$D[new.data$D<0] <- 0 #### here need to replace negative values with 0s
  # 
  # # saving preds
  # all.d.preds[[p]] <- as.data.frame(new.data$D)
  
  # testing saving standard errors
  preds <- predict.glm(model, newdata = new.data, type = "response", se.fit = TRUE)
  preds$fit <- preds$fit-1
  preds$fit[preds$fit < 0] <- 0
  
  ############################################
  
  # # # look at distance model predictions 
  # new.data2 <- cbind(new.data, preds$fit)
  # colnames(new.data2) <- c("time", "D")
  # # 
  # # # # # concatenating data
  # predss <- rbind(data, new.data2)
  # # 
  # # # # # plotting predicted change in distance
  # print(ggplot(data = predss, mapping = aes(x=time, y=D)) +
  #   geom_point())
  
  ############################################
  
  # saving preds
  all.d.preds[[p]] <- preds$fit

  # saving standard errors
  all.se[[p]] <- preds$se.fit
  
  ## for each sampling time - tuning parameters will be based on predicted 
  ## distance from the plume
  
  # t.pars <- double()
  # 
  # for (o in 1:nrow(new.data)) {
  #   
  #   # tuning params from kernel function
  # 
  #   t.par <- k_function(new.data[o,2])
  #   
  #   t.pars <- rbind(t.pars, t.par)
  # 
  # }
  # 
  # all.t.pars[[p]] <- t.pars
  
}

############################################

# # # look at distance model predictions 
# new.data2 <- cbind(new.data, preds$fit)
# colnames(new.data2) <- c("time", "D")
# # 
# # # # # concatenating data
# predss <- rbind(data, new.data2)
# # 
# # # # # plotting predicted change in distance
# ggplot(data = predss, mapping = aes(x=time, y=D)) +
#   geom_point()

############################################

# Put kernel here and you can make sigma dependent on standard error - calculate lower CI for all predictions and choose the lowest? 

# standard errors for tuning sigma
unlist.se <- unlist(all.se)
median.se <- median(unlist.se)
max.se <- max(unlist.se)


# equal inclusion weights for tuning y_scaler
# equal weights
ep <- rep(w/W, W)


h <- 1 # to avoid 0s, 1 is added to the minimum y value
s <- median.se # sigma - controls how quickly the function drops to 1
# make this dependent on the number of wells and the amount of samples you want per slice
y_scaler <- ep[1] # controls the height of the function, for half-normal distribution its pi

# half-normal kernel function
k_function <- function(x){(1/y_scaler) * (sqrt(2)/(sqrt(pi)))*exp(-((x^2)/(2*s^2)))}

# add 1 later when multiplying weights

# filename
png(paste0(path, "kernel.png"), width = 620, height = 400)

# plotting kernel
ggplot(data = data.frame(x=c(0,max.dist)), aes(x=x)) +
  stat_function(fun = k_function)

dev.off()

# calculating tuning parameters using the kernel function

all.t.pars <- list()

for (o in 1:length(all.d.preds)) {
  
  t.pars <- double()
  
  for (w1 in 1:length(all.d.preds[[o]])) {
   
    # tuning params from kernel function; add 1 to avoid numbers <1
    
    t.par <- 1 + k_function(all.d.preds[[o]][w1])
    
    t.pars <- rbind(t.pars, t.par)
     
  }
  
  all.t.pars[[o]] <- t.pars
  
}

# tuning params for per time slices

ups <- list()

for (m in 1:length(ts2)) {
  
  tps <- double()
  
  for (n in 1:length(all.t.pars)) {
    tp <- all.t.pars[[n]][m,]
    tps <- rbind(tps, tp)
  }
  
  # scaling weights by tuning params
  sp <- ep * tps
  
  # constant for re-scaling
  c <- sum(ep)/sum(sp)
  
  # re-scaling weigths so they sum to required number of samples
  up <- sp*c
  
  # checking sum
  # sum(up)
  
  ups[[m]] <- up
  
}


# Sampling ----------------------------------------------------------------

# equal weights ######################################################

# the number of wells to be sampled in each slice depending on n  
samp.well.nr <- w

# well list for the sampling
well.list <- cbind(well_coords$well.id, well_coords$X1, well_coords$X2)

# model settings
# nseg = c(6,6,6)
# bdeg = 2
# pord = 1

# loading smst functions
# source("C://Users//2608904R//OneDrive - University of Glasgow//PhD//well_influence_analysis_sim_study//sm-st.R",
#        chdir = TRUE)

# loading matrix B for predictions
# load("C://Users//2608904R//OneDrive - University of Glasgow//PhD//Balanced Sampling for Groundwater Monitoring//Comparison of BSMs//mat_B//simple_B.RData")


# results will be collected here in the loop
results <- data.frame(double())
results.plume <- data.frame(double())
results.out <- data.frame(double())
in.ratios <- data.frame(double())
sbs <- data.frame(double())
pmass <- data.frame(double())

# collecting samples
lpm.app2.samples <- list()
rand.app2.samples <- list()
lpm.up.samples <- list()

for (i in 1:100) {
  
  # all samples ####################################################
  
  # spatial balance when taking all samples (for comparison)
  all.sb <- BalancedSampling::sb(p=rep(nrow(well_coords)/nrow(well_coords), nrow(well_coords)), x=cbind(well_coords$X1, well_coords$X2), s=c(well_coords$well.id))
  
  # disabled for testing constant samp locs
  all.sbs <- rep(all.sb, length(ts2))
  
  # spatial model
  # all.model <- mgcv::gam(data = obs.fut, formula = log(y) ~ s(X1,X2, k=6))

  # disabeled for testing spatial model above
  all.model <- sm.st(
    x = obs.fut[,2:3],
    t = obs.fut[,4],
    y = log(obs.fut[,5]),
    lambda = log.post,
    nseg = nseg,
    bdeg = bdeg,
    pord = pord
  )
  
  ################################## Whole domain
  
  # spatial model
  # all.preds <- predict.gam(all.model, newdata = true.data2)
  
  # disabeled for testing spatial model
  # calculating predictions on the grid
  all.preds <- fut.B$B %*% all.model$alpha
  
  # spatial model
  # all.preds <- cbind(true.data2, all.preds)
  
  # spatial model
  # all.rmspe <- sqrt(sum((log(all.preds$y) - all.preds$all.preds)^2)/nrow(all.preds))
  
  # disabeled for testing spatial model
  # calculating rmspe
  all.rmspe <- sqrt(sum((log(true.data2$y) - all.preds)^2)/nrow(all.preds))
  
  # RMSE - only observations
  all.obs.preds <- predict.smst(all.model)
  
  all.rmse <- sqrt(sum((log(obs.fut$y) - all.obs.preds)^2)/length(all.obs.preds))
  
  ################################## Plume only
  
  # disabled for testing spatial model
  # filtering predictions for plume only
  all.plume.preds <- cbind(true.data2, all.preds) %>% filter(true.data2$y >= limit)
  
  # spatial model
  # all.plume.preds <- all.preds %>% filter(y >= limit)

  #calculating rmspe for plume only
  all.plume.rmspe <- sqrt(sum((log(all.plume.preds$y) - all.plume.preds$all.preds)^2)/nrow(all.plume.preds))
  
  # Plume mass
  all.pred.plume <- cbind(true.data2, all.preds) %>% filter(all.preds >= log(limit))
  
  all.pmass <- sum(exp(1)^all.pred.plume$all.preds)/3500
  
  ################################## without plume
  
  # disabled for testing spatial model
  # filtering predictions
  all.out.preds <- cbind(true.data2, all.preds) %>% filter(true.data2$y < limit)
  
  # spatial model
  # all.out.preds <- all.preds %>% filter(y < limit)
  
  
  #calculating rmspe 
  all.out.rmspe <- sqrt(sum((log(all.out.preds$y) - all.out.preds$all.preds)^2)/nrow(all.out.preds))
  
  
  # equal weights ##################################################
  
  # inclusion weights
  group.p <- ep
  
  # samples will be collected in this data frame
  lpm.app2.sample <- data.frame()
  
  # disabled for testing constant sample locs
  lpm.app2.sbs <- data.frame(double())
  
  # sampling different wells at each upcoming event using equal weigths
  # Disable when testing constant samp locs
  
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
  
  # testing constant sampling locs  
########################################  
  # # randomly selecting wells to sample
  # samp.wells <- as.data.frame(lpm2_kdtree(x=well.list, prob=group.p))
  # colnames(samp.wells) <- "well.id"
  # # at each time slice
  # samp.time <- as.data.frame(ts2)
  # colnames(samp.time) <- "Time"
  # # selecting observations from the selected wells and at the selected time slice
  # temp.samples <- left_join(samp.time, obs.fut)
  # temp.samples <- left_join(samp.wells, temp.samples)
  # # collecting samples from the different time slices
  # lpm.app2.sample <- rbind(lpm.app2.sample, temp.samples)
  # # calculating spatial balance
  # lpm.app2.sb <- BalancedSampling::sb(p=ep, x=cbind(well_coords$X1, well_coords$X2), s=c(samp.wells$well.id))
########################################
  
  # collecting samples
  lpm.app2.samples[[i]] <- lpm.app2.sample
  
  # disabled for testing constant samp locs
  colnames(lpm.app2.sbs) <- c("lpm.app2.sbs")
  
  # spatial model
  # lpm.app2.model <- mgcv::gam(data = lpm.app2.sample, formula = log(y) ~ s(X1,X2, k=6))
  
  # disabeled for checking spatial model
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
  
  ################################## Whole domain
  
  # disabeled for checking spatial model
  # calculating predictions on the grid
  lpm.app2.preds <- fut.B$B %*% lpm.app2.model$alpha
  
  # spatial model
  # lpm.app2.preds <- predict.gam(lpm.app2.model, newdata = true.data2)
  
  # lpm.app2.preds <- cbind(true.data2, lpm.app2.preds)
  
  # spatial model
  # lpm.app2.rmspe <- sqrt(sum((log(lpm.app2.preds$y) - lpm.app2.preds$lpm.app2.preds)^2)/nrow(lpm.app2.preds))
  
  # disabled for testing spatial model
  # calculating rmspe
  lpm.app2.rmspe <- sqrt(sum((log(true.data2$y) - lpm.app2.preds)^2)/nrow(lpm.app2.preds))
  
  # RMSE - only observations
  lpm.app2.obs.preds <- predict.smst(lpm.app2.model)
  
  lpm.app2.rmse <- sqrt(sum((log(lpm.app2.sample$y) - lpm.app2.obs.preds)^2)/length(lpm.app2.obs.preds))
  
  ################################## Plume only
  
  # disabled for testing spatial model
  # filtering predictions for plume only
  lpm.app2.plume.preds <- cbind(true.data2, lpm.app2.preds) %>% filter(true.data2$y >= limit)
  
  # spatial model
  # lpm.app2.plume.preds <- lpm.app2.preds %>% filter(y >= limit)
  
  #calculating rmspe for plume only
  lpm.app2.plume.rmspe <- sqrt(sum((log(lpm.app2.plume.preds$y) - lpm.app2.plume.preds$lpm.app2.preds)^2)/nrow(lpm.app2.plume.preds))
  
  # Plume mass
  lpm.app2.pred.plume <- cbind(true.data2, lpm.app2.preds) %>% filter(lpm.app2.preds >= log(limit))
  
  lpm.app2.pmass <- sum(exp(1)^lpm.app2.pred.plume$lpm.app2.preds)/3500
  
  ################################## without plume
  
  # disabled for testing spatial model
  # filtering predictions
  lpm.app2.out.preds <- cbind(true.data2, lpm.app2.preds) %>% filter(true.data2$y < limit)
  
  # spatial model
  # lpm.app2.out.preds <- lpm.app2.preds %>% filter(y < limit)
  
  #calculating rmspe 
  lpm.app2.out.rmspe <- sqrt(sum((log(lpm.app2.out.preds$y) - lpm.app2.out.preds$lpm.app2.preds)^2)/nrow(lpm.app2.out.preds))
  
  # unequal weights ################################################
  
  
  # samples will be collected in this data frame
  lpm.up.sample <- data.frame()
  
  # disabled for testing constant samp locs
  lpm.up.sbs <- data.frame()
  
  # disabled for testing constant samp locs
  # sampling different wells at each upcoming event using proportional weigths
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
  
  # Testing constant samp locs
#################################
  # # inclusion weights
  #   group.p <- c(ups[[1]])
  #   # randomly selecting wells to sample
  #   samp.wells <- as.data.frame(lpm2_kdtree(x=well.list, prob=group.p))
  #   colnames(samp.wells) <- "well.id"
  #   # at each time slice
  #   samp.time <- as.data.frame(ts2)
  #   colnames(samp.time) <- "Time"
  #   # selecting observations from the selected wells and at the selected time slice
  #   temp.samples <- left_join(samp.time, obs.fut)
  #   temp.samples <- left_join(samp.wells, temp.samples)
  #   # collecting samples from the different time slices
  #   lpm.up.sample <- rbind(lpm.up.sample, temp.samples)
  #   # calculating spatial balance
  #   lpm.up.sb <- sb(p=ep, x=cbind(well_coords$X1, well_coords$X2), s=c(samp.wells$well.id))
################################# 
  
  # collecting samples
  lpm.up.samples[[i]] <- lpm.up.sample
  
  # disabled for testing constant samp locs
  colnames(lpm.up.sbs) <- c("lpm.up.sbs")
  
  # spatial model
  # lpm.up.model <- mgcv::gam(data = lpm.up.sample, formula = log(y) ~ s(X1,X2, k=6))
  
  # disabled for testing spatial model
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
  
  ############################################### Whole domain
  
  # disabled for testing spatial model
  # calculating predictions on the grid
  lpm.up.preds <- fut.B$B %*% lpm.up.model$alpha
  
  # spatial model
  # lpm.up.preds <- predict.gam(lpm.up.model, newdata = true.data2)
  
  # lpm.up.preds <- cbind(true.data2, lpm.up.preds)
  
  # spatial model
  # lpm.up.rmspe <- sqrt(sum((log(lpm.up.preds$y) - lpm.up.preds$lpm.up.preds)^2)/nrow(lpm.up.preds))

  # disabled for testing spatial model
  # calculating rmspe
  lpm.up.rmspe <- sqrt(sum((log(true.data2$y) - lpm.up.preds)^2)/nrow(lpm.up.preds))
  
  # RMSE - only observations
  lpm.up.obs.preds <- predict.smst(lpm.up.model)
  
  lpm.up.rmse <- sqrt(sum((log(lpm.up.sample$y) - lpm.up.obs.preds)^2)/length(lpm.up.obs.preds))
  
  ############################################### Plume only
  
  # disabled for testing spatial model
  # filtering predictions for plume only
  lpm.up.plume.preds <- cbind(true.data2, lpm.up.preds) %>% filter(true.data2$y >= limit)
  
  # spatial model
  # lpm.up.plume.preds <- lpm.up.preds %>% filter(y >= limit)
  
  #calculating rmspe for plume only
  lpm.up.plume.rmspe <- sqrt(sum((log(lpm.up.plume.preds$y) - lpm.up.plume.preds$lpm.up.preds)^2)/nrow(lpm.up.plume.preds))
  
  # Plume mass
  lpm.up.pred.plume <- cbind(true.data2, lpm.up.preds) %>% filter(lpm.up.preds >= log(limit))
  
  lpm.up.pmass <- sum(exp(1)^lpm.up.pred.plume$lpm.up.preds)/3500
  
  ################################## without plume
  
  # disabled for testing spatial model
  # filtering predictions
  lpm.up.out.preds <- cbind(true.data2, lpm.up.preds) %>% filter(true.data2$y < limit)
  
  # spatial model
  # lpm.up.out.preds <- lpm.up.preds %>% filter(y < limit)
  
  #calculating rmspe 
  lpm.up.out.rmspe <- sqrt(sum((log(lpm.up.out.preds$y) - lpm.up.out.preds$lpm.up.preds)^2)/nrow(lpm.up.out.preds))
  
  
  # simple random sampling ###############################################
  
  # samples will be collected in this data frame
  rand.app2.sample <- data.frame()
  
  # Disabled for testing constant samp locs
  rand.app2.sbs <- data.frame()
  
# Disabled for testing constant samp locs
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

# testing constant sampling locs
##################################################
  # # randomly selecting wells to sample
  # samp.wells <- as.data.frame(sample(well.list[,1], samp.well.nr))
  # colnames(samp.wells) <- "well.id"
  # # at each time slice
  # samp.time <- as.data.frame(ts2)
  # colnames(samp.time) <- "Time"
  # # selecting observations from the selected wells and at the selected time slice
  # temp.samples <- left_join(samp.time, obs.fut)
  # temp.samples <- left_join(samp.wells, temp.samples)
  # # collecting samples from the different time slices
  # rand.app2.sample <- rbind(rand.app2.sample, temp.samples)
  # # calculating spatial balance
  # rand.app2.sb <- sb(p=ep, x=cbind(well_coords$X1, well_coords$X2), s=c(samp.wells$well.id))
##################################################
  
  rand.app2.samples[[i]] <- rand.app2.sample
  
  # Disabled for testing constant samp locs
  colnames(rand.app2.sbs) <- c("rand.app2.sbs")
  
  # app 2 srs model ---------------------------------------------------------
  
  # spatial model
  # rand.app2.model <- mgcv::gam(data = rand.app2.sample, formula = log(y) ~ s(X1,X2, k=6))
  
  # disabled for testing spatial model
  rand.app2.model <- sm.st(
    x = rand.app2.sample[,3:4],
    t = rand.app2.sample[,2],
    y = log(rand.app2.sample[,5]),
    lambda = log.post,
    nseg = nseg,
    bdeg = bdeg,
    pord = pord
  )
  
  ############################################### Whole domain
  
  # disabled for testing spatial model
  # calculating predictions on the grid
  rand.app2.preds <- fut.B$B %*% rand.app2.model$alpha
  
  # spatial model
  # rand.app2.preds <- predict.gam(rand.app2.model, newdata = true.data2)
  
  # rand.app2.preds <- cbind(true.data2, rand.app2.preds)
  
  # spatial model
  # rand.app2.rmspe <- sqrt(sum((log(rand.app2.preds$y) - rand.app2.preds$rand.app2.preds)^2)/nrow(rand.app2.preds))
  
  # disabled for testing spatial model
  # calculating rmspe
  rand.app2.rmspe <- sqrt(sum((log(true.data2$y) - rand.app2.preds)^2)/nrow(rand.app2.preds))
  
  # RMSE - only observations
  rand.app2.obs.preds <- predict.smst(rand.app2.model)
  
  rand.app2.rmse <- sqrt(sum((log(rand.app2.sample$y) - rand.app2.obs.preds)^2)/length(rand.app2.obs.preds))
  
  ############################################### Plume only
  
  # disabled for testing spatial model
  # filtering predictions for plume only
  rand.app2.plume.preds <- cbind(true.data2, rand.app2.preds) %>% filter(true.data2$y >= limit)
  
  # spatial model
  # rand.app2.plume.preds <- rand.app2.preds %>% filter(y >= limit)
  
  #calculating rmspe for plume only
  rand.app2.plume.rmspe <- sqrt(sum((log(rand.app2.plume.preds$y) - rand.app2.plume.preds$rand.app2.preds)^2)/nrow(rand.app2.plume.preds))
  
  # Plume mass
  rand.app2.pred.plume <- cbind(true.data2, rand.app2.preds) %>% filter(rand.app2.preds >= log(limit))
  
  rand.app2.pmass <- sum(exp(1)^rand.app2.pred.plume$rand.app2.preds)/3500
  
  ################################## without plume
  
  # disabled for testing spatial model
  # filtering predictions
  rand.app2.out.preds <- cbind(true.data2, rand.app2.preds) %>% filter(true.data2$y < limit)
  
  # spatial model
  # rand.app2.out.preds <- rand.app2.preds %>% filter(y < limit)
  
  #calculating rmspe 
  rand.app2.out.rmspe <- sqrt(sum((log(rand.app2.out.preds$y) - rand.app2.out.preds$rand.app2.preds)^2)/nrow(rand.app2.out.preds))
  

  ############################################## binding results
  
  results <- rbind(results, cbind(all.rmspe, all.rmse, rand.app2.rmspe, rand.app2.rmse, lpm.app2.rmspe, lpm.app2.rmse, lpm.up.rmspe, lpm.up.rmse))
  
  results.plume <- rbind(results.plume, cbind(all.plume.rmspe, rand.app2.plume.rmspe, lpm.app2.plume.rmspe, lpm.up.plume.rmspe))
  
  results.out <- rbind(results.out, cbind(all.out.rmspe, rand.app2.out.rmspe, lpm.app2.out.rmspe, lpm.up.out.rmspe))
  
  # Disabled for testing constant samp locs
  sbs <- rbind(sbs, cbind(all.sbs, rand.app2.sbs, lpm.app2.sbs, lpm.up.sbs))
  
  # plume mass
  pmass <- rbind(pmass, cbind(all.pmass, rand.app2.pmass, lpm.app2.pmass, lpm.up.pmass))
  
  # for testing constant samp locs
  # sbs <- rbind(sbs, cbind(all.sb, rand.app2.sb, lpm.app2.sb, lpm.up.sb))
  
  ############################################## number of wells in plume vs out plume
  
  # all samples from plume
  all.in.plume <- obs.fut %>%
    filter(y >= limit)
  
  # simple random samples from plume
  rand.in.plume <- rand.app2.sample %>%
    filter(y >= limit)
  
  # euqal prob samples from plume
  ep.in.plume <- lpm.app2.sample %>%
    filter(y >= limit)
  
  # ep.out.plume <- lpm.app2.sample %>%
  #   filter(y < limit)
  
  # proportional samples from plume
  up.in.plume <- lpm.up.sample %>%
    filter(y >= limit)
  
  # up.out.plume <- lpm.up.sample %>%
  #   filter(y < limit)
  
  all.ratio <- nrow(all.in.plume) / nrow(obs.fut)
  
  rand.ratio <- nrow(rand.in.plume) / nrow(rand.app2.sample)
  
  # # number of wells in plume from equal samples
  # ep.wells.in <- unique(ep.in.plume$well.id)
  # # wells total
  # ep.wells <- unique(lpm.app2.sample$well.id)
  # ratio of sampled wells in plume vs out plume
  # ep.ratio <- length(ep.wells.in) / (length(ep.wells) - length(ep.wells.in))
  
  ep.ratio <- nrow(ep.in.plume) / nrow(lpm.app2.sample)
  
  # # number of wells in plume from proportional samples
  # up.wells.in <- unique(up.in.plume$well.id)
  # # wells total
  # up.wells <- unique(lpm.up.sample$well.id)
  # ratio
  # up.ratio <- length(up.wells.in) / (length(up.wells) - length(up.wells.in))
  
  up.ratio <- nrow(up.in.plume) / nrow(lpm.up.sample)
  
  ############################################ binding ratio results
  
  in.ratios <- rbind(in.ratios, cbind(all.ratio, rand.ratio, ep.ratio, up.ratio))
}

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

save(lpm.app2.samples, file = paste0(path, 'LPM_samples.RData'))
save(rand.app2.samples, file = paste0(path, 'SRS_samples.RData'))
save(lpm.up.samples, file = paste0(path, 'pLPM_samples.RData'))



# hist(results$rand.app2.rmspe)
# hist(results$lpm.app2.rmspe)

############################ PLOTTING RESULTS ###############################

############################# whole domain

min <- min(
  c(
    min(results$all.rmspe), min(results$rand.app2.rmspe), min(results$lpm.app2.rmspe), min(results$lpm.up.rmspe)
  )
)

max <- max(
  c(
    max(results$all.rmspe), max(results$rand.app2.rmspe), max(results$lpm.app2.rmspe), max(results$lpm.up.rmspe)
  )
)

ew_plot <- ggplot(data = results, mapping = aes(x = '', y = lpm.app2.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("equal weights") +
  theme(axis.title.y=element_blank()) 

uw_plot <- ggplot(data = results, mapping = aes(x = '', y = lpm.up.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("prop. weights") +
  theme(axis.title.y=element_blank())

rand_plot <- ggplot(data = results, mapping = aes(x = '', y = rand.app2.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("simple random") +
  theme(axis.title.y=element_blank())

all_plot <- ggplot(data = results, mapping = aes(x = '', y = all.rmspe)) +
  geom_boxplot() +
  ylim(min, max) +
  ggtitle("all pot. samples") +
  theme(axis.title.y=element_blank())

layout <- rbind(c(1,2,3,4))

# filename
png(paste0(path, "rmspe_total.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout, top = text_grob("RMSPE: total", size = 18))

dev.off()


############################# whole domain RMSE

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


############################# Plume only

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

############################# without plume

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

############################## plume ratios

# total ratio of wells in plume in true data
# points.in.plume <- true.data2 %>% filter(y>=limit)
# obs.in.plume <- inner_join(well_coords, points.in.plume)
# wells.in.plume <- length(unique(obs.in.plume$well.id))
# 
# max.ratio <- wells.in.plume/nrow(well_coords)

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
# png(paste0(path, "plume_ratio.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout)

# dev.off()

############################# spatial balance
# change y = ... .sbs for non-constant samp locs, .sb for constant samp locs

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
# png(paste0(path, "spatial_balance.png"), width = 620, height = 400)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout) #top = text_grob("spatial balance", size = 18))

# dev.off()


############################# Plume mass

real.plume <- true.data2 %>% filter(y >= limit)
real.pmass <- sum(real.plume$y)/3500

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


# plotting network

# plume outline
# plume coordinates
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


# plotting examples of samples and predictions

# all

# creating df for plotting results
all.preds.df <- cbind(true.data2, all.preds)

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


# rand

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

# equal

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

# prop

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


# real data

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
