
# where to save figures
path <- "C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\GWSDAT\\chull\\comp_ex\\test\\"

# real data - ONLY TO TEST GWSDAT EXAMPLES
load("C://Users//2608904R//OneDrive - University of Glasgow//PhD//well_influence_analysis_sim_study//data//comp_ex.RData")

# unit corrections ug/l to mg/l
results_uni <- ifelse(e_benzene_data$Units == 'ug/l', e_benzene_data$Results / 1000, e_benzene_data$Results)

require(dplyr)

# adding unit corrected results to original data
e_benzene_data_uni <- cbind(e_benzene_data, results_uni)

# isolating required columns for observations
obs <- e_benzene_data_uni %>%
  select(WellName, XCoord, YCoord, SampleDate, results_uni) %>%
  rename(well.id = WellName, X1 = XCoord, X2 = YCoord, Time = SampleDate, y = results_uni)

# trying a 3d plot
# require(plotly)
# 
# plot_ly(x=obs$X1, y=obs$X2, z=obs$Time, type = 'scatter3d', mode='markers', color=log(obs$y))


# try providing undersampled scenario by removing a portion of the observations

# obs <- obs[sample(nrow(obs), 300), ]

# loading smst functions
source("C://Users//2608904R//OneDrive - University of Glasgow//PhD//well_influence_analysis_sim_study//sm-st.R",
       chdir = TRUE)

# model settings
nseg = c(6,6,6)
bdeg = 2
pord = 1

# creating matrix B
xrange <- rbind(range(obs$X1), 
                range(obs$X2), 
                range(obs$Time))

B <- st.matrices(obs[,2:4], 
                 xrange = xrange, 
                 nseg = nseg,
                 bdeg = bdeg,
                 pord = pord)

# LPM sampling with decreasing n 

# list of sample ratios to try
n.list <- seq(from=.05, to=1, by=.05)

# for collecting the results in the loop
results <- data.frame(double())

model.pars <- data.frame(double())

# number of observations
N <- nrow(obs)

# potential samples for sampling funciton
require(tibble)
pot.obs <- cbind(obs$X1, obs$X2, obs$Time)

# adding observation id numbers
obs.id <- rowid_to_column(obs)

# counter for example gifs
# ex <- sample.int(10, 1)

# number of iterations
for (y in 1:100) {
  
  tryCatch({
    
    # looping through list of ratios
    for (x in 1:length(n.list)) {
      
      n <- nrow(obs) * n.list[x]
      
      # equal inclusion weights
      ep <- rep(n/N, N)
      
      # LPM sampling - yields row numbers of the selected samples
      require(SamplingBigData)
      sample <- as.data.frame(lpm2_kdtree(x=pot.obs, prob=ep))
      colnames(sample) <- 'rowid'
      
      # obtaining X1, X2, Time and y values corresponding to samples
      sample <- left_join(sample, obs.id, by='rowid')
      
      # modelling
      model <- sm.st(
        x = sample[,3:4],
        t = sample[,5],
        y = log(sample[,6]),
        lambda = log.post,
        nseg = nseg,
        bdeg = bdeg,
        pord = pord
      )
      
      # RMSE - how well do we predict data points that were left out
      preds <- B$B %*% model$alpha
      
      preds.df <- cbind(obs, preds)
      preds.df <- rowid_to_column(preds.df)
      
      left.out <- preds.df %>% filter(!(rowid %in% sample$rowid))
      
      rmse <- sqrt(sum((log(left.out$y) - left.out$preds)^2)/nrow(left.out))
      
      # pulling results together
      sum <- data.frame('iter' = y,
                        'n' = n.list[x],
                        'RMSE' = rmse,
                        'esd' = model$pen.fit.model$cache$estimate.sd)

      results <- rbind(results, sum)
      
    }
    
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

save(results, file = paste0(path, 'results.RData'))


# plotting results

up95 <- function(vector) quantile(vector, 0.975)
low95 <- function(vector) quantile(vector, 0.025)

require(ggplot2)

# plotting RMSE: estimated data points vs real data points
median.RMSE <- results %>% group_by(n) %>% filter(n != 1) %>% reframe(median(RMSE))
max.RMSE <- results %>% group_by(n) %>% filter(n != 1) %>% reframe(up95(RMSE))
min.RMSE <- results %>% group_by(n) %>% filter(n != 1) %>% reframe(low95(RMSE))

df.RMSE <- data.frame('n' = median.RMSE$n, 
                      'median' = median.RMSE$`median(RMSE)`, 
                      'max' = max.RMSE$`up95(RMSE)`, 
                      'min' = min.RMSE$`low95(RMSE)`)

png(paste0(path, 'RMSE.png'),
    width = 800, height = 600)

ggplot(data = df.RMSE, aes(y=median, x=n)) +
  geom_line(size=2) +
  geom_ribbon(aes(x=n, ymax=max, ymin=min), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df.RMSE$min), max(df.RMSE$max)) +
  xlim(0, 1) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16)) +
  xlab("Ratio of samples used") +
  ylab("RMSE")

dev.off()


# plotting cache: esd
median.esd <- results %>% group_by(n) %>% reframe(median(esd))
max.esd <- results %>% group_by(n) %>% reframe(up95(esd))
min.esd <- results %>% group_by(n) %>% reframe(low95(esd))

df.esd <- data.frame('n' = median.esd$n, 
                     'median' = median.esd$`median(esd)`, 
                     'max' = max.esd$`up95(esd)`, 
                     'min' = min.esd$`low95(esd)`)

png(paste0(path, 'esd.png'),
    width = 800, height = 600)

ggplot(data = df.esd, aes(y=median, x=n)) +
  geom_line(size=2) +
  geom_ribbon(aes(x=n, ymax=max, ymin=min), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df.esd$min), max(df.esd$max)) +
  xlim(0, 1) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16)) +
  xlab("Ratio of samples used") +
  ylab("Estimated standard deviation")

dev.off()
