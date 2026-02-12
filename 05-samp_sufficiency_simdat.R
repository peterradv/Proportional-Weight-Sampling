

# Observations as uploaded by user ----------------------------------------

# where to save figures
path <- "..."

# plume data
load("data//complex_plume.RData")

# well coordinates
load("data//rand_24.RData")


# Trimming data if you only want to work with convex hull
require(dplyr)
# well coordinates without well.id
ps <- cbind(well_coords$X1, well_coords$X2)

# creating polygon of wells and checking which data points are inside
require(sp)
true.data.inpoly <- point.in.polygon(true.data$X1, true.data$X2, ps[chull(ps[,1:2]),1], ps[chull(ps[,1:2]),2])

# attaching indicator (in poly or not) to true.data
true.data.inpoly <- as.data.frame(cbind(true.data, true.data.inpoly))

# filtering data for points that are within the poly
true.data <- true.data.inpoly %>% filter(true.data.inpoly > 0)

# removing indicator column
true.data <- true.data %>% select(-true.data.inpoly)

# checking result in plot of convex hull
require(metR)
require(ggplot2)
require(RColorBrewer)
ggplot(data = true.data %>% filter(Time == 3467.5), aes(x=X1, y=X2)) +
  geom_contour_fill(aes(z=log(y)), bins = 30) +
  geom_point(data = well_coords, aes(x=X1, y=X2), size = 3) +
  geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1, size=6)


# # Example plot for distance between well and plume
# ggplot(data=true.data %>% filter(Time == 3467.5),
#        mapping = aes(x=X1, y=X2)) +
#   geom_contour_fill(aes(z=log(y)), bins=60) +
#   scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
#   geom_point(data = well_coords, size=5) +
#   geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1, size=7) +
#   # geom_point(data = true.data %>% 
#   #              filter(Time == 2007.5) %>% 
#   #              filter(y > 0.0017) %>% 
#   #              filter(y < 0.0023), 
#   #            aes(x=X1, y=X2), color = 'red') +
#   xlab("Easting") +
#   ylab("Northing") + 
#   labs(fill="log(y)") +
#   xlim(1, 100) +
#   ylim(1, 35) +
#   theme(axis.text=element_text(size=15),
#         axis.title=element_text(size=16), 
#         legend.text = element_text(size=15), 
#         legend.key.size = unit(1, 'cm'), 
#         legend.title = element_text(size=15)) #+
#   # annotate("text", x = 63, y = 15, label = "D", size=7) +
#   # annotate("segment", x = 35, xend = 83.389262, y = 15, yend = 9.671141,
#   #          colour = "black", arrow=arrow(ends = "both"), size=1.5)
  

obs <- left_join(well_coords, true.data)


# GWSDAT model ------------------------------------------------------------


# loading smst functions
source("data//sm-st.R",
       chdir = TRUE)

# model settings
nseg = c(6,6,6)
bdeg = 2
pord = 1

# modelling
model0 <- sm.st(
  x = obs[,2:3],
  t = obs[,4],
  y = log(obs[,5]),
  lambda = log.post,
  nseg = nseg,
  bdeg = bdeg,
  pord = pord
)


# Target estimation surface -----------------------------------------------


# creating matrix B
xrange <- rbind(range(true.data$X1), 
                range(true.data$X2), 
                range(true.data$Time))

rownames(xrange) <- c("X1", "X2", "Time")

B <- st.matrices(true.data[,1:3], 
                 xrange = xrange, 
                 nseg = nseg,
                 bdeg = bdeg,
                 pord = pord)

# loading matrix B
#source('C://Users//2608904R//OneDrive - University of Glasgow//PhD//Balanced Sampling for Groundwater Monitoring//Comparison of BSMs//mat_B//complex_B.RData')


# Estimating surface ------------------------------------------------------


# estimated surface
preds0 <- B$B %*% model0$alpha

preds0.df <- cbind(true.data, preds0)


# LPM sampling with decreasing n ------------------------------------------

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

# true plume mass
limit = 0.002
plume.dat <- true.data %>% filter(y >= limit)
real.pmass <- sum(plume.dat$y)/3500

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
      
      # estimated surface
      preds <- B$B %*% model$alpha
      
      # RMSPE compared to preds0
      rmsd <- sqrt(sum((preds0 - preds)^2)/nrow(preds0))
      
      # RMSPE to truth
      rmspe <- sqrt(sum((log(true.data$y) - preds)^2)/nrow(preds))
      
      # RMSE - how well do we predict data points that were left out
      preds.df <- cbind(true.data, preds)
      obs.preds.df <- left_join(well_coords, preds.df)
      obs.preds.df <- rowid_to_column(obs.preds.df)
      
      left.out <- obs.preds.df %>% filter(!(rowid %in% sample$rowid))
      
      rmse <- sqrt(sum((log(left.out$y) - left.out$preds)^2)/nrow(left.out))
      
      # Plume mass ratio
      est.plume.dat <- preds.df %>% filter(y >= limit)
      est.pmass <- sum(exp(1)^est.plume.dat$preds)/3500
      pmass.ratio <- est.pmass/real.pmass
      
      # checking significance of spatial component in a gam 
      # require(mgcv)
      # gam <- gam(y ~ s(X1, X2) + s(Time), data = sample)
      # 
      # sum.gam <- summary(gam)
      # 
      # spatial.signif <- sum.gam$s.table[1,4]
      
      # model parameters
      # cache <- data.frame(#'edf' = model$pen.fit.model$cache$effective.df,
      #                     #'rdf' = model$pen.fit.model$cache$residual.df, 
      #                     'esd' = model$pen.fit.model$cache$estimate.sd, 
      #                     'iter' = y, 
      #                     'n' = n.list[x])
      # 
      # model.pars <- rbind(model.pars, cache)
      
      # pulling results together
      sum <- data.frame('iter' = y,
                        'n' = n.list[x],
                        'RMSD' = rmsd,
                        'RMSPE' = rmspe,
                        'RMSE' = rmse,
                        'pmass' = pmass.ratio,
                        'esd' = model$pen.fit.model$cache$estimate.sd)
                        #'spatial.pval' = spatial.signif
      
      results <- rbind(results, sum)
      
      # # plotting GIFs
      # if (y == ex) {
      # 
      #   for (h in 1:length(unique(true.data$Time))) {
      # 
      #     samp_coords <- sample %>%
      #       filter(Time == unique(true.data$Time)[h]) %>%
      #       select(X1, X2)
      # 
      # 
      #     require(metR)
      #     require(ggplot2)
      # 
      #     assign(
      #       paste0("up.ts", h),
      #       ggplot(data = preds.df %>% filter(Time == unique(true.data$Time)[h]),
      #              mapping = aes(x = X1, y = X2)) +
      #         geom_contour_fill(aes(z=preds), bins=60) +
      #         geom_point(data = well_coords) +
      #         geom_point(data = samp_coords, color = "green", size=3) +
      #         geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      #         scale_fill_gradientn(colors = rev(brewer.pal(11, "RdYlBu")), limits = c(-25,10)) +
      #         xlab("Easting") +
      #         ylab("Northing") +
      #         labs(fill="Conc.(mg/L)") +
      #         ylim(0,35) +
      #         xlim(0, 100)
      #     )
      #   }
      # 
      #   require(animation)
      #   saveGIF(expr = {
      #     print(up.ts1)
      #     print(up.ts2)
      #     print(up.ts3)
      #     print(up.ts4)
      #     print(up.ts5)
      #     print(up.ts6)
      #     print(up.ts7)
      #     print(up.ts8)
      #     print(up.ts9)
      #     print(up.ts10)
      #     print(up.ts11)
      #     print(up.ts12)
      #     print(up.ts13)
      #     print(up.ts14)
      #     print(up.ts15)
      #     print(up.ts16)
      #     print(up.ts17)
      #     print(up.ts18)
      #     print(up.ts19)
      #     print(up.ts20)
      #   }, interval = 1, ani.height=600, ani.width=800,
      #   movie.name = paste0(path, '\\chull\\', n.list[x], "_complexlog.gif"))
      # 
      # }
      
    }
    
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

save(results, file = paste0(path, 'results.RData'))


# Plotting results --------------------------------------------------------


up95 <- function(vector) quantile(vector, 0.975)
low95 <- function(vector) quantile(vector, 0.025)


# # plotting RMSPE compared to full model
# ggplot(data = results, aes(y=RMSD, x=n)) +
#   geom_point(size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='RMSPE as a function of % of total observations: compared to complete model')


# plotting RMSPE compared to truth
median.RMSPE <- results %>% group_by(n) %>% reframe(median(RMSPE))
max.RMSPE <- results %>% group_by(n) %>% reframe(up95(RMSPE))
min.RMSPE <- results %>% group_by(n) %>% reframe(low95(RMSPE))

df.RMSPE <- data.frame('n' = median.RMSPE$n, 
                       'median' = median.RMSPE$`median(RMSPE)`, 
                       'max' = max.RMSPE$`up95(RMSPE)`, 
                       'min' = min.RMSPE$`low95(RMSPE)`)

png(paste0(path, 'RMSPE.png'),
    width = 800, height = 600)

# ggplot(data = results, aes(y=RMSPE, x=n)) +
#   geom_point(size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='RMSPE as a function of % of total observations: compared to truth')

ggplot(data = df.RMSPE, aes(y=median, x=n)) +
  geom_line(size=2) +
  geom_ribbon(aes(x=n, ymax=max, ymin=min), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df.RMSPE$min), max(df.RMSPE$max)) +
  xlim(0, 1) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16)) +
  xlab("Ratio of samples used") +
  ylab("RMSPE")

dev.off()


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

# ggplot(data = results, aes(y=RMSE, x=n)) +
#   geom_point(size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='RMSE as a function of % of total observations')

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


# plotting pmass ratio (est / real)
median.pmass <- results %>% group_by(n) %>% reframe(median(pmass))
max.pmass <- results %>% group_by(n) %>% reframe(up95(pmass))
min.pmass <- results %>% group_by(n) %>% reframe(low95(pmass))

df.pmass <- data.frame('n' = median.pmass$n, 
                      'median' = median.pmass$`median(pmass)`, 
                      'max' = max.pmass$`up95(pmass)`, 
                      'min' = min.pmass$`low95(pmass)`)

png(paste0(path, 'pmass.png'),
    width = 800, height = 600)

# ggplot(data = results, aes(y=pmass, x=n)) +
#   geom_point(size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='Plume mass ratio: estimated / real') +
#   scale_y_continuous(trans='log10') +
#   geom_hline(yintercept=1)

ggplot(data = df.pmass, aes(y=median, x=n)) +
  geom_line(size=2) +
  geom_ribbon(aes(x=n, ymax=max, ymin=min), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df.pmass$min), max(df.pmass$max)) +
  xlim(0, 1) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16)) +
  scale_y_continuous(trans='log10') +
  geom_hline(yintercept=1) +
  xlab("Ratio of samples used") +
  ylab("Estimated plume mass / real plume mass")

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

# ggplot(data = model.pars) +
#   geom_point(aes(y=esd, x=n), size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='estimated standard deviation')

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

# plotting p-values from GAMs
# ggplot(data = results, aes(y=spatial.pval, x=n)) +
#   geom_point(size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='p-value of spatial component in GAM')

# plotting cache: edf
# ggplot(data = model.pars) +
#   geom_point(aes(y=edf, x=n), size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='effective degrees of freedom')

# plotting cache: rdf
# ggplot(data = model.pars) +
#   geom_point(aes(y=rdf, x=n), size=2, alpha = .3) +
#   #geom_smooth() +
#   ggtitle(label='residual degrees of freedom')

# # Looking at rates of change in the figures
# 
# # for collecting results
# medians2 <- data.frame(double())
# 
# # calculating median RMSPEs
# for (j in 1:length(n.list)) {
#   
#   t1 <- results %>% filter(n == n.list[j])
#     
#   median <- median(t1$RMSD)
#     
#   medians <- data.frame('n' = n.list[j], 
#                         'median' = median)
#   
#   medians2 <- rbind(medians2, medians)
# }
# 
# # plotting median RMSPEs
# ggplot(data = results, aes(y=RMSD, x=n)) +
#   geom_point(size=2, alpha = .3) +
#   geom_point(data = medians2, aes(x=n, y=median), color = 'red', size = 3) +
#   ggtitle(label='RMSPE as a function of % of total observations: compared to complete model')
# 
# # calculating slope
# slope <- diff(medians2$median)/diff(medians2$n)
# 
# # min slope
# medians2[(which.min(slope)):(which.min(slope)+1),]
# 
# medians2[which(slope <= -3),]
