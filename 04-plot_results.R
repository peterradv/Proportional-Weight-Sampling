

#################### 20 samps #################

assign('sg20_2x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\20samps\\2x10\\rmspe_plume.RData")))
assign('sg20_4x5', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\20samps\\4x5\\rmspe_plume.RData")))
assign('sg20_5x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\20samps\\5x4\\rmspe_plume.RData")))
assign('sg20_10x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\20samps\\10x2\\rmspe_plume.RData")))

# 20 samples in total - balanced lpm
df <- data.frame(
  samples = c(20, 20, 20, 20,
              20, 20, 20, 20,
              20, 20, 20, 20),
  freq = c(10, 5, 4, 2,
           10, 5, 4, 2,
           10, 5, 4, 2),
  # Total RMSPE
  medRMSPE = c(median(sg20_2x10$rand.app2.rmspe), median(sg20_4x5$rand.app2.rmspe), median(sg20_5x4$rand.app2.rmspe), median(sg20_10x2$rand.app2.rmspe),
               median(sg20_2x10$lpm.app2.rmspe), median(sg20_4x5$lpm.app2.rmspe), median(sg20_5x4$lpm.app2.rmspe), median(sg20_10x2$lpm.app2.rmspe),
               median(sg20_2x10$lpm.up.rmspe), median(sg20_4x5$lpm.up.rmspe), median(sg20_5x4$lpm.up.rmspe), median(sg20_10x2$lpm.up.rmspe)),
  # Plume RMSPE
  # medRMSPE = c(median(sg20_2x10$rand.app2.plume.rmspe), median(sg20_4x5$rand.app2.plume.rmspe), median(sg20_5x4$rand.app2.plume.rmspe), median(sg20_10x2$rand.app2.plume.rmspe),
  #              median(sg20_2x10$lpm.app2.plume.rmspe), median(sg20_4x5$lpm.app2.plume.rmspe), median(sg20_5x4$lpm.app2.plume.rmspe), median(sg20_10x2$lpm.app2.plume.rmspe),
  #              median(sg20_2x10$lpm.up.plume.rmspe), median(sg20_4x5$lpm.up.plume.rmspe), median(sg20_5x4$lpm.up.plume.rmspe), median(sg20_10x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_30x2$all.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_30x2$all.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe),
               up95(sg60_30x2$all.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_30x2$all.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_30x2$all.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe),
                low95(sg60_30x2$all.rmspe)),
  method = c("srs", "srs", "srs", "srs",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\20samps\\medRMSPE_plume.png"),
    width = 500, height = 400)

# ggplot(data = df, aes(x=freq, y=medRMSPE)) +
#   geom_point(aes(color=method)) +
#   ylim(1.5,12) +
#   ggtitle(label='20 samples total') +
#   geom_smooth(aes(color=method))

ggplot(data = df, aes(x=freq, y=medRMSPE)) +
  geom_line(aes(color=method), size=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = method, fill=method), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=15)) +
  xlab("Frequency") +
  ylab("RMSPE")

dev.off()




#################### 30 samps #################

assign('sg30_3x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg30_5x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\6x5\\rmspe_plume.RData")))
assign('sg30_10x3', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\10x3\\rmspe_plume.RData")))
# assign('sg30_15x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\15x2\\rmspe_plume.RData")))

# 30 samples in total - balanced lpm
df <- data.frame(
  samples = c(30, 30, 30, #30,
              30, 30, 30, #30,
              30, 30, 30), #30),
  freq = c(10, 5, 3, #2,
           10, 5, 3, #2,
           10, 5, 3), #2),
  # medRMSPE = c(median(sg30_3x10$rand.app2.rmspe), median(sg30_5x6$rand.app2.rmspe), median(sg30_10x3$rand.app2.rmspe), #median(sg30_15x2$rand.app2.rmspe),
  #              median(sg30_3x10$lpm.app2.rmspe), median(sg30_5x6$lpm.app2.rmspe), median(sg30_10x3$lpm.app2.rmspe), #median(sg30_15x2$lpm.app2.rmspe),
  #              median(sg30_3x10$lpm.up.rmspe), median(sg30_5x6$lpm.up.rmspe), median(sg30_10x3$lpm.up.rmspe)), #median(sg30_15x2$lpm.up.rmspe)),
  medRMSPE = c(median(sg30_3x10$rand.app2.plume.rmspe), median(sg30_5x6$rand.app2.plume.rmspe), median(sg30_10x3$rand.app2.plume.rmspe), #median(sg30_15x2$rand.app2.plume.rmspe),
               median(sg30_3x10$lpm.app2.plume.rmspe), median(sg30_5x6$lpm.app2.plume.rmspe), median(sg30_10x3$lpm.app2.plume.rmspe), #median(sg30_15x2$lpm.app2.plume.rmspe),
               median(sg30_3x10$lpm.up.plume.rmspe), median(sg30_5x6$lpm.up.plume.rmspe), median(sg30_10x3$lpm.up.plume.rmspe)), #median(sg30_15x2$lpm.up.plume.rmspe)),
  method = c("srs", "srs", "srs", #"srs",
             "eLPM", "eLPM", 'eLPM', #'eLPM',
             'pLPM', 'pLPM', 'pLPM') #'pLPM'
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\medRMSPE_plume.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medRMSPE)) +
  geom_point(aes(color=method)) +
  ylim(1.5,12) +
  ggtitle(label='30 samples total') +
  geom_smooth(aes(color=method))

dev.off()


#################### 60 samps #################

assign('sg60_6x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\10x6\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\15x4\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\20x3\\rmspe_plume.RData")))

# 60 samples in total - balanced lpm
df <- data.frame(
  samples = c(60, 60, 60, 60,
              60, 60, 60, 60,
              60, 60, 60, 60),
  freq = c(10, 6, 4, 2,
           10, 6, 4, 2,
           10, 6, 4, 2),
  # medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
  #              median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
  #              median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  method = c("srs", "srs", "srs", "srs",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\medRMSPE_plume.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medRMSPE)) +
  geom_point(aes(color=method)) +
  ylim(1.5,12) +
  ggtitle(label='60 samples total') +
  geom_smooth(aes(color=method))

dev.off()


#################### 120 samps #################

assign('sg120_12x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\12x10\\rmspe_total.RData")))
assign('sg120_20x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\15x8\\rmspe_total.RData")))
assign('sg120_30x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\24x5\\rmspe_total.RData")))
# assign('sg120_40x3', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test_convex\\120samps\\24x5\\rmspe_plume.RData")))

# 120 samples in total - balanced lpm
df <- data.frame(
  samples = c(120, 120, 120, #120,
              120, 120, 120, #120,
              120, 120, 120), #120),
  freq = c(10, 8, 6, #5,
           10, 8, 6, #5,
           10, 8, 6), #5),
  medRMSPE = c(median(sg120_12x10$rand.app2.rmspe), median(sg120_20x6$rand.app2.rmspe), median(sg120_30x4$rand.app2.rmspe), #median(sg120_40x3$rand.app2.rmspe),
               median(sg120_12x10$lpm.app2.rmspe), median(sg120_20x6$lpm.app2.rmspe), median(sg120_30x4$lpm.app2.rmspe), #median(sg120_40x3$lpm.app2.rmspe),
               median(sg120_12x10$lpm.up.rmspe), median(sg120_20x6$lpm.up.rmspe), median(sg120_30x4$lpm.up.rmspe)), #median(sg120_40x3$lpm.up.rmspe)),
  # medRMSPE = c(median(sg120_12x10$rand.app2.plume.rmspe), median(sg120_20x6$rand.app2.plume.rmspe), median(sg120_30x4$rand.app2.plume.rmspe), #median(sg120_40x3$rand.app2.plume.rmspe),
  #              median(sg120_12x10$lpm.app2.plume.rmspe), median(sg120_20x6$lpm.app2.plume.rmspe), median(sg120_30x4$lpm.app2.plume.rmspe), #median(sg120_40x3$lpm.app2.plume.rmspe),
  #              median(sg120_12x10$lpm.up.plume.rmspe), median(sg120_20x6$lpm.up.plume.rmspe), median(sg120_30x4$lpm.up.plume.rmspe)), #median(sg120_40x3$lpm.up.plume.rmspe)),
  method = c("srs", "srs", "srs", #"srs",
             "eLPM", "eLPM", 'eLPM', #'eLPM',
             'pLPM', 'pLPM', 'pLPM') #'pLPM'
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\medRMSPE_total.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medRMSPE)) +
  geom_point(aes(color=method)) +
  ylim(7.5,8.5) +
  ggtitle(label='120 samples total') +
  geom_smooth(aes(color=method))

dev.off()


#################### 240 samps #################

assign('sg240_24x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\240samps\\24x10\\rmspe_plume.RData")))
assign('sg240_30x8', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\240samps\\30x8\\rmspe_plume.RData")))
assign('sg240_40x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\240samps\\40x6\\rmspe_plume.RData")))
assign('sg240_48x5', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\240samps\\48x5\\rmspe_plume.RData")))

# 240 samples in total - balanced lpm
df <- data.frame(
  samples = c(240, 240, 240, 240,
              240, 240, 240, 240,
              240, 240, 240, 240),
  freq = c(10, 8, 6, 5,
           10, 8, 6, 5,
           10, 8, 6, 5),
  # medRMSPE = c(median(sg240_24x10$rand.app2.rmspe), median(sg240_30x8$rand.app2.rmspe), median(sg240_40x6$rand.app2.rmspe), median(sg240_48x5$rand.app2.rmspe),
  #              median(sg240_24x10$lpm.app2.rmspe), median(sg240_30x8$lpm.app2.rmspe), median(sg240_40x6$lpm.app2.rmspe), median(sg240_48x5$lpm.app2.rmspe),
  #              median(sg240_24x10$lpm.up.rmspe), median(sg240_30x8$lpm.up.rmspe), median(sg240_40x6$lpm.up.rmspe), median(sg240_48x5$lpm.up.rmspe)),
  medRMSPE = c(median(sg240_24x10$rand.app2.plume.rmspe), median(sg240_30x8$rand.app2.plume.rmspe), median(sg240_40x6$rand.app2.plume.rmspe), median(sg240_48x5$rand.app2.plume.rmspe),
               median(sg240_24x10$lpm.app2.plume.rmspe), median(sg240_30x8$lpm.app2.plume.rmspe), median(sg240_40x6$lpm.app2.plume.rmspe), median(sg240_48x5$lpm.app2.plume.rmspe),
               median(sg240_24x10$lpm.up.plume.rmspe), median(sg240_30x8$lpm.up.plume.rmspe), median(sg240_40x6$lpm.up.plume.rmspe), median(sg240_48x5$lpm.up.plume.rmspe)),
  method = c("srs", "srs", "srs", "srs",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\noise_0\\freq_test\\240samps\\medRMSPE_plume.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medRMSPE)) +
  geom_point(aes(color=method)) +
  ylim(1,2.5) +
  ggtitle(label='240 samples total') +
  geom_smooth(aes(color=method))

dev.off()


#################### PLUME MASS ################

limit = 0.002
plume.dat <- true.data %>% filter(y >= limit)
real.pmass <- sum(plume.dat$y)/3500

#################### 20 samps #################

assign('sg20_2x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\20samps\\2x10\\pmass.RData")))
assign('sg20_4x5', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\20samps\\4x5\\pmass.RData")))
assign('sg20_5x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\20samps\\5x4\\pmass.RData")))
assign('sg20_10x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\20samps\\10x2\\pmass.RData")))

# 20 samples in total - balanced lpm
df <- data.frame(
  samples = c(20, 20, 20, 20,
              20, 20, 20, 20,
              20, 20, 20, 20),
  freq = c(10, 5, 4, 2,
           10, 5, 4, 2,
           10, 5, 4, 2),
  medpmass = c(median(sg20_2x10$rand.app2.pmass), median(sg20_4x5$rand.app2.pmass), median(sg20_5x4$rand.app2.pmass), median(sg20_10x2$rand.app2.pmass),
               median(sg20_2x10$lpm.app2.pmass), median(sg20_4x5$lpm.app2.pmass), median(sg20_5x4$lpm.app2.pmass), median(sg20_10x2$lpm.app2.pmass),
               median(sg20_2x10$lpm.up.pmass), median(sg20_4x5$lpm.up.pmass), median(sg20_5x4$lpm.up.pmass), median(sg20_10x2$lpm.up.pmass)),
  method = c("srs", "srs", "srs", "srs",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\20samps\\medpmass.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medpmass)) +
  geom_point(aes(color=method)) +
  ylim(1,500) +
  ggtitle(label='Plume mass, 20 samples total') +
  geom_smooth(aes(color=method)) +
  geom_hline(yintercept = real.pmass, color='red')

dev.off()


#################### 30 samps #################

assign('sg30_3x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\3x10\\pmass.RData")))
assign('sg30_5x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\6x5\\pmass.RData")))
assign('sg30_10x3', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\10x3\\pmass.RData")))
# assign('sg30_15x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\30samps\\15x2\\pmass.RData")))

# 30 samples in total - balanced lpm
df <- data.frame(
  samples = c(30, 30, 30, #30,
              30, 30, 30, #30,
              30, 30, 30), #30),
  freq = c(10, 6, 3, #2,
           10, 6, 3, #2,
           10, 6, 3), #2),
  medpmass = c(median(sg30_3x10$rand.app2.pmass), median(sg30_5x6$rand.app2.pmass), median(sg30_10x3$rand.app2.pmass), #median(sg30_15x2$rand.app2.pmass),
               median(sg30_3x10$lpm.app2.pmass), median(sg30_5x6$lpm.app2.pmass), median(sg30_10x3$lpm.app2.pmass), #median(sg30_15x2$lpm.app2.pmass),
               median(sg30_3x10$lpm.up.pmass), median(sg30_5x6$lpm.up.pmass), median(sg30_10x3$lpm.up.pmass)), #median(sg30_15x2$lpm.up.pmass)),
  method = c("srs", "srs", "srs", #"srs",
             "eLPM", "eLPM", 'eLPM', #'eLPM',
             'pLPM', 'pLPM', 'pLPM') #'pLPM'
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\30samps\\medpmass.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medpmass)) +
  geom_point(aes(color=method)) +
  ylim(1,500) +
  ggtitle(label='30 samples total') +
  geom_smooth(aes(color=method)) +
  geom_hline(yintercept = 430, color='red')

dev.off()


#################### 60 samps #################

assign('sg60_6x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\6x10\\pmass.RData")))
assign('sg60_10x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\10x6\\pmass.RData")))
assign('sg60_15x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\15x4\\pmass.RData")))
assign('sg60_30x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\20x3\\pmass.RData")))

# 60 samples in total - balanced lpm
df <- data.frame(
  samples = c(60, 60, 60, 60,
              60, 60, 60, 60,
              60, 60, 60, 60),
  freq = c(10, 6, 4, 2,
           10, 6, 4, 2,
           10, 6, 4, 2),
  medpmass = c(median(sg60_6x10$rand.app2.pmass), median(sg60_10x6$rand.app2.pmass), median(sg60_15x4$rand.app2.pmass), median(sg60_30x2$rand.app2.pmass),
               median(sg60_6x10$lpm.app2.pmass), median(sg60_10x6$lpm.app2.pmass), median(sg60_15x4$lpm.app2.pmass), median(sg60_30x2$lpm.app2.pmass),
               median(sg60_6x10$lpm.up.pmass), median(sg60_10x6$lpm.up.pmass), median(sg60_15x4$lpm.up.pmass), median(sg60_30x2$lpm.up.pmass)),
  method = c("srs", "srs", "srs", "srs",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\60samps\\medpmass.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medpmass)) +
  geom_point(aes(color=method)) +
  ylim(1,500) +
  ggtitle(label='60 samples total') +
  geom_smooth(aes(color=method)) +
  geom_hline(yintercept = 430, color='red')

dev.off()


#################### 120 samps #################

assign('sg120_12x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\12x10\\pmass.RData")))
assign('sg120_20x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\15x8\\pmass.RData")))
assign('sg120_30x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\24x5\\pmass.RData")))
# assign('sg120_40x3', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test_convex\\120samps\\24x5\\pmass.RData")))

# 120 samples in total - balanced lpm
df <- data.frame(
  samples = c(120, 120, 120, #120,
              120, 120, 120, #120,
              120, 120, 120), #120),
  freq = c(10, 8, 6, #5,
           10, 8, 6, #5,
           10, 8, 6), #5),
  medpmass = c(median(sg120_12x10$rand.app2.pmass), median(sg120_20x6$rand.app2.pmass), median(sg120_30x4$rand.app2.pmass), #median(sg120_40x3$rand.app2.pmass),
               median(sg120_12x10$lpm.app2.pmass), median(sg120_20x6$lpm.app2.pmass), median(sg120_30x4$lpm.app2.pmass), #median(sg120_40x3$lpm.app2.pmass),
               median(sg120_12x10$lpm.up.pmass), median(sg120_20x6$lpm.up.pmass), median(sg120_30x4$lpm.up.pmass)), #median(sg120_40x3$lpm.up.pmass)),
  method = c("srs", "srs", "srs", #"srs",
             "eLPM", "eLPM", 'eLPM', #'eLPM',
             'pLPM', 'pLPM', 'pLPM') #'pLPM'
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_24\\noise_15\\freq_test\\120samps\\medpmass.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medpmass)) +
  geom_point(aes(color=method)) +
  ylim(100,440) +
  ggtitle(label='120 samples total') +
  geom_smooth(aes(color=method)) +
  geom_hline(yintercept = 430, color='red')

dev.off()


#################### 240 samps #################

assign('sg240_24x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\240samps\\24x10\\pmass.RData")))
assign('sg240_30x8', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\240samps\\30x8\\pmass.RData")))
assign('sg240_40x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\240samps\\40x6\\pmass.RData")))
assign('sg240_48x5', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\240samps\\48x5\\pmass.RData")))

# 240 samples in total - balanced lpm
df <- data.frame(
  samples = c(240, 240, 240, 240,
              240, 240, 240, 240,
              240, 240, 240, 240),
  freq = c(10, 8, 6, 5,
           10, 8, 6, 5,
           10, 8, 6, 5),
  medpmass = c(median(sg240_24x10$rand.app2.pmass), median(sg240_30x8$rand.app2.pmass), median(sg240_40x6$rand.app2.pmass), median(sg240_48x5$rand.app2.pmass),
               median(sg240_24x10$lpm.app2.pmass), median(sg240_30x8$lpm.app2.pmass), median(sg240_40x6$lpm.app2.pmass), median(sg240_48x5$lpm.app2.pmass),
               median(sg240_24x10$lpm.up.pmass), median(sg240_30x8$lpm.up.pmass), median(sg240_40x6$lpm.up.pmass), median(sg240_48x5$lpm.up.pmass)),
  method = c("srs", "srs", "srs", "srs",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48\\noise_0\\freq_test\\240samps\\medpmass.png"),
    width = 500, height = 400)

ggplot(data = df, aes(x=freq, y=medpmass)) +
  geom_point(aes(color=method)) +
  ylim(1,600) +
  ggtitle(label='240 samples total') +
  geom_smooth(aes(color=method)) +
  geom_hline(yintercept = real.pmass, color='red')

dev.off()


############################# as a function of TOTAL NUMBER OF SAMPLES ###########
############ PLUME MASS##################################################

up95 <- function(vector) quantile(vector, 0.975)
low95 <- function(vector) quantile(vector, 0.025)

assign('sg60_6x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\30samps\\3x10\\pmass.RData")))
assign('sg60_10x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\60samps\\6x10\\pmass.RData")))
assign('sg60_15x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\120samps\\12x10\\pmass.RData")))
assign('sg60_30x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\240samps\\24x10\\pmass.RData")))


# 60 samples in total - balanced lpm
df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  medpmass = c(median(sg60_6x10$rand.app2.pmass/real.pmass), median(sg60_10x6$rand.app2.pmass/real.pmass), median(sg60_15x4$rand.app2.pmass/real.pmass), median(sg60_30x2$rand.app2.pmass/real.pmass), 
               median(sg60_30x2$all.pmass/real.pmass),
               median(sg60_6x10$lpm.app2.pmass/real.pmass), median(sg60_10x6$lpm.app2.pmass/real.pmass), median(sg60_15x4$lpm.app2.pmass/real.pmass), median(sg60_30x2$lpm.app2.pmass/real.pmass), 
               median(sg60_30x2$all.pmass/real.pmass),
               median(sg60_6x10$lpm.up.pmass/real.pmass), median(sg60_10x6$lpm.up.pmass/real.pmass), median(sg60_15x4$lpm.up.pmass/real.pmass), median(sg60_30x2$lpm.up.pmass/real.pmass), 
               median(sg60_30x2$all.pmass/real.pmass)),
  # upper 95% bound
  upbounds = c(up95(sg60_6x10$rand.app2.pmass/real.pmass), up95(sg60_10x6$rand.app2.pmass/real.pmass), up95(sg60_15x4$rand.app2.pmass/real.pmass), up95(sg60_30x2$rand.app2.pmass/real.pmass), 
               up95(sg60_30x2$all.pmass/real.pmass),
               up95(sg60_6x10$lpm.app2.pmass/real.pmass), up95(sg60_10x6$lpm.app2.pmass/real.pmass), up95(sg60_15x4$lpm.app2.pmass/real.pmass), up95(sg60_30x2$lpm.app2.pmass/real.pmass), 
               up95(sg60_30x2$all.pmass/real.pmass),
               up95(sg60_6x10$lpm.up.pmass/real.pmass), up95(sg60_10x6$lpm.up.pmass/real.pmass), up95(sg60_15x4$lpm.up.pmass/real.pmass), up95(sg60_30x2$lpm.up.pmass/real.pmass), 
               up95(sg60_30x2$all.pmass/real.pmass)),
  # lower 95% bound
  lowbounds = c(low95(sg60_6x10$rand.app2.pmass/real.pmass), low95(sg60_10x6$rand.app2.pmass/real.pmass), low95(sg60_15x4$rand.app2.pmass/real.pmass), low95(sg60_30x2$rand.app2.pmass/real.pmass), 
                low95(sg60_30x2$all.pmass/real.pmass),
                low95(sg60_6x10$lpm.app2.pmass/real.pmass), low95(sg60_10x6$lpm.app2.pmass/real.pmass), low95(sg60_15x4$lpm.app2.pmass/real.pmass), low95(sg60_30x2$lpm.app2.pmass/real.pmass), 
                low95(sg60_30x2$all.pmass/real.pmass),
                low95(sg60_6x10$lpm.up.pmass/real.pmass), low95(sg60_10x6$lpm.up.pmass/real.pmass), low95(sg60_15x4$lpm.up.pmass/real.pmass), low95(sg60_30x2$lpm.up.pmass/real.pmass), 
                low95(sg60_30x2$all.pmass/real.pmass)),
  
  method = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)



png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\pmass_ratio.png"),
    width = 1000, height = 800)

# ggplot(data = df, aes(x=samples, y=medpmass)) +
#   geom_line(aes(color=method), linewidth=2) +
#   geom_line(aes(x=samples, y=upbounds, color=method), linetype='dotted', linewidth=2) +
#   geom_line(aes(x=samples, y=lowbounds, color=method), linetype='dotted', linewidth=2) +
#   ylim(min(df$lowbounds),430) +
#   ggtitle(label='number of samples v median plume mass') +
#   geom_hline(yintercept = 430, color='red')

ggplot(data = df, aes(x=samples, y=medpmass)) +
  geom_line(aes(color=method), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = method, fill=method), alpha=0.2, linewidth=1, linetype = 'dotted') +
  #ylim(min(df$lowbounds), max(df$upbounds)) +
  ylim(0, max(df$upbounds)) +
  xlim(min(df$samples), max(df$samples)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=15)) +
  xlab("Sample size") +
  ylab("Plume mass ratio (estimated/true)") +
  #ggtitle(label='Estimated plume mass as a function of the total number of samples') +
  geom_hline(yintercept = 1, color='black', linewidth=1) +
  #geom_text(aes(30, 1, label = 'true plume mass', vjust = -.8, hjust = -.2))
  scale_y_continuous(trans='log10')

dev.off()

# boxplots

#30
load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\30samps\\3x10\\pmass.RData")
#60
load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\60samps\\6x10\\pmass.RData")
#120
load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\120samps\\12x10\\pmass.RData")
#240
load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\240samps\\24x10\\pmass.RData")

if (max(pmass[,2:5]) > real.pmass) {
  ew_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = lpm.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)) +
    ggtitle("equal weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black') +
    scale_y_continuous(trans='log10', limits = c(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)))+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
  
  uw_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = lpm.up.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)) +
    ggtitle("prop. weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black')+
    scale_y_continuous(trans='log10', limits = c(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)))+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
  
  rand_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = rand.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)) +
    ggtitle("simple random") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black')+
    scale_y_continuous(trans='log10', limits = c(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)))+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
  
  all_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = all.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)) +
    ggtitle("all pot. samples") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black')+
    scale_y_continuous(trans='log10', limits = c(min(pmass[,2:5]/real.pmass), max(pmass[,2:5]/real.pmass)))+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
} else {
  ew_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = lpm.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), real.pmass/real.pmass) +
    ggtitle("equal weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black')+
    scale_y_continuous(trans='log10', limits = min(pmass[,2:5]/real.pmass), real.pmass/real.pmass)+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
  
  uw_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = lpm.up.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), real.pmass/real.pmass) +
    ggtitle("prop. weights") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black')+
    scale_y_continuous(trans='log10', limits = min(pmass[,2:5]/real.pmass), real.pmass/real.pmass)+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
  
  rand_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = rand.app2.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), real.pmass/real.pmass) +
    ggtitle("simple random") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black')+
    scale_y_continuous(trans='log10', limits = min(pmass[,2:5]/real.pmass), real.pmass/real.pmass)+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
  
  all_plot <- ggplot(data = pmass/real.pmass, mapping = aes(x = '', y = all.pmass)) +
    geom_boxplot() +
    ylim(min(pmass[,2:5]/real.pmass), real.pmass/real.pmass) +
    ggtitle("all pot. samples") +
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept = 1, color='black')+
    scale_y_continuous(trans='log10', limits = min(pmass[,2:5]/real.pmass), real.pmass/real.pmass)+
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=16), 
          legend.text = element_text(size=15), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=15))
}



layout <- rbind(c(1,2,3,4))

# filename
png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\240samps\\pmass_ratio_box.png"),
    width = 800, height = 600)

grid.arrange(all_plot, rand_plot, ew_plot, uw_plot, layout_matrix = layout) #, top = text_grob("plume mass", size = 18))

dev.off()

####################### RMSPE ###########################################################

assign('sg60_6x10', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\240samps\\24x10\\rmspe_total.RData")))


# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# 60 samples in total - balanced lpm
df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_30x2$all.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_30x2$all.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe),
               median(sg60_30x2$all.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_30x2$all.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_30x2$all.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe),
               up95(sg60_30x2$all.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_30x2$all.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_30x2$all.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe),
                low95(sg60_30x2$all.rmspe)),
  
  
  # Plume RMSPE
  # medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe), median(sg60_30x2$all.plume.rmspe),
  #              median(sg60_6x10$lpm.app2.plume.rmspe),  median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe), median(sg60_30x2$all.plume.rmspe),
  #              median(sg60_6x10$lpm.up.plume.rmspe),  median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe), median(sg60_30x2$all.plume.rmspe)),
  # 
  # # RMSPE upper bounds - plume
  # upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
  #              up95(sg60_30x2$all.plume.rmspe),
  #              up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
  #              up95(sg60_30x2$all.plume.rmspe),
  #              up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
  #              up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds - plume
  # lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
  #               low95(sg60_30x2$all.plume.rmspe),
  #               low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
  #               low95(sg60_30x2$all.plume.rmspe),
  #               low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
  #               low95(sg60_30x2$all.plume.rmspe)),
  
  # RMSE
  # medRMSPE = c(median(sg60_6x10$rand.app2.rmse), median(sg60_10x6$rand.app2.rmse), median(sg60_15x4$rand.app2.rmse), median(sg60_30x2$rand.app2.rmse),
  #              median(sg60_30x2$all.rmse),
  #              median(sg60_6x10$lpm.app2.rmse), median(sg60_10x6$lpm.app2.rmse), median(sg60_15x4$lpm.app2.rmse), median(sg60_30x2$lpm.app2.rmse),
  #              median(sg60_30x2$all.rmse),
  #              median(sg60_6x10$lpm.up.rmse), median(sg60_10x6$lpm.up.rmse), median(sg60_15x4$lpm.up.rmse), median(sg60_30x2$lpm.up.rmse),
  #              median(sg60_30x2$all.rmse)),
  # RMSE upper bounds
  # upbounds = c(up95(sg60_6x10$rand.app2.rmse), up95(sg60_10x6$rand.app2.rmse), up95(sg60_15x4$rand.app2.rmse), up95(sg60_30x2$rand.app2.rmse),
  #              up95(sg60_30x2$all.rmse),
  #              up95(sg60_6x10$lpm.app2.rmse), up95(sg60_10x6$lpm.app2.rmse), up95(sg60_15x4$lpm.app2.rmse), up95(sg60_30x2$lpm.app2.rmse),
  #              up95(sg60_30x2$all.rmse),
  #              up95(sg60_6x10$lpm.up.rmse), up95(sg60_10x6$lpm.up.rmse), up95(sg60_15x4$lpm.up.rmse), up95(sg60_30x2$lpm.up.rmse),
  #              up95(sg60_30x2$all.rmse)),
  # # RMSE lower bounds
  # lowbounds = c(low95(sg60_6x10$rand.app2.rmse), low95(sg60_10x6$rand.app2.rmse), low95(sg60_15x4$rand.app2.rmse), low95(sg60_30x2$rand.app2.rmse),
  #               low95(sg60_30x2$all.rmse),
  #               low95(sg60_6x10$lpm.app2.rmse), low95(sg60_10x6$lpm.app2.rmse), low95(sg60_15x4$lpm.app2.rmse), low95(sg60_30x2$lpm.app2.rmse),
  #               low95(sg60_30x2$all.rmse),
  #               low95(sg60_6x10$lpm.up.rmse), low95(sg60_10x6$lpm.up.rmse), low95(sg60_15x4$lpm.up.rmse), low95(sg60_30x2$lpm.up.rmse),
  #               low95(sg60_30x2$all.rmse)),
  
  method = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\rmspe_samples.png"),
   width = 800, height = 600)

# ggplot(data = df, aes(x=samples, y=medRMSPE)) +
#   geom_line(aes(color=method), size=2) +
#   #geom_smooth(aes(color=method)) +
#   geom_line(aes(x=samples, y=upbounds, color=method),linetype='dotted', size=2) +
#   #geom_smooth(aes(x=samples, y=upbounds, color=method)) +
#   geom_line(aes(x=samples, y=lowbounds, color=method),linetype='dotted', size=2) +
#   #geom_smooth(aes(x=samples, y=upbounds, color=method)) +
#   ylim(min(df$lowbounds),max(df$upbounds)) +
#   ggtitle(label='RMSPE as a function of the total number of samples taken')

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=method), size=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = method, fill=method), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=15)) +
  xlab("Sample size") +
  ylab("RMSPE")
  #ggtitle(label='RMSE as a function of the total number of samples')

dev.off()



#################################### General Frequency Plot ################################

up95 <- function(vector) quantile(vector, 0.975)
low95 <- function(vector) quantile(vector, 0.025)

load("C://Users//2608904R//OneDrive - University of Glasgow//PhD//well_influence_analysis_sim_study//data//complex_plume.RData")

path <- 'C:\\Users\\2608904R\\OneDrive - University of Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\rand_48_3\\noise_15\\nseg_6\\convex\\'
samp <- 240
file <- 'rmspe_plume'

samps <- paste0(samp, 'samps')
freqs <- sort(
  as.numeric(
    list.dirs(path = paste0(path, samps), full.names = FALSE, recursive = FALSE)), 
  decreasing = TRUE)

assign('freq.1', get(load(paste0(path, samps, '\\', freqs[1], '\\', file, '.RData'))))
assign('freq.2', get(load(paste0(path, samps, '\\', freqs[2], '\\', file, '.RData'))))
assign('freq.3', get(load(paste0(path, samps, '\\', freqs[3], '\\', file, '.RData'))))
assign('freq.4', get(load(paste0(path, samps, '\\', freqs[4], '\\', file, '.RData'))))
# assign('freq.5', get(load(paste0(path, samps, '\\', freqs[5], '\\', file, '.RData'))))
# assign('freq.6', get(load(paste0(path, samps, '\\', freqs[6], '\\', file, '.RData'))))


limit = 0.002
plume.dat <- true.data %>% filter(y >= limit)
real.pmass <- sum(plume.dat$y)/3500


df <- data.frame(
  samples = rep(samp, 3*length(freqs)),
  
  freq = rep(freqs, 3),
  
  # Total RMSPE
  # RMSPE = c(median(freq.1$rand.app2.rmspe), median(freq.2$rand.app2.rmspe), median(freq.3$rand.app2.rmspe), median(freq.4$rand.app2.rmspe),
  #           #median(freq.5$rand.app2.rmspe), median(freq.6$rand.app2.rmspe),
  #           median(freq.1$lpm.app2.rmspe), median(freq.2$lpm.app2.rmspe), median(freq.3$lpm.app2.rmspe), median(freq.4$lpm.app2.rmspe),
  #           #median(freq.5$lpm.app2.rmspe), median(freq.6$lpm.app2.rmspe),
  #           median(freq.1$lpm.up.rmspe), median(freq.2$lpm.up.rmspe), median(freq.3$lpm.up.rmspe), median(freq.4$lpm.up.rmspe)),
  #           #median(freq.5$lpm.up.rmspe), median(freq.6$lpm.up.rmspe)),
  
  # Plume RMSPE
  RMSPE = c(median(freq.1$rand.app2.plume.rmspe), median(freq.2$rand.app2.plume.rmspe), median(freq.3$rand.app2.plume.rmspe), median(freq.4$rand.app2.plume.rmspe),
            #median(freq.5$rand.app2.plume.rmspe), median(freq.6$rand.app2.plume.rmspe),
            median(freq.1$lpm.app2.plume.rmspe), median(freq.2$lpm.app2.plume.rmspe), median(freq.3$lpm.app2.plume.rmspe), median(freq.4$lpm.app2.plume.rmspe),
            #median(freq.5$lpm.app2.plume.rmspe), median(freq.6$lpm.app2.plume.rmspe),
            median(freq.1$lpm.up.plume.rmspe), median(freq.2$lpm.up.plume.rmspe), median(freq.3$lpm.up.plume.rmspe), median(freq.4$lpm.up.plume.rmspe)),
            #median(freq.5$lpm.up.plume.rmspe), median(freq.6$lpm.up.plume.rmspe)),
  
  # Plume Mass Ratio
  # RMSPE = c(median(freq.1$rand.app2.pmass/real.pmass), median(freq.2$rand.app2.pmass/real.pmass), median(freq.3$rand.app2.pmass/real.pmass), median(freq.4$rand.app2.pmass/real.pmass),
  #           #median(freq.5$rand.app2.pmass/real.pmass), median(freq.6$rand.app2.pmass/real.pmass),
  #           median(freq.1$lpm.app2.pmass/real.pmass), median(freq.2$lpm.app2.pmass/real.pmass), median(freq.3$lpm.app2.pmass/real.pmass), median(freq.4$lpm.app2.pmass/real.pmass),
  #           #median(freq.5$lpm.app2.pmass/real.pmass), median(freq.6$lpm.app2.pmass/real.pmass),
  #           median(freq.1$lpm.up.pmass/real.pmass), median(freq.2$lpm.up.pmass/real.pmass), median(freq.3$lpm.up.pmass/real.pmass), median(freq.4$lpm.up.pmass/real.pmass)),
  #           #median(freq.5$lpm.up.pmass/real.pmass), median(freq.6$lpm.up.pmass/real.pmass)),

  # RMSPE upper bounds
  # upbounds = c(up95(freq.1$rand.app2.rmspe), up95(freq.2$rand.app2.rmspe), up95(freq.3$rand.app2.rmspe), up95(freq.4$rand.app2.rmspe),
  #              #up95(freq.5$rand.app2.rmspe), up95(freq.6$rand.app2.rmspe),
  #              up95(freq.1$lpm.app2.rmspe), up95(freq.2$lpm.app2.rmspe), up95(freq.3$lpm.app2.rmspe), up95(freq.4$lpm.app2.rmspe),
  #              #up95(freq.5$lpm.app2.rmspe), up95(freq.5$lpm.app2.rmspe),
  #              up95(freq.1$lpm.up.rmspe), up95(freq.2$lpm.up.rmspe), up95(freq.3$lpm.up.rmspe), up95(freq.4$lpm.up.rmspe)),
  #              #up95(freq.5$lpm.up.rmspe), up95(freq.6$lpm.up.rmspe)),
  
  # Plume RMSPE upper bounds
  upbounds = c(up95(freq.1$rand.app2.plume.rmspe), up95(freq.2$rand.app2.plume.rmspe), up95(freq.3$rand.app2.plume.rmspe), up95(freq.4$rand.app2.plume.rmspe),
               #up95(freq.5$rand.app2.plume.rmspe), up95(freq.6$rand.app2.plume.rmspe),
               up95(freq.1$lpm.app2.plume.rmspe), up95(freq.2$lpm.app2.plume.rmspe), up95(freq.3$lpm.app2.plume.rmspe), up95(freq.4$lpm.app2.plume.rmspe),
               #up95(freq.5$lpm.app2.plume.rmspe), up95(freq.5$lpm.app2.plume.rmspe),
               up95(freq.1$lpm.up.plume.rmspe), up95(freq.2$lpm.up.plume.rmspe), up95(freq.3$lpm.up.plume.rmspe), up95(freq.4$lpm.up.plume.rmspe)),
               #up95(freq.5$lpm.up.plume.rmspe), up95(freq.6$lpm.up.plume.rmspe)),
  
  # Plume mass upper bounds
  # upbounds = c(up95(freq.1$rand.app2.pmass/real.pmass), up95(freq.2$rand.app2.pmass/real.pmass), up95(freq.3$rand.app2.pmass/real.pmass), up95(freq.4$rand.app2.pmass/real.pmass),
  #              #up95(freq.5$rand.app2.pmass/real.pmass), up95(freq.6$rand.app2.pmass/real.pmass),
  #              up95(freq.1$lpm.app2.pmass/real.pmass), up95(freq.2$lpm.app2.pmass/real.pmass), up95(freq.3$lpm.app2.pmass/real.pmass), up95(freq.4$lpm.app2.pmass/real.pmass),
  #              #up95(freq.5$lpm.app2.pmass/real.pmass), up95(freq.5$lpm.app2.pmass/real.pmass),
  #              up95(freq.1$lpm.up.pmass/real.pmass), up95(freq.2$lpm.up.pmass/real.pmass), up95(freq.3$lpm.up.pmass/real.pmass), up95(freq.4$lpm.up.pmass/real.pmass)),
  #              #up95(freq.5$lpm.up.pmass/real.pmass), up95(freq.6$lpm.up.pmass/real.pmass)),
  
  # RMSPE lower bounds
  # lowbounds = c(low95(freq.1$rand.app2.rmspe), low95(freq.2$rand.app2.rmspe), low95(freq.3$rand.app2.rmspe), low95(freq.4$rand.app2.rmspe),
  #               #low95(freq.5$rand.app2.rmspe), low95(freq.6$rand.app2.rmspe),
  #               low95(freq.1$lpm.app2.rmspe), low95(freq.2$lpm.app2.rmspe), low95(freq.3$lpm.app2.rmspe), low95(freq.4$lpm.app2.rmspe),
  #               #low95(freq.5$lpm.app2.rmspe), low95(freq.6$lpm.app2.rmspe),
  #               low95(freq.1$lpm.up.rmspe), low95(freq.2$lpm.up.rmspe), low95(freq.3$lpm.up.rmspe), low95(freq.4$lpm.up.rmspe)),
  #               #low95(freq.5$lpm.up.rmspe), low95(freq.6$lpm.up.rmspe)),
  
  # Plume RMSPE lower bounds
  lowbounds = c(low95(freq.1$rand.app2.plume.rmspe), low95(freq.2$rand.app2.plume.rmspe), low95(freq.3$rand.app2.plume.rmspe), low95(freq.4$rand.app2.plume.rmspe),
                #low95(freq.5$rand.app2.plume.rmspe), low95(freq.6$rand.app2.plume.rmspe),
                low95(freq.1$lpm.app2.plume.rmspe), low95(freq.2$lpm.app2.plume.rmspe), low95(freq.3$lpm.app2.plume.rmspe), low95(freq.4$lpm.app2.plume.rmspe),
                #low95(freq.5$lpm.app2.plume.rmspe), low95(freq.6$lpm.app2.plume.rmspe),
                low95(freq.1$lpm.up.plume.rmspe), low95(freq.2$lpm.up.plume.rmspe), low95(freq.3$lpm.up.plume.rmspe), low95(freq.4$lpm.up.plume.rmspe)),
                #low95(freq.5$lpm.up.plume.rmspe), low95(freq.6$lpm.up.plume.rmspe)),
  
  # Plume mass lower bounds
  # lowbounds = c(low95(freq.1$rand.app2.pmass/real.pmass), low95(freq.2$rand.app2.pmass/real.pmass), low95(freq.3$rand.app2.pmass/real.pmass), low95(freq.4$rand.app2.pmass/real.pmass),
  #               #low95(freq.5$rand.app2.pmass/real.pmass), low95(freq.6$rand.app2.pmass/real.pmass),
  #               low95(freq.1$lpm.app2.pmass/real.pmass), low95(freq.2$lpm.app2.pmass/real.pmass), low95(freq.3$lpm.app2.pmass/real.pmass), low95(freq.4$lpm.app2.pmass/real.pmass),
  #               #low95(freq.5$lpm.app2.pmass/real.pmass), low95(freq.6$lpm.app2.pmass/real.pmass),
  #               low95(freq.1$lpm.up.pmass/real.pmass), low95(freq.2$lpm.up.pmass/real.pmass), low95(freq.3$lpm.up.pmass/real.pmass), low95(freq.4$lpm.up.pmass/real.pmass)),
  #               #low95(freq.5$lpm.up.pmass/real.pmass), low95(freq.6$lpm.up.pmass/real.pmass)),
  
  method = c(rep('SRS', length(freqs)),
             rep('eLPM', length(freqs)),
             rep('pLPM', length(freqs)))
)

# RMSPE

# png(paste0(path, samps, '\\RMSPE_freq.png'),
#     width = 800, height = 600)
# 
# ggplot(data = df, aes(x=freq, y=RMSPE)) +
#   geom_line(aes(color=method), size=2) +
#   geom_ribbon(aes(x=freq, ymax=upbounds, ymin=lowbounds, color = method, fill=method), alpha=0.2, linewidth=1, linetype = 'dotted') +
#   ylim(min(df$lowbounds), max(df$upbounds)) +
#   theme(axis.text=element_text(size=15),
#         axis.title=element_text(size=16),
#         legend.text = element_text(size=15),
#         legend.key.size = unit(1, 'cm'),
#         legend.title = element_text(size=15),
#         plot.title = element_text(size=16)) +
#   xlab("Number of Sampling Events") +
#   ylab("RMSPE") +
#   ggtitle(paste0('Total Number of Samples = ', samp))
# 
# dev.off()

# Plume RMSPE

png(paste0(path, samps, '\\plume_RMSPE_freq.png'),
    width = 800, height = 600)

ggplot(data = df, aes(x=freq, y=RMSPE)) +
  geom_line(aes(color=method), size=2) +
  geom_ribbon(aes(x=freq, ymax=upbounds, ymin=lowbounds, color = method, fill=method), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16),
        legend.text = element_text(size=15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=15),
        plot.title = element_text(size=16)) +
  xlab("Number of Sampling Events") +
  ylab("Plume RMSPE") +
  ggtitle(paste0('Total Number of Samples = ', samp))

dev.off()

# Plume mass

# png(paste0(path, samps, '\\pmass_freq.png'),
#     width = 800, height = 600)
# 
# ggplot(data = df, aes(x=freq, y=RMSPE)) +
#   geom_line(aes(color=method), size=2) +
#   geom_ribbon(aes(x=freq, ymax=upbounds, ymin=lowbounds, color = method, fill=method), alpha=0.2, linewidth=1, linetype = 'dotted') +
#   ylim(min(df$lowbounds), max(df$upbounds)) +
#   theme(axis.text=element_text(size=15),
#         axis.title=element_text(size=16),
#         legend.text = element_text(size=15),
#         legend.key.size = unit(1, 'cm'),
#         legend.title = element_text(size=15),
#         plot.title = element_text(size=16)) +
#   xlab("Number of Sampling Events") +
#   ylab("Plume mass") +
#   ggtitle(paste0('Total Number of Samples = ', samp)) +
#   scale_y_continuous(trans='log10') +
#   geom_hline(yintercept = 1, color='black', linewidth=1)
# 
# dev.off()
