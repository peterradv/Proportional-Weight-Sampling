
# Functions to calculate 95% variability bands
up95 <- function(vector) quantile(vector, 0.975)
low95 <- function(vector) quantile(vector, 0.025)

#### COMPLEX PLUME

### RANDOM NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_complex_rand_48.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_complex_rand_48.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_complex_rand_24.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_complex_rand_24.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

### GRID NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_complex_grid_48.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_complex_grid_48.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\grid_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\grid_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\grid_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\grid_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_complex_grid_24.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\grid_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\grid_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\grid_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\grid_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_complex_grid_24.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

#### MID PLUME

### GRID NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\grid_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\grid_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\grid_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\grid_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_mid_grid_48.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\grid_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\grid_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\grid_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\grid_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_mid_grid_48.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\grid_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\grid_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\grid_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\grid_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_mid_grid_24.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\grid_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\grid_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\grid_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\grid_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_mid_grid_24.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()


### RANDOM NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_mid_rand_48.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_mid_rand_48.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\rand_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\rand_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\rand_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\rand_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_mid_rand_24.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\mid plume\\rand_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\mid plume\\rand_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\mid plume\\rand_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\mid plume\\rand_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_mid_rand_24.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

#### SIMPLE PLUME

### GRID NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\grid_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\grid_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\grid_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\grid_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_simple_grid_48.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\grid_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\grid_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\grid_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\grid_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_simple_grid_48.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\grid_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\grid_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\grid_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\grid_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_simple_grid_24.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\grid_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\grid_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\grid_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\grid_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_simple_grid_24.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()


### RANDOM NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_simple_rand_48.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_simple_rand_48.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\rand_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\rand_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\rand_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\rand_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\RMSE_simple_rand_24.png"),
   width = 800, height = 600)

ggplot(data = df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\simple plume\\rand_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\simple plume\\rand_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\simple plume\\rand_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\simple plume\\rand_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.df <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

png(paste0("results\\figures\\plume_RMSE_simple_rand_24.png"),
   width = 800, height = 600)

ggplot(data = plume.df, aes(x=samples, y=medRMSPE)) +
  geom_line(aes(color=Design), linewidth=2) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.2, linewidth=1, linetype = 'dotted') +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=15), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample size") +
  ylab("RMSE")

dev.off()

######## Plotting ideas for paper #################

# Needed: RMSE, plume RMSE, spatial balance, ratio of samples from plume

## 3D plot per plume with results for the 4 network types on the z axis

#### Cmplex plume

# Data

### RANDOM NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfr48 <- data.frame(
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfr48 <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfr24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\rand_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\rand_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\rand_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\rand_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfr24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

### GRID NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\grid_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\grid_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\grid_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\grid_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfg48 <- data.frame(
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("results\\complex plume\\grid_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("results\\complex plume\\grid_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("results\\complex plume\\grid_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("results\\complex plume\\grid_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfg48 <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfg24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\grid_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfg24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# adding scenario to dfs
dfg24$scenario <- rep("g24", nrow(dfg24))
dfg48$scenario <- rep("g48", nrow(dfg48))
dfr24$scenario <- rep("r24", nrow(dfr24))
dfr48$scenario <- rep("r48", nrow(dfr48))

df <- rbind(dfg24, dfg48, dfr24, dfr48)

# Plotting

# png(paste0("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Publications\\Environmental Management Paper\\figures\\plume_RMSE_complex_grid_24.png"),
#    width = 800, height = 600)

# ggplot(data = df, aes(x=samples, y=medRMSPE, z=scenario)) +
#   geom_line(aes(color=Design), linewidth=2) +
#   geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.1, linetype="dotted") +
#   ylim(min(df$lowbounds), max(df$upbounds)) +
#   theme(axis.text=element_text(size=20),
#         axis.title=element_text(size=20), 
#         legend.text = element_text(size=20), 
#         legend.key.size = unit(2, 'cm'), 
#         legend.title = element_text(size=20)) +
#   xlab("Sample size") +
#   ylab("RMSE") +
#   annotate("text", label="R24", x=250, y=8.2, size=8) +
#   annotate("text", label="R48", x=485, y=4.7, size=8) +
#   annotate("text", label="G24", x=250, y=5.6, size=8) +
#   annotate("text", label="G48", x=485, y=4.1, size=8) 

# # dev.off()

# library(plotly)

# plot_ly(x=df$samples, y=df$medRMSPE, z=df$scenario, type='scatter3d', mode='lines+markers', color = df$Design)

# plot_ly(df, x=~samples, y=~medRMSPE, z=~scenario, type="scatter3d", mode="markers", color=~Design, marker=list(size=3))


# ggplot(data = df, aes(factor(samples),medRMSPE)) +
#   geom_boxplot(aes(fill=scenario)) +
#   geom_point(aes(color=Design))


ggplot(data = df, aes(x=samples, y=medRMSPE, z=scenario)) +
  #geom_point(aes(color=scenario), size=5, alpha=0.5) +
  geom_line(aes(color=Design), linewidth=2, alpha=0.7) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.05, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=upbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=lowbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2.5, 'cm'), 
        legend.title = element_text(size=24)) +
  xlab("Sample size") +
  ylab("RMSE") +
  annotate("text", label="R24", x=250, y=8.3, size=9) +
  annotate("text", label="R48", x=485, y=4.8, size=9) +
  annotate("text", label="G24", x=250, y=5.8, size=9) +
  annotate("text", label="G48", x=484, y=4.2, size=9) 

save(df, file="results_df.RData")

# adding scenario to dfs plume
plume.dfg24$scenario <- rep("g24", nrow(plume.dfg24))
plume.dfg48$scenario <- rep("g48", nrow(plume.dfg48))
plume.dfr24$scenario <- rep("r24", nrow(plume.dfr24))
plume.dfr48$scenario <- rep("r48", nrow(plume.dfr48))

plume.df <- rbind(plume.dfg24, plume.dfg48, plume.dfr24, plume.dfr48)

save(plume.df, file="complex_plume_results_df.RData")

ggplot(data = plume.df, aes(x=samples, y=medRMSPE, z=scenario)) +
  #geom_point(aes(color=scenario), size=5, alpha=0.5) +
  geom_line(aes(color=Design), linewidth=2, alpha=0.7) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.05, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=upbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=lowbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2.5, 'cm'), 
        legend.title = element_text(size=24)) +
  xlab("Sample size") +
  ylab("RMSE") +
  annotate("text", label="R24", x=255, y=5.0, size=9) +
  annotate("text", label="R48", x=484, y=4.2, size=9) +
  annotate("text", label="G24", x=255, y=4.4, size=9) +
  annotate("text", label="G48", x=484, y=2.4, size=9)



#### MID PLUME

### GRID NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfg48 <- data.frame(
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfg48 <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfg24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\grid_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfg24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)



### RANDOM NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfr48 <- data.frame(
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfr48 <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfr24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\rand_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfr24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# adding scenario to dfs
dfg24$scenario <- rep("g24", nrow(dfg24))
dfg48$scenario <- rep("g48", nrow(dfg48))
dfr24$scenario <- rep("r24", nrow(dfr24))
dfr48$scenario <- rep("r48", nrow(dfr48))

df <- rbind(dfg24, dfg48, dfr24, dfr48)

save(df, file="mid_results_df.RData")

ggplot(data = df, aes(x=samples, y=medRMSPE, z=scenario)) +
  #geom_point(aes(color=scenario), size=5, alpha=0.5) +
  geom_line(aes(color=Design), linewidth=2, alpha=0.7) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.05, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=upbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=lowbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2.5, 'cm'), 
        legend.title = element_text(size=24),
        legend.position = 'none') +
  xlab("Sample size") +
  ylab("RMSE") +
  annotate("text", label="R24", x=250, y=8.1, size=9) +
  annotate("text", label="R48", x=485, y=5.1, size=9) +
  annotate("text", label="G24", x=250, y=5.3, size=9) +
  annotate("text", label="G48", x=485, y=4.0, size=9) 


# adding scenario to dfs plume
plume.dfg24$scenario <- rep("g24", nrow(plume.dfg24))
plume.dfg48$scenario <- rep("g48", nrow(plume.dfg48))
plume.dfr24$scenario <- rep("r24", nrow(plume.dfr24))
plume.dfr48$scenario <- rep("r48", nrow(plume.dfr48))

plume.df <- rbind(plume.dfg24, plume.dfg48, plume.dfr24, plume.dfr48)

save(plume.df, file="mid_plume_results_df.RData")

ggplot(data = plume.df, aes(x=samples, y=medRMSPE, z=scenario)) +
  #geom_point(aes(color=scenario), size=5, alpha=0.5) +
  geom_line(aes(color=Design), linewidth=2, alpha=0.7) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.05, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=upbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=lowbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2.5, 'cm'), 
        legend.title = element_text(size=24),
        legend.position = 'none') +
  xlab("Sample size") +
  ylab("RMSE") +
  annotate("text", label="R24", x=250, y=4.9, size=9) +
  annotate("text", label="R48", x=485, y=4.8, size=9) +
  annotate("text", label="G24", x=250, y=3.3, size=9) +
  annotate("text", label="G48", x=485, y=2.5, size=9)



#### SIMPLE PLUME

### GRID NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfg48 <- data.frame(
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfg48 <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfg24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\grid_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfg24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)



### RANDOM NETWORK

## 48 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfr48 <- data.frame(
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
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)


# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_48\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfr48 <- data.frame(
  samples = c(30, 60, 120, 240, 480,
              30, 60, 120, 240, 480,
              30, 60, 120, 240, 480),
  freq = c(10, 10, 10, 10, 10,
           10, 10, 10, 10, 10,
           10, 10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe),
               median(sg60_30x2$all.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe),
               up95(sg60_30x2$all.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe),
                low95(sg60_30x2$all.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

## 24 WELLS

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\30samps\\3x10\\rmspe_total.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\60samps\\6x10\\rmspe_total.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\120samps\\12x10\\rmspe_total.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\240samps\\24x10\\rmspe_total.RData")))

# plot(x=sg60_10x6$lpm.app2.rmse, y=sg60_15x4$lpm.app2.rmse)

# RMSE
# Creating df for plotting
dfr24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.rmspe), median(sg60_10x6$rand.app2.rmspe), median(sg60_15x4$rand.app2.rmspe), median(sg60_30x2$rand.app2.rmspe),
               median(sg60_6x10$lpm.app2.rmspe), median(sg60_10x6$lpm.app2.rmspe), median(sg60_15x4$lpm.app2.rmspe), median(sg60_30x2$lpm.app2.rmspe),
               median(sg60_6x10$lpm.up.rmspe), median(sg60_10x6$lpm.up.rmspe), median(sg60_15x4$lpm.up.rmspe), median(sg60_30x2$lpm.up.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.rmspe), up95(sg60_10x6$rand.app2.rmspe), up95(sg60_15x4$rand.app2.rmspe), up95(sg60_30x2$rand.app2.rmspe),
               up95(sg60_6x10$lpm.app2.rmspe), up95(sg60_10x6$lpm.app2.rmspe), up95(sg60_15x4$lpm.app2.rmspe), up95(sg60_30x2$lpm.app2.rmspe),
               up95(sg60_6x10$lpm.up.rmspe), up95(sg60_10x6$lpm.up.rmspe), up95(sg60_15x4$lpm.up.rmspe), up95(sg60_30x2$lpm.up.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.rmspe), low95(sg60_10x6$rand.app2.rmspe), low95(sg60_15x4$rand.app2.rmspe), low95(sg60_30x2$rand.app2.rmspe),
                low95(sg60_6x10$lpm.app2.rmspe), low95(sg60_10x6$lpm.app2.rmspe), low95(sg60_15x4$lpm.app2.rmspe), low95(sg60_30x2$lpm.app2.rmspe),
                low95(sg60_6x10$lpm.up.rmspe), low95(sg60_10x6$lpm.up.rmspe), low95(sg60_15x4$lpm.up.rmspe), low95(sg60_30x2$lpm.up.rmspe)),  
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# Plume RMSE

# Loading data
assign('sg60_6x10', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\30samps\\3x10\\rmspe_plume.RData")))
assign('sg60_10x6', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\60samps\\6x10\\rmspe_plume.RData")))
assign('sg60_15x4', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\120samps\\12x10\\rmspe_plume.RData")))
assign('sg60_30x2', get(load("C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\rand_24\\240samps\\24x10\\rmspe_plume.RData")))

# Creating df for plotting
plume.dfr24 <- data.frame(
  samples = c(30, 60, 120, 240,
              30, 60, 120, 240,
              30, 60, 120, 240),
  freq = c(10, 10, 10, 10,
           10, 10, 10, 10,
           10, 10, 10, 10),
  # Plume RMSPE
  medRMSPE = c(median(sg60_6x10$rand.app2.plume.rmspe), median(sg60_10x6$rand.app2.plume.rmspe), median(sg60_15x4$rand.app2.plume.rmspe), median(sg60_30x2$rand.app2.plume.rmspe),
               median(sg60_6x10$lpm.app2.plume.rmspe), median(sg60_10x6$lpm.app2.plume.rmspe), median(sg60_15x4$lpm.app2.plume.rmspe), median(sg60_30x2$lpm.app2.plume.rmspe),
               median(sg60_6x10$lpm.up.plume.rmspe), median(sg60_10x6$lpm.up.plume.rmspe), median(sg60_15x4$lpm.up.plume.rmspe), median(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE upper bounds
  upbounds = c(up95(sg60_6x10$rand.app2.plume.rmspe), up95(sg60_10x6$rand.app2.plume.rmspe), up95(sg60_15x4$rand.app2.plume.rmspe), up95(sg60_30x2$rand.app2.plume.rmspe),
               up95(sg60_6x10$lpm.app2.plume.rmspe), up95(sg60_10x6$lpm.app2.plume.rmspe), up95(sg60_15x4$lpm.app2.plume.rmspe), up95(sg60_30x2$lpm.app2.plume.rmspe),
               up95(sg60_6x10$lpm.up.plume.rmspe), up95(sg60_10x6$lpm.up.plume.rmspe), up95(sg60_15x4$lpm.up.plume.rmspe), up95(sg60_30x2$lpm.up.plume.rmspe)),
  # RMSPE lower bounds
  lowbounds = c(low95(sg60_6x10$rand.app2.plume.rmspe), low95(sg60_10x6$rand.app2.plume.rmspe), low95(sg60_15x4$rand.app2.plume.rmspe), low95(sg60_30x2$rand.app2.plume.rmspe),
                low95(sg60_6x10$lpm.app2.plume.rmspe), low95(sg60_10x6$lpm.app2.plume.rmspe), low95(sg60_15x4$lpm.app2.plume.rmspe), low95(sg60_30x2$lpm.app2.plume.rmspe),
                low95(sg60_6x10$lpm.up.plume.rmspe), low95(sg60_10x6$lpm.up.plume.rmspe), low95(sg60_15x4$lpm.up.plume.rmspe), low95(sg60_30x2$lpm.up.plume.rmspe)),
  Design = c("SRS", "SRS", "SRS", "SRS",
             "eLPM", "eLPM", 'eLPM', 'eLPM',
             'pLPM', 'pLPM', 'pLPM', 'pLPM')
)

# adding scenario to dfs
dfg24$scenario <- rep("g24", nrow(dfg24))
dfg48$scenario <- rep("g48", nrow(dfg48))
dfr24$scenario <- rep("r24", nrow(dfr24))
dfr48$scenario <- rep("r48", nrow(dfr48))

df <- rbind(dfg24, dfg48, dfr24, dfr48)

save(df, file="simple_results_df.RData")

ggplot(data = df, aes(x=samples, y=medRMSPE, z=scenario)) +
  #geom_point(aes(color=scenario), size=5, alpha=0.5) +
  geom_line(aes(color=Design), linewidth=2, alpha=0.7) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.05, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=upbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=lowbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  ylim(min(df$lowbounds), max(df$upbounds)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2.5, 'cm'), 
        legend.title = element_text(size=24),
        legend.position = 'none') +
  xlab("Sample size") +
  ylab("RMSE") +
  annotate("text", label="R24", x=250, y=7.4, size=9) +
  annotate("text", label="R48", x=485, y=5.6, size=9) +
  annotate("text", label="G24", x=250, y=4.8, size=9) +
  annotate("text", label="G48", x=485, y=4.3, size=9) 


# adding scenario to dfs plume
plume.dfg24$scenario <- rep("g24", nrow(plume.dfg24))
plume.dfg48$scenario <- rep("g48", nrow(plume.dfg48))
plume.dfr24$scenario <- rep("r24", nrow(plume.dfr24))
plume.dfr48$scenario <- rep("r48", nrow(plume.dfr48))

plume.df <- rbind(plume.dfg24, plume.dfg48, plume.dfr24, plume.dfr48)

save(plume.df, file="simple_plume_results_df.RData")

ggplot(data = plume.df, aes(x=samples, y=medRMSPE, z=scenario)) +
  #geom_point(aes(color=scenario), size=5, alpha=0.5) +
  geom_line(aes(color=Design), linewidth=2, alpha=0.7) +
  geom_ribbon(aes(x=samples, ymax=upbounds, ymin=lowbounds, color = Design, fill=Design), alpha=0.05, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=upbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  # geom_line(aes(x=samples, y=lowbounds, color = Design), alpha=0.1, linetype="dotted", show.legend = FALSE) +
  ylim(min(plume.df$lowbounds), max(plume.df$upbounds)) +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2.5, 'cm'), 
        legend.title = element_text(size=24),
        legend.position = 'none') +
  xlab("Sample size") +
  ylab("RMSE") +
  annotate("text", label="R24", x=255, y=3.3, size=9) +
  annotate("text", label="R48", x=485, y=3.9, size=9) +
  annotate("text", label="G24", x=255, y=2.8, size=9) +
  annotate("text", label="G48", x=485, y=2.0, size=9)


################ SB  ###############

### SIMPLE PLUME

# Loading data

path <- "C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\"

# SR24

assign('sr2430', get(load(paste0(path,"rand_24\\30samps\\3x10\\sbs.RData"))))
assign('sr2460', get(load(paste0(path,"rand_24\\60samps\\6x10\\sbs.RData"))))
assign('sr24120', get(load(paste0(path,"rand_24\\120samps\\12x10\\sbs.RData"))))
assign('sr24240', get(load(paste0(path,"rand_24\\240samps\\24x10\\sbs.RData"))))

sbr24 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(sr2430$rand.app2.sbs, sr2430$lpm.app2.sbs, sr2430$lpm.up.sbs, 
              sr2460$rand.app2.sbs, sr2460$lpm.app2.sbs, sr2460$lpm.up.sbs,
              sr24120$rand.app2.sbs, sr24120$lpm.app2.sbs, sr24120$lpm.up.sbs,
              sr24240$rand.app2.sbs, sr24240$lpm.app2.sbs, sr24240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=sbr24, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# SG24

assign('sg2430', get(load(paste0(path,"grid_24\\30samps\\3x10\\sbs.RData"))))
assign('sg2460', get(load(paste0(path,"grid_24\\60samps\\6x10\\sbs.RData"))))
assign('sg24120', get(load(paste0(path,"grid_24\\120samps\\12x10\\sbs.RData"))))
assign('sg24240', get(load(paste0(path,"grid_24\\240samps\\24x10\\sbs.RData"))))

sbg24 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(sg2430$rand.app2.sbs, sg2430$lpm.app2.sbs, sg2430$lpm.up.sbs, 
              sg2460$rand.app2.sbs, sg2460$lpm.app2.sbs, sg2460$lpm.up.sbs,
              sg24120$rand.app2.sbs, sg24120$lpm.app2.sbs, sg24120$lpm.up.sbs,
              sg24240$rand.app2.sbs, sg24240$lpm.app2.sbs, sg24240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=sbg24, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# SR48

assign('sr4830', get(load(paste0(path,"rand_48\\30samps\\3x10\\sbs.RData"))))
assign('sr4860', get(load(paste0(path,"rand_48\\60samps\\6x10\\sbs.RData"))))
assign('sr48120', get(load(paste0(path,"rand_48\\120samps\\12x10\\sbs.RData"))))
assign('sr48240', get(load(paste0(path,"rand_48\\240samps\\24x10\\sbs.RData"))))

sbr48 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(sr4830$rand.app2.sbs, sr4830$lpm.app2.sbs, sr4830$lpm.up.sbs, 
              sr4860$rand.app2.sbs, sr4860$lpm.app2.sbs, sr4860$lpm.up.sbs,
              sr48120$rand.app2.sbs, sr48120$lpm.app2.sbs, sr48120$lpm.up.sbs,
              sr48240$rand.app2.sbs, sr48240$lpm.app2.sbs, sr48240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=sbr48, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# SG48

assign('sg4830', get(load(paste0(path,"grid_48\\30samps\\3x10\\sbs.RData"))))
assign('sg4860', get(load(paste0(path,"grid_48\\60samps\\6x10\\sbs.RData"))))
assign('sg48120', get(load(paste0(path,"grid_48\\120samps\\12x10\\sbs.RData"))))
assign('sg48240', get(load(paste0(path,"grid_48\\240samps\\24x10\\sbs.RData"))))

sbg48 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(sg4830$rand.app2.sbs, sg4830$lpm.app2.sbs, sg4830$lpm.up.sbs, 
              sg4860$rand.app2.sbs, sg4860$lpm.app2.sbs, sg4860$lpm.up.sbs,
              sg48120$rand.app2.sbs, sg48120$lpm.app2.sbs, sg48120$lpm.up.sbs,
              sg48240$rand.app2.sbs, sg48240$lpm.app2.sbs, sg48240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=sbg48, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")



### MID PLUME

# Loading data

path <- "C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\"

# R24

assign('mr2430', get(load(paste0(path,"rand_24\\30samps\\3x10\\sbs.RData"))))
assign('mr2460', get(load(paste0(path,"rand_24\\60samps\\6x10\\sbs.RData"))))
assign('mr24120', get(load(paste0(path,"rand_24\\120samps\\12x10\\sbs.RData"))))
assign('mr24240', get(load(paste0(path,"rand_24\\240samps\\24x10\\sbs.RData"))))

mbr24 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(mr2430$rand.app2.sbs, mr2430$lpm.app2.sbs, mr2430$lpm.up.sbs, 
              mr2460$rand.app2.sbs, mr2460$lpm.app2.sbs, mr2460$lpm.up.sbs,
              mr24120$rand.app2.sbs, mr24120$lpm.app2.sbs, mr24120$lpm.up.sbs,
              mr24240$rand.app2.sbs, mr24240$lpm.app2.sbs, mr24240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=mbr24, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# G24

assign('mg2430', get(load(paste0(path,"grid_24\\30samps\\3x10\\sbs.RData"))))
assign('mg2460', get(load(paste0(path,"grid_24\\60samps\\6x10\\sbs.RData"))))
assign('mg24120', get(load(paste0(path,"grid_24\\120samps\\12x10\\sbs.RData"))))
assign('mg24240', get(load(paste0(path,"grid_24\\240samps\\24x10\\sbs.RData"))))

mbg24 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(mg2430$rand.app2.sbs, mg2430$lpm.app2.sbs, mg2430$lpm.up.sbs, 
              mg2460$rand.app2.sbs, mg2460$lpm.app2.sbs, mg2460$lpm.up.sbs,
              mg24120$rand.app2.sbs, mg24120$lpm.app2.sbs, mg24120$lpm.up.sbs,
              mg24240$rand.app2.sbs, mg24240$lpm.app2.sbs, mg24240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=mbg24, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# R48

assign('mr4830', get(load(paste0(path,"rand_48\\30samps\\3x10\\sbs.RData"))))
assign('mr4860', get(load(paste0(path,"rand_48\\60samps\\6x10\\sbs.RData"))))
assign('mr48120', get(load(paste0(path,"rand_48\\120samps\\12x10\\sbs.RData"))))
assign('mr48240', get(load(paste0(path,"rand_48\\240samps\\24x10\\sbs.RData"))))

mbr48 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(mr4830$rand.app2.sbs, mr4830$lpm.app2.sbs, mr4830$lpm.up.sbs, 
              mr4860$rand.app2.sbs, mr4860$lpm.app2.sbs, mr4860$lpm.up.sbs,
              mr48120$rand.app2.sbs, mr48120$lpm.app2.sbs, mr48120$lpm.up.sbs,
              mr48240$rand.app2.sbs, mr48240$lpm.app2.sbs, mr48240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=mbr48, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# G48

assign('mg4830', get(load(paste0(path,"grid_48\\30samps\\3x10\\sbs.RData"))))
assign('mg4860', get(load(paste0(path,"grid_48\\60samps\\6x10\\sbs.RData"))))
assign('mg48120', get(load(paste0(path,"grid_48\\120samps\\12x10\\sbs.RData"))))
assign('mg48240', get(load(paste0(path,"grid_48\\240samps\\24x10\\sbs.RData"))))

mbg48 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(mg4830$rand.app2.sbs, mg4830$lpm.app2.sbs, mg4830$lpm.up.sbs, 
              mg4860$rand.app2.sbs, mg4860$lpm.app2.sbs, mg4860$lpm.up.sbs,
              mg48120$rand.app2.sbs, mg48120$lpm.app2.sbs, mg48120$lpm.up.sbs,
              mg48240$rand.app2.sbs, mg48240$lpm.app2.sbs, mg48240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=mbg48, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")


### COMPLEX PLUME

# Loading data

path <- "C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\"

# R24

assign('cr2430', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\30samps\\3x10\\sbs.RData"))))
assign('cr2460', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\60samps\\6x10\\sbs.RData"))))
assign('cr24120', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\120samps\\12x10\\sbs.RData"))))
assign('cr24240', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\240samps\\24x10\\sbs.RData"))))

cbr24 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(cr2430$rand.app2.sbs, cr2430$lpm.app2.sbs, cr2430$lpm.up.sbs, 
              cr2460$rand.app2.sbs, cr2460$lpm.app2.sbs, cr2460$lpm.up.sbs,
              cr24120$rand.app2.sbs, cr24120$lpm.app2.sbs, cr24120$lpm.up.sbs,
              cr24240$rand.app2.sbs, cr24240$lpm.app2.sbs, cr24240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=cbr24, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# G24

assign('cg2430', get(load(paste0(path,"grid_24\\30samps\\3x10\\sbs.RData"))))
assign('cg2460', get(load(paste0(path,"grid_24\\60samps\\6x10\\sbs.RData"))))
assign('cg24120', get(load(paste0(path,"grid_24\\120samps\\12x10\\sbs.RData"))))
assign('cg24240', get(load(paste0(path,"grid_24\\240samps\\24x10\\sbs.RData"))))

cbg24 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(cg2430$rand.app2.sbs, cg2430$lpm.app2.sbs, cg2430$lpm.up.sbs, 
              cg2460$rand.app2.sbs, cg2460$lpm.app2.sbs, cg2460$lpm.up.sbs,
              cg24120$rand.app2.sbs, cg24120$lpm.app2.sbs, cg24120$lpm.up.sbs,
              cg24240$rand.app2.sbs, cg24240$lpm.app2.sbs, cg24240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=cbg24, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size=24),
        legend.position = "none") +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# R48

assign('cr4830', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\30samps\\3x10\\sbs.RData"))))
assign('cr4860', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\60samps\\6x10\\sbs.RData"))))
assign('cr48120', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\120samps\\12x10\\sbs.RData"))))
assign('cr48240', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\240samps\\24x10\\sbs.RData"))))

cbr48 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(cr4830$rand.app2.sbs, cr4830$lpm.app2.sbs, cr4830$lpm.up.sbs, 
              cr4860$rand.app2.sbs, cr4860$lpm.app2.sbs, cr4860$lpm.up.sbs,
              cr48120$rand.app2.sbs, cr48120$lpm.app2.sbs, cr48120$lpm.up.sbs,
              cr48240$rand.app2.sbs, cr48240$lpm.app2.sbs, cr48240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=cbr48, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")

# G48

assign('cg4830', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\30samps\\3x10\\sbs.RData"))))
assign('cg4860', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\60samps\\6x10\\sbs.RData"))))
assign('cg48120', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\120samps\\12x10\\sbs.RData"))))
assign('cg48240', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\240samps\\24x10\\sbs.RData"))))

cbg48 <- data.frame(
  samples = c(rep(30,3000), 
              rep(60, 3000), 
              rep(120, 3000), 
              rep(240, 3000)),
  sbs = c(cg4830$rand.app2.sbs, cg4830$lpm.app2.sbs, cg4830$lpm.up.sbs, 
              cg4860$rand.app2.sbs, cg4860$lpm.app2.sbs, cg4860$lpm.up.sbs,
              cg48120$rand.app2.sbs, cg48120$lpm.app2.sbs, cg48120$lpm.up.sbs,
              cg48240$rand.app2.sbs, cg48240$lpm.app2.sbs, cg48240$lpm.up.sbs),
  design = c(rep('SRS',1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000),
            rep('SRS', 1000), rep('eLPM', 1000), rep('pLPM', 1000))
)

ggplot(data=cbg48, aes(x=factor(samples), y=sbs, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size=24)) +
  xlab("Sample Size") +
  ylab("Relative Spatial Balance")


############## RATIOS #################

### SIMPLE PLUME

# Loading data

path <- "C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\simple plume\\"

# SR24

assign('sr2430', get(load(paste0(path,"rand_24\\30samps\\3x10\\in_ratios.RData"))))
assign('sr2460', get(load(paste0(path,"rand_24\\60samps\\6x10\\in_ratios.RData"))))
assign('sr24120', get(load(paste0(path,"rand_24\\120samps\\12x10\\in_ratios.RData"))))
assign('sr24240', get(load(paste0(path,"rand_24\\240samps\\24x10\\in_ratios.RData"))))

sbr24 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(sr2430$rand.ratio, sr2430$ep.ratio, sr2430$up.ratio, 
              sr2460$rand.ratio, sr2460$ep.ratio, sr2460$up.ratio,
              sr24120$rand.ratio, sr24120$ep.ratio, sr24120$up.ratio,
              sr24240$rand.ratio, sr24240$ep.ratio, sr24240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=sbr24, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# SG24

assign('sg2430', get(load(paste0(path,"grid_24\\30samps\\3x10\\in_ratios.RData"))))
assign('sg2460', get(load(paste0(path,"grid_24\\60samps\\6x10\\in_ratios.RData"))))
assign('sg24120', get(load(paste0(path,"grid_24\\120samps\\12x10\\in_ratios.RData"))))
assign('sg24240', get(load(paste0(path,"grid_24\\240samps\\24x10\\in_ratios.RData"))))

sbg24 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(sg2430$rand.ratio, sg2430$ep.ratio, sg2430$up.ratio, 
              sg2460$rand.ratio, sg2460$ep.ratio, sg2460$up.ratio,
              sg24120$rand.ratio, sg24120$ep.ratio, sg24120$up.ratio,
              sg24240$rand.ratio, sg24240$ep.ratio, sg24240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=sbg24, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# SR48

assign('sr4830', get(load(paste0(path,"rand_48\\30samps\\3x10\\in_ratios.RData"))))
assign('sr4860', get(load(paste0(path,"rand_48\\60samps\\6x10\\in_ratios.RData"))))
assign('sr48120', get(load(paste0(path,"rand_48\\120samps\\12x10\\in_ratios.RData"))))
assign('sr48240', get(load(paste0(path,"rand_48\\240samps\\24x10\\in_ratios.RData"))))

sbr48 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(sr4830$rand.ratio, sr4830$ep.ratio, sr4830$up.ratio, 
              sr4860$rand.ratio, sr4860$ep.ratio, sr4860$up.ratio,
              sr48120$rand.ratio, sr48120$ep.ratio, sr48120$up.ratio,
              sr48240$rand.ratio, sr48240$ep.ratio, sr48240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=sbr48, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# SG48

assign('sg4830', get(load(paste0(path,"grid_48\\30samps\\3x10\\in_ratios.RData"))))
assign('sg4860', get(load(paste0(path,"grid_48\\60samps\\6x10\\in_ratios.RData"))))
assign('sg48120', get(load(paste0(path,"grid_48\\120samps\\12x10\\in_ratios.RData"))))
assign('sg48240', get(load(paste0(path,"grid_48\\240samps\\24x10\\in_ratios.RData"))))

sbg48 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(sg4830$rand.ratio, sg4830$ep.ratio, sg4830$up.ratio, 
              sg4860$rand.ratio, sg4860$ep.ratio, sg4860$up.ratio,
              sg48120$rand.ratio, sg48120$ep.ratio, sg48120$up.ratio,
              sg48240$rand.ratio, sg48240$ep.ratio, sg48240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=sbg48, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")



### MID PLUME

# Loading data

path <- "C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\mid plume\\"

# R24

assign('mr2430', get(load(paste0(path,"rand_24\\30samps\\3x10\\in_ratios.RData"))))
assign('mr2460', get(load(paste0(path,"rand_24\\60samps\\6x10\\in_ratios.RData"))))
assign('mr24120', get(load(paste0(path,"rand_24\\120samps\\12x10\\in_ratios.RData"))))
assign('mr24240', get(load(paste0(path,"rand_24\\240samps\\24x10\\in_ratios.RData"))))

mbr24 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(mr2430$rand.ratio, mr2430$ep.ratio, mr2430$up.ratio, 
              mr2460$rand.ratio, mr2460$ep.ratio, mr2460$up.ratio,
              mr24120$rand.ratio, mr24120$ep.ratio, mr24120$up.ratio,
              mr24240$rand.ratio, mr24240$ep.ratio, mr24240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=mbr24, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# G24

assign('mg2430', get(load(paste0(path,"grid_24\\30samps\\3x10\\in_ratios.RData"))))
assign('mg2460', get(load(paste0(path,"grid_24\\60samps\\6x10\\in_ratios.RData"))))
assign('mg24120', get(load(paste0(path,"grid_24\\120samps\\12x10\\in_ratios.RData"))))
assign('mg24240', get(load(paste0(path,"grid_24\\240samps\\24x10\\in_ratios.RData"))))

mbg24 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(mg2430$rand.ratio, mg2430$ep.ratio, mg2430$up.ratio, 
              mg2460$rand.ratio, mg2460$ep.ratio, mg2460$up.ratio,
              mg24120$rand.ratio, mg24120$ep.ratio, mg24120$up.ratio,
              mg24240$rand.ratio, mg24240$ep.ratio, mg24240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=mbg24, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# R48

assign('mr4830', get(load(paste0(path,"rand_48\\30samps\\3x10\\in_ratios.RData"))))
assign('mr4860', get(load(paste0(path,"rand_48\\60samps\\6x10\\in_ratios.RData"))))
assign('mr48120', get(load(paste0(path,"rand_48\\120samps\\12x10\\in_ratios.RData"))))
assign('mr48240', get(load(paste0(path,"rand_48\\240samps\\24x10\\in_ratios.RData"))))

mbr48 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(mr4830$rand.ratio, mr4830$ep.ratio, mr4830$up.ratio, 
              mr4860$rand.ratio, mr4860$ep.ratio, mr4860$up.ratio,
              mr48120$rand.ratio, mr48120$ep.ratio, mr48120$up.ratio,
              mr48240$rand.ratio, mr48240$ep.ratio, mr48240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=mbr48, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# G48

assign('mg4830', get(load(paste0(path,"grid_48\\30samps\\3x10\\in_ratios.RData"))))
assign('mg4860', get(load(paste0(path,"grid_48\\60samps\\6x10\\in_ratios.RData"))))
assign('mg48120', get(load(paste0(path,"grid_48\\120samps\\12x10\\in_ratios.RData"))))
assign('mg48240', get(load(paste0(path,"grid_48\\240samps\\24x10\\in_ratios.RData"))))

mbg48 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(mg4830$rand.ratio, mg4830$ep.ratio, mg4830$up.ratio, 
              mg4860$rand.ratio, mg4860$ep.ratio, mg4860$up.ratio,
              mg48120$rand.ratio, mg48120$ep.ratio, mg48120$up.ratio,
              mg48240$rand.ratio, mg48240$ep.ratio, mg48240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=mbg48, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")


### COMPLEX PLUME

# Loading data

path <- "C:\\Users\\peter\\OneDrive\\Dokumentumok\\PhD - Uni Glasgow\\PhD\\Balanced Sampling for Groundwater Monitoring\\Simulation Study\\sim_tests\\complex plume\\"

# R24

assign('cr2430', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\30samps\\3x10\\in_ratios.RData"))))
assign('cr2460', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\60samps\\6x10\\in_ratios.RData"))))
assign('cr24120', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\120samps\\12x10\\in_ratios.RData"))))
assign('cr24240', get(load(paste0(path,"rand_24\\noise_15\\freq_test\\240samps\\24x10\\in_ratios.RData"))))

cbr24 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(cr2430$rand.ratio, cr2430$ep.ratio, cr2430$up.ratio, 
              cr2460$rand.ratio, cr2460$ep.ratio, cr2460$up.ratio,
              cr24120$rand.ratio, cr24120$ep.ratio, cr24120$up.ratio,
              cr24240$rand.ratio, cr24240$ep.ratio, cr24240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=cbr24, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# G24

assign('cg2430', get(load(paste0(path,"grid_24\\30samps\\3x10\\in_ratios.RData"))))
assign('cg2460', get(load(paste0(path,"grid_24\\60samps\\6x10\\in_ratios.RData"))))
assign('cg24120', get(load(paste0(path,"grid_24\\120samps\\12x10\\in_ratios.RData"))))
assign('cg24240', get(load(paste0(path,"grid_24\\240samps\\24x10\\in_ratios.RData"))))

cbg24 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(cg2430$rand.ratio, cg2430$ep.ratio, cg2430$up.ratio, 
              cg2460$rand.ratio, cg2460$ep.ratio, cg2460$up.ratio,
              cg24120$rand.ratio, cg24120$ep.ratio, cg24120$up.ratio,
              cg24240$rand.ratio, cg24240$ep.ratio, cg24240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=cbg24, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size=24),
        legend.position = "none") +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# R48

assign('cr4830', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\30samps\\3x10\\in_ratios.RData"))))
assign('cr4860', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\60samps\\6x10\\in_ratios.RData"))))
assign('cr48120', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\120samps\\12x10\\in_ratios.RData"))))
assign('cr48240', get(load(paste0(path,"rand_48_3\\noise_15\\nseg_6\\240samps\\24x10\\in_ratios.RData"))))

cbr48 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(cr4830$rand.ratio, cr4830$ep.ratio, cr4830$up.ratio, 
              cr4860$rand.ratio, cr4860$ep.ratio, cr4860$up.ratio,
              cr48120$rand.ratio, cr48120$ep.ratio, cr48120$up.ratio,
              cr48240$rand.ratio, cr48240$ep.ratio, cr48240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=cbr48, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16), 
        legend.text = element_text(size=16), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=16)) +
  xlab("Sample Size") +
  ylab("Plume Samples / Sample Size")

# G48

assign('cg4830', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\30samps\\3x10\\in_ratios.RData"))))
assign('cg4860', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\60samps\\6x10\\in_ratios.RData"))))
assign('cg48120', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\120samps\\12x10\\in_ratios.RData"))))
assign('cg48240', get(load(paste0(path,"grid_48\\noise_0\\freq_test\\240samps\\24x10\\in_ratios.RData"))))

cbg48 <- data.frame(
  samples = c(rep(30,300), 
              rep(60, 300), 
              rep(120, 300), 
              rep(240, 300)),
  ratios = c(cg4830$rand.ratio, cg4830$ep.ratio, cg4830$up.ratio, 
              cg4860$rand.ratio, cg4860$ep.ratio, cg4860$up.ratio,
              cg48120$rand.ratio, cg48120$ep.ratio, cg48120$up.ratio,
              cg48240$rand.ratio, cg48240$ep.ratio, cg48240$up.ratio),
  design = c(rep('SRS',100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100),
            rep('SRS', 100), rep('eLPM', 100), rep('pLPM', 100))
)

ggplot(data=cbg48, aes(x=factor(samples), y=ratios, fill=design)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=24), 
        legend.text = element_text(size=24), 
        legend.key.size = unit(2, 'cm'), 
        legend.title = element_text(size=24)) +
  xlab("Sample Size") +

  ylab("RPlume Samples / Sample Size")
