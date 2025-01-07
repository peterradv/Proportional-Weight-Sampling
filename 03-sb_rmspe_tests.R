
# rand.means <- double()
# for (i in 1:100) {
#   rand.mean <- mean(sbs$rand.app2.sbs[i:i+1])
#   
#   rand.means <- rbind(rand.means, rand.mean)
# }
# 
# 
# balanced.means <- double()
# for (i in 1:100) {
#   balanced.mean <- mean(sbs$lpm.app2.sbs[i:i+1])
#   
#   balanced.means <- rbind(balanced.means, balanced.mean)
# }
# 
sbxrmspe <- as.data.frame(cbind(sbs$rand.app2.sbs, results$rand.app2.rmspe, results.plume$rand.app2.plume.rmspe, results.out$rand.app2.out.rmspe, sbs$lpm.app2.sbs, results$lpm.app2.rmspe, results.plume$lpm.app2.plume.rmspe, results.out$lpm.app2.out.rmspe))

# for constant samp locs
# sbxrmspe <- as.data.frame(cbind(sbs$rand.app2.sb, results$rand.app2.rmspe, results.plume$rand.app2.plume.rmspe, results.out$rand.app2.out.rmspe, sbs$lpm.app2.sb, results$lpm.app2.rmspe, results.plume$lpm.app2.plume.rmspe, results.out$lpm.app2.out.rmspe))


# # rand total
# 
# png(paste0(path, "rand_total.png"), width = 620, height = 400)
# 
# ggplot(data = sbxrmspe, aes(x=V1, y=V2)) +
#   geom_point() +
#   ylab("RMSPE") +
#   xlab("spatial balance") +
#   ggtitle("random sampling rmspe against spatial balance")
# 
# dev.off()
# 
# # rand plume
# 
# png(paste0(path, "rand_plume.png"), width = 620, height = 400)
# 
# ggplot(data = sbxrmspe, aes(x=V1, y=V3)) +
#   geom_point() +
#   ylab("plume RMSPE") +
#   xlab("spatial balance") +
#   ggtitle("random sampling plume rmspe against spatial balance")
# 
# dev.off()
# 
# # rand out of plume
# 
# png(paste0(path, "rand_out.png"), width = 620, height = 400)
# 
# ggplot(data = sbxrmspe, aes(x=V1, y=V4)) +
#   geom_point() +
#   ylab("out of plume RMSPE") +
#   xlab("mean spatial balance of 2 campaigns") +
#   ggtitle("random sampling out of plume rmspe against spatial balance")
# 
# dev.off()
# 
# # balanced total
# 
# png(paste0(path, "balanced_total.png"), width = 620, height = 400)
# 
# ggplot(data = sbxrmspe, aes(x=V5, y=V6)) +
#   geom_point() +
#   ylab("RMSPE") +
#   xlab("spatial balance") +
#   ggtitle("balanced sampling rmspe against spatial balance")
# 
# dev.off()
# 
# # balanced plume
# 
# png(paste0(path, "balanced_plume.png"), width = 620, height = 400)
# 
# ggplot(data = sbxrmspe, aes(x=V5, y=V7)) +
#   geom_point() +
#   ylab("plume RMSPE") +
#   xlab("spatial balance") +
#   ggtitle("balanced sampling plume rmspe against spatial balance")
# 
# dev.off()
# 
# # balanced out of plume
# 
# png(paste0(path, "balanced_out.png"), width = 620, height = 400)
# 
# ggplot(data = sbxrmspe, aes(x=V5, y=V8)) +
#   geom_point() +
#   ylab("out of plume RMSPE") +
#   xlab("mean spatial balance of 2 campaigns") +
#   ggtitle("balanced sampling out of plume rmspe against spatial balance")
# 
# dev.off()

# trying some other plots
group_by(sbxrmspe, V1) %>%
  arrange(desc(V1))

sbxrmspe2 <- data.frame(SB = c(sbs$rand.app2.sb, sbs$lpm.app2.sb),
                        RMSPE = c(results$rand.app2.rmspe, results$lpm.app2.rmspe),
                        pRMSPE = c(results.plume$rand.app2.plume.rmspe, results.plume$lpm.app2.plume.rmspe),
                        type = c(rep("srs", 200), rep("lpm", 200)))

png(paste0(path, "sbxrmspe.png"), width = 620, height = 400)

ggplot(data = sbxrmspe2, aes(x=SB, y=RMSPE, shape=type, color=type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("RMSPE") +
  xlab("spatial balance") +
  ggtitle("Relationship between spatial balance and RMSPE")

dev.off()
  
# ggplot(data = sbxrmspe2, aes(x=SB, y=pRMSPE, shape=type, color=type)) +
#   geom_point() +
#   #geom_smooth(method = "lm") +
#   ylab("RMSPE") +
#   xlab("spatial balance") +
#   ggtitle("Relationship between spatial balance and plume RMSPE")

## finding worst/best samples

# simple random sample with the worst spatial balance 
which.max(sbxrmspe$V1)
# value of that spatial balance
sbxrmspe$V1[which.max(sbxrmspe$V1)]
# corresponding rmspe
sbxrmspe$V2[which.max(sbxrmspe$V1)]
# corresponding plume rmspe
sbxrmspe$V3[which.max(sbxrmspe$V1)]

worst.rand.sample <- rand.app2.samples[[which.max(sbxrmspe$V1)]]

# plot of random samples
# slice <- 2
# samp_coords <-worst.rand.sample %>%
#   filter(Time == ts2) %>%
#   select(X1, X2)
# ggplot(data = true.data2 %>% filter(Time == ts2), mapping = aes(x = X1, y = X2)) +
#   geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
#   geom_point(data = well_coords) +
#   geom_point(data = samp_coords, color = "green", size=4) +
#   geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
#   scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
#   ylim(0,35) +
#   xlim(0, 100)
# gif of random samples
for (h in 1:length(ts2)) {
  
  samp_coords <- rand.app2.samples[[which.max(sbxrmspe$V1)]] %>%
    filter(Time == ts2[h]) %>%
    select(X1, X2)
  
  assign(
    paste0("rand.ts", h),
    ggplot(data = true.data2 %>% filter(Time == ts2[h]), mapping = aes(x = X1, y = X2)) +
      geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
      geom_point(data = well_coords) +
      geom_point(data = samp_coords, color = "green", size=4) +
      geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
      ylim(0,35) +
      xlim(0, 100)
  )
}

saveGIF(expr = {
  print(rand.ts1)
  print(rand.ts2)
  print(rand.ts3)
  print(rand.ts4)
  print(rand.ts5)
}, interval = 1, ani.height=600, ani.width=800, 
movie.name = paste0(path, "rand_worst.gif"))




# balanced LPM sample with the best spatial balance
which.min(sbxrmspe$V5)
# value of that spatial balance
sbxrmspe$V5[which.min(sbxrmspe$V5)]
# corresponding rmspe
sbxrmspe$V6[which.min(sbxrmspe$V5)]
# corresponding plume rmspe
sbxrmspe$V7[which.min(sbxrmspe$V5)]

best.lpm.sample <- lpm.app2.samples[[which.min(sbxrmspe$V5)]]

# plot of balanced LPM samples
# slice <- 1
# samp_coords <- best.lpm.sample %>%
#   filter(Time == ts2) %>%
#   select(X1, X2)
# ggplot(data = true.data2 %>% filter(Time == ts2), mapping = aes(x = X1, y = X2)) +
#   geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
#   geom_point(data = well_coords) +
#   geom_point(data = samp_coords, color = "green", size=4) +
#   geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
#   scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
#   ylim(0,35) +
#   xlim(0, 100)
# gif of lpm samples
for (h in 1:length(ts2)) {
  
  samp_coords <- lpm.app2.samples[[which.min(sbxrmspe$V5)]] %>%
    filter(Time == ts2[h]) %>%
    select(X1, X2)
  
  assign(
    paste0("lpm.ts", h),
    ggplot(data = true.data2 %>% filter(Time == ts2[h]), mapping = aes(x = X1, y = X2)) +
      geom_point(mapping = aes(color = log(y)), size=3.5, alpha=1, show.legend = FALSE) +
      geom_point(data = well_coords) +
      geom_point(data = samp_coords, color = "green", size=4) +
      geom_text(data = well_coords, label = well_coords$well.id, vjust = -.5, hjust = -.1) +
      scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlBu"))) +
      ylim(0,35) +
      xlim(0, 100)
  )
}

saveGIF(expr = {
  print(lpm.ts1)
  print(lpm.ts2)
  print(lpm.ts3)
  print(lpm.ts4)
  print(lpm.ts5)
}, interval = 1, ani.height=600, ani.width=800, 
movie.name = paste0(path, "lpm_best.gif"))

# samples without y
best.lpm.sample.noy <- best.lpm.sample %>% select(-y)

worst.rand.sample.noy <- worst.rand.sample %>% select(-y)

# samples with new y
best.lpm.sample.mid <- left_join(best.lpm.sample.noy, true.data2)

worst.rand.sample.mid <- left_join(worst.rand.sample.noy, true.data2)

# model of new plume
lpm.app2.model <- sm.st(
  x = best.lpm.sample.mid[,3:4],
  t = best.lpm.sample.mid[,2],
  y = log(best.lpm.sample.mid[,5]),
  lambda = log.post,
  nseg = nseg,
  bdeg = bdeg,
  pord = pord
) 

rand.app2.model <- sm.st(
  x = worst.rand.sample.mid[,3:4],
  t = worst.rand.sample.mid[,2],
  y = log(worst.rand.sample.mid[,5]),
  lambda = log.post,
  nseg = nseg,
  bdeg = bdeg,
  pord = pord
) 

# rmspes with new plume

# LPM TOTAL
# calculating predictions on the grid
lpm.app2.preds <- fut.B$B %*% lpm.app2.model$alpha
# calculating rmspe
lpm.app2.rmspe <- sqrt(sum((log(true.data2$y) - lpm.app2.preds)^2)/nrow(lpm.app2.preds))

# LPM PLUME
# filtering predictions for plume only
lpm.app2.plume.preds <- cbind(true.data2, lpm.app2.preds) %>% filter(log(true.data2$y) >= limit)
#calculating rmspe for plume only
lpm.app2.plume.rmspe <- sqrt(sum((log(lpm.app2.plume.preds$y) - lpm.app2.plume.preds$lpm.app2.preds)^2)/nrow(lpm.app2.plume.preds))


# SRS TOTAL
# calculating predictions on the grid
rand.app2.preds <- fut.B$B %*% rand.app2.model$alpha
# calculating rmspe
rand.app2.rmspe <- sqrt(sum((log(true.data2$y) - rand.app2.preds)^2)/nrow(rand.app2.preds))

# SRS PLUME
# filtering predictions for plume only
rand.app2.plume.preds <- cbind(true.data2, rand.app2.preds) %>% filter(log(true.data2$y) >= limit)
#calculating rmspe for plume only
rand.app2.plume.rmspe <- sqrt(sum((log(rand.app2.plume.preds$y) - rand.app2.plume.preds$rand.app2.preds)^2)/nrow(rand.app2.plume.preds))


## trying spatial model


best.lpm.spatial <- best.lpm.sample %>%
  filter(Time == ts2[1])

worst.rand.spatial <- worst.rand.sample %>%
  filter(Time == ts2[1])


best.lpm.model <- mgcv::gam(data = best.lpm.sample.mid, formula = log(y) ~ s(X1,X2, k=6))

worst.rand.model <- mgcv::gam(data = worst.rand.sample.mid, formula = log(y) ~ s(X1,X2, k=6))


best.lpm.preds <- predict.gam(best.lpm.model, newdata = true.data2)

best.lpm.preds <- cbind(true.data2, best.lpm.preds)
names(best.lpm.preds)[5] <- 'preds'

worst.rand.preds <- predict.gam(worst.rand.model, newdata = true.data2)

worst.rand.preds <- cbind(true.data2, worst.rand.preds)
names(worst.rand.preds)[5] <- 'preds'


# rmspes 

# LPM TOTAL
best.lpm.rmspe <- sqrt(sum((log(best.lpm.preds$y) - best.lpm.preds$preds)^2)/nrow(best.lpm.preds))

# LPM PLUME
best.lpm.preds.plume <- best.lpm.preds %>% filter(log(y)>=limit)
best.lpm.plume.rmspe <- sqrt(sum((log(best.lpm.preds.plume$y) - best.lpm.preds.plume$preds)^2)/nrow(best.lpm.preds.plume))


# SRS TOTAL
worst.rand.rmspe <- sqrt(sum((log(worst.rand.preds$y) - worst.rand.preds$preds)^2)/nrow(worst.rand.preds))

# SRS PLUME
worst.rand.preds.plume <- worst.rand.preds %>% filter(log(y)>=limit)
worst.rand.plume.rmspe <- sqrt(sum((log(worst.rand.preds.plume$y) - worst.rand.preds.plume$preds)^2)/nrow(worst.rand.preds.plume))

print(c(worst.spatial.rmspe, best.spatial.rmspe))
print(c(worst.spatial.plume.rmspe, best.spatial.plume.rmspe))




