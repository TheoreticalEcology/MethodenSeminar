
#### data
dat = read.table("../data/SEEDLING.SURVIVAL01.06.min.edge.SWARS.txt", header = T)
datsp = read.csv("../data/NDD.ABUND.SPPLIST.w.NEIGHBOR.EFFECTS.txt", header = T, sep = "\t")


# rename data columns to how variables are named in modeling files
names(dat)[-1] = c("conSeed", "hetSeed", "sp", "surv", "plot", "conAdult", "hetAdult")

# generate total densities
dat$totSeed = dat$conSeed + dat$hetSeed
dat$totAdult = dat$conAdult + dat$hetAdult

# add abundance and shade tolerance
dat$abund = log1p(datsp$BA[dat$sp])     # plot(scale(log1p(datsp$BA)) ~ Abund[, 2]) 
dat$shade = datsp$gm.fit.PCA1[dat$sp]   # plot(datsp$gm.fit.PCA1 ~ Abund[, 3])
datsp$abund = log1p(datsp$BA)
datsp$shade = datsp$gm.fit.PCA1


# check columns
!names(dat) %in% names(dat)


#### plots

# general plot: density per abundance
plot <- ggplot(id, aes(x = abund, y = conSeed_pos, group = abund)) +
  geom_point(alpha = .04, size = 0.4, color = 'black')

# complete data highlighting mean and max number of conspecific seedlings 
# and species with no range 
complete <- plot +
  geom_path(alpha = 0.4, linemitre = 1) +
  geom_point(stat ='summary', fun.y = 'mean', shape = 8, size = 2, color = 'red') +
  geom_point(stat ='summary', fun.y = 'max', size = 2, color = 'darkred') +
  geom_point(data = gdsing, mapping=aes(x = abund, y = meanSeed), alpha = 0.75, color = 'lightblue') +
  geom_text(aes(x = 0, y = 100), size = 4,  label='n = 180') +
  coord_trans(y = 'log1p') +
  scale_y_continuous(limits = c(0,100), breaks = c(seq(0,25,5), c(1,50,75,100))) +
  guides(color = 'none', fill = 'none') +
  theme_bw() + 
  theme(plot.title = element_text()) +
  labs(
    title = 'Complete density range',
    x = 'Species abundance adults',
    y = 'Conspecific seedling density'
  )

# species with range (>1 values) in conspecific seedlings
range1plus <- ggplot(gdmulti, aes(x = abund, y = maxSeed, group = abund))+
  geom_point(color = 'darkred') +
  geom_point(gdmulti, mapping=aes(x = abund, y = meanSeed), shape = 8, color = 'red') +
  geom_point(gdmulti, mapping=aes(x = abund, y = minSeed), color = 'salmon') +
  geom_text(aes(x = 0.5, y = 100), size = 4, label='n = 99') +
  coord_trans(y = 'log1p') +
  scale_y_continuous(limits = c(0,100), breaks = c(seq(0,25,5), c(1,50,75,100))) +
  theme_bw() + theme(plot.title = element_text()) +
  labs(
    title = 'Density range n > 1', size = 8,
    x = 'Species abundance adults',
    y = 'Conspecific seedling density'
  )

# species with no range (just 1 value) in conspecific seedlings
range0 <- ggplot(gdsing, aes(x = abund, y = maxSeed))+
  geom_point(alpha = 0.8, color = 'lightblue') +
  geom_text(aes(x = 0.5, y = 100), size = 4,  label='n = 81') +
  coord_trans(y = 'log1p') +
  scale_y_continuous(limits = c(0,100), breaks = c(seq(0,25,5), c(1,50,75,100))) +
  theme_bw() + theme(plot.title = element_text()) +
  labs(
    title = 'Density range n = 1', size = 7,
    x = 'Species abundance adults',
    y = 'Conspecific seedling density'
  )

# plots combined on 1 page
combined <- grid.arrange(range0 + rremove('xylab'), range1plus + rremove('xylab'), complete + rremove('xylab'), layout_matrix = rbind(c(1,2), c(3,3)))



# plots combined on 1 page with Text
annotate_figure(combined,
                top = text_grob('Seedling Density per Species Abundance', face = 'bold', size = 14),
                bottom = text_grob('Species abundance adults', size = 12),
                left = text_grob('Conspecific seedling density',size = 12, rot = 90))
