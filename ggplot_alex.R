
#### data
txt = dget('../comita/winbugs data.txt')
# str(txt)
# str(txt$PREDS)
# summary(txt$PREDS)


txt$PREDS = matrix(stack(as.data.frame(txt$PREDS))$values, byrow = T, ncol = 5)
txt$ABUND = matrix(stack(as.data.frame(txt$ABUND))$values, byrow = T, ncol = 3)

Predictors = txt$PREDS
Abund = txt$ABUND


dat = data.frame(Predictors[, c(2, 4, 3, 5)])
names(dat) = c("conSeed", "hetSeed", "conAdult", "hetAdult")
dat$surv = txt$SD
dat$sp = txt$SPP
dat$plot = txt$PLOT

dat$abund = Abund[dat$sp, 2]
dat$shade = Abund[dat$sp, 3]


# generate pos densities (mimic adding up mean again)
dat$conSeed_pos = dat$conSeed + abs(min(dat$conSeed))
dat$hetSeed_pos = dat$hetSeed + abs(min(dat$hetSeed))
dat$conAdult_pos = dat$conAdult + abs(min(dat$conAdult))
dat$hetAdult_pos = dat$hetAdult + abs(min(dat$hetAdult))

# generate total densities
dat$totSeed_pos = dat$conSeed_pos + dat$hetSeed_pos
dat$totAdult_pos = dat$conAdult_pos + dat$hetAdult_pos


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
