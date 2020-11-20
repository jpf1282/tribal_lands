


drought.plot <- ggplot(data_t1and2_long, aes(time, drt_median, colour = time)) +
  geom_boxplot(varwidth = T) +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  scale_y_reverse(breaks = seq(-1.5,1,by=.5), labels = c("More Drought   -1.5","-1.0","-0.5","0","0.5","Less Drought   1.0")) +
  scale_colour_manual(values = c('#E0E022','#ff0000'),aesthetics = c("colour", "fill")) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = c("time 1" = "Historical",
                              "time 2" = "Present-day")) +
  theme(legend.position = "none")

ggsave(plot=drought.plot,filename = "drought_boxplot.pdf")



# Katie's Added Plots -----------------------------------------------------
library(ggbeeswarm)

data_t1and2_long <- filter(data_long, tribe %in% tribes_time1and2_lst$tribe)

# Heat Days Count
heat.plot <- ggplot(data_t1and2_long, aes(time, h_100_hist, colour = time)) +
  geom_boxplot(varwidth = T) +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  scale_y_continuous(breaks = seq(0, 60, by=20), labels = c("0", "20", "40", "Days Above 100 F   60")) +
  scale_colour_manual(values = c('#E0E022','#ff0000'),aesthetics = c("colour", "fill")) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = c("time 1" = "Historical",
                              "time 2" = "Present-day")) +
  theme(legend.position = "none")

# Wildfire Mean
fire.mean.plot <- ggplot(data_t1and2_long, aes(time, fire_mean, colour = time)) +
  geom_boxplot(varwidth = T) +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  scale_y_continuous(breaks = seq(0, 4), labels = c("Low Wildfire Hazard Potential   0", "1", "2", "3", "High Wildfire Hazard Potential   4")) +
  scale_colour_manual(values = c('#E0E022','#ff0000'),aesthetics = c("colour", "fill")) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = c("time 1" = "Historical",
                              "time 2" = "Present-day")) +
  theme(legend.position = "none")

# Wildfire Median
fire.median.plot <- ggplot(data_t1and2_long, aes(time, fire_median, colour = time)) +
  geom_boxplot(varwidth = T) +
  geom_quasirandom(alpha = 1/10, varwidth = TRUE) +
  scale_y_continuous(breaks = seq(0, 4), labels = c("Low Wildfire Hazard Potential   0", "1", "2", "3", "High Wildfire Hazard Potential   4")) +
  scale_colour_manual(values = c('#E0E022','#ff0000'),aesthetics = c("colour", "fill")) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = c("time 1" = "Historical",
                              "time 2" = "Present-day")) +
  theme(legend.position = "none")


