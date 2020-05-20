


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