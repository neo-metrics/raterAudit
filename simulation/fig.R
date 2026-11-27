library(ggplot2)
m <- read.csv("simulation/summary.csv"); t1 <- subset(m, shift == 0)
a <- aggregate(cbind(jmle, mmlQ, wald, fried, perm) ~ J + I, t1, mean)
L <- reshape(a, direction = "long", varying = c("jmle","mmlQ","wald","fried","perm"), v.names = "rate",
             timevar = "test", times = c("Fixed chi-square (JMLE)","Fixed chi-square (MML)","Wald (MML)","Friedman","Within-item permutation"))
L$test <- factor(L$test, levels = c("Fixed chi-square (JMLE)","Fixed chi-square (MML)","Wald (MML)","Friedman","Within-item permutation"))
L$I <- factor(paste(L$I, "items"), levels = paste(c(10,20,40), "items"))
p <- ggplot(L, aes(J, rate, linetype = test, shape = test)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 7.5, alpha = .15) +
  geom_hline(yintercept = 5, linewidth = .3) + geom_line(linewidth = .5) + geom_point(size = 2) +
  facet_wrap(~ I) + scale_x_continuous(breaks = c(3,5,7,10)) +
  scale_shape_manual(values = c(16, 1, 17, 2, 15)) +
  labs(x = "Number of raters", y = "Empirical Type I error rate (%)", linetype = NULL, shape = NULL) +
  theme_bw(base_size = 11, base_family = "serif") + theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  guides(linetype = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) + theme(legend.key.width = unit(1.6, "lines"))
ggsave("simulation/figure1.png", p, width = 6.5, height = 3.9, dpi = 300)
print(a)
