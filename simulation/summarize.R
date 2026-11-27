# Combine simulation output into summary.csv (rejection rates in %, alpha = .05)
d <- rbind(read.csv("simulation/res_part0.csv"), read.csv("simulation/res_part1.csv"))
n <- rbind(read.csv("simulation/res_nonpar0.csv"), read.csv("simulation/res_nonpar1.csv"))
a <- aggregate(cbind(jmle = p_jmle_Q < .05, mmlQ = p_mml_Q < .05, wald = p_wald < .05) ~ J + I + K + shift, d, mean)
b <- aggregate(cbind(fried = p_friedman < .05, perm = p_perm < .05) ~ J + I + K + shift, n, mean)
m <- merge(a, b); m[, 5:9] <- round(100 * m[, 5:9], 1)
m <- m[order(m$shift, m$K, m$I, m$J), ]
write.csv(m, "simulation/summary.csv", row.names = FALSE)
# Size-adjusted power (I = 20, K = 5): critical value = 5th percentile of null p values
adj <- function(x, v, J) { c0 <- quantile(x[x$J == J & x$I == 20 & x$K == 5 & x$shift == 0, v], .05, na.rm = TRUE)
  sapply(c(.5, 1), function(s) round(100 * mean(x[x$J == J & x$I == 20 & x$K == 5 & x$shift == s, v] <= c0), 1)) }
print(do.call(rbind, lapply(c(3, 5, 7, 10), function(J) c(J = J,
  unlist(lapply(c("p_jmle_Q", "p_mml_Q", "p_wald"), adj, x = d, J = J)),
  unlist(lapply(c("p_friedman", "p_perm"), adj, x = n, J = J))))))
