library(ggplot2)

load("HolderExponent1D_2048.RData")

Test1 <- cbind(R = rep(1, length(HExp1D1)), H = HExp1D1)
Test3 <- cbind(R = rep(3, length(HExp1D3)), H = HExp1D3)
Test4 <- cbind(R = rep(4, length(HExp1D4)), H = HExp1D4)
Test9 <- cbind(R = rep(9, length(HExp1D9)), H = HExp1D9)

Hexp1 <- data.frame(rbind(Test1, Test3, Test4, Test9))
Hexp1$R <- factor(Hexp1$R, levels = c("1", "3", "4", "9"), labels = c(525, 1275, 2250, 7500))

# Figure 3-This figure gives the plot of the distribution of $\hat{H}(t)$ values of four rim segments
ggplot(Hexp1, aes(x = R, y = H, fill = R)) +
  geom_boxplot(width = 0.45, position = position_dodge(width = 0.9), fatten = NULL) +
  labs(x = "Ring no.", y = "H(t)") +
  scale_fill_manual(name = "Ring no.", labels = c("525", "1275", "2250", "7500"), values = c("#CC0033", "#FFCC33", "#00AFBB", "#9933CC")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.45, linetype = "solid", size = 1) +
  theme_bw()+theme(legend.position = "none")

# Checking for statistical significance (p-value) in difference of means
# [1]-Difference between rings 1 & 3

# Using the proposed H method and all H(t) values
t.test(HExp1D1, HExp1D3)
wilcox.test(HExp1D1, HExp1D3, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)]) 

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D3[seq(1, length(HExp1D3), by = 50)]) 
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D3[seq(1, length(HExp1D3), by = 50)], alternative = "two.sided") 

# Using the R/S method and all H(t) values
t.test(HExp1D1c, HExp1D3c)
wilcox.test(HExp1D1c, HExp1D3c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D3c[seq(1, length(HExp1D3c), by = 50)], alternative = "two.sided")

# [2]-Difference between rings 1 & 4
# Using the proposed H method and all H(t) values
t.test(HExp1D1, HExp1D4)
wilcox.test(HExp1D1, HExp1D4, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)]) 

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)]) 
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)], alternative = "two.sided") 

# Using the R/S method and all H(t) values
t.test(HExp1D1c, HExp1D4c)
wilcox.test(HExp1D1c, HExp1D4c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)], alternative = "two.sided")

# [3]-Difference between rings 1 & 9
# Using the proposed H method and all H(t) values
t.test(HExp1D1, HExp1D9)
wilcox.test(HExp1D1, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)]) 

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) 
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") 

# Using the R/S method and all H(t) values
t.test(HExp1D1c, HExp1D9c)
wilcox.test(HExp1D1c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D6c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [4]-Difference between rings 3 & 4
# Using the proposed H method and all H(t) values
t.test(HExp1D3, HExp1D4)
wilcox.test(HExp1D3, HExp1D4, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)]) 
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)], alternative = "two.sided") 

# Using the R/S method and all H(t) values
t.test(HExp1D3c, HExp1D4c)
wilcox.test(HExp1D3c, HExp1D4c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)], alternative = "two.sided")

# [5]-Difference between rings 3 & 9
# Using the proposed H method and all H(t) values
t.test(HExp1D3, HExp1D9)
wilcox.test(HExp1D3, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) 
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") 

# Using the R/S method and all H(t) values
t.test(HExp1D3c, HExp1D9c)
wilcox.test(HExp1D3c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [6]-Difference between rings 4 & 9
# Using the proposed H method and all H(t) values
t.test(HExp1D4, HExp1D9)
wilcox.test(HExp1D4, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) 
wilcox.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") 

# Using the R/S method and all H(t) values
t.test(HExp1D4c, HExp1D9c)
wilcox.test(HExp1D4c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")