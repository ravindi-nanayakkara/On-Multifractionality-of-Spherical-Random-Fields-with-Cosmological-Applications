library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(colorspace)

load("HolderExponent1D_2048.RData")

# Box plots of H(t) values of the considered 9 rings using our method

Test1 <- cbind(R = rep(1, length(HExp1D1)), H = HExp1D1)
Test2 <- cbind(R = rep(2, length(HExp1D2)), H = HExp1D2)
Test3 <- cbind(R = rep(3, length(HExp1D3)), H = HExp1D3)
Test4 <- cbind(R = rep(4, length(HExp1D4)), H = HExp1D4)
Test5 <- cbind(R = rep(5, length(HExp1D5)), H = HExp1D5)
Test6 <- cbind(R = rep(6, length(HExp1D6)), H = HExp1D6)
Test7 <- cbind(R = rep(7, length(HExp1D7)), H = HExp1D7)
Test8 <- cbind(R = rep(8, length(HExp1D8)), H = HExp1D8)
Test9 <- cbind(R = rep(9, length(HExp1D9)), H = HExp1D9)
Hexp <- data.frame(rbind(Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8, Test9))
Hexp$R <- factor(Hexp$R, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), labels = c(525, 875, 1275, 2250, 5000, 5900, 7035, 7275, 7500))

ggplot(Hexp, aes(x = R, y = H, fill = R)) +
  geom_boxplot(width = 0.5, fatten = NULL) +
  labs(x = "Ring no.", y = "H") +
  scale_fill_discrete(name = "Ring no.", labels = c("525", "875", "1275", "2250", "5000", "5900", "7035", "7275", "7500")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.5, linetype = "solid", size = 1) +
  theme_bw()


# Box plots of H(t) values of the considered 9 rings using R/S method

Test1c <- cbind(R = rep(1, length(HExp1D1c)), H = HExp1D1c)
Test2c <- cbind(R = rep(2, length(HExp1D2c)), H = HExp1D2c)
Test3c <- cbind(R = rep(3, length(HExp1D3c)), H = HExp1D3c)
Test4c <- cbind(R = rep(4, length(HExp1D4c)), H = HExp1D4c)
Test5c <- cbind(R = rep(5, length(HExp1D5c)), H = HExp1D5c)
Test6c <- cbind(R = rep(6, length(HExp1D6c)), H = HExp1D6c)
Test7c <- cbind(R = rep(7, length(HExp1D7c)), H = HExp1D7c)
Test8c <- cbind(R = rep(8, length(HExp1D8c)), H = HExp1D8c)
Test9c <- cbind(R = rep(9, length(HExp1D9c)), H = HExp1D9c)
Hexpc <- data.frame(rbind(Test1c, Test2c, Test3c, Test4c, Test5c, Test6c, Test7c, Test8c, Test9c))
Hexpc$R <- factor(Hexpc$R, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), labels = c(525, 875, 1275, 2250, 5000, 5900, 7035, 7275, 7500))

ggplot(Hexpc, aes(x = R, y = H, fill = R)) +
  geom_boxplot(width = 0.5, fatten = NULL) +
  labs(x = "Ring no.", y = "Conventional H") +
  scale_fill_discrete(name = "Ring no.", labels = c("525", "875", "1275", "2250", "5000", "5900", "7035", "7275", "7500")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.5, linetype = "solid", size = 1) +
  theme_bw()

# Distribution box plots of H(t) values for 4 rings

Hexp1 <- data.frame(rbind(Test1, Test3, Test4, Test9))
Hexp1$R <- factor(Hexp1$R, levels = c("1", "3", "4", "9"), labels = c(525, 1275, 2250, 7500))

ggplot(Hexp1, aes(x = R, y = H, fill = R)) +
  geom_boxplot(width = 0.45, position = position_dodge(width = 0.9), fatten = NULL) +
  labs(x = "Ring no.", y = "H(t)") +
  scale_fill_manual(name = "Ring no.", labels = c("525", "1275", "2250", "7500"), values = c("#CC0033", "#FFCC33", "#00AFBB", "#9933CC")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.45, linetype = "solid", size = 1) +
  theme_bw()

# Checking for statistical significance (p-value) in difference of means

# [1]-Difference between Ring 1 & 2

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D2)
wilcox.test(HExp1D1, HExp1D2, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)]) # p-value=0.2523
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)]) # p-value=0.6880

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D2[seq(1, length(HExp1D2), by = 50)]) # p-value=8.96e-11
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D2[seq(1, length(HExp1D2), by = 50)], alternative = "two.sided") # p-value=5.198e-12

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D2c)
wilcox.test(HExp1D1c, HExp1D2c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D2c[seq(1, length(HExp1D2c), by = 50)], alternative = "two.sided")

# [2]-Difference between Ring 1 & 3

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D3)
wilcox.test(HExp1D1, HExp1D3, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)]) # p-value=0.06604

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D3[seq(1, length(HExp1D3), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D3[seq(1, length(HExp1D3), by = 50)], alternative = "two.sided") # p-value=3.048e-15

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D3c)
wilcox.test(HExp1D1c, HExp1D3c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D3c[seq(1, length(HExp1D3c), by = 50)], alternative = "two.sided")

# [3]-Difference between Ring 1 & 4

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D4)
wilcox.test(HExp1D1, HExp1D4, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)]) # p-value=0.004012

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)], alternative = "two.sided") # p-value=7.939e-11

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D4c)
wilcox.test(HExp1D1c, HExp1D4c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)], alternative = "two.sided")

# [4]-Difference between Ring 1 & 5

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D5)
wilcox.test(HExp1D1, HExp1D5, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)]) # p-value=0.004092

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)], alternative = "two.sided") # p-value=7.939e-11

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D5c)
wilcox.test(HExp1D1c, HExp1D5c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)], alternative = "two.sided")

# [5]-Difference between Ring 1 & 6

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D6)
wilcox.test(HExp1D1, HExp1D6, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)]) # p-value=3.59e-05

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)], alternative = "two.sided") # p-value=7.939e-11

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D6c)
wilcox.test(HExp1D1c, HExp1D6c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)], alternative = "two.sided")

# [6]-Difference between Ring 1 & 7

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D7)
wilcox.test(HExp1D1, HExp1D7, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)]) # p-value=0.9295

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)], alternative = "two.sided") # p-value=6.445e-15

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D7c)
wilcox.test(HExp1D1c, HExp1D7c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)], alternative = "two.sided")

# [7]-Difference between Ring 1 & 8

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D8)
wilcox.test(HExp1D1, HExp1D8, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value=0.06894

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value=1.552e-14
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)], alternative = "two.sided") # p-value=1.731e-13

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D8c)
wilcox.test(HExp1D1c, HExp1D8c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)], alternative = "two.sided")

# [8]-Difference between Ring 1 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D1, HExp1D9)
wilcox.test(HExp1D1, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1[seq(1, length(HExp1D1), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value=0.2829

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value=1.858e-08
wilcox.test(HExp1D1[seq(1, length(HExp1D1), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # p-value=1.533e-08

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D1c, HExp1D9c)
wilcox.test(HExp1D1c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D6c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D1c[seq(1, length(HExp1D1c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

#################################################################################################################

# [9]-Difference between Ring 2 & 3

# Using our method
# Using all the H(t) values
t.test(HExp1D2, HExp1D3)
wilcox.test(HExp1D2, HExp1D3, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)])
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D3[seq(1, length(HExp1D3), by = 50)]) # p-value=8.405e-07
wilcox.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D3[seq(1, length(HExp1D3), by = 50)], alternative = "two.sided") # p-value=7.812e-06

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D2c, HExp1D3c)
wilcox.test(HExp1D2c, HExp1D3c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
wilcox.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D3c[seq(1, length(HExp1D3c), by = 50)], alternative = "two.sided")

# [10]-Difference between Ring 2 & 4

# Using our method
# Using all the H(t) values
t.test(HExp1D2, HExp1D4)
wilcox.test(HExp1D2, HExp1D4, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)])
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)], alternative = "two.sided") # p-value=4.039e-15

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D2c, HExp1D4c)
wilcox.test(HExp1D2c, HExp1D4c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
wilcox.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)], alternative = "two.sided")

# [11]-Difference between Ring 2 & 5

# Using our method
# Using all the H(t) values
t.test(HExp1D2, HExp1D5)
wilcox.test(HExp1D2, HExp1D5, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)])
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)], alternative = "two.sided") # p-value=3.365e-15

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D2c, HExp1D5c)
wilcox.test(HExp1D2c, HExp1D5c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
wilcox.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)], alternative = "two.sided")

# [12]-Difference between Ring 2 & 6

# Using our method
# Using all the H(t) values
t.test(HExp1D2, HExp1D6)
wilcox.test(HExp1D2, HExp1D6, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)])
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)], alternative = "two.sided") # p-value=3.365e-15

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D2c, HExp1D6c)
wilcox.test(HExp1D2c, HExp1D6c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
wilcox.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)], alternative = "two.sided")

# [13]-Difference between Ring 2 & 7

# Using our method
# Using all the H(t) values
t.test(HExp1D2, HExp1D7)
wilcox.test(HExp1D2, HExp1D7, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)])
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D2c, HExp1D7c)
wilcox.test(HExp1D2c, HExp1D7c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
wilcox.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)], alternative = "two.sided")

# [14]-Difference between Ring 2 & 8

# Using our method
# Using all the H(t) values
t.test(HExp1D2, HExp1D8)
wilcox.test(HExp1D2, HExp1D8, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)])
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value=1.351e-13
wilcox.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)], alternative = "two.sided") # p-value=6.306e-14

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D2c, HExp1D8c)
wilcox.test(HExp1D2c, HExp1D8c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
wilcox.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)], alternative = "two.sided")

# [15]-Difference between Ring 2 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D2, HExp1D9)
wilcox.test(HExp1D2, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2[seq(1, length(HExp1D2), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value=0.0002859
wilcox.test(HExp1D2[seq(1, length(HExp1D2), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # p-value=0.000623

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D2c, HExp1D9c)
wilcox.test(HExp1D2c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D2c[seq(1, length(HExp1D2c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [16]-Difference between Ring 3 & 4

# Using our method
# Using all the H(t) values
t.test(HExp1D3, HExp1D4)
wilcox.test(HExp1D3, HExp1D4, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)]) # p-value=3.616e-14
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D4[seq(1, length(HExp1D4), by = 50)], alternative = "two.sided") # p-value=3.606e-12

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D3c, HExp1D4c)
wilcox.test(HExp1D3c, HExp1D4c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D4c[seq(1, length(HExp1D4c), by = 50)], alternative = "two.sided")

# [17]-Difference between Ring 3 & 5

# Using our method
# Using all the H(t) values
t.test(HExp1D3, HExp1D5)
wilcox.test(HExp1D3, HExp1D5, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D3c, HExp1D5c)
wilcox.test(HExp1D3c, HExp1D5c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)], alternative = "two.sided")

# [18]-Difference between Ring 3 & 6

# Using our method
# Using all the H(t) values
t.test(HExp1D3, HExp1D6)
wilcox.test(HExp1D3, HExp1D6, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D3c, HExp1D6c)
wilcox.test(HExp1D3c, HExp1D6c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)], alternative = "two.sided")

# [19]-Difference between Ring 3 & 7

# Using our method
# Using all the H(t) values
t.test(HExp1D3, HExp1D7)
wilcox.test(HExp1D3, HExp1D7, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)]) # p-value= 7.319e-09
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)], alternative = "two.sided") # p-value=1.317e-07

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D3c, HExp1D7c)
wilcox.test(HExp1D3c, HExp1D7c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)], alternative = "two.sided")

# [20]-Difference between Ring 3 & 8

# Using our method
# Using all the H(t) values
t.test(HExp1D3, HExp1D8)
wilcox.test(HExp1D3, HExp1D8, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value=0.4247
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)], alternative = "two.sided") # p-value=0.1458

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D3c, HExp1D8c)
wilcox.test(HExp1D3c, HExp1D8c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)], alternative = "two.sided")

# [21]-Difference between Ring 3 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D3, HExp1D9)
wilcox.test(HExp1D3, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3[seq(1, length(HExp1D3), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value=3.186e-11
wilcox.test(HExp1D3[seq(1, length(HExp1D3), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # p-value=4.605e-10

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D3c, HExp1D9c)
wilcox.test(HExp1D3c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D3c[seq(1, length(HExp1D3c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [22]-Difference between Ring 4 & 5

# Using our method
# Using all the H(t) values
t.test(HExp1D4, HExp1D5)
wilcox.test(HExp1D4, HExp1D5, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D5[seq(1, length(HExp1D5), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D4c, HExp1D5c)
wilcox.test(HExp1D4c, HExp1D5c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
wilcox.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D5c[seq(1, length(HExp1D5c), by = 50)], alternative = "two.sided")

# [23]-Difference between Ring 4 & 6

# Using our method
# Using all the H(t) values
t.test(HExp1D4, HExp1D6)
wilcox.test(HExp1D4, HExp1D6, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D4c, HExp1D6c)
wilcox.test(HExp1D4c, HExp1D6c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
wilcox.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)], alternative = "two.sided")

# [24]-Difference between Ring 4 & 7

# Using our method
# Using all the H(t) values
t.test(HExp1D4, HExp1D7)
wilcox.test(HExp1D4, HExp1D7, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)]) # p-value=1.464e-05
wilcox.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)], alternative = "two.sided") # p-value=3.498e-05

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D4c, HExp1D7c)
wilcox.test(HExp1D4c, HExp1D7c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
wilcox.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)], alternative = "two.sided")

# [25]-Difference between Ring 4 & 8

# Using our method
# Using all the H(t) values
t.test(HExp1D4, HExp1D8)
wilcox.test(HExp1D4, HExp1D8, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)], alternative = "two.sided") # p-value=5.169e-11

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D4c, HExp1D8c)
wilcox.test(HExp1D4c, HExp1D8c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
wilcox.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)], alternative = "two.sided")

# [26]-Difference between Ring 4 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D4, HExp1D9)
wilcox.test(HExp1D4, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4[seq(1, length(HExp1D4), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D4[seq(1, length(HExp1D4), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # 3.717e-13

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D4c, HExp1D9c)
wilcox.test(HExp1D4c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D4c[seq(1, length(HExp1D4c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [27]-Difference between Ring 5 & 6

# Using our method
# Using all the H(t) values
t.test(HExp1D5, HExp1D6)
wilcox.test(HExp1D5, HExp1D6, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)])
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D6[seq(1, length(HExp1D6), by = 50)], alternative = "two.sided") # p-value=1.19e-13

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D5c, HExp1D6c)
wilcox.test(HExp1D5c, HExp1D6c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
wilcox.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D6c[seq(1, length(HExp1D6c), by = 50)], alternative = "two.sided")

# [28]-Difference between Ring 5 & 7

# Using our method
# Using all the H(t) values
t.test(HExp1D5, HExp1D7)
wilcox.test(HExp1D5, HExp1D7, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)])
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D5c, HExp1D7c)
wilcox.test(HExp1D5c, HExp1D7c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
wilcox.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)], alternative = "two.sided")

# [29]-Difference between Ring 5 & 8

# Using our method
# Using all the H(t) values
t.test(HExp1D5, HExp1D8)
wilcox.test(HExp1D5, HExp1D8, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)])
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)], alternative = "two.sided") # p-value=1.624e-15

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D5c, HExp1D8c)
wilcox.test(HExp1D5c, HExp1D8c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
wilcox.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)], alternative = "two.sided")

# [30]-Difference between Ring 5 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D5, HExp1D9)
wilcox.test(HExp1D5, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5[seq(1, length(HExp1D5), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D5[seq(1, length(HExp1D5), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # p-value=3.717e-13

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D5c, HExp1D9c)
wilcox.test(HExp1D5c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D5c[seq(1, length(HExp1D5c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [31]-Difference between Ring 6 & 7

# Using our method
# Using all the H(t) values
t.test(HExp1D6, HExp1D7)
wilcox.test(HExp1D6, HExp1D7, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)])
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D6[seq(1, length(HExp1D6), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D6[seq(1, length(HExp1D6), by = 50)], HExp1D7[seq(1, length(HExp1D7), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D6c, HExp1D7c)
wilcox.test(HExp1D6c, HExp1D7c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
wilcox.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)], HExp1D7c[seq(1, length(HExp1D7c), by = 50)], alternative = "two.sided")

# [32]-Difference between Ring 6 & 8

# Using our method
# Using all the H(t) values
t.test(HExp1D6, HExp1D8)
wilcox.test(HExp1D6, HExp1D8, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)])
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D6[seq(1, length(HExp1D6), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D6[seq(1, length(HExp1D6), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)], alternative = "two.sided") # p-value=1.624e-15

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D6c, HExp1D8c)
wilcox.test(HExp1D6c, HExp1D8c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
wilcox.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)], alternative = "two.sided")

# [33]-Difference between Ring 6 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D6, HExp1D9)
wilcox.test(HExp1D6, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D6[seq(1, length(HExp1D6), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D6[seq(1, length(HExp1D6), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D6[seq(1, length(HExp1D6), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # p-value=3.717e-13

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D6c, HExp1D9c)
wilcox.test(HExp1D6c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D6c[seq(1, length(HExp1D6c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [34]-Difference between Ring 7 & 8

# Using our method
# Using all the H(t) values
t.test(HExp1D7, HExp1D8)
wilcox.test(HExp1D7, HExp1D8, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)])
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D7[seq(1, length(HExp1D7), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)]) # p-value=3.968e-14
wilcox.test(HExp1D7[seq(1, length(HExp1D7), by = 50)], HExp1D8[seq(1, length(HExp1D8), by = 50)], alternative = "two.sided") # p-value=4.552e-13

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D7c, HExp1D8c)
wilcox.test(HExp1D7c, HExp1D8c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
wilcox.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)], HExp1D8c[seq(1, length(HExp1D8c), by = 50)], alternative = "two.sided")

# [35]-Difference between Ring 7 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D7, HExp1D9)
wilcox.test(HExp1D7, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D7[seq(1, length(HExp1D7), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D7[seq(1, length(HExp1D7), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D7[seq(1, length(HExp1D7), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # p-value < 2.2e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D7c, HExp1D9c)
wilcox.test(HExp1D7c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D7c[seq(1, length(HExp1D7c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")

# [36]-Difference between Ring 8 & 9

# Using our method
# Using all the H(t) values
t.test(HExp1D8, HExp1D9)
wilcox.test(HExp1D8, HExp1D9, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D8[seq(1, length(HExp1D8), by = 50)])
shapiro.test(HExp1D9[seq(1, length(HExp1D9), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D8[seq(1, length(HExp1D8), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)]) # p-value < 2.2e-16
wilcox.test(HExp1D8[seq(1, length(HExp1D8), by = 50)], HExp1D9[seq(1, length(HExp1D9), by = 50)], alternative = "two.sided") # p-value=8.037e-16

# Using the conventional method
# Using all the H(t) values
t.test(HExp1D8c, HExp1D9c)
wilcox.test(HExp1D8c, HExp1D9c, alternative = "two.sided")

# Checking the normality assumption ensuring the independence of observations using H(t) values with step 50
shapiro.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)])
shapiro.test(HExp1D9c[seq(1, length(HExp1D9c), by = 50)])

# Carrying out the T-test and Wilcoxon's test to check for difference in means
t.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)])
wilcox.test(HExp1D8c[seq(1, length(HExp1D8c), by = 50)], HExp1D9c[seq(1, length(HExp1D9c), by = 50)], alternative = "two.sided")
