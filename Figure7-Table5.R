library(ggplot2)

load("HolderExponent2Dall_2048.RData")

Test1 <- cbind(R = rep(1, length(HExp2D1)), H = HExp2D1)
Test2 <- cbind(R = rep(2, length(HExp2D2)), H = HExp2D2)
Test3 <- cbind(R = rep(3, length(HExp2D3)), H = HExp2D3)
Test4 <- cbind(R = rep(4, length(HExp2D4)), H = HExp2D4)

Hexp1 <- data.frame(rbind(Test1, Test2, Test3, Test4))
Hexp1$R <- factor(Hexp1$R, levels = c("1", "2", "3", "4"), labels = c("warm", "cold", "mixture", "borderline"))

# Figure 7-This figure gives the plot of the distribution of $\hat{H}(t)$ values for chosen sky windows
ggplot(Hexp1, aes(x = R, y = H, fill = R)) +
  geom_boxplot(width = 0.45, position = position_dodge(width = 0.9), fatten = NULL) +
  labs(x = "Type of sky window", y = "H") +
  scale_fill_manual(name = "Type of sky window", labels = c("warm", "cold", "mixture", "borderline"), values = c("#CC0033", "#FFCC33", "#00AFBB", "#9933CC")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = 0.45, linetype = "solid", size = 1) +
  theme_bw() +
  theme(legend.position = "none")

# Checking for statistical significance (p-value) in difference of means
# [1]-Difference between warm and cold regions
# Checking the normality assumption of H(t) values
shapiro.test(HExp2D1)
shapiro.test(HExp2D2)

t.test(HExp2D1, HExp2D2)
wilcox.test(HExp2D1, HExp2D2, alternative = "two.sided")

# [2]-Difference between warm and mixture regions
# Checking the normality assumption of H(t) values
shapiro.test(HExp2D1)
shapiro.test(HExp2D3)

t.test(HExp2D1, HExp2D3)
wilcox.test(HExp2D1, HExp2D3, alternative = "two.sided")

# [3]-Difference between warm and borderline regions
# Checking the normality assumption of H(t) values
shapiro.test(HExp2D1)
shapiro.test(HExp2D4)

t.test(HExp2D1, HExp2D4)
wilcox.test(HExp2D1, HExp2D4, alternative = "two.sided")

# [4]-Difference between cold and mixture regions
# Checking the normality assumption of H(t) values
shapiro.test(HExp2D2)
shapiro.test(HExp2D3)

t.test(HExp2D2, HExp2D3)
wilcox.test(HExp2D2, HExp2D3, alternative = "two.sided")

# [5]-Difference between cold and borderline regions
# Checking the normality assumption of H(t) values
shapiro.test(HExp2D2)
shapiro.test(HExp2D4)

t.test(HExp2D2, HExp2D4)
wilcox.test(HExp2D2, HExp2D4, alternative = "two.sided")

# [6]-Difference between mixture and borderline regions
# Checking the normality assumption of H(t) values
shapiro.test(HExp2D3)
shapiro.test(HExp2D4)

t.test(HExp2D3, HExp2D4)
wilcox.test(HExp2D3, HExp2D4, alternative = "two.sided")