#DRAW AND SHADE THE CRITICAL VALUE

library(ggplot2)

# 1. Parameters
mean_weight = mean(dfMPG$Weight)
mu <- mean_weight
alpha <- 0.05
df <- 290   # placeholder df for Welch t-test illustration

t_critical_left  <- qt(alpha / 2, df = df)
t_critical_right <- qt(1 - alpha / 2, df = df)

# 2. Data for plotting
plot_data <- dfMPG
x <- sampSlopeHolder

# 3. Base plot with t distribution
p <- ggplot(plot_data, aes(x = sampSlopeHolder)) +
  stat_function(
    fun = dt,
    args = list(df = df),
    linewidth = 1
  ) +
  labs(
    title = "Sampling Distribution of X̄_LM − X̄_LF (α = 0.05)",
    x = expression(bar(X)[LM] - bar(X)[LF]),
    y = "Density"
  ) +
  theme_minimal()

# 4. Shade critical regions
p <- p +
  geom_area(
    data = subset(plot_data, x < t_critical_left),
    aes(y = dt(x, df)),
    fill = "red",
    alpha = 0.4
  ) +
  geom_area(
    data = subset(plot_data, x > t_critical_right),
    aes(y = dt(x, df)),
    fill = "red",
    alpha = 0.4
  )

# 5. Add critical value lines
p <- p +
  geom_vline(
    xintercept = c(t_critical_left, t_critical_right),
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = c(t_critical_left, t_critical_right),
    y = 0.02,
    label = c(expression(-t^"*"), expression(t^"*")),
    hjust = c(1.2, -0.2)
  )

print(p)
