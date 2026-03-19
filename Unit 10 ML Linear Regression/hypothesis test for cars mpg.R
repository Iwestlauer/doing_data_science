#hypothesis testing for cars mpg

library(ggplot2)

# Parameters
alpha <- 0.05
df <- 390

t_critical_left  <- qt(alpha/2, df)
t_critical_right <- qt(1 - alpha/2, df)

# Create x values for the t distribution
x <- seq(-4, 4, length.out = 1000)

plot_data <- data.frame(
  x = x,
  y = dt(x, df)
)

# Base plot
p <- ggplot(plot_data, aes(x, y)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Sampling Distribution of t-statistic for Slope",
    x = "t value",
    y = "Density"
  ) +
  theme_minimal()

# Shade rejection regions
p <- p +
  geom_area(
    data = subset(plot_data, x < t_critical_left),
    fill = "red",
    alpha = 0.4
  ) +
  geom_area(
    data = subset(plot_data, x > t_critical_right),
    fill = "red",
    alpha = 0.4
  )

# Add critical value lines
p <- p +
  geom_vline(xintercept = c(t_critical_left, t_critical_right),
             linetype = "dashed") +
  annotate("text",
           x = t_critical_left,
           y = 0.02,
           label = round(t_critical_left,2),
           hjust = 1.3) +
  annotate("text",
           x = t_critical_right,
           y = 0.02,
           label = round(t_critical_right,2),
           hjust = -0.3)

print(p)

p +
  annotate("text",
           x = -3.8,
           y = 0.3,
           label = "t = -29.7 \n(far in \nrejection \nregion)",
           color = "blue")
