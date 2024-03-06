##Install packages needed
install.packages(c("ggpubr", "ggplot2", "effects", "blme", "cowplot"))

#Open packages
library(ggplot2, ggpubr, readxl, Matrix, effects, blme, car, cowplot, grid)

##EXPERIMENT 1

#call and open data
mixed_split <- read_excel("Desktop/Vocal number paper/analyses/mixed_split.xlsx")
View(mixed_split)

#defining variables as factors
split.f <- as.factor(mixed_split$split_n)

#blmer Bayesian Mixed effects logistic model for response split with two fixed effects leaf number and group size and group ID as ramdom factor
model <- model <- bglmer(split.f ~ leaf_number +  group_size + order + (1|group), data=mixed_split, family = binomial(link = "logit"))
summary(model)

# Create a data frame with predictor values for prediction
grid <- expand.grid(leaf_number = seq(min(mixed_split$leaf_number), max(mixed_split$leaf_number), length.out = 100),
                    group_size = seq(min(mixed_split$group_size), max(mixed_split$group_size), length.out = 100))


## Extract predicting values for plotting
predictions <-as.data.frame(Effect(c("leaf_number", "group_size"), model))

# Create ggplot2 figure
print (ggplot(predictions, aes(x = leaf_number, y = fit, color = as.factor(group_size))) +
         geom_line() +
         labs(
           x = "Roosts Broadcasting Calls",
           y = "Predicted Probability of Group Fragmentation", color = "Group Size") + theme_minimal()) 

# Fit Bayesian mixed effects model using blme for first to enter as response, leaf number and group size as fixed effects and group ID as ramdom effects
b_model <- blmer(first ~ leaf_number + group_size + (1|group), data = mixed_split)

# Display summary
summary(b_model)
Anova(b_model)

# Fit the model using blme for difference in time
b_model1 <- blmer(diff ~ leaf_number + group_size + (1|group), data = mixed_split)

# Display summary
summary(b_model1)
Anova(b_model1)


# Fit the model using blme for total time to enter
b_model2 <- blmer(total ~ leaf_number + group_size + (1|group), data = mixed_split)

# Display summary
summary(b_model2)
Anova(b_model2)

# Create the first plot for first to enter
plot1 <- ggplot(mixed_split, aes(x = leaf_number, y = first, color = as.factor(group_size))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, linetype = "dashed", color = "blue") +
  labs(
    x = "Roosts with Calls",
    y = "First to Enter (sec)",
    color = "Group Size") +
  theme_minimal()

# Create the second plot for difference in time
plot2 <- ggplot(mixed_split, aes(x = leaf_number, y = diff, color = as.factor(group_size))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, linetype = "dashed", color = "blue") +
  labs(
    x = "Roosts with Calls",
    y = "Difference in Time (sec)",
    color = "Group Size") +
  theme_minimal()

# Create the third plot for total time to enter
plot3 <- ggplot(mixed_split, aes(x = leaf_number, y = total, color = as.factor(group_size))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, linetype = "dashed", color = "blue") +
  labs(
    x = "Roosts with Calls",
    y = "Total Time to Enter (sec)",
    color = "Group Size") +
  theme_minimal()

# Add labels to each plot
plot1 <- plot1 + annotation_custom(textGrob("A)", gp=gpar(fontsize=12, fontface="bold")), 
                                   xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
plot2 <- plot2 + annotation_custom(textGrob("B)", gp=gpar(fontsize=12, fontface="bold")), 
                                   xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
plot3 <- plot3 + annotation_custom(textGrob("C)", gp=gpar(fontsize=12, fontface="bold")), 
                                   xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

# Combine the plots with shared x-axis and legend using cowplot
combined_plot <- plot_grid(plot1, plot2, plot3)

# Display the combined plot
print(combined_plot)

#blmer Bayesian Mixed effects model for total time with group size and group ID as ramdom factor
model <- model <- blmer(total ~  group_size + (1|group), data=mixed_split)
summary(model)
Anova(b_model)

##EXPERIMENT 2

#call data
Second <- read_excel("Desktop/Vocal number paper/analyses/Second part data1.xlsx")
View(Second)

# Bayesian mixed effects model for
model2 <- blmer(number ~ leaf + (1|group), data=Second)
summary(model2)
Anova(model2)

# Create the boxplot with individual points
ggplot(Second, aes(x = leaf, y = number)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2), color = "blue", alpha = 0.5) +
  labs(
       x = "Call Rates",
       y = "Number of Invididuals") +theme_minimal()
