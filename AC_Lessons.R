# Load required libraries
library(ggplot2)
library(ggpubr)

# Create data frames for each question
adversary_knowledge <- data.frame(
  Category = c("Not at all", "Reputation", "Acquaintances", "Colleagues", "Close Colleagues"),
  Count = c(1, 5, 11, 8, 4)
)

challenge_level <- data.frame(
  Category = c("Much easier", "Easier", "About the same", "More challenging", "Much more challenging"),
  Count = c(0, 3, 7, 18, 1)
)

quality_rating <- data.frame(
  Category = c("Much lower quality", "Lower", "About the same", "Higher", "Much higher quality"),
  Count = c(0, 0, 9, 15, 5)
)

conflict_level <- data.frame(
  Category = c("Unresolvable Conflict", "Major Conflict", "Minor Conflict", "None"),
  Count = c(0, 2, 12, 15)
)

# Function to create bar plots with mean lines
create_bar_plot <- function(data, title) {
  # Assign numeric values to categories for mean calculation
  data$Numeric <- seq(0, nrow(data) - 1)  # 0, 1, 2, ... for categories
  
  # Calculate the weighted mean
  weighted_mean <- sum(data$Numeric * data$Count) / sum(data$Count)
  
  ggplot(data, aes(x = Count, y = reorder(Category, Numeric))) +
    geom_bar(stat = "identity", fill = "navy", color = "black") +
    geom_hline(yintercept = weighted_mean +1, color = "red", linetype = "dashed", size = 2) +
    labs(title = title, x = "Count", y = "") +
    xlim(0, 20) + # Set consistent x-axis limits
    theme_bw() +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          plot.title = element_text(size = 12, face = "bold"))
}



# Create individual plots
plot1 <- create_bar_plot(adversary_knowledge, "Relationship with Adversary before AC")
plot2 <- create_bar_plot(challenge_level, "Challenge of AC Relative to Other Studies")
plot3 <- create_bar_plot(quality_rating, "Quality of AC Compared to Other Works")
plot4 <- create_bar_plot(conflict_level, "Conflict During AC")

# Arrange plots in a 2x2 grid using ggarrange
final_plot <- ggarrange(plot1, plot2, plot3, plot4,
                        ncol = 2, nrow = 2,
                        labels = c("A", "B", "C", "D"))

# Display the final plot
print(final_plot)
ggsave("Figure.png", 
       plot = final_plot, 
       width = 11, 
       height = 7, 
       dpi = 200, 
       units = "in")


