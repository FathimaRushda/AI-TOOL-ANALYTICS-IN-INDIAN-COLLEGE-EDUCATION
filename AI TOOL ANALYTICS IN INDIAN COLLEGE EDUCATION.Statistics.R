install.packages("dplyr")
library(dplyr)
# Load necessary data
df=read.csv("C:\\Users\\Rushda\\Desktop\\Rushda\\Students.csv")

View(df)
head(df)
summary(df)
str(df)


## 1. T test
result <- t.test(Daily_Usage_Hours ~ Willing_to_Pay_for_Access, data = df)
print(result)

if (result$p.value < 0.05) {
  print("Reject the null hypothesis: Statistically significant difference.")
} else {
  print("Fail to reject the null hypothesis: No statistically significant difference.")
}



## 2. z test

# Clean text
df$Internet_Access <- trimws(tolower(df$Internet_Access))

# Ensure num_tools exists
df$num_tools <- sapply(strsplit(as.character(df$AI_Tools_Used), ","), function(x) length(trimws(x)))
df$num_tools <- as.numeric(df$num_tools)

# Groups
group_high <- df$num_tools[df$Internet_Access == "high"]
group_poor <- df$num_tools[df$Internet_Access == "poor"]

# Check if both groups have enough data
if (length(group_high) > 1 & length(group_poor) > 1) {
  
  # Means
  mean_high <- mean(group_high, na.rm = TRUE)
  mean_poor <- mean(group_poor, na.rm = TRUE)
  
  # SDs
  sd_high <- sd(group_high, na.rm = TRUE)
  sd_poor <- sd(group_poor, na.rm = TRUE)
  
  # Sample sizes
  n_high <- length(na.omit(group_high))
  n_poor <- length(na.omit(group_poor))
  
  # Standard error
  se <- sqrt((sd_high^2 / n_high) + (sd_poor^2 / n_poor))
  
  # Z-stat
  z_value <- (mean_high - mean_poor) / se
  
  # P-value
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  
  # Output
  cat("Mean (High):", mean_high, "\n")
  cat("Mean (Poor):", mean_poor, "\n")
  cat("Z value:", z_value, "\n")
  cat("p-value:", p_value, "\n")
  
  if (p_value < 0.05) {
    cat("✅ Reject the null: Significant difference in mean num_tools.\n")
  } else {
    cat("❌ Fail to reject the null: No significant difference in mean num_tools.\n")
  }
  
} else {
  cat("❌ Not enough data in one or both groups.\n")
}



## 3. F test

result_f <- var.test(
  Impact_on_Grades ~ Willing_to_Pay_for_Access,
  data = df
)

print(result_f)

if (result_f$p.value < 0.05) {
  print("Reject null: Variances are significantly different.")
} else {
  print("Fail to reject null: Variances are not significantly different.")
}



## 4. ANOVA

anova_model <- aov(Daily_Usage_Hours ~ Stream, data = df)
summary(anova_model)
p_value <- summary(anova_model)[[1]][["Pr(>F)"]][1]
if (p_value < 0.05) {
  print("Reject null: At least one Stream has different mean hours.")
} else {
  print("Fail to reject null: No significant difference among Streams.")
}


## 5. CHI SQUARE

tbl <- table(df$Stream, df$Preferred_AI_Tool)
chisq.test(tbl)
result_chi <- chisq.test(tbl)

print(result_chi)

if (result_chi$p.value < 0.05) {
  print("Reject null: Significant association between tool and willingness.")
} else {
  print("Fail to reject null: No significant association.")
}















