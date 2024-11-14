#The following packages should be installed in order to run this analysis
#install.packages(tidyverse)
#install.packages(dplyr)
#install.packages(ggpubr)
#install.packages(psych)

# Part 1: Analysis of pre-test and post test score
#Import score to R
library(readxl)
Pre_Post_score <- read_excel("pre-posttest.xlsx", sheet ="score")
View(Pre_Post_score)

# calculate mean and sd
library("dplyr")

#get mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#summarize statistic
group_by(Pre_Post_score, Test) %>%
  summarise(
    count = n(),
    mean = mean(Score, na.rm = TRUE),
    median = median(Score, na.rm = TRUE),
    mode = get_mode(Score),
    sd = sd(Score, na.rm = TRUE)
  )

#box plot
library("ggpubr")
ggboxplot(Pre_Post_score, x = "Test", y = "Score", 
          color = "Test", palette = c("#00AFBB", "#E7B800"),
          order = c("Pre-test", "Post-test"),
          ylab = "Score", xlab = "Test")

#Normality test (n < 50 use Shapiro-Wilk normality test)
# compute the difference
score_pre <- with(Pre_Post_score,Score[Test == "Pre-test"])
score_pre
score_post <- with(Pre_Post_score,Score[Test == "Post-test"])
score_post
score_diff <- with(Pre_Post_score, 
                   Score[Test == "Pre-test"] - Score[Test == "Post-test"])
score_diff
# Shapiro-Wilk normality test for the differences
shapiro.test(score_pre) # => p-value = 0.093, Fail to reject null hypothesis (normal distribution)
shapiro.test(score_post) # => p-value = 7.899e-09, null hypothesis (not normal distribution)
shapiro.test(score_diff) # => p-value = 0.5079, Fail to reject null hypothesis (normal distribution)

#Pre_Post_score_2 <- read_excel("pre-posttest.xlsx", sheet ="Sheet2")
#View(Pre_Post_score_2)

pre_scores <- Pre_Post_score$Score[Pre_Post_score$Test == "Pre-test"]
post_scores <- Pre_Post_score$Score[Pre_Post_score$Test == "Post-test"]

# Data is not normal distribution (non-parametric), so paired t test are used
res_diff <- wilcox.test(pre_scores, post_scores, paired = TRUE)
#res_diff <- t.test(pre_scores, post_scores, paired = TRUE)
res_diff
res_diff$p.value # => p-value = 1.405e-06 reject null hypothesis (score of pre-test and post test are different)

##################### END PART 1 #######################

#Part2: Analysis of pre and post test score by topic
#load data
Pre_Post_score_2 <- read_excel("pre-posttest.xlsx", sheet ="score_part")
View(Pre_Post_score_2)

results <- list()

# Loop through each pair
for (pair in list(
  c("Airborn_pre", "Airborn_post"),
  c("Droplet_pre", "Droplet_post"),
  c("Contact_pre", "Contact_post"),
  c("stop_the_spread_pre", "stop_the_spread_post")
)) {
  # Perform Wilcoxon signed-rank test
  test_result <- wilcox.test(Pre_Post_score_2[[pair[1]]], Pre_Post_score_2[[pair[2]]], paired = TRUE)
  # Store the result in the list
  results[[paste(pair, collapse = "_vs_")]] <- test_result
}

# View all results
results

#################### END PART 2 ############################

# Part 3: Analysis of pre-test score by rotate
#Group by pass surgery?
# Sx pass ??
Pre_Post_score_3 <- read_excel("pre-posttest.xlsx", sheet ="groupby_sx")
View(Pre_Post_score_3)

# Detailed descriptive statistics by group
library(psych)
descriptive_stats <- describeBy(Pre_Post_score_3$Score, group = Pre_Post_score_3$Sx, mat = TRUE)

print(descriptive_stats)

descriptive_stats <- aggregate(Score ~ Sx, data = Pre_Post_score_3, 
                               FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))

# Format the output
descriptive_stats <- do.call(data.frame, descriptive_stats)
colnames(descriptive_stats) <- c("Group", "Mean", "SD", "Count")
print(descriptive_stats)

# Levene's test
library(car)
levene_test_result <- leveneTest(Score ~ Sx, Pre_Post_score_3)
print(levene_test_result)

# Perform oneway anova
anova_result <- aov(Score ~ Sx, data = Pre_Post_score_3)
summary(anova_result)

# Perform Tukey's Honest Significant Difference test
tukey_result <- TukeyHSD(anova_result)

# View the results
print(tukey_result)

###################### END PART 3 ##########################