library(readr)
library(readxl)
df <- read.csv("matches.csv", header = TRUE)
head(matches)
str(matches)
summary(matches)

# print column names 
print(colnames(df))

#created new column toss_result_match to compare the toss_winning team and winner of the match 
df$toss_result_match <- ifelse(df$toss_winner == df$winner, "Won Match", "Lost Match")
table(df$toss_result_match)

#Added contingency table comparing toss decision vs toss-winning team match result
table_decision <- table(df$toss_result_match, df$toss_decision)
table_decision

#perform chi-square test 
chisq_result <- chisq.test(table_decision)
chisq_result

# Calculate the percentage of matches won/lost within each toss decision (bat/field)

percentages <- prop.table(table_decision, margin = 2) * 100
percentages

#Create a stacked barplot to compare the percentage of matches won and lost 
#for toss-winning teams by their toss decision (bat or field)

bp <- barplot(
  percentages,
  col = c("blue", "pink"),    # two colours for Won/Lost
  xlab = "Coin Toss Win Decision",
  ylab = "Percentage of Matches",
  main = "Stacked Bar of Match Result by Toss Decision",
  ylim = c(0, 100),
  legend.text = rownames(percentages),
  args.legend = list(x = "topright")
)
text(
  x = bp, 
  y = percentages[1,] / 2, 
  labels = paste0(round(percentages[1,], 1), "%"), 
  col = "white", cex = 0.9
)

text(
  x = bp, 
  y = percentages[1,] + percentages[2,] / 2, 
  labels = paste0(round(percentages[2,], 1), "%"), 
  col = "black", cex = 0.9
)
