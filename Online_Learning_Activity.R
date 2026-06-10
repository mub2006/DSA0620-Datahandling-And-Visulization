# ============================================================
# SET 9 - Online Learning Activity Analysis
# ============================================================

# First, create the dataset
df <- data.frame(
  Student_ID     = c("L01","L02","L03","L04","L05","L06"),
  Gender         = c("Male","Female","Male","Female","Male","Female"),
  Age            = c(20, 22, 19, 21, 23, 20),
  Course         = c("R","R","SQL","R","R","SQL"),
  Study_Time     = c(3.5, 4.2, 2.0, 5.0, 2.5, 4.0),
  Videos_Watched = c(12, 15, 8, 18, 9, 14),
  Quiz_Score     = c(78, 85, 65, 92, 70, 88),
  Login_Date     = c("2025-01-05","2025-01-05","2025-02-08",
                     "2025-02-08","2025-03-12","2025-03-12"),
  stringsAsFactors = FALSE
)

# OR load from CSV:
# df <- read.csv("Online_Learning_Activity.csv", stringsAsFactors = FALSE)

cat("Dataset:\n")
print(df)


# ============================================================
# QUESTION 1: Histogram & Boxplot of Quiz_Score
# ============================================================

# --- Histogram ---
hist(
  df$Quiz_Score,
  breaks = 5,
  col    = "#4C72B0",
  border = "white",
  main   = "Histogram of Quiz Scores",
  xlab   = "Quiz Score",
  ylab   = "Number of Students",
  xlim   = c(55, 100),
  las    = 1
)
abline(v = mean(df$Quiz_Score),   col = "red",    lwd = 2, lty = 2)
abline(v = median(df$Quiz_Score), col = "orange", lwd = 2, lty = 3)
legend("topleft",
       legend = c(paste("Mean =",   round(mean(df$Quiz_Score), 1)),
                  paste("Median =", round(median(df$Quiz_Score), 1))),
       col = c("red","orange"), lwd = 2, lty = c(2,3), bty = "n"
)

# --- Boxplot by Course ---
boxplot(
  Quiz_Score ~ Course,
  data   = df,
  col    = c("#4C72B0","#DD8452"),
  main   = "Quiz Score by Course",
  xlab   = "Course",
  ylab   = "Quiz Score",
  las    = 1
)
stripchart(
  Quiz_Score ~ Course,
  data   = df,
  method = "jitter",
  add    = TRUE,
  pch    = 19,
  col    = "black",
  cex    = 1.2
)

# Summary
print(summary(df$Quiz_Score))
tapply(df$Quiz_Score, df$Course, summary)

# --- Interpretation ---
# - Scores range from 65 to 92; mean ≈ 79.7, median ≈ 81.5
# - Distribution is slightly left-skewed due to one low score (L03: 65)
# - 4 out of 6 students scored above 75 → satisfactory performance
# - R-course mean (81.2) > SQL-course mean (76.5)
# - SQL has high variance (65 vs 88); R course shows more consistent results


# ============================================================
# QUESTION 2: Bubble Scatter Plot
#             Study_Time vs Quiz_Score, bubble size = Videos_Watched
# ============================================================

# Color by Course
r_col   <- rgb(76/255, 114/255, 176/255, alpha = 0.65)
sql_col <- rgb(221/255, 132/255, 82/255, alpha = 0.65)
point_colors <- ifelse(df$Course == "R", r_col, sql_col)

plot(
  df$Study_Time, df$Quiz_Score,
  type = "n",
  main = "Study Time vs Quiz Score\n(Bubble Size = Videos Watched)",
  xlab = "Study Time (hrs)",
  ylab = "Quiz Score",
  xlim = c(1.5, 5.8),
  ylim = c(58, 100),
  las  = 1
)
grid(col = "gray85", lty = 1)

symbols(
  df$Study_Time, df$Quiz_Score,
  circles = df$Videos_Watched / max(df$Videos_Watched),
  inches  = 0.35,
  add     = TRUE,
  bg      = point_colors,
  fg      = "white",
  lwd     = 1.5
)
text(df$Study_Time, df$Quiz_Score, labels = df$Student_ID,
     cex = 0.75, font = 2)

# Trend line
abline(lm(Quiz_Score ~ Study_Time, data = df),
       col = "firebrick", lwd = 2, lty = 2)

r_val <- cor(df$Study_Time, df$Quiz_Score)
mtext(paste0("Pearson r = ", round(r_val, 2)), side = 3, adj = 1, cex = 0.85)

legend("bottomright",
       legend = c("Course: R","Course: SQL","Trend line"),
       pch    = c(21, 21, NA),
       lty    = c(NA, NA, 2),
       pt.bg  = c(r_col, sql_col),
       pt.cex = 2,
       col    = c("gray30","gray30","firebrick"),
       lwd    = 2, bty = "n", cex = 0.9
)

cat(sprintf("Pearson r (Study_Time vs Quiz_Score) = %.3f\n", r_val))

# --- Interpretation ---
# - Strong positive correlation (r = 0.982): more study time → higher score
# - Larger bubbles (more videos) cluster at top-right (higher scores)
# - L04: 5 hrs, 18 videos, score 92 — best performer
# - L03: 2 hrs,  8 videos, score 65 — needs improvement
# - Female students consistently score higher than male peers
# - Watching more videos and studying longer drives better performance


# ============================================================
# QUESTION 3: Date Conversion, Monthly Average, Line Chart + Moving Average
# ============================================================

# --- Convert to Date ---
df$Login_Date <- as.Date(df$Login_Date, format = "%Y-%m-%d")
cat("Login_Date class:", class(df$Login_Date), "\n")

# --- Extract Year-Month ---
df$YearMonth <- format(df$Login_Date, "%Y-%m")

# --- Monthly Average Quiz Score ---
monthly_avg <- aggregate(Quiz_Score ~ YearMonth, data = df, FUN = mean)
monthly_avg <- monthly_avg[order(monthly_avg$YearMonth), ]
monthly_avg$Month_Num <- seq_len(nrow(monthly_avg))

cat("\nMonthly Average Quiz Scores:\n")
print(monthly_avg)

# --- 2-Point Moving Average ---
monthly_avg$MA2 <- as.numeric(
  stats::filter(monthly_avg$Quiz_Score, rep(1/2, 2), sides = 1)
)

# --- Line Chart ---
plot(
  monthly_avg$Month_Num, monthly_avg$Quiz_Score,
  type = "b",
  pch  = 19, cex = 1.6,
  col  = "#4C72B0",
  lwd  = 2.5,
  main = "Average Quiz Score per Month\n(with 2-Point Moving Average)",
  xlab = "Month",
  ylab = "Average Quiz Score",
  xaxt = "n",
  ylim = c(74, 85),
  las  = 1
)
axis(1, at = monthly_avg$Month_Num, labels = monthly_avg$YearMonth)
grid(nx = NA, ny = NULL, col = "gray85", lty = 1)

# Moving average line
lines(monthly_avg$Month_Num, monthly_avg$MA2,
      col = "#E07B39", lwd = 2.5, lty = 2)
points(monthly_avg$Month_Num, monthly_avg$MA2,
       pch = 17, cex = 1.4, col = "#E07B39")

# Value labels
text(monthly_avg$Month_Num, monthly_avg$Quiz_Score,
     labels = round(monthly_avg$Quiz_Score, 1),
     pos = 3, cex = 0.9, col = "#4C72B0", font = 2)

# Overall trend line
abline(lm(Quiz_Score ~ Month_Num, data = monthly_avg),
       col = "firebrick", lwd = 1.5, lty = 3)

legend("bottomright",
       legend = c("Monthly Average","2-pt Moving Average","Overall Trend"),
       col    = c("#4C72B0","#E07B39","firebrick"),
       lwd    = c(2.5, 2.5, 1.5),
       lty    = c(1, 2, 3),
       pch    = c(19, 17, NA),
       bty    = "n", cex = 0.9
)

# --- Interpretation ---
# - Jan 2025: avg = 81.5 (highest)
# - Feb 2025: avg = 78.5 (dip due to L03's low SQL score of 65)
# - Mar 2025: avg = 79.0 (partial recovery)
# - Overall trend: mild downward drift of ~2 points over 3 months
# - Moving average smooths noise; confirms plateau at ~79 in Feb–Mar
# - High intra-month variance in Feb (range = 27 pts) signals uneven learning

