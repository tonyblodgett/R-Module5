# Importing data
# Read CSV file from current directory
# view data
nfldata <- read.csv("NFL Data Unique Clean.csv ", header = TRUE)
View(nfldata)

# Get mean draft round per conference
# load library
library(dplyr)

# using pipes, filter the data for only those with good draft levels
# add drafted players to new variable
nfldata_draft_only <- (nfldata %>%
  filter(Draft.Round > 0 & Draft.Round < 8) %>%
  select (Conference, Draft.Round))

#Kernel Density Plot
d <- density(nfldata_draft_only$Draft.Round)
windows()
plot(d, main="Kernel Density of Draft Rounds")
polygon(d, col="red", border="blue")


#using pipes
#group players by conference
#summarize by mean and count
View(nfldata_draft_only %>%
  group_by(Conference) %>%
  summarize(mean_draft = mean(Draft.Round, na.rm = TRUE),
            count_rnd = n(),
            min_rnd = min(Draft.Round),
            max_rnd = max(Draft.Round),
            sd = sd(Draft.Round, na.rm = TRUE)))

# Box plots
# ++++++++++++++++++++
# Plot round by Conference
library("ggpubr")
windows()
p <- ggboxplot(nfldata_draft_only, x = "Conference", y = "Draft.Round", 
          ylab = "Round", xlab = "Conference")
p + rotate_x_text(45)


# 1 IV with 2 or more levels use one-way ANOVA
res.aov <- aov(Draft.Round ~ Conference, data = nfldata_draft_only)
summary(res.aov)
