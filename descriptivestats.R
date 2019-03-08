### This script produces histograms, means, and SDs for number correct responses, percent correct responses, total degrees off,
### and percent degrees off for VSPLOT 24, VSPLOT 15 derived from 24, and VSPLOT 15. It also colors people differently who have a 
### valid code of "N" (i.e., not valid)
###
### Ellyn Butler
### March 6, 2019

# Load packages
library('ggplot2')
library('gridExtra')

# Read in the data
df <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1878.gz.csv")
demo <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1891.gz.csv")

# Plot total number correct (VSPLOT15.VSPLOT15_CR, VSPLOT24.VSPLOT24_CR, VSPLOT24.VSPLOT15_CR)
df_1515 <- df[!(is.na(df$VSPLOT15.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions_v.valid_code", "VSPLOT15.VSPLOT15_CR", "VSPLOT15.VSPLOT15_SUM_DEG_OFF")]
df_2424 <- df[!(is.na(df$VSPLOT24.VSPLOT24_CR)), c("test_sessions.subid", "test_sessions_v.valid_code", "VSPLOT24.VSPLOT24_CR", "VSPLOT24.VSPLOT24_SUM_DEG_OFF")]
df_2415 <- df[!(is.na(df$VSPLOT24.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions_v.valid_code", "VSPLOT24.VSPLOT15_CR", "VSPLOT24.VSPLOT15_SUM_DEG_OFF")]

df_1515$CollectionStatus <- "Original"
df_2415$CollectionStatus <- "Derived"

colnames(df_1515) <- c("subid", "valid_code", "CR", "SUM_DEG_OFF", "CollectionStatus")
colnames(df_2415) <- c("subid", "valid_code", "CR", "SUM_DEG_OFF", "CollectionStatus")
colnames(df_2424) <- c("subid", "valid_code", "CR", "SUM_DEG_OFF")

df_15 <- rbind(df_1515, df_2415)
df_15$CollectionStatus <- factor(df_15$CollectionStatus)

p_CR_15 <- ggplot(df_15, aes(x=CR, fill=CollectionStatus, color=CollectionStatus)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16) + 
	labs(title="Total Correct for VSPLOT 15",x="# Correct", y = "# of Participants") +
	theme(plot.title = element_text(size=12), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

p_24 <- 
