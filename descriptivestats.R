### This script produces histograms, means, and SDs for number correct responses, percent correct responses, total degrees off,
### and percent degrees off for VSPLOT 24, VSPLOT 15 derived from 24, and VSPLOT 15. It also colors people differently who have a 
### valid code of "N" (i.e., not valid)
###
### Ellyn Butler
### March 6, 2019 - present

# Load packages
library('ggplot2')
library('gridExtra')

# Read in the data
df <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1878.gz.csv")
demo <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1891.gz.csv")

# Remove invalid data ("N")
df <- df[df$test_sessions_v.valid_code != "N",]

# Prepare dataframes
df_1515 <- df[!(is.na(df$VSPLOT15.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions_v.valid_code", "VSPLOT15.VSPLOT15_CR", "VSPLOT15.VSPLOT15_SUM_DEG_OFF", "VSPLOT15.VSPLOT15_RTCR")]
df_2424 <- df[!(is.na(df$VSPLOT24.VSPLOT24_CR)), c("test_sessions.subid", "test_sessions_v.valid_code", "VSPLOT24.VSPLOT24_CR", "VSPLOT24.VSPLOT24_SUM_DEG_OFF", "VSPLOT24.VSPLOT24_RTCR")]
df_2415 <- df[!(is.na(df$VSPLOT24.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions_v.valid_code", "VSPLOT24.VSPLOT15_CR", "VSPLOT24.VSPLOT15_SUM_DEG_OFF", "VSPLOT24.VSPLOT15_RTCR")]

#df_1515$CollectionStatus <- "Original"
#df_2415$CollectionStatus <- "Derived"

colnames(df_1515) <- c("subid", "valid_code", "CR", "SUM_DEG_OFF", "MED_RESP_TIME") #, "CollectionStatus")
colnames(df_2415) <- c("subid", "valid_code", "CR", "SUM_DEG_OFF", "MED_RESP_TIME") #, "CollectionStatus")
colnames(df_2424) <- c("subid", "valid_code", "CR", "SUM_DEG_OFF", "MED_RESP_TIME")

#df_15 <- rbind(df_1515, df_2415)
#df_15$CollectionStatus <- factor(df_15$CollectionStatus)

# Plot number correct
m_CR_1515 <- round(mean(df_1515$CR), digits=2)
sd_CR_1515 <- round(sd(df_1515$CR), digits=2)
N_CR_1515 <- nrow(df_1515)
sub_CR_1515 <- paste0("(Mean = ", m_CR_1515, ", SD = ", sd_CR_1515, ", N = ", N_CR_1515, ")")
p_CR_1515 <- ggplot(df_1515, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="violetred4", fill="violetred3") + 
	labs(title="Total Correct for VSPLOT 15 (Original)",x="# Correct", y = "# of Participants", subtitle=sub_CR_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_CR_2415 <- round(mean(df_2415$CR), digits=2)
sd_CR_2415 <- round(sd(df_2415$CR), digits=2)
N_CR_2415 <- nrow(df_2415)
sub_CR_2415 <- paste0("(Mean = ", m_CR_2415, ", SD = ", sd_CR_2415, ", N = ", N_CR_2415, ")")
p_CR_2415 <- ggplot(df_2415, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="steelblue4", fill="steelblue3") + 
	labs(title="Total Correct for VSPLOT 15 (Derived)",x="# Correct", y = "# of Participants", subtitle=sub_CR_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_CR_2424 <- round(mean(df_2424$CR), digits=2)
sd_CR_2424 <- round(sd(df_2424$CR), digits=2)
N_CR_2424 <- nrow(df_2424)
sub_CR_2424 <- paste0("(Mean = ", m_CR_2424, ", SD = ", sd_CR_2424, ", N = ", N_CR_2424, ")")
p_CR_2424 <- ggplot(df_2424, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=22, colour="darkseagreen4", fill="darkseagreen3") + 
	labs(title="Total Correct for VSPLOT 24",x="# Correct", y = "# of Participants", subtitle=sub_CR_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot total degrees off
m_degoff_1515 <- round(mean(df_1515$SUM_DEG_OFF), digits=2)
sd_degoff_1515 <- round(sd(df_1515$SUM_DEG_OFF), digits=2)
N_degoff_1515 <- nrow(df_1515)
sub_degoff_1515 <- paste0("(Mean = ", m_degoff_1515, ", SD = ", sd_degoff_1515, ", N = ", N_degoff_1515, ")")
p_degoff_1515 <- ggplot(df_1515, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="violetred4", fill="violetred3") + 
	labs(title="Degrees Off for VSPLOT 15 (Original)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_degoff_2415 <- round(mean(df_2415$SUM_DEG_OFF), digits=2)
sd_degoff_2415 <- round(sd(df_2415$SUM_DEG_OFF), digits=2)
N_degoff_2415 <- nrow(df_2415)
sub_degoff_2415 <- paste0("(Mean = ", m_degoff_2415, ", SD = ", sd_degoff_2415, ", N = ", N_degoff_2415, ")")
p_degoff_2415 <- ggplot(df_2415, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="steelblue4", fill="steelblue3") + 
	labs(title="Degrees Off for VSPLOT 15 (Derived)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_degoff_2424 <- round(mean(df_2424$SUM_DEG_OFF), digits=2)
sd_degoff_2424 <- round(sd(df_2424$SUM_DEG_OFF), digits=2)
N_degoff_2424 <- nrow(df_2424)
sub_degoff_2424 <- paste0("(Mean = ", m_degoff_2424, ", SD = ", sd_degoff_2424, ", N = ", N_degoff_2424, ")")
p_degoff_2424 <- ggplot(df_2424, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=22, colour="darkseagreen4", fill="darkseagreen3") + 
	labs(title="Degrees Off for VSPLOT 24",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot speed
m_medtime_1515 <- round(mean(df_1515[!(is.na(df_1515$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_1515 <- round(sd(df_1515[!(is.na(df_1515$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_1515 <- nrow(df_1515[!(is.na(df_1515$MED_RESP_TIME)),])
sub_medtime_1515 <- paste0("(Mean = ", m_medtime_1515, ", SD = ", sd_medtime_1515, ", N = ", N_medtime_1515, ")")
p_medtime_1515 <- ggplot(df_1515[!(is.na(df_1515$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="violetred4", fill="violetred3") + 
	labs(title="Median Resp. Time for VSPLOT 15 (Original)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_medtime_2415 <- round(mean(df_2415[!(is.na(df_2415$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_2415 <- round(sd(df_2415[!(is.na(df_2415$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_2415 <- nrow(df_2415[!(is.na(df_2415$MED_RESP_TIME)),])
sub_medtime_2415 <- paste0("(Mean = ", m_medtime_2415, ", SD = ", sd_medtime_2415, ", N = ", N_medtime_2415, ")")
p_medtime_2415 <- ggplot(df_2415[!(is.na(df_2415$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="steelblue4", fill="steelblue3") + 
	labs(title="Median Resp. Time for VSPLOT 15 (Derived)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_medtime_2424 <- round(mean(df_2424[!(is.na(df_2424$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_2424 <- round(sd(df_2424[!(is.na(df_2424$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_2424 <- nrow(df_2424[!(is.na(df_2424$MED_RESP_TIME)),])
sub_medtime_2424 <- paste0("(Mean = ", m_medtime_2424, ", SD = ", sd_medtime_2424, ", N = ", N_medtime_2424, ")")
p_medtime_2424 <- ggplot(df_2424[!(is.na(df_2424$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=22, colour="darkseagreen4", fill="darkseagreen3") + 
	labs(title="Median Resp. Time for VSPLOT 24",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))



# Return pdf
pdf("/home/butellyn/plot_validation/plots/distributions_CRdegoffmedtime.pdf", width=18, height=5)
grid.arrange(p_CR_1515, p_CR_2415, p_CR_2424, ncol=3)
grid.arrange(p_degoff_1515, p_degoff_2415, p_degoff_2424, ncol=3)
grid.arrange(p_medtime_1515, p_medtime_2415, p_medtime_2424, ncol=3)
dev.off()



