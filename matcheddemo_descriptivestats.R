### This script produces histograms, means, and SDs for number correct responses, percent correct responses, total degrees off,
### and percent degrees off for VSPLOT 24, VSPLOT 15 derived from 24, and VSPLOT 15. It also colors people differently who have a 
### valid code of "N" (i.e., not valid)
###
### Ellyn Butler
### March 22, 2019 - present

# Load packages
library('ggplot2')
library('gridExtra')
library('prodlim')

# Read in the data
df <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1878.gz.csv")
demo <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1891.gz.csv")
df2 <- merge(df, demo) #, by=c("test_sessions.subid", "test_sessions_v.battery", "test_sessions_v.dotest", "test_sessions.datasetid"))

# Remove invalid data ("N")
df2 <- df2[df2$test_sessions_v.valid_code != "N",]

# Prepare dataframes
df_1515 <- df2[!(is.na(df2$VSPLOT15.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions_v.dotest", "test_sessions.datasetid", "test_sessions_v.battery", "test_sessions_v.valid_code", "VSPLOT15.VSPLOT15_CR", "VSPLOT15.VSPLOT15_SUM_DEG_OFF", "VSPLOT15.VSPLOT15_RTCR", "test_sessions_v.age", "test_sessions_v.education", "test_sessions_v.feducation", "test_sessions_v.gender", "test_sessions_v.handedness", "test_sessions_v.meducation")]
df_2424 <- df2[!(is.na(df2$VSPLOT24.VSPLOT24_CR)), c("test_sessions.subid", "test_sessions_v.dotest", "test_sessions.datasetid", "test_sessions_v.battery", "test_sessions_v.valid_code", "VSPLOT24.VSPLOT24_CR", "VSPLOT24.VSPLOT24_SUM_DEG_OFF", "VSPLOT24.VSPLOT24_RTCR", "test_sessions_v.age", "test_sessions_v.education", "test_sessions_v.feducation", "test_sessions_v.gender", "test_sessions_v.handedness", "test_sessions_v.meducation")]
df_2415 <- df2[!(is.na(df2$VSPLOT24.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions_v.dotest", "test_sessions.datasetid", "test_sessions_v.battery", "test_sessions_v.valid_code", "VSPLOT24.VSPLOT15_CR", "VSPLOT24.VSPLOT15_SUM_DEG_OFF", "VSPLOT24.VSPLOT15_RTCR", "test_sessions_v.age", "test_sessions_v.education", "test_sessions_v.feducation", "test_sessions_v.gender", "test_sessions_v.handedness", "test_sessions_v.meducation")]

#df_1515$CollectionStatus <- "Original"
#df_2415$CollectionStatus <- "Derived"

colnames(df_1515) <- c("subid", "dotest", "datasetid", "battery", "valid_code", "CR", "SUM_DEG_OFF", "MED_RESP_TIME", "Age", "Edu", "FatherEdu", "Gender", "Handedness", "MotherEdu") #, "CollectionStatus")
colnames(df_2415) <- c("subid", "dotest", "datasetid", "battery", "valid_code", "CR", "SUM_DEG_OFF", "MED_RESP_TIME", "Age", "Edu", "FatherEdu", "Gender", "Handedness", "MotherEdu") #, "CollectionStatus")
colnames(df_2424) <- c("subid", "dotest", "datasetid", "battery", "valid_code", "CR", "SUM_DEG_OFF", "MED_RESP_TIME", "Age", "Edu", "FatherEdu", "Gender", "Handedness", "MotherEdu")

rownames(df_1515) <- 1:nrow(df_1515)
rownames(df_2415) <- 1:nrow(df_2415)
rownames(df_2424) <- 1:nrow(df_2424)

# Match subjects on "test_sessions_v.age", "test_sessions_v.education", "test_sessions_v.feducation", "test_sessions_v.gender", "test_sessions_v.handedness", "test_sessions_v.meducation", "test_sessions_v.wrat"
alldemo <- row.match(df_1515[, !(colnames(df_1515) %in% c("subid", "dotest", "datasetid", "battery", "CR", "SUM_DEG_OFF", "MED_RESP_TIME"))], df_2415[, !(colnames(df_2415) %in% c("subid", "dotest", "datasetid", "battery", "CR", "SUM_DEG_OFF", "MED_RESP_TIME"))])
somedemo <- row.match(df_1515[, !(colnames(df_1515) %in% c("subid", "dotest", "datasetid", "battery", "CR", "SUM_DEG_OFF", "MED_RESP_TIME", "FatherEdu", "Handedness", "MotherEdu"))], df_2415[, !(colnames(df_2415) %in% c("subid", "dotest", "datasetid", "battery", "CR", "SUM_DEG_OFF", "MED_RESP_TIME", "FatherEdu", "Handedness", "MotherEdu"))])


# --------------------------------------------- alldemo --------------------------------------------- #

df_1515_alldemo <- df_1515[!(is.na(alldemo)),]
df_2415_alldemo <- df_2415[alldemo[!(is.na(alldemo))], ]

df_2424_rows <- row.match(df_2415_alldemo[,c("subid", "dotest", "datasetid", "battery")], df_2424[,c("subid", "dotest", "datasetid", "battery")])
df_2424_alldemo <- df_2424[df_2424_rows, ]

# Plot number correct
m_CR_1515 <- round(mean(df_1515_alldemo$CR), digits=2)
sd_CR_1515 <- round(sd(df_1515_alldemo$CR), digits=2)
N_CR_1515 <- nrow(df_1515_alldemo)
sub_CR_1515 <- paste0("(Mean = ", m_CR_1515, ", SD = ", sd_CR_1515, ", N = ", N_CR_1515, ")")
p_CR_1515 <- ggplot(df_1515_alldemo, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="violetred4", fill="violetred3", bins=(max(df_1515_alldemo$CR)-min(df_1515_alldemo$CR)+1)) + 
	labs(title="Total Correct for VSPLOT 15 (Original: All Demo)",x="# Correct", y = "# of Participants", subtitle=sub_CR_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_CR_2415 <- round(mean(df_2415_alldemo$CR), digits=2)
sd_CR_2415 <- round(sd(df_2415_alldemo$CR), digits=2)
N_CR_2415 <- nrow(df_2415_alldemo)
sub_CR_2415 <- paste0("(Mean = ", m_CR_2415, ", SD = ", sd_CR_2415, ", N = ", N_CR_2415, ")")
p_CR_2415 <- ggplot(df_2415_alldemo, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="steelblue4", fill="steelblue3", bins=(max(df_2415_alldemo$CR)-min(df_2415_alldemo$CR)+1)) + 
	labs(title="Total Correct for VSPLOT 15 (Derived: All Demo)",x="# Correct", y = "# of Participants", subtitle=sub_CR_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_CR_2424 <- round(mean(df_2424_alldemo$CR), digits=2)
sd_CR_2424 <- round(sd(df_2424_alldemo$CR), digits=2)
N_CR_2424 <- nrow(df_2424_alldemo)
sub_CR_2424 <- paste0("(Mean = ", m_CR_2424, ", SD = ", sd_CR_2424, ", N = ", N_CR_2424, ")")
p_CR_2424 <- ggplot(df_2424_alldemo, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="darkseagreen4", fill="darkseagreen3", bins=(max(df_2424_alldemo$CR)-min(df_2424_alldemo$CR)+1)) + 
	labs(title="Total Correct for VSPLOT 24 (All Demo)",x="# Correct", y = "# of Participants", subtitle=sub_CR_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot percent correct
m_per_1515 <- 100*round(mean(df_1515_alldemo$CR/15), digits=3)
sd_per_1515 <- 100*round(sd(df_1515_alldemo$CR/15), digits=3)
N_per_1515 <- nrow(df_1515_alldemo)
df_1515_alldemo$per_CR <- 100*(df_1515_alldemo$CR/15)
sub_per_1515 <- paste0("(Mean = ", m_per_1515, ", SD = ", sd_per_1515, ", N = ", N_per_1515, ")")
p_per_1515 <- ggplot(df_1515_alldemo, aes(x=per_CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="violetred4", fill="violetred3", bins=(max(df_1515_alldemo$CR)-min(df_1515_alldemo$CR)+1)) + 
	labs(title="Percent Correct for VSPLOT 15 (Original: All Demo)",x="% Correct", y = "# of Participants", subtitle=sub_per_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_per_2415 <- 100*round(mean(df_2415_alldemo$CR/15), digits=3)
sd_per_2415 <- 100*round(sd(df_2415_alldemo$CR/15), digits=3)
N_per_2415 <- nrow(df_2415_alldemo)
df_2415_alldemo$per_CR <- 100*(df_2415_alldemo$CR/15)
sub_per_2415 <- paste0("(Mean = ", m_per_2415, ", SD = ", sd_per_2415, ", N = ", N_per_2415, ")")
p_per_2415 <- ggplot(df_2415_alldemo, aes(x=per_CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="steelblue4", fill="steelblue3", bins=(max(df_2415_alldemo$CR)-min(df_2415_alldemo$CR)+1)) + 
	labs(title="Percent Correct for VSPLOT 15 (Derived: All Demo)",x="% Correct", y = "# of Participants", subtitle=sub_per_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_per_2424 <- 100*round(mean(df_2424_alldemo$CR/24), digits=3)
sd_per_2424 <- 100*round(sd(df_2424_alldemo$CR/24), digits=3)
N_per_2424 <- nrow(df_2424_alldemo)
df_2424_alldemo$per_CR <- 100*(df_2424_alldemo$CR/24)
sub_per_2424 <- paste0("(Mean = ", m_per_2424, ", SD = ", sd_per_2424, ", N = ", N_per_2424, ")")
p_per_2424 <- ggplot(df_2424_alldemo, aes(x=per_CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="darkseagreen4", fill="darkseagreen3", bins=(max(df_2424_alldemo$CR)-min(df_2424_alldemo$CR)+1)) + 
	labs(title="Percent Correct for VSPLOT 24 (All Demo)",x="% Correct", y = "# of Participants", subtitle=sub_per_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot total degrees off
m_degoff_1515 <- round(mean(df_1515_alldemo$SUM_DEG_OFF), digits=2)
sd_degoff_1515 <- round(sd(df_1515_alldemo$SUM_DEG_OFF), digits=2)
N_degoff_1515 <- nrow(df_1515_alldemo)
sub_degoff_1515 <- paste0("(Mean = ", m_degoff_1515, ", SD = ", sd_degoff_1515, ", N = ", N_degoff_1515, ")")
p_degoff_1515 <- ggplot(df_1515_alldemo, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="violetred4", fill="violetred3") + 
	labs(title="Degrees Off for VSPLOT 15 (Original: All Demo)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_degoff_2415 <- round(mean(df_2415_alldemo$SUM_DEG_OFF), digits=2)
sd_degoff_2415 <- round(sd(df_2415_alldemo$SUM_DEG_OFF), digits=2)
N_degoff_2415 <- nrow(df_2415_alldemo)
sub_degoff_2415 <- paste0("(Mean = ", m_degoff_2415, ", SD = ", sd_degoff_2415, ", N = ", N_degoff_2415, ")")
p_degoff_2415 <- ggplot(df_2415_alldemo, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="steelblue4", fill="steelblue3") + 
	labs(title="Degrees Off for VSPLOT 15 (Derived: All Demo)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_degoff_2424 <- round(mean(df_2424_alldemo$SUM_DEG_OFF), digits=2)
sd_degoff_2424 <- round(sd(df_2424_alldemo$SUM_DEG_OFF), digits=2)
N_degoff_2424 <- nrow(df_2424_alldemo)
sub_degoff_2424 <- paste0("(Mean = ", m_degoff_2424, ", SD = ", sd_degoff_2424, ", N = ", N_degoff_2424, ")")
p_degoff_2424 <- ggplot(df_2424_alldemo, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=22, colour="darkseagreen4", fill="darkseagreen3") + 
	labs(title="Degrees Off for VSPLOT 24 (All Demo)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot speed
m_medtime_1515 <- round(mean(df_1515_alldemo[!(is.na(df_1515_alldemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_1515 <- round(sd(df_1515_alldemo[!(is.na(df_1515_alldemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_1515 <- nrow(df_1515_alldemo[!(is.na(df_1515_alldemo$MED_RESP_TIME)),])
sub_medtime_1515 <- paste0("(Mean = ", m_medtime_1515, ", SD = ", sd_medtime_1515, ", N = ", N_medtime_1515, ")")
p_medtime_1515 <- ggplot(df_1515_alldemo[!(is.na(df_1515_alldemo$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="violetred4", fill="violetred3") + 
	labs(title="Median Resp. Time for VSPLOT 15 (Original: All Demo)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_medtime_2415 <- round(mean(df_2415_alldemo[!(is.na(df_2415_alldemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_2415 <- round(sd(df_2415_alldemo[!(is.na(df_2415_alldemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_2415 <- nrow(df_2415_alldemo[!(is.na(df_2415_alldemo$MED_RESP_TIME)),])
sub_medtime_2415 <- paste0("(Mean = ", m_medtime_2415, ", SD = ", sd_medtime_2415, ", N = ", N_medtime_2415, ")")
p_medtime_2415 <- ggplot(df_2415_alldemo[!(is.na(df_2415_alldemo$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="steelblue4", fill="steelblue3") + 
	labs(title="Median Resp. Time for VSPLOT 15 (Derived: All Demo)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_medtime_2424 <- round(mean(df_2424_alldemo[!(is.na(df_2424_alldemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_2424 <- round(sd(df_2424_alldemo[!(is.na(df_2424_alldemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_2424 <- nrow(df_2424_alldemo[!(is.na(df_2424_alldemo$MED_RESP_TIME)),])
sub_medtime_2424 <- paste0("(Mean = ", m_medtime_2424, ", SD = ", sd_medtime_2424, ", N = ", N_medtime_2424, ")")
p_medtime_2424 <- ggplot(df_2424_alldemo[!(is.na(df_2424_alldemo$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=22, colour="darkseagreen4", fill="darkseagreen3") + 
	labs(title="Median Resp. Time for VSPLOT 24 (All Demo)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))



# Return pdf
pdf("/home/butellyn/plot_validation/plots/distributions_CRdegoffmedtime_alldemo.pdf", width=18, height=5)
grid.arrange(p_CR_1515, p_CR_2415, p_CR_2424, ncol=3)
grid.arrange(p_per_1515, p_per_2415, p_per_2424, ncol=3)
grid.arrange(p_degoff_1515, p_degoff_2415, p_degoff_2424, ncol=3)
grid.arrange(p_medtime_1515, p_medtime_2415, p_medtime_2424, ncol=3)
dev.off()


# --------------------------------------------- somedemo --------------------------------------------- #

df_1515_somedemo <- df_1515[!(is.na(somedemo)),]
df_2415_somedemo <- df_2415[somedemo[!(is.na(somedemo))], ]

df_2424_rows <- row.match(df_2415_somedemo[,c("subid", "dotest", "datasetid", "battery")], df_2424[,c("subid", "dotest", "datasetid", "battery")])
df_2424_somedemo <- df_2424[df_2424_rows, ]

# Plot number correct
m_CR_1515 <- round(mean(df_1515_somedemo$CR), digits=2)
sd_CR_1515 <- round(sd(df_1515_somedemo$CR), digits=2)
N_CR_1515 <- nrow(df_1515_somedemo)
sub_CR_1515 <- paste0("(Mean = ", m_CR_1515, ", SD = ", sd_CR_1515, ", N = ", N_CR_1515, ")")
p_CR_1515 <- ggplot(df_1515_somedemo, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="violetred4", fill="violetred3", bins=(max(df_1515_somedemo$CR)-min(df_1515_somedemo$CR)+1)) + 
	labs(title="Total Correct for VSPLOT 15 (Original: Some Demo)",x="# Correct", y = "# of Participants", subtitle=sub_CR_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_CR_2415 <- round(mean(df_2415_somedemo$CR), digits=2)
sd_CR_2415 <- round(sd(df_2415_somedemo$CR), digits=2)
N_CR_2415 <- nrow(df_2415_somedemo)
sub_CR_2415 <- paste0("(Mean = ", m_CR_2415, ", SD = ", sd_CR_2415, ", N = ", N_CR_2415, ")")
p_CR_2415 <- ggplot(df_2415_somedemo, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="steelblue4", fill="steelblue3", bins=(max(df_2415_somedemo$CR)-min(df_2415_somedemo$CR)+1)) + 
	labs(title="Total Correct for VSPLOT 15 (Derived: Some Demo)",x="# Correct", y = "# of Participants", subtitle=sub_CR_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_CR_2424 <- round(mean(df_2424_somedemo$CR), digits=2)
sd_CR_2424 <- round(sd(df_2424_somedemo$CR), digits=2)
N_CR_2424 <- nrow(df_2424_somedemo)
sub_CR_2424 <- paste0("(Mean = ", m_CR_2424, ", SD = ", sd_CR_2424, ", N = ", N_CR_2424, ")")
p_CR_2424 <- ggplot(df_2424_somedemo, aes(x=CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="darkseagreen4", fill="darkseagreen3", bins=(max(df_2424_somedemo$CR)-min(df_2424_somedemo$CR)+1)) + 
	labs(title="Total Correct for VSPLOT 24 (Some Demo)",x="# Correct", y = "# of Participants", subtitle=sub_CR_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot percent correct
m_per_1515 <- 100*round(mean(df_1515_somedemo$CR/15), digits=3)
sd_per_1515 <- 100*round(sd(df_1515_somedemo$CR/15), digits=3)
N_per_1515 <- nrow(df_1515_somedemo)
df_1515_somedemo$per_CR <- 100*(df_1515_somedemo$CR/15)
sub_per_1515 <- paste0("(Mean = ", m_per_1515, ", SD = ", sd_per_1515, ", N = ", N_per_1515, ")")
p_per_1515 <- ggplot(df_1515_somedemo, aes(x=per_CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="violetred4", fill="violetred3", bins=(max(df_1515_somedemo$CR)-min(df_1515_somedemo$CR)+1)) + 
	labs(title="Percent Correct for VSPLOT 15 (Original: Some Demo)",x="% Correct", y = "# of Participants", subtitle=sub_per_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_per_2415 <- 100*round(mean(df_2415_somedemo$CR/15), digits=3)
sd_per_2415 <- 100*round(sd(df_2415_somedemo$CR/15), digits=3)
N_per_2415 <- nrow(df_2415_somedemo)
df_2415_somedemo$per_CR <- 100*(df_2415_somedemo$CR/15)
sub_per_2415 <- paste0("(Mean = ", m_per_2415, ", SD = ", sd_per_2415, ", N = ", N_per_2415, ")")
p_per_2415 <- ggplot(df_2415_somedemo, aes(x=per_CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="steelblue4", fill="steelblue3", bins=(max(df_2415_somedemo$CR)-min(df_2415_somedemo$CR)+1)) + 
	labs(title="Percent Correct for VSPLOT 15 (Derived: Some Demo)",x="% Correct", y = "# of Participants", subtitle=sub_per_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_per_2424 <- 100*round(mean(df_2424_somedemo$CR/24), digits=3)
sd_per_2424 <- 100*round(sd(df_2424_somedemo$CR/24), digits=3)
N_per_2424 <- nrow(df_2424_somedemo)
df_2424_somedemo$per_CR <- 100*(df_2424_somedemo$CR/24)
sub_per_2424 <- paste0("(Mean = ", m_per_2424, ", SD = ", sd_per_2424, ", N = ", N_per_2424, ")")
p_per_2424 <- ggplot(df_2424_somedemo, aes(x=per_CR)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, colour="darkseagreen4", fill="darkseagreen3", bins=(max(df_2424_somedemo$CR)-min(df_2424_somedemo$CR)+1)) + 
	labs(title="Percent Correct for VSPLOT 24 (Some Demo)",x="% Correct", y = "# of Participants", subtitle=sub_per_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot total degrees off
m_degoff_1515 <- round(mean(df_1515_somedemo$SUM_DEG_OFF), digits=2)
sd_degoff_1515 <- round(sd(df_1515_somedemo$SUM_DEG_OFF), digits=2)
N_degoff_1515 <- nrow(df_1515_somedemo)
sub_degoff_1515 <- paste0("(Mean = ", m_degoff_1515, ", SD = ", sd_degoff_1515, ", N = ", N_degoff_1515, ")")
p_degoff_1515 <- ggplot(df_1515_somedemo, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="violetred4", fill="violetred3") + 
	labs(title="Degrees Off for VSPLOT 15 (Original: Some Demo)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_degoff_2415 <- round(mean(df_2415_somedemo$SUM_DEG_OFF), digits=2)
sd_degoff_2415 <- round(sd(df_2415_somedemo$SUM_DEG_OFF), digits=2)
N_degoff_2415 <- nrow(df_2415_somedemo)
sub_degoff_2415 <- paste0("(Mean = ", m_degoff_2415, ", SD = ", sd_degoff_2415, ", N = ", N_degoff_2415, ")")
p_degoff_2415 <- ggplot(df_2415_somedemo, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="steelblue4", fill="steelblue3") + 
	labs(title="Degrees Off for VSPLOT 15 (Derived: Some Demo)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_degoff_2424 <- round(mean(df_2424_somedemo$SUM_DEG_OFF), digits=2)
sd_degoff_2424 <- round(sd(df_2424_somedemo$SUM_DEG_OFF), digits=2)
N_degoff_2424 <- nrow(df_2424_somedemo)
sub_degoff_2424 <- paste0("(Mean = ", m_degoff_2424, ", SD = ", sd_degoff_2424, ", N = ", N_degoff_2424, ")")
p_degoff_2424 <- ggplot(df_2424_somedemo, aes(x=SUM_DEG_OFF)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=22, colour="darkseagreen4", fill="darkseagreen3") + 
	labs(title="Degrees Off for VSPLOT 24 (Some Demo)",x="Degrees Off", y = "# of Participants", subtitle=sub_degoff_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

# Plot speed
m_medtime_1515 <- round(mean(df_1515_somedemo[!(is.na(df_1515_somedemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_1515 <- round(sd(df_1515_somedemo[!(is.na(df_1515_somedemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_1515 <- nrow(df_1515_somedemo[!(is.na(df_1515_somedemo$MED_RESP_TIME)),])
sub_medtime_1515 <- paste0("(Mean = ", m_medtime_1515, ", SD = ", sd_medtime_1515, ", N = ", N_medtime_1515, ")")
p_medtime_1515 <- ggplot(df_1515_somedemo[!(is.na(df_1515_somedemo$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="violetred4", fill="violetred3") + 
	labs(title="Median Resp. Time for VSPLOT 15 (Original: Some Demo)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_1515) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_medtime_2415 <- round(mean(df_2415_somedemo[!(is.na(df_2415_somedemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_2415 <- round(sd(df_2415_somedemo[!(is.na(df_2415_somedemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_2415 <- nrow(df_2415_somedemo[!(is.na(df_2415_somedemo$MED_RESP_TIME)),])
sub_medtime_2415 <- paste0("(Mean = ", m_medtime_2415, ", SD = ", sd_medtime_2415, ", N = ", N_medtime_2415, ")")
p_medtime_2415 <- ggplot(df_2415_somedemo[!(is.na(df_2415_somedemo$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=16, colour="steelblue4", fill="steelblue3") + 
	labs(title="Median Resp. Time for VSPLOT 15 (Derived: Some Demo)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_2415) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

m_medtime_2424 <- round(mean(df_2424_somedemo[!(is.na(df_2424_somedemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
sd_medtime_2424 <- round(sd(df_2424_somedemo[!(is.na(df_2424_somedemo$MED_RESP_TIME)),"MED_RESP_TIME"]), digits=2)
N_medtime_2424 <- nrow(df_2424_somedemo[!(is.na(df_2424_somedemo$MED_RESP_TIME)),])
sub_medtime_2424 <- paste0("(Mean = ", m_medtime_2424, ", SD = ", sd_medtime_2424, ", N = ", N_medtime_2424, ")")
p_medtime_2424 <- ggplot(df_2424_somedemo[!(is.na(df_2424_somedemo$MED_RESP_TIME)),], aes(x=MED_RESP_TIME)) + theme_minimal() +
	geom_histogram(position="identity", alpha=0.5, bins=22, colour="darkseagreen4", fill="darkseagreen3") + 
	labs(title="Median Resp. Time for VSPLOT 24 (Some Demo)",x="Median Response Time (ms)", y = "# of Participants", subtitle=sub_medtime_2424) +
	theme(plot.title = element_text(size=15), axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))



# Return pdf
pdf("/home/butellyn/plot_validation/plots/distributions_CRdegoffmedtime_somedemo.pdf", width=18, height=5)
grid.arrange(p_CR_1515, p_CR_2415, p_CR_2424, ncol=3)
grid.arrange(p_per_1515, p_per_2415, p_per_2424, ncol=3)
grid.arrange(p_degoff_1515, p_degoff_2415, p_degoff_2424, ncol=3)
grid.arrange(p_medtime_1515, p_medtime_2415, p_medtime_2424, ncol=3)
dev.off()



