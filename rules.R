### This script produces plots to show how Adam Savitt's validation code transfers to the VSPLOT 15
###
### Ellyn Butler
### April 2, 2019 - present

# Load packages
library('ggplot2')
library('gridExtra')
library('prodlim')
library('psych')
library('PerFit')
library('dplyr')

# Read in the data
df <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1878.gz.csv")
demo <- read.csv("/home/analysis/psycha1/cnb_validation/3360_1891.gz.csv")
df2 <- merge(df, demo)

# Remove invalid data ("N")
df2 <- df2[df2$VSPLOT24.valid_code != "N",]
df2 <- df2[df2$VSPLOT15.valid_code != "N",]

##### ----------------------------------------------- #####
corrcols <- grep('_CORR',colnames(df2), value=TRUE)
rtcols <- grep('_RT_',colnames(df2), value=TRUE)
PLOT <- df2[!(is.na(df2$VSPLOT24.VSPLOT15_CR)) ,c("test_sessions.datasetid", "test_sessions.siteid", "test_sessions.famid", "test_sessions.subid", "test_sessions.bblid", "test_sessions_v.battery", "test_sessions_v.dotest", corrcols, rtcols, "VSPLOT24.VSPLOT15_CR")]

# Check that each row has unique identifiers
PLOT$concatIDs <- NA
for (i in 1:nrow(PLOT)) {
	catID <- paste0(PLOT[i, "test_sessions.datasetid"], PLOT[i, "test_sessions.siteid"], PLOT[i, "test_sessions.famid"], PLOT[i, "test_sessions.subid"], PLOT[i, "test_sessions.bblid"], PLOT[i, "test_sessions_v.battery"], PLOT[i, "test_sessions_v.dotest"])
	PLOT[i, "concatIDs"] <- catID
}

rownames(PLOT) <- 1:nrow(PLOT)
if (!(TRUE %in% PLOT$concatIDs)) {
	perfrows <- as.numeric(rownames(PLOT[PLOT$VSPLOT24.VSPLOT15_CR == 15,]))
	items <- 15
	dat <- PLOT[,8:37]
	dat[,(items+1):(2*items)] <- log(dat[,(items+1):(2*items)])

	res <- matrix(NA,dim(dat)[1],items)

	for (j in 1:items) {
		mod <- lm(dat[,(j+items)]~dat[,j],data=dat,na.action=na.exclude)
		res[,j] <- scale(residuals(mod,na.action=na.exclude))
	}
	res2 <- res
	res2[abs(res2) < 2] <- 0
	res2[abs(res2) > 2] <- 1

	outlier_score_2cut <- 1 - rowMeans(res2,na.rm=TRUE)

	dat2 <- dat[,1:items]
	acc3e <- rowMeans(dat2[,colMeans(dat2,na.rm=TRUE) >= min(tail(sort(colMeans(dat2,na.rm=TRUE)),3))])

	pfit1 <- r.pbis(dat2)$PFscores[,1]
	pfit2 <- E.KB(dat2)$PFscores[,1]

	# Mean-Impute Person Fit scores
	pfit1[perfrows] <- 1
	pfit2[perfrows] <- 1
	pfit1[is.na(pfit1)] <- mean(pfit1, na.rm = TRUE)
	pfit2[is.na(pfit2)] <- mean(pfit2, na.rm = TRUE)

	sc <- (0.33*outlier_score_2cut) + (0.15*acc3e) + (0.20*pfit1) + (0.32*pfit2)

	PLOT <- data.frame(PLOT[,1:7],sc,outlier_score_2cut,acc3e,pfit1,pfit2)
	colnames(PLOT) <- c("test_sessions.datasetid", "test_sessions.siteid", "test_sessions.famid", "test_sessions.subid", "test_sessions.bblid", "test_sessions_v.battery", "test_sessions_v.dotest", "PerfValid", "Outlier", "Acc3Easy", "PersonFit1", "PersonFit2")
}

df3 <- merge(df2, PLOT)

##### ----------------------------------------------- #####

# Prepare dataframes
df_1515 <- df2[!(is.na(df2$VSPLOT15.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions.siteid", "VSPLOT15.VSPLOT15_TOT_RT", "VSPLOT15.VSPLOT15_SUM_DEG_OFF", "VSPLOT15.VSPLOT15_SUM_EXCESS", "VSPLOT15.VSPLOT15_SUM_DEFICIT", "VSPLOT15.VSPLOT15_CR", "test_sessions_v.gender")]
df_2415 <- df3[!(is.na(df3$VSPLOT24.VSPLOT15_CR)), c("test_sessions.subid", "test_sessions.siteid", "VSPLOT24.VSPLOT15_TOT_RT", "VSPLOT24.VSPLOT15_SUM_DEG_OFF", "VSPLOT24.VSPLOT15_SUM_EXCESS", "VSPLOT24.VSPLOT15_SUM_DEFICIT", "PerfValid", "Outlier", "PersonFit1", "PersonFit2", "VSPLOT24.VSPLOT15_CR", "test_sessions_v.gender")]

#df_1515$CollectionStatus <- "Original"
#df_2415$CollectionStatus <- "Derived"

colnames(df_1515) <- c("subid", "siteid", "TOT_RT", "SUM_DEG_OFF", "SUM_EXCESS", "SUM_DEFICIT", "CR", "sex") #, "CollectionStatus")
colnames(df_2415) <- c("subid", "siteid", "TOT_RT", "SUM_DEG_OFF", "SUM_EXCESS", "SUM_DEFICIT", "PerfValid", "Outlier", "PersonFit1", "PersonFit2", "CR", "sex") #, "CollectionStatus")

rownames(df_1515) <- 1:nrow(df_1515)
rownames(df_2415) <- 1:nrow(df_2415)

# 1515
df_1515$Status <- "Not"
for (i in 1:nrow(df_1515)) {
	if (df_1515[i,"TOT_RT"] > 375000 | ((df_1515[i,"SUM_DEG_OFF"] > 312.5)  & ((df_1515[i,"SUM_EXCESS"] > 125) | (df_1515[i,"SUM_DEFICIT"] > 28.125)))) {
		df_1515[i, "Status"] <- "Flagged"
	}
}
df_1515$Status <- factor(df_1515$Status)

df_1515$TOT_RT_sec <- df_1515$TOT_RT/1000

# 2415
df_2415$Status <- "Not"
df_2415$ReactionTime <- 0
df_2415$DegOffAndExcess <- 0
df_2415$DegOffAndDeficit <- 0

for (i in 1:nrow(df_2415)) {
	if (df_2415[i,"TOT_RT"] > 375000 | ((df_2415[i,"SUM_DEG_OFF"] > 312.5)  & ((df_2415[i,"SUM_EXCESS"] > 125) | (df_2415[i,"SUM_DEFICIT"] > 28.125)))) {
		df_2415[i, "Status"] <- "Flagged"
	}
	if (df_2415[i,"TOT_RT"] > 375000) { 
		df_2415[i, "ReactionTime"] <- 1
	}
	if (df_2415[i,"SUM_DEG_OFF"] > 312.5 & df_2415[i,"SUM_EXCESS"] > 125) {
		df_2415[i, "DegOffAndExcess"] <- 1
	}
	if (df_2415[i,"SUM_DEG_OFF"] > 312.5 & df_2415[i,"SUM_DEFICIT"] > 28.125) {
		df_2415[i, "DegOffAndDeficit"] <- 1
	}
}
df_2415$Status <- factor(df_2415$Status)

df_2415$TOT_RT_sec <- df_2415$TOT_RT/1000

df_2415$PerfValidDich <- "Not"
for (i in 1:nrow(df_2415)) {
	if (df_2415[i, "PerfValid"] < .5563) { df_2415[i, "PerfValidDich"] <- "Flagged" }
}
df_2415$PerfValidDich <- factor(df_2415$PerfValidDich)


##### ---------------- Make plots ---------------- #####

valid_rule <- paste0("TOT_RT > 375000 | ((SUM_DEG_OFF > 312.5 & SUM_EXCESS > 125) | SUM_DEFICIT > 28.125)")

# BBL
df_1515_BBL <- df_1515[df_1515$siteid %in% c("22QIBBC", "BBL_22Q", "BBL_AFF4", "BBL_NAYA", "CNBGRANT", "COGS2L", "CONTE", "CONTE12", "EONS3", "EPIGO", "GO2", "GO3", "GO3FOLL", "GOGRANT", "GRMPY", "LIBI", "MOGO", "MOTIVE", "W_EFFORT", "070", "071", "ASDOD", "BPD", "DAY2", "EXECTS", "FNDM", "FNDM-K", "FORM_AB", "HAND", "HARMONY", "NSBRIHP", "OLFFC", "OLFOXT", "RAINE", "TIPS"), ]

N_orig <- nrow(df_1515_BBL)
per_not_orig <- 100*round(nrow(df_1515_BBL[df_1515_BBL$Status == "Flagged",])/N_orig, digits=3)
subtit_original <- paste0(valid_rule, "\n\n", "N = ", N_orig, " (", per_not_orig, "% Flagged)")
p_original_BBL <- ggplot(df_1515_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=Status)) +
	labs(title="BBL Original VSPLOT 15: Flagged Subjects", x="Total Response Time (seconds)", y="Sum Degrees Off", subtitle=subtit_original) +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

df_2415_BBL <- df_2415[df_2415$siteid %in% c("22QIBBC", "BBL_22Q", "BBL_AFF4", "BBL_NAYA", "CNBGRANT", "COGS2L", "CONTE", "CONTE12", "EONS3", "EPIGO", "GO2", "GO3", "GO3FOLL", "GOGRANT", "GRMPY", "LIBI", "MOGO", "MOTIVE", "W_EFFORT", "070", "071", "ASDOD", "BPD", "DAY2", "EXECTS", "FNDM", "FNDM-K", "FORM_AB", "HAND", "HARMONY", "NSBRIHP", "OLFFC", "OLFOXT", "RAINE", "TIPS"), ]

N_derived <- nrow(df_2415_BBL)
per_not_derived <- 100*round(nrow(df_2415_BBL[df_2415_BBL$Status == "Flagged",])/N_derived, digits=3)
subtit_derived <- paste0(valid_rule, "\n\n", "N = ", N_derived, " (", per_not_derived, "% Flagged)")
p_derived_BBL <- ggplot(df_2415_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=Status)) +
	labs(title="BBL Derived VSPLOT 15: Flagged Subjects", x="Total Response Time (seconds)", y="Sum Degrees Off", subtitle=subtit_derived) +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

# Random 1/5th for ease of visualization
df_2415_rand_BBL <- sample_n(df_2415_BBL[df_2415_BBL$Status == "Not", ], 200)
df_2415_rand_BBL <- rbind(df_2415_rand_BBL, df_2415_BBL[df_2415_BBL$Status == "Flagged", ])
p_rand_derived_BBL <- ggplot(df_2415_rand_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=Status)) +
	labs(title="BBL Derived VSPLOT 15: Flagged Subjects", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

p_rand_perfvalid_BBL <- ggplot(df_2415_rand_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="BBL Derived VSPLOT 15: Perfomance Validity", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = PerfValid)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()

p_rand_perfvaliddich_BBL <- ggplot(df_2415_rand_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=PerfValidDich)) +
	labs(title="BBL Derived VSPLOT 15: Perfomance Validity", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

p_rand_outlier_BBL <- ggplot(df_2415_rand_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="BBL Derived VSPLOT 15: Outlier", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = Outlier)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()

p_rand_perfit1_BBL <- ggplot(df_2415_rand_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="BBL Derived VSPLOT 15: Person Fit 1", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = PersonFit1)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()

p_rand_perfit2_BBL <- ggplot(df_2415_rand_BBL, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="BBL Derived VSPLOT 15: Person Fit 2", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = PersonFit2)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()


# SAX
df_1515_SAX <- df_1515[df_1515$siteid %in% c("SAXEXCLD", "SAXGEN"), ]

N_orig <- nrow(df_1515_SAX)
per_not_orig <- 100*round(nrow(df_1515_SAX[df_1515_SAX$Status == "Flagged",])/N_orig, digits=3)
subtit_orig <- paste0(valid_rule, "\n\n", "N = ", N_orig, " (", per_not_orig, "% Flagged)")
p_original_SAX <- ggplot(df_1515_SAX[df_1515_SAX$CollectionStatus == "Original",], aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=Status)) +
	labs(title="SAX Original VSPLOT 15: Flagged Subjects", x="Total Response Time (seconds)", y="Sum Degrees Off", subtitle=subtit_orig) +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

df_2415_SAX <- df_2415[df_2415$siteid %in% c("SAXEXCLD", "SAXGEN"), ]

N_derived <- nrow(df_2415_SAX)
per_not_derived <- 100*round(nrow(df_2415_SAX[df_2415_SAX$Status == "Flagged",])/N_derived, digits=3)
subtit_derived <- paste0(valid_rule, "\n\n", "N = ", N_derived, " (", per_not_derived, "% Flagged)")
p_derived_SAX <- ggplot(df_2415_SAX, aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=Status)) +
	labs(title="SAX Derived VSPLOT 15: Flagged Subjects", x="Total Response Time (seconds)", y="Sum Degrees Off", subtitle=subtit_derived) +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

# Random 1/5th for ease of visualization
df_2415_rand_SAX <- sample_n(df_2415_SAX[df_2415_SAX$Status == "Not", ], 200)
df_2415_rand_SAX <- rbind(df_2415_rand_SAX, df_2415_SAX[df_2415_SAX$Status == "Flagged", ])
p_rand_derived_SAX <- ggplot(df_2415_rand_SAX, aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=Status)) +
	labs(title="SAX Derived VSPLOT 15: Flagged Subjects", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

p_rand_perfvalid_SAX <- ggplot(df_2415_rand_SAX, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="SAX Derived VSPLOT 15: Perfomance Validity", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = PerfValid)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()

p_rand_perfvaliddich_SAX <- ggplot(df_2415_rand_SAX, aes(x=TOT_RT_sec, y=SUM_DEG_OFF, color=PerfValidDich)) +
	labs(title="SAX Derived VSPLOT 15: Perfomance Validity", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point() + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18))

p_rand_outlier_SAX <- ggplot(df_2415_rand_SAX, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="SAX Derived VSPLOT 15: Outlier", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = Outlier)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()

p_rand_perfit1_SAX <- ggplot(df_2415_rand_SAX, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="SAX Derived VSPLOT 15: Person Fit 1", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = PersonFit1)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()

p_rand_perfit2_SAX <- ggplot(df_2415_rand_SAX, aes(x=TOT_RT_sec, y=SUM_DEG_OFF)) +
	labs(title="SAX Derived VSPLOT 15: Person Fit 2", x="Total Response Time (seconds)", y="Sum Degrees Off") +
	ylim(c(0,1000)) + xlim(c(0, 1000)) +
	geom_point(aes(colour = PersonFit2)) + theme_minimal() +
	theme(plot.title = element_text(face="bold", size=18)) +
	scale_colour_gradient2()


### Create histograms of values of subjects flagged by Tyler but not by house rules
# Degrees off
noteither_degoff <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=SUM_DEG_OFF)) +
	geom_histogram(color="slateblue4" , fill="slateblue1") + theme_minimal() +
	labs(title="Not Flagged by Tyler or House", x="Sum Degrees Off") +
	#xlim(c(min(df_2415_BBL$SUM_DEG_OFF), max(df_2415_BBL$SUM_DEG_OFF))) +
	theme(plot.title = element_text(face="bold", size=18))

nothouse_degoff <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=SUM_DEG_OFF)) +
	geom_histogram(color="lightblue4" , fill="lightblue1") + theme_minimal() +
	labs(title="Flagged by Tyler, Not by House", x="Sum Degrees Off") +
	#xlim(c(min(df_2415_BBL$SUM_DEG_OFF), max(df_2415_BBL$SUM_DEG_OFF))) +
	theme(plot.title = element_text(face="bold", size=18))

nottyler_degoff <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=SUM_DEG_OFF)) +
	geom_histogram(color="lightpink4" , fill="lightpink1") + theme_minimal() +
	labs(title="Not by Tyler, Flagged by House", x="Sum Degrees Off") +
	#xlim(c(min(df_2415_BBL$SUM_DEG_OFF), max(df_2415_BBL$SUM_DEG_OFF))) +
	theme(plot.title = element_text(face="bold", size=18))

both_degoff <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=SUM_DEG_OFF)) +
	geom_histogram(color="brown4" , fill="brown1") + theme_minimal() +
	labs(title="Flagged by Tyler and House", x="Sum Degrees Off") +
	#xlim(c(min(df_2415_BBL$SUM_DEG_OFF), max(df_2415_BBL$SUM_DEG_OFF))) +
	theme(plot.title = element_text(face="bold", size=18))

# Total response time
noteither_tottime <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=TOT_RT_sec)) +
	geom_histogram(color="slateblue4" , fill="slateblue1") + theme_minimal() +
	labs(title="Not Flagged by Tyler or House", x="Total Response Time (seconds)") +
	#xlim(c(min(df_2415_BBL$TOT_RT_sec), max(df_2415_BBL$TOT_RT_sec))) +
	theme(plot.title = element_text(face="bold", size=18))

nothouse_tottime <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=TOT_RT_sec)) +
	geom_histogram(color="lightblue4" , fill="lightblue1") + theme_minimal() +
	labs(title="Flagged by Tyler, Not by House", x="Total Response Time (seconds)") +
	#xlim(c(min(df_2415_BBL$TOT_RT_sec), max(df_2415_BBL$TOT_RT_sec))) +
	theme(plot.title = element_text(face="bold", size=18))

nottyler_tottime <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=TOT_RT_sec)) +
	geom_histogram(color="lightpink4" , fill="lightpink1") + theme_minimal() +
	labs(title="Not by Tyler, Flagged by House", x="Total Response Time (seconds)") +
	#xlim(c(min(df_2415_BBL$TOT_RT_sec), max(df_2415_BBL$TOT_RT_sec))) +
	theme(plot.title = element_text(face="bold", size=18))

both_tottime <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=TOT_RT_sec)) +
	geom_histogram(color="brown4" , fill="brown1") + theme_minimal() +
	labs(title="Flagged by Tyler and House", x="Total Response Time (seconds)") +
	#xlim(c(min(df_2415_BBL$TOT_RT_sec), max(df_2415_BBL$TOT_RT_sec))) +
	theme(plot.title = element_text(face="bold", size=18))

# Number correct
noteither_cr <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=CR)) +
	geom_histogram(color="slateblue4" , fill="slateblue1", bins=15) + theme_minimal() +
	labs(title="Not Flagged by Tyler or House", x="Total Correct") +
	#xlim(c(min(df_2415_BBL$CR), max(df_2415_BBL$CR))) +
	theme(plot.title = element_text(face="bold", size=18))

nothouse_cr <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=CR)) +
	geom_histogram(color="lightblue4" , fill="lightblue1", bins=15) + theme_minimal() +
	labs(title="Flagged by Tyler, Not by House", x="Total Correct") +
	#xlim(c(min(df_2415_BBL$CR), max(df_2415_BBL$CR))) +
	theme(plot.title = element_text(face="bold", size=18))

nottyler_cr <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=CR)) +
	geom_histogram(color="lightpink4" , fill="lightpink1", bins=15) + theme_minimal() +
	labs(title="Not by Tyler, Flagged by House", x="Total Correct") +
	#xlim(c(min(df_2415_BBL$CR), max(df_2415_BBL$CR))) +
	theme(plot.title = element_text(face="bold", size=18))

both_cr <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=CR)) +
	geom_histogram(color="brown4" , fill="brown1", bins=15) + theme_minimal() +
	labs(title="Flagged by Tyler and House", x="Total Correct") +
	#xlim(c(min(df_2415_BBL$CR), max(df_2415_BBL$CR))) +
	theme(plot.title = element_text(face="bold", size=18))

# Outlier
noteither_out <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=Outlier)) +
	geom_histogram(color="slateblue4" , fill="slateblue1", bins=15) + theme_minimal() +
	labs(title="Not Flagged by Tyler or House", x="Outlier Score") +
	#xlim(c(min(df_2415_BBL$Outlier), max(df_2415_BBL$Outlier))) +
	theme(plot.title = element_text(face="bold", size=18))

nothouse_out <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=Outlier)) +
	geom_histogram(color="lightblue4" , fill="lightblue1", bins=15) + theme_minimal() +
	labs(title="Flagged by Tyler, Not by House", x="Outlier Score") +
	#xlim(c(min(df_2415_BBL$Outlier), max(df_2415_BBL$Outlier))) +
	theme(plot.title = element_text(face="bold", size=18))

nottyler_out <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=Outlier)) +
	geom_histogram(color="lightpink4" , fill="lightpink1", bins=15) + theme_minimal() +
	labs(title="Not by Tyler, Flagged by House", x="Outlier Score") +
	#xlim(c(min(df_2415_BBL$Outlier), max(df_2415_BBL$Outlier))) +
	theme(plot.title = element_text(face="bold", size=18))

both_out <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=Outlier)) +
	geom_histogram(color="brown4" , fill="brown1", bins=15) + theme_minimal() +
	labs(title="Flagged by Tyler and House", x="Outlier Score") +
	#xlim(c(min(df_2415_BBL$Outlier), max(df_2415_BBL$Outlier))) +
	theme(plot.title = element_text(face="bold", size=18))

# Person Fit 1
noteither_perf1 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=PersonFit1)) +
	geom_histogram(color="slateblue4" , fill="slateblue1") + theme_minimal() +
	labs(title="Not Flagged by Tyler or House", x="Person Fit 1") +
	#xlim(c(min(df_2415_BBL$PersonFit1), max(df_2415_BBL$PersonFit1))) +
	theme(plot.title = element_text(face="bold", size=18))

nothouse_perf1 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=PersonFit1)) +
	geom_histogram(color="lightblue4" , fill="lightblue1") + theme_minimal() +
	labs(title="Flagged by Tyler, Not by House", x="Person Fit 1") +
	#xlim(c(min(df_2415_BBL$PersonFit1), max(df_2415_BBL$PersonFit1))) +
	theme(plot.title = element_text(face="bold", size=18))

nottyler_perf1 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=PersonFit1)) +
	geom_histogram(color="lightpink4" , fill="lightpink1") + theme_minimal() +
	labs(title="Not by Tyler, Flagged by House", x="Person Fit 1") +
	#xlim(c(min(df_2415_BBL$PersonFit1), max(df_2415_BBL$PersonFit1))) +
	theme(plot.title = element_text(face="bold", size=18))

both_perf1 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=PersonFit1)) +
	geom_histogram(color="brown4" , fill="brown1") + theme_minimal() +
	labs(title="Flagged by Tyler and House", x="Person Fit 1") +
	#xlim(c(min(df_2415_BBL$PersonFit1), max(df_2415_BBL$PersonFit1))) +
	theme(plot.title = element_text(face="bold", size=18))

# Person Fit 2
noteither_perf2 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=PersonFit2)) +
	geom_histogram(color="slateblue4" , fill="slateblue1") + theme_minimal() +
	labs(title="Not Flagged by Tyler or House", x="Person Fit 2") +
	#xlim(c(min(df_2415_BBL$PersonFit2), max(df_2415_BBL$PersonFit2))) +
	theme(plot.title = element_text(face="bold", size=18))

nothouse_perf2 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=PersonFit2)) +
	geom_histogram(color="lightblue4" , fill="lightblue1") + theme_minimal() +
	labs(title="Flagged by Tyler, Not by House", x="Person Fit 2") +
	#xlim(c(min(df_2415_BBL$PersonFit2), max(df_2415_BBL$PersonFit2))) +
	theme(plot.title = element_text(face="bold", size=18))

nottyler_perf2 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=PersonFit2)) +
	geom_histogram(color="lightpink4" , fill="lightpink1") + theme_minimal() +
	labs(title="Not by Tyler, Flagged by House", x="Person Fit 2") +
	#xlim(c(min(df_2415_BBL$PersonFit2), max(df_2415_BBL$PersonFit2))) +
	theme(plot.title = element_text(face="bold", size=18))

both_perf2 <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=PersonFit2)) +
	geom_histogram(color="brown4" , fill="brown1") + theme_minimal() +
	labs(title="Flagged by Tyler and House", x="Person Fit 2") +
	#xlim(c(min(df_2415_BBL$PersonFit2), max(df_2415_BBL$PersonFit2))) +
	theme(plot.title = element_text(face="bold", size=18))

# Performance Validity
noteither_perfit <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=PerfValid)) +
	geom_histogram(color="slateblue4" , fill="slateblue1") + theme_minimal() +
	labs(title="Not Flagged by Tyler or House", x="Performance Validity") +
	#xlim(c(min(df_2415_BBL$PerfValid), max(df_2415_BBL$PerfValid))) +
	theme(plot.title = element_text(face="bold", size=18))

nothouse_perfit <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Not" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=PerfValid)) +
	geom_histogram(color="lightblue4" , fill="lightblue1") + theme_minimal() +
	labs(title="Flagged by Tyler, Not by House", x="Performance Validity") +
	#xlim(c(min(df_2415_BBL$PerfValid), max(df_2415_BBL$PerfValid))) +
	theme(plot.title = element_text(face="bold", size=18))

nottyler_perfit <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Not", ], aes(x=PerfValid)) +
	geom_histogram(color="lightpink4" , fill="lightpink1") + theme_minimal() +
	labs(title="Not by Tyler, Flagged by House", x="Performance Validity") +
	#xlim(c(min(df_2415_BBL$PerfValid), max(df_2415_BBL$PerfValid))) +
	theme(plot.title = element_text(face="bold", size=18))

both_perfit <- ggplot(df_2415_BBL[df_2415_BBL$Status == "Flagged" & df_2415_BBL$PerfValidDich == "Flagged", ], aes(x=PerfValid)) +
	geom_histogram(color="brown4" , fill="brown1") + theme_minimal() +
	labs(title="Flagged by Tyler and House", x="Performance Validity") +
	#xlim(c(min(df_2415_BBL$PerfValid), max(df_2415_BBL$PerfValid))) +
	theme(plot.title = element_text(face="bold", size=18))

##### Return pdf
pdf(file="/home/butellyn/plot_validation/plots/rules_scatter.pdf", width=16, height=8)
grid.arrange(p_original_BBL, p_derived_BBL, ncol=2)
grid.arrange(p_rand_derived_BBL, p_rand_perfvalid_BBL, ncol=2)
grid.arrange(p_rand_derived_BBL, p_rand_perfvaliddich_BBL, ncol=2)
grid.arrange(p_rand_derived_BBL, p_rand_outlier_BBL, ncol=2)
grid.arrange(p_rand_derived_BBL, p_rand_perfit1_BBL, ncol=2)
grid.arrange(p_rand_derived_BBL, p_rand_perfit2_BBL, ncol=2)
grid.arrange(p_original_SAX, p_derived_SAX, ncol=2)
grid.arrange(p_rand_derived_SAX, p_rand_perfvalid_SAX, ncol=2)
grid.arrange(p_rand_derived_SAX, p_rand_perfvaliddich_SAX, ncol=2)
grid.arrange(p_rand_derived_SAX, p_rand_outlier_SAX, ncol=2)
grid.arrange(p_rand_derived_SAX, p_rand_perfit1_SAX, ncol=2)
grid.arrange(p_rand_derived_SAX, p_rand_perfit2_SAX, ncol=2)
dev.off()

pdf(file="/home/butellyn/plot_validation/plots/flagged_discrepancies.pdf", width=16, height=5)
grid.arrange(noteither_degoff, nothouse_degoff, nottyler_degoff, both_degoff, ncol=4)
grid.arrange(noteither_tottime, nothouse_tottime, nottyler_tottime, both_tottime, ncol=4)
grid.arrange(noteither_cr, nothouse_cr, nottyler_cr, both_cr, ncol=4)
grid.arrange(noteither_out, nothouse_out, nottyler_out, both_out, ncol=4)
grid.arrange(noteither_perf1, nothouse_perf1, nottyler_perf1, both_perf1, ncol=4)
grid.arrange(noteither_perf2, nothouse_perf2, nottyler_perf2, both_perf2, ncol=4)
grid.arrange(noteither_perfit, nothouse_perfit, nottyler_perfit, both_perfit, ncol=4)
dev.off()









