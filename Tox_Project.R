####################################################
##                                                ##
## TINY TOX DATA MANAGEMENT AND REPORTING SYSTEM  ##
##                                                ##
##                                                ##
## The objective of this project is to streamline ##
## what we do with raw data from tox studies,     ##
## and create a nice report (if possible,         ##
## an interactive interface).                     ##
##                                                ##
## Initiation day: Aug 21, 2014                   ##
##                                                ##
####################################################


# Setting up working directory
# This is going to be changed based on real-life conditions.

# Open the R or Rmd file in the working directory using RStudio.
# This way, one does not have to set wd manually.

# Importing basic study information from excel files

# Use "options(width=xxx)" to increase the output width 

library(xlsx)
filename <- 'Minitox.xlsx'
studyinfo <- read.xlsx(filename, sheetIndex = 1)

# Showing study information

paste(studyinfo[1,1], ':', studyinfo[1,2])
study_title <- paste('Title', ': ', studyinfo[3,2], ' ', 'A ', studyinfo[15,2], '-Day Minitox Oral Toxicity Study in Male Rats', sep='')
print(study_title)

# Importing body weight data from excel files

bw <- read.xlsx(filename, sheetIndex = 2)

# Creating a new dataframe with only main animal data
# And excluding null groups
bw_main <- subset(bw, TK == 'NO' & DAY_1 > 0)

# Obtaining valid group numbers
bw_group_no <- subset(bw, DAY_1 > 0)
group_no = length(unique(bw_group_no$DOSE_LEVEL))

# Obtaining information of study days
# Adding na.rm to include cases where animals die and
# no data are entered.
bw_days = 0
for (i in 1:(ncol(bw_main)-4)) {
    if (is.na(mean(bw_main[,(4+i)], na.rm = TRUE)) == 'FALSE') {
        if (mean(bw_main[,(4+i)], na.rm = TRUE) > 0) {
            bw_days <- bw_days + 1 
        }
    } 
    else if (is.na(mean(bw_main[,(4+i)], na.rm = TRUE)) == 'TRUE') {}
}
# This is a very well concealed bug. 
# Use "ncol(bw_main)-4" instead of "ncol(bw_main)"


# Obtaining descriptive statistics of main animals
library(psych)
# Adding the following line to ensure the order of factors in DOSE LEVELS
bw_main$DOSE_LEVEL <- factor(bw_main$DOSE_LEVEL, levels = unique(bw_main$DOSE_LEVEL))
bw_main_desc <- describeBy(bw_main, bw_main$DOSE_LEVEL)
bw_main_desc_output = list()
for (i in 1:group_no) {
    bw_main_desc_i <- bw_main_desc[[i]][5:(4 + bw_days),3:4]        
    bw_main_desc_output[[i]] = bw_main_desc_i
}
bw_main_desc_output

# Obtaining group doses
group_names = paste(as.character(unique(bw_main$DOSE_LEVEL)), 'mg/kg')

# Obtaining dates
bw_dates <- rownames(bw_main_desc_output[[1]])

# Renaming the component names of bw_main_desc_output
names(bw_main_desc_output) = group_names

# Transforming the bw_main_desc_output into a neat table
# Using unicode '\U00b1' to describe the plus minus sign
library(plyr)
for (i in 1:length(bw_main_desc_output)) {
        bw_main_desc_output[[i]]$avg = paste(as.character(round(bw_main_desc_output[[i]]$mean)),
                                             as.character(round(bw_main_desc_output[[i]]$sd)), sep=' \U00b1 ') }
    output_temp1 <- lapply(bw_main_desc_output, function(x) {x[,3]})
    output_temp2 <- lapply(output_temp1, function(x) {t(x)})
bw_main_desc_output <- ldply(output_temp2)

colnames(bw_main_desc_output) <- c('DOSE', bw_dates)
print(bw_main_desc_output)

# Outputing average and SD summary data for report drafting
# First, the average data
bw_main_avg_output = list()
for (i in 1:group_no) {
    bw_main_avg_i <- round(bw_main_desc[[i]][5:(4 + bw_days),3], digits = 0)    
    bw_main_avg_output[[i]] = bw_main_avg_i
}
names(bw_main_avg_output) = group_names
bw_main_avg_output <- ldply(bw_main_avg_output)
colnames(bw_main_avg_output) <- c('DOSE', bw_dates)

# Second, the SD data
bw_main_sd_output = list()
for (i in 1:group_no) {
    bw_main_sd_i <- round(bw_main_desc[[i]][5:(4 + bw_days),4], digits = 0)        
    bw_main_sd_output[[i]] = bw_main_sd_i
}
names(bw_main_sd_output) = group_names
bw_main_sd_output <- ldply(bw_main_sd_output)
colnames(bw_main_sd_output) <- c('DOSE', bw_dates)


# Conducting Levene's Test between different dose groups

library(car)
bw_main_aov <- bw_main_desc_output
bw_main_aov[group_no+1,1] = c('LEVENE')
bw_main_aov[group_no+2,1] = c('ANOVA (p=)')
bw_main_aov[group_no+3,1] = c('KRUSKAL-WALLIS (p=)')
bw_main_aov[group_no+4,1] = c('- SUMMARY -')
count_aov = ncol(bw_main_aov) - 1
bw_main_aov <- data.frame(lapply(bw_main_aov, as.character), stringsAsFactors = FALSE)
# I'm not sure about what happened here. bw_main_aov was unintentionally 
# changed into a dataframe with factor-type elements. 
# Therefore, I had to manually change them back into characters.
for (i in 1:count_aov) {
    bw_day_levene <- leveneTest(bw_main[,4+i] ~ as.character(bw_main$DOSE_LEVEL), center = mean)
# Add a consideration for null results of the Levene's test
    if (is.na(bw_day_levene[1,3] == TRUE)) {
        bw_main_aov[group_no+1,i+1] = c('-')
        bw_main_aov[group_no+2,i+1] = c('-')
        bw_main_aov[group_no+3,i+1] = c('-')
    }
    else if (bw_day_levene[1,3] > 0.05) {
        bw_main_aov[group_no+1,i+1] = c('PASS')
        bw_day_aov <- aov(bw_main[,4+i] ~ as.character(DOSE_LEVEL), data = bw_main)
        bw_main_aov[group_no+2,i+1] = round(summary(bw_day_aov)[[1]][1,5], digits = 4)
        bw_main_aov[group_no+3,i+1] = c('-')
           }
    else {
        bw_day_levene_log <- leveneTest(log(bw_main[,4+i]) ~ as.character(bw_main$DOSE_LEVEL), center = mean)
        bw_day_levene_sqrt <- leveneTest(sqrt(bw_main[,4+i]) ~ as.character(bw_main$DOSE_LEVEL), center = mean)
        bw_day_levene_recip <- leveneTest(1/(bw_main[,4+i]) ~ as.character(bw_main$DOSE_LEVEL), center = mean)
        if (bw_day_levene_log[1,3] > 0.05) {
            bw_day_aov <- aov(log(bw_main[,4+i]) ~ as.character(DOSE_LEVEL), data = bw_main)
            bw_main_aov[group_no+2,i+1] = round(summary(bw_day_aov)[[1]][1,5], digits = 4)
            bw_main_aov[group_no+3,i+1] = c('-')
            bw_main_aov[group_no+1,i+1] = c('~')
        }
        else if (bw_day_levene_sqrt[1,3] > 0.05) {
            bw_day_aov <- aov(sqrt(bw_main[,4+i]) ~ as.character(DOSE_LEVEL), data = bw_main)
            bw_main_aov[group_no+2,i+1] = round(summary(bw_day_aov)[[1]][1,5], digits = 4)
            bw_main_aov[group_no+3,i+1] = c('-')
            bw_main_aov[group_no+1,i+1] = c('~')
        }
        else if (bw_day_levene_recip[1,3] > 0.05) {
            bw_day_aov <- aov(1/bw_main[,4+i] ~ as.character(DOSE_LEVEL), data = bw_main)
            bw_main_aov[group_no+2,i+1] = round(summary(bw_day_aov)[[1]][1,5], digits = 4)
            bw_main_aov[group_no+3,i+1] = c('-')
            bw_main_aov[group_no+1,i+1] = c('~')
        }
        else { bw_main_aov[group_no+1,i+1] = c('FAIL')
               bw_main_aov[group_no+2,i+1] = c('-')
               bw_day_kruskal <- kruskal.test(bw_main[,4+i] ~ bw_main$DOSE_LEVEL)
               bw_main_aov[group_no+3,i+1] = round(bw_day_kruskal$p.value, digits = 4)
        }}
    
}

# Sanity check
bw_main_aov

# Create a summary row
# 1. Beware of the intricate order of judgments. 
# Go with the most stringent first.
# 2. Use 'else if' instead of multiple 'if's,
# as the latter will create unforeseeable errors.
for (i in 1:count_aov) {
    if (bw_main_aov[group_no+2,i+1] == "-") {
        if (as.numeric(bw_main_aov[group_no+3,i+1]) - 0.001 < 0) {
            bw_main_aov[group_no+4, i+1] = '***'
        }
        else if (as.numeric(bw_main_aov[group_no+3,i+1]) - 0.01 < 0) {
            bw_main_aov[group_no+4, i+1] = '**'
        }
        else if (as.numeric(bw_main_aov[group_no+3,i+1]) - 0.05 < 0) {
            bw_main_aov[group_no+4, i+1] = '*'
        }
        else {
            bw_main_aov[group_no+4, i+1] = '---'        
        }
    }
    else if (as.numeric(bw_main_aov[group_no+2,i+1]) - 0.001 < 0) {
        bw_main_aov[group_no+4, i+1] = '***'
    }
    else if (as.numeric(bw_main_aov[group_no+2,i+1]) - 0.01 < 0) {
        bw_main_aov[group_no+4, i+1] = '**'
    }
    else if (as.numeric(bw_main_aov[group_no+2,i+1]) - 0.05 < 0) {
        bw_main_aov[group_no+4, i+1] = '*'
    }
    else {
        bw_main_aov[group_no+4, i+1] = '---'
    }
}

# Sanity check
bw_main_aov

    

# Post-hoc statistical analysis for ANOVA (including non-significant results)
library(pgirmess)
bw_main_posthoc <- bw_main_aov
bw_main_posthoc[1,2:ncol(bw_main_posthoc)] = "---"
for (i in 1:count_aov) {
# Add a consideration for failed Levene's Test
    if (paste(bw_main_posthoc[(group_no+1):(group_no+3),i+1], collapse='') == c('---')) {
        bw_main_posthoc[2:group_no,i+1] = rep('-', times = group_no - 1)
    }
    else if (bw_main_posthoc[group_no+2, i+1] >= 0 & bw_main_posthoc[group_no+1, i+1] == 'PASS') {
        bw_main_Tukey <- TukeyHSD(aov(bw_main[,4+i] ~ bw_main$DOSE_LEVEL))
        bw_main_posthoc[2:group_no,i+1] = bw_main_Tukey[[1]][1:(group_no-1),4]
    }
    else if (bw_main_posthoc[group_no+2, i+1] >= 0 & bw_main_posthoc[group_no+1, i+1] == '~') {
        bw_day_levene_log <- leveneTest(log(bw_main[,4+i]) ~ as.character(bw_main$DOSE_LEVEL), center = mean)
        bw_day_levene_sqrt <- leveneTest(sqrt(bw_main[,4+i]) ~ as.character(bw_main$DOSE_LEVEL), center = mean)
        bw_day_levene_recip <- leveneTest(1/(bw_main[,4+i]) ~ as.character(bw_main$DOSE_LEVEL), center = mean)
        if (bw_day_levene_log[1,3] > 0.05) {
            bw_main_Tukey <- TukeyHSD(aov(log(bw_main[,4+i]) ~ bw_main$DOSE_LEVEL))
            bw_main_posthoc[2:group_no, i+1] = bw_main_Tukey[[1]][1:(group_no-1),4]}
        else if (bw_day_levene_sqrt[1,3] > 0.05) {
            bw_main_Tukey <- TukeyHSD(aov(sqrt(bw_main[,4+i]) ~ bw_main$DOSE_LEVEL))
            bw_main_posthoc[2:group_no, i+1] = bw_main_Tukey[[1]][1:(group_no-1),4]}
        else if (bw_day_levene_recip[1,3] > 0.05) {
            bw_main_Tukey <- TukeyHSD(aov(1/(bw_main[,4+i]) ~ bw_main$DOSE_LEVEL))
            bw_main_posthoc[2:group_no, i+1] = bw_main_Tukey[[1]][1:(group_no-1),4]}
    }
    else if (bw_main_posthoc[group_no+3, i+1] >= 0) {
        bw_main_Kruskal <- kruskalmc(bw_main[,4+i], bw_main$DOSE_LEVEL, prob = 0.05)
        bw_main_posthoc[2:group_no, i+1] = bw_main_Kruskal[[3]][1:group_no-1,3]
    }
}
# About kruskalmc: Its post-hoc p value can only be defined in the command line.
# To avoid "unnecessary excessive work", p is always defined as 0.05 here.
# Further work can be done to test cases where p is 0.01 or 0.001.

# Sanity check
bw_main_posthoc

# Cleaning up the post-hoc table
for (i in 2:group_no) {
    for (j in 2:(count_aov+1)) {
        if (bw_main_posthoc[i,j] == 'FALSE') {bw_main_posthoc[i,j] = '---'}
        else if (bw_main_posthoc[i,j] == 'TRUE') {bw_main_posthoc[i,j] = 'p<0.05'}
        else if (bw_main_posthoc[i,j] == '---' | bw_main_posthoc[i,j] == '-') {bw_main_posthoc[i,j] = '---'}
        else if (as.numeric(bw_main_posthoc[i,j]) < 0.001) {bw_main_posthoc[i,j] = 'p<0.001'}
        else if (as.numeric(bw_main_posthoc[i,j]) < 0.01) {bw_main_posthoc[i,j] = 'p<0.01'}
        else if (as.numeric(bw_main_posthoc[i,j]) < 0.05) {bw_main_posthoc[i,j] = 'p<0.05'}
        else {bw_main_posthoc[i,j] = '---'}
    }    
}

# I spent almost hours debugging the above few lines of codes.
# The problem was that I left out the parentheses around 'count_aov+1'.
# I almost lost my sanity.

# Sanity check
bw_main_posthoc    

# Creating line charts for body weight data
# Starting by re-creating mean+sd data
bw_main_desc <- describeBy(bw_main, bw_main$DOSE_LEVEL)
bw_main_mean = list()
bw_main_sd = list()
for (i in 1:group_no) {
    bw_main_mean[[i]] <- bw_main_desc[[i]][5:(4 + bw_days),3]
    bw_main_sd[[i]] <- round(bw_main_desc[[i]][5:(4 + bw_days),4], digits = 2)
}

bw_main_chart <- data.frame(unlist(bw_main_mean))
colnames(bw_main_chart) = 'mean'
bw_main_sd <- data.frame(unlist(bw_main_sd))
colnames(bw_main_sd) = 'sd'
bw_main_chart$sd = unlist(bw_main_sd)
# Note that without the 'unlist' function, 'bw_main_chart$sd' will 
# become a datafrome with the datafrome of 'bw_main_chart', which
# will render plotting with errorbars below IMPOSSIBLE!!!
# I f*cking spent more than one hour figuring this out.
bw_main_chart$day = bw_dates
bw_main_chart$dose = rep(group_names, each = count_aov)

# Sanity check
bw_main_chart

# Creating faceted overview of body weight changes over all days for
# individual dose groups
# introducing an ordered definition for dose factors
bw_main_chart$dose <- factor(bw_main_chart$dose, levels = unique(bw_main_chart$dose))
library(ggplot2)
bw_main_chart_facet <- ggplot(bw_main_chart, aes(x = day, y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, size = 0.5) + geom_bar(stat = 'identity', width = 0.75, fill = 'burlywood', color = 'black') + facet_grid(~ dose) + scale_y_continuous(limits = c(0,350), breaks = c(0,50,100,150,200,250,300), name = 'Mean Body Weight (g)') + theme(axis.title.x = element_blank())
bw_main_chart_facet
# For a quick reference of designated colors in ggplot, check:
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# Creating a line plot to compare group body weight data
pd <- position_dodge(0.1)
# Predefining the position dodge value so that
# the plot command below gets neater

bw_main_chart_allinone <- ggplot(bw_main_chart, aes(x = day, y = mean, group = dose)) +  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, size = 0.5, position = pd) + geom_line(stat = 'identity', size =0.5, aes(color = dose)) + scale_y_continuous(name = "Mean Body Weight (g)") + geom_point(aes(color = dose), size = 4, position = pd) + theme(axis.title.x = element_blank()) + guides(color = guide_legend(title = ""))
# Placing the errorbar parts before lines and points
bw_main_chart_allinone
    




# Analyzing body weight gain data
# Creating a table and generating body weight gain data
bwg_main <- bw_main[,1:4]
for (i in 1:(count_aov-1)) {
    bwg_main[,i+4] <- bw_main[,i+5] - bw_main[i+4]
    colnames(bwg_main)[i+4] = colnames(bw_main)[i+5]
}

# Adding a column of sums
bwg_main$SUM <- 0
for (i in 1:nrow(bwg_main)){
    bwg_main$SUM[i] <- sum(bwg_main[i,-(1:4)])
}


# Obtaining descriptive statistics of main animals
bwg_main_desc <- describeBy(bwg_main, bwg_main$DOSE_LEVEL)
bwg_main_desc_output = list()
for (i in 1:group_no) {
    bwg_main_desc_i <- bwg_main_desc[[i]][5:(4 + bw_days),3:4]        
    bwg_main_desc_output[[i]] = bwg_main_desc_i
}
bwg_main_desc_output

# Obtaining dates
bwg_dates <- rownames(bwg_main_desc_output[[1]])

# Renaming the component names of bw_main_desc_output
names(bwg_main_desc_output) = group_names

# Transforming the bw_main_desc_output into a neat table
# Using unicode '\U00b1' to describe the plus minus sign
for (i in 1:length(bwg_main_desc_output)) {
    bwg_main_desc_output[[i]]$avg = paste(as.character(round(bwg_main_desc_output[[i]]$mean)),
                                         as.character(round(bwg_main_desc_output[[i]]$sd)), sep=' \U00b1 ') }
output_temp1 <- lapply(bwg_main_desc_output, function(x) {x[,3]})
output_temp2 <- lapply(output_temp1, function(x) {t(x)})
bwg_main_desc_output <- ldply(output_temp2)

colnames(bwg_main_desc_output) <- c('DOSE', bwg_dates)
print(bwg_main_desc_output)


# Outputing average and SD summary data for report drafting
# First, the average data
bwg_main_avg_output = list()
for (i in 1:group_no) {
    bwg_main_avg_i <- round(bwg_main_desc[[i]][5:(4 + bw_days),3], digits = 0)    
    bwg_main_avg_output[[i]] = bwg_main_avg_i
}
names(bwg_main_avg_output) = group_names
bwg_main_avg_output <- ldply(bwg_main_avg_output)
colnames(bwg_main_avg_output) <- c('DOSE', bwg_dates)

# Second, the SD data
bwg_main_sd_output = list()
for (i in 1:group_no) {
    bwg_main_sd_i <- round(bwg_main_desc[[i]][5:(4 + bw_days),4], digits = 0)        
    bwg_main_sd_output[[i]] = bwg_main_sd_i
}
names(bwg_main_sd_output) = group_names
bwg_main_sd_output <- ldply(bwg_main_sd_output)
colnames(bwg_main_sd_output) <- c('DOSE', bwg_dates)



# Conducting Levene's Test between different dose groups

bwg_main_aov <- bwg_main_desc_output
bwg_main_aov[group_no+1,1] = c('LEVENE')
bwg_main_aov[group_no+2,1] = c('ANOVA (p=)')
bwg_main_aov[group_no+3,1] = c('KRUSKAL-WALLIS (p=)')
bwg_main_aov[group_no+4,1] = c('- SUMMARY -')
count_aov_bwg = ncol(bwg_main_aov) - 1
bwg_main_aov <- data.frame(lapply(bwg_main_aov, as.character), stringsAsFactors = FALSE)
# I'm not sure about what happened here. bw_main_aov was unintentionally 
# changed into a dataframe with factor-type elements. 
# Therefore, I had to manually change them back into characters.

# There are more complicated issues here.
# Below zero numbers have to be considered here, as they cannot be applied
# to sqrt and log functions. 
# REF http://www.basic.northwestern.edu/statguidefiles/oneway_anova_alts.html
for (i in 1:count_aov_bwg) {
    bwg_day_levene <- leveneTest(bwg_main[,4+i] ~ as.character(bwg_main$DOSE_LEVEL), center = mean)
    if (bwg_day_levene[1,3] > 0.05) {
        bwg_main_aov[group_no+1,i+1] = c('PASS')
        bwg_day_aov <- aov(bwg_main[,4+i] ~ as.character(DOSE_LEVEL), data = bwg_main)
        bwg_main_aov[group_no+2,i+1] = round(summary(bwg_day_aov)[[1]][1,5], digits = 4)
        bwg_main_aov[group_no+3,i+1] = c('-')
    }
    else {
        bwg_day_levene_log <- leveneTest(log(bwg_main[,4+i]) ~ as.character(bwg_main$DOSE_LEVEL), center = mean)
        bwg_day_levene_sqrt <- leveneTest(sqrt(bwg_main[,4+i]) ~ as.character(bwg_main$DOSE_LEVEL), center = mean)
        bwg_day_levene_recip <- leveneTest(1/(bwg_main[,4+i]) ~ as.character(bwg_main$DOSE_LEVEL), center = mean)
        if (!all(bwg_main[,4+i]>0)) {
# Adding na.rm to avoid null data
            increment = -min(bwg_main[,4+i], na.rm = TRUE) + 1
            bwg_main_incre <- bwg_main[,4+i] + increment
            bwg_day_levene_log_incre <- leveneTest(log(bwg_main_incre) ~ as.character(bwg_main$DOSE_LEVEL), center = mean)
            bwg_day_levene_sqrt_incre <- leveneTest(sqrt(bwg_main_incre) ~ as.character(bwg_main$DOSE_LEVEL), center = mean)
            if (bwg_day_levene_log_incre[1,3] > 0.05) {
                bwg_day_aov <- aov(log(bwg_main_incre) ~ as.character(bwg_main$DOSE_LEVEL))
                bwg_main_aov[group_no+2,i+1] = round(summary(bwg_day_aov)[[1]][1,5], digits = 4)
                bwg_main_aov[group_no+3,i+1] = c('-')
                bwg_main_aov[group_no+1,i+1] = c('~')
            }
            else if (bwg_day_levene_sqrt_incre[1,3] > 0.05) {
                bwg_day_aov <- aov(sqrt(bwg_main_incre) ~ as.character(bwg_main$DOSE_LEVEL))
                bwg_main_aov[group_no+2,i+1] = round(summary(bwg_day_aov)[[1]][1,5], digits = 4)
                bwg_main_aov[group_no+3,i+1] = c('-')
                bwg_main_aov[group_no+1,i+1] = c('~')
            }
            else {bwg_main_aov[group_no+1,i+1] = c('FAIL')
                  bwg_main_aov[group_no+2,i+1] = c('-')
                  bwg_day_kruskal <- kruskal.test(bwg_main[,4+i] ~ bwg_main$DOSE_LEVEL)
                  bwg_main_aov[group_no+3,i+1] = round(bwg_day_kruskal$p.value, digits = 4)
            }
        }
        else if (all(bwg_main[,4+i]>0) & bwg_day_levene_log[1,3] > 0.05) {
            bwg_day_aov <- aov(log(bwg_main[,4+i]) ~ as.character(DOSE_LEVEL), data = bwg_main)
            bwg_main_aov[group_no+2,i+1] = round(summary(bwg_day_aov)[[1]][1,5], digits = 4)
            bwg_main_aov[group_no+3,i+1] = c('-')
            bwg_main_aov[group_no+1,i+1] = c('~')
        }
        else if (all(bwg_main[,4+i]>0) & bwg_day_levene_sqrt[1,3] > 0.05) {
            bwg_day_aov <- aov(sqrt(bwg_main[,4+i]) ~ as.character(DOSE_LEVEL), data = bwg_main)
            bwg_main_aov[group_no+2,i+1] = round(summary(bwg_day_aov)[[1]][1,5], digits = 4)
            bwg_main_aov[group_no+3,i+1] = c('-')
            bwg_main_aov[group_no+1,i+1] = c('~')
        }
        else if (bwg_day_levene_recip[1,3] > 0.05) {
            bwg_day_aov <- aov(1/(bwg_main[,4+i]) ~ as.character(DOSE_LEVEL), data = bwg_main)
            bwg_main_aov[group_no+2,i+1] = round(summary(bwg_day_aov)[[1]][1,5], digits = 4)
            bwg_main_aov[group_no+3,i+1] = c('-')
            bwg_main_aov[group_no+1,i+1] = c('~')
        }
        else { bwg_main_aov[group_no+1,i+1] = c('FAIL')
               bwg_main_aov[group_no+2,i+1] = c('-')
               bwg_day_kruskal <- kruskal.test(bwg_main[,4+i] ~ bwg_main$DOSE_LEVEL)
               bwg_main_aov[group_no+3,i+1] = round(bwg_day_kruskal$p.value, digits = 4)
        }}
    }
# Remember not to input non integer number here. 
# Otherwise you could have spent hours debugging a strange bug.


# Sanity check
bwg_main_aov

# Create a summary row
for (i in 1:count_aov_bwg) {
    if (bwg_main_aov[group_no+2,i+1] == "-") {
        if (as.numeric(bwg_main_aov[group_no+3,i+1]) - 0.001 < 0) {
            bwg_main_aov[group_no+4, i+1] = '***'
        }
        else if (as.numeric(bwg_main_aov[group_no+3,i+1]) - 0.01 < 0) {
            bwg_main_aov[group_no+4, i+1] = '**'
        }
        else if (as.numeric(bwg_main_aov[group_no+3,i+1]) - 0.05 < 0) {
            bwg_main_aov[group_no+4, i+1] = '*'
        }
        else {
            bwg_main_aov[group_no+4, i+1] = '---'        
        }
    }
    else if (as.numeric(bwg_main_aov[group_no+2,i+1]) - 0.001 < 0) {
        bwg_main_aov[group_no+4, i+1] = '***'
    }
    else if (as.numeric(bwg_main_aov[group_no+2,i+1]) - 0.01 < 0) {
        bwg_main_aov[group_no+4, i+1] = '**'
    }
    else if (as.numeric(bwg_main_aov[group_no+2,i+1]) - 0.05 < 0) {
        bwg_main_aov[group_no+4, i+1] = '*'
    }
    else {
        bwg_main_aov[group_no+4, i+1] = '---'
    }
}

# Sanity check
bwg_main_aov



# Post-hoc statistical analysis for ANOVA (including non-significant results)
bwg_main_posthoc <- bwg_main_aov
bwg_main_posthoc[1,2:ncol(bwg_main_posthoc)] = "---"
for (i in 1:count_aov_bwg) {
    if (bwg_main_posthoc[group_no+2, i+1] >= 0 & bwg_main_posthoc[group_no+1, i+1] == 'PASS') {
        bwg_main_Tukey <- TukeyHSD(aov(bwg_main[,4+i] ~ bwg_main$DOSE_LEVEL))
        bwg_main_posthoc[2:group_no,i+1] = bwg_main_Tukey[[1]][1:(group_no-1),4]
    }
    else if (bwg_main_posthoc[group_no+2, i+1] >= 0 & bwg_main_posthoc[group_no+1, i+1] == '~') {
        bwg_day_levene_log <- leveneTest(log(bwg_main[,4+i]) ~ bwg_main$DOSE_LEVEL, center = mean)
        bwg_day_levene_sqrt <- leveneTest(sqrt(bwg_main[,4+i]) ~ bwg_main$DOSE_LEVEL, center = mean)
        bwg_day_levene_recip <- leveneTest(1/(bwg_main[,4+i]) ~ bwg_main$DOSE_LEVEL, center = mean)
        if (!all(bwg_main[,4+i]>0)) {
            increment = -min(bwg_main[,4+i], na.rm = TRUE) + 1
            bwg_main_incre <- bwg_main[,4+i] + increment
            bwg_day_levene_log_incre <- leveneTest(log(bwg_main_incre) ~ bwg_main$DOSE_LEVEL, center = mean)
            bwg_day_levene_sqrt_incre <- leveneTest(sqrt(bwg_main_incre) ~ bwg_main$DOSE_LEVEL, center = mean)
            if (bwg_day_levene_log_incre[1,3] > 0.05) {
                bwg_main_Tukey <- TukeyHSD(aov(log(bwg_main_incre) ~ bwg_main$DOSE_LEVEL))
                bwg_main_posthoc[2:group_no, i+1] = bwg_main_Tukey[[1]][1:(group_no-1),4]}
            else if (bwg_day_levene_sqrt_incre[1,3] > 0.05) {
                bwg_main_Tukey <- TukeyHSD(aov(sqrt(bwg_main_incre) ~ bwg_main$DOSE_LEVEL))
                bwg_main_posthoc[2:group_no, i+1] = bwg_main_Tukey[[1]][1:(group_no-1),4]}            
            }
        else if (bwg_day_levene_recip[1,3] > 0.05) {
            bwg_main_Tukey <- TukeyHSD(aov(1/(bwg_main[,4+i]) ~ bwg_main$DOSE_LEVEL))
            bwg_main_posthoc[2:group_no, i+1] = bwg_main_Tukey[[1]][1:(group_no-1),4]}
    }
    else if (bwg_main_posthoc[group_no+3, i+1] >= 0) {
        bwg_main_Kruskal <- kruskalmc(bwg_main[,4+i], bwg_main$DOSE_LEVEL, prob = 0.05)
        bwg_main_posthoc[2:group_no, i+1] = bwg_main_Kruskal[[3]][1:group_no-1,3]
    }
}

# Sanity check
bwg_main_posthoc

# Cleaning up the post-hoc table
for (i in 2:group_no) {
    for (j in 2:(count_aov_bwg+1)) {
        if (bwg_main_posthoc[i,j] == 'FALSE') {bwg_main_posthoc[i,j] = '---'}
        else if (bwg_main_posthoc[i,j] == 'TRUE') {bwg_main_posthoc[i,j] = 'p<0.05'}
        else if (bwg_main_posthoc[i,j] == '---') {bwg_main_posthoc[i,j] = '---'}
        else if (as.numeric(bwg_main_posthoc[i,j]) < 0.001) {bwg_main_posthoc[i,j] = 'p<0.001'}
        else if (as.numeric(bwg_main_posthoc[i,j]) < 0.01) {bwg_main_posthoc[i,j] = 'p<0.01'}
        else if (as.numeric(bwg_main_posthoc[i,j]) < 0.05) {bwg_main_posthoc[i,j] = 'p<0.05'}
        else {bwg_main_posthoc[i,j] = '---'}
    }    
}

# Sanity check
bwg_main_posthoc    

# Creating line charts for body weight data
# Starting by re-creating mean+sd data
bwg_main_desc <- describeBy(bwg_main, bwg_main$DOSE_LEVEL)
bwg_main_mean = list()
bwg_main_sd = list()
for (i in 1:group_no) {
    bwg_main_mean[[i]] <- bwg_main_desc[[i]][5:(4 + length(bwg_dates)),3]
    bwg_main_sd[[i]] <- round(bwg_main_desc[[i]][5:(4 + length(bwg_dates)),4], digits = 2)
}

bwg_main_chart <- data.frame(unlist(bwg_main_mean))
colnames(bwg_main_chart) = 'mean'
bwg_main_sd <- data.frame(unlist(bwg_main_sd))
colnames(bwg_main_sd) = 'sd'
bwg_main_chart$sd = unlist(bwg_main_sd)
# Note that without the 'unlist' function, 'bw_main_chart$sd' will 
# become a datafrome with the datafrome of 'bw_main_chart', which
# will render plotting with errorbars below IMPOSSIBLE!!!
# I f*cking spent more than one hour figuring this out.
bwg_main_chart$day = bwg_dates
bwg_main_chart$dose = rep(group_names, each = count_aov_bwg)

# Sanity check
bwg_main_chart

# Creating faceted overview of body weight changes over all days for
# individual dose groups
library(ggplot2)
bwg_main_chart_group <- subset(bwg_main_chart, day != 'SUM')
bwg_main_chart_group$dose <- factor(bwg_main_chart_group$dose, levels = unique(bwg_main_chart_group$dose))
bwg_main_chart_facet <- ggplot(bwg_main_chart_group, aes(x = day, y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, size = 0.5) + geom_bar(stat = 'identity', width = 0.75, fill = 'burlywood', color = 'black') + facet_grid(~ dose) + scale_y_continuous(name = 'Mean Body Weight Gain (g)') + theme(axis.title.x = element_blank())
bwg_main_chart_facet

# For sum data
bwg_main_chart_sum <- subset(bwg_main_chart, day == 'SUM')
bwg_main_chart_sum$dose <- factor(bwg_main_chart_sum$dose, levels = unique(bwg_main_chart_sum$dose))
bwg_main_chart_all <- ggplot(bwg_main_chart_sum, aes(x = dose, y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, size = 0.5) + geom_bar(stat = 'identity', width = 0.75, fill = 'burlywood', color = 'black') + scale_y_continuous(name = 'Body Weight Gain Throughout (g)') + theme(axis.title.x = element_blank())
bwg_main_chart_all

# For a quick reference of designated colors in ggplot, check:
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# Creating a line plot to compare group body weight data
pd <- position_dodge(0.1)
# Predefining the position dodge value so that
# the plot command below gets neater
bwg_main_chart_allinone <- ggplot(bwg_main_chart_group, aes(x = day, y = mean, group = dose)) +  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, size = 0.5, position = pd) + geom_line(stat = 'identity', size =0.5, aes(color = dose)) + scale_y_continuous(name = "Mean Body Weight Gain (g)") + geom_point(aes(color = dose), size = 4, position = pd) + theme(axis.title.x = element_blank()) + guides(color = guide_legend(title = ""))
# Placing the errorbar parts before lines and points
bwg_main_chart_allinone




# Importing food consumption data from excel files

fc <- read.xlsx(filename, sheetIndex = 3)

# Obtaining valid group numbers
# 'group_no' from the body weight section can be used here.

# Obtaining information of columns of food consumption data
fc_days = bw_days - 1

# Transforming raw fc data into workable ones
fc_raw <- fc[,(1:(4+fc_days))]

# Obtaining column headers of food consumption data
tmp_header <- vector(mode = 'character')
for (i in 1:fc_days) {
    tmp_header = c((tmp_header), paste('DAY_', i, '_', i+1, sep=''))
}

# Tabulating fc_main with body weight gain data
fc_main <- fc_raw
for (i in 1:fc_days) {
    fc_main[,i+4] = fc[,i+4] - fc[,i+5]
    colnames(fc_main)[i+4] = tmp_header[i]
}

# Creating a new dataframe with only main animal data
# And excluding null groups
fc_main <- subset(fc_main, TK == 'NO' & paste(unique(fc$DAY_1), collapse='') != paste(rep('0', times = nrow(fc)), collapse=''))


# Calculating sum of total food consumption
fc_main_sum = rep(0, nrow(fc_main))
for (i in 1:fc_days) {
    fc_main_sum = fc_main_sum + fc_main[,i+4]
}
fc_main$SUM = fc_main_sum


# Obtaining descriptive statistics of food consumption data of main animals
fc_main$DOSE_LEVEL <- factor(fc_main$DOSE_LEVEL, levels = unique(fc_main$DOSE_LEVEL))
fc_main_desc <- describeBy(fc_main, fc_main$DOSE_LEVEL)
fc_main_desc_output = list()
for (i in 1:group_no) {
    fc_main_desc_i <- fc_main_desc[[i]][4:(4 + fc_days),3:4]        
    fc_main_desc_output[[i]] = fc_main_desc_i
}
fc_main_desc_output

# Obtaining group doses
group_names_fc = paste(as.character(unique(fc_main$DOSE_LEVEL)), 'mg/kg')

# Obtaining dates
fc_dates <- rownames(fc_main_desc_output[[1]])

# Renaming the component names of bw_main_desc_output
names(fc_main_desc_output) = group_names_fc

# Transforming the fc_main_desc_output into a neat table
for (i in 1:length(fc_main_desc_output)) {
    fc_main_desc_output[[i]]$avg = paste(as.character(round(fc_main_desc_output[[i]]$mean)),
                                         as.character(round(fc_main_desc_output[[i]]$sd)), sep=' \U00b1 ')}
output_temp1_fc <- lapply(fc_main_desc_output, function(x) {x[,3]})
output_temp2_fc <- lapply(output_temp1_fc, function(x) {t(x)})
fc_main_desc_output <- ldply(output_temp2_fc)
colnames(fc_main_desc_output) <- c('DOSE', fc_dates)
print(fc_main_desc_output)


# Outputing average and SD summary data for report drafting
# First, the average data
fc_main_avg_output = list()
for (i in 1:group_no) {
    fc_main_avg_i <- round(fc_main_desc[[i]][4:(4 + fc_days),3], digits = 0)    
    fc_main_avg_output[[i]] = fc_main_avg_i
}
names(fc_main_avg_output) = group_names_fc
fc_main_avg_output <- ldply(fc_main_avg_output)
colnames(fc_main_avg_output) <- c('DOSE', fc_dates)

# Second, the SD data
fc_main_sd_output = list()
for (i in 1:group_no) {
    fc_main_sd_i <- round(fc_main_desc[[i]][4:(4 + fc_days),4], digits = 0)        
    fc_main_sd_output[[i]] = fc_main_sd_i
}
names(fc_main_sd_output) = group_names_fc
fc_main_sd_output <- ldply(fc_main_sd_output)
colnames(fc_main_sd_output) <- c('DOSE', fc_dates)


# Creating faceted overview of body weight changes over all days for

# Starting by re-creating mean+sd data
fc_main_mean = list()
fc_main_sd = list()
for (i in 1:group_no) {
    fc_main_mean[[i]] <- fc_main_desc[[i]][4:(4 + fc_days),3]
    fc_main_sd[[i]] <- round(fc_main_desc[[i]][4:(4 + fc_days),4], digits = 2)
}

fc_main_chart <- data.frame(unlist(fc_main_mean))
colnames(fc_main_chart) = 'mean'
fc_main_sd <- data.frame(unlist(fc_main_sd))
colnames(fc_main_sd) = 'sd'
fc_main_chart$sd = unlist(fc_main_sd)
fc_main_chart$day = fc_dates
fc_main_chart$dose = rep(group_names, each = ncol(fc_main_desc_output)-1)

# Creating a bar graph go compare chronological food consumption changes 
fc_main_chart_sans_sum <- subset(fc_main_chart, day!='SUM')
library(ggplot2)
fc_main_chart_sans_sum$dose <- factor(fc_main_chart_sans_sum$dose, levels = unique(fc_main_chart_sans_sum$dose))
fc_main_chart_facet <- ggplot(fc_main_chart_sans_sum, aes(x = day, y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, size = 0.5) + geom_bar(stat = 'identity', width = 0.75, fill = 'burlywood', color = 'black') + facet_grid(~ dose) + scale_y_continuous(limits = c(0,60), breaks = seq(from = 0, to = 60, by = 10), name = 'Mean Food Consumption per cage (g)') + theme(axis.title.x = element_blank())
fc_main_chart_facet


# Creating a line plot to compare group food consumption changes
pd <- position_dodge(0.1)
# Predefining the position dodge value so that
# the plot command below gets neater
fc_main_chart_allinone <- ggplot(fc_main_chart_sans_sum, aes(x = day, y = mean, group = dose)) +  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, size = 0.5, position = pd) + geom_line(stat = 'identity', size =0.5, aes(color = dose)) + scale_y_continuous(name = "Mean Food Consumption per cage (g)", limits = c(0,60), breaks = seq(from = 0, to = 60, by = 10)) + geom_point(aes(color = dose), size = 4, position = pd) + theme(axis.title.x = element_blank()) + guides(color = guide_legend(title = ""))
# Placing the errorbar parts before lines and points
fc_main_chart_allinone

# Creating a bar graph to compare group food consumption changes over all dosing days
fc_main_sum <- subset(fc_main_chart, day == 'SUM')
fc_main_sum$dose <- factor(fc_main_sum$dose, levels = unique(fc_main_sum$dose))
fc_main_chart_sum <- ggplot(fc_main_sum, aes(x = dose, y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean +sd), width = 0.5, size = 0.5) + geom_bar(stat = 'identity', width = 0.75, fill = 'burlywood', color = 'black') + scale_y_continuous(name = 'Mean Food Consumption per cage (g)') + xlab('Through All Dosing Days')
fc_main_chart_sum





# Importing clin-path data from excel files

cp <- read.xlsx(filename, sheetIndex = 4)

# Excluding null rows
cp_main <- subset(cp, cp[4] > 0)

# Obtaining valid group numbers
group_no_cp = length(unique(cp_main$DOSE_LEVEL))

# Obtaining information of numbers of indices to test
cp_number = ncol(cp_main) - 3

# Obtaining descriptive statistics of all indices
cp_main$DOSE_LEVEL <- factor(cp_main$DOSE_LEVEL, levels = unique(cp_main$DOSE_LEVEL))
cp_main_desc <- describeBy(cp_main, cp_main$DOSE_LEVEL)
cp_main_desc_output = list()
for (i in 1:group_no_cp) {
    cp_main_desc_i <- cp_main_desc[[i]][4:nrow(cp_main_desc[[i]]),3:4]        
    cp_main_desc_output[[i]] = cp_main_desc_i
}
cp_main_desc_output

# Obtaining group doses
group_names_cp = paste(as.character(unique(cp_main$DOSE_LEVEL)), 'mg/kg')

# Obtaining names of indices 
cp_indices <- rownames(cp_main_desc_output[[1]])

# Renaming the component names of bw_main_desc_output
names(cp_main_desc_output) = group_names_cp

# Outputing average and SD summary data for report drafting
# First, the average data
cp_main_avg_output = list()
for (i in 1:group_no_cp) {
    cp_main_avg_i <- round(cp_main_desc[[i]][4:(3 + length(cp_indices)),3], digits = 2)    
    cp_main_avg_output[[i]] = cp_main_avg_i
}
names(cp_main_avg_output) = group_names_cp
cp_main_avg_output <- ldply(cp_main_avg_output)
colnames(cp_main_avg_output) <- c('DOSE', cp_indices)

# Second, the SD data
cp_main_sd_output = list()
for (i in 1:group_no_cp) {
    cp_main_sd_i <- round(cp_main_desc[[i]][4:(3 + length(cp_indices)),4], digits = 2)    
    cp_main_sd_output[[i]] = cp_main_sd_i
}
names(cp_main_sd_output) = group_names_cp
cp_main_sd_output <- ldply(cp_main_sd_output)
colnames(cp_main_sd_output) <- c('DOSE', cp_indices)


# Transforming the cp_main_desc_output into a neat table
for (i in 1:length(cp_main_desc_output)) {
    cp_main_desc_output[[i]]$avg = paste(as.character(round(cp_main_desc_output[[i]]$mean, digits = 2)),
                                         as.character(round(cp_main_desc_output[[i]]$sd, digits = 2)), sep=' \U00b1 ') }
output_temp1_cp <- lapply(cp_main_desc_output, function(x) {x[,3]})
output_temp2_cp <- lapply(output_temp1_cp, function(x) {t(x)})
cp_main_desc_output <- ldply(output_temp2_cp)

colnames(cp_main_desc_output) <- c('DOSE', cp_indices)
print(cp_main_desc_output)

# Conducting Levene's Test between different dose groups


cp_main_aov <- cp_main_desc_output
cp_main_aov[(group_no_cp+1),1] = c('LEVENE')
cp_main_aov[(group_no_cp+2),1] = c('ANOVA (p=)')
cp_main_aov[(group_no_cp+3),1] = c('KRUSKAL-WALLIS (p=)')
cp_main_aov[(group_no_cp+4),1] = c('- SUMMARY -')
cp_main_aov <- data.frame(lapply(cp_main_aov, as.character), stringsAsFactors = FALSE)
# I'm not sure about what happened here. bw_main_aov was unintentionally 
# changed into a dataframe with factor-type elements. 
# Therefore, I had to manually change them back into characters.

for (i in 1:length(cp_indices)) {
    cp_levene <- leveneTest(cp_main[,3+i] ~ as.character(cp_main$DOSE_LEVEL), center = mean)
    if (is.na(cp_levene[1,3]) == TRUE) {
        cp_main_aov[(group_no_cp+2),(i+1)] = c('-')
        cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
        cp_main_aov[(group_no_cp+1),(i+1)] = c('-')
    }
    else if (cp_levene[1,3] > 0.05) {
        cp_main_aov[(group_no_cp+1),(i+1)] = c('PASS')
        cp_aov <- aov(cp_main[,3+i] ~ as.character(DOSE_LEVEL), data = cp_main)
        cp_main_aov[(group_no_cp+2),(i+1)] = round(summary(cp_aov)[[1]][1,5], digits = 4)
        cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
    }
    else {
        if (0 %in% cp_main[,3+i]) {
            cp_main_incre <- cp_main[,3+i] + 1
            cp_levene_log_incre <- leveneTest(log(cp_main_incre) ~ as.character(cp_main$DOSE_LEVEL), center = mean)
            cp_levene_sqrt_incre <- leveneTest(sqrt(cp_main_incre) ~ as.character(cp_main$DOSE_LEVEL), center = mean)
            cp_levene_recip_incre <- leveneTest(1/(cp_main_incre) ~ as.character(cp_main$DOSE_LEVEL), center = mean)
            if (cp_levene_log_incre[1,3] > 0.05) {
                cp_aov <- aov(log(cp_main_incre) ~ as.character(cp_main$DOSE_LEVEL))
                cp_main_aov[(group_no_cp+2),(i+1)] = round(summary(cp_aov)[[1]][1,5], digits = 4)
                cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
                cp_main_aov[(group_no_cp+1),(i+1)] = c('~')
            }
            else if (cp_levene_sqrt_incre[1,3] > 0.05) {
                cp_aov <- aov(sqrt(cp_main_incre) ~ as.character(cp_main$DOSE_LEVEL))
                cp_main_aov[(group_no_cp+2),(i+1)] = round(summary(cp_aov)[[1]][1,5], digits = 4)
                cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
                cp_main_aov[(group_no_cp+1),(i+1)] = c('~')
            }
            else if (cp_levene_recip_incre[1,3] > 0.05) {
                cp_aov <- aov(1/cp_main_incre ~ as.character(cp_main$DOSE_LEVEL))
                cp_main_aov[(group_no_cp+2),(i+1)] = round(summary(cp_aov)[[1]][1,5], digits = 4)
                cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
                cp_main_aov[(group_no_cp+1),(i+1)] = c('~')            
            }
            else {cp_main_aov[(group_no_cp+1),(i+1)] = c('FAIL')
                  cp_main_aov[(group_no_cp+2),(i+1)] = c('-')
                  cp_kruskal <- kruskal.test(cp_main[,3+i] ~ cp_main$DOSE_LEVEL)
                  cp_main_aov[(group_no_cp+3),(i+1)] = round(cp_kruskal$p.value, digits = 4)
            }
        }
        else if (!0 %in% cp_main[,3+i]) {
            cp_levene_log <- leveneTest(log(cp_main[,3+i]) ~ as.character(cp_main$DOSE_LEVEL), center = mean)
            cp_levene_sqrt <- leveneTest(sqrt(cp_main[,3+i]) ~ as.character(cp_main$DOSE_LEVEL), center = mean)
            cp_levene_recip <- leveneTest(1/(cp_main[,3+i]) ~ as.character(cp_main$DOSE_LEVEL), center = mean)
            if (cp_levene_log[1,3] > 0.05) {
                cp_aov <- aov(log(cp_main[,3+i]) ~ as.character(DOSE_LEVEL), data = cp_main)
                cp_main_aov[(group_no_cp+2),(i+1)] = round(summary(cp_aov)[[1]][1,5], digits = 4)
                cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
                cp_main_aov[(group_no_cp+1),(i+1)] = c('~')
            }
            else if (cp_levene_sqrt[1,3] > 0.05) {
                cp_aov <- aov(sqrt(cp_main[,3+i]) ~ as.character(DOSE_LEVEL), data = cp_main)
                cp_main_aov[(group_no_cp+2),(i+1)] = round(summary(cp_aov)[[1]][1,5], digits = 4)
                cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
                cp_main_aov[(group_no_cp+1),(i+1)] = c('~')
            }
            else if (cp_levene_recip[1,3] > 0.05) {
                cp_aov <- aov(1/cp_main[,3+i] ~ as.character(DOSE_LEVEL), data = cp_main)
                cp_main_aov[(group_no_cp+2),(i+1)] = round(summary(cp_aov)[[1]][1,5], digits = 4)
                cp_main_aov[(group_no_cp+3),(i+1)] = c('-')
                cp_main_aov[(group_no_cp+1),(i+1)] = c('~')            
            }
            else {
                cp_main_aov[(group_no_cp+1),(i+1)] = c('FAIL')
                cp_main_aov[(group_no_cp+2),(i+1)] = c('-')
                cp_kruskal <- kruskal.test(cp_main[,3+i] ~ cp_main$DOSE_LEVEL)
                cp_main_aov[(group_no_cp+3),(i+1)] = round(cp_kruskal$p.value, digits = 4)
            }
        }
        }
}


# Sanity check
cp_main_aov

# Create a summary row
for (i in c(1:length(cp_indices))) {
    if (cp_main_aov[group_no_cp+2,i+1] == "-") {
# Adding a condition where the Leneve's test result is NULL
        if (is.na(as.numeric(cp_main_aov[group_no_cp+3, i+1])) == TRUE) {
            cp_main_aov[group_no_cp+4, i+1] = '---'
        }
        else if (as.numeric(cp_main_aov[group_no_cp+3,i+1]) - 0.001 < 0) {
            cp_main_aov[group_no_cp+4, i+1] = '***'
        }
        else if (as.numeric(cp_main_aov[group_no_cp+3,i+1]) - 0.01 < 0) {
            cp_main_aov[group_no_cp+4, i+1] = '**'
        }
        else if (as.numeric(cp_main_aov[group_no_cp+3,i+1]) - 0.05 < 0) {
            cp_main_aov[group_no_cp+4, i+1] = '*'
        }
        else {
            cp_main_aov[group_no_cp+4, i+1] = '---'        
        }
    }
    else if (as.numeric(cp_main_aov[group_no_cp+2,i+1]) - 0.001 < 0) {
        cp_main_aov[group_no_cp+4, i+1] = '***'
    }
    else if (as.numeric(cp_main_aov[group_no_cp+2,i+1]) - 0.01 < 0) {
        cp_main_aov[group_no_cp+4, i+1] = '**'
    }
    else if (as.numeric(cp_main_aov[group_no_cp+2,i+1]) - 0.05 < 0) {
        cp_main_aov[group_no_cp+4, i+1] = '*'
    }
    else {
        cp_main_aov[group_no_cp+4, i+1] = '---'
    }
}

# Sanity check
cp_main_aov



# Post-hoc statistical analysis for ANOVA (including non-significant results)
cp_main_posthoc <- cp_main_aov
cp_main_posthoc[1,2:ncol(cp_main_posthoc)] = "---"
for (i in c(1:length(cp_indices))) {
# Adding a consideration to treat NULL data
    if (paste(cp_main_posthoc[(group_no_cp+1):(group_no_cp+3), i+1], collapse='') == c('---')) {
        cp_main_posthoc[2:group_no_cp,i+1] = rep('---', times = group_no_cp - 1)
    }
    else if (cp_main_posthoc[group_no_cp+2, i+1] >= 0 & cp_main_posthoc[group_no_cp+1, i+1] == 'PASS') {
        cp_main_Tukey <- TukeyHSD(aov(cp_main[,3+i] ~ cp_main$DOSE_LEVEL))
        cp_main_posthoc[2:group_no_cp,i+1] = cp_main_Tukey[[1]][1:(group_no_cp-1),4]
    }
    else if (cp_main_posthoc[group_no_cp+2, i+1] >= 0 & cp_main_posthoc[group_no_cp+1, i+1] == '~') {
# Added quite a few lines here to tailor ANOVA on transformed data.
# Otherwise, there's no need for the transformation!!!
        cp_main_log <- leveneTest(log(cp_main[,3+i]) ~ as.character(DOSE_LEVEL), data = cp_main)
        cp_main_sqrt <- leveneTest(sqrt(cp_main[,3+i]) ~ as.character(DOSE_LEVEL), data = cp_main)
        cp_main_recip <- leveneTest(1/(cp_main[,3+i]) ~ as.character(DOSE_LEVEL), data = cp_main)
# Testing recip in the first place, as it, in many cases
# is more powerful than the other two transformations
        if (cp_main_recip[1,3] > 0.05) {
            cp_main_Tukey <- TukeyHSD(aov(1/cp_main[,3+i] ~ cp_main$DOSE_LEVEL))
            cp_main_posthoc[2:group_no_cp,i+1] = cp_main_Tukey[[1]][1:(group_no_cp-1),4]
        }
        else if (cp_main_log[1,3] > 0.05) {
            cp_main_Tukey <- TukeyHSD(aov(log(cp_main[,3+i]) ~ cp_main$DOSE_LEVEL))
            cp_main_posthoc[2:group_no_cp,i+1] = cp_main_Tukey[[1]][1:(group_no_cp-1),4]
        }
        else if (cp_main_sqrt[1,3] > 0.05) {
            cp_main_Tukey <- TukeyHSD(aov(sqrt(cp_main[,3+i]) ~ cp_main$DOSE_LEVEL))
            cp_main_posthoc[2:group_no_cp,i+1] = cp_main_Tukey[[1]][1:(group_no_cp-1),4]
        }
        
    }
    else if (cp_main_posthoc[group_no_cp+3, i+1] >= 0) {
        cp_Kruskal <-  kruskalmc(cp_main[,3+i] ~ cp_main$DOSE_LEVEL, prob = 0.05) 
        cp_main_posthoc[2:group_no_cp, i+1] = cp_Kruskal[[3]][1:(group_no_cp-1),3]
    }
}





# Sanity check
cp_main_posthoc

# Cleaning up the post-hoc table
for (i in 2:group_no_cp) {
    for (j in 2:(length(cp_indices)+1)) {
# Adding a condition for non-numeric values here.
        if (cp_main_posthoc[i,j] == 'FALSE') {cp_main_posthoc[i,j] = '---'}
        else if (cp_main_posthoc[i,j] == 'TRUE') {cp_main_posthoc[i,j] = 'p<0.05'}
        else if (cp_main_posthoc[i,j] == '---') {cp_main_posthoc[i,j] = '---'}
# The above line has to be inserted where it is now. If it is placed in the end,
# this section simply does not work.
# For such cases, always place logical judgment lines ahead.
        else if (as.numeric(cp_main_posthoc[i,j]) < 0.001) {cp_main_posthoc[i,j] = 'p<0.001'}
        else if (as.numeric(cp_main_posthoc[i,j]) < 0.01) {cp_main_posthoc[i,j] = 'p<0.01'}
        else if (as.numeric(cp_main_posthoc[i,j]) < 0.05) {cp_main_posthoc[i,j] = 'p<0.05'}
        else {cp_main_posthoc[i,j] = '---'}
    }    
}

# Sanity check
cp_main_posthoc    

# Creating a clean table with only significant results
cp_main_posthoc_clean <- data.frame(cp_main_posthoc[,1])
colnames(cp_main_posthoc_clean) = 'DOSE'
j = 1
for (i in 2:(length(cp_indices)+1)) {
    if (paste(cp_main_posthoc[1:group_no_cp,i], collapse='') == paste(rep('---', times = group_no_cp), collapse='')) {}
# Tried an array of conditions here and this seems simple and working ...
# And using a fixed length of '---...---' cannot meet the needs of studyes 
# where there are different number of groups
    else {
        cp_main_posthoc_clean[,j+1] = cp_main_posthoc[,i]
        colnames(cp_main_posthoc_clean)[j+1] = colnames(cp_main_posthoc)[i]
        j = j + 1
    }
}

# Sanity check
cp_main_posthoc_clean

# Obtaining the list of indices to be plotted
cp_indices_comp <- colnames(cp_main_posthoc_clean)[-1]


# Creating cp_main_comp which contains data
# that need to be plotted

cp_main_comp <- cp_main[,1:3]

for (i in 1:length(cp_indices)) {
    if (colnames(cp_main)[i+3] %in% cp_indices_comp) {
        cp_main_comp <- cbind(cp_main_comp, cp_main[,i+3])
    }
    else {}
}
colnames(cp_main_comp)[4:ncol(cp_main_comp)] = cp_indices_comp

# Sanity check
cp_main_comp

# Re-creating mean+sd data
cp_main_comp_desc <- describeBy(cp_main_comp, cp_main_comp$DOSE_LEVEL)
cp_main_comp_mean = list()
cp_main_comp_sd = list()
for (i in 1:group_no_cp) {
    cp_main_comp_mean[[i]] <- cp_main_comp_desc[[i]][4:(length(cp_indices_comp)+3),3]
    cp_main_comp_sd[[i]] <- round(cp_main_comp_desc[[i]][4:(length(cp_indices_comp)+3),4], digits = 2)
}

cp_main_comp_chart <- data.frame(unlist(cp_main_comp_mean))
colnames(cp_main_comp_chart) = 'mean'
cp_main_comp_sd <- data.frame(unlist(cp_main_comp_sd))
colnames(cp_main_comp_sd) = 'sd'
cp_main_comp_chart$sd = unlist(cp_main_comp_sd)

# Obtaining dose numbers
dose_levels_cp <- unique(cp_main$DOSE_LEVEL)

cp_main_comp_chart$indices = cp_indices_comp
cp_main_comp_chart$dose = rep(dose_levels_cp, each = length(cp_indices_comp))

# Sanity check
cp_main_comp_chart

# Creating faceted overview of body weight changes over all days for
# individual dose groups
library(ggplot2)
cp_main_comp_facet <- ggplot(cp_main_comp_chart, aes(x = as.factor(dose), y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, size = 0.5) + geom_bar(stat = 'identity', width = 0.75, fill = 'burlywood', color = 'black') + facet_wrap(~ indices, ncol = 3, scales = 'free') + ylab('') + xlab('Dose (mg/kg/day)')
cp_main_comp_facet

# Event reporting

# Loading event information
event <- read.xlsx(filename, sheetIndex = 5)

# Ditching all clean ones in clinical observation
if (paste((unique(event[,4] == 'NA')), sep='', collapse='') == "TRUE") {
    clin_obs <- matrix(nrow = 1, ncol = 1)
    clin_obs = 'NO EVENTS TO REPORT'
} else {
    # 'else {' has to be strictly following }
    # otherwise it won't work.
    event_clin <- subset(event, event[,4] != 'NA')
    # Writing all information into a vector
    clin_obs <- matrix(nrow = nrow(event_clin), ncol = 1)
    for (i in 1:nrow(event_clin)) {
        clin_obs[i] <- paste('Animal ', event_clin[i,1], ' (', ifelse(event_clin[i,3] == 'YES', 'TK animal)  ', 'main animal)'), ': ', event_clin[i,4], sep = "")}
}

# Priting out a nice summary of the clinical observations
write.table(format(clin_obs, justify='left'), row.names=F, col.names=F, quote=F)

# Ditching all clean ones in gross findings in necropsy
if (paste((unique(event[,5] == 'NA')), sep='', collapse='') == "TRUE") {
    necro_obs <- matrix(nrow = 1, ncol = 1)
    necro_obs = 'NO EVENTS TO REPORT'
} else {
    event_necro <- subset(event, event[,5] != 'NA')
    necro_obs <- matrix(nrow = nrow(event_necro), ncol = 1)
    for (i in 1:nrow(event_necro)) {
        necro_obs[i] <- paste('Animal ', event_necro[i,1], ' (', ifelse(event_necro[i,3] == 'YES', 'TK animal)  ', 'main animal)'), ' - ', event_necro[i,5], sep = "")}
}

# Priting out a nice summary of the necrospy observations
write.table(format(necro_obs, justify='left'), row.names=F, col.names=F, quote=F)


# Reviewing key tables and figures

bw_main_desc_output
bw_main_posthoc
bw_main_chart_facet
bw_main_chart_allinone

bwg_main_desc_output
bwg_main_posthoc
bwg_main_chart_facet
bwg_main_chart_all
bwg_main_chart_allinone


fc_main_desc_output
fc_main_chart_facet
fc_main_chart_allinone
fc_main_chart_sum

cp_main_desc_output
cp_main_posthoc_clean
cp_main_comp_facet



# Exporting to an excel sheet to create tables for word reports

# Exporting summary data of body weight, body weight gain, food 
# consumption , and clin path

# Using the 'interleave' function from the gdata package
library(gdata)
bw_avg_sd_output <- interleave(bw_main_avg_output, bw_main_sd_output)
bwg_avg_sd_output <- interleave(bwg_main_avg_output, bwg_main_sd_output)
fc_avg_sd_output <- interleave(fc_main_avg_output, fc_main_sd_output)
cp_avg_sd_output <- interleave(cp_main_avg_output, cp_main_sd_output)

# Deleting pre-existing excel output files
file.remove("output_individual.xlsx")
file.remove("output_summary.xlsx")

write.xlsx(bw_avg_sd_output, "output_summary.xlsx", col.names=T, row.names=F, showNA=T, sheetName='BW', append=T)

write.xlsx(bwg_avg_sd_output, "output_summary.xlsx", col.names=T, row.names=F, showNA=T, sheetName='BWG', append=T)

write.xlsx(fc_avg_sd_output, "output_summary.xlsx", col.names=T, row.names=F, showNA=T, sheetName='FC', append=T)

write.xlsx(cp_avg_sd_output, "output_summary.xlsx", col.names=T, row.names=F, showNA=T, sheetName='CP', append=T)

# Exporting statistical analysis for all summaries above sans FC

write.xlsx(bw_main_posthoc, "output_summary.xlsx", col.names=T, row.names=F, showNA=T, sheetName='BW-stat', append=T)

write.xlsx(bwg_main_posthoc, "output_summary.xlsx", col.names=T, row.names=F, showNA=T, sheetName='BWG-stat', append=T)

write.xlsx(cp_main_posthoc, "output_summary.xlsx", col.names=T, row.names=F, showNA=T, sheetName='CP-stat', append=T)

# Exporting individual data for body weight, body weight gain, food
# consumption, and clin path

write.xlsx(bw_main, "output_individual.xlsx", col.names=T, row.names=F, showNA=T, sheetName='BW', append=T)

write.xlsx(bwg_main, "output_individual.xlsx", col.names=T, row.names=F, showNA=T, sheetName='BWG', append=T)

write.xlsx(fc_main, "output_individual.xlsx", col.names=T, row.names=F, showNA=T, sheetName='FC', append=T)

write.xlsx(cp_main, "output_individual.xlsx", col.names=T, row.names=F, showNA=T, sheetName='CP', append=T)

####### PLAYGROUND ####### 
####### PLAYGROUND ####### 
####### PLAYGROUND ####### 
####### PLAYGROUND ####### 

# Boxplots for body weight data
bw_main_chart_boxplot <- ggplot(bw_main_chart, aes(x = day, y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, size = 0.5) + geom_boxplot() + facet_grid(~ dose) + scale_y_continuous(name = 'Mean Body Weight (g)') + theme(axis.title.x = element_blank()) + scale_x_discrete(labels = c(1:bw_days))  + coord_flip() + xlab('Day')
bw_main_chart_boxplot 

# Creating a table to show the percent changes in BW and FC

# First about body weight
# Creating bw_main_per to calculate percent changes in BW compared to Day 1

bw_main_per <- bw_main[,1:(bw_days+4)]
for (i in 2:bw_days) {
    bw_main_per[,i+4] = round((bw_main_per[,i+4] / bw_main_per[,5] - 1) * 100, digits = 1)
}
bw_main_per[,5] = 0

# Creating an output for body weight changes in percentage
bw_main_per_desc <- describeBy(bw_main_per, bw_main_per$DOSE_LEVEL)
bw_main_per_output = list()
for (i in 1:group_no) {
    bw_main_per_desc_i <- bw_main_per_desc[[i]][5:(4 + bw_days),3:4]        
    bw_main_per_output[[i]] = bw_main_per_desc_i
}
names(bw_main_per_output) = group_names

# Packing the output
for (i in 1:length(bw_main_per_output)) {
    bw_main_per_output[[i]]$avg = paste(as.character(round(bw_main_per_output[[i]]$mean, digits = 1)),
                                         as.character(round(bw_main_per_output[[i]]$sd, digits = 1)), sep=' ¡À ') }
output_temp1 <- lapply(bw_main_per_output, function(x) {x[,3]})
output_temp2 <- lapply(output_temp1, function(x) {t(x)})
bw_main_per_output <- ldply(output_temp2)

colnames(bw_main_per_output) <- c('DOSE', bw_dates)
print(bw_main_per_output)

# Reorganizing the data for plotting
bw_main_per_desc <- describeBy(bw_main_per, bw_main_per$DOSE_LEVEL)
bw_main_per_mean = list()
bw_main_per_sd = list()
for (i in 1:group_no) {
    bw_main_per_mean[[i]] <- bw_main_per_desc[[i]][5:(4 + bw_days),3]
    bw_main_per_sd[[i]] <- round(bw_main_per_desc[[i]][5:(4 + bw_days),4], digits = 2)
}

bw_main_per_chart <- data.frame(unlist(bw_main_per_mean))
colnames(bw_main_per_chart) = 'mean'
bw_main_per_sd <- data.frame(unlist(bw_main_per_sd))
colnames(bw_main_per_sd) = 'sd'
bw_main_per_chart$sd = unlist(bw_main_per_sd)
bw_main_per_chart$day = bw_dates
bw_main_per_chart$dose = rep(group_names, each = count_aov)

# Plotting for body weight change percentages

bw_main_per_chart_facet <- ggplot(bw_main_per_chart, aes(x = day, y = mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5, size = 0.5) + geom_bar(stat = 'identity', width = 0.75, fill = 'burlywood', color = 'black') + facet_grid(~ dose) + scale_y_continuous(name = '% Changes in Body Weight Compared to Day 1', limits = c(-15, 20)) + scale_x_discrete(label = c(substring(bw_dates[1:length(bw_dates)], 5, length(bw_dates)))) + xlab('Day')

p <- bw_main_per_chart_facet

# Adding a rectangular annotation
p <- p + annotate('rect', ymin = -12, ymax = -2, xmin = 1, xmax = 8, alpha = 0.2)
p

# Also adding a line to label the "danger zone"
p <- p + geom_hline(aes(yintercept=-10), color = 'black', linetype = 'dashed')
p

# Creatiing a new dataframe and then applying it to create a annotation on one facet
x = c(4,2,2,2)
y = c(-12, -12, -12, -12)
dose = group_names
labs = c('10% reduction', '', '', '')
# Using empty labs elements to circumvent the issue of applying a label to
# only one of the many facets of this plot
anno_info = data.frame(x, y, dose, labs)

p1 <- p + geom_text(aes(x, y, label = labs, group = NULL), data = anno_info)
p1

# Creating a line plot to compare group body weight data
pd <- position_dodge(0.1)
x_names <- unique(bw_main_per_chart$day)
x_names_rest <- x_names[-1]

bw_main_per_chart_allinone <- ggplot(bw_main_per_chart, aes(x = day, y = mean, group = dose)) +  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, size = 0.5, position = pd) + geom_line(stat = 'identity', size =0.5, aes(color = dose)) + scale_y_continuous(name = "% Changes in Body Weight Compared to Day 1", limits = c(-15, 20)) + geom_point(aes(color = dose), size = 4, position = pd) + theme(axis.title.x = element_blank()) + guides(color = guide_legend(title = "")) + scale_x_discrete(labels = c(x_names[1], substring(x_names_rest, 5, 6)))
# Placing the errorbar parts before lines and points
bw_main_per_chart_allinone

p <- bw_main_per_chart_allinone

# Creating lines at 10% reduction and annotating
p1 <- p + geom_hline(aes(yintercept = -10), color = 'black', linetype = 'dashed') + annotate('text', x = 3.6, y = -12, label = '>10% reduction compared to Day 1', size = 4.5) + annotate('rect', xmin = 0, xmax = 9, ymin = -15, ymax = -10, alpha = 0.2)
p1


# Now working on body weight gain against the vehicle group

# Creating an output for body weight changes in percentage
bw_main_veh_desc <- describeBy(bw_main, bw_main$DOSE_LEVEL)
bw_main_veh_output = list()
for (i in 1:group_no) {
    bw_main_veh_output_i <- bw_main_veh_desc[[i]][5:(4 + bw_days),3]        
    bw_main_veh_output[[i]] = bw_main_veh_output_i
}
names(bw_main_veh_output) = group_names

# Packing the output
bw_main_veh_output <- ldply(bw_main_veh_output)
colnames(bw_main_veh_output) <- c('DOSE', bw_dates)
print(bw_main_veh_output)

# Calculating percent changes against the vehicle group within individual days
for (i in 2:group_no) {
    bw_main_veh_output[i,2:(bw_days+1)] = round((bw_main_veh_output[i,2:(bw_days+1)] / bw_main_veh_output[1,2:(bw_days+1)] - 1) * 100, digits = 1)
} 
bw_main_veh_output[1,2:(bw_days+1)] = 0


# Reorganizing the data for plotting
bw_main_veh_chart <- stack(bw_main_veh_output[,2:ncol(bw_main_veh_output)])
bw_main_veh_chart$dose = rep(group_names, times = bw_days)
colnames(bw_main_veh_chart)[1:2] = c('value', 'day')

# Plotting to compare body weight changes against 
bw_main_veh_chart_allinone <- ggplot(bw_main_veh_chart, aes(x = day, y = value, group = dose)) + geom_line(stat = 'identity', size = 0.5, aes(color = dose)) + scale_y_continuous(name = "% Changes in Body Weight Compared to the Vechile Group", limits = c(-15,0))  + geom_point(aes(color = dose), size = 4, position = pd) + theme(axis.title.x = element_blank()) + guides(color = guide_legend(title = "")) + scale_x_discrete(labels = c(x_names[1], substring(x_names_rest, 5, 6))) + geom_hline(aes(yintercept = -10), color = 'black', linetype = 'dashed') + annotate('text', x = 4, y = -11, label = '10% reduction compared to the vehicle')



# Then about food consumption
# Comparing food consumption against Day 1 within a dose group
# is of limited use.

# Now working on food consumption against the vehicle group

# Creating an output for food consumption changes in percentage
fc_main_veh_desc <- describeBy(fc_main, fc_main$DOSE_LEVEL)
fc_main_veh_output = list()
for (i in 1:group_no) {
    fc_main_veh_output_i <- fc_main_veh_desc[[i]][5:(4 + fc_days),3]        
    fc_main_veh_output[[i]] = fc_main_veh_output_i
}
names(fc_main_veh_output) = group_names

# Packing the output
fc_main_veh_output <- ldply(fc_main_veh_output)
colnames(fc_main_veh_output) <- c('DOSE', fc_dates[-1])
print(fc_main_veh_output)

# Calculating percent changes against the vehicle group within individual days
for (i in 2:group_no) {
    fc_main_veh_output[i,2:(fc_days+1)] = round((fc_main_veh_output[i,2:(fc_days+1)] / fc_main_veh_output[1,2:(fc_days+1)] - 1) * 100, digits = 1)
} 
fc_main_veh_output[1,2:(fc_days+1)] = 0


# Reorganizing the data for plotting
fc_main_veh_chart <- stack(fc_main_veh_output[,2:ncol(fc_main_veh_output)])
fc_main_veh_chart$dose = rep(group_names, times = fc_days)
colnames(fc_main_veh_chart)[1:2] = c('value', 'day')

# Plotting to compare body weight changes against 
# Using '\n' to wrap a very long label of an axis
fc_main_veh_chart_allinone <- ggplot(fc_main_veh_chart, aes(x = day, y = value, group = dose)) + geom_line(stat = 'identity', size = 0.5, aes(color = dose)) + scale_y_continuous(name = "% Changes in Food Consumption \n Compared to the Vechile Group", limits = c(-100,10))  + geom_point(aes(color = dose), size = 4, position = pd) + theme(axis.title.x = element_blank()) + guides(color = guide_legend(title = "")) + scale_x_discrete(labels = c(x_names[1], substring(x_names_rest, 5, 6))) 



# Playing with rCharts
# Update: not deployed due to messy java stuffs

