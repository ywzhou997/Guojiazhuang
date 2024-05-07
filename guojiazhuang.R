#===================================
# Title: guojiazhuang
# Date: March 1, 2022
# Updated: October 25, 2023
# Author: Yuwei Zhou 周毓葦
# Source: 
#     CSSA-IA. 1998. Anyang Yinxu Guojiazhuang Shangdai Muzang 安陽殷墟郭家莊商代墓葬.
#         Beijing: Zhongguo dabaikequanshu chubanshe. 
#     Zhu, Fenghan. 2004. “Anyang Yinxu Guojiazhuang Wanshang Mudi Chushi 安阳殷墟郭家庄
#         晚商墓地初识.” In Shangzhou Jiazu Xingtai Yanjiu 商周家族形态研究, 583–97. 
#         天津: 天津古籍出版社. 
#     Tang, Jigen. 2004. “The Social Organization of Late Shang China—A Mortuary Perspective.” 
#         London: University College of London.
# Introduction: This file is the final project for Data Analysis for Digital
# Humanities. This project studies tombs in Guojiazhuang, Anyang. I mainly
# tackles the problem of grouping: how should we effectively group them, and by
# what criteria? Possible factors that I have been considering about: the length,
# width, the depth, and the length to width ratio of the tomb, the coffin, and
# the waist pit; the orientation, the prescence/abscence of waits pits, the
# burial style, the period. A GIS component of this project is ideal but might be
# hard to incorporate due to the shortage of time
#====================================

#install.packages("ggbeeswarm")
#install.packages("grid")
#install.packages("ggbiplot")


# loading library
library(readxl)
library(xlsx)
library(dplyr)
library(ggplot2)
library(scales)
library(devtools)
library(showtext)
library(ggbiplot)
library(DAAG)
library(plyr)
library(stringr)
library(grid)
library(gridExtra)
library(randomcoloR)
library(ggbeeswarm)
showtext_auto()

# ============Prep: Data Cleaning1================
# Data Cleaning: this section reads in the raw data and perform parsing and cleaning
# Input: 郭家莊墓葬copy.xlsx
# Output: 郭家莊墓葬形制31.xlsx

# Read in and clean up Guojiazhuang_muzang
setwd("/Users/zhouyuwei/Desktop/Guojiazhuang")
guojiazhuang_muzang_raw <-
  read_excel("./郭家莊墓葬copy.xlsx",
             col_types = c(rep("text", 25)))
head(guojiazhuang_muzang_raw)

# Clean up data
for (i in 1:nrow(guojiazhuang_muzang_raw)) {
  temp <- guojiazhuang_muzang_raw[i, ]
  if (!is.na(temp$tomb_dim)) {  #Parsing tomb dimension
    h <- strsplit(temp$tomb_dim, '+', fixed = TRUE)[[1]][1]
    str2 <- strsplit(temp$tomb_dim, '+', fixed = TRUE)[[1]][2]
    w <- strsplit(str2, '-', fixed = TRUE)[[1]][1]
    d <- strsplit(str2, '-', fixed = TRUE)[[1]][2]
    guojiazhuang_muzang_raw$tomb_length[i] <- h
    guojiazhuang_muzang_raw$tomb_width[i] <- w
    guojiazhuang_muzang_raw$tomb_depth[i] <- d
  } else{
    guojiazhuang_muzang_raw$tomb_length[i] <- NA
    guojiazhuang_muzang_raw$tomb_width[i] <- NA
    guojiazhuang_muzang_raw$tomb_depth[i] <- NA
  }
  
  if (!is.na(temp$coffin_dim)) {  #Parsing coffin dimension
    h <- strsplit(temp$coffin_dim, '+', fixed = TRUE)[[1]][1]
    str2 <- strsplit(temp$coffin_dim, '+', fixed = TRUE)[[1]][2]
    w <- strsplit(str2, '-', fixed = TRUE)[[1]][1]
    d <- strsplit(str2, '-', fixed = TRUE)[[1]][2]
    guojiazhuang_muzang_raw$coffin_length[i] <- h
    guojiazhuang_muzang_raw$coffin_width[i] <- w
    guojiazhuang_muzang_raw$coffin_depth[i] <- d
  } else{
    guojiazhuang_muzang_raw$coffin_length[i] <- NA
    guojiazhuang_muzang_raw$coffin_width[i] <- NA
    guojiazhuang_muzang_raw$coffin_depth[i] <- NA
  }
  
  if (!is.na(temp$waist_pit_dim)) {  #Parsing waist pit dimension
    h <- strsplit(temp$waist_pit_dim, '+', fixed = TRUE)[[1]][1]
    str2 <- strsplit(temp$waist_pit_dim, '+', fixed = TRUE)[[1]][2]
    w <- strsplit(str2, '-', fixed = TRUE)[[1]][1]
    d <- strsplit(str2, '-', fixed = TRUE)[[1]][2]
    guojiazhuang_muzang_raw$waist_pit_length[i] <- h
    guojiazhuang_muzang_raw$waist_pit_width[i] <- w
    guojiazhuang_muzang_raw$waist_pit_depth[i] <- d
  } else{
    guojiazhuang_muzang_raw$waist_pit_length[i] <- NA
    guojiazhuang_muzang_raw$waist_pit_width[i] <- NA
    guojiazhuang_muzang_raw$waist_pit_depth[i] <- NA
  }
  
  if (!is.na(temp$burial_style)) {  # simplify burial styles
    if (grepl("俯", temp$burial_style)) {
      guojiazhuang_muzang_raw$burial_style[i] <- "俯"
    }
    if (grepl("仰", temp$burial_style)) {
      guojiazhuang_muzang_raw$burial_style[i] <- "仰"
    }
    if (grepl("屈", temp$burial_style)) {
      guojiazhuang_muzang_raw$burial_style[i] <- "屈"
    }
  }
  
  if (!is.na(temp$orientation)) {   #simplify orientation
    if (grepl("/", temp$orientation)) {
      guojiazhuang_muzang_raw$orientation[i] <-
        strsplit(temp$orientation, '/', fixed = TRUE)[[1]][1]
    }
  }
}
guojiazhuang_muzang_raw[, c("tomb_dim", "coffin_dim", "waist_pit_dim",
                            "note")] <- list(NULL)
guojiazhuang_muzang_raw[guojiazhuang_muzang_raw == "?"] <- NA


guojiazhuang_muzang_raw %>% filter(!num == "122", !num == "123") %>%
  write.xlsx2(showNA = FALSE,"./郭家莊墓葬形制31.xlsx")



# ==========Prep:Data Cleaning and Primary Analysis================
# This section preforms data analysis on the cleaned-up morturary data of Guojiazhuang
# Input: 郭家莊墓葬31.xlsx
# Output: 郭家莊墓葬形制44.xlsx

# Read in the cleaned data guojiazhuang_muzang
guojiazhuang_muzang <-
  read_excel("./郭家莊墓葬形制31.xlsx", col_types = c(rep("text", 31)))
head(guojiazhuang_muzang)

# taking out unexcavated tombs M122, 123
guojiazhuang_muzang <- guojiazhuang_muzang[, -1]
guojiazhuang_muzang <-
  guojiazhuang_muzang %>% subset(!(
    guojiazhuang_muzang$num == "122" |
      guojiazhuang_muzang$num == "123"
  ))
head(guojiazhuang_muzang)

# Orientation
# Four groups can be clearly identified
summary(as.numeric(guojiazhuang_muzang$orientation))

# Histogram shows four distinct tomb orientation
guojiazhuang_muzang %>%
  filter(!is.na(orientation)) %>%
  ggplot(aes(x = as.numeric(orientation))) +
  geom_histogram(binwidth = 1) +
  xlab("orientation") + ylab("Frequency") +
  theme_minimal()+
  ggtitle("Guojiazhuang Tomb Orientation")

# Enter categorical data for orientation
for (i in 1:nrow(guojiazhuang_muzang)) {
  if (!is.na(guojiazhuang_muzang$orientation[i])) {
    if (as.numeric(guojiazhuang_muzang$orientation[i]) > 350 |
        as.numeric(guojiazhuang_muzang$orientation[i]) < 50) {
      guojiazhuang_muzang$orientation_des[i] <- "NE"
    } else if(as.numeric(guojiazhuang_muzang$orientation[i]) > 50 &
              as.numeric(guojiazhuang_muzang$orientation[i]) < 150){
      guojiazhuang_muzang$orientation_des[i] <- "SE"
    } else if(as.numeric(guojiazhuang_muzang$orientation[i]) > 150 &
              as.numeric(guojiazhuang_muzang$orientation[i]) < 240){
      guojiazhuang_muzang$orientation_des[i] <- "SW"
    } else if(as.numeric(guojiazhuang_muzang$orientation[i]) > 240 &
            as.numeric(guojiazhuang_muzang$orientation[i]) < 300){
      guojiazhuang_muzang$orientation_des[i] <- "NW"
    }
  } else{
    guojiazhuang_muzang$orientation_des[i] <- NA
  }
}

guojiazhuang_muzang %>%
  filter(!is.na(orientation)) %>%
  ggplot(aes(x = as.numeric(orientation), fill = orientation_des)) +
  geom_histogram(color = "white", binwidth = 5) +
  xlab("orientation") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Orientation") + 
  theme_minimal()+
  scale_fill_discrete(name = "Tomb Orientation")

#alternative visualization
guojiazhuang_muzang %>%
  filter(!is.na(orientation)) %>%
  ggplot(aes(x = as.numeric(orientation), fill= orientation_des)) +
  geom_histogram(color = "white", binwidth = 5) +
  ylim(-20,25) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 10), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0, 90, 180, 270)) +
  coord_polar(start = 0) +
  xlab("Orientation") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Orientation")

# Calculate the area for tomb, coffin, and waist pit
for (i in 1:nrow(guojiazhuang_muzang)) {
  if (!is.na(guojiazhuang_muzang$tomb_length[i]) &
      !is.na(guojiazhuang_muzang$tomb_width[i])) {
    guojiazhuang_muzang$tomb_area[i] <-
      as.numeric(guojiazhuang_muzang$tomb_length[i]) *
      as.numeric(guojiazhuang_muzang$tomb_width[i])
    guojiazhuang_muzang$tomb_LWratio[i] <-
      as.numeric(guojiazhuang_muzang$tomb_length[i]) /
      as.numeric(guojiazhuang_muzang$tomb_width[i])
  } else {
    guojiazhuang_muzang$tomb_area[i] <- NA
    guojiazhuang_muzang$tomb_LWratio[i] <- NA
  }
  if (!is.na(guojiazhuang_muzang$coffin_length[i]) &
      !is.na(guojiazhuang_muzang$coffin_width[i])) {
    guojiazhuang_muzang$coffin_area[i] <-
      as.numeric(guojiazhuang_muzang$coffin_length[i]) *
      as.numeric(guojiazhuang_muzang$coffin_width[i])
    guojiazhuang_muzang$coffin_LWratio[i] <-
      as.numeric(guojiazhuang_muzang$coffin_length[i]) /
      as.numeric(guojiazhuang_muzang$coffin_width[i])
  } else{
    guojiazhuang_muzang$coffin_area[i] <- NA
    guojiazhuang_muzang$coffin_LWratio[i] <- NA
  }
  if (!is.na(guojiazhuang_muzang$waist_pit_length[i]) &
      !is.na(guojiazhuang_muzang$waist_pit_width[i])) {
    guojiazhuang_muzang$waist_pit_area[i] <-
      as.numeric(guojiazhuang_muzang$waist_pit_length[i]) *
      as.numeric(guojiazhuang_muzang$waist_pit_width[i])
    guojiazhuang_muzang$waist_pit_LWratio[i] <-
      as.numeric(guojiazhuang_muzang$waist_pit_length[i]) /
      as.numeric(guojiazhuang_muzang$waist_pit_width[i])
  } else{
    guojiazhuang_muzang$waist_pit_area[i] <- NA
    guojiazhuang_muzang$waist_pit_LWratio[i] <- NA
  }
}

# number of human sacrifice
for (i in 1:nrow(guojiazhuang_muzang)) {
  if (is.na(guojiazhuang_muzang$hum_sacrifice[i])) {
    guojiazhuang_muzang$hum_sacrifice[i] <- as.numeric(0)
  }
  else {
    guojiazhuang_muzang$hum_sacrifice[i] <- as.numeric(guojiazhuang_muzang$hum_sacrifice[i])
  }
}
guojiazhuang_muzang$hum_sacrifice <- as.numeric(guojiazhuang_muzang$hum_sacrifice)

# Presence/absence of waist pit
for (i in 1:nrow(guojiazhuang_muzang)) {
  if (is.na(guojiazhuang_muzang$waist_pit_area[i])) {
    guojiazhuang_muzang$waist_pit[i] <- FALSE
  }
  else {
    guojiazhuang_muzang$waist_pit[i] <- TRUE
  }
}

# Present/absence of burial dog and position
for (i in 1:nrow(guojiazhuang_muzang)) {
  if (!is.na(guojiazhuang_muzang$burial_dog_waist[i]) || (!is.na(guojiazhuang_muzang$burial_dog_filling[i])) || (!is.na(guojiazhuang_muzang$burial_dog_ercengtai[i]))) {
    guojiazhuang_muzang$burial_dog[i] <- TRUE
  }
  else {
    guojiazhuang_muzang$burial_dog[i] <- FALSE
  }
}

# Number of bronze ritual vessels
guojiazhuang_muzang$ritual_bronze <- NA
vessel_list <- c("铜觚", "铜鼎", "铜爵", "铜簋", "铜尊", "铜斝", "铜卣", "铜甗", "铜觥", "铜觯")
for (i in 1:nrow(guojiazhuang_muzang)) {
  if (!is.na(guojiazhuang_muzang$burial_good_bronze[i])) {
    bronze_list <- str_split(guojiazhuang_muzang$burial_good_bronze[i], "[.]")[[1]]
    temp <- c()
    k = 0
    for(j in vessel_list){
      if(sum(str_count(bronze_list, j)) > 0){
        temp <- c(temp, rep(j, sum(str_count(bronze_list, j))))
        k = k+sum(str_count(bronze_list, j))
      }
    }
    if(!is.null(temp)){
      guojiazhuang_muzang$ritual_bronze_type[i] <- list(temp)
      guojiazhuang_muzang$ritual_bronze_num[i] <- k 
      guojiazhuang_muzang$ritual_bronze[i] <- TRUE
    } else{
      guojiazhuang_muzang$ritual_bronze_type[i] <- NA
      guojiazhuang_muzang$ritual_bronze_num[i] <- 0
      guojiazhuang_muzang$ritual_bronze[i] <- FALSE
    }
  } else {
    guojiazhuang_muzang$ritual_bronze_type[i] <- NA
    guojiazhuang_muzang$ritual_bronze_num[i] <- 0
    guojiazhuang_muzang$ritual_bronze[i] <- FALSE
  }
}

# Present/absence of burial jade, bone, and cowry
for (i in 1:nrow(guojiazhuang_muzang)) {
  if (is.na(guojiazhuang_muzang$burial_good_jade[i])) {
    guojiazhuang_muzang$burial_jade[i] <- FALSE
  }
  else {
    guojiazhuang_muzang$burial_jade[i] <- TRUE
  }
}

guojiazhuang_muzang[guojiazhuang_muzang$num == "160", c("ritual_bronze", "burial_jade")] <- TRUE
guojiazhuang_muzang[guojiazhuang_muzang$num == "172", c("ritual_bronze", "burial_jade")] <- TRUE
guojiazhuang_muzang[guojiazhuang_muzang$num == "170", c("ritual_bronze", "burial_jade")] <- TRUE
guojiazhuang_muzang[guojiazhuang_muzang$num == "185", c("ritual_bronze")] <- FALSE
guojiazhuang_muzang[guojiazhuang_muzang$num == "185", c("ritual_bronze_type")] <- NA
guojiazhuang_muzang[guojiazhuang_muzang$num == "185", c("ritual_bronze_num")] <- 0
write.xlsx2(guojiazhuang_muzang, showNA = FALSE,
            "./郭家莊墓葬形制44.xlsx")

# ===========Start from here: General Distribution Pattern==========
# # Plot the tomb orientation, we find that
# # Most of the tombs are NS, some are EW, distinct two clusters

# Tomb area and mean
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area)) %>%
  ggplot(aes(x = tomb_area)) +
  geom_histogram(fill = "white",
                 color = "black",
                 binwidth = 1000) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Area Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(tomb_area)),
    color = "blue",
    linetype = "dashed",
    linewidth = 1) +
  theme_minimal() +
  scale_x_continuous(label = comma, limits = c(0, 120000))

#Tomb area without the big tombs and associated sacrifical pits
guojiazhuang_muzang %>%
  subset(
    !(
      guojiazhuang_muzang$num == "160" |
        guojiazhuang_muzang$num == "172" |
        guojiazhuang_muzang$num == "51" |
        guojiazhuang_muzang$num == "52" |
        guojiazhuang_muzang$num == "58" |
        guojiazhuang_muzang$num == "143" |
        guojiazhuang_muzang$num == "146" |
        guojiazhuang_muzang$num == "147" |
        guojiazhuang_muzang$num == "148" 
    )
  ) %>%
  filter(!is.na(tomb_area)) %>%
  ggplot(aes(x = tomb_area)) +
  geom_histogram(fill = "white",
                 color = "black",
                 binwidth = 1000) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Area Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(tomb_area)),
    color = "blue",
    linetype = "dashed",
    size = 1) +
  scale_x_continuous(label = comma, limits = c(0, 120000))

# Tomb size change with time
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area), !is.na(period)) %>%
  ggplot(aes(x = tomb_area, fill = period)) +
  geom_density(alpha = .4,
               color = "black") +
  xlab("Waist Pit area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Size Distribution with mean") +
  scale_fill_discrete(breaks = c("二", "三", "四早", "四晚")) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

# Tomb size change with time (taking out big tombs and associated pits)
guojiazhuang_muzang %>%
  subset(
    !(
      guojiazhuang_muzang$num == "160" |
        guojiazhuang_muzang$num == "172" |
        guojiazhuang_muzang$num == "51" |
        guojiazhuang_muzang$num == "52" |
        guojiazhuang_muzang$num == "58" |
        guojiazhuang_muzang$num == "143" |
        guojiazhuang_muzang$num == "146" |
        guojiazhuang_muzang$num == "147" |
        guojiazhuang_muzang$num == "148" 
    )
  ) %>%
  filter(!is.na(tomb_area), !is.na(period)) %>%
  ggplot(aes(x = tomb_area, fill = period)) +
  geom_density(alpha = .4,
               color = "black") +
  xlab("Waist Pit area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Size Distribution with mean") +
  scale_fill_discrete(breaks = c("二", "三", "四早", "四晚")) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

a <- guojiazhuang_muzang %>% filter(!is.na(tomb_area), !is.na(period))
b <- guojiazhuang_muzang %>% filter(!is.na(tomb_area), !is.na(period), ritual_bronze)
c <- guojiazhuang_muzang %>% filter(!is.na(tomb_area), !is.na(period), hum_sacrifice != 0)
ggplot() +
  geom_violin(a, mapping = aes(x = period, y = tomb_area), fill = "lightgrey", alpha = 0.5, color = "grey") +
  geom_boxplot(a, mapping = aes(x = period, y = tomb_area),
    width = .1,
    notch = FALSE,
    fill = "white",
    varwidth = TRUE, outlier.alpha = 0, alpha = 0.5) +
  labs(x = "Period",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb size ~ period") +
  #geom_point(a, mapping = aes(x = period, y = tomb_area), color = "black") +
  geom_point(b, mapping = aes(x = period, y = tomb_area, color = "ritual bronze"), size = 2, position = position_nudge(x = 0.05)) +
  geom_point(c, mapping = aes(x = period, y = tomb_area, color = "human sacrifice"), size = 2, position = position_nudge(x = -0.05))+
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  scale_y_continuous(labels = comma) +
  scale_color_discrete(name = "") +
  theme_minimal()


# Coffin area and mean
guojiazhuang_muzang %>%
  filter(!is.na(coffin_area)) %>%
  ggplot(aes(x = coffin_area)) +
  geom_histogram(color = "black",
                 fill = "white",
                 binwidth = 500) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Coffin Area Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(coffin_area)),
    color = "blue",
    linetype = "dashed",
    size = 1)

# waist pit area and mean
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit_area)) %>%
  ggplot(aes(x = waist_pit_area)) +
  geom_histogram(color = "black",
                 fill = "white",
                 binwidth = 200) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Waist Pit Area Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(waist_pit_area)),
    color = "blue",
    linetype = "dashed",
    size = 1)

# Tomb LW ratio and mean
guojiazhuang_muzang %>%
  filter(!is.na(tomb_LWratio)) %>%
  ggplot(aes(x = tomb_LWratio)) +
  geom_histogram(color = "black",
                 fill = "white",
                 binwidth = 0.05) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb LW ratio Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(tomb_LWratio)),
    color = "blue",
    linetype = "dashed",
    size = 1)

# Coffin LW ratio and mean
guojiazhuang_muzang %>%
  filter(!is.na(coffin_LWratio)) %>%
  ggplot(aes(x = coffin_LWratio)) +
  geom_histogram(color = "black",
                 fill = "white",
                 binwidth = 0.05) +
  xlab("coffin area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Coffin LW ratio Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(coffin_LWratio)),
    color = "blue",
    linetype = "dashed",
    size = 1)

# Waist Pit LW ratio and mean
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit_LWratio)) %>%
  ggplot(aes(x = waist_pit_LWratio)) +
  geom_histogram(color = "black",
                 fill = "white",
                 binwidth = 0.05) +
  xlab("Waist pit LW ratio (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Waist Pit LW ratio Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(coffin_LWratio)),
    color = "blue",
    linetype = "dashed",
    size = 1)

# Tomb LW ratio and mean - change with time
guojiazhuang_muzang %>%
  filter(!is.na(tomb_LWratio),!is.na(period)) %>%
  ggplot(aes(x = tomb_LWratio, fill = period)) +
  geom_density(alpha = .4,
               color = "black") +
  xlab("Tomb LW ratio (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb LW ratio Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(tomb_LWratio)),
    color = "blue",
    linetype = "dashed",
    size = 1) +
  scale_fill_discrete(breaks = c("二", "三", "四早", "四晚"))

# Coffin LW ratio and mean - change with time
guojiazhuang_muzang %>%
  filter(!is.na(coffin_LWratio),!is.na(period)) %>%
  ggplot(aes(x = coffin_LWratio, fill = period)) +
  geom_density(alpha = .4,
               color = "black") +
  xlab("Coffin LW ratio (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Coffin LW ratio Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(coffin_LWratio)),
    color = "blue",
    linetype = "dashed",
    size = 1) +
  scale_fill_discrete(breaks = c("二", "三", "四早", "四晚"))


# Waist Pit LW ratio and mean - change with time
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit_LWratio),!is.na(period)) %>%
  ggplot(aes(x = waist_pit_LWratio, fill = period)) +
  geom_density(alpha = .4,
               color = "black") +
  xlab("Waist Pit LW ratio (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Waist Pit LW ratio Distribution with mean") +
  geom_vline(
    aes(xintercept = mean(waist_pit_LWratio)),
    color = "blue",
    linetype = "dashed",
    size = 1) +
  scale_fill_discrete(breaks = c("二", "三", "四早", "四晚"))

# =========Relationships between categorical elements========
# waist_pit, period, orientation_des, burial_style

# existence of waist pit & period
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(period)) %>%
  ggplot(aes(x = period, fill = waist_pit)) +
  geom_bar(position = "stack") +
  xlab("Period") +
  ggtitle("Waist pit ~ period") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  scale_fill_discrete(name = "Waist pit") +
  theme_minimal()

# jade ~ period
guojiazhuang_muzang %>%
  filter(!is.na(burial_jade),!is.na(period)) %>%
  ggplot(aes(x = period, fill = burial_jade)) +
  geom_bar(position = "stack") +
  xlab("Period") +
  ggtitle("Jade ~ period") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  scale_fill_discrete(name = "Jade") +
  theme_minimal()

#burial dog~ period
guojiazhuang_muzang %>%
  filter(!is.na(burial_dog),!is.na(period)) %>%
  ggplot(aes(x = period, fill = burial_dog)) +
  geom_bar(position = "stack") +
  xlab("Period") +
  ggtitle("Sacrificial Dog ~ period") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  scale_fill_discrete(name = "Sacrificial Dog") +
  theme_minimal()

#ritual_bronze ~ period
guojiazhuang_muzang %>%
  filter(!is.na(ritual_bronze),!is.na(period)) %>%
  ggplot(aes(x = period, fill = ritual_bronze)) +
  geom_bar(position = "stack") +
  xlab("Period") +
  ggtitle("Ritual bronze ~ period") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  scale_fill_discrete(name = "Ritual Bronze") +
  theme_minimal()

#Cowry ~ period
guojiazhuang_muzang %>%
  filter(!is.na(period)) %>%
  ggplot(aes(x = period, fill = is.na(burial_good_cowery))) +
  geom_bar(position = "stack") +
  xlab("Period") +
  ggtitle("Cowey shell ~ period") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  scale_fill_discrete(name = "Cowry shell") +
  theme_minimal()

#Human sacrifice ~ period
guojiazhuang_muzang %>%
  filter(!is.na(period)) %>%
  ggplot(aes(x = period, fill = hum_sacrifice != 0)) +
  geom_bar(position = "stack") +
  xlab("Period") +
  ggtitle("Human Sacrifice ~ period") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  scale_fill_discrete(name = "Human Sacrifice") +
  theme_minimal()

# period & orientation
guojiazhuang_muzang %>%
  filter(!is.na(period),!is.na(orientation_des)) %>%
  ggplot(aes(x = period, fill = orientation_des)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  ggtitle("Orientation ~ Period") +
  scale_fill_discrete(name = "Tomb Orientation") +
  theme_minimal()

# existence of waist pit & orientation
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(orientation_des)) %>%
  ggplot(aes(x = waist_pit, fill = orientation_des)) +
  geom_bar(position = "stack") +
  xlab("Waist pit") +
  ggtitle("Tomb orientation and existence of waist pit") +
  theme_minimal()


# existence of waist pit & burial style
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(burial_style)) %>%
  ggplot(aes(x = waist_pit, fill = burial_style)) +
  geom_bar(position = "dodge") +
  xlab("Has waist pit") +
  ggtitle("Existence of waist pit and burial style") +
  scale_fill_discrete(name = "Burial Style")

# period & orientation
guojiazhuang_muzang %>%
  filter(!is.na(period),!is.na(orientation_des)) %>%
  ggplot(aes(x = period, fill = orientation_des)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  ggtitle("Period and orientation") +
  scale_fill_discrete(name = "Tomb Orientation")

guojiazhuang_muzang %>%
  filter(!is.na(period),!is.na(orientation_des)) %>%
  ggplot(aes(x = orientation_des, fill = period)) +
  geom_bar(position = "dodge") +
  scale_fill_discrete(limits = c("二", "三", "四早", "四晚")) +
  ggtitle("Period and orientation") +
  scale_x_discrete(name = "Tomb Orientation")


# period & burial style
guojiazhuang_muzang %>%
  filter(!is.na(period),!is.na(burial_style)) %>%
  ggplot(aes(x = period, fill = burial_style)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(limits = c("二", "三", "四早", "四晚")) +
  ggtitle("Burial style ~ period") +
  scale_fill_discrete(name = "Burial Style") +
  theme_minimal()

# orientation & burial style
guojiazhuang_muzang %>%
  filter(!is.na(orientation_des),!is.na(burial_style)) %>%
  ggplot(aes(x = orientation_des, fill = burial_style)) +
  geom_bar(position = "dodge") +
  ggtitle("Orientation and burial style")+
  scale_fill_discrete(name = "Burial Style")

#=====Hypothesis Testing=======
library(vcd)

guojiazhuang_muzang[,c("period", "burial_dog", "orientation_des")] %>%
  filter(!is.na(orientation_des),!is.na(burial_dog), !is.na(period)) %>%
  table %>%
  mosaic(shade = TRUE)

guojiazhuang_muzang[,c("burial_jade", "burial_dog", "period")] %>%
  filter(!is.na(burial_jade),!is.na(burial_dog), !is.na(period)) %>%
  table %>%
  mosaic(shade = TRUE)

fisher.test(guojiazhuang_muzang$ritual_bronze, guojiazhuang_muzang$waist_pit) # <0.05
fisher.test(guojiazhuang_muzang$burial_dog, guojiazhuang_muzang$waist_pit) #burial dog and waist pit are dependent
fisher.test(guojiazhuang_muzang$ritual_bronze, guojiazhuang_muzang$period)
fisher.test(guojiazhuang_muzang$burial_jade, guojiazhuang_muzang$period)
fisher.test(guojiazhuang_muzang$burial_jade, guojiazhuang_muzang$period)
fisher.test(guojiazhuang_muzang$burial_style, guojiazhuang_muzang$period)
fisher.test(guojiazhuang_muzang$burial_style, guojiazhuang_muzang$waist_pit)
fisher.test(guojiazhuang_muzang$ritual_bronze, guojiazhuang_muzang$burial_jade)  #<0.05
fisher.test(guojiazhuang_muzang$ritual_bronze, guojiazhuang_muzang$burial_dog) 
fisher.test(guojiazhuang_muzang$burial_dog, !is.na(guojiazhuang_muzang$burial_good_bronze))
fisher.test(!is.na(guojiazhuang_muzang$burial_good_bronze), !is.na(guojiazhuang_muzang$burial_good_cowery))

table(guojiazhuang_muzang$burial_dog, !is.na(guojiazhuang_muzang$burial_good_bronze))
table(guojiazhuang_muzang$burial_dog, guojiazhuang_muzang$waist_pit)
table(guojiazhuang_muzang$orientation_des, guojiazhuang_muzang$burial_dog_waist)
table(guojiazhuang_muzang$ritual_bronze, guojiazhuang_muzang$burial_jade)
table(guojiazhuang_muzang$orientation_des, guojiazhuang_muzang$burial_dog_filling)
table(guojiazhuang_muzang$orientation_des, guojiazhuang_muzang$burial_dog_ercengtai)
table(guojiazhuang_muzang$ritual_bronze, guojiazhuang_muzang$waist_pit)


#ritual bronze and burial jade
guojiazhuang_muzang[,c("ritual_bronze", "burial_jade")] %>%
  filter(!is.na(burial_jade), !is.na(ritual_bronze))  %>%
  table() %>%
  mosaic(shade = TRUE, legend = TRUE)

guojiazhuang_muzang[,c("waist_pit", "burial_dog")] %>%
  filter(!is.na(waist_pit), !is.na(burial_dog))  %>%
  table() %>%
  mosaic(shade = TRUE, legend = TRUE)


# Pearson Correlation - existence and number of ritual bronze not correlated with tomb size
regular_size <- guojiazhuang_muzang %>% subset(!(
    guojiazhuang_muzang$num == "160" |
      guojiazhuang_muzang$num == "172" |
      guojiazhuang_muzang$num == "170" |
      guojiazhuang_muzang$num == "51" |
      guojiazhuang_muzang$num == "52" |
      guojiazhuang_muzang$num == "58" |
      guojiazhuang_muzang$num == "143" |
      guojiazhuang_muzang$num == "146" |
      guojiazhuang_muzang$num == "147" |
      guojiazhuang_muzang$num == "148" |
      guojiazhuang_muzang$num == "122" |
      guojiazhuang_muzang$num == "123"))

for(i in 1:nrow(regular_size)){
  if(!is.na(regular_size$burial_good_cowery[i])){
    regular_size$burial_cowery[i] <- TRUE
  } else{
    regular_size$burial_cowery[i] <- FALSE
  }
}

for(i in 1:nrow(regular_size)){
  if(is.na(regular_size$burial_good_bronze[i])){
    regular_size$weapon_bronze[i] <- FALSE
  } else{
    if(grepl("钺", regular_size$burial_good_bronze[i]) | 
       grepl("戈", regular_size$burial_good_bronze[i]) | 
       grepl("刀", regular_size$burial_good_bronze[i]) |
       grepl("矛", regular_size$burial_good_bronze[i]) |
       grepl("镞", regular_size$burial_good_bronze[i]) |
       grepl("锛", regular_size$burial_good_bronze[i])) {
      regular_size$weapon_bronze[i] <- TRUE
    } else{
      regular_size$weapon_bronze[i] <- FALSE
    }
  }
}

for(i in 1:nrow(regular_size)){
  if(is.na(regular_size$burial_good_pottery[i])){
    regular_size$pottery_num[i] <- 0
  } else{
    regular_size$pottery_num[i] <- length(strsplit(guojiazhuang_muzang$burial_good_pottery[i], '[.]')[[1]])
  }
}

for(i in 1:nrow(regular_size)){
  if(is.na(regular_size$burial_good_bronze[i])){
    regular_size$bronze_num[i] <- 0
  } else{
    regular_size$bronze_num[i] <- length(strsplit(guojiazhuang_muzang$burial_good_bronze[i], '[.]')[[1]])
  }
}

corMat <- cor(na.omit(guojiazhuang_muzang[,c("tomb_area", "coffin_area", "waist_pit_area")]))
round(corMat, 3)

r <- cor.test(guojiazhuang_muzang$tomb_area, guojiazhuang_muzang$coffin_area, method = "pearson", conf.level = 0.95)
r$conf.int <- round(r$conf.int, 3)
ggplot(na.omit(guojiazhuang_muzang[,c("tomb_area", "coffin_area", "waist_pit_area")]), 
       aes(tomb_area, coffin_area)) +
  geom_point() +  labs(x = "Tomb Area (sq. cm)",
                       y = "Coffin Area (sq.cm)",
                       title = "Tomb Area ~ Coffin Area", 
                       subtitle = paste("0.95 confidence interval r = ", r$conf.int[1], "-", r$conf.int[2]), by = "") +
  scale_x_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  geom_smooth(method="lm") +
  theme_minimal()

s <- cor.test(guojiazhuang_muzang$tomb_area, guojiazhuang_muzang$waist_pit_area, method = "pearson", conf.level = 0.95)
s$conf.int <- round(s$conf.int, 3)
ggplot(na.omit(guojiazhuang_muzang[,c("tomb_area", "coffin_area", "waist_pit_area")]), 
       aes(tomb_area, waist_pit_area)) +
  geom_point() +  labs(x = "Tomb Area (sq. cm)",
                       y = "Waist Pit Area (sq.cm)",
                       title = "Tomb Area ~ Waist Pit Area", 
                       subtitle = paste("0.95 confidence interval r = ", s$conf.int[1], "-", s$conf.int[2]), by = "") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_smooth(method="lm") +
  theme_minimal()

ggplot(na.omit(guojiazhuang_muzang[,c("num", "tomb_area", "coffin_area", "waist_pit_area")])) +
  geom_point(aes(reorder(num, -tomb_area), waist_pit_area, color = "Waist Pit")) +
  geom_point(aes(reorder(num, -tomb_area), coffin_area, color = "Coffin")) +
  geom_point(aes(reorder(num, -tomb_area), tomb_area, color = "Tomb")) +
  scale_y_continuous(labels = comma) +
  labs(x = "Tombs",
       y = "Area (sq.cm)",
       title = "Distribution of tomb, coffin, and waist pit area") +
  scale_color_manual(name = "", values = c("Tomb" = "red", "Coffin" = "black", "Waist Pit" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 5))
  

sd(na.omit(guojiazhuang_muzang$waist_pit_area))
sd(na.omit(guojiazhuang_muzang$coffin_area))
sd(na.omit(guojiazhuang_muzang$tomb_area))

summary(aov(tomb_area ~ ritual_bronze, data = guojiazhuang_muzang))
t.test(tomb_area ~ ritual_bronze, data = regular_size)
shapiro.test(regular_size$tomb_area)
#Mann–Whitney U test / Wilcoxon rank-sum test
wilcox.test(tomb_area ~ ritual_bronze,data = regular_size,exact = FALSE)
wilcox.test(tomb_area ~ burial_dog,data = regular_size,exact = FALSE)
wilcox.test(tomb_area ~ burial_jade,data = regular_size,exact = FALSE)


# Quantitative Methods in Archaeology using R P179 ANOVA
# tomb sizes in different area - report image
report_tomb_aov <- aov(tomb_area~Report_zone_simp, guojiazhuang_muzang)
summary(report_tomb_aov)
TukeyHSD(report_tomb_aov, order = TRUE)
plot(TukeyHSD(report_tomb_aov, order = TRUE), las = 1) #difference between zhong and nan is not significant
# tomb sizes in different area - report image - taking out large size tombs
report_tomb_aov <- aov(tomb_area~Report_zone_simp, guojiazhuang_muzang %>% subset(!(
  guojiazhuang_muzang$num == "160" |
    guojiazhuang_muzang$num == "172" |
    guojiazhuang_muzang$num == "170" |
    guojiazhuang_muzang$num == "51" |
    guojiazhuang_muzang$num == "52" |
    guojiazhuang_muzang$num == "58" |
    guojiazhuang_muzang$num == "143" |
    guojiazhuang_muzang$num == "146" |
    guojiazhuang_muzang$num == "147" |
    guojiazhuang_muzang$num == "148" |
    guojiazhuang_muzang$num == "122" |
    guojiazhuang_muzang$num == "123")))
summary(report_tomb_aov)
TukeyHSD(report_tomb_aov, order = TRUE)
plot(TukeyHSD(report_tomb_aov, order = TRUE), las = 1) #difference among three zones not significant


#Dunn test
report_tomb_dunn <- DunnTest(tomb_area~Report_zone_simp, guojiazhuang_muzang)
report_tomb_dunn
#take out large tomb
report_tomb_dunn <- DunnTest(tomb_area~Report_zone_simp, guojiazhuang_muzang %>% subset(!(
  guojiazhuang_muzang$num == "160" |
    guojiazhuang_muzang$num == "172" |
    guojiazhuang_muzang$num == "170" |
    guojiazhuang_muzang$num == "51" |
    guojiazhuang_muzang$num == "52" |
    guojiazhuang_muzang$num == "58" |
    guojiazhuang_muzang$num == "143" |
    guojiazhuang_muzang$num == "146" |
    guojiazhuang_muzang$num == "147" |
    guojiazhuang_muzang$num == "148" |
    guojiazhuang_muzang$num == "122" |
    guojiazhuang_muzang$num == "123")))
report_tomb_dunn

# Tomb sizes in different area - Zhu zone B
ZhuB_tomb_aov <- aov(tomb_area~Zhu_zoneB, guojiazhuang_muzang)
summary(ZhuB_tomb_aov)
TukeyHSD(ZhuB_tomb_aov, order = TRUE)
plot(TukeyHSD(ZhuB_tomb_aov, order = TRUE), las = 1) #difference between consecutive zones are significant
# Tomb sizes in different area - Zhu zone B - taking out large tombs
ZhuB_tomb_aov <- aov(tomb_area~Zhu_zoneB, guojiazhuang_muzang %>% subset(!(
  guojiazhuang_muzang$num == "160" |
    guojiazhuang_muzang$num == "172" |
    guojiazhuang_muzang$num == "170" |
    guojiazhuang_muzang$num == "51" |
    guojiazhuang_muzang$num == "52" |
    guojiazhuang_muzang$num == "58" |
    guojiazhuang_muzang$num == "143" |
    guojiazhuang_muzang$num == "146" |
    guojiazhuang_muzang$num == "147" |
    guojiazhuang_muzang$num == "148" |
    guojiazhuang_muzang$num == "122" |
    guojiazhuang_muzang$num == "123")))
summary(ZhuB_tomb_aov)
TukeyHSD(ZhuB_tomb_aov, order = TRUE)
plot(TukeyHSD(ZhuB_tomb_aov, order = TRUE), las = 1) #difference between consecutive zones are significant

#Dunn test
# Tomb sizes in different area - Zhu zone B
ZhuB_tomb_dunn <- DunnTest(tomb_area~Zhu_zoneB, guojiazhuang_muzang)
ZhuB_tomb_dunn

# Tomb sizes in different area - Zhu zone B - taking out large tombs
ZhuB_tomb_dunn <- DunnTest(tomb_area~Zhu_zoneB, guojiazhuang_muzang %>% subset(!(
  guojiazhuang_muzang$num == "160" |
    guojiazhuang_muzang$num == "172" |
    guojiazhuang_muzang$num == "170" |
    guojiazhuang_muzang$num == "51" |
    guojiazhuang_muzang$num == "52" |
    guojiazhuang_muzang$num == "58" |
    guojiazhuang_muzang$num == "143" |
    guojiazhuang_muzang$num == "146" |
    guojiazhuang_muzang$num == "147" |
    guojiazhuang_muzang$num == "148" |
    guojiazhuang_muzang$num == "122" |
    guojiazhuang_muzang$num == "123")))
ZhuB_tomb_dunn




#PCA
library(devtools)
install_github("vqv/ggbiplot", force = TRUE)
library(ggbiplot)

gjz.pca <- prcomp(na.omit(guojiazhuang_muzang[,c("tomb_area", "coffin_area", "waist_pit_area", "ritual_bronze_num", "burial_jade")]), center = TRUE,scale. = TRUE)
ggbiplot(gjz.pca, choice = c(3,4))

# # Linear regression and PCA to find the relationship among numeric data
# gjz_numeric <- na.omit(guojiazhuang_muzang)[, c(3, 5:14, 16:18, 20:22)]
# gjz_numeric[] <- lapply(gjz_numeric, function(x) as.numeric(as.character(x)))
#
# # Linear regression to see statistically significant relationship between orientation
# # and other numeric variables
# ori_reg <- lm(orientation_180~., data=gjz_numeric)
# summary(ori_reg) # result: orientation is random
#
# tomb_length_reg <- lm(tomb_length~., data=gjz_numeric)
# summary(tomb_length_reg)
# # expected: strong correlation with tomb area, coffin area, coffin length, and coffin waist
# # unexpected: strong correlation with tomb width, suggesting existence of a certain L/W ratio
# #             no correlation with waits pit at all
#
# hist(guojiazhuang_muzang$tomb_LWratio, n = 40)
# hist(guojiazhuang_muzang$coffin_LWratio, n = 40)
# hist(guojiazhuang_muzang$waist_pit_LWratio, n = 40) # ratio of waist pit high;y standardized
#
# waist_pit_length_reg <- lm(waist_pit_length~., data=gjz_numeric)
# summary(waist_pit_length_reg)
# # unexpected: strong correlation with the width of the waist pit,
# # no correlation with tomb
#
# tomb_LWratio_reg <- lm(coffin_LWratio~., data=gjz_numeric)
# summary(tomb_LWratio_reg)
# # the ratio of legth and width of the coffin is less standardized
#
# gjz_pca <- prcomp(gjz_numeric, center = TRUE,scale. = TRUE)
# ggbiplot(gjz_pca, labels=rownames(gjz_numeric))
# ggbiplot(gjz_pca, labels=rownames(gjz_numeric), ellipse=TRUE, group = gjz_numeric_period$period)




# ====Testing 1998 report====
for (i in 1:nrow(guojiazhuang_muzang)) {
  guojiazhuang_muzang$Report_zone_simp[i] <-
    strsplit(guojiazhuang_muzang$Report_zone[i], "")[[1]][1]
}

# ====Report groups====
#box plot for tomb size
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = Report_zone_simp, y = tomb_area)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(
    width = .1,
    notch = TRUE,
    fill = "steelblue",
    varwidth = TRUE) +
  labs(x = "Tomb Group Archaeological Report",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Group Distribution (Report)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = c("北", "中", "南")) +
  theme_light()


guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = Report_zone_simp, y = tomb_area)) +
  geom_boxplot(width = .8, alpha = 0.7, outlier.alpha = 0) +
  geom_quasirandom(aes(color = Report_zone_simp)) +
  labs(x = "Tomb Group Archaeological Report",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Group Distribution (Report)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = c("北", "中", "南")) +
  theme_minimal()


# boxplot for tomb size, taking out tomb 172 and tomb 160, 
# and 170, and the associated chariot pits
guojiazhuang_muzang %>%
  subset(
    !(
      guojiazhuang_muzang$num == "160" |
        guojiazhuang_muzang$num == "172" |
        guojiazhuang_muzang$num == "170" |
        guojiazhuang_muzang$num == "51" |
        guojiazhuang_muzang$num == "52" |
        guojiazhuang_muzang$num == "58" |
        guojiazhuang_muzang$num == "143" |
        guojiazhuang_muzang$num == "146" |
        guojiazhuang_muzang$num == "147" |
        guojiazhuang_muzang$num == "148"
    )
  ) %>%
  filter(!is.na(tomb_area),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = Report_zone_simp, y = tomb_area)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = .1,
               notch = TRUE,
               fill = "steelblue",
               varwidth = TRUE) +
  labs(x = "Tomb Group Archaeological Report",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Group Distribution without big tombs (Report)") +
  stat_summary(
    aes(label = round(stat(y), 1)),
    geom = "text",
    fun = function(y) {
      o <- boxplot.stats(y)$out
      if (length(o) == 0) NA
      else o }, hjust = -1) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(limits = c("北", "中", "南")) +
  theme_light()

#histogram for tomb size
mu <- guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Report_zone_simp)) %>%
  ddply(
    "Report_zone_simp",
    summarise,
    grp.mean = mean(tomb_area),
    grp.median = median(tomb_area)
  )
mu <- arrange(mu, desc(mu$grp.mean))
mu
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = tomb_area, color = Report_zone_simp)) +
  scale_color_discrete(breaks = c("北", "中", "南")) +
  geom_histogram(fill = "white", binwidth = 1000) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Area Distribution and Mean (Report)") +
  geom_vline(
    data = mu,
    aes(xintercept = grp.median,
        color = Report_zone_simp,
        linetype = "mean"),
    linetype = "dashed", size = 0.5,
    show.legend = TRUE) +
  geom_vline(
    data = mu,
    aes(xintercept = grp.median,
        color = Report_zone_simp,
        linetype = "median"),
    linetype = "solid", size = .5,
    show.legend = TRUE) +
  scale_x_continuous(labels = comma)

# histogran for tomb size, taking out tomb 172 and tomb 160, and 170 and the associated chariot pits
mu2 <- guojiazhuang_muzang %>%
  subset(
    !(
      guojiazhuang_muzang$num == "160" |
        guojiazhuang_muzang$num == "172" |
        guojiazhuang_muzang$num == "170" |
        guojiazhuang_muzang$num == "51" |
        guojiazhuang_muzang$num == "52" |
        guojiazhuang_muzang$num == "58" |
        guojiazhuang_muzang$num == "143" |
        guojiazhuang_muzang$num == "146" |
        guojiazhuang_muzang$num == "147" |
        guojiazhuang_muzang$num == "148"
    )
  ) %>%
  filter(!is.na(tomb_area),!is.na(Report_zone_simp)) %>%
  ddply(
    "Report_zone_simp",
    summarise,
    grp.mean = mean(tomb_area),
    grp.median = median(tomb_area)
  )
mu2
guojiazhuang_muzang %>%
  subset(
    !(
      guojiazhuang_muzang$num == "160" |
        guojiazhuang_muzang$num == "172" |
        guojiazhuang_muzang$num == "170" |
        guojiazhuang_muzang$num == "51" |
        guojiazhuang_muzang$num == "52" |
        guojiazhuang_muzang$num == "58" |
        guojiazhuang_muzang$num == "143" |
        guojiazhuang_muzang$num == "146" |
        guojiazhuang_muzang$num == "147" |
        guojiazhuang_muzang$num == "148"
    )
  ) %>%
  filter(!is.na(tomb_area),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = tomb_area, color = Report_zone_simp)) +
  geom_histogram(fill = "white", binwidth = 1000) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Area Distribution without big tombs (Report)") +
  geom_vline(
    data = mu2,
    aes(xintercept = grp.mean, color = Report_zone_simp),
    linetype = "dashed",
    size = 0.5
  ) +
  geom_vline(
    data = mu2,
    aes(xintercept = grp.median, color = Report_zone_simp),
    linetype = "solid",
    size = 0.5
  ) +
  scale_x_continuous(labels = comma)

# tomb orientation
guojiazhuang_muzang %>%
  filter(!is.na(orientation_des),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = Report_zone_simp, fill = orientation_des)) +
  scale_x_discrete(limits = c("北", "中", "南")) +
  geom_bar(position = "dodge") +
  xlab("Tomb group (report)") +
  theme_light() +
  ggtitle("Tomb orientation in each group (Report)") +
  scale_fill_discrete(name = "Tomb orientation")

# existence of waist pit
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = Report_zone_simp, fill = waist_pit)) +
  scale_x_discrete(limits = c("北", "中", "南")) +
  geom_bar(position = "dodge") +
  xlab("Tomb group (report)") +
  theme_light() +
  ggtitle("Prescence of waist pit in each group (Report)") +
  scale_fill_discrete(name = "")

# tomb period
guojiazhuang_muzang %>%
  filter(!is.na(period),!is.na(Report_zone_simp)) %>%
  ggplot(aes(x = Report_zone_simp, fill = period)) +
  scale_x_discrete(limits = c("北", "中", "南")) +
  geom_bar(position = "dodge") +
  xlab("Tomb group (report)") +
  ggtitle("Tomb period in each group (Report)") +
  theme_light() +
  scale_fill_discrete(breaks = c("二", "三", "四早", "四晚"), name = "Tomb Period")


guojiazhuang_muzang[,c("period", "orientation_des", "Report_zone_simp")] %>%
  filter(!is.na(orientation_des), !is.na(period), !is.na(Report_zone_simp))  %>%
  table() %>%
  mosaic(shade = TRUE, legend = TRUE)


#====Report subgroups====
# Tomb size distribution
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Report_zone)) %>%
  ggplot(aes(x = Report_zone, y = tomb_area)) +
  geom_boxplot(notch = FALSE,
               fill = "steelblue",
               varwidth = TRUE) +
  labs(x = "Tomb Group Archaeological Report",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Group Distribution (Report)") +
  # stat_summary(
  #   aes(label = round(stat(y), 1)),
  #   geom = "text",
  #   fun = function(y) {
  #     o <- boxplot.stats(y)$out
  #     if (length(o) == 0)
  #       NA
  #     else
  #       o
  #   },
  #   hjust = -.5
  # ) +
  scale_y_continuous(labels = comma)

# Tomb orientation in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(orientation_des),!is.na(Report_zone)) %>%
  ggplot(aes(x = Report_zone, fill = orientation_des)) +
  geom_bar(position = "stack") +
  xlab("Has waist pit") +
  ggtitle("Tomb orientation in each subgroup (Report)")

# waist pits in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(Report_zone)) %>%
  ggplot(aes(x = Report_zone, fill = waist_pit)) +
  geom_bar(position = "dodge") +
  xlab("Has waist pit") +
  ggtitle("Tomb orientation in each subgroup (Report)")

guojiazhuang_muzang[,c("period", "orientation_des", "Report_zone")] %>%
  filter(!is.na(orientation_des), !is.na(period), !is.na(Report_zone))  %>%
  table() %>%
  mosaic(shade = TRUE, legend = TRUE)

guojiazhuang_muzang[,c("waist_pit", "orientation_des", "Report_zone")] %>%
  filter(!is.na(orientation_des), !is.na(waist_pit), !is.na(Report_zone))  %>%
  table() %>%
  mosaic(shade = TRUE, legend = TRUE)

#====Quantity of burial goods====
bronze_good_total <- function (k) {
  bronze_type <- c()
  for (i in 1:nrow(k)) {
    current <- k[i, ]
    temp <- strsplit(current$burial_good_bronze, ".", fixed = TRUE)
    temp[[1]] <- str_replace_all(temp[[1]], c("A|B|C|V|I"), "")
    bronze_type <- c(bronze_type, temp[[1]])
  }
  return(na.omit(bronze_type))
}

# Quantity of burial goods in northern group
# except the big tombs and chariot pit
bronze_report_north <- guojiazhuang_muzang %>%
  subset(Report_zone_simp == "北") %>%
  subset(
    !(
      num == "172" | num == "51" |
        num == "52" | num == "58" |
        num == "143" | num == "146" |
        num == "147" | num == "148"
    )
  ) %>%
  bronze_good_total()
sort(table(bronze_report_north), decreasing = TRUE)

rit_brz <- c("铜觚", "铜鼎", "铜爵", "铜簋", "铜尊","铜斝","铜卣", "铜甗", "铜觥","铜觯" )
col_rit_brz <- c("#F8766D", "#D89000", "#39B600",  "#A3A500", "#00BF7D", "#00BFC4", "#00B0F6", "#9590FF", "#E76BF3", "#FF62BC")
names(col_rit_brz) <- rit_brz

dt_n <-
  data.frame(sort(table(bronze_report_north), decreasing = TRUE))

ggplot(dt_n, aes(x = "", y = Freq, fill = bronze_report_north)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  ggtitle("Bronze Tomb Goods in Northern Group (Report)") +
  theme_minimal()+
  scale_fill_discrete(name = "")

# Look at the bronze ritual vessels that we are interested in
dt_n %>% subset(
  bronze_report_north == "铜觚" |
    bronze_report_north == "铜鼎" |
    bronze_report_north == "铜爵" |
    bronze_report_north == "铜簋" |
    bronze_report_north == "铜尊" |
    bronze_report_north == "铜斝" |
    bronze_report_north == "铜卣" |
    bronze_report_north == "铜甗" |
    bronze_report_north == "铜觥" |
    bronze_report_north == "铜觯"
) %>% 
  ggplot(aes(x = "", y = -Freq, fill = bronze_report_north)) +
  geom_col() +
  coord_polar(theta = "y") +
  scale_fill_manual(name = "Ritual Vessel", values = col_rit_brz) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  ggtitle("Bronze Ritual and Serving Goods in Northern Group (Report)") +
  theme_minimal()


# Quantity of burial goods in middle group
bronze_report_mid <- guojiazhuang_muzang %>%
  subset(Report_zone_simp == "中") %>%
  bronze_good_total()

sort(table(bronze_report_mid), decreasing = TRUE)

dt_m <- data.frame(sort(table(bronze_report_mid), decreasing = TRUE))

ggplot(dt_m, aes(x = "", y = Freq, fill = bronze_report_mid)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  ggtitle("Bronze Tomb Goods in Middle Group (Report)") +
  theme_minimal()+
  scale_fill_discrete(name = "")

# Look at the bronze ritual vessels that we are interested in
dt_m %>% subset(
  bronze_report_mid == "铜觚" |
    bronze_report_mid == "铜鼎" |
    bronze_report_mid == "铜爵" |
    bronze_report_mid == "铜簋" |
    bronze_report_mid == "铜尊" |
    bronze_report_mid == "铜斝" |
    bronze_report_mid == "铜卣" |
    bronze_report_mid == "铜甗" |
    bronze_report_mid == "铜觥" |
    bronze_report_mid == "铜觯"
) %>%
  ggplot(aes(x = "", y = -Freq, fill = bronze_report_mid)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Ritual Vessel", values = col_rit_brz) +
  ggtitle("Bronze Ritual and Serving Goods in Middle Group (Report)") +
  theme_minimal()


# Quantity of burial goods in southern group
bronze_report_south <-
  guojiazhuang_muzang %>% subset(Report_zone_simp == "南") %>% bronze_good_total()
sort(table(bronze_report_south), decreasing = TRUE)
dt_s <- data.frame(sort(table(bronze_report_south), decreasing = TRUE))
ggplot(dt_s, aes(x = "", y = Freq, fill = bronze_report_south)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  ggtitle("Bronze Tomb Goods in Southern Group (Report)") +
  theme_minimal()+
  scale_fill_discrete(name = "")

dt_s %>% subset(   #Look at the bronze ritual vessels
  bronze_report_south == "铜觚" |
    bronze_report_south == "铜鼎" |
    bronze_report_south == "铜爵" |
    bronze_report_south == "铜簋" |
    bronze_report_south == "铜尊" |
    bronze_report_south == "铜斝" |
    bronze_report_south == "铜卣" |
    bronze_report_south == "铜甗" |
    bronze_report_south == "铜觥" |
    bronze_report_south == "铜觯"
) %>%
  ggplot(aes(x = "", y = Freq, fill = bronze_report_south)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  scale_fill_manual(name = "Ritual Vessel", values = col_rit_brz) +
  ggtitle("Bronze Ritual and Serving Goods in Southern Group (Report)") + 
  theme_minimal()



# ====Testing Zhu Fenghan 2004 article====
# ====Zhu groups====
# Tomb size distribution
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Zhu_zoneB)) %>%
  ggplot(aes(x = Zhu_zoneB, y = tomb_area)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(
    width = .1,
    notch = TRUE,
    fill = "steelblue",
    varwidth = TRUE
  ) +
  labs(x = "Tomb group (Zhu)",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Area Distribution (Zhu)") +
  scale_y_continuous(labels = comma)  +   
  # stat_summary(
  #   aes(label = stat(y)),
  #   geom = "text",
  #   fun = function(y) {
  #     o <- boxplot.stats(y)$out
  #     if (length(o) == 0)
  #       NA
  #     else
  #       o
  #   },
  #   hjust = -1
  # ) +
  scale_x_discrete(limits = c("甲", "乙", "丙", "丁")) +
  theme_minimal()


#alternative visualization
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Zhu_zoneB)) %>%
  ggplot(aes(x = Zhu_zoneB, y = tomb_area)) +
  geom_boxplot(width = .8, alpha = 0.7, outlier.alpha = 0) +
  geom_quasirandom(aes(color = Zhu_zoneB)) +
  labs(x = "Tomb group (Zhu)",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Area Distribution (Zhu)") +
  scale_y_continuous(labels = comma)  +   
scale_x_discrete(limits = c("甲", "乙", "丙", "丁")) +
  theme_minimal()


# histogram for tomb size
mu <- guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Zhu_zoneB)) %>%
  ddply(
    "Zhu_zoneB",
    summarise,
    grp.mean = mean(tomb_area),
    grp.median = median(tomb_area),
    grp.max = max(tomb_area),
    grp.min = min(tomb_area)
  )
mu
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Zhu_zoneB)) %>%
  ggplot(aes(x = tomb_area, color = Zhu_zoneB)) +
  geom_histogram(fill = NA, binwidth = 1000) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Area Distribution (Zhu)") +
  geom_vline(
    data = mu,
    aes(xintercept = grp.mean, color = Zhu_zoneB),
    linetype = "dashed",
    size = 0.5
  ) +
  geom_vline(
    data = mu,
    aes(xintercept = grp.median, color = Zhu_zoneB),
    linetype = "solid",
    size = 1
  ) +
  scale_x_continuous(labels = comma)

# Orientation
guojiazhuang_muzang %>%
  filter(!is.na(orientation_des),!is.na(Zhu_zoneB)) %>%
  ggplot(aes(x = Zhu_zoneB, fill = orientation_des)) +
  geom_bar(position = "dodge") +
  xlab("Tomb zone (Zhu)") +
  ggtitle("Tomb zone (Zhu) and tomb orientation") +
  scale_x_discrete(limits = c("甲", "乙", "丙", "丁"))

# Waist pit
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(Zhu_zoneB)) %>%
  ggplot(aes(x = Zhu_zoneB, fill = waist_pit)) +
  geom_bar(position = "dodge") +
  xlab("Tomb zone (Zhu)") +
  ggtitle("Tomb zone (Zhu) and existence of waist pit") +
  scale_x_discrete(limits = c("甲", "乙", "丙", "丁")) +
  scale_fill_discrete(name = "Waist Pit")



# vcd residual based-shadinng
guojiazhuang_muzang[,c("period", "Zhu_zoneB", "orientation_des")] %>%
  filter(!is.na(orientation_des),!is.na(Zhu_zoneB), !is.na(period)) %>%
  table %>%
  mosaic(shade = TRUE)

guojiazhuang_muzang[,c("waist_pit", "Zhu_zoneB", "orientation_des")] %>%
  filter(!is.na(orientation_des),!is.na(Zhu_zoneB), !is.na(waist_pit)) %>%
  table %>%
  mosaic(shade = TRUE)

#====Zhu subgroups====
# Tomb size distribution
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Zhu_zoneA)) %>%
  ggplot(aes(x = Zhu_zoneA, y = tomb_area)) +
  geom_boxplot(notch = FALSE,
               fill = "steelblue",
               varwidth = TRUE) +
  labs(x = "Tomb group Archaeological Report",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Area Distribution (Zhu)") +
  stat_summary(
    aes(label = round(stat(y), 1)),
    geom = "text",
    fun = function(y) {
      o <- boxplot.stats(y)$out
      if (length(o) == 0)
        NA
      else
        o
    },
    hjust = -.3
  ) +
  scale_y_continuous(labels = comma)

# histogram for tomb size subgroups
mu <- guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Zhu_zoneA)) %>%
  ddply(
    "Zhu_zoneA",
    summarise,
    grp.mean = mean(tomb_area),
    grp.median = median(tomb_area),
    grp.max = max(tomb_area),
    grp.min = min(tomb_area)
  )
mu
for (i in 1:nrow(mu)) {
  if (mu$Zhu_zoneA[i] == "北B" | mu$Zhu_zoneA[i] == "北C" |
      mu$Zhu_zoneA[i] == "北D" | mu$Zhu_zoneA[i] == "北E" |
      mu$Zhu_zoneA[i] == "北F" | mu$Zhu_zoneA[i] == "北G" |
      mu$Zhu_zoneA[i] == "北H" | mu$Zhu_zoneA[i] == "北I") {
    mu$Zhu_zoneB[i] <- "甲"
  } else if (mu$Zhu_zoneA[i] == "中A1" | mu$Zhu_zoneA[i] == "中A2" |
             mu$Zhu_zoneA[i] == "中A3" | mu$Zhu_zoneA[i] == "中A4") {
    mu$Zhu_zoneB[i] <- "乙"
  } else if (mu$Zhu_zoneA[i] == "中B" | mu$Zhu_zoneA[i] == "中C" |
             mu$Zhu_zoneA[i] == "中D" | mu$Zhu_zoneA[i] == "中E" |
             mu$Zhu_zoneA[i] == "中F" | mu$Zhu_zoneA[i] == "中G") {
    mu$Zhu_zoneB[i] <- "丙"
  } else if (mu$Zhu_zoneA[i] == "中H" | mu$Zhu_zoneA[i] == "南A" |
             mu$Zhu_zoneA[i] == "南B" | mu$Zhu_zoneA[i] == "南C") {
    mu$Zhu_zoneB[i] <- "丁"
  } else {
    mu$Zhu_zoneB[i] <- NA
  }
}
ggplot(mu, aes(
  y = grp.mean,
  x = reorder(Zhu_zoneA, grp.mean),
  fill = Zhu_zoneB)) +
  geom_bar(stat = "identity") +
  xlab("Subgroups (Zhu)") + ylab("Tomb area (sq.cm)") +
  ggtitle("Guojiazhuang Tomb Area Distribution among Subgroups (Zhu)") +
  scale_fill_discrete(breaks = c("甲", "乙", "丙", "丁"), name = "Zones") +
  theme_minimal()


# histogram for tomb size subgroups taking out M160, M172, and associated chariot pits
mu <- guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Zhu_zoneA)) %>%
  subset(
    !(num == "160" |num == "172" |num == "51" |
        num == "52" |num == "58" |num == "143" |
        num == "146" |num == "147" |num == "148"
    )
  ) %>%
  ddply(
    "Zhu_zoneA",
    summarise,
    grp.mean = mean(tomb_area),
    grp.median = median(tomb_area),
    grp.max = max(tomb_area),
    grp.min = min(tomb_area)
  )
mu
for (i in 1:nrow(mu)) {
  if (mu$Zhu_zoneA[i] == "北B" | mu$Zhu_zoneA[i] == "北C" |
      mu$Zhu_zoneA[i] == "北D" | mu$Zhu_zoneA[i] == "北E" |
      mu$Zhu_zoneA[i] == "北F" | mu$Zhu_zoneA[i] == "北G" |
      mu$Zhu_zoneA[i] == "北H" | mu$Zhu_zoneA[i] == "北I") {
    mu$Zhu_zoneB[i] <- "甲"
  } else if (mu$Zhu_zoneA[i] == "中A1" | mu$Zhu_zoneA[i] == "中A2" |
             mu$Zhu_zoneA[i] == "中A3" | mu$Zhu_zoneA[i] == "中A4") {
    mu$Zhu_zoneB[i] <- "乙"
  } else if (mu$Zhu_zoneA[i] == "中B" | mu$Zhu_zoneA[i] == "中C" |
             mu$Zhu_zoneA[i] == "中D" | mu$Zhu_zoneA[i] == "中E" |
             mu$Zhu_zoneA[i] == "中F" | mu$Zhu_zoneA[i] == "中G") {
    mu$Zhu_zoneB[i] <- "丙"
  } else if (mu$Zhu_zoneA[i] == "中H" | mu$Zhu_zoneA[i] == "南A" |
             mu$Zhu_zoneA[i] == "南B" | mu$Zhu_zoneA[i] == "南C") {
    mu$Zhu_zoneB[i] <- "丁"
  } else {
    mu$Zhu_zoneB[i] <- NA
  }
}
ggplot(mu, aes(
  y = grp.mean,
  x = reorder(Zhu_zoneA, grp.mean),
  fill = Zhu_zoneB)) +
  geom_bar(stat = "identity") +
  xlab("Subgroups (Zhu)") + ylab("Tomb area (sq.cm)") +
  ggtitle("Guojiazhuang Tomb Area Distribution among Subgroups (Zhu)") +
scale_fill_discrete(breaks = c("甲", "乙", "丙", "丁"), name = "Zones") +
  theme_minimal()

# Tomb orientation in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(orientation_des),!is.na(Zhu_zoneA)) %>%
  ggplot(aes(x = Zhu_zoneA, fill = orientation_des)) +
  geom_bar(position = "dodge") +
  xlab("Subgroup (Zhu)") +
  ggtitle("Tomb orientation in each subgroup (Zhu)") +
  scale_fill_discrete(name = "Orientation")

# Tomb period in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(period),!is.na(Zhu_zoneA)) %>%
  ggplot(aes(x = Zhu_zoneA, fill = period)) +
  geom_bar(position = "dodge") +
  xlab("Subgroup (Zhu)") +
  ggtitle("Tomb period in each subgroup (Zhu)") +
  scale_fill_discrete(name = "Period")

# vcd residual based-shadinng
guojiazhuang_muzang[,c("period", "Zhu_zoneA", "orientation_des")] %>%
  filter(!is.na(orientation_des),!is.na(Zhu_zoneA), !is.na(period)) %>%
  table %>%
  mosaic(shade = TRUE)

guojiazhuang_muzang[,c("waist_pit", "Zhu_zoneA", "orientation_des")] %>%
  filter(!is.na(orientation_des),!is.na(Zhu_zoneA), !is.na(waist_pit)) %>%
  table %>%
  mosaic(shade = TRUE)
  
# waist pits in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(Zhu_zoneA)) %>%
  ggplot(aes(x = Zhu_zoneA, fill = waist_pit)) +
  geom_bar(position = "dodge") +
  xlab("Subgroup (Zhu)") +
  ggtitle("Tomb orientation in each subgroups (Zhu)") +
  scale_fill_discrete(name = "Waist Pit")


#====Quantity of bronze burial goods====
bronze_good_total <- function (k) {
  bronze_type <- c()
  for (i in 1:nrow(k)) {
    current <- k[i, ]
    temp <- strsplit(current$burial_good_bronze, ".", fixed = TRUE)
    temp[[1]] <- str_replace_all(temp[[1]], c("A|B|C|V|I"), "")
    bronze_type <- c(bronze_type, temp[[1]])
  }
  return(na.omit(bronze_type))
}

ZhuA <- sort(na.omit(unique(guojiazhuang_muzang$Zhu_zoneA)))
bronze_Zhu <- data.frame(matrix(ncol = 4, nrow = length(ZhuA)))
colnames(bronze_Zhu) <-
  c("Zhu_zone_A",
    "bronze_goods",
    "bronze_cnt",
    "bronze_ritual_cnt")
head(bronze_Zhu)

for (i in 1:length(ZhuA)) {
  bronze_Zhu$bronze_goods[i] <- list(guojiazhuang_muzang %>%
                                       subset(Zhu_zoneA == ZhuA[i]) %>%
                                       bronze_good_total())
  bronze_Zhu$Zhu_zone_A[i] <- ZhuA[i]
}

for (i in 1:nrow(bronze_Zhu)) {
  if (bronze_Zhu$Zhu_zone_A[i] == "北B" |
      bronze_Zhu$Zhu_zone_A[i] == "北C" |
      bronze_Zhu$Zhu_zone_A[i] == "北D" |
      bronze_Zhu$Zhu_zone_A[i] == "北E" |
      bronze_Zhu$Zhu_zone_A[i] == "北F" |
      bronze_Zhu$Zhu_zone_A[i] == "北G" |
      bronze_Zhu$Zhu_zone_A[i] == "北H" |
      bronze_Zhu$Zhu_zone_A[i] == "北I") {
    bronze_Zhu$Zhu_zone_B[i] <- "甲"
  } else if (bronze_Zhu$Zhu_zone_A[i] == "中A1" |
             bronze_Zhu$Zhu_zone_A[i] == "中A2" |
             bronze_Zhu$Zhu_zone_A[i] == "中A3" |
             bronze_Zhu$Zhu_zone_A[i] == "中A4") {
    bronze_Zhu$Zhu_zone_B[i] <- "乙"
  } else if (bronze_Zhu$Zhu_zone_A[i] == "中B" |
             bronze_Zhu$Zhu_zone_A[i] == "中C" |
             bronze_Zhu$Zhu_zone_A[i] == "中D" |
             bronze_Zhu$Zhu_zone_A[i] == "中E" |
             bronze_Zhu$Zhu_zone_A[i] == "中F" |
             bronze_Zhu$Zhu_zone_A[i] == "中G") {
    bronze_Zhu$Zhu_zone_B[i] <- "丙"
  } else {
    bronze_Zhu$Zhu_zone_B[i] <- "丁"
  }
  head(bronze_Zhu)
  
  bronze_Zhu$bronze_cnt[i] <- length(bronze_Zhu$bronze_goods[[i]])
  bronze_Zhu$bronze_ritual_cnt[i] <-
    length(  #Look at the bronze ritual vessels
      which(
        bronze_Zhu$bronze_goods[[i]] == "铜觚" |
          bronze_Zhu$bronze_goods[[i]] ==
          "铜鼎" |
          bronze_Zhu$bronze_goods[[i]] == "铜爵" |
          bronze_Zhu$bronze_goods[[i]] == "铜簋" |
          bronze_Zhu$bronze_goods[[i]] == "铜尊" |
          bronze_Zhu$bronze_goods[[i]] == "铜斝" |
          bronze_Zhu$bronze_goods[[i]] == "铜卣" |
          bronze_Zhu$bronze_goods[[i]] == "铜甗" |
          bronze_Zhu$bronze_goods[[i]] == "铜觥" |
          bronze_Zhu$bronze_goods[[i]] == "铜觯"
      )
    )
}

# number of bronze goods in each subgroup
ggplot(bronze_Zhu, aes(y = bronze_cnt,
  x = reorder(Zhu_zone_A, bronze_cnt),
  fill = Zhu_zone_B)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = bronze_cnt), nudge_y = 6) +
  scale_fill_discrete(breaks = c("甲", "乙", "丙", "丁")) +
  xlab("Subgroups (Zhu)") + ylab("number of bronze goods") +
  ggtitle("Guojiazhuang Bronze Goods Distribution among Subgroups (Zhu)")

# number of bronze ritual goods in each subgroup
ggplot(bronze_Zhu,
       aes(
         y = bronze_ritual_cnt,
         x = reorder(Zhu_zone_A, bronze_ritual_cnt),
         fill = Zhu_zone_B
       )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = bronze_ritual_cnt), nudge_y = .5) +
  scale_fill_discrete(breaks = c("甲", "乙", "丙", "丁")) +
  xlab("Subgroups (Zhu)") + ylab("number of bronze ritual goods") +
  ggtitle("Guojiazhuang Bronze Ritual Goods Distribution among Subgroups (Zhu)")


#====Combination of pottery vessels====
#initialize a list of pottery vessel for each tombs, name it pottery_Zhu
pottery_good_total <- function (k) {
  pottery_type <- c()
  for (i in 1:nrow(k)) {
    current <- k[i, ]
    temp <- strsplit(current$burial_good_pottery, ".", fixed = TRUE)
    temp[[1]] <- str_replace_all(temp[[1]], c("A|B|C|D|E|F|G|H|a|b|c|V|X|I"), "")
    pottery_type <- c(pottery_type, temp[[1]])
  }
  return(na.omit(pottery_type))
}

sort(table(pottery_good_total(guojiazhuang_muzang)), decreasing = TRUE)

pottery_Zhu <- (guojiazhuang_muzang %>%
                  filter(!is.na(burial_good_pottery),!is.na(Zhu_zoneA)))[, c("num",
                                                                             "period",
                                                                             "burial_good_pottery",
                                                                             "Zhu_zoneA",
                                                                             "Zhu_zoneB")]
head(pottery_Zhu)

for (i in 1:nrow(pottery_Zhu)) {  #parse burial goods
  current <- pottery_Zhu[i, ]
  temp <- strsplit(current$burial_good_pottery, ".", fixed = TRUE)
  temp[[1]] <-str_replace_all(temp[[1]], c("A|B|C|D|E|F|G|H|a|b|c|V|X|I"), "")
  pottery_Zhu$pottery_list[i] <- temp
}


# find the top 7 most frequently appeared pottery type
freq_pottery <- sort(table(pottery_good_total(guojiazhuang_muzang)), 
                     decreasing = TRUE)[1:7]
freq_pottery
freq_pottery <- names(freq_pottery) 


#generate all unique combinations of the 7 elements up to 4
comb <- lapply(1:4, function(y) combn(freq_pottery, y)) %>% 
  lapply(function(y)
    y[,!duplicated(apply(y, 2, paste, collapse="."))])
comb

#generate the names of all the combinations
comb_names <- c()
for(i in 1:length(comb)){
  if(i == 1) {
    for(j in 1:length(comb[[i]])){
      comb_names <- c(comb_names, paste(comb[[i]][j]))
    }
  } else{
    cur <- as.matrix(comb[[i]])
    for(k in 1:ncol(cur)){
      comb_names <- c(comb_names, paste(cur[,k], collapse = "_"))
    }
  }
}
comb_names

#in pottery_Zhu, create a column for every combination
for(i in 1:length(comb_names)) {
  rest <- c(rep(NA, nrow(pottery_Zhu)))
  colName <- comb_names[i]
  curComb <- strsplit(comb_names[i], "_", fixed = TRUE)[[1]]
  for(j in 1:nrow(pottery_Zhu)){
    current <- pottery_Zhu[j,]
    if(sum(curComb %in% current$pottery_list[[1]]) == length(curComb)) {
      rest[j] <- TRUE
    } else {
      rest[j] <- FALSE
    }
  }
  pottery_Zhu <- cbind(pottery_Zhu, rest)
  names(pottery_Zhu)[names(pottery_Zhu) == "rest"] <- colName
}
head(pottery_Zhu)

write.xlsx2(pottery_Zhu, showNA = FALSE, "./pottery_combinations.xlsx")

# function to generate pottery combinations
generate_comb <- function(m, n) {
  result = 0
  if(n == 2){
    result <- data.frame(Freq = colSums(m[,c(14:34)])) %>% subset(Freq > 0) 
    result <- result[result$Freq >= 3,,drop = FALSE]
  } else if (n == 3) {
    result <- data.frame(Freq = colSums(m[,c(35:69)])) %>% subset(Freq > 0) 
    result <- result[result$Freq >= 3,,drop = FALSE]
  } else if (n == 4) {
    result <- data.frame(Freq = colSums(m[,c(70:104)])) %>% subset(Freq > 0) 
    result <- result[result$Freq >= 2,,drop = FALSE]
  }
  result <- result[order(result$Freq, nchar(rownames(result)), decreasing = TRUE),,drop = FALSE]
  return(result)
}

# create a common legend for graphs p (2vessels), q (3vessels), r (4vessels)
a_p <- pottery_Zhu[nchar(names(pottery_Zhu)) < 6 & nchar(names(pottery_Zhu)) > 3]
a_p <- a_p[,colSums(a_p) > 3]
a_p <- colSums(a_p) %>% sort(decreasing = TRUE)
cols_p <- randomColor(length(a_p))
names(cols_p) <- names(a_p)

a_q <- pottery_Zhu[nchar(names(pottery_Zhu)) > 6 & nchar(names(pottery_Zhu)) < 9]
a_q <- a_q[,colSums(a_q) > 3 ]
a_q <- colSums(a_q) %>% sort(decreasing = TRUE)
cols_q <- randomColor(length(a_q))
names(cols_q) <- names(a_q)

a_r <- pottery_Zhu[nchar(names(pottery_Zhu)) > 9 & nchar(names(pottery_Zhu)) < 12]
a_r <- a_r[,colSums(a_r) > 1 ]
a_r <- colSums(a_r) %>% sort(decreasing = TRUE)
cols_r <- randomColor(length(a_r))
names(cols_r) <- names(a_r)


# preparing graph for every period+zone combination: 
# p1-12: comb of 2 vessels; q1-12: comb of 3 vessels; r1-12: comb of 4 vessels
# later: can simplify using lapply
# https://stackoverflow.com/questions/42123920/arbitrary-number-of-plots-for-grid-arrange

# 甲组 group I
Zhu_I_2 <- pottery_Zhu %>% subset(period == "二" &  Zhu_zoneB == "甲")
p1 <- generate_comb(Zhu_I_2, 2)
q1 <- generate_comb(Zhu_I_2, 3)
r1 <- generate_comb(Zhu_I_2, 4)
print(paste(p1, q1, r1, sep = " "))
pp1 <- ggplot(p1, aes(x = "", y = Freq, fill = rownames(p1))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("甲组二期 Group I Period 2", "  Burial n = ", nrow(Zhu_I_2), sep = ""))
qq1 <- ggplot(q1) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_minimal() + xlab("") +
  ggtitle(paste("甲组二期 Group I Period 2", "  Burial n = ", nrow(Zhu_I_2), sep = ""))
rr1 <- ggplot(r1) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_minimal() + xlab("") +
  ggtitle(paste("甲组二期 Group I Period 2", "  Burial n = ", nrow(Zhu_I_2), sep = ""))

Zhu_I_3 <- pottery_Zhu %>% subset(period == "三" &  Zhu_zoneB == "甲")
p2 <- generate_comb(Zhu_I_3, 2)
q2 <- generate_comb(Zhu_I_3, 3)
r2 <- generate_comb(Zhu_I_3, 4)
print(paste(p2, q2, r2, sep = " "))
pp2 <- ggplot(p2, aes(x = "", y = Freq, fill = reorder(rownames(p2), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("甲组三期 Group I Period 3", "  Burial n = ", nrow(Zhu_I_3), sep = ""))
qq2 <- ggplot(q2, aes(x = "", y = Freq, fill = reorder(rownames(q2), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("甲组三期 Group I Period 3", "  Burial n = ", nrow(Zhu_I_3), sep = ""))
rr2 <- ggplot(r2, aes(x = "", y = Freq, fill = reorder(rownames(r2), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_r) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("甲组三期 Group I Period 3", "  Burial n = ", nrow(Zhu_I_3), sep = ""))


Zhu_I_4 <- pottery_Zhu %>% subset((period == "四早" &  Zhu_zoneB == "甲") | (period == "四晚" &  Zhu_zoneB == "甲"))
p3 <- generate_comb(Zhu_I_4, 2)
q3 <- generate_comb(Zhu_I_4, 3)
r3 <- generate_comb(Zhu_I_4, 4)
print(paste(p3, q3, r3, sep = " "))
pp3 <- ggplot(p3, aes(x = "", y = Freq, fill = reorder(rownames(p3), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("甲组四期 Group I Period 4", "  Burial n = ", nrow(Zhu_I_4), sep = ""))
qq3 <- ggplot(q3, aes(x = "", y = Freq, fill = reorder(rownames(q3), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("甲组四期 Group I Period 4", "  Burial n = ", nrow(Zhu_I_4), sep = ""))
rr3 <- ggplot(r3, aes(x = "", y = Freq, fill = reorder(rownames(r3), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_r) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("甲组四期 Group I Period 4", "  Burial n = ", nrow(Zhu_I_4), sep = ""))

# 乙组 group II
Zhu_II_2 <- pottery_Zhu %>% subset(period == "二" &  Zhu_zoneB == "乙")
p4 <- generate_comb(Zhu_II_2, 2)
q4 <- generate_comb(Zhu_II_2, 3)
r4 <- generate_comb(Zhu_II_2, 4)
pp4 <- qq4 <- rr4 <- ggplot(p4) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_minimal() + xlab("") +
  ggtitle("乙组二期 Group II Period 2  Burials n = 0")

Zhu_II_3 <- pottery_Zhu %>% subset(period == "三" &  Zhu_zoneB == "乙")
p5 <- generate_comb(Zhu_II_3, 2)
q5 <- generate_comb(Zhu_II_3, 3)
r5 <- generate_comb(Zhu_II_3, 4)

print(paste(p5, q5, r5, sep = " "))
pp5 <- ggplot(p5, aes(x = "", y = Freq, fill = reorder(rownames(p5), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("乙组三期 Group II Period 3", "  Burials n = ", nrow(Zhu_II_3), sep = ""))
qq5 <- ggplot(q5, aes(x = "", y = Freq, fill = reorder(rownames(q5), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("乙组三期 Group II Period 3", "  Burials n = ", nrow(Zhu_II_3), sep = ""))
rr5 <- ggplot(r5) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_minimal() + xlab("") +
  ggtitle(paste("乙组三期 Group II Period 3", "  Burials n = ", nrow(Zhu_II_3), sep = ""))

Zhu_II_4 <- pottery_Zhu %>% subset((period == "四早" &  Zhu_zoneB == "乙") | (period == "四晚" &  Zhu_zoneB == "乙"))
Zhu_II_4_comb
p6 <- generate_comb(Zhu_II_4, 2)
q6 <- generate_comb(Zhu_II_4, 3)
r6 <- generate_comb(Zhu_II_4, 4)
print(paste(p6, q6, r6, sep = " "))
pp6 <- ggplot(p6, aes(x = "", y = Freq, fill = reorder(rownames(p6), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("乙组四期 Group II Period 4", "  Burials n = ", nrow(Zhu_II_4), sep = ""))
qq6 <- ggplot(q6, aes(x = "", y = Freq, fill = reorder(rownames(q6), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("乙组四期 Group II Period 4", "  Burials n = ", nrow(Zhu_II_4), sep = ""))
rr6 <- ggplot(r6, aes(x = "", y = Freq, fill = reorder(rownames(r6), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_r) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("乙组四期 Group II Period 4", "  Burials n = ", nrow(Zhu_II_4), sep = ""))

#丙组 Gourp III
Zhu_III_2 <- pottery_Zhu %>% subset(period == "二" &  Zhu_zoneB == "丙")
p7 <- generate_comb(Zhu_III_2, 2)
q7 <- generate_comb(Zhu_III_2, 3)
r7 <- generate_comb(Zhu_III_2, 4)
print(paste(p7, q7, r7, sep = " "))
pp7 <- ggplot(p7, aes(x = "", y = Freq, fill = reorder(rownames(p7), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组二期 Group III Period 2", "  Burials n = ", nrow(Zhu_III_2), sep = ""))
qq7 <- ggplot(q7, aes(x = "", y = Freq, fill = reorder(rownames(q7), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组二期 Group III Period 2", "  Burials n = ", nrow(Zhu_III_2), sep = ""))
rr7 <- ggplot(r7) +
  geom_col() +
  coord_polar(theta = "y")  +
  theme_minimal() + xlab("") +
  ggtitle(paste("丙组二期 Group III Period 2", "  Burials n = ", nrow(Zhu_III_2), sep = ""))

Zhu_III_3 <- pottery_Zhu %>% subset(period == "三" &  Zhu_zoneB == "丙")
p8 <- generate_comb(Zhu_III_3, 2)
q8 <- generate_comb(Zhu_III_3, 3)
r8 <- generate_comb(Zhu_III_3, 4)
print(paste(p8, q8, r8, sep = " "))
pp8 <- ggplot(p8, aes(x = "", y = Freq, fill = reorder(rownames(p8), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组三期 Group III Period 3", "  Burials n = ", nrow(Zhu_III_3), sep = ""))
qq8 <- ggplot(q8, aes(x = "", y = Freq, fill = reorder(rownames(q8), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组三期 Group III Period 3", "  Burials n = ", nrow(Zhu_III_3), sep = ""))
rr8 <- ggplot(r8, aes(x = "", y = Freq, fill = reorder(rownames(r8), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_r) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组三期 Group III Period 3", "  Burials n = ", nrow(Zhu_III_3), sep = ""))

Zhu_III_4 <- pottery_Zhu %>% subset((period == "四早" &  Zhu_zoneB == "丙") | (period == "四晚" &  Zhu_zoneB == "丙"))
p9 <- generate_comb(Zhu_III_4, 2)
q9 <- generate_comb(Zhu_III_4, 3)
r9 <- generate_comb(Zhu_III_4, 4)
print(paste(p9, q9, r9, sep = " "))
pp9 <- ggplot(p9, aes(x = "", y = Freq, fill = reorder(rownames(p9), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组四期 Group III Period 4", "Burials n = ", nrow(Zhu_III_4), sep = ""))
qq9 <- ggplot(q9, aes(x = "", y = Freq, fill = reorder(rownames(q9), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组四期 Group III Period 4", "Burials n = ", nrow(Zhu_III_4), sep = ""))
rr9 <- ggplot(r9, aes(x = "", y = Freq, fill = reorder(rownames(r9), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_r) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丙组四期 Group III Period 4", " Burials n = ", nrow(Zhu_III_4), sep = ""))

# 丁组
Zhu_IV_2 <- pottery_Zhu %>% subset(period == "二" &  Zhu_zoneB == "丁")
Zhu_IV_2_comb <- 
Zhu_IV_2_comb
p10 <- generate_comb(Zhu_IV_2, 2)
q10 <- generate_comb(Zhu_IV_2, 3)
r10 <- generate_comb(Zhu_IV_2, 4)
print(paste(p10, q10, r10, sep = " "))
  
pp10 <- qq10 <- rr10 <- ggplot(q10) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丁组二期 Group IV Period 2 Burials n = ", nrow(Zhu_IV_2), sep = ""))

Zhu_IV_3 <- pottery_Zhu %>% subset(period == "三" &  Zhu_zoneB == "丁")
p11 <- generate_comb(Zhu_IV_3, 2)
q11 <- generate_comb(Zhu_IV_3, 3)
r11 <- generate_comb(Zhu_IV_3, 4)
print(paste(p11, q11, r11, sep = " "))
pp11 <- qq11 <- rr11 <- ggplot(q11) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丁组三期 Group IV Period 3 Burials n = ", nrow(Zhu_IV_3), sep = ""))

Zhu_IV_4 <- pottery_Zhu %>% subset((period == "四早" &  Zhu_zoneB == "丁") | (period == "四晚" &  Zhu_zoneB == "丁"))
p12 <- generate_comb(Zhu_IV_4, 2)
q12 <- generate_comb(Zhu_IV_4, 3)
r12 <- generate_comb(Zhu_IV_4, 4)
print(paste(p12, q12, r12, sep = " "))
pp12 <- ggplot(p12, aes(x = "", y = Freq, fill = reorder(rownames(p12), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丁组四期 Group IV Period 4", " Burials n = ", nrow(Zhu_IV_4), sep = ""))

qq12 <- ggplot(q12, aes(x = "", y = Freq, fill = reorder(rownames(q12), Freq))) +
  geom_col() +
  coord_polar(theta = "y") + 
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  geom_text(aes(label = Freq), position = position_stack(vjust = .5)) +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丁组四期 Group IV Period 4", " Burials n = ", nrow(Zhu_IV_4), sep = ""))

rr12 <- ggplot(r12) +
  geom_col() +
  coord_polar(theta = "y") +
  theme_minimal() + xlab("") +
  theme(legend.position = "none") +
  ggtitle(paste("丁组四期 Group IV Period 4", " Burials n = ", nrow(Zhu_IV_4), sep = ""))

#display all the plots
grid.arrange(pp1, pp2, pp3, pp4, pp5, pp6, pp7, pp8, pp9, pp10, pp11, pp12, nrow = 4, ncol = 3)
grid.arrange(qq1, qq2, qq3, qq4, qq5, qq6, qq7, qq8, qq9, qq10, qq11, qq12, nrow = 4, ncol = 3)
grid.arrange(rr1, rr2, rr3, rr4, rr5, rr6, rr7, rr8, rr9, rr10, rr11, rr12, nrow = 4, ncol = 3)

# display legend
qplot(x=cols_p, y = 1, fill = names(cols_p), geom = "tile") +
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  xlab(aes(label = cols)) +
  theme_void()
qplot(x=cols_q, y = 1, fill = names(cols_q), geom = "tile") +
  scale_fill_manual(name = "Pottery combination", values = cols_q) +
  xlab(aes(label = cols)) +
  theme_void()
qplot(x=cols_r, y = 1, fill = names(cols_r), geom = "tile") +
  scale_fill_manual(name = "Pottery combination", values = cols_r) +
  xlab(aes(label = cols)) +
  theme_void()

#=====Primary pottery type by period and group====
pottery_good_total <- function (k) {
  if(nrow(k) == 0) {
    return(NA)
  } else{
    pottery_type <- c()
    for (i in 1:nrow(k)) {
      current <- k[i, ]
      temp <- strsplit(current$burial_good_pottery, ".", fixed = TRUE)
      temp[[1]] <- str_replace_all(temp[[1]], c("A|B|C|D|E|F|G|H|a|b|c|V|X|I"), "")
      pottery_type <- c(pottery_type, temp[[1]])
    }
    return(na.omit(pottery_type))
  }
}

#Displayed by group--no clear distinction
pottery_Zhu.df <- data.frame(matrix(nrow = length(na.omit(unique(guojiazhuang_muzang$Zhu_zoneB))), ncol = 2))
colnames(pottery_Zhu.df) <- c("Zhu_zoneB", "pottery_list")
pottery_Zhu.df$Zhu_zoneB <- c("甲", "乙", "丙", "丁")
for(i in 1:nrow(pottery_Zhu.df)) {
  pottery_Zhu.df$pottery_list[i] <- guojiazhuang_muzang %>%
    subset(Zhu_zoneB == pottery_Zhu.df[[1]][i]) %>%
    pottery_good_total() %>% list()
  sort(table(pottery_Zhu.df$pottery_list[i]), decreasing = TRUE)
}

b <- na.omit(unique(pottery_good_total(subset(guojiazhuang_muzang, !is.na(Zhu_zoneB)))))
colsT <- randomColor(length(b))
names(colsT) <- b

zt_n1 <-
  data.frame(sort(table(pottery_Zhu.df$pottery_list[1]), decreasing = TRUE))
zt_n2 <-
  data.frame(sort(table(pottery_Zhu.df$pottery_list[2]), decreasing = TRUE))
zt_n3 <-
  data.frame(sort(table(pottery_Zhu.df$pottery_list[3]), decreasing = TRUE))
zt_n4 <-
  data.frame(sort(table(pottery_Zhu.df$pottery_list[4]), decreasing = TRUE))


ztp1 <- ggplot(data = zt_n1, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("Group I (Zhu)") +
  theme(legend.position = "none")
ztp2 <- ggplot(data = zt_n2, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("Group II (Zhu)") +
  theme(legend.position = "none")
ztp3 <- ggplot(data = zt_n3, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("Group III (Zhu)") +
  theme(legend.position = "none")
ztp4 <- ggplot(data = zt_n4, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("Group IV (Zhu)") +
  theme(legend.position = "none")

grid.arrange(ztp1, ztp2, ztp3, ztp4, nrow = 2, ncol = 2) #display all the plots


# By period and by group
pottery_Zhu_period.df <- data.frame(matrix(nrow = 12, ncol = 3))
colnames(pottery_Zhu_period.df) <- c("Zhu_zoneB_period", "pottery_list", "n_burial")
pottery_Zhu_period.df$Zhu_zoneB_period <- 
  c("甲二", "甲三", "甲四", "乙二", "乙三", "乙四", 
    "丙二", "丙三", "丙四", "丁二", "丁三", "丁四")
for(i in 1:nrow(pottery_Zhu_period.df)) {
  pottery_Zhu_period.df$pottery_list[i] <- guojiazhuang_muzang %>% 
    subset(Zhu_zoneB == str_split(pottery_Zhu_period.df[[1]][i], "")[[1]][1]) %>% 
    filter(grepl(str_split(pottery_Zhu_period.df[[1]][i], "")[[1]][2], period)) %>%
    pottery_good_total() %>% list()
  pottery_Zhu_period.df$n_burial[i] <- guojiazhuang_muzang %>% 
    subset(Zhu_zoneB == str_split(pottery_Zhu_period.df[[1]][i], "")[[1]][1]) %>% 
    filter(grepl(str_split(pottery_Zhu_period.df[[1]][i], "")[[1]][2], period)) %>% nrow()
  sort(table(pottery_Zhu_period.df$pottery_list[i]), decreasing = TRUE)
}

b <- na.omit(unique(pottery_good_total(subset(guojiazhuang_muzang))))
colsT <- randomColor(length(b))
names(colsT) <- b

ztp_n1 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[1]), decreasing = TRUE))
ztp_n2 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[2]), decreasing = TRUE))
ztp_n3 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[3]), decreasing = TRUE))
ztp_n4 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[4]), decreasing = TRUE))
ztp_n5 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[5]), decreasing = TRUE))
ztp_n6 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[6]), decreasing = TRUE))
ztp_n7 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[7]), decreasing = TRUE))
ztp_n8 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[8]), decreasing = TRUE))
ztp_n9 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[9]), decreasing = TRUE))
ztp_n10 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[10]), decreasing = TRUE))
ztp_n11 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[11]), decreasing = TRUE))
ztp_n12 <-
  data.frame(sort(table(pottery_Zhu_period.df$pottery_list[12]), decreasing = TRUE))


ztpv1 <- ggplot(data = ztp_n1, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("甲組二期 Group I Period 2 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[1]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv2 <- ggplot(data = ztp_n2, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("甲組三期 Group I Period 3 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[2]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv3 <- ggplot(data = ztp_n3, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("甲組四期 Group I Period 4 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[3]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv4 <- ggplot(data = ztp_n4) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("乙組二期 Group II Period 2 (Zhu)", " Burials n = ", 0, sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv5 <- ggplot(data = ztp_n5, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("乙組三期 Group II Period 3 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[5]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv6 <- ggplot(data = ztp_n6, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("乙組四期 Group II Period 4 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[6]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv7 <- ggplot(data = ztp_n7, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("丙組二期 Group III Period 2 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[7]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv8 <- ggplot(data = ztp_n8, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("丙組三期 Group III Period 3 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[8]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv9 <- ggplot(data = ztp_n9, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("丙組四期 Group III Period 4 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[9]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv10 <- ggplot(data = ztp_n10, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("丁組二期 Group IV Period 2 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[10]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv11 <- ggplot(data = ztp_n11, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("丁組三期 Group IV Period 3 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[11]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")
ztpv12 <- ggplot(data = ztp_n12, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("丁組四期 Group IV Period 4 (Zhu)", " Burials n = ", sum(pottery_Zhu_period.df$n_burial[12]), sep = "")) +
  xlab("")+
  theme(legend.position = "none")

grid.arrange(ztpv1, ztpv2, ztpv3, ztpv4, ztpv5, ztpv6, ztpv7, ztpv8, ztpv9, ztpv10, 
             ztpv11, ztpv12, nrow = 4, ncol = 3) #display all the plots


# ====Testing Tang Jigen 2004====
# ====Tang areas (B-level cluster)====
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Tang_zoneB)) %>%
  ggplot(aes(x = Tang_zoneB, y = as.numeric(tomb_area))) +
  geom_boxplot(notch = FALSE,
               fill = "steelblue",
               varwidth = TRUE) +
  labs(x = "Tomb Zone (Zhu)",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Area Distribution (Tang)") +
  stat_summary(
    aes(label = round(stat(y), 1)),
    geom = "text",
    fun = function(y) {
      o <- boxplot.stats(y)$out
      if (length(o) == 0)
        NA
      else
        o
    },
    hjust = -.3
  ) +
  scale_y_continuous(labels = comma)

# histogram
mu <- guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Tang_zoneB)) %>%
  ddply(
    "Tang_zoneB",
    summarise,
    grp.mean = mean(tomb_area),
    grp.median = median(tomb_area),
    grp.max = max(tomb_area),
    grp.min = min(tomb_area)
  )
mu
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Tang_zoneB)) %>%
  ggplot(aes(x = tomb_area, color = Tang_zoneB)) +
  geom_histogram(fill = NA, binwidth = 1000) +
  xlab("Tomb area (sq.cm)") + ylab("Frequency") +
  ggtitle("Guojiazhuang Tomb Area Distribution (Tang)") +
  geom_vline(
    data = mu,
    aes(xintercept = grp.mean, color = Tang_zoneB),
    linetype = "dashed",
    size = 0.5
  ) +
  geom_vline(
    data = mu,
    aes(xintercept = grp.median, color = Tang_zoneB),
    linetype = "solid",
    size = 1
  ) +
  scale_x_continuous(labels = comma)

# Orientation
guojiazhuang_muzang %>%
  filter(!is.na(orientation_des),!is.na(Tang_zoneB)) %>%
  ggplot(aes(x = Tang_zoneB, fill = orientation_des)) +
  geom_bar(position = "stack") +
  xlab("Tomb group (Tang)") +
  ggtitle("Tomb group (Tang) and tomb orientation")

# Waist pit
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(Tang_zoneB)) %>%
  ggplot(aes(x = Tang_zoneB, fill = waist_pit)) +
  geom_bar(position = "stack") +
  xlab("Tomb group (Tang)") +
  ggtitle("Tomb group (Tang) and existence of waist pit")


# ====Tang subgroups (level A cluster)====
# Tomb size distribution
guojiazhuang_muzang %>%
  filter(!is.na(tomb_area),!is.na(Tang_zoneA)) %>%
  ggplot(aes(x = Tang_zoneA, y = tomb_area)) +
  geom_boxplot(notch = FALSE,
               fill = "steelblue",
               varwidth = TRUE) +
  labs(x = "Tomb group Archaeological Report",
       y = "Tomb Area (sq.cm)",
       title = "Guojiazhuang Tomb Area Distribution (Zhu)") +
  stat_summary(
    aes(label = round(stat(y), 1)),
    geom = "text",
    fun = function(y) {
      o <- boxplot.stats(y)$out
      if (length(o) == 0)
        NA
      else
        o
    },
    hjust = -.3
  ) +
  scale_y_continuous(labels = comma)

# Tomb orientation in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(orientation_des),!is.na(Tang_zoneA)) %>%
  ggplot(aes(x = Tang_zoneA, fill = orientation_des)) +
  geom_bar(position = "stack") +
  xlab("Subgroups") +
  ggtitle("Tomb orientation in each subgroup (Tang)")

# waist pits in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(waist_pit),!is.na(Tang_zoneA)) %>%
  ggplot(aes(x = Tang_zoneA, fill = waist_pit)) +
  geom_bar(position = "stack") +
  xlab("Subgroups") +
  ggtitle("Tomb orientation in each subgroups (Tang)")

# period in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(period),!is.na(Tang_zoneA)) %>%
  ggplot(aes(x = Tang_zoneA, fill = period)) +
  geom_bar(position = "stack") +
  xlab("Subgroups") +
  ggtitle("Tomb period in each subgroups (Tang)") +
  scale_fill_discrete(breaks = c("二", "三", "四早", "四晚"))

# vcd residual based-shadinng
guojiazhuang_muzang[,c("period", "Tang_zoneA", "orientation_des")] %>%
  filter(!is.na(orientation_des),!is.na(Tang_zoneA), !is.na(period)) %>%
  table %>%
  mosaic(shade = TRUE)

# waist pits in the subgroups
guojiazhuang_muzang %>%
  filter(!is.na(Tang_zoneA)) %>%
  ggplot(aes(x = Tang_zoneA, fill = burial_dog_waist)) +
  geom_bar(position = "stack") +
  xlab("Has waist pit") +
  ggtitle("Tomb orientation in each subgroups (Tang)")


#====Types of pottery goods in B-level groups====
# except the big tombs and chariot pit
pottery_Tang.df <- data.frame(matrix(nrow = length(na.omit(unique(guojiazhuang_muzang$Tang_zoneB))), ncol = 2))
colnames(pottery_Tang.df) <- c("Tang_zoneB", "pottery_list")
pottery_Tang.df$Tang_zoneB <- seq(1, 15, by = 1)
for(i in 1:nrow(pottery_Tang.df)) {
  pottery_Tang.df$pottery_list[i] <- guojiazhuang_muzang %>%
    subset(Tang_zoneB == paste("B", i, sep = "")) %>%
    pottery_good_total() %>% list()
  sort(table(pottery_Tang.df$pottery_list[i]), decreasing = TRUE)
}

dt_n1 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[1]), decreasing = TRUE))
dt_n2 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[2]), decreasing = TRUE))
dt_n3 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[3]), decreasing = TRUE))
dt_n4 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[4]), decreasing = TRUE))
dt_n5 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[5]), decreasing = TRUE))
dt_n6 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[6]), decreasing = TRUE))
dt_n7 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[7]), decreasing = TRUE))
dt_n8 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[8]), decreasing = TRUE))
dt_n9 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[9]), decreasing = TRUE))
dt_n10 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[10]), decreasing = TRUE))
dt_n11 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[11]), decreasing = TRUE))
dt_n12 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[12]), decreasing = TRUE))
dt_n13 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[13]), decreasing = TRUE))
dt_n14 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[14]), decreasing = TRUE))
dt_n15 <-
  data.frame(sort(table(pottery_Tang.df$pottery_list[15]), decreasing = TRUE))

b <- na.omit(unique(pottery_good_total(subset(guojiazhuang_muzang, !is.na(Tang_zoneB)))))
colsT <- randomColor(length(b))
names(colsT) <- b

tp1 <- ggplot(data = dt_n1, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B1 (Tang)") +
  theme(legend.position = "none")
tp2 <- ggplot(data = dt_n2, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B2 (Tang)") +
  theme(legend.position = "none")
tp3 <- ggplot(data = dt_n3, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B3 (Tang)") +
  theme(legend.position = "none")
tp4 <- ggplot(data = dt_n4, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B4 (Tang)") +
  theme(legend.position = "none")
tp5 <- ggplot(data = dt_n5, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B5 (Tang)") +
  theme(legend.position = "none")
tp6 <- ggplot(data = dt_n6, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B6 (Tang)") +
  theme(legend.position = "none")
tp7 <- ggplot(data = dt_n7, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B7 (Tang)") +
  theme(legend.position = "none")
tp8 <- ggplot(data = dt_n8, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B8 (Tang)") +
  theme(legend.position = "none")
tp9 <- ggplot(data = dt_n9, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B9 (Tang)") +
  theme(legend.position = "none")
tp10 <- ggplot(data = dt_n10, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B10 (Tang)") +
  theme(legend.position = "none")
tp11 <- ggplot(data = dt_n11, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B11 (Tang)") +
  theme(legend.position = "none")
tp12 <- ggplot(data = dt_n12, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B12 (Tang)") +
  theme(legend.position = "none")
tp13 <- ggplot(data = dt_n13, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B13 (Tang)") +
  theme(legend.position = "none")
tp14 <- ggplot(data = dt_n14, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B14 (Tang)") +
  theme(legend.position = "none")
tp15 <- ggplot(data = dt_n15, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle("B15 (Tang)") +
  theme(legend.position = "none")


#display all the plots
grid.arrange(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, 
             tp14, tp15, nrow = 4, ncol = 4)




#====pottery goods general trend=====

pottery.df <- data.frame(matrix(nrow = 4, ncol = 2))
colnames(pottery.df) <- c("period", "pottery_list")
pottery.df$period <- c("二", "三", "四早", "四晚")
for(i in 1:nrow(pottery.df)) {
  pottery.df$pottery_list[i] <- guojiazhuang_muzang %>% 
    filter(period == pottery.df$period[i]) %>%
    pottery_good_total() %>% list()
  sort(table(pottery.df$pottery_list[i]), decreasing = TRUE)
}

dtp_n1 <-
  data.frame(sort(table(pottery.df$pottery_list[1]), decreasing = TRUE))
dtp_n2 <-
  data.frame(sort(table(pottery.df$pottery_list[2]), decreasing = TRUE))
dtp_n3 <-
  data.frame(sort(table(pottery.df$pottery_list[3]), decreasing = TRUE))
dtp_n4 <-
  data.frame(sort(table(pottery.df$pottery_list[4]), decreasing = TRUE))


d <- na.omit(unique(pottery_good_total(subset(guojiazhuang_muzang))))
colsT <- hue_pal()(length(d))
names(colsT) <- d

tpp1 <- ggplot(data = dtp_n1, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  xlab("")+
  ggtitle("第二期 Period II n = 14") +
  theme(legend.position = "none")
tpp2 <- ggplot(data = dtp_n2, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  xlab("")+
  ggtitle("第三期 Period III n = 60") +
  theme(legend.position = "none")
tpp3 <- ggplot(data = dtp_n3, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  xlab("")+
  ggtitle("第四期早 IVa n = 39") +
  theme(legend.position = "none")
tpp4 <- ggplot(data = dtp_n4, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  xlab("")+
  ggtitle("第四期晚 IVb n = 30") +
  theme(legend.position = "none")



#display all the plots
grid.arrange(tpp1, tpp2, tpp3, tpp4, nrow = 2, ncol = 2)




#======pottery combination=====

l <- guojiazhuang_muzang %>% filter(num == "263" | num == "264"| num == "265"| num == "266"| num == "267"| num == "269"| num == "270") %>% 
  pottery_good_total() %>% list()
data.frame(sort(table(l), decreasing = TRUE)) %>% 
  ggplot(aes(x = l, y = Freq, fill = l)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("l", "n = 7")) +
  theme(legend.position = "none")

m <- guojiazhuang_muzang %>% filter(num == "61" | num == "63"| num == "64"| num == "66"| num == "67"| num == "69"| num == "70") %>%
  pottery_good_total() %>% list()
data.frame(sort(table(m), decreasing = TRUE)) %>% 
  ggplot(aes(x = m, y = Freq, fill = m)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("m", "n = 6")) +
  theme(legend.position = "none")

n <- guojiazhuang_muzang %>% filter(num == "99" | num == "98"| num == "137"| num == "138") %>%
  pottery_good_total() %>% list()
data.frame(sort(table(n), decreasing = TRUE)) %>% 
  ggplot(aes(x = n, y = Freq, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Pottery combination", values = colsT) +
  ggtitle(paste("n", "n = 6")) +
  theme(legend.position = "none")

  
pottery_list <- guojiazhuang_muzang %>% filter(num == "263" | num == "264"| num == "265"| num == "266"| num == "267"| num == "269"| num == "270")
pottery_list <- pottery_list[,c(1:6)]
head(pottery_list)

for (i in 1:nrow(pottery_list)) {  #parse burial goods
  current <- pottery_list[i, ]
  temp <- strsplit(current$burial_good_pottery, ".", fixed = TRUE)
  temp[[1]] <-str_replace_all(temp[[1]], c("A|B|C|D|E|F|G|H|a|b|c|V|X|I"), "")
  pottery_list$pottery_list[i] <- temp
}


# find the top 7 most frequently appeared pottery type
freq_pottery <- sort(table(pottery_good_total(pottery_list)), 
                     decreasing = TRUE)
freq_pottery
freq_pottery <- names(freq_pottery) 


#generate all unique combinations of the 7 elements up to 4
comb <- lapply(1:4, function(y) combn(freq_pottery, y)) %>% 
  lapply(function(y)
    y[,!duplicated(apply(y, 2, paste, collapse="."))])

#generate the names of all the combinations
comb_names <- c()
for(i in 1:length(comb)){
  if(i == 1) {
    for(j in 1:length(comb[[i]])){
      comb_names <- c(comb_names, paste(comb[[i]][j]))
    }
  } else{
    cur <- as.matrix(comb[[i]])
    for(k in 1:ncol(cur)){
      comb_names <- c(comb_names, paste(cur[,k], collapse = "_"))
    }
  }
}

#in pottery_Zhu, create a column for every combination
for(i in 1:length(comb_names)) {
  rest <- c(rep(NA, nrow(pottery_list)))
  colName <- comb_names[i]
  curComb <- strsplit(comb_names[i], "_", fixed = TRUE)[[1]]
  for(j in 1:nrow(pottery_list)){
    current <- pottery_list[j,]
    if(sum(curComb %in% current$pottery_list[[1]]) == length(curComb)) {
      rest[j] <- TRUE
    } else {
      rest[j] <- FALSE
    }
  }
  pottery_list <- cbind(pottery_list, rest)
  names(pottery_list)[names(pottery_list) == "rest"] <- colName
}
head(pottery_list)

colnames(pottery_list)
# function to generate pottery combinations
generate_comb <- function(m, n) {
  result = 0
  if(n == 2){
    result <- data.frame(Freq = colSums(m[,c(17:52)])) %>% subset(Freq > 0) 
    result <- result[result$Freq >= 2,,drop = FALSE]
  } else if (n == 3) {
    result <- data.frame(Freq = colSums(m[,c(53:136)])) %>% subset(Freq > 0) 
    result <- result[result$Freq >= 2,,drop = FALSE]
  } else if (n == 4) {
    result <- data.frame(Freq = colSums(m[,c(137:262)])) %>% subset(Freq > 0) 
    result <- result[result$Freq >= 2,,drop = FALSE]
  }
  result <- result[order(result$Freq, nchar(rownames(result)), decreasing = TRUE),,drop = FALSE]
  return(result)
}

# create a common legend for graphs p (2vessels), q (3vessels), r (4vessels)
a_p <- pottery_list[nchar(names(pottery_list)) < 6 & nchar(names(pottery_list)) > 3]
a_p <- colSums(a_p) %>% sort(decreasing = TRUE)
cols_p <- randomColor(length(a_p))
names(cols_p) <- names(a_p)

a_q <- pottery_list[nchar(names(pottery_list)) > 6 & nchar(names(pottery_list)) < 9]
a_q <- colSums(a_q) %>% sort(decreasing = TRUE)
cols_q <- randomColor(length(a_q))
names(cols_q) <- names(a_q)

a_r <- pottery_list[nchar(names(pottery_list)) > 9 & nchar(names(pottery_list)) < 12]
a_r <- colSums(a_r) %>% sort(decreasing = TRUE)
cols_r <- randomColor(length(a_r))
names(cols_r) <- names(a_r)


# 甲组 group I
p1 <- generate_comb(pottery_list, 2)
p11 <- data.frame(rownames(p1))
p11 <- cbind(p11, p1)
colnames(p11) <- c("names", "Freq")
q1 <- generate_comb(pottery_list, 3)
q11 <- data.frame(rownames(q1))
q11 <- cbind(q11, q1)
colnames(q11) <- c("names", "Freq")
r1 <- generate_comb(pottery_list, 4)
print(paste(p1, q1, r1, sep = " "))
ggplot(data = p11, aes(x = reorder(names, -Freq), y = Freq, fill = names)) +
  geom_bar(stat = "identity") +
  ggtitle("2") +
  scale_fill_manual(name = "Pottery combination", values = cols_p) +
  theme(legend.position = "none")
ggplot(data = q11, aes(x = reorder(names, -Freq), y = Freq, fill = names)) +
  geom_bar(stat = "identity") +
  ggtitle("3") +
  theme(legend.position = "none")


#================================This is the end!=======================

# #===Burial Goods===
# 
# #====Bronze goods====
# bronze_type <- c()
# for (i in 1:nrow(guojiazhuang_muzang)) {
#   current <- guojiazhuang_muzang[i, ]
#   temp <- strsplit(current$burial_good_bronze, ".", fixed = TRUE)
#   temp[[1]] <- str_replace_all(temp[[1]], c("A|B|C|V|I"), "")
#   guojiazhuang_muzang$bronze_list[i] <- temp
#   bronze_type <- c(bronze_type, temp[[1]])
# }
# bronze_type <- unique(bronze_type)
# 
# colnames(bronze_egde) <- c("source", "target", "weight")
# source <- c()
# target <- c()
# 
# # 同一vector內兩兩組隊成為一個edge, push the result into source and target lists.
# for (i in 1:nrow(guojiazhuang_muzang)) {
#   cur <- unique(guojiazhuang_muzang$bronze_list[[i]])
#   if (length(cur) > 1) {
#     # print(i)
#     # print(paste("bronze_list length is ", length(cur)))
#     for (j in 1:(length(cur) - 1)) {
#       for (k in (j + 1):length(cur)) {
#         # print(paste("j is", j, "k is", k))
#         source <- c(source, cur[j])
#         # print(paste("current j is ", cur[j]))
#         target <- c(target, cur[k])
#         # print(paste("current k is ",cur[k]))
#         # print("end")
#       }
#     }
#   }
# }
# 
# bronze_edge <-
#   data.frame(source, target, weight = rep(c(1), times = length(source)))
# bronze_edge <-
#   bronze_edge[order(bronze_edge$source, bronze_edge$target), ]
# 
# # Check for duplicates, if there are, add to weight.
# i = 1
# while (i < nrow(bronze_edge)) {
#   j <- i + 1
#   while (j < nrow(bronze_edge)) {
#     # print(paste("currently on round ", i, "examining row ", j))
#     if (((bronze_edge$source[i] == bronze_edge$source[j]) &
#          (bronze_edge$target[i] == bronze_edge$target[j])
#     ) |
#     ((bronze_edge$source[i] == bronze_edge$target[j]) &
#      (bronze_edge$target[i] == bronze_edge$source[j])
#     )) {
#       # print(paste("found duplicates for ", i, edge_list$source[i], edge_list$target[i],
#       # "at row ", j, edge_list$source[j], edge_list$target[j]))
#       bronze_edge <- dplyr::slice(bronze_edge,-c(j))
#       # print(paste("length of edge_list is ", nrow(edge_list)))
#       bronze_edge$weight[i] <- bronze_edge$weight[i] + 1
#       j <- j - 1
#     }
#     j <- j + 1
#   }
#   i <- i + 1
# }
# 
# write.xlsx2(
#   bronze_edge,
#   showNA = FALSE,
#   "/Users/zhouyuwei/Desktop/Guojiazhuang/guojiazhuang_bronze_edge.xlsx"
# )
# 
# 
# 
# 
# 
# bronze_type <- c()
# for (i in 1:nrow(guojiazhuang_muzang)) {
#   current <- guojiazhuang_muzang[i, ]
#   temp <- strsplit(current$burial_good_bronze, ".", fixed = TRUE)
#   temp[[1]] <- str_replace_all(temp[[1]], c("A|B|C|V|I"), "")
#   guojiazhuang_muzang$bronze_list[i] <- temp
#   bronze_type <- c(bronze_type, temp[[1]])
# }
# bronze_type <- na.omit(unique(bronze_type))
# 
# bronze_obj <-
#   data.frame(matrix(ncol = 2, nrow = length(bronze_type)))
# colnames(bronze_obj) <- c("bronze_type", "tombs", "cnt")
# bronze_obj$bronze_type <- bronze_type
# 
# for (i in 1:length(bronze_type)) {
#   temp <- c()
#   for (j in 1:nrow(guojiazhuang_muzang)) {
#     current <- guojiazhuang_muzang[j, ]
#     if (grepl(bronze_type[i], current$burial_good_bronze)) {
#       temp <- c(temp, current$num)
#     }
#   }
#   bronze_obj$tombs[i] <- list(temp)
#   bronze_obj$cnt[i] <- length(temp)
# }
# 
# 
# 
# 
# arrange(bronze_obj, desc(cnt))
# comb <- function(c) {
#   total = 2 ^ length(c)
#   ret_obj <-
#     data.frame(matrix(
#       ncol = 3,
#       nrow = total = 2 ^ length(c) - 1
#     ))
#   colnames(bronze_obj) <- c("bronze_type", "tombs", "cnt")
#   for (i in 1:length(c)) {
#     
#   }
# }
# Reduce(intersect,
#        list(bronze_obj$tombs[1][[1]], bronze_obj$tombs[2][[1]], bronze_obj$tombs[3][[1]]))
# 
# 
# 
# #====pottery goods====
# pottery_type <- c()
# for (i in 1:nrow(guojiazhuang_muzang)) {
#   current <- guojiazhuang_muzang[i, ]
#   temp <- strsplit(current$burial_good_pottery, ".", fixed = TRUE)
#   temp[[1]] <-
#     str_replace_all(temp[[1]], c("A|B|C|D|E|F|G|H|a|b|c|V|X|I"), "")
#   guojiazhuang_muzang$pottery_list[i] <- temp
#   pottery_type <- c(pottery_type, temp[[1]])
# }
# pottery_type <- unique(pottery_type)
# 
# colnames(pottery_egde) <- c("source", "target", "weight")
# source <- c()
# target <- c()
# 
# # 同一vector內兩兩組隊成為一個edge, push the result into source and target lists.
# for (i in 1:nrow(guojiazhuang_muzang)) {
#   cur <- unique(guojiazhuang_muzang$pottery_list[[i]])
#   if (length(cur) > 1) {
#     # print(i)
#     # print(paste("pottery_list length is ", length(cur)))
#     for (j in 1:(length(cur) - 1)) {
#       for (k in (j + 1):length(cur)) {
#         # print(paste("j is", j, "k is", k))
#         source <- c(source, cur[j])
#         # print(paste("current j is ", cur[j]))
#         target <- c(target, cur[k])
#         # print(paste("current k is ",cur[k]))
#         # print("end")
#       }
#     }
#   }
# }
# 
# pottery_edge <-
#   data.frame(source, target, weight = rep(c(1), times = length(source)))
# pottery_edge <-
#   pottery_edge[order(pottery_edge$source, pottery_edge$target), ]
# 
# # Check for duplicates, if there are, add to weight.
# i = 1
# while (i < nrow(pottery_edge)) {
#   j <- i + 1
#   while (j < nrow(pottery_edge)) {
#     # print(paste("currently on round ", i, "examining row ", j))
#     if (((pottery_edge$source[i] == pottery_edge$source[j]) &
#          (pottery_edge$target[i] == pottery_edge$target[j])
#     ) |
#     ((pottery_edge$source[i] == pottery_edge$target[j]) &
#      (pottery_edge$target[i] == pottery_edge$source[j])
#     )) {
#       # print(paste("found duplicates for ", i, edge_list$source[i], edge_list$target[i],
#       # "at row ", j, edge_list$source[j], edge_list$target[j]))
#       pottery_edge <- dplyr::slice(pottery_edge,-c(j))
#       # print(paste("length of edge_list is ", nrow(edge_list)))
#       pottery_edge$weight[i] <- pottery_edge$weight[i] + 1
#       j <- j - 1
#     }
#     j <- j + 1
#   }
#   i <- i + 1
# }
# 
# write.xlsx2(
#   pottery_edge,
#   showNA = FALSE,
#   "/Users/zhouyuwei/Desktop/Guojiazhuang/guojiazhuang_pottery_edge.xlsx"
# )

#====Cluster Analysis of burial goods===

library(stringr)
all_pottery <- names(sort(table(pottery_good_total(guojiazhuang_muzang)), decreasing = TRUE))
pottery_cluster_ana <- data.frame(matrix(0, nrow = nrow(guojiazhuang_muzang), ncol = (length(all_pottery)+3)))
colnames(pottery_cluster_ana) <- c("num", "burial_good_pottery", "pottery_type", all_pottery)
pottery_list <- guojiazhuang_muzang[,c("num", "burial_good_pottery")]
for(i in 1:nrow(pottery_list)) {
  pottery_type <- c()
  current <- pottery_list[i,2]
  pottery_cluster_ana[i, c(1,2)] <- pottery_list[i,]
  if(!is.na(current$burial_good_pottery)){
    temp <- strsplit(current$burial_good_pottery, ".", fixed = TRUE)
    pottery_type <- str_replace_all(temp[[1]], c("A|B|C|D|E|F|G|H|a|b|c|V|X|I"), "")
    pottery_cluster_ana[i, c(3)] <- paste(pottery_type, collapse = ",")
    # for(j in pottery_type){ # not considering the number
    #   if(pottery_cluster_ana[i,j] == 0){
    #     pottery_cluster_ana[i,j] = pottery_cluster_ana[i,j] + 1
    #   }
    # } 
    for(j in pottery_type){ # considering the number of vessels
      if(pottery_cluster_ana[i,j] == 0){ 
        pottery_cluster_ana[i,j] = pottery_cluster_ana[i,j] + 1
      }
    }
  } else {
    pottery_cluster_ana[i, c(3)] <- NA
  }
}
View(pottery_cluster_ana)

# Case 1: Hiearchical Cluster Analysis
for(i in 1:nrow(pottery_cluster_ana)) {
  temp <- guojiazhuang_muzang[guojiazhuang_muzang$num == pottery_cluster_ana$num[i],]
  pottery_cluster_ana$burial_jade[i] <- temp$burial_jade
  pottery_cluster_ana$burial_dog[i] <- temp$burial_dog
  pottery_cluster_ana$waist_pit[i] <- temp$waist_pit
  pottery_cluster_ana$ritual_bronze[i] <- temp$ritual_bronze
  #pottery_cluster_ana$orientation_des[i] <- temp$orientation_des
}

z <- pottery_cluster_ana[,-c(1,2,3,27:30)]
m <- apply(z, 2, mean)
s <- apply(z, 2, sd)
z <- scale(z, m, s)
distance <- dist(z)
print(distance, digit = 3)
hc.c <- hclust(distance, method = "complete")
hc.a <- hclust(distance, method = "average")
hc.s <- hclust(distance, method = "single")
plot(hc.s, labels = paste(pottery_cluster_ana$pottery_type, sep = ""), cex = 0.5, xlab = "Distance")
#plot(hc.c, labels = paste(pottery_cluster_ana$burial_jade, pottery_cluster_ana$burial_dog, pottery_cluster_ana$waist_pit, pottery_cluster_ana$ritual_bronze, pottery_cluster_ana$num, sep = " "), cex = 0.5, xlab = "Distance")

member.c <- cutree(hc.c, 3)
member.a <- cutree(hc.a, 3)
table(member.c, member.a)


#k-means clustering
install.packages("factoextra")
library(factoextra)
fviz_nbclust(z, kmeans, method = "wss") + labs(subtitle = "Elbow method")

kc <- kmeans(z, 3)
print(kc)
pottery_cluster_ana$cluster <- kc$cluster
x = pottery_cluster_ana[pottery_cluster_ana$cluster == "1",]


#cluster analysis of various features
features <- guojiazhuang_muzang[c("num", "period", "orientation_des", "waist_pit", "burial_dog", "ritual_bronze")]
z <- na.omit(features[, -c(1,1)])
m <- apply(z, 2, mean)
s <- apply(z, 2, sd)
z <- scale(z, m, s)
distance <- dist(z)

#Period II
View(guojiazhuang_muzang %>% filter(num == "63"| num == "64"))
View(guojiazhuang_muzang %>% filter(num == "22" | num == "38"))
     
#Period III
View(guojiazhuang_muzang %>% filter(num == "83" | num == "88"| num == "89"| num == "94" | num == "96"))
View(guojiazhuang_muzang %>% filter(num == "263" | num == "265"| num == "269"| num == "270"))
View(guojiazhuang_muzang %>% filter(num == "273" | num == "276" | num == "277"| num == "278" | num == "279" ))
View(guojiazhuang_muzang %>% filter(num == "61" | num == "63"| num == "64"| num == "66"| num == "67"|  num == "70"))
View(guojiazhuang_muzang %>% filter(num == "99" | num == "98"| num == "137"| num == "138"))
View(guojiazhuang_muzang %>% filter(num == "225" | num == "226"| num == "228"| num == "230"| num == "231"| num == "232"))
View(guojiazhuang_muzang %>% filter(num == "27" | num == "35"| num == "57"| num == "55"))

#Period IVa
View(guojiazhuang_muzang %>% filter(num == "174" | num == "180"))
View(guojiazhuang_muzang %>% filter(num == "162" | num == "168"))
View(guojiazhuang_muzang %>% filter(num == "112" | num == "111"))
View(guojiazhuang_muzang %>% filter(num == "216" | num == "217" | num == "218"| num == "220" | num == "221" | num == "223" ))
View(guojiazhuang_muzang %>% filter(num == "6" | num == "7"))
View(guojiazhuang_muzang %>% filter(num == "87" | num == "93"))

#Period IVb
View(guojiazhuang_muzang %>% filter(num == "53" | num == "20"| num == "22"| num == "38"))
View(guojiazhuang_muzang %>% filter(num == "34" | num == "45"| num == "19"| num == "47"))
View(guojiazhuang_muzang %>% filter(num == "61" | num == "63"| num == "64"| num == "66"| num == "67"| num == "69"| num == "70"))
View(guojiazhuang_muzang %>% filter(num == "263" | num == "264"| num == "265"| num == "266"| num == "267"| num == "269"| num == "270"))
View(guojiazhuang_muzang %>% filter(num == "174" | num == "180"))
View(guojiazhuang_muzang %>% filter(num == "283" | num == "281"))
View(guojiazhuang_muzang %>% filter(num == "276" | num == "273" | num == "274" | num == "275"))
View(guojiazhuang_muzang %>% filter(num == "247" | num == "248"))


View(guojiazhuang_muzang %>% filter(num == "177" | num == "185"))
View(guojiazhuang_muzang %>% filter(num == "174" | num == "180"))
View(guojiazhuang_muzang %>% filter(num == "192" | num == "176"))
View(guojiazhuang_muzang %>% filter(num == "202" | num == "203"))

View(guojiazhuang_muzang %>% filter(num == "277" | num == "278" | num == "279" | num == "280"))

View(guojiazhuang_muzang %>% filter(num == "216" | num == "217" | num == "220" | num == "223" | num == "218" | num == "221"))


View(guojiazhuang_muzang %>% filter(num == "99" | num == "98"| num == "137"| num == "138"))
View(guojiazhuang_muzang %>% filter(num == "22" | num == "38" | num == "32" | num == "20" | num == "53"))
View(guojiazhuang_muzang %>% filter(num == "61" | num == "63"| num == "64"| num == "66"| num == "67"|  num == "70" | num == "69"))
View(guojiazhuang_muzang %>% filter(num == "225" | num == "226"| num == "228"| num == "230"| num == "231"| num == "232"))
View(guojiazhuang_muzang %>% filter(num == "83" | num == "88"| num == "89"| num == "94" | num == "96"))





#======visualizing new clusters======
install.packages("GGally")
library(GGally)
library(viridis)
#Scenario 1
cluster1 <- guojiazhuang_muzang %>% filter(num == "263" | num == "264"| num == "265"| num == "266"| num == "267"|  num == "269"| num == "270")
cluster1 <- guojiazhuang_muzang %>% filter(num == "22" | num == "38" | num == "32" | num == "53")
cluster1 <- guojiazhuang_muzang %>% filter(num == "61" | num == "63"| num == "64"| num == "66"| num == "67"|  num == "70" | num == "69")

#Scenario 2
cluster1 <- guojiazhuang_muzang %>% filter(num == "247" | num == "248" | num == "190" | num == "193" | num == "180" | num == "174" | num == "7" | num == "6" | num == "290" | num == "289")

#Scenario 3
cluster1 <- guojiazhuang_muzang %>% filter(num == "291" | num == "234" | num == "001" | num == "3" | num == "1" | num == "50" | num == "65" | num == "75")

#Scenario 1-2-3
cluster1 <- guojiazhuang_muzang %>% filter(num == "22" | num == "38" | num == "32" | num == "53"|
                                             num == "263" | num == "264"| num == "265"| num == "266"| num == "267"|  num == "269"| num == "270"|
                                             num == "61" | num == "63"| num == "64"| num == "66"| num == "67"|  num == "70" | num == "69"|
                                             num == "247" | num == "248" | num == "190" | num == "193" | num == "180" | num == "174" | num == "7" | num == "6" | num == "290" | num == "289" |
                                             num == "291" | num == "234" | num == "001" | num == "3" | num == "1" | num == "50" | num == "65" | num == "75")
cluster1$group[cluster1$num == "22" | cluster1$num == "38" | cluster1$num == "32" | cluster1$num == "53"] <- 1
cluster1$group[cluster1$num == "263" | cluster1$num == "264"| cluster1$num == "265"| cluster1$num == "266"| cluster1$num == "267"|  cluster1$num == "269"| cluster1$num == "270"] <- 2
cluster1$group[cluster1$num == "61" | cluster1$num == "63"| cluster1$num == "64"| cluster1$num == "66"| cluster1$num == "67"|  cluster1$num == "70" | cluster1$num == "69"] <- 3
cluster1$group[cluster1$num == "247" | cluster1$num == "248" | cluster1$num == "190" | cluster1$num == "193" | cluster1$num == "180" | cluster1$num == "174" | cluster1$num == "7" | cluster1$num == "6" | cluster1$num == "290" | cluster1$num == "289"] <- 4
cluster1$group[cluster1$num == "291" | cluster1$num == "234" | cluster1$num == "001" | cluster1$num == "3" | cluster1$num == "1" | cluster1$num == "50" | cluster1$num == "65" | cluster1$num == "75"] <- 5

cluster1_name_pottery <- names(sort(table(cluster1 %>% pottery_good_total()), decreasing = TRUE))
cluster1_name_bronze <- names(sort(table(cluster1 %>% bronze_good_total()), decreasing = TRUE))
cluster1 <- cluster1[,c("num", "period", "burial_good_bronze", "burial_good_pottery","burial_good_bone", "burial_good_cowery", "waist_pit", "burial_dog", "burial_jade", "hum_sacrifice", "orientation_des", "burial_style", "group")]
df <- data.frame(matrix(FALSE, nrow = nrow(cluster1), ncol = (length(cluster1_name_pottery) + length(cluster1_name_bronze))))
colnames(df) <- vessel_list <- c(cluster1_name_pottery,cluster1_name_bronze)
cluster1 <- cbind(df, cluster1)

for(i in 1:nrow(cluster1)) {
  temp <- cluster1[i,]
  c <- c()
  for(j in 1:length(vessel_list)){
      c <- c(c, grepl(vessel_list[j], paste(temp$burial_good_pottery,temp$burial_good_bronze, sep = ".")))
  }
  cluster1[i, c(1:length(vessel_list))] <- c
  }


vis1 <- cluster1 %>% arrange(match(period, c("二", "三", "四早", "四晚"))) %>% 
  transform(name = paste(num, period, sep = "."), 
            burial_bone = !is.na(burial_good_bone), 
            burial_cowry = !is.na(burial_good_cowery), 
            hum_sacrifice = (hum_sacrifice != 0), 
            orientation = orientation_des) %>%
  subset(select = -c(burial_good_pottery, burial_good_bronze, num, period, 
                     burial_good_bone, burial_good_cowery, orientation_des)) %>% 
  relocate(name) %>% 
  relocate(orientation, .after = last_col()) %>% 
  relocate(burial_style, .after = last_col())
#n <- ncol(vis1)
#vis1 <- cbind(vis1, ord = c(1:1)) #decides the order in the final visualization
vis1 <- cbind(vis1, ord = rowSums(vis1[,-c(1, 48, 52, 51)]))
vis1 <- melt(vis1, id.vars = c("name", "ord", "group")) 
vis1[is.na(vis1)] <- FALSE
vis1[vis1 == "俯"] = "Prone"
vis1[vis1 == "仰"] = "Supine"
vis1[vis1 == "屈"] = "Crouched"

cols <- c("FALSE" = "lightgrey", "TRUE" = "red", "NE" = "#F8766D", "SW" = "#00BFC4", "NW" = "#619CFF", "SE" = "#C77CFF", "Supine" = "#00BA38", "Prone" = "#C49A00",  "Crouched" = "#7CAE00")

ggplot(vis1, mapping = aes(x = variable, y = reorder(name, ord), fill = value)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  geom_text(vis1[vis1$variable == "burial_style" & vis1$value != "FALSE",], mapping = aes(label = value), size = 2)+
  geom_text(vis1[vis1$variable == "orientation" & vis1$value != "FALSE",], mapping = aes(label = value), size = 2)+
  scale_fill_manual(values = cols, breaks = c("FALSE", "TRUE")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.title = element_blank()) +
  coord_fixed() +
  xlab("Features") + ylab("Tombs")

# Scenario 1
ggplot(vis1, mapping = aes(x = variable, y = reorder(name, ord), fill = value)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  geom_text(vis1[vis1$variable == "burial_style" & vis1$value != "FALSE",], mapping = aes(label = value), size = 2)+
  geom_text(vis1[vis1$variable == "orientation" & vis1$value != "FALSE",], mapping = aes(label = value), size = 2)+
  scale_fill_manual(values = cols, breaks = c("FALSE", "TRUE")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.title = element_blank()) +
  coord_fixed() +
  xlab("Features") + ylab("Tombs")

# Scenario 3
# ggplot(vis1, mapping = aes(x = variable, y = reorder(name, ord), fill = value)) +
#   geom_tile(aes(width= 0.7, height= 0.7), linetype = 1, size = 1) +
#   geom_text(vis1[vis1$variable == "burial_style" & vis1$value != "FALSE",], mapping = aes(label = value), size = 2)+
#   geom_text(vis1[vis1$variable == "orientation" & vis1$value != "FALSE",], mapping = aes(label = value), size = 2)+
#   scale_fill_manual(values = cols, breaks = c("FALSE", "TRUE")) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), legend.title = element_blank()) +
#   coord_fixed() +
#   xlab("Features") + ylab("Tombs")

# Scenario 2
# ggplot(vis1, mapping = aes(x = variable, y = reorder(name, ord), fill = value)) +
#   geom_tile(aes(width= 0.7, height= 0.7), linetype = 1, size = 1) +
#   geom_hline(yintercept = c(2.5, 4.5, 6.5, 8.5), cex = 1.5,
#               colour='grey') +
#   geom_text(vis1[vis1$variable == "burial_style" & vis1$value != "FALSE",], mapping = aes(label = value), size = 3)+
#   geom_text(vis1[vis1$variable == "orientation" & vis1$value != "FALSE",], mapping = aes(label = value), size = 3)+
#   scale_fill_manual(values = cols, breaks = c("FALSE", "TRUE")) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust=1), legend.title = element_blank()) +
#   coord_fixed() + 
#   xlab("Features") + ylab("Tombs")

