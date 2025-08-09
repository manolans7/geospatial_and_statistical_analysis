rm(list = ls(all = TRUE))
# Load required library
#("coin")
library(coin)
library(readr)
library(tidyverse)
#StatsCSV <- read_csv("E:/5_STATS/StatsCSV.csv")
StatsCSV <- read_csv("~/Library/CloudStorage/Box-Box/Bat:tree connectivity proj/stats/RevisedStats.csv")

filtered<- (StatsCSV %>%
              select(Site, Time))
view(filtered)


data_split <- split(filtered$Time, filtered$Site)
print(data_split)

s1<- data_split$S1
s2<- data_split$S2
s3<- data_split$S3
s4<- data_split$S4
s5<- data_split$S5
s6<- data_split$S6
s7<- data_split$S7
s8<- data_split$S8
s9<- data_split$S9
s10<- data_split$S10
s11<- data_split$S11
s12<- data_split$S12
s13<- data_split$S3
s14<- data_split$S14
s15<- data_split$S15
s16<- data_split$S16
s17<- data_split$S17
s18<- data_split$S18
s19<- data_split$S19
s20<- data_split$S20
s21<- data_split$S21
s22<- data_split$S22
s23<- data_split$S23
s24<- data_split$S24
s25<- data_split$S25
s26<- data_split$S26
s27<- data_split$S27
s28<- data_split$S28
s29<- data_split$S29
s30<- data_split$S30



mad10noGap <- c(s2, s3, s7, s8, s9, s11, s12, s16,s17,s18,s19,s21,s24,s26,s27,s28)
mad10Gap <- c(s20,s14,s1,s13,s25,s4,s23,s5,s6,s10,s15,s22,s29,s30)
print(mad10noGap)

MAD10 <- wilcox.test(mad10noGap, mad10Gap)
print(MAD10)

# SOG10
sog10noGap <- c(s2, s3, s7, s8, s9, s11, s12, s16,s17,s18,s19,s21,s24,s26,s27,s28)
sog10Gap <- c(s20,s14,s1,s13,s25,s4,s23,s5,s6,s10,s15,s22,s29,s30)

SOG10 <- wilcox.test(sog10noGap, sog10Gap)
print(SOG10)

# PCC10
pcc10no_trees <- c(s5,s6,s10,s15,s22,s23,s29,s30) 
pcc10group2 <- c(s8,s9,s4,s1,s25,s12,s13,s27)
pcc10group3<- c(s26,s2,s14,s24,s20,s21,s16,s19,s7,s3,s28,s17,s18,s11)
# pcc10group2 <- c(s8,s9,s4,s1,s25,s12,s13,s27,s26,s2,s14,s24,s20,s21,s16)
# pcc10group3 <- c(s19,s7,s3,s28,s17,s18,s11)

# Combine the data into a single vector
all_data <- c(pcc10no_trees, pcc10group2, pcc10group3)

# Create a grouping factor with appropriate levels
pcc10group <- factor(rep(1:3, times = c(length(pcc10no_trees), length(pcc10group2), length(pcc10group3))))

# Perform Kruskal-Wallis test
pcc10kruskal_test_result <- kruskal_test(all_data ~ pcc10group)

# Print the test result
print(pcc10kruskal_test_result)

#wilcox
#no trees v t1
PCC10w1 <- wilcox.test(pcc10no_trees, pcc10group2)
print(PCC10w1)

# PCC10NTT1 no trees vs trees 1

PCC10w2 <- wilcox.test(pcc10no_trees, pcc10group3)
print(PCC10w2)

# PCC10NTT2 no trees vs trees 2

pcc10w3 <- wilcox.test(pcc10group2, pcc10group3)
print(pcc10w3)

pcc10w4<- wilcox.test(c(pcc10group2,pcc10group3),pcc10no_trees)
print(pcc10w4)

pcc10w5 <- wilcox.test(c(pcc10group3,pcc10no_trees),pcc10group2)
print(pcc10w5)


# RUG10
rug10no_trees <- c(s5,s6,s10,s15,s22,s23,s29,s30) 
rug10group2 <- c(s2,s28,s18,s27, s9, s17, s12, s7, s26, s4, s16)
rug10group3 <- c(s14,s1,s24,s21,s8,s13,s25,s19,s20,s3,s11)

# Combine the data into a single vector
rug10all_data <- c(rug10no_trees, rug10group2, rug10group3)

# Create a grouping factor with appropriate levels
rug10group <- factor(rep(1:3, times = c(length(rug10no_trees), length(rug10group2), length(rug10group3))))

# Perform Kruskal-Wallis test
rug10kruskal_test_result <- kruskal_test(rug10all_data ~ rug10group)

# Print the test result
print(rug10kruskal_test_result)

#wilcox
#no trees v t1
rug10w1 <- wilcox.test(rug10no_trees, rug10group2)
print(rug10w1)

# no trees vs trees 1

rug10w2 <- wilcox.test(rug10no_trees, rug10group3)
print(rug10w2)

#  no trees vs trees 2

rug10w3 <- wilcox.test(rug10group2, rug10group3)
print(rug10w3)

rug10w4 <- wilcox.test(rug10group2, c(rug10group3, rug10no_trees))
print(rug10w4)

rug10w4 <- wilcox.test(rug10no_trees, c(rug10group3, rug10group2))
print(rug10w4)


# MTH10
no_trees <- c(s5,s6,s10,s15,s22,s23,s29,s30) 
group2 <- c(s10,s4,s1,s17,s21,s24,s25,s19,s2,s12,s14,s7,s8,s9,s27)
group3 <- c(s18,s13,s26,s3,s20,s11,s28, s16)

# Combine the data into a single vector
all_data <- c(no_trees, group2, group3)

# Create a grouping factor with appropriate levels
group <- factor(rep(1:3, times = c(length(no_trees), length(group2), length(group3))))

# Perform Kruskal-Wallis test
mth10kruskal_test_result <- kruskal_test(all_data ~ group)

# Print the test result
print(mth10kruskal_test_result)

mth10w1 <- wilcox.test(no_trees, group2)
print(mth10w1)

# no trees vs trees 1

mth10w2 <- wilcox.test(no_trees, group3)
print(mth10w2)

#  no trees vs trees 2

mth10w3 <- wilcox.test(group2,group3)
print(mth10w3)

mth10w4 <- wilcox.test(no_trees, c(group2, group3))
print(mth10w4)

mth10w5 <- wilcox.test(group3, c(group2, no_trees))
print(mth10w5)
-------
  
  
  #30
  
  mad30no_gap <- c(s2,s3,s8,s9,s12,s16,s17,s18,s19,s24,s27,s28,s21)
mad30gap <- c(s7,s20,s14,s26,s13,s11,s25,s1,s4)
mad30allgap <- c(s5,s6,s10,s15,s22,s23,s29,s30)

# Combine the data into a single vector
mad30all_data <- c(mad30no_gap, mad30gap, mad30allgap)

# Create a grouping factor with appropriate levels
mad30group <- factor(rep(1:3, times = c(length(mad30no_gap), length(mad30gap), length(mad30allgap))))

# Perform Kruskal-Wallis test
mad30kruskal_test_result <- kruskal_test(mad30all_data ~ mad30group)

# Print the test result
print(mad30kruskal_test_result)  

mad30W1 <- wilcox.test(mad30no_gap, mad30gap)
print(mad30W1)

MAD30W2 <- wilcox.test(mad30no_gap, mad30allgap)
print(MAD30W2)

MAD30W3 <- wilcox.test(mad30gap, mad30allgap)
print(MAD30W3)

MAD30W4 <- wilcox.test(c(mad30gap, mad30allgap),mad30no_gap)
print(MAD30W4)

SOG30 <- wilcox.test(mad30gap,mad30no_gap)
print(SOG30)

# PCC30
pcc30no_trees <- c(s5,s6,s10,s15,s22,s23,s29,s30) 
pcc30group2 <- c(s9,s4,s25,s1,s8,s14,s12,s27,s13,s21,s16)
pcc30group3 <- c(s26,s11,s20,s24,s7,s2,s17,s19,s3,s28,s18)

# Combine the data into a single vector
pcc30all_data <- c(pcc30no_trees, pcc30group2, pcc30group3)

# Create a grouping factor with appropriate levels
pcc30group <- factor(rep(1:3, times = c(length(pcc30no_trees), length(pcc30group2), length(pcc30group3))))

# Perform Kruskal-Wallis test
pcc30kruskal_test_result <- kruskal_test(pcc30all_data ~ pcc30group)

# Print the test result
print(pcc30kruskal_test_result)

# PCC30t only sites with trees

#no trees v t1
PCC30w1 <- wilcox.test(pcc30no_trees, pcc30group2)
print(PCC30w1)

# no trees v t2
PCC30w2 <- wilcox.test(pcc30no_trees, pcc30group3)
print(PCC30w2)

#  trees1 vs trees 2
PCC30w3 <- wilcox.test(pcc10group2, pcc30group3)
print(PCC30w3)

PCC30w4 <- wilcox.test(c(pcc10group2, pcc30group3),pcc30no_trees)
print(PCC30w4)
PCC30w5 <- wilcox.test(c(pcc30no_trees, pcc30group3),pcc10group2)
print(PCC30w5)


# RUG30
rug30no_trees <- c(s5,s6,s10,s15,s22,s23,s29,s30)
rug30group2 <- c(s2,s7,s9,s24,s17,s21,s18,s26,s14,s13)
rug30group3 <- c(s8, s4,s20,s19,s1,s12,s25,s11,s3,s27,s16,s28)
# Combine the data into a single vector
all_data <- c(rug30no_trees, rug30group2, rug30group3)

# Create a grouping factor with appropriate levels
rug30group <- factor(rep(1:3, times = c(length(rug30no_trees), length(rug30group2), length(rug30group3))))

# Perform Kruskal-Wallis test
rug30kruskal_test_result <- kruskal_test(all_data ~ rug30group)

# Print the test result
print(rug30kruskal_test_result)


# rug30t only sites with trees
rug30w1 <- wilcox.test(rug30group2, rug30group3)
print(rug30w1)
#  no trees vs trees 1
rug30w2 <- wilcox.test(rug30no_trees, rug30group2)
print(rug30w2)
#  no trees vs trees 2
rug30w3 <- wilcox.test(rug30no_trees, rug30group3)
print(rug30w3)

rug30w4 <- wilcox.test(c(rug30no_trees,rug30group2), rug30group3)
print(rug30w4)

# MTH30
mth30no_trees <- c(s5,s6,s15,s22,s23,s29,s30)
mth30group2 <- c(s10,s4,s1,s17,s25,s14,s19,s7,s24,s9,s2)
mth30group3 <- c(s27,s21,s12,s26,s8,s3,s18,s20,s13,s28,s11,s16)

# Combine the data into a single vector
all_data <- c(mth30no_trees, mth30group2, mth30group3)

# Create a grouping factor with appropriate levels
mth30group <- factor(rep(1:3, times = c(length(mth30no_trees), length(mth30group2), length(mth30group3))))

# Perform Kruskal-Wallis test
mth30kruskal_test_result <- kruskal_test(all_data ~ mth30group)

# Print the test result
print(mth30kruskal_test_result)


#  only sites with trees

mth30w1 <- wilcox.test(mth30group2, mth30group3)
print(mth30w1)
#no v t1
mth30w2 <- wilcox.test(mth30no_trees, mth30group2)
print(mth30w2)
#no v t2
mth30w3 <- wilcox.test(mth30no_trees, mth30group3)
print(mth30w3)

mth30w4 <- wilcox.test(mth30no_trees, c(mth30group2,mth30group3))
print(mth30w4)


------
  #50
  
  # MAD50
  mad50noGap <- c(s2,s3,s8,s9,s12,s16,s17,s18,s19,s21,s24,s28)
mad50gap <- c(s7,s14,s27,s13,s11,s20,s1,s25,s26,s4,s15)
mad50allgap<- c(s5,s6,s10,s22,s23,s29,s30)

# Combine the data into a single vector
mad50all_data <- c(mad50noGap, mad50gap, mad50allgap)

# Create a grouping factor with appropriate levels
mad50group <- factor(rep(1:3, times = c(length(mad50noGap), length(mad50gap), length(mad50allgap))))

# Perform Kruskal-Wallis test
mad50kruskal_test_result <- kruskal_test(mad50all_data ~ mad50group)

# Print the test result
print(mad50kruskal_test_result)

# no gap v gap
MAD50w1 <- wilcox.test(mad50noGap, mad50gap)
print(MAD50w1)
# no gap v all gap
MAD50w2 <- wilcox.test(mad50noGap, mad50allgap)
print(MAD50w2)
# allgap v gap
MAD50w3 <- wilcox.test(mad50allgap, mad50gap)
print(MAD50w3)
#allgap+gap v no gap
MAD50w4 <- wilcox.test(c(mad50allgap, mad50gap),mad50noGap)
print(MAD50w4)

SOG50 <- wilcox.test(c(mad50allgap, mad50gap),mad50noGap)
print(SOG50)



# PCC50
pcc50no_trees <- c(s5,s6,s10,s22,s23,s29,s30)
pcc50group2 <- c(s15,s8,s9,s25,s14,s4,s26,s1,s20,s13,s21,s11)
pcc50group3 <-  c(s27,s17,s12,s24,s16)
pcc50group4 <- c(s19,s3,s7,s28,s18,s2)

# Combine the data into a single vector
pcc50all_data <- c(pcc50no_trees, pcc50group2, pcc50group3, pcc50group4)

# Create a grouping factor with appropriate levels
pcc50group <- factor(rep(1:4, times = c(length(pcc50no_trees), length(pcc50group2), length(pcc50group3), length(pcc50group4))))

# Perform Kruskal-Wallis test
pcc50kruskal_test_result <- kruskal_test(pcc50all_data ~ pcc50group)

# Print the test result
print(pcc50kruskal_test_result)

#  trees 1 vs 2

PCC50w1 <- wilcox.test(pcc50group2, pcc50group3)
print(PCC50w1)
# trees1 vs no trees

PCC50w2 <- wilcox.test(pcc50group2, pcc50no_trees)
print(PCC50w2)
# trees2 vs no trees

PCC50w3 <- wilcox.test(pcc50group3, pcc50no_trees)
print(PCC50w3)

PCC50w4 <- wilcox.test(pcc50group2, pcc50group4)
print(PCC50w4)
# trees1 vs no trees

PCC50w5 <- wilcox.test(pcc50group4, pcc50no_trees)
print(PCC50w5)
# trees2 vs no trees

PCC50w6 <- wilcox.test(pcc50group3, pcc50group4)
print(PCC50w6)

PCC50w7 <- wilcox.test(c(pcc50group3,pcc50group2,pcc50group4), pcc50no_trees)
print(PCC50w7)

PCC50w8 <- wilcox.test(c(pcc50no_trees,pcc50group4,pcc50group2),pcc50group3 )
print(PCC50w8)


# RUG50
rug50no_trees <- c(s5,s6,s10,s22,s23,s29,s30) 
rug50group2 <- c(s15,s2,s7,s24,s9,s21,s18,s13,s14,s17,s8,s26,s19,s11)
rug50group3 <- c( s4,s12,s25,s3,s1,s16,s20,s27,s28 )

# Combine the data into a single vector
rug50all_data <- c(rug50no_trees, rug50group2, rug50group3)

# Create a grouping factor with appropriate levels
rug50group <- factor(rep(1:3, times = c(length(rug50no_trees), length(rug50group2), length(rug50group3))))

# Perform Kruskal-Wallis test
rug50kruskal_test_result <- kruskal_test(rug50all_data ~ rug50group)

# Print the test result
print(rug50kruskal_test_result)

# RUG50t no trees vs t1

rug50w1 <- wilcox.test(rug50no_trees, rug50group2)
print(rug50w1)
# RUG50t no trees vs t2

rug50w2 <- wilcox.test(rug50no_trees, rug50group3)
print(rug50w2)
# RUG50t t2 vs t1
rug50w3 <- wilcox.test(rug50group2, rug50group3)
print(rug50w3)


# MTH50
mth50no_trees <- c(s5,s6,s22,s23,s29,s30) 
mth50group2 <- c(s10,s1,s17,s15,s25,s14,s2)
mth50group3 <- c(s21,s27,s12,s7,s19,s26,s8,s3,s18,s24,s20,s9,s13,s28,s4,s11,s16)

# Combine the data into a single vector
mth50all_data <- c(mth50no_trees, mth50group2, mth50group3)

# Create a grouping factor with appropriate levels
mth50group <- factor(rep(1:3, times = c(length(mth50no_trees), length(mth50group2), length(mth50group3))))

# Perform Kruskal-Wallis test
mth50kruskal_test_result <- kruskal_test(mth50all_data ~ mth50group)

# Print the test result
print(mth50kruskal_test_result)

# Mann-Whitney
mth50w1 <- wilcox.test(mth50no_trees, mth50group2)
print(mth50w1)

mth50w2 <- wilcox.test(mth50no_trees, mth50group3)
print(mth50w2)

mth50w3 <- wilcox.test(mth50group2, mth50group3)
print(mth50w3)

mth50w4 <- wilcox.test(c(mth50no_trees,mth50group2),mth50group3)
print(mth50w4)  

