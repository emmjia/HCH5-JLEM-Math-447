# loading libraries
library(dplyr)

# creating table
warp <- as.data.frame(x = matrix(data = c(rep(x = c("Block 1", "Block 2"),
                                              each = 16),
                                          rep(x = c(50, 75, 100, 125),
                                              each = 4,
                                              times = 2),
                                          rep(x = c(40, 60, 80, 100),
                                              times = 8),
                                          c(17, 16, 24, 28,
                                            12, 18, 17, 27,
                                            16, 18, 25, 30,
                                            21, 23, 23, 29,
                                            20, 21, 22, 27,
                                            9, 13, 12, 31,
                                            12, 21, 23, 23,
                                            17, 21, 22, 31)),
                                 nrow = 32,
                                 byrow = FALSE))
names(warp) <- c("Blocks", "Temp", "Copper", "Warp")
attach(warp)

# running ANOVA
warp.aov <- aov(formula = as.numeric(Warp) ~ factor(Blocks) + factor(Temp)*factor(Copper),
                data = warp)
summary(warp.aov)

# test space
select(obj = )

# sum of squares components
n = 2; a = 4; b = 4
part2 <- ((17 + 16 + 24 + 28 + 12 + 18 + 17 + 27 + 16 + 18 + 25 + 30 + 21 + 23 + 23 + 29 + 20 + 21 + 22 + 27 + 9 + 13 + 12 + 31 + 12 + 21 + 23 + 23 + 17 + 21 + 22 + 31)**2) / (32)
total <- ((17)**2 + (16)**2 + (24)**2 + (28)**2 + (12)**2 + (18)**2 + (17)**2 + (27)**2 + (16)**2 + (18)**2 + (25)**2 + (30)**2 + (21)**2 + (23)**2 + (23)**2 + (29)**2 + (20)**2 + (21)**2 + (22)**2 + (27)**2 + (9)**2 + (13)**2 + (12)**2 + (31)**2 + (12)**2 + (21)**2 + (23)**2 + (23)**2 + (17)**2 + (21)**2 + (22)**2 + (31)**2)
blocks <- ((17 + 16 + 24 + 28 + 12 + 18 + 17 + 27 + 16 + 18 + 25 + 30 + 21 + 23 + 23 + 29)**2 + (20 + 21 + 22 + 27 + 9 + 13 + 12 + 31 + 12 + 21 + 23 + 23 + 17 + 21 + 22 + 31)**2) / (16)
A <- ((17 + 16 + 24 + 28 + 20 + 21 + 22 + 27)**2 + (12 + 18 + 17 + 27 + 9 + 13 + 12 + 31)**2 + (16 + 18 + 25 + 30 + 12 + 21 + 23 + 23)**2 + (21 + 23 + 23 + 29 + 17 + 21 + 22 + 31)**2) / (8)
B <- ((17 + 20 + 12 + 9 + 16 + 12 + 21 + 17)**2 + (16 + 21 + 18 + 13 + 18 + 21 + 23 + 21)**2 + (24 + 22 + 17 + 12 + 25 + 23 + 23 + 22)**2 + (28 + 27 + 27 + 31 + 30 + 23 + 29 + 31)**2) / (8)
AB <- ((17 + 20)**2 + (16 + 21)**2 + (24 + 22)**2 + (28 + 27)**2 + (12 + 9)**2 + (18 + 13)**2 + (17 + 12)**2 + (27 + 31)**2 + (16 + 12)**2 + (18 + 21)**2 + (25 + 23)**2 + (30 + 23)**2 + (21 + 17)**2 + (23 + 21)**2 + (23 + 22)**2 + (29 + 31)**2) / (2)

# sum of squares
SS_total <- total - part2
SS_block <- blocks - part2
SS_A <- A - part2
SS_B <- B - part2
SS_AB <- AB - part2 - SS_A - SS_B
SS_e <- SS_total - SS_block - SS_A - SS_B - SS_AB

# mean sum of squares
MS_total <- SS_total / 31
MS_block <- SS_block / 1
MS_A <- SS_A / 3
MS_B <- SS_B / 3
MS_AB <- SS_AB / 9
MS_e <- SS_e / 15

# F test
F_A <- MS_A / MS_e
F_B <- MS_B / MS_e
F_AB <- MS_AB / MS_e

F_A0 <- 14.25
F_B0 <- 14.25
F_AB0 <- 3.77

F_A > F_A0
F_B > F_B0
F_AB > F_AB0