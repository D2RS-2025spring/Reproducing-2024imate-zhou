# Figure-1-C
library(readxl)
library(Hmisc)
library(circlize)

plot_data <- read_excel("E:/Desktop/309/研一/作业/DRS2025/Figure1/Origin Data/Figure-C.xlsx", 1)

cor1 <- cor(plot_data)
rt <- as.matrix(plot_data)
df <- rcorr(rt)

write.csv(df$r, "correlation_matrix.csv")
write.csv(df$P, "p_values_matrix.csv")

cor1[cor1 == 1] <- 0

pos_col <- "lightblue"
neg_col <- "lightpink"
col1 <- matrix(ifelse(cor1 >= 0, pos_col, neg_col), nrow = nrow(cor1), ncol = ncol(cor1))

pdf("Figure-1-C.pdf", width = 8, height = 8)  # 可根据需要调整尺寸
par(mar = c(2, 2, 2, 4))
circos.par(gap.degree = rep(2, ncol(cor1)),
           start.degree = 180,
           track.margin = c(0.02, 0.02))
chordDiagram(cor1, grid.col = NULL, col = col1, transparency = 0.5, symmetric = TRUE)
dev.off()

