##Figure4C-E
library(ggplot2)
library(ggpmisc)
library(readxl)
library(dplyr)
library(ggpubr)

fit<- read_excel("D:/HuaweiMoveData/Users/yzy/Desktop/Figure 4/Origin Data/regression.xlsx",1)

head(fit)
names(fit)

##Figure4C##
p1<-ggplot(fit, aes(x=TOAA, y=Networkcomplexity),group=Group)+
  
  theme_bw()+theme(legend.position="top")+theme_bw() + theme(panel.grid =element_blank())+
  geom_point(aes(color=Group),size=4.355)+
  geom_smooth(method = 'lm',color="black",formula= y ~ x,se= T)+
  
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme(axis.title= element_text(size=14))+
  ylab("Networkcom plexity")+xlab("Total organic acid anions (μmol g-1 RDW)")+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))
p1


##Figure4D##

library(ggplot2)
library(ggpmisc)

p2 <- ggplot(fit, aes(x = `NaOH-Po`, y = Networkcomplexity, color = Group)) +
  geom_point(size = 4.355) +
  geom_smooth(method = 'lm', formula = y ~ x, se = TRUE, color = "black") +
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "*\", \"*")),
    formula = y ~ x,
    parse = TRUE,
    label.y = "bottom",
    label.x = "right"
  ) +
  labs(x = "Available P (mg kg)", y = "Network complexity") +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )
p2


##Figure6F##
plant_growth <- read.delim("D:/HuaweiMoveData/Users/yzy/Desktop/Figure 4/Origin Data/plant growth.txt", row.names = 1)
otu <- read.delim("D:/HuaweiMoveData/Users/yzy/Desktop/Figure 4/Origin Data/otu.txt", row.names = 1)
library(randomForest)
set.seed(123)
otu_forest <- randomForest(plant_age~., data = otu, importance = TRUE, ntree = 500, nPerm = 1000)
otu_forest

importance_otu.scale <- data.frame(importance(otu_forest, scale = TRUE), check.names = FALSE)
importance_otu.scale

importance_otu.scale <- importance_otu.scale[order(importance_otu.scale$'%IncMSE', decreasing = TRUE), ]
importance_otu.scale

library(ggplot2)
importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)

p <- ggplot(importance_otu.scale, aes(OTU_name, `%IncMSE`)) +
  geom_col(width = 0.5, fill = '#FFC068', color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 16))

p <- p +
  annotate('text', label = 'Total plant P accumulation', x = 9, y = 15, size = 4) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 45.6), x = 9, y = 13, size = 3, parse = TRUE)
p
