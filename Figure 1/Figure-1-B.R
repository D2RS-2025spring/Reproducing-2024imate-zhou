#Figure-1-B
library(ggplot2)  
library(ggsignif)  
library(ggdist)
library(readxl)

data <- read_excel("E:/Desktop/309/研一/作业/DRS2025/Figure1/Origin Data/Figure-B.xlsx",1)
names(data)  

Vec1 <- c("aCO2", "eCO2")  
comb_list <- list(c("aCO2", "eCO2"))  
Custom.color <- c("#d3838a", "#4a9a5b")  

P1 <- ggplot(data, aes(x = group, y = AP, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("Available P") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P1

P2 <- ggplot(data, aes(x = group, y = CAPI, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("Ca-Pi") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P2


P3 <- ggplot(data, aes(x = group, y = NaOHPO, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("NaOH-Po") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P3


P4 <- ggplot(data, aes(x = group, y = MBP, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) + 
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) +  
  scale_fill_manual(values = Custom.color) + 
  scale_color_manual(values = Custom.color) +  
  ylab("MBP") +   
  theme(  
    axis.ticks.x = element_line(size = 0, color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),  
    panel.grid.minor.y = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
    legend.position = "none",  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 13,color = "black"), 
    axis.text.x = element_text(size = 10, hjust = 0.2,color = "black"),  
    axis.text.y = element_text(size = 10,color = "black"),   
    plot.title = element_text(hjust = 0.5)  
  ) +  
  geom_signif(comparisons = comb_list, step_increase = .1, map_signif_level = TRUE, vjust = 0.5, hjust = 0)

P4


library(gridExtra)
P5 <- grid.arrange(P1, P2, P3, P4, ncol = 2)
print(P5)
ggsave("Figure-1-B.pdf", dpi = 1000, height = 150, width = 225, units = "mm")