#Figure2-A
library(ggplot2) 
library(readxl)
fit<- read_excel("C:/Users/ASUS/Desktop/Figure 2/Origin Data/Figure2-1.xlsx",1)
names(fit)
p1 <- ggplot(fit, aes(x=Treatment, y=AMF, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("AMF (nmol NLFA g-1 soil)") +  
  theme_bw() +   
  theme(panel.grid = element_blank(),  
        legend.position="none",  
        strip.text = element_text(size = rel(0.90)),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size=10),  
        axis.text.x = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.text.y = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.line.x.bottom = element_line(color = 'black'),  
        axis.line.y.left = element_line(color = 'black'),  
        axis.line.y.right = element_line(color = 'black'),  
        axis.text.y.right = element_blank(),  
        axis.ticks.y.right = element_blank(),  
        panel.border = element_blank())  

p1
ggsave("F2-A.pdf", dpi=1000, height =45,width=45,units="mm")

#Figure-C
library(ggplot2)  
library(ggsignif)  
library(ggdist)
library(readxl)

data <- read_excel("C:/Users/ASUS/Desktop/Figure 2/Origin Data/Figure2-4.xlsx",1)
names(data)  
Vec1 <- c("aCO2", "eCO2")  
comb_list <- list(c("aCO2", "eCO2"))  
Custom.color <- c("#d3838a", "#4a9a5b")  

P1 <- ggplot(data, aes(x = group, y = AMF, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) +  
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) + 
  scale_fill_manual(values = Custom.color) +  
  scale_color_manual(values = Custom.color) + 
  ylab("Shannon index") +   
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
ggsave("F2-C.pdf", dpi=1000, height =75,width=75,units="mm")

P2 <- ggplot(data, aes(x = group, y = ALPB, fill = group)) +  
  geom_jitter(mapping = aes(color = group), width = .05, alpha = 0.5, size = 2) +  
  geom_boxplot(position = position_nudge(x = 0.14), width = 0.1, outlier.size = 0, outlier.alpha = 0) +  
  stat_halfeye(mapping = aes(fill = group), width = 0.2, .width = 0, justification = -1.2, point_colour = NA, alpha = 0.6) + 
  scale_fill_manual(values = Custom.color) +  
  scale_color_manual(values = Custom.color) + 
  ylab("Shannon index") +   
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
ggsave("F2-G.pdf", dpi=1000, height =75,width=75,units="mm")

#Figure2-E
p2 <- ggplot(fit, aes(x=Treatment, y=ALPB, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("ALP-producing bacteria (*105 copies g-1 soil)") +  
  theme_bw() +   
  theme(panel.grid = element_blank(),  
        legend.position="none",  
        strip.text = element_text(size = rel(0.90)),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size=10),  
        axis.text.x = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.text.y = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.line.x.bottom = element_line(color = 'black'),  
        axis.line.y.left = element_line(color = 'black'),  
        axis.line.y.right = element_line(color = 'black'),  
        axis.text.y.right = element_blank(),  
        axis.ticks.y.right = element_blank(),  
        panel.border = element_blank())  

p2
ggsave("F2-E.pdf", dpi=1000, height =45,width=45,units="mm")

#Figure-B
library(readxl)
library(ggplot2)
library(grid)
library(ggpubr)
library(gridExtra)
library(tidyr)
library(RColorBrewer)
df <- read_excel("C:/Users/ASUS/Desktop/Figure 2/Origin Data/Figure2-3.xlsx",1)
names(df)

df$Families<- factor(df$Families,levels=c("Ambisporaceae","Glomeraceae", "Paraglomeraceae","Archaeosporaceae"
),

labels = c("Ambisporaceae","Glomeraceae", "Paraglomeraceae","Archaeosporaceae"))


levels(df$Families)

p1<- ggplot(data=df, aes(x=AMF_Treatment, y=AMF,fill=Families))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("F2-B.pdf", dpi=1000, height =75,width=100,units="mm")

df <- read_excel("C:/Users/ASUS/Desktop/Figure 2/Origin Data/Figure2-2.xlsx",1)
names(df)

df$Families<- factor(df$Families,levels=c("Others","Unclassified", "Phyllobacteriaceae","Pseudomonadaceae"
                                          ,"Xanthomonadaceae","Rhizobiaceae","Bradyrhizobiaceae"
),

labels = c("Others","Unclassified", "Phyllobacteriaceae","Pseudomonadaceae"
           ,"Xanthomonadaceae" ,"Rhizobiaceae","Bradyrhizobiaceae"))


levels(df$Families)

p1<- ggplot(data=df, aes(x=PhoD_Treatment, y=PhoD,fill=Families))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD",
                                    "#D56128",
                                    "#9F9F98","#000000"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("F2-F.pdf", dpi=1000, height =75,width=100,units="mm")
