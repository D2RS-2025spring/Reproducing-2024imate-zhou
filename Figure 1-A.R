#Figure-A
library(ggcharts)
library(readxl)
plot_data <- read_excel("E:/Desktop/309/研一/作业/DRS2025/Figure1/Origin Data/Figure-A.xlsx",1)
names(plot_data)
ggplot(plot_data,aes(Microbes,Factor))+
  geom_point(size=3,aes(color=Group))+
  geom_errorbar(aes(xmin = Microbes - Error_bar, xmax = Microbes +Error_bar,color=Group),width = 0.25,cex=0.7)+
  labs(y="Traits", x="Responsivity (%)")+
  geom_vline(aes(xintercept =0), size=0.6, linetype="dashed", colour="gray2")+
  geom_hline(aes(yintercept =8.5), size=0.4, linetype="dashed", colour="gray52")+
  geom_hline(aes(yintercept =11.5), size=0.4, linetype="dashed", colour="gray52")+
  scale_x_continuous(expand = c(0, 0), limit = c(-100, 100))+
  scale_color_manual(values=c("#71959F","#3FBDA7","#F1606C","#3E516F"))+ 
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=10))
ggsave("Figure-A.pdf", dpi=1000, height =150,width=150,units="mm")


