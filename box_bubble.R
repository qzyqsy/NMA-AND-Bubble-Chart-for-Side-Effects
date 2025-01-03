install.packages("reshape2")
library(reshape2)
library(ggplot2)

data <- read.table("box_bubble_0427.csv", sep = ",", check.names=F,header = T, fileEncoding = 'GBK')
head(data)

#转成长格式数据
data_melt<-melt(data,id="id")
names(data_melt)=c("id","sample","value")#重命名数据列名
head(data_melt)
#颜色设置
#col <- c("#EE8C85","#E6C458","#859FD2","#48B78D","#61B2C5")
col <- c("#E41A1C","#377EB8","#FF7F00","#984EA3","#4DAF4A")

##1.气泡图
p1 <- 
  ggplot(data_melt,aes(x=sample,y=id,color=sample,size=value))+
  #ggplot(data_melt,aes(x=sample,y=id,color=sample,size=ifelse(value==0, NA, value)))+ ##可不显示0值
  geom_point(alpha=0.9)+#alpha透明度
  scale_color_manual(values = col)+
  #scale_fill_manual(values = col)+ 
  theme(panel.background = element_blank(),
        panel.grid = element_line("gray"),
        panel.border = element_rect(colour = "black",fill=NA))+
  theme(axis.text.x = element_blank(),
        axis.text.y=element_text(colour="black", size=12),
        axis.title.y = element_text(vjust=0.2, size = 12,colour="black"))+
  theme(legend.text=element_text(size=10),legend.title = element_blank())+#图例
  labs(x=" ",y="  ",fill="")+
  labs(title = "hemopoietic system TEAE")+
  theme(plot.title = element_text(hjust = 0.5))

p1
##2.箱线图
p2 <- ggplot(data = data_melt,aes(x=sample,y=value,height=.5))+ 
  geom_boxplot(aes(fill=sample),color="black",width=0.3)+ #以箱形图展示，按分类变量x填充不同的颜色,以及宽度设置
  scale_fill_manual(values = col)+ #修改填充颜色
  ylab(" ")+xlab(" ")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank())+
  theme(axis.text.y=element_text(colour="black", size=12),
        axis.text.x=element_text(angle = 45,vjust = 1,hjust=1,colour="black",size=10),#x轴
        axis.title.y = element_text(vjust=1.5, size = 12))+
  
  theme(legend.position = "none" ,legend.title = element_blank())
p2
library(patchwork)
p <- p1/p2+plot_layout(heights = c(2.5, 1)) 
p
#保存
ggsave("box_bubble.pdf", p, width = 7, height = 8.5)
ggsave("box_bubble.png", p, width = 7, height = 8.5)
#ggsave("box_bubble.tiff", p, width = 7, height = 8.5)

