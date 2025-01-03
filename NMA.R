

# 设定工作目录
setwd("D:\\Medsci\\R\\1.R的GEMTC连续变量") 

#装载gemtc包，如果第一次用，需要先安装：install.packages("gemtc")
library(gemtc)

# 把我们的数据文件导入到R里面
data <- read.csv("data.csv", sep=",", header=T)  

# 相当于数据的预处理
network <- mtc.network(data) 

#网状图的输出
plot(network)

summary(network)

plot(network,  vertex.label.cex=1, 
     vertex.size=20, vertex.shape="square", vertex.label.color="darkblue", 
     vertex.label.dist=4, vertex.label.degree=-pi/3, 
     vertex.color="red", 
     dynamic.edge.width=TRUE,
     edge.color="blue", 
     vertex.label.font=2)

plot(network,  dynamic.edge.width=FALSE)

# 构建模型
model <-mtc.model(network, type="consistency", n.chain=3, likelihood="normal", link="identity", linearModel="random")

# 迭代，产生结果
results <- mtc.run(model, n.adapt = 20000, n.iter = 50000, thin = 1)

summary(results)

# 与placebo相比较的森林图
forest(relative.effect(results, "Placebo"))

# 收敛性诊断
gelman.diag(results)

gelman.plot(results)

# 输出轨迹图、密度图等
plot(results)


# 排序概率
ranks <- rank.probability(results, preferredDirection = -1)


print(ranks)

# 计算SUCRA的函数
sucra <- function(ranks) {
  apply(ranks, 1, function(p) {
    a <- length(p)
    sum(cumsum(p[-a]))/(a-1)
  })
}

a<-sucra(ranks)

a <- round(relative.effect.table(results),2)

write.csv(a, "leaguetable.csv")  # 导出联赛表


# 将干预按照SUCRA大小进行排序的函数
relative.effect.table.sort <- function(result) {
  ts <- rownames(as.matrix(sort(sucra(rank.probability(result, preferredDirection = 1)))))
  tbl <- array(NA, dim=c(length(ts), length(ts), 3), dimnames=list(ts, ts, c("2.5%", "50%", "97.5%")))
  comps <- combn(ts, 2)
  
  for (i in 1:ncol(comps)) {
    comp <- comps[,i]
    samples <- as.matrix(relative.effect(result, comp[1], comp[2], preserve.extra=FALSE)$samples)
    q <- quantile(samples, prob=c(0.025, 0.5, 0.975))
    tbl[comp[1], comp[2],] <- unname(q)
    q.inv <- c(-q[3], -q[2], -q[1])
    tbl[comp[2], comp[1],] <- unname(q.inv)
  }
  
  attr(tbl, "model") <- result[['model']]
 
  class(tbl) <- "mtc.relative.effect.table"
  
  tbl
}

a <- round(exp(relative.effect.table.sort(results)),digits = 2)  # 到处按照SUCRA排序的联赛表
write.csv(a, "S1.csv")




# 不一致性模型

modelume <-mtc.model(network, type="ume", n.chain=4, likelihood="normal",link="identity", linearModel="random")

results <- mtc.run(modelume, n.adapt = 20000, n.iter = 50000, thin = 1)

summary(results)



# 局部不一致性， 节点劈裂法
resultnodesplit <-mtc.nodesplit(network, n.adapt = 20000, n.iter = 50000, thin = 1, n.chain=4,likelihood="normal",link="identity",linearModel="random")

b<-summary(resultnodesplit) 

plot(b, digits = 2)


tiff("1.tiff", height=5000,width=4000, res= 600)
plot(b, digits = 2)
dev.off()


# 异质性分析，谨慎使用
resultanohe <- mtc.anohe(network, n.adapt = 20000, n.iter = 50000, thin = 1, n.chain=4,likelihood="normal",link="identity",linearModel="random")

c<-summary(resultanohe)
print(c)
plot(c)






#########################分类变量#############################


setwd("D:\\Medsci\\R\\R的GEMTC分类变量") 

library(gemtc)

data <- read.csv("data2.csv", sep=",", header=T)  

network <- mtc.network(data) 

plot(network)

model <-mtc.model(network, type="consistency", n.chain=4, likelihood="binom", link="log", linearModel="random")

results <- mtc.run(model, n.adapt = 5000, n.iter = 20000, thin = 1)

summary(results)

forest(relative.effect(results, "ACEi"))

plot(results) 
gelman.plot(results)

ranks <- rank.probability(results, preferredDirection = -1)

print(ranks)

sucra <- function(ranks) {
  apply(ranks, 1, function(p) {
    a <- length(p)
    sum(cumsum(p[-a]))/(a-1)
  })
}

a<-sucra(ranks)

a <- round(exp(relative.effect.table(results)),2)

write.csv(a, "leaguetable.csv")


modelume <-mtc.model(network, type="ume", n.chain=4,likelihood="binom",link="log",linearModel="random")

results <- mtc.run(modelume, n.adapt = 5000, n.iter = 20000, thin = 1)


resultnodesplit <-mtc.nodesplit(network, n.adapt = 20000, n.iter = 50000, thin = 1, n.chain=4,likelihood="binom",link="log",linearModel="random")

b<-summary(resultnodesplit) 


plot(b)


tiff("1.tiff", height=10000,width=4000, res= 600)
plot(b)
dev.off()


# 谨慎使用
resultanohe <- mtc.anohe(network, n.adapt = 20000, n.iter = 50000, thin = 1, n.chain=4,likelihood="binom",link="log",linearModel="random")
c<-summary(resultanohe)
print(c)
plot(c)


###  用ggplot包制作排序概率图

a<-ranks[,]


library(reshape2)


b<-melt(a)


c<-as.data.frame(b)


library(ggplot2)

p <-ggplot(c, aes(x=Var2, y=value, colour=Var1))+geom_line(size=1)

p1<-p+geom_point(size=2)+labs(x="Rank",y="Probability")+theme(axis.text = element_text(size=12),axis.title=element_text(size=15),panel.background=element_blank(),axis.line = element_line(color="black"),
                                                              legend.title=element_blank(),legend.position="bottom")


p2<-p+geom_point(size=2)+labs(x="Rank",y="Probability")+theme(axis.text = element_text(size=12),axis.title=element_text(size=15),axis.line = element_line(color="black"),
                                                              legend.title=element_blank(),legend.position="bottom")
ggsave("temp.pdf", p1, width=7.48, height=6.48)


#######制作累积排序图#################
d<-t(apply(ranks, 1, cumsum))

e<-melt(d)

f<-as.data.frame(e)

p <-ggplot(f, aes(x=Var2, y=value, colour=Var1))+geom_line(size=1)

p1<-p+geom_point(size=2)+labs(x="Rank",y="Cumulative Probability")+theme(axis.text = element_text(size=12),axis.title=element_text(size=15),panel.background=element_blank(),axis.line = element_line(color="black"),
                                                                         legend.title=element_blank(),legend.position="bottom")

p2<-p+geom_point(size=2)+labs(x="Rank",y="Cumulative Probability")+theme(axis.text = element_text(size=12),axis.title=element_text(size=15),axis.line = element_line(color="black"),
                                                                         legend.title=element_blank(),legend.position="bottom")
ggsave("temp2.pdf", p1, width=7.48, height=6.48)

