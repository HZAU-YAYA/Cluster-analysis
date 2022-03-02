#时间2022/2/24 作者：徐博雅 版本：v1.1
library(dplyr)
library(ggplot2)
library(ggsci)
library(ggtree)
library(treeio)
library(cowplot)
args <- commandArgs(trailingOnly = TRUE)#调用外部参数
file_1<-args[1]
file_2<-args[2]
print(file_1)
print(file_2)

abundance = read.table(file_1, header=T, row.names=1, sep="\t", comment.char="",quote = "")#导入数据
group=read.table(file_2, header=T, sep="\t", comment.char="",quote = "")
otu<-abundance
for (n in 1:nrow(abundance)) {
  otu[n,length(abundance[1,])+1]<-sum(abundance[n,1:length(abundance[1,])])
  }
otu<-otu[order(-otu[,length(otu[1,])]),]
otu<-otu[1:20,1:length(otu[1,])-1]
c<- rownames(otu)
for (i in 1:20) {
    w=strsplit(c[i],split = "__")[[1]]
    c[i]<-w[length(w)]
    }
rownames(otu)<-c #行名太长，故只将种名定义为行名
#画树
tree = stats::hclust(vegan::vegdist(t(otu), method = 'bray'), 
              method = 'average') %>%
  tidytree::as.phylo()
# 选择节点，方便后续分开上色
tree =tidytree::groupClade(tree, .node=16)
groupInfo <-base::split(group[,1], group[,2])
tree <- tidytree::groupOTU(tree, groupInfo)
# 绘制聚类图
p1 = ggtree(tree,size=2)
p1 = ggtree(tree,size=2,aes(color=group, linetype=group)) + 
  ggtree::geom_tiplab(size=6,aes(color=group))+theme(legend.title = element_text(size = 20),legend.position = "top")
pdf("树状图.pdf")
p1
dev.off()
p2=otu %>%
  mutate(Species = rownames(otu)) %>%
  reshape2::melt(id.vars = 'Species') %>%
  ggplot(aes(variable, value, fill = Species))+
  geom_bar(stat = 'identity', position = 'fill')+
  scale_x_discrete(limits = colnames(otu))+
  scale_fill_igv()+
  scale_y_continuous(expand = c(0,0))+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  xlab("Sample") +
  ylab("relative abundance")
ggsave("example2.eps",device = cairo_ps)
png("堆叠图.png")
p2
dev.off()
pdf("堆叠图.pdf")
p2
dev.off()
#绘制组合图
treelabel<-p1$data[1:12,]
treelabel<-treelabel[order(treelabel$y),]#使树状图顺序样品与堆叠图样品顺序相等
p2 = p2+theme(axis.ticks.y = element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.line = element_blank(),
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 20),
              axis.title=element_text(size = 20))+ scale_x_discrete(limits = treelabel$label)+
  coord_flip()

p3=ggdraw()+
  draw_plot(p1, 0, 0.06, 0.3, 0.95)+
  draw_plot(p2, 0.3, 0, 0.7, 1)#拼接图片
png("组合图.png",width = 2300,height = 1121)
p3
dev.off()
pdf("组合图.pdf",width = 23.70,height = 11.00)
p3
dev.off()



