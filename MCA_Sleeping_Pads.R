setwd("C:/Users/johma/Desktop/main/kouluhommii/Maisteriopinnot/Multivariate Statistical Analysis/Proju/")

library(ggplot2)
library(reshape)
library(ca)
library(ggthemes)
library(scales)
library(tidyverse)
set.seed(123)

df_orig <- read.table("original_data.csv",header = T,sep = ";",dec = ",")

df_mca <- df_orig %>% filter(Weight_g < 1000)

df_mca$s_class <- cut(df_mca$R_Value, breaks=c(0,3,5,10000), labels=c("Summer","Season_3","Season_4"),include.lowest = T,right = F)
df_mca$w_class <- cut(df_mca$Weight_g, breaks=c(0,400,600,800,10000), labels=c("Ultralight","Light","Heavy","Too_Heavy"),include.lowest = T,right = F)
df_mca$t_class <- cut(df_mca$Thickness_cm, breaks=c(0,5,8,10000), labels=c("Thin","Normal","Thick"),include.lowest = T,right = F)
categorical_data <- df_mca %>% select(c(Type,s_class,w_class,t_class))
rs <- sample.int(nrow(df_mca),10)
write.table(df_mca,"newdf.txt",sep="\t",row.names=FALSE)
# rs <- 6 87 65 13 49 84 51 35 55 27
# df_mca[rs,] %>% select(c(Make_Model,R_Value,Weight_g,Thickness_cm,Type))
# cbind(df_mca[rs,"Make_Model"],categorical_data[rs,])
summary(df_mca %>% select(c(R_Value,Weight_g,Thickness_cm)))

# Correlations

round(cor(df_mca %>% select(c(R_Value,Weight_g,Thickness_cm))),2)

mosaicplot(table(categorical_data))

colors_real <- c("indianred1","lightskyblue3","mediumseagreen")


pdf(file="Figures/violin.pdf", height=6, width=8)
test_melt <- melt(df_mca %>% select(c(R_Value,Weight_g,Thickness_cm,Type)), "Type")
ggplot(test_melt, aes(x=Type,y=value,fill=Type)) + 
  geom_violin(trim=TRUE)+
  facet_wrap(.~variable,scales = "free_y")+
  geom_boxplot(width=0.1)+
  theme_tufte(ticks = F)+
  scale_fill_manual(values=colors_real)+
  theme(legend.position="none",
        axis.text.x=element_text(angle=45),
        text = element_text(size=16))
dev.off()



pdf(file="Figures/typecounts.pdf", height=6, width=8)
ggplot(df_mca,aes(x=Type))+
  geom_bar(fill=colors_real)+
  labs(title="",x="Type of pad",y="Count of pads")+
  theme_tufte(ticks = F)+
  theme(plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text( colour = "black"),
        axis.title.y = element_text( colour = "black"),
        panel.grid.major.y = element_line(color = "white"),
        legend.position="none",
        panel.ontop = T,
        text = element_text(size=14),
        axis.line.x = element_line(color="black"))+
  geom_abline(slope=0,intercept=0,  col = "black") 
dev.off()

pdf(file="Figures/typecounts2.pdf", height=6, width=8)
test_melt2 <- melt(categorical_data, "Type")
ggplot(test_melt2,aes(x=value))+
  geom_bar()+
  # facet_wrap(variable~Type,scales = "free_y", strip.position = "top")+
  facet_grid(Type~variable,scales = "free_x") +
  # labs(title="",x="Type of pad",y="Count of pads")+
  theme_tufte(ticks = F)+
  theme(plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text( colour = "black"),
        axis.title.y = element_text( colour = "black"),
        legend.position="none",
        panel.ontop = T,
        text = element_text(size=14),
        panel.grid.major.y = element_line(color = "white"),
        axis.text.x.bottom =element_text(angle=45,hjust = 1))
dev.off()

pdf(file="Figures/scatters.pdf", height=6, width=8)
p <- sample.int(nrow(df_mca))
class <- as.factor(df_mca$Type)
pch_ <- 15:17
colors_real <- c("red","blue","forestgreen")
plot(df_mca[p,]%>%select(c(R_Value,Weight_g,Thickness_cm)),
     col=alpha(colors_real[class[p]], 0.6),
     pch=pch_[class[p]],
     cex=2,
     gap=0,
     lower.panel = NULL,
     main="Red = Foam, Blue = Inflatable, Green = Self-Inflating")
dev.off()


# pairs(df_mca%>%select(c(R_Value,Weight_g,Thickness_cm)),labels = c("R-Value","Weight (g)","Thickness (cm)"),)



# pdf(file="Figures/histograms.pdf")
# par(mfrow=c(3,1))
# hist(df_mca$R_Value,main="R-Value",xlab="")
# hist(df_mca$Weight_g,main="Weight (g)",xlab="g")
# hist(df_mca$Thickness_cm,main="Thickness (cm)",xlab="cm")
# dev.off()
# 

# Analysis


table(categorical_data$Type)

pad.mca <- mjca(categorical_data,lambda="indicator",reti = T)
summary(pad.mca)
pdf(file="Figures/mca.pdf", height=6, width=8)
par(bty = 'n') 
ccol <- 1:13
pad.mca
plot(pad.mca, arrows=c(T,T),map="symmetric",col=c(1,2,4,4,4))
# points(pad.mca$colcoord)
dev.off()
#Points allows us to add the observations to the plot
# test_c <- pad.mca$colcoord[,1:2]
# df_mca[1:10,] %>% select(c(Make_Model,s_class,w_class,t_class,Type))
# Remember the angles are important:
# Between different categories:
# less than 90 degrees = attaction
# more than 90 degrees = repulsion
# 90 degrees = independent
angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}
angle(t(test_c[1,]),test_c[2,])
# Among the same category:
# less than 90 degrees = similar profile
# more than 90 degrees = profile differs

# Mass: mass (weight) of the point
# QLT: quality of representation of the variable:category
# INR: The inertia of the point
# k=1: Principal coordinates of the first dimension
# COR: The relative contribution of the principal axis to the point inertia
# CTR: The absolute contribution of the point to the inertia of the axis
S <- summary(pad.mca)
scree <- data.frame(value = data.frame(S$scree)[,3],
                    grp = paste('PC',1:length(data.frame(S$scree)[,3])))
results_mca <- data.frame(cbind(S$columns[,1],S$columns[,2:4]/1000))
colnames(results_mca) <- c("Category","Mass","Quality of representation", "Inertia of the point")
results_mca


pdf(file="Figures/scree.pdf", height=6, width=8)
ggplot(scree,aes(x=grp))+
  geom_bar(fill="grey55",stat = "identity",aes(y=value))+
  labs(title="",x="Component",y="% of variation explained")+
  theme_tufte(ticks = F)+
  theme(plot.title = element_text(hjust = 0.5, face="bold", colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text( colour = "black"),
        axis.title.y = element_text( colour = "black"),
        panel.grid.major.y = element_line(color = "white"),
        legend.position="none",
        panel.ontop = T,
        text = element_text(size=14),
        axis.line.x = element_line(color="black"))+
  scale_x_discrete(expand= c(0,0))+
  scale_y_continuous(expand= c(0,0))
dev.off()
