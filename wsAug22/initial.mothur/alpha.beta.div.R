#### First run import.data.R ####

#reorder Type
alpha.expdata$Type <- factor(alpha.expdata$Type, levels = c("water", "sed", "soil"))


##### initial look at alpha diversity

ggplot(data=alpha.expdata, mapping = aes(x = Type, y = sobs))+
  geom_boxplot()

sobs.anova <- aov(sobs ~ Type, data=alpha.expdata)
summary(sobs.anova)

ggplot(data=alpha.expdata, mapping = aes(x = Type, y = shannon))+
  geom_boxplot()

shannon.anova <- aov(shannon ~ Type, data=alpha.expdata)
summary(shannon.anova)
TukeyHSD(shannon.anova)

ggplot(data=alpha.expdata, mapping = aes(x = Type, y = invsimpson))+
  geom_boxplot()

invsimpson.anova <- aov(invsimpson ~ Type, data=alpha.expdata)
summary(invsimpson.anova)
TukeyHSD(invsimpson.anova)

### beta diversity!!!
## first NMS on Bray Curtis

bc.nms <- metaMDS(as.dist(bc), k=2, trymin=50, trymax=500, wascores = F)
ordiplot(bc.nms)

#plot looks reasonable, lets make it pretty

bc.points <- data.frame(bc.nms$points)
bc.plot <- ggplot(data = bc.points, mapping = aes(x=MDS1, y=MDS2, label = rownames(bc)))  

x <- max(bc.points$MDS1)/1.5  
y <- min(bc.points$MDS2)

bc.plot +
  geom_point(mapping = aes(fill=factor(alpha.expdata$Type), shape = factor(alpha.expdata$Site)), size =4)+
  scale_shape_manual(values = c(21, 23))+
  annotate("text", x, y, label=paste("stress = ", round(bc.nms$stress, digits =3)))+
  labs(fill = "Sample Type", shape = "Site")+
  guides(fill = guide_legend(override.aes = list (shape = 22)))+
  theme_bw()+
  ggtitle("Bray Curtis")

# finding help for a package or function (not great, I'd rather google an issue but this is built in)
??ggplot2



## first NMS on Jaccard

jc.nms <- metaMDS(as.dist(jc), k=2, trymin=50, trymax=500, wascores = F)
ordiplot(jc.nms)

#plot looks reasonable, lets make it pretty

jc.points <- data.frame(jc.nms$points)
jc.plot <- ggplot(data = jc.points, mapping = aes(x=MDS1, y=MDS2, label = rownames(jc)))  

x <- max(jc.points$MDS1)/1.5  
y <- min(jc.points$MDS2)

jc.plot +
  geom_point(mapping = aes(fill=factor(alpha.expdata$Type), shape = factor(alpha.expdata$Site)), size =4)+
  scale_shape_manual(values = c(21, 23))+
  annotate("text", x, y, label=paste("stress = ", round(jc.nms$stress, digits =3)))+
  labs(fill = "Sample Type", shape = "Site")+
  guides(fill = guide_legend(override.aes = list (shape = 22)))+
  ggtitle("Jaccard")+
  theme_bw()



## first NMS on Theta YC

tyc.nms <- metaMDS(as.dist(tyc), k=2, trymin=50, trymax=500, wascores = F)
ordiplot(tyc.nms)

#plot looks reasonable, lets make it pretty

tyc.points <- data.frame(tyc.nms$points)
tyc.plot <- ggplot(data = tyc.points, mapping = aes(x=MDS1, y=MDS2, label = rownames(tyc)))  

x <- max(tyc.points$MDS1)/1.5  
y <- min(tyc.points$MDS2)

tyc.plot +
  geom_point(mapping = aes(fill=factor(alpha.expdata$Type), shape = factor(alpha.expdata$Site)), size =4)+
  scale_shape_manual(values = c(21, 23))+
  # geom_text()+
  annotate("text", x, y, label=paste("stress = ", round(tyc.nms$stress, digits =3)))+
  labs(fill = "Sample Type", shape = "Site")+
  guides(fill = guide_legend(override.aes = list (shape = 22)))+
  ggtitle("Theta YC")+
  theme_bw()
ggsave(filename = "tyc.nms.jpg")


# fit experiemtnal or environmental data onto ordination space
#envfit #function in vegan, univariate strength to one point in ordination
#ordisurf # function in vegan, univariate? topography of where that variable is higher

#### Hypothesis testing
#permanova

jc.perm <- adonis(as.dist(jc) ~ alpha.expdata$Type * alpha.expdata$Site, perm = 99, rm.na = TRUE)
jc.perm

jc.perm2 <- adonis(as.dist(jc) ~ alpha.expdata$Site * alpha.expdata$Type, perm = 99, rm.na = TRUE)
jc.perm2

bc.perm <- adonis(as.dist(bc) ~ alpha.expdata$Type * alpha.expdata$Site, perm = 99, rm.na = TRUE)
bc.perm

bc.perm2 <- adonis(as.dist(bc) ~ alpha.expdata$Site * alpha.expdata$Type, perm = 99, rm.na = TRUE)
bc.perm2

tyc.perm <- adonis(as.dist(tyc) ~ alpha.expdata$Type * alpha.expdata$Site, perm = 99, rm.na = TRUE)
tyc.perm

tyc.perm2 <- adonis(as.dist(tyc) ~ alpha.expdata$Site * alpha.expdata$Type, perm = 99, rm.na = TRUE)
tyc.perm2


# indicator species, try to find OTUs that are responsible for Type being sig varibility
##look at indicspecies R help for good explaination of what it's doing
indic <- multipatt(otu[,-1], alpha.expdata$Type, control = how(nperm=99))

summary(indic)

write.csv(file = "indicator.species.csv",
          indic$sign %>%
            rownames_to_column(var = "OTU") %>%
            mutate(p.fdr = round(p.adjust(p.value, "fdr"), 3)) %>%
            right_join(taxa, by = "OTU") %>%
            arrange(index)
          )












