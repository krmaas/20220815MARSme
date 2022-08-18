---
title: "taxaBarplot"
author: "Kendra Maas"
date: "11/13/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

###Taxa Barplot
```{r Barplot, fig.height=10, fig.width=8}
taxa.bars <- read.table(file="../yOURpROJECT.selected.taxa.txt", header=T, stringsAsFactors = FALSE)


 taxa.col <- c(
     "Stenotrophomonas"="#d94801", #orange brown
     "Alphaproteobacteria"="#f16913", #orange
     "Bacteriodetes_S24_7"="#79C360", #green
     "Enterococcus"="#A6CEE3", #lightest blue
     "Lactobacillus"="#7DB4D5", #second lightest blue
     "Clostridiales"="#08519c", # darkest blue
     "Staphylococcus"="#6A3D9A", #purple
     "Streptococcus"="#3A89BD", #second darkest blue
     "Firmicutes_other" = "#043363", # really dark blue
     "Bacteroidetes_other"="#4a773a", #dark green
     "Xanthomonadaceae"="#fdae6b", #light orange
     "Bacteria_other"="#d3d3d3", #grey
     "Akkermansia"="#FB9A99") #black


 tax <- melt(taxa.bars, id.vars = c("group", "treatment", "day", "site",
                                    "alt_treat1", "alt_treat1ton", "alt_treat",
                                    "alt_date", "alt_date2",
                                    "alt_treat2", "no_treat_day8", "no_treat_day8_fig",
                                    "no_treat_Ant", "CaCFUgTissue", "mouse"))



 ##check that melt occured the way you expected and that factors are factors
 # str(tax)



 tax2 <- aggregate(value~variable+no_treat_day8_fig+site, data=tax, FUN=sum)

tax2$variable <- factor(tax2$variable,levels=c("Alphaproteobacteria",
                                                  "Stenotrophomonas",
                                                  "Akkermansia",
                                                  "Bacteriodetes_S24_7",
                                                  "Firmicutes_other",
                                                  "Clostridiales",
                                                  "Enterococcus",
                                                  "Lactobacillus",
                                               "Streptococcus",
                                               "Staphylococcus",
                                               "Bacteria_other"))
filter(tax2, site=="T")%>%
ggplot(  aes( y=value, x=factor(no_treat_day8_fig), color=NULL, fill=factor(variable), order=-as.numeric(variable)))+ #order=- makes the stack the same order as the legend
   geom_bar(position="fill", stat="identity")+
     xlab("Treatment")+
     ylab("Percent total community")+
     # facet_grid(.~site)+
     scale_fill_manual(values=taxa.col, guide=guide_legend(title="Taxonomic level"))+
     theme_bw()
 ggsave(file="barplot.tiff", height=5, width=7, dpi=600)
ggsave(file="barplot1.pdf", height=5, width=7)
ggsave(file="barplot1.jpg", height=5, width=7)

summarize by day

tax2 <- aggregate(value~variable+alt_date+treatment+site, data=tax, FUN=sum)

tax2 <- tax %>%
     group_by(variable+alt_date) %>%
     summarise_each(funs(sum))

ggplot(tax2,  aes( y=value, x=factor(alt_date), color=NULL, fill=factor(variable), order=-as.numeric(variable)))+ #order=- makes the stack the same order as the legend
   geom_bar(position="fill", stat="identity")+
     xlab("day")+
     ylab("Percent total community")+
    facet_grid(site~treatment)+
     scale_fill_manual(values=taxa.col, guide=guide_legend(title="Taxonomic level"))+
     theme_bw()
 ggsave(file="barplot.altdate.treatment.pdf", height=5, width=5, dpi=600)


 

```