#############
# Line plot #
#############

## Figure 1 b

# Load packages
library(ggplot2)
library(reshape2)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load data
data <-read.delim("Figure 1 b.txt", sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
data <- melt(data,id = 'ppm')

# Define the category
data[0 < data$ppm & data$ppm < 45,4] <- 'Alkyl C'
data[45 < data$ppm & data$ppm < 60,4] <- 'Methoxyl C'
data[60 < data$ppm & data$ppm < 95,4] <- 'O-alkyl C'
data[95 < data$ppm & data$ppm < 110,4] <- 'Acetal C'
data[110 < data$ppm & data$ppm < 145,4] <- 'Aromatic C'
data[145 < data$ppm & data$ppm < 160,4] <- 'O-aromatic C'
data[160 < data$ppm & data$ppm < 215,4] <- 'Carbonxyl C'

# Plot line plot
p <- ggplot(data, aes(x=ppm, y=value*100, color=variable)) +
  geom_line(size=1.5)+
  scale_color_manual(values =  c('#707070','#ECB388','#B7583E'))+
  labs(x = 'ppm', y =expression(paste("Intensity", ' ', '(','%',')')), title = 'Soil organic carbon chemical composition')+ 
  theme_classic() +
  theme(axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18), plot.title = element_text(size = 20),
        legend.position = 'top', 
        legend.title=element_blank(), legend.text=element_text(size=18))+
  scale_x_continuous(expand = c(0,0),breaks = seq(0,250,30))+
  scale_y_continuous(limits = c(-0.05,0.6))+
  geom_vline(xintercept = 45, color = 'gray', size = 1, linetype = "dashed")+
  geom_vline(xintercept = 60, color = 'gray', size = 1, linetype = "dashed")+
  geom_vline(xintercept = 95, color = 'gray', size = 1, linetype = "dashed")+
  geom_vline(xintercept = 110, color = 'gray', size = 1, linetype = "dashed")+
  geom_vline(xintercept = 145, color = 'gray', size = 1, linetype = "dashed")+
  geom_vline(xintercept = 160, color = 'gray', size = 1, linetype = "dashed")+
  geom_vline(xintercept = 215, color = 'gray', size = 1, linetype = "dashed")

# Save plot
ggsave('Figure 1 b.pdf', p, width = 8, height = 5)

###############
# Violin plot #
###############

## Figure 2 a and b, Figure 3 c
## Take Figure 2 a as an example

# Load packages
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(reshape2)
library(readxl)
library(PMCMRplus)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load data
data <- read.delim('Figure 2 a.txt',stringsAsFactors = FALSE, check.names = FALSE)
data <- melt(data,id = c('group','time'))
data$time <- as.factor(data$time)
data$group<-factor(data$group,levels = c('Mollisol','Inceptisol','Ultisol'),ordered = T)

#Plot violin plot
p <- ggplot(data, aes(x = time, y = value)) +
  geom_hline(yintercept = 0,color = 'gray54', size = 1, linetype = "longdash")+
  geom_violin(aes(fill = group), color = 'white', 
              trim = FALSE, position = position_dodge(0.9),alpha = 0.8)+
  scale_fill_manual(values =  c('#707070','#ECB388','#B7583E'))+
  stat_ydensity(aes(fill = group),color = 'white',
                trim = FALSE,scale = "width")+
  labs(x = '', y = 'Differences compared to CK', title = 'Chlorophyll contents of maize plant (SynCom I)',cex.lab = 3)+ 
  theme_classic() +
  theme(axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18), plot.title = element_text(size = 24),
        legend.position = 'top', 
        legend.title=element_blank(), legend.text=element_text(size=18))+
  facet_wrap(~time, scales = 'free_x', ncol = 3)+
  theme(strip.text = element_blank())+
  scale_y_continuous(limits = c(-20,25))+
  geom_boxplot(aes(color = group),fill = 'white', width=0.25, 
               position=position_dodge(0.9),outlier.shape = NA) +
  scale_color_manual(values =  rev(c('black','black','black')))

# Save plot
ggsave('Figure 2 a.pdf', h, width = 6, height = 5)

# Significant test
kruskal.test(value~group,data=data)
PMCMRplus::kwAllPairsDunnTest(value~group,data=data)

############
# Bar plot #
############

#Figure 2 c

# Load packages
library(ggplot2)
library(ggpubr)
library(reshape2)
library(plyr)
library(readxl)
library(PMCMRplus)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load data
abun <- read.delim('Figure 2 c.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
abun <- melt(abun,'group')

# Prepare Data
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

data <- data_summary(abun, varname='value', groupnames = c('variable',"group"))
data$se <- data$sd/(6^(1/2))

# Plot bar plot
p <- ggplot(data, aes(variable, value/10^7, group = group, fill = group)) +
  geom_bar(stat="identity", color="black", width = 0.8,
           position=position_dodge(0.8)) +
  geom_errorbar(aes(ymin=(value-se)/10^7, ymax=(value+se)/10^7), width=.2,
                position=position_dodge(0.8))+
  labs(x = '', 
       y = expression(paste('Absolute Abundance', ' ', '(',10^7,')')),
       title = "Distribution of bin's lineage in soils")+
  scale_fill_manual(values =  rev(c('#707070','#ECB388','#B7583E')))+
  theme_classic() +
  theme(axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18), plot.title = element_text(size = 24),
        legend.position = 'top', 
        legend.title=element_blank(), legend.text=element_text(size=15))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()

# Save plot
ggsave('Figure 2 c.pdf', p, width = 8, height = 7)

#Significant test
abun <- read.delim('Figure 2 c.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
abun$group <- as.factor(abun$group)
kruskal.test(abun$Conexibacteraceae~group,data=abun)
posthoc.kruskal.dunn.test(abun$Conexibacteraceae~group,data=abun)
kruskal.test(abun$Micrococcaceae~group,data=abun)
posthoc.kruskal.dunn.test(abun$Micrococcaceae~group,data=abun)
kruskal.test(abun$Rhizobiaceae~group,data=abun)
posthoc.kruskal.dunn.test(abun$Rhizobiaceae~group,data=abun)
kruskal.test(abun$Xanthomonadaceae~group,data=abun)
posthoc.kruskal.dunn.test(abun$Xanthomonadaceae~group,data=abun)
kruskal.test(abun$Burkholderiales~group,data=abun)
posthoc.kruskal.dunn.test(abun$Burkholderiales~group,data=abun)

##############
# Radar plot #
##############

## Figure 3 b

# Load packages
library(magrittr)
library(ggradar)
library(dplyr)
library(scales)
library(tibble)
library(ggplot2)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load data
data <- read.table("Figure 3 b.txt", row.names = 1, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# Prepare data
data[1:4,] %>%
  rownames_to_column( var = "group" ) %>%
  mutate_at(vars(-group),funs(rescale)) %>% 
  select(1:9) -> mydata

# Plot radar plot
p <- ggradar(mydata[1:4,],
             base.size = 3,
             values.radar = c("0%","50%","100%"),
             group.point.size = 3,
             group.colours = c('#0a3d64','#eaad5a','#a8b293','#df8053'),
             group.line.width = 1.5,
             axis.label.size = 3,
             grid.label.size = 5,
             grid.line.width = 1.5,
             axis.line.colour = "gray",
             gridline.mid.colour = "gray",
             background.circle.colour = "white",
             gridline.max.linetype = "solid",
             legend.title = "NA",
             legend.text.size = 6,
             legend.position = "right")

# Save plot
ggsave('Figure 3 b.pdf', p, width = 4, height = 4)

############
# PCA plot #
############

# Figure 3 e

# Load packages
library(ggplot2)
library(reshape2)
library(vegan)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load data
od <- read.delim('Figure 3 e_1.txt',sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
source <- read.delim('Figure 3 e_2.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

# Prepare data
od <- melt(od, id = c('colname','group'))
source <- melt(source, id = c('colname'))

# Merge data
data <- merge(od,source, by=c('colname','variable'))
colnames(data) <- c('row','col','group','value','carbon')
data <- data[3:5]
data <- dcast(data,group~carbon,mean)

# Remove the first line and water treatment
row.names(data) <- data$group
data <- data[-1]
data <- data[,-7]

pca <- rda(data, scale = TRUE)
summary(pca, scaling = 1)

pca_eig <- pca$CA$eig
pca_exp <- pca$CA$eig / sum(pca$CA$eig)
site.scaling1 <- scores(pca, choices = 1:2, scaling = 1, display = 'site')
site.scaling1
env.scaling2 <- scores(pca, choices = 1:2, scaling = 2, display = 'sp')
env.scaling2 <- summary(pca, scaling = 2)$species[ ,1:2]
env.scaling2
site.scaling1 <- data.frame(summary(pca, scaling = 1)$sites[ ,1:2])
env.scaling1 <- data.frame(summary(pca, scaling = 1)$species[ ,1:2])

#Adding group Information
site.scaling1$sample <- rownames(site.scaling1)
site.scaling1$group <- c(rep('SynCom', 4), rep('PGPRs', 4))
env.scaling1$group <- rownames(env.scaling1)
#write.table(env.scaling1, 'env.scaling1.txt', col.names = NA, sep = '\t', quote = FALSE)

# Adding carbon source information
type <- read.delim('Figure 3 e_3.txt',sep = '\t', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
env.scaling1 <- merge(env.scaling1, type, by='group')
env.scaling1 <- subset(env.scaling1, env.scaling1$type != 'Water')

# Plot PCA plot
p <- ggplot(site.scaling1, aes(PC1, PC2)) +
  geom_point(aes(color = group),size = 6) +
  geom_vline(xintercept = 0, color = 'gray', size = 0.5) + 
  geom_hline(yintercept = 0, color = 'gray', size = 0.5)+
  scale_color_manual(values = c('#57A5B1','#F3E4C7','#EEA47D','#DA5F5A','#CCB29F',"#89384D",
                                '#F8CB87','#ADD5D4')) + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18), plot.title = element_text(size = 20),
        legend.position = 'right', 
        legend.title=element_text(size = 18), legend.text=element_text(size=18))+
  labs(x = 'PC1 (59.87%)', y = 'PC2 (24.39%)',title = 'PCA')+
  scale_x_continuous(limits = c(-2.5,2.5),breaks = seq(-2.5,2.5,1))+
  geom_segment(data = env.scaling1, 
               aes(x = 0, y = 0, xend = PC1*3, yend = PC2*3, color = type), 
               arrow = arrow(length = unit(0.2, 'cm')), 
               size = 0.8, alpha = 0.8)

# Save plot
ggsave('Figure 3 e.pdf', p, width = 8,height = 5)

#############
# Diversity #
#############

#Figure 4 a

# Load packages
library(picante)
library(ggsci)
library(ggplot2)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load data
otu <- read.delim('Figure 4 a_1.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
group <- read.delim('Figure 4 a_2.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)

# Shannon index
shannon <- as.data.frame(diversity(otu, index = 'shannon', base = exp(1)))
colnames(shannon)=c('shannon')

# Prepare data
dat <- cbind(shannon,group)
dat$group1<-factor(dat$group1,levels = c('CK','SynCom','PGPRs'))
dat$group2<-factor(dat$group2,levels = c('Mollisols','Cambisol','Acrisol'))

# Plot alpha diversity
p <- ggplot(dat, aes(group1, shannon))+
  geom_boxplot(aes(fill = group1), color = 'black',
               width=0.7, position=position_dodge(0.9))+#绘制箱线图
  scale_fill_manual(values = c('gray','#ADD5D4','#F8CB87',
                               'gray','#ADD5D4','#F8CB87',
                               'gray','#ADD5D4','#F8CB87'))+
  labs(x = '', y ='Shannon index', title = 'Alpha diversity index')+ 
  theme_classic() +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18), plot.title = element_text(size = 20),
        legend.position = 'top', 
        legend.title=element_blank(), legend.text=element_text(size=18))
ggsave('Figure 4 a.pdf', p, width = 6, height = 6)

######################
# Circle + tree plot #
######################

## Figure 4 d

# Load packages
library(ggtreeExtra)
library(ggstar)
library(ggplot2)
library(ggtree)
library(treeio)
library(ggnewscale)
library(ggsci)
library(tidytree)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load tree data (Mega X)
tree <- read.newick("Figure 4 d_1.nwk", node.label = "support")

# This dataset will to be plotted point and bar.
dat <- read.delim('Figure 4 d_2.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

# Plot tree plot in R
p <- ggtree(tree, layout="circular", open.angle = 0, size = 0.8, 
            ladderize = F, branch.length = 'none')

# Adding catagory and abundance
p1 <- p + 
  scale_size_continuous(range=c(1, 6),
                        guide=guide_legend(keywidth=0.5, keyheight=0.5,order=2))+
  geom_tiplab(size=3, offset=7.2)+
  geom_hilight(node=57, fill='#FFB319')+ #Actinobacteria
  geom_hilight(node=67, fill='#FFE194')+ #Gemmatimonadetes
  geom_hilight(node=15, fill='#BE9FE1')+ #Planctomycetes
  geom_hilight(node=70, fill='#D5C455')+ #candidate_division_WPS-1
  geom_hilight(node=75, fill='#60A9A6')+ #candidate_division_WPS-2
  geom_hilight(node=c(77,23), fill='#E8F6EF')+ #Chloroflexi
  geom_hilight(node=47, fill='#E8F6EF')+ #Chloroflexi
  geom_hilight(node=79, fill='#B8DFD8')+ #Acidobacteria
  geom_hilight(node=87, fill='#EDE59A')+ #Proteobacteria
  geom_hilight(node=39, fill='#DAA592')+ #Armatimonadetes
  geom_hilight(node=92, fill='#FF616D')+ #Bacteroidetes
  geom_hilight(node=c(93), fill='#EDE59A')+ #Proteobacteria
  geom_hilight(node=46, fill='#957DAD')+ #Candidatus_Saccharibacteria
  geom_fruit(data=dat,geom=geom_point2,mapping = aes(y = label, size = OTUsize), 
             color = '#444444',position="identity")
p1
d <- dat[3]
rownames(d) <- dat$label
pa <- gheatmap(p1, d, offset = 0.3, width = 0.3, 
               colnames = F,low = "#D8E3E7", high = "#E2979C", color = "white",
               legend_title = c(expression(paste(delta, "OTU_Niche"["Syncom P-CK"]))))+
  labs(title = 'Ecological niche at OTU level in Acrisol')+
  theme(plot.title = element_text(size = 20),
        legend.position = 'right', 
        legend.text=element_text(size=18))

# Save plot
ggsave("Figure 4 d.pdf", pa, width =6,height =6)

############
# Heat map #
############

#Figure 4 e

# Load packages
library(pheatmap)
library(vegan)
library(ggplot2)

# Set working directory
setwd("C:/Users/J/Desktop")

# Load data
df <- read.delim('Figure 4 e.txt', sep = '\t',stringsAsFactors = FALSE, check.names = FALSE)

# Plot heat map
p <- ggplot(data = df, aes(x=row, y=col))+
  geom_tile(aes(fill=delta))+
  scale_fill_gradient2(limits = c(-1.1,1.1),low = "#CCD1E4",mid = 'white', high = "#EEC373")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        axis.title = element_text(size = 18), plot.title = element_text(size = 20),
        legend.position = 'right', 
        legend.title=element_blank(), legend.text=element_text(size=18),
        panel.grid.major=element_line(colour=NA))+
  labs(x='',y='',title = 'Shifted niche overlap (p < 0.05) in Mollisol (SynCom)')+
  coord_flip()

# Save plot
ggsave('Figure 4 e.pdf', h, width = 6, height = 5)

