# Figures for the Volta paper
## Juan Rocha
## juan.rocha@su.se

## Here I kept the code that worked droping all intermediate stages. It comes 
# from the draft Rmd document that is taking too long time to compile. So Now I 
# calculate all analysis and figures separately and only import objects there.

## load libraries
library(tidyverse)
library(forcats)
library(gdata)
library(readxl)
# library (corrgram)
# library(GGally)
library(broom)

# load libraries for mapping
library(maptools)
library(rgeos)
library(RColorBrewer)
# library(ggmap)
library(grid)
library(gridExtra)

# load libraries for clustering plots
library (vegan)

setwd("~/Documents/Projects/TAI/scripts/TAI-Volta")
## load the results obtained with 180116_Analysis.R
load("180129_Volta.RData")


# load map for visualizations and data
# volta.shp <- readShapeSpatial("~/Documents/Projects/TAI/TransformedData/Bundling/Volta_bundling1.shp", proj4string = CRS('+init=epsg:4378'))

# this plots the names on the centers
# plot(volta.shp)
# invisible(text(getSpPPolygonsLabptSlots(volta.shp),
#  labels=as.character(volta.shp@data$ADM_2), cex=0.4))


#############
#### Figure 1
##############
## Retrive the coordinates from the multi-dimensional scaling:
coords <- as_tibble(mds$points) %>% mutate(TAI_ID1 = df$TAI_ID1)

p <- ggplot(data = left_join(clusters, coords) %>%
        gather(key = cluster_algorithm, value = cluster, 2:7) %>%
        mutate(cluster = as.factor(cluster)),
     aes(MDS1, MDS2)) + geom_point(aes(color = cluster), shape = 1) +
    # geom_density_2d(aes(color = cluster), alpha = 0.2) +
    stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.5) +
    scale_color_brewer(palette = "Accent") +
    scale_fill_brewer(palette = "Accent") +
    # coord_equal()+
    facet_wrap(~cluster_algorithm, ncol = 6) + theme_light(base_size = 6)+
    labs(x = "", y = "") + theme (legend.key.size = unit(.4,"cm"))
# p

## Basic map:
# minimize the dataset to what you really nead so joined tables are not super heavy
volta.shp@data <- dplyr::select(volta.shp@data, c(1:3)) %>% mutate(TAI_ID1 = as_factor(as.character(TAI_ID1)))

volta_f <- tidy(volta.shp, region = "TAI_ID1") %>% as_tibble() %>% left_join(volta.shp@data, by = c("id" = "TAI_ID1")) %>% dplyr::rename(region = Region) %>% mutate(id = as_factor(id))

df <- mutate(df, id = TAI_ID1)

# old way without data
g <- ggplot(
    data = left_join(volta_f,
        #gather(df, key = second_tier, value = value, 3:28)),
        gather(clusters, key = clus_algorithm, value = clus, 2:7),
    by = c("id" = "TAI_ID1")),
        # %>%  filter(second_tier == "sd_kcals"),
    aes(long, lat, group = id)) +
    geom_polygon(aes(fill = as.factor(clus)), color = "white" , size = 0.05, alpha = 0.8) +
    scale_fill_brewer(name = "cluster", palette = "Accent") +
    coord_equal() + theme_void(base_size = 7) +
    facet_wrap(~clus_algorithm, ncol = 6) +
    theme(legend.key.size = unit(.4,"cm"))
# g

quartz(height = 3, width = 7, pointsize = 6)

gg <- list(p,g)
source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(1:2), ncol = 1, nrow = 2, byrow = F)
m <- multiplot(plotlist = gg, layout = layout)

quartz.save("figures/fig_1.png", type = "png", width = 7, height = 3, dpi = 500)

##############
### Figure 2
##############

df$clus <- clusters$Hierarchical


df_ostrom <- data_frame(
    first_tier = as_factor(c(
        rep("Ecological", 5), rep("Users", 6), rep("Social", 4), rep("Resource", 4), rep("Interactions", 7 )
    )) ,
    second_tier = as_factor(colnames(df[3:28]))
)
## correct colouring scheme: first tier has it's own color, second tier has a set of shades from the first. But the scheme should keep the same ordering on first and second tiers.

tier2cols <- c(
    brewer.pal(5, "Reds") %>% rev(), # Ecological
    brewer.pal(6, "Blues")%>% rev(), # Users
    brewer.pal(4, "Greens")%>% rev(), # Social
    brewer.pal(4, "Purples")%>% rev(), # Resource
    brewer.pal(7, "Oranges")%>% rev() # Interactions
)

## This should be one of the key figures, Fig 2 with main results.
# quartz(width = 3, height = 4)
mini_map <- ggplot(
    data = left_join(volta_f,
        gather(clusters, key = clus_algorithm, value = clus, 2:7) %>%
        filter(clus_algorithm == "Hierarchical"),
    by = c("id" = "TAI_ID1")),
    aes(long, lat, group = id)) +
    geom_polygon(aes(fill = as.factor(clus)), color = "white" , size = 0.1, alpha = 0.8) +
    scale_fill_brewer(name = "cluster", palette = "Accent") +
    coord_equal() + theme_void(base_size = 7) + ggtitle("A") +
    theme(legend.key.size = unit(0.3,"cm"), legend.position = c(0.2, 0.3))


# quartz(width = 4, height = 4)
flowers <- df %>% gather(
    key = second_tier, value = value, 3:28, factor_key = TRUE) %>%
    left_join(df_ostrom) %>%
    group_by(second_tier, clus, first_tier) %>%
    summarize(mean = mean(value), sd = sd(value)) %>%
    ggplot(aes(fct_rev(second_tier), mean)) +
    geom_hline(aes(yintercept = 1), color = "grey") +
    geom_col(aes(fill = second_tier, color = first_tier), alpha = 0.5, size = 0.2) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd , color = first_tier), size = 0.2, width = 0.5) +
    guides(fill = guide_legend(ncol = 5, nrow = 7)) +
    scale_color_brewer(name = "First tier", palette = "Set1") +
    scale_fill_manual(name = "Second tier", values = tier2cols) +
    coord_polar() +
    facet_wrap(~clus) + theme_void(base_size = 6) +
    theme(
        legend.position = "bottom", legend.direction = "vertical", legend.key.size = unit(.25,"cm"), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(x="", y = "") #+ ggtitle("B")

boxes <- df %>% gather(
    key = second_tier, value = value, 3:28, factor_key = TRUE) %>%
    left_join(df_ostrom) %>%
    # group_by(second_tier, clus, first_tier) %>%
    mutate(clus = clus %>% as.character() %>% as_factor()) %>%
    ggplot(aes(clus, value)) +
    geom_boxplot(aes(fill = clus, color = clus), alpha = 0.5, size = 0.2) +
    guides(fill = guide_legend(ncol = 3, nrow = 2)) +  #coord_flip() +
    facet_wrap(~second_tier, scales = 'free', ncol = 7 ) +
    scale_fill_brewer(name = "cluster", palette = "Accent") + scale_color_brewer(name = "cluster", palette = "Accent") +  theme_light(base_size = 6) + xlab("cluster") + theme(legend.position = c(0.9, 0.1), legend.direction = "horizontal", legend.key.size = unit(.5,"cm")) + ggtitle("B")

quartz(width = 7, height = 7, pointsize = 6)
gg <- list(mini_map, flowers, boxes)
source('~/Dropbox/Code/multiplot.R')
layout <- matrix(c(rep(1, 12), rep(2, 16)), ncol = 7, nrow = 4, byrow = F)
layout <- rbind(layout, matrix(rep(3, 28), ncol = 7, nrow = 4))
multiplot(plotlist = gg, layout = layout)

quartz.save("figures/fig_2.pdf", type = "pdf", width = 7, height = 7, dpi = 500)



####################
## Figure 3
#####################

# create colors
levelCols <- brewer.pal(k, "Accent") # check for better colours with alpha, k = 7

## an especial plot function that does all at once
plot.cube <- function (x, main, c1, c2, c3, sub, ... ){ #takes a cube object
  c <- c("purple",c1, c2, c3) # will never use purple on loop
  for (i in 2:4){
    ## plot
    ordiplot(x[[1]], type='none', main= paste( main, "vs", sub[i], sep = " "), xlab = NULL, ylab = NULL, cex = 0.8, lwd = 0.1)#,
    #xlim = c(-1.2,1.2), ylim = c(-1,1))
    points(
        x[[1]]$points, cex=0.7, lwd=0.8,
        col= levelCols[as.vector(clusters$Hierarchical)])

    ordihull(
        x[[1]], groups= as.vector(clusters$Hierarchical),
        label = FALSE, cex=0.2,col= levelCols, lty = 0,
        draw = 'polygon', alpha = 0.25)

    ## Plot environmental fitting
    plot(x[[i]], p.max=0.001, col= c[i], cex = 1, lwd = 0.2)
    # plot(x[[3]], p.max=0.05, col= c2, cex=.5)
    # plot(x[[4]], p.max=0.05, col= c3, cex=.5)
  }
}

quartz(width = 4, height = 4, pointsize = 6)
par(mfrow = c(4,3), mai = c(0.15,0.15,0.15,0.1))
# Interactions
plot.cube(int.side, main = "Interactions", sub = c( " ","Resource", "Ecological", "Social"),
          c1 = "gray20", # resource
          c2 = "dodgerblue4", # biophysic
          c3 = "darkred") # users
# Resouce
plot.cube(res.side, main = "Resource", sub = c("", "Ecological", "Interactions", "Social"),
          c1 = "dodgerblue4", c2 = "darkgreen", c3 = "darkred")
# biophysic
plot.cube(bio.side, main = "Ecological", sub = c("", "Resource", "Interactions", "Social"),
           c1 = "gray20", c2 = "darkgreen", c3 = "darkred")
# Users
plot.cube(soc.side, main = "Social", sub = c('', "Resource", "Interactions", "Ecological"),
          c1 = "gray20", c2 = "darkgreen", c3 = "dodgerblue4")

quartz.save("figures/fig_3.png", type = "png", width = 4, height = 4, dpi = 500, pointsize = 6)

##################
# Figure 4
##################

## Temporal Cube

mds2 <- list()
for (i in seq_along(c(2005, 2007:2009)) ){  # seq_along(levels(dat$year))
  resp <- crop.dat %>%
    filter (Year == c(2005, 2007:2009)[i]) %>%  #levels(dat$year)
    dplyr::select(TAI_ID1, crop, kcal_crop) %>%
    spread (key = crop, value = kcal_crop) %>%
    dplyr::select(-TAI_ID1)

  mds2[[i]] <- metaMDS(resp, dist = 'manhattan', trymax = 1000)

}

# lapply(mds2, function (x) {x$converged == TRUE})
## Plot them all
quartz(width = 4, height = 4, pointsize = 6)
par(mfcol = c(3,4), mai = c(0.3,0.15,0.15,0.1))

for (i in 1:length(mds2)){

    # NMDS
   ordiplot(mds2[[i]], type='none' , main = c(2005, 2007:2009)[i] , xlab = "Resource", ylab = "")
   points(mds2[[i]]$points, cex=1, lwd=1.5, col= levelCols[as.vector(clusters$Hierarchical)])
   ordihull(mds2[[i]], clusters$Hierarchical, label = FALSE, cex=0.2,
            col= levelCols, lty = 0, draw = 'polygon', alpha = 0.25)

   # environmental fitting
   ef1 <- envfit(mds2[[i]],resource[-1], permu=999)
   ef2 <- envfit(mds2[[i]], social2[-1], permu=999)
   ef3 <- envfit(mds2[[i]], eco[-1], permu = 999)

   plot(ef1, p.max=0.05, col= "gray20", cex=0.8)

   ordiplot(mds2[[i]], type='none' , xlab = "Social", ylab = "")
   points(mds2[[i]]$points, cex=1, lwd=1.5, col= levelCols[as.vector(clusters$Hierarchical)])
   ordihull(mds2[[i]], clusters$Hierarchical, label = FALSE, cex=0.2,
            col= levelCols, lty = 0, draw = 'polygon', alpha = 0.25)
   plot(ef2, p.max=0.05, col="darkred", cex=.8)

   ordiplot(mds2[[i]], type='none' , xlab = "Ecological",  ylab = "")
   points(mds2[[i]]$points, cex=1, lwd=1.5, col= levelCols[as.vector(clusters$Hierarchical)])
   ordihull(mds2[[i]],clusters$Hierarchical, label = FALSE, cex=0.2,
            col= levelCols, lty = 0, draw = 'polygon', alpha = 0.25)
   plot(ef3, p.max = 0.05, col = "dodgerblue4", cex = 0.8)

}
## J171017: Note that to maintain consistency with colours I don't plot the mclus.out, that is a pam clustering over the new mds for each year analysed. Instead I plot the original aggregated fitting over all years

quartz.save("figures/fig_4.pdf", type = "pdf", width = 4, height = 4, dpi = 500, pointsize = 6)

################################
#  regression figures
################################
quartz(width = 3.5, height = 4, pointsize = 6)

bind_rows(
    bind_rows(df_kc),
    bind_rows(df_sd)
) %>% mutate(term = as_factor(term) %>% fct_rev()) %>%
filter(term != 'area') %>%
    ggplot(aes(estimate, term)) +
        geom_vline(xintercept = 0, color = "grey84", linetype = 2) +
        geom_point(aes(shape = ifelse(
            p.value < 0.05, "< 0.05" ,
                ifelse(p.value < 0.1, "< 0.1", "> 0.1")
            )), size = 1, show.legend = TRUE) +
        scale_shape_manual(name = "p value", values = c(19,7,1)) +
        geom_errorbarh(aes(xmin = estimate - std.error , xmax = estimate + std.error, height = .25), size = 0.25)+
        #geom_text(data = df_r2, x=-Inf, y=Inf, hjust = 0, vjust = 45, size = 3, aes(label = paste("italic(R) ^ 2 == ", round(r_squared,2))), parse = T) +
        theme_light(base_size = 6)+ theme(legend.position = "right")+
        facet_grid(response ~ type, scales = "free")

quartz.save("figures/fig_5.pdf", type = "pdf", width = 3.5, height = 4, dpi = 500, pointsize = 6)

#################################
# Supplementary material Figures
#################################

## SMFig 1
## Missing values
missing <- as.data.frame(with(filter(dat, !is.na(area), !is.na(prod)), table(Year, crop)))
g <- ggplot(missing, aes(Year, crop)) +
  geom_tile(aes(fill = Freq)) +
  theme_minimal(base_size = 6) +
  scale_fill_gradient("N",low = "#FFA50080", high = "#0000FF80") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
legend.key.width = unit(0.2, "cm"))

quartz(width = 3, height = 2.5, pointsize = 6)
g

quartz.save("figures/sm_fig1.png", type = "png", width = 3, height = 2.5, dpi = 500, pointsize = 6)

## SMFig 2 big map
## Normalized values second tier variables and their distribution
g <- ggplot(
    data = left_join(volta_f,
        gather(df, key = second_tier, value = value, 3:28)) ,
        # %>% filter(second_tier == "Aridity"),
    aes(long, lat, group = id)) +
    geom_polygon(aes(fill = value), size = 0.01, alpha = 0.8) +
    coord_equal() + theme_void(base_size = 7) + facet_wrap(~second_tier) +
    scale_fill_gradient("normalized values",low = alpha("blue", 0.7),high = alpha("orange", 0.7)) +
    theme(legend.key.height = unit(0.2, "cm"), legend.direction = "horizontal", legend.position = c(0.8, 0.1))

quartz()
g

quartz.save("figures/sm_fig2.pdf", type = "pdf", width = 7, height = 7, dpi = 500)


## SMFig 3
## number of cluster selection
quartz(width = 2, height = 2, pointsize = 6)
clust_results %>% filter(!is.na(Number_clusters))%>% ggplot(
    aes(as.factor(Number_clusters))) +
    geom_bar() + labs(x = "Number of clusters", y = "Frequency among all indexes") + theme_grey(base_size=6)

quartz.save("figures/sm_fig3.png", type = "png", width = 2, height = 2, dpi = 500)

## SMFig 3
### this should be Fig SM complementing Fig 1 and 2
quartz(width = 6, height = 6, pointsize = 6)
df %>% gather(
    key = second_tier, value = value, 3:28, factor_key = TRUE) %>%
    left_join(df_ostrom) %>%
    group_by(second_tier, clus, first_tier) %>%
    ggplot(aes(fct_rev(second_tier), value)) +
    geom_boxplot(aes(fill = second_tier, color = first_tier), alpha = 0.5, size = 0.2) +
    guides(fill = guide_legend(ncol = 6, nrow = 5)) + coord_flip() + xlab("") +
    scale_color_brewer(name = "First tier", palette = "Set1") +
    scale_fill_manual(name = "Second tier", values = tier2cols) +
    facet_grid(first_tier~clus, scales = "free_y") +
  theme_light(base_size = 6) +
  theme(legend.position = "bottom", legend.direction = "vertical", legend.key.size = unit(.5,"cm"))

quartz.save("figures/sm_fig3.pdf", type = "pdf", width = 6, height = 6, dpi = 500)

  # ## SMFig 4
  # ## Correlogram
  # library (corrgram)
  # par(mai = c(0,0,0,0))
  # corrgram(df[3:28], upper.panel = 'panel.cor', lower.panel = "panel.pts", diag.panel = "panel.density" , pch = 46, alpha = 0.5)
  #
  # correlations <- GGally::ggscatmat(
  #     df, columns = 3:28) +
  #     theme_minimal(base_size = 5)


### SM Fig 6
### Meaningful differences graph for Kate
differences_list <- purrr::map(df[3:28], function(x){
    aov(x ~ as.factor(clus), data = df) %>%
    TukeyHSD() %>% tidy()
})

for(i in seq_along(differences_list)){
    differences_list[[i]]$vars <- names(df[3:28])[i]
}

quartz(width = 7, height = 7)
differences_list %>% bind_rows() %>% mutate(vars = as_factor(vars)) %>%
    ggplot(aes(estimate, comparison)) +
    geom_point(aes(shape = ifelse(
        adj.p.value < 0.05, "< 0.05" ,
            ifelse(adj.p.value < 0.1, "< 0.1", "> 0.1")
        )), size = 1.5, show.legend = TRUE) +
    scale_shape_manual(name = "p value", values = c(19,7,1)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = 0.5), size = 0.25) +
    geom_vline(xintercept = 0, color = "grey") + facet_wrap(~vars)+ theme_light(base_size = 7) + theme(legend.position = c(0.8, 0.05), legend.direction = "horizontal", legend.key.size = unit(.5,"cm"))

quartz.save("figures/sm_fig5.png", type = "png", width = 7, height = 7, dpi = 500)
