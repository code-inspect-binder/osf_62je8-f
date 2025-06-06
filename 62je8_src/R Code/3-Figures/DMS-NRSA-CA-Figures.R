#=========================================================================#
#                               NRSA-CA                                   #
#=========================================================================#

# National River and Streams Assessment (NRSA)-Community Assembly (CA)

# Author: David Murray-Stoker (dstoker92@gmail.com)
# Last Updated: 1 April 2020

## Table of Contents
# Line  24: Load Packages and Data
# Line  60: Site Map
# Line 110: Additive Partitioning
# Line 130: Taxonomic Richness
# Line 150: Taxonomic Diversity
# Line 185: Community Composition
# Line 236: CUS BRT
# Line 269: Ecoregion BRTs
# Line 523: End of Script


#=========================================================================#
#                             Load Packages & Data                        #
#==========================================================================
library(plyr) # Load before dplyr to avoid any potential issues with both packages

library(cowplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsn)
library(grid)
library(gridExtra)
library(gtable)
library(mapdata)
library(reshape2)
library(rgdal)
library(vegan)
library(viridis)
library(viridisLite)

## Read in workspace 
load("~/Documents/Research/NRSA-CA/R Code/1-Primary Analysis/DMS-NRSA-CA-Workspace.RData")

## Source code for helper functions and themes for creating figures
source("~/Documents/R Materials/R Script/DMS-ggplot2-supplement.R")

## Set file directory for figure files
setwd("~/Documents/Research/NRSA-CA/Publication Figures")

## Set vetor of colours from viridis pallete
viridis.3 <- viridis(n = 3)

#=========================================================================#
#                                Figures                                  #
#==========================================================================

#-------------------------------------------------------------------------#
#                                Site Map                                 #
#--------------------------------------------------------------------------

## Figure was created by evaluating the supplementary R code and data
## provided by King et al. 2019 (Ecological Applications), deposited by
## Katelyn King on Zenodo at:

# https://zenodo.org/record/3246537#.XgDUKRdKhSw

## Site Map---------------------------------------------------------------#

## Set base projection and site locations
usa <- map_data("usa")  # pull out the USA map
base.map <- ggplot(data = usa) + 
	geom_polygon(aes(x = long, y = lat, group = group), 
							 fill = "white", color = "black") + 
	coord_fixed(1.3) 
points <- select(final.data, site.long, site.lat, ecoregion)

## Add points to the US map
site.map <- base.map +
	geom_point(data = points, size = 1, aes(x = site.long, y = site.lat, 
																					colour = ecoregion, shape = ecoregion)) +
	scale_colour_manual(values = viridis(n = 9), name = "Ecoregion") + 
	scale_shape_manual(values = c(0:8),  name = "Ecoregion") +
	north(data = usa, symbol = 3, scale = 0.1, location = "bottomright", 
				anchor = c(x = -120, y = 27)) +
	ggsn::scalebar(data = usa, dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84", 
								 st.size = 2, location = "bottomleft") +
	theme(panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line.x = element_blank(), 
				axis.title.x = element_blank(),
				axis.ticks.x = element_blank(),
				axis.text.x = element_blank(),
				axis.line.y = element_blank(), 
				axis.title.y = element_blank(),
				axis.ticks.y = element_blank(),
				axis.text.y = element_blank(),
				panel.background = element_rect(colour = "black", size = 0.5, fill = NA)) +
	theme(legend.position = c(0.88, 0.20), legend.text = element_text(size = 9) ) +
	theme(legend.text = element_text(size = 9))

## Export the figure
ggsave("Figure_1-site_map.pdf", plot = site.map, width = 12, height = 12,
			 units = "in", device = "pdf", dpi = 1000)



#-------------------------------------------------------------------------#
#                         Additive Partitioning                           #
#--------------------------------------------------------------------------

adipart.plot <- ggplot(data = richness.partitioning.results.final, 
											 aes(x = statistic.type, y = statistic, fill = diversity)) + 
	geom_bar(stat = "identity", width = 0.5) +
	scale_fill_manual(labels = c(expression(alpha), expression(paste(beta[1])), expression(paste(beta[2]))),
										values = viridis.3,
										limits = c("alpha", "beta1", "beta2")) + 
	scale_x_discrete(labels = c("observed" = "Observed", "simulated" = "Simulated")) + 
	scale_y_continuous(breaks = c(0.0, 0.5, 1.0), limits = c(0, 1.1)) +
	labs(x = "Type", y = "Partitioned Diversity") +
	basic.theme +
	theme(legend.position = c(0.5, 0.45),
				legend.title = element_blank(),
				legend.text = element_text(size = 12)) +
	ggtitle("(a)")


#-------------------------------------------------------------------------#
#                          Taxonomic Richness                             #
#--------------------------------------------------------------------------

richness.plot <- ggplot(data = taxonomic.richness.summary, 
												aes(x = ecoregion, y = mean)) + 
	geom_point(size = 3, color = viridis(n = 9)) + 
	geom_errorbar(data = taxonomic.richness.summary, 
								mapping = aes(x = ecoregion, ymin = mean - se, ymax = mean + se), 
								width = 0.2, size = 1, color = viridis(n = 9)) + 
	labs(x = "Ecoregion", y = "Taxonomic Richness") +
	scale_y_continuous(breaks = c(20, 30, 40, 50), limits = c(20, 50)) +
	scale_x_discrete(limits = c("WMT", "XER", "NPL", "SPL", "TPL", "UMW",
															"NAP", "SAP", "CPL")) +
	geom_text(aes(label = c("c", "a", "cd", "a", "d", "cd", "b", "b", "c"),
								vjust = -1.5)) +
	remove.x.axis.labels +
	ggtitle("(b)")


#-------------------------------------------------------------------------#
#                          Taxonomic Diversity                            #
#--------------------------------------------------------------------------

diversity.plot <- ggplot(data = taxonomic.diversity.summary, 
												aes(x = ecoregion, y = mean)) + 
	geom_point(size = 3, color = viridis(n = 9)) + 
	geom_errorbar(data = taxonomic.diversity.summary, 
								mapping = aes(x = ecoregion, ymin = mean - se, ymax = mean + se), 
								width = 0.2, size = 1, color = viridis(n = 9)) + 
	labs(x = "Ecoregion", y = "Taxonomic Diversity") +
	scale_y_continuous(breaks = c(2.0, 2.6, 3.2), limits = c(1.95, 3.25)) +
	scale_x_discrete(limits = c("WMT", "XER", "NPL", "SPL", "TPL", "UMW",
															"NAP", "SAP", "CPL")) +
	geom_text(aes(label = c("de", "ab", "de", "a", "e", "de", "cd", "bc", "cde"),
								vjust = -1.5)) +
	basic.theme +
	ggtitle("(c)")


## Putting panel figure together
g1 <- ggplotGrob(adipart.plot)
g2 <- ggplotGrob(richness.plot)
g3 <- ggplotGrob(diversity.plot)
panel.figure <- rbind(g1, g2, g3, size = "first")
panel.figure$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)
grid.newpage()
grid.draw(panel.figure)

## Export the figure
ggsave("Figure_2-community_diversity.pdf", plot = grid.draw(panel.figure), 
			 width = 6, height = 8,
			 units = "in", device = "pdf", dpi = 1000)


#-------------------------------------------------------------------------#
#                            Community Composition                        #
#--------------------------------------------------------------------------

## NMDS plot for the manuscript

## Set figure dimensions
pdf("Figure_3-community_composition.pdf", width = 10, height = 10)

## Create the figure
ordiplot(BC.NMDS, choices = c(1, 2), type = "none", display = "sites",
				 xlim = c(-2, 2), ylim = c(-1, 1), xlab = "NMDS 1", ylab = "NMDS 2")
ordiellipse(BC.NMDS, groups = final.data$ecoregion, 
						draw = "lines", col = viridis(n = 9), label = FALSE, lwd = 5)
mtext(text = "Stress = 0.206", side = 3, line = -1.5, adj = 0.95, 
			cex = 1.05)
legend(1.35, 0.45, c("CPL", "NAP", "NPL", "SAP", "SPL", "TPL", "UMW", "WMT", "XER"),  
			 cex = 0.8, lty = 1, lwd = 4, x.intersp = 0.95, y.intersp = 0.95,
			 col = viridis(n = 9))

## Export the figure
dev.off()


## NMDS plot (axes 1 and 2)
axes.1_2 <-ordiplot(BC.NMDS, choices = c(1, 2), type = "none", display = "sites",
										xlim = c(-2, 2), ylim = c(-1, 1), xlab = "NMDS 1", ylab = "NMDS 2")
ordiellipse(BC.NMDS, groups = final.data$ecoregion, 
						draw = "lines", col = viridis(n = 9), label = FALSE, lwd = 5)
mtext(text = "Stress = 0.206", side = 3, line = -1.5, adj = 0.95, 
			cex = 1.05)
mtext(text = "(a)", side = 3, line = 1.75, adj = 0.025, cex = 1.25)
legend(1.35, 0.45, c("CPL", "NAP", "NPL", "SAP", "SPL", "TPL", "UMW", "WMT", "XER"),  
			 cex = 0.8, lty = 1, lwd = 4, x.intersp = 0.25, y.intersp = 0.25,
			 col = viridis(n = 9))

## NMDS plot (axes 1 and 3) NOTE: Placed in the supplement
axes.1_3 <- ordiplot(BC.NMDS, choices = c(1, 3), type = "none", display = "sites",
										 xlim = c(-2, 2), ylim = c(-1, 1), 
										 xlab = "NMDS 1", ylab = "NMDS 3")
ordiellipse(axes.1_3, groups = final.data$ecoregion, 
						draw = "lines", col = viridis(n = 9), label = FALSE, lwd = 5)
mtext(text = "(b)", side = 3, line = 1.75, adj = 0.025, cex = 1.25)


## Both panels (axes.1_2 & axes.1_3) were exported as separate plots and aligned 
## using Inkscape.


#-------------------------------------------------------------------------#
#                          Boosted Regression Trees                       #
#--------------------------------------------------------------------------

## CUS BRT plot-----------------------------------------------------------#
CUS.BRT.plot <- 
  ggplot(data = CUS.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  basic.theme +
	theme(legend.position = c(0.85, 0.825),
				legend.title = element_blank())

## Export the figure
ggsave("Figure_4-CUS_BRT.pdf", plot = CUS.BRT.plot, 
			 width = 6, height = 8,
			 units = "in", device = "pdf", dpi = 1000)


## Plots for each ecoregion-----------------------------------------------#

## CPL BRT plot
CPL.BRT.plot <- 
  ggplot(data = CPL.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.all.axis.labels +
	theme(axis.text.x   = element_text(size = 12, colour = "gray40")) +
	ggtitle("(i) CPL")

## NAP BRT plot
NAP.BRT.plot <- 
  ggplot(data = NAP.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.all.axis.labels +
  ggtitle("(g) NAP")

## NPL BRT plot
NPL.BRT.plot <- 
  ggplot(data = NPL.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.all.axis.labels +
  ggtitle("(e) NPL")

## SAP BRT plot
SAP.BRT.plot <- 
  ggplot(data = SAP.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.all.axis.labels +
  ggtitle("(h) SAP")

## SPL BRT plot
SPL.BRT.plot <- 
  ggplot(data = SPL.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.x.axis.title +
	theme(axis.title.y = element_text(size = 14, colour = "white")) +
  ggtitle("(c) SPL")

## TPL BRT plot
TPL.BRT.plot <- 
  ggplot(data = TPL.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.y.axis.labels +
  ggtitle("(f) TPL")

## UMW BRT plot
UMW.BRT.plot <- 
  ggplot(data = UMW.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.all.axis.labels +
  ggtitle("(d) UMW")

## WMT BRT plot
WMT.BRT.plot <- 
  ggplot(data = WMT.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
	scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
  remove.all.axis.labels +
	theme(legend.position = c(0.65, 0.775),
				legend.title = element_blank(),
				axis.text.y   = element_text(size = 12, colour = "gray40")) +
  ggtitle("(a) WMT")

## XER BRT plot
XER.BRT.plot <- 
  ggplot(data = XER.BRT, 
         aes(x = predictors, y = influence, fill = predictor.type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 5, linetype = 3, size = 1, colour = "gray60") +
  scale_fill_manual(values = viridis.3) +
  coord_flip() +
  labs(x = "Predictor", y = "Relative Influence") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40)) +
	scale_x_discrete(limits = c("site.centrality", 
															"mean.annual.flow", "basin.area",
															"site.long", "site.lat",
															"pct.ISC", "pct.urb", "pct.ag", "pct.for",
															"ALG.cover", "NAT.cover", "LWD.reach",
															"DOC", "cond", "pH.lab", "total.P", "NH4", "NO3"),
									 labels = c("site.centrality" = "Cent", 
									 					  "mean.annual.flow" = "Flow", "basin.area" = "Area",
									 					  "site.long" = "Long", "site.lat" = "Lat",
									 					  "pct.ISC" = "ISC", "pct.urb" = "Urb", "pct.ag" = "Ag", "pct.for" = "For",
									 					  "ALG.cover" = "Alg", "NAT.cover" = "Nat", "LWD.reach" = "LWD",
									 					  "DOC" = "DOC", "cond" = "Cond", "pH.lab" = "pH", 
									 					  "total.P" = "TP", "NH4" = expression(NH[4]), "NO3" = expression(NO[3]))) +
	remove.x.axis.labels +
	theme(axis.title.y   = element_text(size = 14, colour = "gray0")) +
  ggtitle("(b) XER")

## Export the figure
ggsave("Figure_5-ecoregions_BRTs.pdf", 
			 plot = grid.arrange(WMT.BRT.plot, UMW.BRT.plot, NAP.BRT.plot,
			 										XER.BRT.plot, NPL.BRT.plot, SAP.BRT.plot,
			 										SPL.BRT.plot, TPL.BRT.plot, CPL.BRT.plot,
			 										nrow = 3, ncol = 3), 
			 width = 12, height = 12,
			 units = "in", device = "pdf", dpi = 1000)


#=========================================================================#
#                             End of Script                               #
#=========================================================================#
