# Code for Final graphs associated with Miranda Chiong's REU project

# Two main graphs for the presentation:
# 1. Establishment years by species or accessioned/unaccessioned
# 2. BAI.inc vs. establishment year by species

library(ggplot2)

path.out <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Analysis-Output"

# Setting up a consistent pallet for our oaks
colorsCommon <- c("sawtooth oak" = "#e69f00", "white oak"= "#56B4e9", "northern pin oak"="#009e73", "bur oak" = "#f0e442", "chestnut oak"="#0072b2", "pin oak" = "#d55e00", "northern red oak"="#cc79a7")

colorsPlanted <- c("Planted"="#f05039", "Unknown"="#3d65a5")


# # # # # # # 
# Load in the data
# # # # # # # 
dfTree <- read.csv(file.path(path.out, "TreeData_toAnalyze.csv"))
dfTree$taxon <- as.factor(dfTree$taxon)
summary(dfTree)

dfTree[dfTree$BAI.first10>1e3 & !is.na(dfTree$BAI.first10),] # finding the outlier that shows up in some graphs

# Creating a simplified genus & species columns for us
dfTree[,c("genus", "sp.epith", "authority")] <- NA
for(i in 1:nrow(dfTree)){
  # Splitting the taxon field into its components
  taxParse <- strsplit(paste(dfTree$taxon[i]), split=" ")[[1]] # Not sure why we need paste today, but so it goes
  dfTree$genus[i] <- taxParse[1]
  if(length(taxParse)>=2) dfTree$sp.epith[i] <- taxParse[2]
  if(length(taxParse)>=3) dfTree$authority[i] <- taxParse[3]
}
dfTree$genus <- as.factor(dfTree$genus)
dfTree$sp.epith <- as.factor(dfTree$sp.epith)
dfTree$authority <- as.factor(dfTree$authority)
summary(dfTree$sp.epith)

# Subsetting to just the trees old enough to analyze
dfTree <- dfTree[!is.na(dfTree$xDate.rho),]
summary(dfTree)

# Checking to see what our species distribution is
summary(dfTree$sp.epith)

# Coding in common names
dfTree$CommonName <- car::recode(dfTree$sp.epith, "'acutissima'='sawtooth oak'; 'alba'='white oak'; 'ellipsoidalis'='northern pin oak'; 'macrocarpa'='bur oak'; 'montana'='chestnut oak'; 'palustris'='pin oak'; 'rubra'='northern red oak'")

summary(dfTree$CommonName)

# Identifying PLANTED vs WILD trees (changing terminology from accessioned vs. unaccessioned)
# We can tell it was planted if it's something like 141-38*4; accessioned trees will always have the *, but some wild-established ones
dfTree$Planted <- as.factor(ifelse(grepl("[*]", dfTree$treeID) & !grepl("U", dfTree$treeID), "Planted", "Unknown"))
summary(as.factor(dfTree$Planted))

head(dfTree[,c("treeID", "Planted")])
# # # # # # # 


# # # # # # # 
# Making the presentation figures
# Events to note:
#  -- 1832: DuPage County Settlement Begins
#  -- 1922: The Morton Arboretum Established
# # # # # # # 
png(file.path(path.out, "PRESENTATION-EstablishmentYears-10yrs_bySpp-PithOnly.png"), height=5.0, width=9.3, units="in", res=360)
ggplot(data=dfTree[!is.na(dfTree$pith.present) & dfTree$pith.present,]) +
  geom_histogram(aes(x=year.first, fill=CommonName), binwidth=10) +
  geom_vline(xintercept=c(1832, 1922)) +
  geom_text(x=1830, y=17, label="DuPage County\nSettlement\nBegins", hjust=1) +
  geom_text(x=1925, y=17, label="The Morton\nArboretum\nEstablished", hjust=0) +
  scale_fill_manual(values=colorsCommon) +
  scale_x_continuous(limits=c(1805, 1980), breaks=seq(1810, 1980, by=20)) +
  labs(fill="Species", x="First Year of Growth", y="count") +
  theme_bw() +
  theme(axis.text=element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.5), color="black", face="bold"),
        legend.text=element_text(size=rel(1.25), color="black"),
        legend.title=element_text(size=rel(1.5), color="black", face="bold"))
dev.off()

png(file.path(path.out, "PRESENTATION-EstablishmentYears-10yrs_byPlanted-PithOnly.png"), height=5.0, width=9.3, units="in", res=360)
ggplot(data=dfTree[!is.na(dfTree$pith.present) & dfTree$pith.present,]) +
  geom_histogram(aes(x=year.first, fill=Planted), binwidth=10) +
  geom_vline(xintercept=c(1832, 1922)) +
  geom_text(x=1830, y=17, label="DuPage County\nSettlement\nBegins", hjust=1) +
  geom_text(x=1925, y=17, label="The Morton\nArboretum\nEstablished", hjust=0) +
  scale_fill_manual(values=colorsPlanted) +
  scale_x_continuous(limits=c(1805, 1980), breaks=seq(1810, 1980, by=20)) +
  labs(fill="Species", x="First Year of Growth", y="count") +
  theme_bw() +
  theme(axis.text=element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.5), color="black", face="bold"),
        legend.text=element_text(size=rel(1.25), color="black"),
        legend.title=element_text(size=rel(1.5), color="black", face="bold"))
dev.off()


png(file.path(path.out, "PRESENTATION-EstablishmentBAI10_vs_Year_bySpp-PithOnly.png"), height=5.0, width=9.3, units="in", res=360)
ggplot(data=dfTree[!is.na(dfTree$pith.present) & dfTree$pith.present,]) +
  geom_point(aes(x=year.first, y=BAI.first10, fill=CommonName), size=5, alpha=0.8, shape=21) +
  geom_vline(xintercept=c(1832, 1922)) +
  geom_text(x=1830, y=1200, label="DuPage County\nSettlement\nBegins", hjust=1) +
  geom_text(x=1925, y=1200, label="The Morton\nArboretum\nEstablished", hjust=0) +
  scale_fill_manual(values=colorsCommon) +
  scale_x_continuous(limits=c(1805, 1980), breaks=seq(1810, 1980, by=20)) +
  labs(fill="Species", x="First Year of Growth", y="Mean Estab Basal Area Inc. (mm^2)") +
  theme_bw() + theme_bw() +
  theme(axis.text=element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.5), color="black", face="bold"),
        legend.text=element_text(size=rel(1.25), color="black"),
        legend.title=element_text(size=rel(1.5), color="black", face="bold"))
dev.off()


# # # # # # # 
