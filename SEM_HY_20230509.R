library(xlsxjars)  # Load the xlsxjars library for handling Excel files
library(rJava)  # Load the rJava library for using Java programs in R
library(devtools)  # Load the devtools library for developing R packages
library(lavaan)  # Load the lavaan library for structural equation modeling analysis
library(piecewiseSEM)  # Load the piecewiseSEM library for fitting piecewise structural equation models
library(semPlot)  # Load the semPlot library for plotting structural equation models
library(semTools)  # Load the semTools library for SEM tools functions
library(tidySEM)  # Load the tidySEM library for organizing structural equation model results
library(xlsx)  # Load the xlsx library for reading and writing Excel files
library(psych)  # Load the psych library for psychological statistical analysis
library(plspm)  # Load the plspm library for partial least squares path modeling

# Read Excel data
Mydata <- read.xlsx(file="E:\\Rworkplace\\R_SEM\\Huangyou\\huangyou20230509.xlsx",sheetIndex=1)
PCAdata <- data.frame(Mydata$AIR,Mydata$IIR,Mydata$IRI,Mydata$IRII,Mydata$IRIII,Mydata$SIR)
par(mfrow=c(1,1));
fa.parallel(PCAdata,fa="pc",n.iter = 100)  # Perform factor analysis and plot scree plot
pc <- principal(PCAdata,nfactors = 2,rotate = "none",scores = TRUE)  # Perform principal component analysis
pc

# Standardize the variables
Mydata$AIR <- (Mydata$AIR-min(Mydata$AIR))/(max(Mydata$AIR)-min(Mydata$AIR))
Mydata$SIR <- (Mydata$SIR-min(Mydata$SIR))/(max(Mydata$SIR)-min(Mydata$SIR))
Mydata$IRIII <- (Mydata$IRIII-min(Mydata$IRIII))/(max(Mydata$IRIII)-min(Mydata$IRIII))
Mydata$IIR <- (Mydata$IIR-min(Mydata$IIR))/(max(Mydata$IIR)-min(Mydata$IIR))
Mydata$IRI <- (Mydata$IRI-min(Mydata$IRI))/(max(Mydata$IRI)-min(Mydata$IRI))
Mydata$IRII <- (Mydata$IRII-min(Mydata$IRII))/(max(Mydata$IRII)-min(Mydata$IRII))
Mydata$Broot60 <- (Mydata$Broot60-min(Mydata$Broot60))/(max(Mydata$Broot60)-min(Mydata$Broot60))
Mydata$Sroot60 <- (Mydata$Sroot60-min(Mydata$Sroot60))/(max(Mydata$Sroot60)-min(Mydata$Sroot60))
Mydata$MBD60 <- (Mydata$MBD60-min(Mydata$MBD60))/(max(Mydata$MBD60)-min(Mydata$MBD60))
Mydata$MSOM60 <- (Mydata$MSOM60-min(Mydata$MSOM60))/(max(Mydata$MSOM60)-min(Mydata$MSOM60))
Mydata$MBSA60 <- (Mydata$MBSA60-min(Mydata$MBSA60))/(max(Mydata$MBSA60)-min(Mydata$MBSA60))
Mydata$MSP60 <- (Mydata$MSP60-min(Mydata$MSP60))/(max(Mydata$MSP60)-min(Mydata$MSP60))
i1=10
i2=1.5

# AIR model
AIRformula ='
# regressions
AIR ~ MSOM60+MSP60+Sroot60
SIR ~ MSOM60+MSP60+Sroot60
IRIII ~ MSOM60+MSP60+Sroot60
Sroot60 ~~ MSP60
'
AIRsem <-sem(AIRformula, data = Mydata)  # Fit the structural equation model
semPaths(AIRsem,what="std", layout="tree2", style="lisrel",nCharNodes=0,
         nCharEdges=0,title=TRUE,fade=FALSE,sizeMan = i1, edge.label.cex = i2)  # Plot SEM path diagram
summary(AIRsem,standardized=TRUE)  # Display the summary of the SEM model
fitmeasures(AIRsem,c('chisq', 'df', 'pvalue',"cfi","rmsea",'srmr', 'AIC',"bic","rmsea.ci.upper"))  # Calculate fit indices for the SEM model
parameterEstimates(AIRsem, ci = TRUE, boot.ci.type = "norm",standardized = TRUE)  # Calculate parameter estimates and their confidence intervals for the SEM model
modindices(AIRsem, standardized = TRUE)  # Check modification indices for the SEM model
fitmeasures(AIRsem)

# IIR model
Mydata$Broot20 <- (Mydata$Broot20-min(Mydata$Broot20))/(max(Mydata$Broot20)-min(Mydata$Broot20))
Mydata$MBD20 <- (Mydata$MBD20-min(Mydata$MBD20))/(max(Mydata$MBD20)-min(Mydata$MBD20))
Mydata$MSOM20 <- (Mydata$MSOM20-min(Mydata$MSOM20))/(max(Mydata$MSOM20)-min(Mydata$MSOM20))
Mydata$SWC20 <- (Mydata$SWC20-min(Mydata$SWC20))/(max(Mydata$SWC20)-min(Mydata$SWC20))
Mydata$MBSA20 <- (Mydata$MBSA20-min(Mydata$MBSA20))/(max(Mydata$MBSA20)-min(Mydata$MBSA20))
Mydata$MSP20 <- (Mydata$MSP60-min(Mydata$MSP20))/(max(Mydata$MSP20)-min(Mydata$MSP20))
i1 <- 10
i2 <- 1.5
IIRformula <- '
IIR ~ MSOM20+MSP20+Sroot20
IRI ~ MSP20+Sroot20+MSOM20
IRII ~ MSP20+Sroot20+MSOM20
MSP20 ~~ Sroot20
'
IIRsem <- cfa(IIRformula, data = Mydata)  # Fit the structural equation model
summary(IIRsem, standardize = T, rsq = T)  # Display the summary of the SEM model
fitmeasures(IIRsem,c('chisq', 'df', 'pvalue',"cfi","rmsea",'srmr', 'AIC',"bic","rmsea.ci.upper"))  # Calculate fit indices for the SEM model
semPaths(IIRsem,what="std", layout="tree", style="lisrel",nCharNodes=0,nCharEdges=0,title=TRUE,fade=FALSE,sizeMan = i1, edge.label.cex = i2)  # Plot SEM path diagram
parameterEstimates(IIRsem, ci = TRUE, boot.ci.type = "perc",standardized = TRUE)  # Calculate parameter estimates and their confidence intervals for the SEM model
modindices(IIRsem, standardized = TRUE)  # Check modification indices for the SEM model
fitmeasures(IIRsem)
summary(AIRsem, standardize = T, rsq = T)  # Display summary information of the AIR model
summary(IIRsem, standardize = T, rsq = T)  # Display summary information of the IIR model
i1 <- 12
i2 <- 2
par(mfrow=c(1,2));
semPaths(AIRsem,what="std", layout="tree", style="lisrel",nCharNodes=0,nCharEdges=0,title=TRUE,fade=FALSE,sizeMan = i1, edge.label.cex = i2)  # Plot the path diagram of the AIR model
semPaths(IIRsem,what="std", layout="tree", style="lisrel",nCharNodes=0,nCharEdges=0,title=TRUE,fade=FALSE,sizeMan = i1, edge.label.cex = i2)  # Plot the path diagram of the IIR model
