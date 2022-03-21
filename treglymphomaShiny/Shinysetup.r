reqPkg = c("data.table", "Matrix", "hdf5r", "ggplot2", "gridExtra",
           "glue", "readr", "RColorBrewer", "R.utils", "Seurat")
newPkg = reqPkg[!(reqPkg %in% installed.packages()[,"Package"])]
if(length(newPkg)){install.packages(newPkg)}
reqPkg = c("shiny", "shinyhelper", "data.table", "Matrix", "hdf5r", 
          "ggplot2", "gridExtra", "magrittr", "ggdendro")
newPkg = reqPkg[!(reqPkg %in% installed.packages()[,"Package"])]
if(length(newPkg)){install.packages(newPkg)}

devtools::install_github("SGDDNB/ShinyCell")


library(Seurat)
library(ShinyCell)


#seu = readRDS("Dropbox (UiO)/Paracetamol_shared/SINGLE_CELL_ANALYSIS/AGGREGATED_ANALYSIS/DIFFERENTIATION/FINAL_MERGED_ANALYSIS_res_0.3/Diff_Merged_Final_EndofPipe_Final_rnaobjects.qcFilt.rds")
CD4T_integrated = readRDS("/Users/ankushs/Dropbox (UiO)/data_visualization/TREGS/Part2_Clustering_datasetfromShinyApp.rds")
remove("CD4T_integrated$old.ident", "CD4T_integrated$SCT.weight", "CD4T_integrated$HTO_classification.global", "CD4T_integrated$HTO_margin","CD4T_integrated$HTO_secondID","CD4T_integrated$SCT.weight")
saveRDS(CD4T_integrated, file = paste0(currentjob,"Part2_Clustering_datasetfromShinyApp_1.rds"))
CD4T_integrated = readRDS("/Users/ankushs/Dropbox (UiO)/data_visualization/TREGS/Part2_Clustering_datasetfromShinyApp_1.rds")

library(Seurat)
library(ShinyCell)
scConf = createConfig(CD4T_integrated)
makeShinyApp(CD4T_integrated, scConf, gene.mapping = TRUE,
             shiny.title = "TREGS") 


