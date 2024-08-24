
######---------- script to populate spata2v3 folder ----------#####

# data sets are uploaded in batches according to their "source"
# this script is structured by:
# -> source  -------------------------------------------------------

source("scripts/init_session.R")




# TENxVisiumData ----------------------------------------------------------

#BiocManager::install("HelenaLC/TENxVisiumData")
library(TENxVisiumData)


eh <- ExperimentHub()        # initialize hub instance
q <- query(eh, "TENxVisium") # retrieve 'TENxVisiumData' records

ids <- q$ah_id

nms <- c("ah_id", "title", "dataprovider", "species", "taxonomyid", "description", "maintainer")

out <- list()

for(i in seq_along(ids)){
  
  title <- q$title[[i]]
  id <- q$ah_id[[i]]
  
  out[[title]] <- 
    list(
      id = ids[[i]],
      title = title,
      desc = q$description[[i]],
      species = q$species[[i]],
      dpr = q$dataprovider[[i]],
      spe = eh[[id]]
    )
  
}

obj_names <- map_chr(out, .f = ~.x$title) %>% unname()
obj_names <- 
  str_subset(obj_names, "v3", negate = T) %>% 
  str_subset("BreastCancer", negate = T) %>% 
  str_subset("Colorectal", negate = T) %>% 
  str_subset("Ovarian", negate = T)


pdf(file = "spata2v3_objects/TENxVisiumData/sample_overview.pdf")
for(i in seq_along(obj_names)){
  
  obj_name <- obj_names[i]
  
  lst <- out[[obj_name]]
  
  spe_all <- out[[obj_name]]$spe
  
  img_df <- imgData(spe_all)
  
  print(img_df)
  
  for(s_id in img_df$sample_id){
    
    # assemble object
    print(s_id)
    spe <- spe_all[, spe_all$sample_id == s_id]
    
    count_mtr <- spe@assays@data$counts
    
    cdf <- colData(spe) 
    
    coords_df <- 
      spatialCoords(spe) %>% 
      as.data.frame() %>% 
      rownames_to_column("barcodes") %>% 
      as_tibble() %>% 
      dplyr::rename(x_orig = pxl_col_in_fullres, y_orig = pxl_row_in_fullres)
    
    coords_df$barcodes <- str_replace(coords_df$barcodes, pattern = "\\.", replacement = "-")
    
    isf <- img_df[img_df$sample_id == s_id,]$scaleFactor
    
    image_obj <- getImg(spe)
    
    image <- 
      image_obj@image %>% 
      as.matrix() %>% 
      EBImage::as.Image() 
    
    if(base::any(coords_df$barcodes %in% visium_spots$VisiumSmall$barcode)){
      
      method <- "VisiumSmall"
      
    } else if(base::any(coords_df$barcodes %in% visium_spots$VisiumLarge$barcode)){
      
      method <- "VisiumLarge"
      
    }
    
    object <- 
      initiateSpataObject(
        sample_name = s_id, 
        coords_df = coords_df, 
        count_mtr = count_mtr, 
        modality = "gene", 
        spatial_method = method, 
        img = image, 
        img_name = "lowres",
        scale_factors = list(image = isf)
      )
    
    # add meta data
    meta_data <- list()
    meta_data$institution <- "10X Genomics"
    meta_data$donor_species <- ifelse(str_detect(obj_name, "Human"), "Homo sapiens", "Mus musculus")
    
    if(str_detect(obj_name, "HumanCerebellum")){
      
      meta_data$organ <- "Cerebellum"
      meta_data$histo_class = "Cortex"
      
    } else if(obj_name == "HumanLymphNode"){
      
      meta_data$organ <- "Lymph Node"
      meta_data$histo_class = "Lymph Node"
      
    } else if(str_detect(obj_name, "Glioblastoma")){
      
      meta_data$organ <- "Cerebrum"
      meta_data$pathology <- "tumor"
      meta_data$histo_class = "Glioblastoma"
      meta_data$grade <- "IV"
      
    } else if(obj_name == "MouseKidneyCoronal"){
      
      meta_data$organ <- "Kidney"
      meta_data$histo_class = "Kidney"
      
    } else if(obj_name == "HumanHeart"){
      
      meta_data$organ <- "Heart"
      meta_data$histo_class <- "Cardiac Muscle"
      
    } else if(obj_name == "HumanSpinalCord"){
      
      meta_data$organ <- "Spinal Cord"
      meta_data$histo_class <- "Spinal Cord"
      
    } else if(str_detect(obj_name, "Brain")){
      
      meta_data$organ <- "Brain"
      
    }
      

      

    meta_data$platform <- method
    
    # save object
    file_name <- str_c(s_id, ".RDS")
    
    dir <- file.path("spata2v3_objects", "TENxVisiumData", file_name)
    
    object <- setSpataDir(object, dir = dir)
    
    object <- addSampleMetaData(object, meta_data = meta_data)
    
    object <- computePixelScaleFactor(object)
    object <- identifyTissueOutline(object)
    
    print(object)
    print(dir)
    
    saveRDS(object, dir)
    
    p_overview <- 
      (plotSurface(object, pt_alpha = 0) + labs(subtitle = object@sample)) +
      (plotSurface(object, color_by = "tissue_section") + legendBottom())
    
    plot(p_overview)
    
  }
  
}
dev.off()


# Kuppe et al. 2022 -------------------------------------------------------

dir.create("spata2v3_objects/Kuppe_et_al_2022")

heart_dirs <- 
  list.files(path = "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Kuppe_et_al_2022", full.names = T) %>% 
  str_subset(pattern = "\\.", negate = T) %>% 
  file.path("outs")

sample_names <- 
  list.files(path = "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Kuppe_et_al_2022") %>% 
  str_subset(pattern = "\\.", negate = T)

mdf <- readRDS("/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Kuppe_et_al_2022/meta_df_processed.RDS")
mdf$major_label <- mdf$major_labl

labels <- c("FZ" = "fibrotic_zone", "IZ" = "ischaemic_zone", "BZ" = "border_zone", "CTRL" = "control", "RZ" = "remote_zone")

pdf(file = "spata2v3_objects/Kuppe_et_al_2022/sample_overview.pdf")
for(i in seq_along(heart_dirs)){
  
  sample_name <- sample_names[i]
  dir <- heart_dirs[i]
  
  object <- 
    initiateSpataObjectVisium(
      sample_name = sample_name, 
      directory_visium = dir
    )
  
  # add meta data
  mdf_sub <- filter(mdf, hca_sample_id == {{sample_name}})
  
  meta_data <- list()
  meta_data$donor_id <- mdf_sub$patient
  meta_data$donor_species <- "Homo sapiens"
  meta_data$institution <- "University Clinic Aachen"
  meta_data$organ <- "Heart"
  
  meta_data$histo_class = "Cardiac Muscle"
  meta_data$histo_class_sub = labels[mdf$major_label[i]]
  
  meta_data$pathology <- "myocardial_infarction"
  meta_data$pub_citation <- 
    "Kuppe, C., Ramirez Flores, R.O., Li, Z. et al. Spatial multi-omic map of human myocardial infarction. Nature 608, 766–777 (2022). https://doi.org/10.1038/s41586-022-05060-x"
  meta_data$pub_doi <- "doi.org/10.1038/s41586-022-05060-x"
  meta_data$pub_journal <- "Nature"
  meta_data$pub_year <- 2022
  
  meta_data$patient_group <- mdf_sub$patient_group
  meta_data$patient_region_id <- mdf_sub$patient_region_id
  meta_data$tags <- c(mdf$major_label[i])
  
  meta_data$platform <- "VisiumSmall"
  
  meta_data$workgroup <- "KuppeLab"
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  # save object
  file_name <- str_c(sample_name, ".RDS")
  dir <- file.path("spata2v3_objects", "Kuppe_et_al_2022", file_name)
  
  object <- setSpataDir(object, dir = dir)
  
  print(object)
  print(dir)
  
  saveRDS(object, dir)
  
  # plot 
  p_overview <- 
    (plotSurface(object, pt_alpha = 0) + labs(subtitle = object@sample)) +
    (plotSurface(object, color_by = "tissue_section") + legendBottom())
  
  plot(p_overview)
  
}
dev.off()


# Ravi et al. 2022 --------------------------------------------------------

dir.create("spata2v3_objects/Ravi_et_al_2022")

mdf <- readRDS("/Users/heilandr/lab/data/spatial_seq/meta/spatial_seq_meta.RDS")

patient_ids <- 
  c('#275UKF', '#270UKF', '#269UKF', '#268UKF', '#266UKF', '#265UKF', '#265UKF', '#262UKF', '#260UKF', '#259UKF',
    '#259UKF', '#256UKF', '#256UKF', '#255UKF', '#251UKF', '#248UKF', '#248UKF', '#243UKF', '#242UKF', '#242UKF',
    '#241UKF', '#296UKF', '#304UKF', '#313UKF', '#313UKF', '#334UKF', '#334UKF', '#212UKF', '#217UKF', '#218UKF') 

patient_ids_new <- 
  str_replace(patient_ids, "#", "UKF") %>% 
  str_remove("UKF$")

ravi_dirs <- 
  list_raw_visium() %>% 
  SPATA2::vselect(contains(patient_ids_new))

pdf("spata2v3_objects/Ravi_et_al_2022/sample_overview.pdf")
for(i in 1:(nrow(source_df)-1)){
  
  old_id <- patient_ids[i]
  new_id <- patient_ids_new[i]
  
  raw_dirs <-
    list_raw_visium() %>%
    vselect(contains(new_id))  
  
  for(raw_dir in raw_dirs){
    
    suffix <- 
      confuns::str_extract_after(raw_dir, pattern = new_id ) %>% 
      str_remove_all(pattern = "_") %>% 
      str_remove(pattern = "P$")
    
    sample_name <- str_c(new_id, suffix)
    
    object <- 
      initiateSpataObjectVisium(
        sample_name = sample_name, 
        directory_visium = file.path(raw_dir, "outs")
        )
    
    sname <- sample_name
    
    # add meta data
    smdf <- source_df_v2[i, ]
    
    mdf_sub <- filter(mdf, Samples_ID == smdf$sample)
    
    meta_data <- list()
    meta_data$donor_id <- new_id
    meta_data$donor_species <- "Homo sapiens"
    meta_data$grade <- ifelse(str_detect(sname, "T"), smdf$who_grade, NA_character_)
    meta_data$histo_class <- ifelse(str_detect(sname, "C"), "Cortex", smdf$hist_classification)
    meta_data$institution <- "University Clinic Freiburg"
    meta_data$organ <- "Cerebrum"
    meta_data$organ_part <- smdf$anatomical_region
    meta_data$pathology <- ifelse(str_detect(sname, "T"), "tumor", "infiltrated")
    meta_data$platform <- getSpatialMethod(object)@name
    meta_data$pub_citation <- 
      "VM Ravi, P Will, J Kueckelhaus, et al. Spatially resolved multi-omics deciphers bidirectional tumor-host interdependence in glioblastoma. Cancer Cell. 2022 Jun 13;40(6):639-655.e13. doi: 10.1016/j.ccell.2022.05.009. PMID: 35700707."
    meta_data$pub_doi <- "10.1016/j.ccell.2022.05.009"
    meta_data$pub_journal <- "Cancer Cell"
    meta_data$pub_year <- 2022
    meta_data$tags <- c("brain", "cns", "cancer")
    meta_data$workgroup <- "MILOlab"
    
    if(nrow(mdf_sub) != 0){
      
      meta_data$age <- as.numeric(mdf_sub$Age_atSampling) %>% round(digits = 0)
      
    }
    
    object <- addSampleMetaData(object, meta_data = meta_data)
    
    # save object
    file_name <- str_c(sample_name, ".RDS")
    dir <- file.path("spata2v3_objects", "Ravi_et_al_2022", file_name)
    
    object <- setSpataDir(object, dir = dir)
    
    print(object)
    print(dir)
    
    saveRDS(object, dir)
    
    p_overview <- 
      (plotSurface(object, pt_alpha = 0) + labs(subtitle = object@sample)) +
      (plotSurface(object, color_by = "tissue_section") + legendBottom())
    
    plot(p_overview)
    
  }
  
}
dev.off()

# 10X Visium example data sets --------------------------------------------

dir.create("spata2v3_objects/10X_example_data_sets")

example_dirs <- 
  list.files(path = "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/10X_example_data_sets", full.names = T) %>% 
  str_subset(pattern = ".RDS", negate = T) %>% 
  str_subset(pattern = "targeted", negate = T)

mdf <- readRDS("/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/10X_example_data_sets/meta_df.RDS")

pdf(file = "spata2v3_objects/10X_example_data_sets/sample_overview.pdf")
for(ed in example_dirs){
  
  sample_name <- 
    confuns::str_extract_after(ed, pattern = "10X_example_data_sets\\/") %>% 
    str_replace_all(pattern = "-", replacement = "_")
  
  object <- 
    initiateSpataObjectVisium(
      sample_name = sample_name, 
      directory_visium = ed
    )
  
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$institution <- "10X Genomics"
  
  if(str_detect(sample_name, "Breast")){
    
    meta_data$organ <- "Breast"
    
  } else if(str_detect(sample_name, "Colorectal")){
    
    meta_data$organ <- "Colon"
    
  } else if(str_detect(sample_name, "Lung")){
    
    meta_data$organ <- "Lung"
    
  } else if(str_detect(sample_name, "Prostate")){
    
    meta_data$organ <- "Prostate"
    
  } else if(str_detect(sample_name, "Ovarian")){
    
    meta_data$organ <- "Ovary"
    
  }
  
  meta_data$pathology <- "tumor"
  meta_data$platform <- getSpatialMethod(object)@name
  meta_data$tags <- c("cancer")
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  file_name <- str_c(sample_name, ".RDS")
  dir <- file.path("spata2v3_objects", "10X_example_data_sets", file_name)
  
  object <- setSpataDir(object, dir = dir)
  
  print(object)
  print(dir)
  
  saveRDS(object, dir)
  
  p_overview <- 
    (plotSurface(object, pt_alpha = 0) + labs(subtitle = object@sample)) +
    (plotSurface(object, color_by = "tissue_section") + legendBottom())
  
  plot(p_overview)
  
}
dev.off()



# 10X Visium example data sets VisiumLarge --------------------------------

subfolder <- "10X_example_data_sets_VisiumLarge"
create_subfolder(subfolder)

dir_main <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/10X_example_data_sets_VisiumLarge"

all_folders <- list.files(dir_main, full.names = T) 

organs <- c("Colon", "Cerebrum", "Kidney", "Lung", "Ovary")
pathology <- c("tumor", "tumor", NA, "tumor", "tumor")

open_overview_pdf(subfolder)
for(i in seq_along(all_folders)){
  
  folder <- all_folders[i]
  sample_name <- confuns::str_extract_after(folder, "VisiumLarge\\/")
  
  object <- 
    initiateSpataObjectVisium(
      directory_visium = folder, 
      sample_name = sample_name
    )
  
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$institution <- "10X Genomics"
  meta_data$organ <- organs[i]
  meta_data$pathology <- pathology[i]
  meta_data$platform <- "VisiumLarge"
  
  if(meta_data$organ == "Cerebrum"){
    
    meta_data$histo_class <- "Glioblastoma"
    
  }
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  object <- setSpataDir(object, dir)
  
  saveSpataObject(object)
  
  plot_overview(object)
  
}
dev.off()

# 10X Visium example data sets GP -----------------------------------------
subfolder <- "10X_example_data_sets_GP"
create_subfolder(subfolder)

main_dir <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/10X_example_data_sets_GeneAndProtein"

all_folders <- list.files(main_dir, full.names = T)
organs <- c("Breast", "Cerebrum", "Tonsil")
histo_class <- c(NA, "Glioblastoma", NA)

open_overview_pdf(subfolder)
for(i in seq_along(all_folders)){
  
  folder <- all_folders[i]
  
  sample_name <- confuns::str_extract_after(folder, "GeneAndProtein\\/")
  
  sample_name <- stringr::str_c(sample_name, "GP")
  
  object <- initiateSpataObjectVisium(sample_name, directory_visium = folder, img_ref = "hires")
  object <- loadImages(object)
  
  # add meta data
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$grade <- ifelse(organs[i] == "Cerebrum", "IV", NA_character_)
  meta_data$histo_class <- histo_class[i]
  meta_data$institution <- "10X Genomics"
  meta_data$organ <- organs[i]
  meta_data$pathology <- c("tumor", "tumor", NA)[i]
  meta_data$platform <- object@platform
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  saveSpataObject(object, dir = dir)
  
  plot_overview(object)
  
}
dev.off()


# 10X example data sets Visium HD -----------------------------------------

subfolder <- "10X_example_data_sets_VHD"
create_subfolder(subfolder)

main_dir <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisiumHD/10X_example_data_sets_VHD"

all_folders <- list.files(main_dir, full.names = T)

open_overview_pdf(subfolder)
for(i in seq_along(all_folders)){
  
  folder <- all_folders[i]
  sample_name <- confuns::str_extract_after(folder, "10X_example_data_sets_VHD\\/")
  
  object <- 
    initiateSpataObjectVisiumHD(
      directory_visium = folder, 
      sample_name = sample_name, 
      img_ref = "hires", 
      img_active = "lowres"
    )
  
  object <- loadImages(object)
  
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$organ <- c("Lung", "Pancreas")[i]
  meta_data$institution <- "10X Genomics"
  meta_data$pathology <- c("tumor", NA)[i]
  meta_data$platform = "VisiumHD"
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  object <- setSpataDir(object, dir)
  
  saveSpataObject(object)
  
  plot_overview(object)
  
}
dev.off()

# Regal et al. 2023 -------------------------------------------------------

dir.create("spata2v3_objects/Regal_et_al_2023")

dirs_spata <- list.files("/Users/heilandr/lab/data/spatial_seq/proc/spata_objects/SPATA2v1_objects", full.names = T)

age <- c(10, 16, 17, 20)
loc <- c(0, 2, 2, 2)
sex <- c("male", "male", "female", "male")
cd34 <- c("neg", rep("pos", 3))

pdf("spata2v3_objects/Regal_et_al_2023/sample_overview.pdf")
for(i in seq_along(dirs_spata)){
  
  sd <- dirs_spata[i]
  
  sobj <- readRDS(sd)
  
  count_mtr <- sobj@data[[1]]$counts
  img <- sobj@images[[1]]@image %>% EBImage::flip()
  coords_df <- sobj@images[[1]]@coordinates[,c("barcodes", "x", "y")]
  sample_name <- str_c("GG_Extern_", i)
  
  object <- 
    initiateSpataObject(
      sample_name = sample_name, 
      count_mtr = count_mtr, 
      coords_df = coords_df, 
      modality = "gene", 
      img = img, 
      img_name = "lowres", 
      scale_factors = list(image = 1), 
      spatial_method = "VisiumSmall"
    )
  
  object <- computePixelScaleFactor(object)
  
  object <- identifyTissueOutline(object)
  
  # add sample meta data 
  meta_data <- list()
  meta_data$pub_citation <- 
    "Regal, J.A., Guerra García, M.E., Jain, V. et al. Ganglioglioma deep transcriptomics reveals primitive neuroectoderm neural precursor-like population.
  acta neuropathol commun 11, 50 (2023). https://doi.org/10.1186/s40478-023-01548-3"
  meta_data$pub_doi <- "https://doi.org/10.1186/s40478-023-01548-3"
  meta_data$pub_journal <- "Acta neuropathologica"
  meta_data$pub_year <- 2023
  meta_data$institution <- "Duke University"
  meta_data$workgroup <- "ReitmanLab"
  meta_data$source <- "Regal_et_al_2023"
  
  meta_data$organ <- ifelse(i == 1, "Brainstem", "Cerebrum")
  if(i > 1){
    
    meta_data$organ_part <- "temporal"
    
  }
  meta_data$histo_class <- "Ganglioglioma"
  meta_data$grade <- "I"
  
  meta_data$tissue_age <- age[i]
  
  meta_data$platform <- "VisiumSmall"
  
  meta_data$donor_species <- "Homo sapiens"
  meta_data$sex <- sex[i]
  meta_data$tags <- c(str_c("CD34_", cd34[i]))
  
  meta_data$pathology <- "tumor"
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  # save object
  file_name <- str_c(sample_name, ".RDS")
  dir <- file.path("spata2v3_objects", "Regal_et_al_2023", file_name)
  
  object <- setSpataDir(object, dir = dir)
  
  print(object)
  print(dir)
  
  saveRDS(object, dir)
  
  p_overview <- 
    (plotSurface(object, pt_alpha = 0) + labs(subtitle = object@sample)) +
    (plotSurface(object, color_by = "tissue_section") + legendBottom())
  
  plot(p_overview)
  
}
dev.off()


# Kueckelhaus et al. 2024 -------------------------------------------------

dir.create("spata2v3_objects/Kueckelhaus_et_al_2024")

# uplaod mouse brain data set

obj <- readRDS("/Users/heilandr/lab/projects/SPATA2/review/data/mc_damaged.RDS")
obj@version$major <- 2
obj@version$minor <- 4

object <- updateSpataObject(obj)

meta_data <- list()

meta_data$comment <- "Injury annotations present as ImageAnotations."

meta_data$donor_species <- "Mus musculus"
meta_data$organ <- "Brain"
meta_data$pathology <- "stab_wound"

meta_data$pub_citation <- ""
meta_data$pub_journal <- "Nature Communications"
meta_data$platform <- "VisiumSmall"

meta_data$institution <- "Ludwig Maximilian University of Munich"
meta_data$workgroup <- "DichgansLab"

meta_data$sex <- "male"
meta_data$source <- "Kueckelhaus_et_al_2024"

object <- addSampleMetaData(object, meta_data = meta_data)

object <- renameSpataObject(object, sample_name = "LMU_MCI")

dir <- "spata2v3_objects/Kueckelhaus_et_al_2024/LMU_MCI.RDS"
object <- setSpataDir(object, dir = dir)

object <- setDefault(object, display_image = T)

object <- identifyTissueOutline(object)

saveSpataObject(object)

# Greenwald et al. 2024 ---------------------------------------------------
library(Seurat)
library(SPATA2)
library(stringr)
library(jsonlite)
library(tidyverse)
library(confuns)

subfolder <- "Greenwald_et_al_2024"
create_subfolder(subfolder)

citation <- readRDS("/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Greenwald_et_al_2024/citation.RDS")

# process meta
meta_df <-  
  read_csv("/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Greenwald_et_al_2024/meta_data_frame.csv")
colnames(meta_df) <- tolower(colnames(meta_df)) %>% str_replace_all("-", "_")

meta_df <- 
 tidyr::separate(meta_df, col = location, into = c("organ_side", "organ_part")) 

meta_df <- meta_df[1:21, ]

meta_df$organ_part[meta_df$organ_side == "bifrontal"] <- "frontal"
meta_df$organ_side[meta_df$organ_side == "bifrontal"] <- "both"

main_dir <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Greenwald_et_al_2024"

all_dirs <- 
  list.files(main_dir, full.names = T) %>%
  str_subset(".csv$", negate = T) %>% 
  str_subset("citation", negate = T)

open_overview_pdf(subfolder)
for(main_dir in all_dirs){
  
  sample_name_prel <- confuns::str_extract_after(main_dir, pattern = "Greenwald_et_al_2024\\/")
  
  sample_name <- str_suggest_vec(sample_name_prel, pool = meta_df$sample_id, max.dist = 10, n.top = 1)
  
  mdf <- filter(meta_df, sample_id == {{sample_name}})
  
  if(nrow(mdf) == 0){
    
    message(glue::glue("No meta data for dir {main_dir}."))
    
  } else {
    
    message(glue::glue("Working on dir {main_dir}."))
    
    # get directories
    all_files <- list.files(main_dir, full.names = T)
    
    dir_mtr <- str_subset(all_files, "\\.h5$")
    dir_coords <- str_subset(all_files, "tissue_positions")
    dir_img <- str_subset(all_files, "lowres_image|hires_image")
    dir_sfs <- str_subset(all_files, "scalefactors")
    
    # counts
    mtr <- Seurat::Read10X_h5(filename = dir_mtr)
    
    # image related
    image <- EBImage::readImage(files = dir_img)
    name_img_ref <- ifelse(str_detect(dir_img, "hires"), "hires", "lowres")
    scale_factors <- read_json(dir_sfs)
    image_sf <- scale_factors[[str_c("tissue_", name_img_ref, "_scalef")]]
    
    # coords 
    if(is_empty(dir_coords)){
      
      coords_df <- visium_spots$VisiumSmall %>% rename(barcodes = barcode)
      coords_df$x_orig <- 0
      coords_df$y_orig <- 0
      
      coords_df <- filter(coords_df, barcodes %in% colnames(mtr))
      
      warning(glue::glue("no coords for sample {sample_name}"))
      
    } else {
      
      coords_df <- 
        read_coords_visium(dir_coords) %>% 
        dplyr::filter(in_tissue == 1)
      
    }
    
    # initiate object
    object <- 
      initiateSpataObject(
        sample_name = sample_name, 
        count_mtr = mtr, 
        coords_df = coords_df, 
        modality = "gene",
        img = image, 
        img_name = name_img_ref, 
        scale_factors = list(image = image_sf), 
        spatial_method = VisiumSmall
      )
    
    # visium specifics
    object <- computePixelScaleFactor(object)
    object <- identifyTissueOutline(object)
    
    spot_size <-
      scale_factors$fiducial_diameter_fullres *
      scale_factors[[stringr::str_c("tissue", name_img_ref, "scalef", sep = "_")]] /
      base::max(dim(image))*100
    
    spot_scale_fct <- 1.15
    
    sp_data <- getSpatialData(object)
    sp_data@method@method_specifics[["spot_size"]] <- spot_size * spot_scale_fct
    object <- setDefault(object, pt_size = spot_size*spot_scale_fct)
    object <- setSpatialData(object, sp_data)
    
    # add meta data
    meta_data <- list()
    meta_data$donor_id <- mdf$patient_id
    meta_data$sex <- ifelse(mdf$sex == "F", "female", "male")
    meta_data$tissue_age <- mdf$age
    meta_data$donor_species <- "Homo sapiens"
    meta_data$grade <- "IV"
    if(str_detect(sample_name, "ZH")){
      
      meta_data$institution <- "University Hospital Zurich"
      
    } else if(str_detect(sample_name, "MGH")){
      
      meta_data$institution <- "Massachusetts General Hospital"
      
    } else if(str_detect(sample_name, "BWH")){
      
      meta_data$institution <- "Brigham and Women's Hospital"
      
    }
    
    meta_data$organ <- "Cerebrum"
    meta_data$organ_side <- mdf$organ_side
    meta_data$organ_part <- mdf$organ_part
    meta_data$grade <- WHOGrades(mdf$grade)
    meta_data$pathology <- "tumor"
    meta_data$platform <- "VisiumSmall"
    
    if(mdf$histology == "GBM"){
      
      meta_data$histo_class <- "Glioblastoma"
      
    } else if(mdf$histology == "ODG"){
      
      meta_data$histo_class <- "Oligodendroglioma"
      
    } else if(mdf$histology == "AA"){
      
      meta_data$histo_class <- "Astrocytoma"
      
    }
    
    meta_data$pub_citation <- citation
    meta_data$pub_journal <- "Cell"
    meta_data$pub_year <- 2024
    meta_data$workgroup <- "TiroshLab"
    
    meta_data$mgmt_status <- mdf$mgmt_status
    
    meta_data$IDH_mut <- str_detect(mdf$tumor, "IDH-mut")
    
    meta_data$tags <- c("cancer", "brain", "cns")
    
    object <- addSampleMetaData(object, meta_data = meta_data)
    
    # save object
    dir <- file.path("spata2v3_objects", "Greenwald_et_al_2024", str_c(sample_name, ".RDS"))
    object <- setSpataDir(object, dir)
    
    saveSpataObject(object)
    
    # plot
    p_overview <- 
      (plotSurface(object, pt_alpha = 0) + labs(subtitle = object@sample)) +
      (plotSurface(object, color_by = "tissue_section") + legendBottom())
    
    plot(p_overview)
    
  }
  
}
dev.off()

# Martinez-Hernandez_et_al_2024 -------------------------------------------
library(Matrix)


subfolder <- "Martinez_Hernandez_et_al_2024"
create_subfolder(subfolder)



dir_main <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Martinez_Hernandez_et_al_2024"
dir_samples <- 
  list.files(dir_main, full.names = T) %>% 
  str_subset("citation.RDS", negate = T) %>% 
  str_subset("meta_data_frame.csv", negate = T) %>% 
  str_subset("supp_info", negate = T)

citation <- readRDS(file.path(dir_main, "citation.RDS"))

meta_df <- read_csv(file.path(dir_main, "meta_data_frame.csv"))
colnames(meta_df) <- tolower(colnames(meta_df)) %>% str_replace_all("-| ", "_") 

meta_df <- filter(meta_df, !is.na(visium_samples))

open_overview_pdf(subfolder)
for(dir_sample in dir_samples){
  
  sample_name <- str_extract_after(dir_sample, "et_al_2024\\/")
  
  mdf <- filter(meta_df, visium_samples == {{sample_name}})
  
  all_files <- list.files(dir_sample, full.names = T)
  
  # mtr 
  mtr <- read_matrix_mtx(dir_sample)
  
  dir_coords <- str_subset(all_files, "tissue_positions")
  dir_img <- str_subset(all_files, "hires_image")
  dir_sfs <- str_subset(all_files, "scalefactors")
  
  # image related
  image <- EBImage::readImage(files = dir_img)
  name_img_ref <- ifelse(str_detect(dir_img, "hires"), "hires", "lowres")
  scale_factors <- read_json(dir_sfs)
  image_sf <- scale_factors[[str_c("tissue_", name_img_ref, "_scalef")]]
  
  # coords 
  if(is_empty(dir_coords)){
    
    coords_df <- visium_spots$VisiumSmall %>% rename(barcodes = barcode)
    coords_df$x_orig <- 0
    coords_df$y_orig <- 0
    
    coords_df <- filter(coords_df, barcodes %in% colnames(mtr))
    
    warning(glue::glue("no coords for sample {sample_name}"))
    
  } else {
    
    coords_df <- read_coords_visium(dir_coords) %>% filter(in_tissue == 1)
    
  }
  
  object <- 
    initiateSpataObject(
      sample_name = sample_name, 
      count_mtr = mtr, 
      modality = "gene", 
      coords_df = coords_df, 
      img = image, 
      img_name = "hires",
      scale_factors = list(image = image_sf), 
      spatial_method = VisiumSmall
    )
  
  object <- computePixelScaleFactor(object)
  object <- identifyTissueOutline(object)
  
  image_lowres <- EBImage::readImage(str_subset(all_files, "lowres_image"))
  
  object <- 
    registerImage(
      object = object, 
      img = image_lowres,
      img_name = "lowres"
    )
  
  object <- activateImage(object, img_name = "lowres", unload = F)
  
  # add meta data
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$histo_class <- ifelse(str_detect(mdf$pathological_diagnosis, "Healthy"), "Healthy tissue", mdf$pathological_diagnosis)
  meta_data$institution <- "Hospital Universitario la Princesa"
  meta_data$organ <- "Thyroid Gland"
  meta_data$pathology <- ifelse(meta_data$histo_class == "Healthy", NA_character_, "autoimmune")
  
  meta_data$pub_citation <- citation
  meta_data$pub_journal <- "Nature Communications"
  meta_data$pub_year <- 2024
  
  meta_data$tissue_age <- mdf$age
  meta_data$sex <- ifelse(mdf$sex == "F", "female", "male")
  meta_data$workgroup <- "MarazuelaLab"
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  # save object
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  object <- setSpataDir(object, dir)
  saveSpataObject(object)
  
  
  plot_overview(object)
  
}
dev.off()



# Ren et al. 2023 ---------------------------------------------------------

subfolder <- "Ren_et_al_2023"
create_subfolder(subfolder)

main_dir <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Ren_et_al_2023"

meta_df <- readxl::read_xlsx(path = file.path(main_dir, "meta_data_frame.xlsx"))

all_folders <- list.files(main_dir, full.names = T) %>% str_subset(pattern = "out$")

open_overview_pdf(subfolder)
for(folder in all_folders){
  
  sample_name <- 
    str_extract_after(folder, pattern = "Ren_et_al_2023\\/") %>% 
    str_remove(pattern = "_spaceranger_out")
  
  mdf <- filter(meta_df, Sample == {{sample_name}})
  
  object <- 
    initiateSpataObjectVisium(
      sample_name = sample_name, 
      directory_visium = folder, 
      img_ref = "hires", 
      img_active = "lowres"
    )
  
  object <- loadImages(object)
  
  # add meta data
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$grade <- "IV"
  meta_data$histo_class <- mdf$histo_class
  meta_data$comment <- as.character(mdf[["Integrated diagnosis"]])
  meta_data$institution <- "West China Hospital"
  meta_data$organ <- mdf$organ
  meta_data$organ_part <- mdf$organ_part
  meta_data$organ_side <- mdf$side
  meta_data$pathology <- "tumor"
  meta_data$platform <- "VisiumSmall"
  meta_data$pub_citation <- citation
  meta_data$pub_journal <- "Nature Communications"
  meta_data$pub_year <- 2023
  meta_data$sex <- NA
  meta_data$tags <- c("brain;cns;cancer")
  meta_data$tissue_age_approx <- mdf$`Age at diagnosis`
  meta_data$workgroup <- "WangLab"
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  # save object
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  saveSpataObject(object, dir = dir)
  
  plot_overview(object)
  
}
dev.off()



# Valdeolivas et al 2024 --------------------------------------------------
subfolder <- "Valdeolivas_et_al_2024"
create_subfolder(subfolder)

dir_main <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Valdeolivas_et_al_2024"

all_folders <- 
  list.files(dir_main, full.names = T) %>% 
  str_subset(pattern = "meta_data", negate = T) %>% 
  str_subset(pattern = "citation", negate = T)

citation <- readRDS(file.path(dir_main, "citation.RDS"))

meta_df <- readxl::read_xlsx(file.path(dir_main, "meta_data_frame_proc.xlsx"))

annotation_dirs <- 
  list.files(all_folders[1], full.names = T) %>% 
  str_subset(".rds$") %>% 
  str_subset(".csv$", negate = T)

open_overview_pdf(subfolder)
for(folder in all_folders[2:length(all_folders)]){
  
  sample_name <- confuns::str_extract_after(folder, "Valdeolivas_et_al_2024\\/")
  
  sn <- 
    str_suggest_vec(str_extract(sample_name, "_.*_") %>% str_remove_all("_"), pool = meta_df$sample_name, max.dist = 20, n.top =1)
  
  mdf <- filter(meta_df, sample_name == {{sn}})
  
  if(nrow(mdf) == 0){
    
    warning(glue::glue("no meta data for dir {folder}."))
    
  }
  
  object <- 
    initiateSpataObjectVisium(
      sample_name = sample_name, 
      directory_visium = folder, 
      img_ref = "hires", 
      img_active = "lowres"
    )
  
  object <- loadImages(object)
  
  
  # add meta data
  meta_data <- list()
  
  # add pathology annotations
  
  ann_dir <- str_subset(annotation_dirs, sample_name)
  
  if(length(ann_dir) == 1){
    
    df <- 
      readRDS(ann_dir) %>% 
      as_tibble() %>% 
      dplyr::rename(barcodes = spot_id)
    
    object <- addFeatures(object, feature_df = df, feature_names = c("seurat_clusters", "Pathologist_Annotations"))
    object@meta_obs$Pathologist_Annotations <- as.factor(object@meta_obs$Pathologist_Annotations)
    object@obj_info$spat_segm_vars <- "Pathologist_Annotations"
    
    meta_data$comment <- "contains histological segmentation variable"
    
    
  }
  
  meta_data$donor_species <- "Homo sapiens"
  meta_data$histo_class <- mdf$histo_class
  meta_data$institution <- "Ludwig Maximilian University of Munich"
  meta_data$organ <- mdf$organ
  meta_data$organ_part <- mdf$organ_part
  meta_data$pathology <- "tumor"
  meta_data$platform <- "VisiumSmall"
  meta_data$pub_citation <- citation
  meta_data$pub_journal <- "Precision Oncology"
  meta_data$pub_year <- 2024
  meta_data$sex <- NA
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  # add directory
  object <- setSpataDir(object, dir = file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS")))
  
  saveSpataObject(object)
  
}
dev.off()


# Akeret et al. 2022 -------------------------------------------------------

subfolder <- "Akeret_et_al_2022"
create_subfolder(subfolder)

main_dir <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Akeret_et_al_2022"

object <- 
  initiateSpataObjectVisium(sample_name = "MouseBrainHemeInjection", directory_visium = main_dir, img_ref = "hires")

object <- loadImages(object)

inj_df <-
  read_csv(file.path(main_dir, "heme_1000_injection_site.csv")) %>% 
  dplyr::rename(barcodes = Barcode) %>% 
  mutate(injection_site = if_else(is.na(injection_site), "normal", "heme") %>% as.factor())

anatomy_df<- 
  read_csv(file.path(main_dir, "heme_1000_anatomy.csv")) %>% 
  dplyr::rename(barcodes = Barcode) %>% 
  mutate(anatomy = as.factor(anatomy))

object <- addFeatures(object, inj_df)
object <- addFeatures(object, anatomy_df)

getFeatureNames(object)

meta_data <- list()
meta_data$comment <- "contains grouping variables 'injection_site' and 'anatomy' from the original publication"
meta_data$donor_species <- "Mus musculus"
meta_data$institution <- "University Hospital Zurich"
meta_data$organ <- "Brain"
meta_data$pathology <- "bleeding_model"
meta_data$platform <- object@platform
meta_data$pub_citation <- readRDS(file.path(main_dir, "citation.RDS"))
meta_data$workgroup <- "Primelab"

object <- addSampleMetaData(object, meta_data = meta_data)

dir <- file.path("spata2v3_objects", subfolder, str_c(object@sample, ".RDS"))
saveSpataObject(object, dir)




# Maynard et al 2021 ------------------------------------------------------

subfolder <- "Maynard_et_al_2021"
create_subfolder(subfolder)

main_dir <- "/Users/heilandr/lab/data/spatial_seq/raw/10XVisium/Maynard_et_al_2021"

all_folders <- 
  list.files(path = main_dir, full.names = T) %>% 
  str_subset(pattern = "[0-9]$")

open_overview_pdf(subfolder)
for(folder in all_folders){
  
  sample_name <- confuns::str_extract_after(folder, pattern = "Maynard_et_al_2021\\/")
  
  object <- 
    initiateSpataObjectVisium(
      sample_name = sample_name, 
      directory_visium = folder, 
      img_ref = "hires", 
      img_active = "lowres"
    )
  
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$histo_class <- "Cortex"
  meta_data$institution <- "Lieber Institute for Brain Development"
  meta_data$organ <- "Cerebrum"
  meta_data$organ_part <- "prefrontal"
  meta_data$pub_citation <- readRDS(file.path(main_dir, "citation.RDS"))
  meta_data$pub_year <- 2021
  meta_data$pub_journal <- "Nature Neuroscience"
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  
  object <- setSpataDir(object, dir)
  
  saveSpataObject(object)
  
  plot_overview(object)
  
}
dev.off()


# Vizgen MERFISH example data sets --------------------------------

subfolder <- "Vizgen_example_data_sets_Cancer"
create_subfolder(subfolder)

dir_main <- "/Users/heilandr/lab/data/spatial_seq/raw/MERFISH/MERSCOPE-FFPE-Human-Immuno-oncology"

all_folders <- list.files(dir_main, full.names = T) 

organs <- c("Breast", "Colon", "Colon", "Liver", "Liver", "Lung", "Lung", "Skin", "Skin", "Ovary", "Ovary", "Ovary", "Ovary", "Prostate", "Prostate", "Uterus", "Uterus", "Uterus")

open_overview_pdf(subfolder)
for(i in seq_along(all_folders)){
  
  folder <- all_folders[i]
  sample_name <- dplyr::last(stringr::str_split(folder, "/")[[1]])
  
  object <- 
    initiateSpataObjectMERFISH(
      directory_merfish = folder, 
      sample_name = sample_name
    )
  
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$institution <- "Vizgen"
  meta_data$organ <- organs[i]
  meta_data$pathology <- "tumor"
  meta_data$platform <- "MERFISH"
  meta_data$pub_citation <- "Vizgen MERFISH FFPE Human Immuno-oncology Data Set,  May 2022"
  meta_data$download_date <- lubridate::as_datetime("2024/01/09")
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  object <- setSpataDir(object, dir)
  
  saveSpataObject(object)
  
  plot_overview(object)
  
}
dev.off()


subfolder <- "Vizgen_example_data_sets_MouseBrain"
create_subfolder(subfolder)

dir_main <- "/Users/heilandr/lab/data/spatial_seq/raw/MERFISH/MERSCOPE-Mouse-Brain-Receptor-Map"

all_folders <- list.files(dir_main, full.names = T) 

open_overview_pdf(subfolder)
for(i in seq_along(all_folders)){
  
  folder <- all_folders[i]
  sample_name <- dplyr::last(stringr::str_split(folder, "/")[[1]])
  
  object <- 
    initiateSpataObjectMERFISH(
      directory_merfish = folder, 
      sample_name = sample_name
    )
  
  meta_data <- list()
  meta_data$donor_species <- "Mus musculus"
  meta_data$institution <- "Vizgen"
  meta_data$organ <- "Brain"
  meta_data$pathology <- NA_character_
  meta_data$platform <- "MERFISH"
  meta_data$pub_citation <- "Vizgen MERFISH Mouse Brain Receptor Map"
  meta_data$download_date <- lubridate::as_datetime("2024/08/19")
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  object <- setSpataDir(object, dir)
  
  saveSpataObject(object)
  
  plot_overview(object)
  
}
dev.off()


subfolder <- "Vizgen_example_data_sets_MouseLiver"
create_subfolder(subfolder)

dir_main <- "/Users/heilandr/lab/data/spatial_seq/raw/MERFISH/MERSCOPE-Mouse-Liver-Map"

all_folders <- list.files(dir_main, full.names = T) 

open_overview_pdf(subfolder)
for(i in seq_along(all_folders)){
  
  folder <- all_folders[i]
  sample_name <- dplyr::last(stringr::str_split(folder, "/")[[1]])
  
  object <- 
    initiateSpataObjectMERFISH(
      directory_merfish = folder, 
      sample_name = sample_name
    )
  
  meta_data <- list()
  meta_data$donor_species <- "Mus musculus"
  meta_data$institution <- "Vizgen"
  meta_data$organ <- "Liver"
  meta_data$pathology <- NA_character_
  meta_data$platform <- "MERFISH"
  meta_data$download_date <- "2024/08/21"
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  object <- setSpataDir(object, dir)
  
  saveSpataObject(object)
  
  plot_overview(object)
  
}
dev.off()


subfolder <- "Vizgen_example_data_sets_HumanBrain_AD"
create_subfolder(subfolder)

dir_main <- "/Users/heilandr/lab/data/spatial_seq/raw/MERFISH/MERSCOPE-1000-Gene-Panel-Human-Brain-AD"

all_folders <- list.files(dir_main, full.names = T) 

open_overview_pdf(subfolder)
for(i in seq_along(all_folders)){
  
  folder <- all_folders[i]
  sample_name <- dplyr::last(stringr::str_split(folder, "/")[[1]])
  
  object <- 
    initiateSpataObjectMERFISH(
      directory_merfish = folder, 
      sample_name = sample_name
    )
  
  meta_data <- list()
  meta_data$donor_species <- "Homo sapiens"
  meta_data$institution <- "Vizgen"
  meta_data$organ <- "Brain"
  meta_data$organ_part <- "Unspecified"
  meta_data$pathology <- "AD"
  meta_data$platform <- "MERFISH"
  meta_data$download_date <- lubridate::as_datetime("2024/08/21")
  
  object <- addSampleMetaData(object, meta_data = meta_data)
  
  dir <- file.path("spata2v3_objects", subfolder, str_c(sample_name, ".RDS"))
  object <- setSpataDir(object, dir)
  
  saveSpataObject(object)
  
  plot_overview(object)
  
}
dev.off()