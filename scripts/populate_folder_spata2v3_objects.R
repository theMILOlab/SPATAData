
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

iterate <- 
  list(
    organ = c("Cerebellum", "Brain", "Heart", "Lymph Node", "Spinal Cord", "Brain", "Brain", "Brain", "Kidney"), 
    human_species = c("Homo sapiens")
  )


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
      rename(x_orig = pxl_col_in_fullres, y_orig = pxl_row_in_fullres)
    
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
    
    if(obj_name == "HumanCerebellum"){
      
      meta_data$organ <- "Cerebellum"
      meta_data$histo_class = "Cortex"
      
    } else if(obj_name == "HumanLymphNode"){
      
      meta_data$organ <- "Lymph Node"
      meta_data$histo_class = "Lymph Node"
      
    } else if(obj_name == "HumanGliolastoma"){
      
      meta_data$organ <- "Cerebrum"
      meta_data$pathology <- "tumor"
      meta_data$histo_class = "Glioblastoma"
      meta_data$grade <- "IV"
      
    } else if(obj_name == "MouseKidneyCoronal"){
      
      meta_data$organ <- "Kidney"
      meta_data$histo_class = "Kidney"
      
    } else {
      
      meta_data$organ <- "Brain"
      meta_data$histo_class <- "Brain"
      
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

source_df$patient_id %>% str_subset(pattern= "UKF") %>% str_c(collapse = "', '") %>% str_c("c('", ., "')")

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
