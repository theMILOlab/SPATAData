


# organs as vector --------------------------------------------------------
male_organs <- 
  gganatogram::hgMale_key %>% 
  dplyr::pull(organ) %>% 
  base::sort()

female_organs <- 
  gganatogram::hgFemale_key %>% 
  dplyr::pull(organ) %>% 
  base::sort()

all_organs <- 
  base::unique(c(female_organs, male_organs)) %>% 
  confuns::vselect(-dplyr::ends_with("Cortex")) %>% # belongs to anatomical region
  confuns::make_pretty_names()



# passwords ---------------------------------------------------------------


password_list <- base::readRDS(file = "data_private/passwords.RDS")



# variables for selectize input -------------------------------------------

variable_info <- list(
  age = c("The age of the patient (binned)."), 
  anatomical_region = c("Additional information when it comes to specifying the localisation of the tissue. E.g. organ = brain, anatomical_region = frontal_lobe"), 
  assay_type = c("The assay type of the sample."), 
  citation = c("How to cite the sample."),
  date_added = c("The day when the sample was added."),
  hist_abbreviation = c("An abbreviation for the histological classification. Make sure to check out all available options before you add a new one."), 
  hist_classification = c("The histological classification of the tissue. Make sure to check out all available options before you add a new one."), 
  link_image = c("Dropbox link to image with which the sample is presented online."),
  link_pub = c("Web link to publication."),
  link_raw = c("Dropbox link to raw dataset."), 
  link_spata = c("Dropbox link to spata object."),
  organ = c("The name of the macroscopic organ to which the sample belongs. The input is limited to preselected options. Use 'anatomical_region', 'hist_classification' or 'tags' for additional input."),
  organization = c("The organization that provided the sample. Make sure to check out all available options before you add a new one."), 
  pathology = c("Main pathology classification such as 'tumor', 'inflamed', 'trauma' etc. Use variable 'tags' for further description."),
  patient_id = c("A unique ID that identies to donor. One patient can be donor of multiple samples. Make sure to check out all available options before you add a new one."), 
  sample = c("A unique ID that identifies the sample itself. Must NOT be equal to the suggested options. This is just a helper to quickly find a name that is not already in use."), 
  species = c("The species of the donor."), 
  stage = c("Only relevant in case of tumors. 0 = de_novo, 1 = first recurrency, 2 = second recurrency etc."), 
  status = c("Either 'h' if no pathology was added or 'p' if sample is pathological."),
  tags = c("A collection of tags to further describe the sample this can include e.g. further pathology classifications ('infiltrated', 'inflamed'),
           or specific mutations. Make sure to check out all available options before you add a new one. Tags must not contain '|'. The string ':' is used to declare
           description of the tag. E.g. IDH_Mutation:wild_type is used to create a variable named IDH_Mutation."),
  who_grade = c("Only relevant if 'tumor' or 'Tumor' is added to variable 'tags'. The WHO grade of the tumor.
                Must be provided in roman style with no empty space between number and suffix. E.g. IV, III, IIa")
)

all_variables <- c("sample", "patient_id", "age", "stage", "status",
                   "organ", "anatomical_region", "pathology",
                   "hist_classification", "hist_abbreviation", "tags",
                   "organization", "who_grade", "species", 
                   "link_spata", "link_raw", "link_image", "citation", "link_pub",
                   "date_added", "assay_type")

selectize_variables <- c("sample", "age", "patient_id",
                         "organ", "anatomical_region", "pathology", 
                         "hist_classification", "hist_abbreviation", 
                         "organization", "who_grade", "species", "assay_type")

text_variables <- c("link_spata", "link_raw", "link_image")


filter_sample_variables <- c("anatomical_region", "pathology", "hist_classification", "who_grade", 
                             "organization", "assay_type", "tags")


