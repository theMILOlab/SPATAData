unnestTags <- function(source_df = sourceDataFrame()){
  
  all_tags <- sourceDataFrameTags(source_df)
  
  col_tags <- stringr::str_subset(all_tags, pattern = ":")
  
  col_names <- str_extract_before(col_tags, pattern = ":")
  
  col_vals <- str_extract_after(col_tags, pattern = ":")
  
  for(i in base::seq_along(col_names)){
    
    col_name <- col_names[i]
    
    tag_pattern <- stringr::str_c("\\|", col_name, "\\:")
    
    tag_cut <- rgx_lookahead(pattern = "\\|")
    
    source_df <- 
      dplyr::mutate(
        .data = source_df, 
        {{col_name}} := dplyr::if_else(
          condition = stringr::str_detect(tags, pattern = tag_pattern), 
          true = stringr::str_extract(string = tags, pattern = rgx_lookbehind(pattern = tag_pattern, match = tag_cut)), 
          false = NA_character_
        )
      )
    
    vals <- source_df[[col_name]][!base::is.na(source_df[[col_name]])]
    
    if(base::all(vals == "TRUE" | vals == "FALSE")){
      
      source_df[[col_name]] <- base::as.logical(source_df[[col_name]])
      
    } else if(!base::any(base::is.numeric(source_df[[col_name]]))) {
      
      source_df[[col_name]] <- base::as.numeric(source_df[[col_name]])
      
    }
    
    print(select(source_df, all_of(c("sample", col_name))))
    
  }
  
  return(source_df)
  
}