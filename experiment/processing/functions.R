#This script is a library of functions for the Nighttime Transpiration project

add_ID_column <- function(dataset, IDs) {
  dataset <- mutate(dataset, ID = IDs)
  return(dataset)
}

add_tx_column_to_selected <- function(dfs) {
  # Loop through each dataframe in the specified list
  for (df_name in dfs) {
    # Check if the dataframe exists
    if (exists(df_name)) {
      # Get the dataframe
      df <- get(df_name)
      # Check if the dataframe contains an "ID" column
      if ("ID" %in% colnames(df)) {
        # Add a "tx" column based on conditions
        df$tx <- ifelse(df$ID %in% c(1,2,11,12,20,21,22,23,31,32), "CXC", 
                        ifelse(df$ID %in% c(3,4,8,9,15,16,27,28,33,34), "CC",
                               ifelse(df$ID %in% c(6, 14, 19,26,30), "CB",
                                      ifelse(df$ID %in% c(7,10,17,24,35), "C",
                                             ifelse(df$ID %in% c(5,13,18,25,29), "B", NA)
                                      )
                               )
                        )
        )
        # Update the dataframe in the environment
        assign(df_name, df, envir = parent.frame())
      } else {
        cat("Warning: The dataframe", df_name, "does not contain an 'ID' column.\n")
      }
    } 
    else {
      cat("Warning: The dataframe", df_name, "does not exist in the environment.\n")
    }
  }
}

add_tx_col <- function(df) {
  df <- df %>%
    mutate(
      tx = case_when(
        ID %in% c(1, 2, 11, 12, 20, 21, 22, 23, 31, 32) ~ "CXC",
        ID %in% c(3, 4, 8, 9, 15, 16, 27, 28, 33, 34) ~ "CC",
        ID %in% c(6, 14, 19, 26, 30) ~ "CB",
        ID %in% c(7, 10, 17, 24, 35) ~ "C",
        ID %in% c(5, 13, 18, 25, 29) ~ "B",
        TRUE ~ NA_character_
      )
    )
  return(df)
}

#set theme
# custom_theme <- theme_cowplot()
# custom_theme <- custom_theme + theme_bw() + theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))
# theme_set(custom_theme)
