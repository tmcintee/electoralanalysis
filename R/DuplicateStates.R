DuplicateStates <- function(df,statemap)
{
  extra_states <- data.frame()
  extra_map <- data.frame()
  statemap$group <- as.character(statemap$group)
  state_list <- list()
  for(i in 1:nrow(df))
  {
    if(!df$State[[i]] %in% state_list)
    {
      state_list[[i]] <- df$State[[i]]
    }
    else
    {
      state_extra <- statemap %>% filter(id == df$State[[i]])
      max_statemap_order <- max(statemap$order)
      df$State[[i]] <- paste0(df$State[[i]],i)
      state_extra <- state_extra %>%
        mutate(id = df$State[[i]],
               group = str_replace(group,".",paste0(i,".")))
      statemap <- bind_rows(statemap,state_extra)
    }
  }
  statemap$group <- factor(statemap$group)
  return(list(df = df,
              base_map = statemap))
}
