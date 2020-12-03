MergeStates <- function(new_name, states,base_data)
{
  #Separate and rename:
  combined_states <- base_data %>%
    filter(region %in% states) %>%
    arrange(order)
  other_states <- base_data %>%
    filter(!region %in% states)
  #identify shared boundaries
  shared_points <- combined_states %>%
    filter(duplicated(lat) & duplicated(long))
  combined_states <- combined_states %>%
    filter(!(duplicated(lat) & duplicated(long)))
  #Identify the subregions that are to be merged.
  shared_group <- unique(shared_points$group)
  min_group <- min(shared_points$group)
  #reassign to shared group
  shared_chunk <- combined_states %>%
    filter(group %in% shared_group) %>%
    mutate(group = min_group)
  other_chunks <- combined_states %>%
    filter(!group %in% shared_group)
  #What we will do is rotate
  start_spot <- which.min(shared_chunk$order)
  first_state <- shared_chunk$region[[start_spot]]
  #select southernmost point.
  for(state in states)
  {
    state_chunk <- shared_chunk %>% filter(region == state)
    state_chunk_gone <- shared_points %>% filter(region == state)
    state_indices <- state_chunk$order
    min_index <- min(state_indices)
    start_index <- min(state_chunk_gone$order)+1
    state_chunk$rel_order <- 1:nrow(state_chunk)
    for(i in 2:nrow(state_chunk_gone))
    {
      if(state_chunk_gone$order[[i]] == state_chunk_gone$order[[i-1]] + 1)
      {
        start_index <- state_chunk_gone$order[[i]]+1
      }
      else
      {
        break
      }
    }
    #catch case where circuit ends correctly anyway:
    if(start_index <= max(state_indices))
    {
      row_num <- which(state_indices == start_index)
      state_chunk$rel_order <- (state_chunk$rel_order - row_num) %% nrow(state_chunk)
      state_chunk <- state_chunk %>%
        arrange(rel_order) %>%
        select(names(shared_chunk))
      state_chunk$order <- state_indices
      shared_chunk[shared_chunk$region == state,] <- state_chunk
    }
  }
  shared_chunk$region <- new_name
  if(nrow(other_chunks) > 0)
  {
    other_chunks$region <- new_name
  }
  all_states <- bind_rows(other_states,shared_chunk,other_chunks)
  return(shared_chunk)
}
