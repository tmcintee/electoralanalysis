MapSimple <- function(df,base_map = fifty_states_mod,colorVector = c("red","blue"))
{
  df <- df %>% mutate(State = tolower(State))
  #Add old_virginia
  if(!"west virginia" %in% df$State)
  {
    df$State[df$State == "virginia"] <- "virginia1860"
  }
  elecVoteCols <- grep(names(df),pattern = "^Elec")
  stateCol <- grep(names(df),pattern = "State")
  df <- df[c(stateCol,elecVoteCols)]
  df <- df %>% gather(key = "Party", value = "Votes",-State) %>% filter(Votes > 0)
  base_map <- base_map %>% filter(id %in% df$State)
  list_master <- DuplicateStates(df,base_map)
  df <- list_master$df
  base_map <- list_master$base_map
  summary_party <- df %>% group_by(Party) %>% summarise(Votes = sum(Votes)) %>% arrange(-Votes)
  parties <- character(length = 4)
  for(i in 1:length(summary_party$Party))
  {
    parties[[i]] <- summary_party$Party[[i]]
  }
  df$Party <- factor(df$Party,levels = summary_party$Party)
  g <- ggplot(df, aes(map_id=State,fill = Party, alpha = Votes))+
    scale_fill_manual(values = colorVector)+
    expand_limits(x = base_map$long, y = base_map$lat)+
    coord_map()+
    scale_alpha_continuous(range = c(0.1,1))+
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "",color = "",alpha = "") +
    theme(legend.position = "bottom",
          panel.background = element_blank(),
          plot.margin = margin(0,0,0,0,"cm"))+
    guides(fill = FALSE,
           alpha = FALSE)
  big <- g+
		geom_map(map = base_map, color = "gray")
  return(big)
}
