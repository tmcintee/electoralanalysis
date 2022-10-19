MapHSV <- function(df,
                   base_map = fifty_states_mod,
                   manual_pivots = character(),
                   alpha_limit = 1,
                   colorVector = NULL,
                   manual_names = NULL)
{
  require(ggstar)
  df <- df %>% mutate(State = tolower(State))
  #Add old_virginia
  if(!"west virginia" %in% df$State)
  {
    df$State[df$State == "virginia"] <- "virginia1860"
  }
  manual_pivots <- tolower(manual_pivots)
  if(length(manual_pivots) == 0)
  {
    pivot_state <- PivotalState(df)
    crucial_states <- CrucialStates(df)
    crucial_states_map <- base_map %>%
      filter(id %in% crucial_states$State) %>%
      group_by(group) %>%
      nest()
    lines <- map_df(crucial_states_map$data, draw.crosshatch, width = .25, pattern= "crosshatch")
  }
  else
  {
    pivot_state <- df %>% filter(State %in% manual_pivots)
    crucial_states <- df %>% filter(State %in% manual_pivots)
  }
  #Create pivot centroids.
  pivot_state_map <- base_map %>%
    filter(id %in% pivot_state$State) %>%
    mutate(g_n = parse_number(str_extract(group,"[1234567890]"))) %>%
    filter(g_n == min(g_n))
  pivmap_list <- pivot_state_map %>% select(long,lat,id) %>% group_split(id)
  piv_select <- list()
  for(i in 1:length(pivmap_list))
  {
    piv_select[[i]] <- pivmap_list[[i]] %>% select(long,lat)
  }
  pivot_centroids <- lapply(piv_select,geosphere::centroid)
  pivot_x <- numeric(length(pivot_centroids))
  pivot_y <- numeric(length(pivot_centroids))
  for(i in 1:length(pivot_centroids))
  {
    pivot_x[[i]] <- pivot_centroids[[i]][[1]]
    pivot_y[[i]] <- pivot_centroids[[i]][[2]]
  }
  #Calculate crucial state centroids.
  crucial_state_map <- base_map %>%
    filter(id %in% crucial_states$State) %>%
    mutate(g_n = parse_number(str_extract(group,"[1234567890]"))) %>%
    filter(g_n == min(g_n))
  crumap_list <- crucial_state_map %>% select(long,lat,id) %>% group_split(id)
  cru_select <- list()
  for(i in 1:length(crumap_list))
  {
    cru_select[[i]] <- crumap_list[[i]] %>% select(long,lat)
  }
  crucial_centroids <- lapply(cru_select,geosphere::centroid)
  crucial_x <- numeric(length(crucial_centroids))
  crucial_y <- numeric(length(crucial_centroids))
  for(i in 1:length(crucial_centroids))
  {
    crucial_x[[i]] <- crucial_centroids[[i]][[1]]
    crucial_y[[i]] <- crucial_centroids[[i]][[2]]
  }
  #Critical state outlines.
  crit_states <- CriticalStates(df)
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
  if(is.null(colorVector))
  {
    colorVector <- c("blue","firebrick3","gold2","forestgreen","magenta3")
    if(str_detect(parties[[2]],".D"))
    {
      colorVector <- colorVector[c(2,1,3,4,5)]
    }  else if(str_detect(parties[[3]],".D"))
    {
      colorVector <- colorVector[c(2,3,1,4,5)]
    }  else if(str_detect(parties[[4]],".D"))
    {
      colorVector <- colorVector[c(2,3,4,1,5)]
    }  else if(str_detect(parties[[3]],".R")) #1912 case
    {
      colorVector <- colorVector[c(1,3,2,4,5)]
    }
  }
  if(!is.null(manual_names))
  {
    df$Party <- factor(df$Party,levels = summary_party$Party,labels = manual_names)
  } else
  {
    df$Party <- factor(df$Party,levels = summary_party$Party)
  }
  if(length(manual_pivots) == 0)
  {
    pivot_geom <- geom_star(size = 5,
                            x = pivot_x,
                            y = pivot_y,
                            fill = "white",
                            color = "black")
      #geom_point(#pch="\u2605", <= Unfortunately does not work with pdf rendering.
                             #shape = 17,
                             #size = 5,
                             #x = pivot_x,
                             #y = pivot_y,
                             #color = "black",
                             #alpha = 1)
    crucial_geom <- geom_star(size = 3,
                              data = df %>% filter(State %in% crucial_states$State),
                              x = crucial_x,
                              y = crucial_y,
                              fill = "white",
                              color = "black",
                              alpha = 1)
    #crucial_geom <- geom_segment(data=lines, aes(x= x, y = y , xend = xend, yend = yend),
                 #inherit.aes = F,color = "black",alpha = 0.3,size =0.1)
  }
  else
  {
    crucial_geom <- geom_star(size = 5,
                            data = df %>% filter(State %in% pivot_state$State),
                            x = pivot_x,
                            y = pivot_y,
                            fill = "white",
                            color = "black",
                            alpha = 1)
    pivot_geom <- geom_point(data = df %>% filter(State %in% pivot_state$State),
                             pch="?",size = 2,
                             x = pivot_x,
                             y = pivot_y,
                             color = "black",
                             alpha = 1)
  }
  g <- ggplot(df, aes(map_id=State,fill = Party, alpha = Votes))+
    scale_fill_manual(values = colorVector)+
    expand_limits(x = base_map$long, y = base_map$lat)+
    coord_map()+
    scale_alpha_continuous(range = c(0.1,alpha_limit))+
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "",color = "",alpha = "") +
    theme(legend.position = "bottom",
          panel.background = element_blank(),
          plot.margin = margin(0,0,0,0,"cm"))+
    guides(fill = FALSE,
           alpha = FALSE)
  big <- g+
		geom_map(map = base_map, color = "gray")+
	  geom_map(map = base_map, color = "black", size = 1, data = df %>% filter(State %in% crit_states$State),alpha = 0)+
    crucial_geom+
    pivot_geom
  little <- g+
    geom_map(map = base_map, data = df %>% filter(Votes !=0), color = "gray")+
    facet_wrap(~Party,nrow = 1)
  nPanel <- nrow(summary_party)
  ggpubr::ggarrange(big,
                    little,
                    ncol = 1,
                    heights = c(1+0.5*nPanel-0.3*(nPanel==4),1))+
    theme(plot.margin = margin(-0.5,-1,-0.5,-1, "cm"))
}
