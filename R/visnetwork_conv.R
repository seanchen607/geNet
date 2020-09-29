############################################################################################
####################### conversion igraph to visnetwork ####################################
############################################################################################

#' gen_visnetwork_data
#' 
#' function to get the object to generate the visnetwork. It is automatically called by the geNet() function. 
#' @param igraph_network igraph object. Mandatory argument.
#' @return list of two objects of class "ffdf".  
#' * nodes: reports info about the nodes and clustering
#' * edges: reports info about the connections between the nodes
#' This format is designed to be used with the visNetwork package.
#' @export
#' @import ff igraph visNetwork
gen_visnetwork_data<-function(igraph_network){
  data<-toVisNetworkData(igraph_network)
  #----------------- generate the group column ----------------------
  # I temporary name the groups based on the color column
  print("-------generating group column-----")
  data$nodes$group<-data$nodes$color
  #---------------------------- add edges tooltip -----------------------------------------------
  print("-------generating edges tooltip-----")
  title_column<-paste(as.character(data$edges$coefficient[]),as.character(data$edges$logpvalue[]),sep=",")
  data$edges$title<-factor(title_column)
  #---------------------------- add physics column -----------------------------------------------
  print("----- add physics column ---")
  # deactivate physiscs for negative edges
  physics_vec<-rep(T,nrow(data$edges))
  inds<-which(data$edges$coefficient<0)
  physics_vec[inds]<-F
  data$edges$physics<-physics_vec
  #---------------------------- add hidden column -----------------------------------------------
  print("---- add hidden column -----")
  # hidden = T for negative edges by default
  hidden_vec<-rep(F,nrow(data$edges))
  inds<-which(data$edges$coefficient<0)
  hidden_vec[inds]<-T
  data$edges$hidden<-hidden_vec
  gc()
  # ----------------------- convert data to ffdf ------------------------------------------------
  print("------ converting data to ffdf ---------")
  data$edges$from<-factor(data$edges$from)
  data$edges$to<-factor(data$edges$to)
  data$edges$color<-factor(data$edges$color)
  data$edges$title<-factor(data$edges$title)
  
  data$nodes$id<-factor(data$nodes$id)
  data$nodes$color<-factor(data$nodes$color)
  data$nodes$label<-factor(data$nodes$label)
  data$nodes$group<-factor(data$nodes$group)
  data_nodes<-ffdf(id=ff(data$nodes$id),color=ff(data$nodes$color),
                   label=ff(data$nodes$label),group=ff(data$nodes$group))
  
  data_edges<-ffdf(from=ff(data$edges$from),to=ff(data$edges$to),
                   pvalue=ff(data$edges$pvalue),coefficient=ff(data$edges$coefficient),
                   color=ff(data$edges$color),adjusted_pvalue=ff(data$edges$adjusted_pvalue),
                   logpvalue=ff(data$edges$logpvalue),weight=ff(data$edges$weight),
                   width=ff(data$edges$width),title=ff(data$edges$title),
                   physics=ff(data$edges$physics),hidden=ff(data$edges$hidden))
  data<-list(nodes=data_nodes,edges=data_edges)
  gc()
  print("Done")
  return(data)
}