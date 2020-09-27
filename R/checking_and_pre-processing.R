####################################################################################
######################### checking and pre-processing functions ####################
####################################################################################

#' checking input df Function
#'
#' function to check the format of the input binary df. It is automatically called by the geNet() function.
#' @param binary_df a binary dataframe to be checked. Mandatory argument. 
#' @return None
#' @export
#' @examples
#' \dontrun{check_input_df(binary_df)}
check_input_geNet<-function(binary_df){
  #-------------- check duplication --------------
  print("--- checking duplicated gene IDs ----")
  inds<-which(duplicated(colnames(binary_df))==T)
  if(length(inds)!=0){
    stop("the input df has duplicated gene ids. Duplicated Ids are not accepted")
  }
  # check if data is binary
  print("--- checking if input is binary ----")
  out<-all(binary_df == 0 | binary_df == 1)
  if(out==F){
    stop("the input dataframe is not binary")
  }
}
#' check_input_visualization
#'
#' function to check the format of the input binary df. It is automatically called by the geNet() function.
#' @param data output of the geNet() function (list of two ffdf objects)
#' @return error message if checking fails
#' @export
#' @examples 
#' \dontrun{check_input_visualization(data)}
check_input_visualization<-function(data){
  if(is.list(data)==T){
    # it needs to be a list of two ffdf objects (nodes and edges)
    if(length(data)!=2){
      stop("the list must contains only 2 elements")
    }
    # minimal information that needs to be present for nodes element
    nodes_element<-data[[1]]
    columns_names<-colnames(nodes_element)
    all_names<-c("id","label","color","group") 
    check_columns<- all_names %in% columns_names
    if(any(check_columns==F)){
      inds<-which(check_columns==F)
      print("the following nodes info are missing:")
      print(all_names[inds])
      stop("nodes info incomplete")
    }
    # minimal information that needs to be present for edges element
    edges_element<-data[[2]]
    columns_names<-colnames(edges_element)
    all_names<- c("from","to","color","pvalue","coefficient",
                 "adjusted_pvalue","logpvalue","weight","width",
                 "title","physics","hidden")
    check_columns<-all_names %in% columns_names
    if(any(check_columns==F)){
      inds<-which(check_columns==F)
      print("the following edges info are missing:")
      print(all_names[inds])
      stop("edges info incomplete")
    }
    print("visualization checking done")
  }else{
    stop("no valid input for visualization: input object is not a list")
  }
}