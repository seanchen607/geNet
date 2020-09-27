########################################################################################
############################# functions to get info on the genes #######################
#########################################################################################


#' Calculate genes frequency
#' 
#' function to calculate the percentage of occurrence of every gene across 
#' all strains.
#' @param input_binary_df Object of class "dataframe", containing the absence/presence of genes
#' @return Object of class "dataframe". The first column reports the ID of the nodes/genes,
#' the second column reports the frequency across all strains
#' @export
#' @examples 
#' \dontrun{generate_freqs_genes(input_binary_df)}
generate_freqs_genes<-function(input_binary_df){
  vector_occurences<-sapply(1:ncol(input_binary_df),function(i){
    occurence_gene_i<-length(which(input_binary_df[,i]!=0))
    return(occurence_gene_i)
  })
  total_n_strains<-nrow(input_binary_df)
  occurences_percentages<-round((vector_occurences/total_n_strains)*100,2)
  matrix_temp<- cbind(colnames(input_binary_df),occurences_percentages)
  df<-as.data.frame(matrix_temp)
  colnames(df)<-c("genes","freq")
  df$freq<-as.numeric(df$freq)
  return(df)
}

