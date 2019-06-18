#################################################
# Compute the Factors for a categorical variable
#################################################
getDomain <- function(node, catName){
  domain <- node[catName]
  fact <- factor(domain[[1]])
  level <- levels(fact)
  return(level)
}
