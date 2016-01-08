# calculates tf-idf for different parameters and using
# different tf-idf-versions
tab_tfidf <- function(ncorpus=20) {
  # I assume a maximum word frequency of 4000
  max_ft <- 4000
  
  # tf-idf without log
  tfidf0 <- function(ft,max_ft,ndocs,ncorpus) (ft/max_ft) * (ncorpus/ndocs)
  
  # traditional tf-idf
  tfidf1 <- function(ft,max_ft,ndocs,ncorpus) (ft/max_ft) * log(ncorpus/ndocs)
  
  # tf-idf with added idf/N
  tfidf2 <- function(ft,max_ft,ndocs,ncorpus) (1/ncorpus + ft/max_ft) * log(ncorpus/ndocs)
  
  # ft = frequency of term / ndocs = how often it showed up in other documents
  df <- expand.grid(ft=c(5,10,20,30),ndocs=c(1,2,3,5,10))
  
  res0 <- apply(df,1,function(r) tfidf0(r["ft"],max_ft,r["ndocs"],ncorpus))
  ranks0 <- order(order(-res0))
  
  res1 <- apply(df,1,function(r) tfidf1(r["ft"],max_ft,r["ndocs"],ncorpus))
  ranks1 <- order(order(-res1))
  
  res2 <- apply(df,1,function(r) tfidf2(r["ft"],max_ft,r["ndocs"],ncorpus))
  ranks2 <- order(order(-res2))
  
  result <- cbind(df,res0,res1,res2,ranks0,ranks1,ranks2)
  result <- result[order(result$ft),]
  
  return(list("ncorpus" = ncorpus, "max_ft" = max_ft, result))
}

# tf-idf for combinations of term frequency in {10,20,30} and
# occurences in {1,2,3} relative to (20, 2)
get_change_matrix <- function(res, colname) {
  m <- matrix(res[res$ft %in% c(10,20,30) & res$ndocs %in% 1:3,colname], ncol=3)
  # num of documents where word is assumed to be present
  rownames(m) <- as.character(1:3)
  
  # num of occurences within the hypothetical document
  colnames(m) <- as.character(1:3*10)
  
  # (A-B)/B
  m <- round((m - m[2,2])/m[2,2],2)
  
  return(m)
}

