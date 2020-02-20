# create a function
desc_table <- function(df, funcs = c(n = function(x) sum(is.finite(x)), 
                                     mean = mean, sd = sd, min = min, 
                                     q25 = function(x) quantile(x, 0.25), 
                                     median = median, 
                                     q75 = function(x) quantile(x, 0.75), 
                                     max = max)){
  #df should be a dataframe
  if (class(df) != "data.frame") stop("first argument should be a data frame")
  #funcs should be a vector of functions
  if (class(funcs) != "list") stop("second argument should be a vector of functions")
  if (all(sapply(funcs, is.function)) == 0) stop("not all elements of funcs are functions")
  #returns a data frame
  ds <- data.frame(matrix(NA, nrow = sum(sapply(df,is.numeric)), ncol = length(funcs)))
  for (i in 1:length(funcs)) {
    ds[,i] <- unlist(lapply(df[sapply(df, is.numeric)],funcs[[i]]))
  }
  ds <- rbind(ds)
  return(ds)
}