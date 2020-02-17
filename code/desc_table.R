# create a function
desc_table <- function(df, funcs){
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

mat <- matrix(rnorm(1000), nrow = 100, ncol = 10)
colnames(mat) <- paste0("numeric", 1:10)
df <- as.data.frame(mat)
df$factor1 <- as.factor(sample(LETTERS[1:5], 100, replace = TRUE))
df$factor2 <- as.factor(sample(letters[6:10], 100, replace = TRUE))
df$logical <- sample(c(TRUE, FALSE), 100, replace = TRUE) 
df$character <- sample(c("BLUB", "BLOBB", "BLABB"), 100, replace = TRUE) 

funcs <- c(function(x) sum(is.finite(x)), mean, sd, min, 
           function(x) quantile(x, 0.25), median, 
           function(x) quantile(x, 0.75), max)