custom_standardise_func <- function(variable) {
    std_variable <- (variable-mean(variable, na.rm=TRUE))/sd(variable, na.rm=TRUE)
    return(std_variable)
}