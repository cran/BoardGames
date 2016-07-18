#' Converts a set of x,y coordinates into a matrix index.
#'
#' This function converts a set of unit x,y coordinates into a matrix index.
#' @param data Matrix or data frame.
#' @param x x-coordinate
#' @param y y-coordinate
#' @keywords element coordinate convert matrix index
#' @export
#' @examples
#' M = matrix(1:20,4,5)
#' xy2index(data=M, x=3, y=2)

xy2index = function(data, x, y){
  return(c(dim(data)[2]-y+1,x))
}


#' Converts a matrix index into a sex of x,y coordinates.
#'
#' This function converts a matrix index into unit x,y plotting coordinates.
#' @param data Matrix or data frame.
#' @param index A vector of index values.
#' @keywords element coordinate convert matrix index
#' @export
#' @examples
#' M = matrix(1:20,4,5)
#' index2xy(data = M, index = c(3,4))

index2xy = function(data, index){
  return(c(index[2],-(index[1]-dim(data)[1]-1)))
}


#' Palindrome checker.
#'
#' This function checks if the supplied vector is a palindrome (reads the same forwards and backwards).
#' @param x  Numeric or character vector.
#' @param case.sensitive Does upper or lower casing matter? Defaults to FALSE.
#' @keywords palindrome check vector case sensitive
#' @export
#' @examples
#' test1 = 123
#' test2 = "12321"
#' test3 = c("a",1,2,3,2,1,"a")
#' is_palindrome(test1)
#' is_palindrome(test2)
#' is_palindrome(test3)

is_palindrome = function(x, case.sensitive = FALSE){
  if(case.sensitive == TRUE){
    return(all(rev(unlist(strsplit(as.character(x),split=""))) == unlist(strsplit(as.character(x),split=""))))
  }
  if(case.sensitive == FALSE){
    return(all(rev(unlist(strsplit(tolower(as.character(x)),split=""))) == unlist(strsplit(tolower(as.character(x)),split=""))))
  }
}

#' Detects if a certain sequence is present in a matrix.
#'
#' This function allows for the detection of a particular sequence in a matrix.
#' @param data  A matrix.
#' @param sequence The desired sequence to search for.
#' @param reps Number of repetitions of the sequence.
#' @param diag Do you want to search diagonals? Defaults to TRUE.
#' @keywords sequence vector matrix detect search
#' @export
#' @examples
#' M = matrix(sample(c(1,2),25,replace=TRUE),5,5)
#' detect_seq(data = M, sequence = "2", reps = 5)
#' #or equivalently
#' detect_seq(data = M, sequence = "22222", reps = 1)

detect_seq = function(data, sequence, reps, diag=TRUE){
  R = get_rows(data)
  C = get_cols(data)
  D = get_diags(data,direction = "both")
  if(diag==TRUE){
    check_list = c(R,C,D)
  }else{
    check_list = c(R,C)
  }
  return(sum(grepl(paste(rep(as.character(sequence) ,reps), collapse = ""), lapply(check_list,function(x){paste(x,collapse = "")})))>=1)
}


