#' Get surrounding elements of an element in a matrix.
#'
#' This function extracts all surrounding elements of a specified element in a matrix and returns the result as a vector.
#' @param data Matrix.
#' @param index Index position of element. Input as a vector of row then column positions.
#' @param type Takes values of "direct" and "all". "direct" returns only the elements directly in contact with the specified element, whereas "all" returns every surrounding element including diagonals. Defaults to "all".
#' @keywords elements surrounding vector matrix
#' @export
#' @examples
#' M = matrix(1:20,4,5)
#' get_surround(data = M, index = c(2,3))

get_surround = function(data, index, type="all"){
  row_height = dim(data)[1]
  col_height = dim(data)[2]
  if(type!= "all" && type!="direct"){
    print(" type has to take values of either 'all' or 'direct' ")
  }else{
    if(type=="all"){
      index1     = c(index[1]-1,index[1]-1,index[1]-1,index[1]  ,index[1]+1,index[1]+1,index[1]+1,index[1]  )
      index2     = c(index[2]-1,index[2]  ,index[2]+1,index[2]+1,index[2]+1,index[2]  ,index[2]-1,index[2]-1)
    }
    if(type=="direct"){
      index1     = c(index[1]-1,index[1]+1,index[1]  ,index[1]  )
      index2     = c(index[2]  ,index[2]  ,index[2]-1,index[2]+1)
    }
  }
  adj_ind1   = index1[which(index1>=1 & index1<=row_height)[which(index1>=1 & index1<=row_height) %in% which(index2>=1 & index2<=col_height)]]
  adj_ind2   = index2[which(index1>=1 & index1<=row_height)[which(index1>=1 & index1<=row_height) %in% which(index2>=1 & index2<=col_height)]]
  empty_ind  = c()
  for(i in 1:length(adj_ind1)){
    empty_ind = c(data[adj_ind1[i],adj_ind2[i]], empty_ind)
    }
  return(empty_ind)
}


#' Get all diagonals vectors of a matrix.
#'
#' This function extracts all diagonal vectors of a matrix and returns the result as a list.
#' @param data Matrix from which to extract diagonal elements
#' @param direction Which side to begin on? Takes values of one of "left", "right" or "both". Defaults to "right".
#' @keywords diagonal vectors matrix
#' @export
#' @examples
#' M = matrix(rnorm(9),3,3)
#' get_diags(M)

get_diags = function(data, direction="right"){
  if(direction=="right"){
    I  = row(data) - col(data)
    return(split(data,I))
  }
  if(direction=="left"){
    I  = row(data) + col(data)
    return(split(data,I))
  }
  if(direction == "both"){
    I1 = row(data) - col(data)
    I2 = row(data) + col(data)
    return(c(split(data,I1),split(data,I2)))
  }
}

#' Get all column vectors of a matrix.
#'
#' This function extracts all column vectors of a matrix and returns the result as a list.
#' @param data Matrix from which to extract column vectors.
#' @keywords extract column vectors matrix
#' @export
#' @examples
#' M = matrix(rnorm(9),3,3)
#' get_cols(M)

get_cols = function(data){
  return(split(data,col(data)))
}


#' Get all row vectors of a matrix.
#'
#' This function extracts all row vectors of a matrix and returns the result as a list.
#' @param data Matrix from which to extract row vectors.
#' @keywords extract row vectors matrix
#' @export
#' @examples
#' M = matrix(rnorm(9),3,3)
#' get_rows(M)

get_rows = function(data){
  return(split(data,row(data)))
}

