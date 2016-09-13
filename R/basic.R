#' @import methods
NULL
#' @import data.table
NULL

#' An S4 class to represent a LabelEncoder.
#'
#' @slot type A character to denote the input type, either character, factor or numeric
#' @slot mapping A data.frame to store the mapping table
setClass("LabelEncoder",
         representation(type = "character", mapping = "data.frame"))

#' An S4 class to represent a LabelEncoder with numeric input.
#'
#' @slot classes A numeric vector to store the unique values of classes
setClass("LabelEncoder.Numeric",
         contains = "LabelEncoder",
         representation(classes = "numeric"))
#' An S4 class to represent a LabelEncoder with character input.
#'
#' @slot classes A character vector to store the unique values of classes
setClass("LabelEncoder.Character",
         contains = "LabelEncoder",
         representation(classes = "character"))
#' An S4 class to represent a LabelEncoder with factor input.
#'
#' @slot classes A factor vector to store the unique values of classes
setClass("LabelEncoder.Factor",
         contains = "LabelEncoder",
         representation(classes = "factor"))
#' An S4 class to represent a OneHotEncoder
#'
#' @slot n_columns An integer value to store the number of columns of input data
#' @slot n_values A numeric vector to store the number of unique values in each column of input data
#' @slot column_encoders A list that stores the LabelEncoder for each column of input data
setClass(
  "OneHotEncoder",
  representation(
    n_columns = "integer",
    n_values = "numeric",
    column_encoders = "list"
  )
)

#' transform
#' transforms a new data set using the fitted encoder
#' @param enc A fitted encoder, i.e., LabelEncoder or OneHotEncoder
#' @param y A vector of character, factor or numeric values
#' @param X A data.frame or matrix
#' @param sparse If TRUE then return a sparse matrix, default = TRUE
#' @param new.feature.error If TRUE then throw an error for new feature values; otherwise the new feature values are ignored, default = TRUE
#' @param ... Additional argument list
#' @return If enc is an OneHotEncoder, the returned value is a sparse or dense matrix. If enc is a LabelEncoder, the returned value is a vector.
#' @export
#' @docType methods
#' @rdname transform-methods
#' @examples
#' # matrix X
#' X1 <- matrix(c(0, 1, 0, 1, 0, 1, 2, 0, 3, 0, 1, 2),c(4,3),byrow=FALSE)
#' oenc <- OneHotEncoder.fit(X1)
#' z <- transform(oenc,X1,sparse=TRUE)
#' # return a sparse matrix
#' print(z)
#'
#' # data.frame X
#' X2 <- cbind(data.frame(X1),X4=c('a','b','d',NA),X5=factor(c(1,2,3,1)))
#' oenc <- OneHotEncoder.fit(X2)
#' z <- transform(oenc,X2,sparse=FALSE)
#' # return a dense matrix
#' print(z)
#'
#' # factor vector y
#' y <- factor(c('a','d','e',NA),exclude=NULL)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <- transform(lenc,factor(c('d','d',NA,'f')))
#' print(z)
#'
#' # character vector y
#' y <- c('a','d','e',NA)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <- transform(lenc,c('d','d',NA,'f'))
#' print(z)
#'
#' # numeric vector y
#' set.seed(123)
#' y <- sample(c(1:10,NA),5)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <-transform(lenc,sample(c(1:10,NA),5))
#' print(z)

setGeneric("transform", function(enc, ...) {
  standardGeneric("transform")
})

#' @rdname transform-methods
#' @aliases transform, LabelEncoder.Numeric,numeric-method
setMethod("transform", signature(enc = "LabelEncoder.Numeric"), function(enc, y) {
  if (!is.numeric(y))
    stop("input should be numeric")
  mapping <- as.data.table(enc@mapping)
  data.table::setkey(mapping, "classes")
  ind = 0L # prevent a warning when build package
  J = 0L
  return(mapping[J(y), ind])
})

#' @rdname transform-methods
#' @aliases transform, LabelEncoder.Character,character-method
setMethod("transform", signature(enc = "LabelEncoder.Character"), function(enc, y) {
  if (!is.character(y))
    stop("input should be character")
  mapping <- as.data.table(enc@mapping)
  data.table::setkey(mapping, "classes")
  ind = 0L # prevent a warning when build package
  J = 0L
  return(mapping[J(y), ind])
})

#' @rdname transform-methods
#' @aliases transform,LabelEncoder.Factor,factor-method
setMethod("transform", signature(enc = "LabelEncoder.Factor"), function(enc, y) {
  if (!is.factor(y))
    stop("input should be factor")
  mapping <- as.data.table(enc@mapping)
  data.table::setkey(mapping, "classes")
  ind = 0L # prevent a warning when build package
  J = 0L
  return(mapping[J(y), ind])
})

#' @rdname transform-methods
#' @aliases transform,OneHotEncoder,Any,logical-method
setMethod("transform", signature(enc = "OneHotEncoder"), function(enc,
                                                                  X,
                                                                  sparse =
                                                                    TRUE,
                                                                  new.feature.error = TRUE) {
  if (ncol(X) != enc@n_columns)
    stop(paste0('input should have ', enc@n_columns, ' columms'))
  if (is.data.table(X)) {
    Label_Matrix <-
      sapply(1:enc@n_columns, function(i)
        transform(enc@column_encoders[[i]], X[, i, with = FALSE][[1]]))
  } else{
    Label_Matrix <-
      sapply(1:enc@n_columns, function(i)
        transform(enc@column_encoders[[i]], X[, i]))
  }
  na_columns = which(colSums(is.na(Label_Matrix)) != 0)
  if (length(na_columns) != 0) {
    if (new.feature.error)
      stop(paste0(
        "new feature value(s) found in column(s): ",
        paste0(na_columns, collapse = ', ')
      ))
    warning(paste0(
      "new feature value(s) found in column(s): ",
      paste0(na_columns, collapse = ', '),
      '\n'
    ))
  }
  n_rows <- nrow(X)
  rows <- rep(1:n_rows, enc@n_columns)
  cols <-
    as.vector(Label_Matrix) + rep(c(0L, cumsum(enc@n_values)[-enc@n_columns]), each = n_rows)
  if (length(na_columns) != 0) {
    na_locations <- which(as.vector(!is.na(Label_Matrix)))
    rows <- rows[na_locations]
    cols <- cols[na_locations]
  }

  sp_matrix <- Matrix::sparseMatrix(i = rows, j = cols, x = 1L)

  if (!sparse)
    return(as.matrix(sp_matrix))
  return(sp_matrix)
})

#' inverse.transform
#' transforms an integer vector back to the original vector
#' @param enc A fitted LabelEncoder
#' @param z A vector of integers
#' @return A vector of characters, factors or numerics.
#' @export
#' @docType methods
#' @rdname inverse.transform-methods
#' @examples
#' # character vector y
#' y <- c('a','d','e',NA)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <- transform(lenc,c('d','d',NA,'f'))
#' print(z)
#' inverse.transform(lenc,z)
#'
#' # factor vector y
#' y <- factor(c('a','d','e',NA),exclude=NULL)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <- transform(lenc,factor(c('a','d',NA,'f')))
#' inverse.transform(lenc,z)
#'
#' # numeric vector y
#' set.seed(123)
#' y <- c(1:10,NA)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' newy <- sample(c(1:10,NA),5)
#' print(newy)
#' z <-transform(lenc,newy)
#' inverse.transform(lenc, z)
setGeneric("inverse.transform", function(enc, z) {
  standardGeneric("inverse.transform")
})

#' @rdname inverse.transform-methods
#' @aliases inverse.transform,LabelEncoder,numeric-method
setMethod("inverse.transform", signature(enc = "LabelEncoder", z = "numeric"), function(enc, z) {
  mapping <- data.table(enc@mapping)
  data.table::setkey(mapping, "ind")
  classes = 0L
  J = 0L
  return(mapping[J(z), classes])
})

#' LabelEncoder.fit
#' fits a LabelEncoder object
#' @param y A vector of characters, factors, or numerics, which can include NA as well
#' @return Returns an object of S4 class LabelEncoder.
#' @export
#' @rdname LabelEncoder.fit
#' @examples
#' # factor y
#' y <- factor(c('a','d','e',NA),exclude=NULL)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <- transform(lenc,factor(c('d','d',NA,'f')))
#' print(z)
#'
#' # character y
#' y <- c('a','d','e',NA)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <- transform(lenc,c('d','d',NA,'f'))
#' print(z)
#'
#' # numeric y
#' set.seed(123)
#' y <- sample(c(1:10,NA),5)
#' lenc <- LabelEncoder.fit(y)
#' # new values are transformed to NA
#' z <-transform(lenc,sample(c(1:10,NA),5))
#' print(z)
LabelEncoder.fit <- function(y) {
  if (!is.numeric(y) &&
      !is.character(y) &&
      !is.factor(y))
    stop("input can only be either numeric or character or factor")
  classes <- sort(unique(y), na.last = TRUE)
  map <- data.table::data.table(classes)
  data.table::setkey(map, "classes")
  ind = 0L # prevent a warning when build package
  map[, ind := 1:length(classes)]
  if (is.numeric(y)) {
    encoder <- new(
      "LabelEncoder.Numeric",
      type = "numeric",
      classes = classes,
      mapping = as.data.frame(map)
    )
  } else if (is.character(y)) {
    encoder <- new(
      "LabelEncoder.Character",
      type = "character",
      classes = classes,
      mapping = as.data.frame(map)
    )
  } else {
    encoder <- new(
      "LabelEncoder.Factor",
      type = "factor",
      classes = classes,
      mapping = as.data.frame(map)
    )
  }
  return(encoder)
}
#' OneHotEncoder.fit
#' fits an OneHotEncoder object
#' @param X A matrix or data.frame, which can include NA
#' @return Returns an object of S4 class OneHotEncoder
#' @export
#' @rdname OneHotEncoder.fit
#' @examples
#' # matrix input
#' X1 <- matrix(c(0, 1, 0, 1, 0, 1, 2, 0, 3, 0, 1, 2),c(4,3),byrow=FALSE)
#' oenc <- OneHotEncoder.fit(X1)
#' z <- transform(oenc,X1,sparse=TRUE)
#' # return a sparse matrix
#' print(z)
#'
#' # data.frame
#' X2 <- cbind(data.frame(X1),X4=c('a','b','d',NA),X5=factor(c(1,2,3,1)))
#' oenc <- OneHotEncoder.fit(X2)
#' z <- transform(oenc,X2,sparse=FALSE)
#' # return a dense matrix
#' print(z)
OneHotEncoder.fit <- function(X) {
  if (!is.matrix(X) &&
      !is.data.frame(X))
    stop("input can only be matrix or data.frame")
  if (is.data.table(X)) {
    column_encoders <-
      sapply(1:ncol(X), function(i)
        LabelEncoder.fit(X[, i, with = FALSE][[1]]))
  } else{
    column_encoders <-
      sapply(1:ncol(X), function(i)
        LabelEncoder.fit(X[, i]))
  }
  n_values <-
    unlist(lapply(column_encoders, function(e)
      length(e@classes)))
  encoder <- new(
    "OneHotEncoder",
    n_columns = ncol(X),
    n_values = n_values,
    column_encoders = column_encoders
  )
  return(encoder)
}
