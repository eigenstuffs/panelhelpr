#' Split panel data
#'
#' Splits panel data (columns as time and rows as units) into horizontal and vertical pre- and post- treatment matrices.
#' @param dat A data frame with columns as time periods and rows as units. Can contain a column for unit names but otherwise should only contain values.
#' @param unit_col A character vector of unit names. Should match the unit column if it exists.
#' @param treatment_column A character object of the first treated period's variable name.
#' @param num_post The amount of post-treatment periods you want to keep. Defaults to NULL which keeps all.
#' @return List of each direction's pre- and post- treatment matrices.
#' @export

split_panel <- function(dat,
                        unit_col,
                        treatment_column,
                        num_post = NULL) {
  cat("num_post:", as.character(num_post), "\n")
  if (!is.null(num_post)) {
    if (num_post < 1) {
      stop("num_post must be greater than 0, or NULL to include all.")
    }
  }
  print("-------------------------")
  print("num_post valid ....")
  print("-------------------------")

  for (i in seq(ncol(dat))) {
    if (all(dat[,i] == unit_col)) {
      print("unit_col removed ....")
      print("-------------------------")
      dat <- dat[,-i]
      break
    }
  }

  if (is.character(treatment_column)) {
    treatment_index <- which(names(dat) == treatment_column)
  } else {
    stop("treatment_column must be the name of the treated variable in the data frame.")
  }

  print("treatment_index created ....")
  print("-------------------------")

  dat <- as.matrix(dat)

  if (!is.null(num_post)) {
    horizontal_post <- as.matrix(dat[,treatment_index:(treatment_index+(num_post-1))])
    colnames(horizontal_post) <- colnames(dat)[treatment_index:(treatment_index+(num_post-1))]
  } else {
    horizontal_post <- as.matrix(dat[,treatment_index:ncol(dat)])
    colnames(horizontal_post) <- colnames(dat)[treatment_index:ncol(dat)]
  }

  horizontal_post <- apply(horizontal_post, 2, function(x) as.numeric(x))

  print("horizontal_post created ....")
  print("-------------------------")

  horizontal_pre <- as.matrix(dat[,1:(treatment_index-1)])
  horizontal_pre <- apply(horizontal_pre, 2, function(x) as.numeric(x))

  rownames(horizontal_post) <- unit_col
  rownames(horizontal_pre) <- unit_col

  print("horizontal_pre created ....")
  print("-------------------------")


  vertical_dat <- t(as.matrix(dat))
  colnames(vertical_dat) <- unit_col

  vertical_treated_row <- which(rownames(vertical_dat) == treatment_column)

  vertical_pre <- as.matrix(vertical_dat[1:(vertical_treated_row-1),])

  print("vertical_pre created ....")
  print("-------------------------")

  if (!is.null(num_post)) {
    vertical_post <- as.matrix(vertical_dat[vertical_treated_row:(vertical_treated_row+(num_post-1)),])
  } else {
    vertical_post <- as.matrix(vertical_dat[vertical_treated_row:nrow(vertical_dat),])
  }


  if (!is.null(num_post)) {
    if (num_post == 1) {
      vertical_post <- t(vertical_post)
      rownames(vertical_post) <- rownames(vertical_dat)[vertical_treated_row]
    }
  }

  print("vertical_post created ....")
  print("-------------------------")


  list(
    horizontal_covariates = horizontal_pre,
    horizontal_outcomes = horizontal_post,
    vertical_covariates = vertical_pre,
    vertical_outcomes = vertical_post
  )
}
