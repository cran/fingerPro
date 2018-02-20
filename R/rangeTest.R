#' Range test
#'
#' Function that excludes the properties of the sediment mixture/s outside the minimum and maximum values in the sediment sources.
#'
#' @param data Data frame containing source and mixtures
#'
#' @return Data frame containing sediment sources and mixtures
#'
#' @export
#'

if(getRversion() >= "2.15.1")  utils::globalVariables(c("s_max", "t_max","s_min","t_min"))

rangeTest <- function(data) {
  # reorder groups
  data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))

  # read land uses (second column)
  land_uses <- data[, 2]
  
  # replace all uses with 'source' except the last one
  levels(land_uses)[1:nlevels(land_uses) - 1] <- "source"
  
  # compute min values for target and source groups
  min <- aggregate(data[, 3:ncol(data)], list(land_uses), min)
  n_min <- c("s_min", "t_min")
  mint <- as.data.frame(t(min[, -1]))
  colnames(mint) <- n_min
  
  # compute max values for target and source groups
  max <- aggregate(data[, 3:ncol(data)], list(land_uses), max)
  n_max <- c("s_max", "t_max")
  maxt <- as.data.frame(t(max[, -1]))
  colnames(maxt) <- n_max
  
  # merge min and max values
  ranges1 <- merge(maxt, mint, by.x = "row.names", by.y = "row.names")
  
  # filter properties out of range
  ranges <- subset(ranges1, s_max < t_max | s_min > t_min)
  rows <- as.vector(ranges$Row.names)
  data <- data[, !(names(data) %in% rows)]
  cat("Attention->", nrow(ranges), "variables from a total of", nrow(ranges1), 
    "were removed:", ranges[, 1], ".")
  cat(" The variable/variables that remains in your dataset is/are:", 
    names(data[, 3:ncol(data)]), ".")
  
  return(data)
}
