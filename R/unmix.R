#' Unmix sediment mixtures
#'
#' Asses the relative contribution of the potential sediment sources for each sediment mixture in the dataset.
#'
#' @param data Data frame containing sediment source and mixtures
#' @param samples Number of samples in each hypercube dimension
#' @param iter Iterations in the source variability analysis
#' @param seed Seed for the random number generator
#'
#' @return Data frame containing the relative contribution of the sediment sources for each sediment mixture and iterations
#'
#' @export
#'
unmix <- function(data, samples = 100L, iter = 100L, seed = 123456L) {
  system.time({
    sources <- inputSource(data)
    mixtures <- inputSample(data)
    
    # verify the number of sources and properties
    if (nrow(sources) >= ncol(mixtures) - 1) 
    if (nrow(sources) >= ncol(mixtures) - 1) {
      warning("As a minimum, n - 1 properties are required to discriminate rigorously between n sources. Additional properties are frequently required to increase the reliability of the results")
    }
    
    
    results <- unmixing_corrected_gof1(sources, mixtures, samples, 
      iter, seed)

    # reorder groups
    data[, 2] <- factor(data[, 2], levels = unique(data[, 2]))
    # read land uses (second column)
    land_uses <- data[, 2]
    # asume last use is target
    target <- levels(land_uses)[nlevels(land_uses)]
    # read sources
    sources <- data[!land_uses == target, ]
    # remove unused groups (target)
    land_uses <- levels(droplevels(sources[, 2]))
    # replace column names
    colnames(results) <- c("id", "GOF", land_uses)
    
    # read land uses (second column)
    land_uses <- data[, 2]
    # asume last use is target
    target <- levels(land_uses)[nlevels(land_uses)]
    # read sources
    mixtures <- data[land_uses == target, ]
    # replace sample names
    results$id <- as.character(results$id)
    for (i in 1:length(mixtures[, 1])) {
      results$id[results$id == toString(i)] <- toString(mixtures[,1][i])
    }
    
    for (i in 1:ncol(results)) {
      results[, i] <- as.numeric(as.character(results[, i]))
    }
    
    results <- results[order(results[, 1], -results[, 2]), ]
    rownames(results) <- 1:nrow(results)
    
    {
      return(results)
    }
  })
}
