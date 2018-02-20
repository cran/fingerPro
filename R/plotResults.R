#' Displays the results in the screen
#'
#' The function performs a density chart of the relative contribution of the potential sediment sources for each sediment mixture in the dataset.
#' 
#' @param data Data frame containing the relative contribution of the potential sediment sources for each sediment mixture in the dataset
#' @param y_high Number of the vertical height of the y-axis
#' @param n Number of charts per row
#' 
#' @export
#' 

if(getRversion() >= "2.15.1")  utils::globalVariables(c("result", "value", "variable"))

plotResults <- function(data, y_high = 10, n = 1) {
  data_plots <- melt(result, id = c(1:2))
  id_unicos <- length(unique(data_plots$id))
  
  plot <- ggplot(data_plots, aes(x = value)) + geom_density(aes(group = variable, 
    colour = variable, fill = variable), alpha = 0.35) + xlim(0, 1) + 
    ylim(0, y_high) + scale_fill_discrete(name = "sources") + scale_colour_discrete(name = "sources")

  # plot<- ggplot(data_plots, aes(x=value)) +
  # geom_freqpoly(aes(group=variable, colour=variable),binwidth = 0.05)+
  # xlim(0, 1)
  # plot<- ggplot(data_plots, aes(x=value)) +
  # geom_histogram(aes(group=variable, colour=variable, fill=variable),
  # alpha=0.2, binwidth = 0.025)+ xlim(0, 1)

  plot <- plot + facet_wrap(~id, ncol = n)

  print(plot)
  
  print(aggregate(. ~ id, data = result, function(x) c(mean = mean(x), SD = sd(x))))
 }
