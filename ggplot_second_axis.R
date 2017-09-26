revenue <- round(runif(30,1e2,2e2))
df <- data.frame(day = as.Date(c((Sys.Date() - 30):(Sys.Date() -1)), origin = '1970-01-01'),
                 revenue,
                 number_of_sales = round(revenue * runif(30,.9,1.1) / 10))


create_second_axis_plot <- function(data2plot,name_x_axis,names_y_axes,shift_y2 = 0.7){
  require(ggplot2)
  require(scales)
  require(tidyr)
  
  # convert second y axis to similar values like primary y axis
  data2plot$col_converted <- data2plot[[names_y_axes[2]]]/ mean(data2plot[[names_y_axes[2]]]) * mean(data2plot[[names_y_axes[1]]]) * shift_y2
  
  # reshape data into one column
  data_converted <- data2plot[,c(name_x_axis,names_y_axes[1],'col_converted')] %>% gather_('column_name','value',c(names_y_axes[1],'col_converted'))
  
  # create basic plot
  p_basic <- ggplot(data_converted, aes_(x = as.name(name_x_axis), y = as.name('value'), colour = as.name('column_name'), group = as.name('column_name'))) + geom_line()
  
  # get colors of two lines and order them alphabetically
  colors_plot <- unique(ggplot_build(p_basic)$data[[1]]$colour)
  colors_plot <- colors_plot[order(names_y_axes)]
  
  # plot with adjusted second axis and colored according to the line color
  p_output <- p_basic + 
    ylab(names_y_axes[1]) + 
    scale_y_continuous(sec.axis = sec_axis(~.* mean(data2plot[[names_y_axes[2]]]) / mean(data2plot[[names_y_axes[1]]]) / shift_y2, name = names_y_axes[2]),
                       label = comma ) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y = element_text(color = colors_plot[1]),
          axis.text.y = element_text(color = colors_plot[1]),
          axis.title.y.right = element_text(color = colors_plot[2]),
          axis.text.y.right = element_text(color = colors_plot[2]),
          legend.position="none")
  return(p_output)
}

create_second_axis_plot(df,name_x_axis = 'day',names_y_axes = c('revenue','number_of_sales'))
