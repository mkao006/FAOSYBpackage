##' Predefined plots
##'
##' @export

plot_dictionary = function(x, y, group, type, data, x_lab, y_lab,
                           legend_lab, col_pallete){
  if(is.null(legend_lab))
    legend_lab = sort(unique(data[, group]))
  legend_wrap = ifelse(length(legend_lab) < 6, 1, 2)
  switch(type,
         "reg_uni_bar" = {
           hideLegend = ifelse(group == x, "none", "top")
           x = paste("factor(", x, ")", sep = "")
           chart =
             ggplot(data = data, aes_string(x = x, y = y)) +
             geom_bar(aes_string(col = group, fill = group),
                      position = "dodge", stat = "identity") +
             scale_fill_manual(labels = legend_lab,
                               values = col_pallete) +
             scale_color_manual(labels = legend_lab,
                                values = col_pallete) +
             theme(axis.text.x = element_text(angle = 45),
                   legend.position = hideLegend) +
             guides(fill = guide_legend(nrow = legend_wrap),
                    color = guide_legend(nrow = legend_wrap)) +
             xlab(x_lab) + ylab(y_lab)
         },
         "reg_uni_line" = {
           breaks = rpretty(min(data[, x]), max(data[, x]), m = 5)
           chart =
             ggplot(data = data, aes_string(x = x, y = y)) +
             geom_line(aes_string(col = group)) +
             geom_point(aes_string(col = group), size = 1.2) +
             scale_color_manual(labels = legend_lab,
                                values = col_pallete) +
             scale_x_continuous(breaks = breaks) +
             guides(color = guide_legend(nrow = legend_wrap)) +
             xlab(x_lab) + ylab(y_lab)
         },
         "multi_dodge_bar" = {
           x = paste("factor(", x, ")", sep = "")
           chart =
             ggplot(data = data, aes_string(x = x, y = "value")) +
             geom_bar(aes_string(col = group, fill = group),
                      position = "dodge", stat = "identity") +
             scale_fill_manual(labels = legend_lab,
                               values = col_pallete) +
             scale_color_manual(labels = legend_lab,
                                values = col_pallete) +
             theme(axis.text.x = element_text(angle = 45)) +
             guides(fill = guide_legend(nrow = legend_wrap),
                    color = guide_legend(nrow = legend_wrap)) +
             xlab(x_lab) + ylab(y_lab)
         },
         "multi_stack_bar" = {
           x = paste("factor(", x, ")", sep = "")
           chart =
             ggplot(data = data, aes_string(x = x, y = "value")) +
             geom_bar(aes_string(col = group, fill = group), stat = "identity",
                      position = "stack") +
             scale_fill_manual(labels = legend_lab,
                               values = col_pallete) +
             scale_color_manual(labels = legend_lab,
                                values = col_pallete) +
             theme(axis.text.x = element_text(angle = 45)) +
             guides(fill = guide_legend(nrow = legend_wrap),
                    color = guide_legend(nrow = legend_wrap)) +
             xlab(x_lab) + ylab(y_lab)
         },
         "multi_line" = {
           breaks = rpretty(min(data[, x]), max(data[, x]), m = 5)
           chart =
             ggplot(data = data, aes_string(x = x, y = "value")) +
             geom_line(aes_string(col = group, fill = group)) +
             geom_point(aes_string(col = group), size = 1.2) +
             scale_fill_manual(labels = legend_lab,
                               values = col_pallete) +
             scale_color_manual(labels = legend_lab,
                                values = col_pallete) +
             scale_x_continuous(breaks = breaks) +
             guides(fill = guide_legend(nrow = legend_wrap),
                    color = guide_legend(nrow = legend_wrap)) +
             xlab(x_lab) + ylab(y_lab)
         },
         "multi_stack_line" = {
           breaks = rpretty(min(data[, x]), max(data[, x]), m = 5)
           chart =
             ggplot(data = data, aes_string(x = x, y = "value")) +
             geom_area(aes_string(col = group, fill = group)) +
             scale_fill_manual(labels = legend_lab,
                               values = col_pallete) +
             scale_color_manual(labels = legend_lab,
                                values = col_pallete) +
             scale_x_continuous(breaks = breaks) +
             guides(fill = guide_legend(nrow = legend_wrap),
                    color = guide_legend(nrow = legend_wrap)) +
             xlab(x_lab) + ylab(y_lab)
         },
         "scatter_plot" = {
           chart =
             ggplot(data = data, aes_string(x = x, y = y)) +
             geom_point(aes_string(col = group)) +
             scale_color_manual(labels = legend_lab,
                                values = col_pallete) +
             guides(color = guide_legend(nrow = legend_wrap)) +
             xlab(x_lab) + ylab(y_lab)
         },
         "top_20_bar" = {
           chart = ggplot(data = data, aes_string(x = y, y = x)) +
             geom_bar(aes_string(col = group, fill = group)) +
             scale_fill_manual(values = col_pallete) +
             scale_color_manual(values = col_pallete) +
             coord_flip() + xlab(y_lab) + ylab(x_lab) +
             theme(axis.text.x = element_text(hjust=0.5))
         },
         "bot_20_bar" = {
           chart = ggplot(data = data, aes_string(x = y, y = x)) +
             geom_bar(aes_string(col = group, fill = group)) +
             scale_fill_manual(values = col_pallete) +
             scale_color_manual(values = col_pallete) +
             coord_flip() + xlab(y_lab) + ylab(x_lab) +
             theme(axis.text.x = element_text(hjust=0.5))
         },
         "top_bot_bar" = {
           chart = ggplot(data = data, aes_string(x = y, y = x)) +
             geom_bar(aes_string(col = group, fill = group)) +
             scale_fill_manual(values = col_pallete) +
             scale_color_manual(values = col_pallete) +
             coord_flip() + xlab(y_lab) + ylab(x_lab) +
             theme(axis.text.x = element_text(hjust=0.5))
         },
         "top_dot" = {
           if (group != "variable") {
             chart = ggplot(data = data, aes_string(x = y, y = x))
           } else {
             chart = ggplot(data = data, aes_string(x = y, y = "value"))
           }
           chart = chart +
             geom_point(aes_string(col = group, fill = group),
                        size = 3, alpha = 0.75) +
             coord_flip() + xlab(y_lab) + ylab(x_lab) +
             theme(axis.text.x = element_text(hjust=0.5))
           if (group != "variable") {
              chart = chart + 
                scale_fill_manual(values = col_pallete) +
                scale_color_manual(values = col_pallete)
           } else {
             chart = chart + 
               scale_fill_manual(labels = legend_lab,
                                 values = col_pallete) +
               scale_color_manual(labels = legend_lab,
                                  values = col_pallete)
           }
           if (length(levels(data[, group])) == 1) {
             chart = chart + theme(legend.position = "none")
           }
         },
         "bot_dot" = {
           chart = ggplot(data = data, aes_string(x = y, y = x)) +
             geom_point(aes_string(col = group, fill = group),
                        size = 3, alpha = 0.75) +
             scale_fill_manual(values = col_pallete) +
             scale_color_manual(values = col_pallete) +
             coord_flip() + xlab(y_lab) + ylab(x_lab) +
             theme(axis.text.x = element_text(hjust=0.5))
           if (length(levels(data[, group])) == 1) {
             chart = chart + theme(legend.position = "none")
           }
         },
         "top_bot_dot" = {
           chart = ggplot(data = data, aes_string(x = y, y = x)) +
             geom_point(aes_string(col = group, fill = group),
                        size = 3, alpha = 0.75) +
             scale_fill_manual(values = col_pallete) +
             scale_color_manual(values = col_pallete) +
             coord_flip() + xlab(y_lab) + ylab(x_lab) +
             theme(axis.text.x = element_text(hjust=0.5))
           if (length(levels(data[, group])) == 1) {
             chart = chart + theme(legend.position = "none")
           }
         },
         "manual" = {
           chart = NULL
         })
  chart
}