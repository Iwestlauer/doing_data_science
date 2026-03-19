colors = c("orange","blue","yellow","green","purple","brown","red")

# color_expression = str_c(colors, collapse = "|")


color_expression <- str_c("\\b(", str_c(colors, collapse = "|"), ")\\b")

has_color <- str_subset(sentences, color_expression)

has_color

matches = str_extract(has_color,color_expression)
matches
matches_all = str_extract_all(has_color,color_expression, simplify = TRUE)
matches_all
class(matches_all)
matches_all = unlist(str_extract_all(has_color,color_expression))
matches_all
matchDF = data.frame(Colors = matches_all)
matchDF %>% ggplot(aes(x = Colors, fill = Colors)) + geom_bar()
matchDF %>% ggplot(aes(x = Colors, fill = Colors)) + geom_bar()+ scale_fill_manual(values=colors)
colors[order(colors)]
matchDF %>% ggplot(aes(x = Colors, fill = Colors)) + geom_bar()+ scale_fill_manual(values=colors[order(colors)])