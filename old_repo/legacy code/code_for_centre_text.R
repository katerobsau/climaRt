# centre text
temp_data = data.frame(lab_y = 20)
sunburst_1 = ggplot(temp_data) +
  geom_bar(aes(x=0, y= lab_y), fill='gray', stat='identity') +
  geom_label(aes(x=0, y = lab_y/2), label= "Test label", color='blue')

sunburst_1 + coord_polar('y')


base_plot <- ggplot(data = plot_data) + 
  geom_bar(aes(x = -1, y = -1), fill = "gray", stat = "identity") +
  geom_text(aes(x= -1, y = 0), label = "Test label", color = 'blue') 

base_plot

