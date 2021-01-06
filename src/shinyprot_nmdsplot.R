nmdsplot <- function(x, col, sh) {ggplot(data = x, mapping = aes(x = nmds_axis1, y = nmds_axis2)) +
    geom_point(mapping = aes(color = col, shape = sh))} 

# 

#shape = depthbin, text = sprintf("Sample: %s", sampleID)