mp_nmds <- function(x) {metaMDS(decostand(t(x), method = "hellinger"), distance = "jaccard")}
