#R code, dbRDA (distance-based), figure

library(vegan)
library(RColorBrewer)
full_data <- RDA

# First 9 columns are species data, rest are environmental
env_data <- full_data[, 1:8]  
species_data <- full_data[, 10:17]
str(species_data)
str(env_data)

# Convert categorical variables
env_data$site <- factor(env_data$site)
env_data$type <- factor(env_data$type)

#standardize environmental variables
env_data_scaled <- env_data
env_data_scaled[, c("width", "depth", "do", "ph", "ec")] <- scale(env_data_scaled[, c("elevation", "width", "depth", "do", "ph", "ec")])

#dbRDA
bray_dist <- vegdist(species_data, method = "bray")
dbrda_model <- capscale(bray_dist ~ site + type + elevation + width + depth + do + ph + ec, data = env_data_scaled, add = TRUE)

# Summary
summary(dbrda_model)

# ANOVA: test overall model
anova(dbrda_model, permutations = 999)

# ANOVA: test each term
anova(dbrda_model, by = "margin", permutations = 999)

# ANOVA: test each axis
anova(dbrda_model, by = "axis", permutations = 999)

#Only site & type were significant. Reanalyzed with reduced model

dbrda_final <- capscale(bray_dist ~ site + type, data = env_data_scaled, add = TRUE)
#overall significance
anova(dbrda_final, permutations = 999)
#indiv terms
anova(dbrda_final, by = "margin", permutations = 999)
#axes
anova(dbrda_final, by = "axis", permutations = 999)
summary(dbrda_final)


#Figure
# Expand right margin to make room for legend
par(xpd = TRUE, mar = c(5, 4, 4, 8))

# Extract factors for coloring
site_factor <- factor(env_data_scaled$site)
type_factor <- factor(env_data_scaled$type)

# Define colors
site_colors <- brewer.pal(length(levels(site_factor)), "Set2")
type_colors <- brewer.pal(3, "Dark2")[1:length(levels(type_factor))]

# Extract site scores for plotting
site_scores <- scores(dbrda_final, display = "sites", choices = c(1,2))

# Calculate species weighted averages
species_scores <- wascores(site_scores, species_data)

# Extract site scores for plotting
site_scores <- scores(dbrda_final, display = "sites", choices = c(1, 2))

# Base empty plot
plot(site_scores,
     type = "n",
     xlim = c(-3, 3),
     ylim = c(-4, 3),
     xlab = "CAP1 (46.1%)",
     ylab = "CAP2 (32.2%)",
     axes = FALSE,
     frame = TRUE)

# Manually add axes and reference lines
axis(1)
axis(2)
segments(x0 = -3, x1 = 3, y0 = 0, y1 = 0, lty = 2, col = "gray40")  # Horizontal
segments(x0 = 0, x1 = 0, y0 = -4, y1 = 3, lty = 2, col = "gray40")  # Vertical

# Add points for sites colored by site
points(dbrda_final, display = "sites", choices = c(1, 2), pch = 16, cex = 2,
       col = site_colors[as.numeric(site_factor)])

# Add legend outside the plot to the right
legend("topright", inset = c(-0.25, 0),
       legend = levels(site_factor),
       col = site_colors, pch = 16,
       title = "Site", cex = 1.5, bty = "n")

# Draw species vectors (arrows from origin)
arrow_scale <- 2  # adjust this if arrows are too short or too long
arrows(0, 0,
       species_scores[, 1] * arrow_scale,
       species_scores[, 2] * arrow_scale,
       length = 0.08, col = "gray30", lwd = 1.2)

# Add species labels
text(species_scores[, 1] * arrow_scale * 1.2,
     species_scores[, 2] * arrow_scale * 1.2,
     labels = rownames(species_scores),
     col = "gray30", cex = 1.2)

# Add ellipses without labels
ordiellipse(dbrda_final, groups = env_data_scaled$type, 
            display = "sites", 
            kind = "sd",
            draw = "polygon", 
            border = NA,
            col = type_colors[env_data$type],
            alpha = 0.2,
            label = FALSE)

# Manually calculate and add labels without borders
centroids <- aggregate(scores(dbrda_final, display = "sites", choices = c(1, 2)), 
                       by = list(type = env_data_scaled$type), FUN = mean)

text(centroids$CAP1, centroids$CAP2, labels = centroids$type, 
     col = type_colors[centroids$type], font = 2, cex = 1.75)

