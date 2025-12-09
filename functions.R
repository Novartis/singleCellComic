# ==============================================================================
#  Load libraries necessary for these functions
# ==============================================================================
library(ggplot2)
library(ggforce)
library(circlize)

# ==============================================================================
#  1. CELL PLOTTING FUNCTION (plotCellComic)
# ==============================================================================
plotCellComic <- function(cell.color.value = 500,
                          sunglasses.size = 0,
                          sunglass.shape.value = 0,
                          moustache.size = 0,
                          headphones.size = 0,
                          rollerblades.size = 0,
                          umbrella.size = 0,
                          umbrella.color.value = 500,
                          mouth.expression.value = 500,
                          hairstyle.value = 0,
                          hair.length = 500) {
  
  p <- ggplot() +
    coord_fixed(xlim = c(-1.5, 2.5), ylim = c(-1.5, 3.5)) +
    theme_void()
  
  rainbow_colors <- c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#8A2BE2")
  color_breaks <- seq(0, 1000, length.out = length(rainbow_colors))
  cell_color_map <- circlize::colorRamp2(color_breaks, rainbow_colors)
  umbrella_color_map <- circlize::colorRamp2(color_breaks, rev(rainbow_colors))
  final_cell_color <- cell_color_map(cell.color.value)
  final_umbrella_color <- umbrella_color_map(umbrella.color.value)
  rescale <- function(input_val, min_val, max_val) {
    min_val + (input_val / 1000) * (max_val - min_val)
  }
  
  visibility_threshold <- 35
  
  shape_to_draw <- ""
  if (is.na(sunglasses.size)) sunglasses.size <- 0
  if (sunglasses.size > visibility_threshold) {
    shape_to_draw <- if (sunglass.shape.value <= 200) { "rectangle"
    } else if (sunglass.shape.value <= 400) { "round"
    } else if (sunglass.shape.value <= 800) { "aviator"
    } else { "no-glass" }
  }
  
  if (is.na(umbrella.size)) umbrella.size <- 0
  if (umbrella.size > visibility_threshold) {
    umbrella_scale <- rescale(umbrella.size, 0.8, 1.2)
    p <- p +
      geom_segment(aes(x = 0.8*umbrella_scale, xend = 0.8*umbrella_scale, y = 1.8*umbrella_scale, yend = 0.2*umbrella_scale), size = 2, color = "black") +
      geom_curve(aes(x = 0.8*umbrella_scale, xend = (0.8 - 0.2)*umbrella_scale, y = 0.2*umbrella_scale, yend = 0.1*umbrella_scale), curvature = 1, size = 2, color = "black")
  }
  
  if (is.na(rollerblades.size)) rollerblades.size <- 0
  if (rollerblades.size > visibility_threshold) {
    rollerblade_scale <- rescale(rollerblades.size, 0.8, 1.2)
    p <- p +
      geom_rect(aes(xmin = -0.65*rollerblade_scale, xmax = -0.35*rollerblade_scale, ymin = -1.0, ymax = -0.7), fill = final_cell_color, color = "black", size = 1) +
      geom_rect(aes(xmin = 0.35*rollerblade_scale, xmax = 0.65*rollerblade_scale, ymin = -1.0, ymax = -0.7), fill = final_cell_color, color = "black", size = 1)
  }
  
  if (is.na(hairstyle.value)) hairstyle.value <- 0
  if (hairstyle.value > visibility_threshold) {
    hair_color <- "black"
    if (hairstyle.value <= 400) {
      num_spikes <- floor(rescale(hairstyle.value, 2, 5))
      spike_length <- rescale(hair.length, 1.1, 1.4)
      angles <- seq(pi/3, 2*pi/3, length.out = num_spikes)
      p <- p +
        geom_segment(aes(x = 0.9 * cos(angles), y = 0.9 * sin(angles),
                         xend = spike_length * cos(angles), yend = spike_length * sin(angles)),
                     color = hair_color, size = 2)
    } else if (hairstyle.value <= 700) {
      num_curls <- floor(rescale(hairstyle.value, 20, 50))
      afro_radius <- rescale(hair.length, 0.9, 1.2)
      angle_range <- seq(pi/4, 3*pi/4, length.out = num_curls)
      noise_r <- rnorm(num_curls, afro_radius, 0.15)
      noise_a <- rnorm(num_curls, 0, 0.1)
      p <- p +
        geom_point(aes(x = noise_r * cos(angle_range + noise_a),
                       y = noise_r * sin(angle_range + noise_a)),
                   color = hair_color, size = rescale(hairstyle.value, 8, 15), shape = 19)
    } else {
      y_end <- rescale(hair.length, -0.8, -1.4)
      p <- p +
        geom_curve(aes(x = -0.6, xend = -1.2, y = 0.8, yend = y_end), curvature = 0.6, color = hair_color, size = 3) +
        geom_curve(aes(x = -0.4, xend = -1.0, y = 0.9, yend = y_end), curvature = 0.5, color = hair_color, size = 3) +
        geom_curve(aes(x = 0.6, xend = 1.2, y = 0.8, yend = y_end), curvature = -0.6, color = hair_color, size = 3) +
        geom_curve(aes(x = 0.4, xend = 1.0, y = 0.9, yend = y_end), curvature = -0.5, color = hair_color, size = 3)
    }
  }
  
  p <- p + ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), fill = final_cell_color, color = "black", size = 1.5)
  
  p <- p +
    geom_curve(aes(x = -0.6, xend = -0.2, y = 0.55, yend = 0.45), curvature = -0.4, size = 2) +
    geom_curve(aes(x = 0.2, xend = 0.6, y = 0.45, yend = 0.55), curvature = -0.4, size = 2)
  
  if (is.na(sunglasses.size) || sunglasses.size <= visibility_threshold || shape_to_draw == "no-glass") {
    p <- p +
      ggforce::geom_circle(aes(x0 = -0.35, y0 = 0.2, r = 0.2), fill = "black") +
      ggforce::geom_circle(aes(x0 = -0.32, y0 = 0.23, r = 0.1), fill = "white") +
      ggforce::geom_circle(aes(x0 = 0.35, y0 = 0.2, r = 0.2), fill = "black") +
      ggforce::geom_circle(aes(x0 = 0.38, y0 = 0.23, r = 0.1), fill = "white")
  }
  
  mouth_curvature <- rescale(mouth.expression.value, 0.8, -0.8)
  mouth_y_position <- rescale(mouth.expression.value, -0.4, -0.7)
  
  p <- p + geom_curve(aes(x = -0.4, xend = 0.4, y = mouth_y_position, yend = mouth_y_position),
                      curvature = mouth_curvature, size = 1, angle = 90)
  
  if (!is.na(sunglasses.size) && sunglasses.size > visibility_threshold) {
    sunglass_scale <- rescale(sunglasses.size, 0.9, 1.1)
    if (shape_to_draw == "round") {
      r_scaled <- 0.3 * sunglass_scale
      p <- p +
        ggforce::geom_circle(aes(x0 = -0.35, y0 = 0.25, r = r_scaled), size = 1.5, fill = "black", color = "black") +
        ggforce::geom_circle(aes(x0 = 0.35, y0 = 0.25, r = r_scaled), size = 1.5, fill = "black", color = "black") +
        geom_segment(aes(x = -0.1*sunglass_scale, xend = 0.1*sunglass_scale, y = 0.25, yend = 0.25), size = 2)
    } else if (shape_to_draw == "aviator") {
      left_lens_base <- data.frame(x = c(-0.7, -0.1, -0.25, -0.55), y = c(0.45, 0.45, 0.0, 0.0))
      left_lens_scaled <- left_lens_base * sunglass_scale
      right_lens_scaled <- data.frame(x = -left_lens_scaled$x, y = left_lens_scaled$y)
      p <- p +
        geom_polygon(data = left_lens_scaled, aes(x = x, y = y), fill = "black") +
        geom_polygon(data = right_lens_scaled, aes(x = x, y = y), fill = "black") +
        geom_segment(aes(x = -0.7*sunglass_scale, xend = 0.7*sunglass_scale, y = 0.45*sunglass_scale, yend = 0.45*sunglass_scale), size = 1.5) +
        geom_curve(aes(x = -0.1*sunglass_scale, xend = 0.1*sunglass_scale, y = 0.3*sunglass_scale, yend = 0.3*sunglass_scale), curvature = -1, size = 1)
    } else if (shape_to_draw == "no-glass") {
      r_scaled <- 0.3 * sunglass_scale
      p <- p +
        ggforce::geom_circle(aes(x0 = -0.35, y0 = 0.25, r = r_scaled), size = 2, color = "black") +
        ggforce::geom_circle(aes(x0 = 0.35, y0 = 0.25, r = r_scaled), size = 2, color = "black") +
        geom_segment(aes(x = -0.1*sunglass_scale, xend = 0.1*sunglass_scale, y = 0.25, yend = 0.25), size = 2)
    } else { # "rectangle"
      w_half <- 0.3 * sunglass_scale
      h_half <- 0.225 * sunglass_scale
      center_y <- 0.22
      p <- p +
        geom_rect(aes(xmin = -0.4 - w_half, xmax = -0.4 + w_half, ymin = center_y - h_half, ymax = center_y + h_half), fill = "black", color = "black") +
        geom_rect(aes(xmin = 0.4 - w_half, xmax = 0.4 + w_half, ymin = center_y - h_half, ymax = center_y + h_half), fill = "black", color = "black") +
        geom_segment(aes(x = -0.1*sunglass_scale, xend = 0.1*sunglass_scale, y = center_y, yend = center_y), size = 2)
    }
  }
  
  p <- p + ggforce::geom_circle(aes(x0 = 0, y0 = -0.1, r = 0.05), fill = "black")
  
  if (is.na(moustache.size)) moustache.size <- 0
  if (moustache.size > visibility_threshold) {
    moustache_scale <- rescale(moustache.size, 0.5, 1.5)
    p <- p +
      geom_curve(aes(x = -0.6*moustache_scale, xend = 0, y = -0.3, yend = -0.3), curvature = -0.6, size = 1.5*moustache_scale) +
      geom_curve(aes(x = -0.5*moustache_scale, xend = 0, y = -0.325, yend = -0.325), curvature = -0.6, size = 1.5*moustache_scale) +
      geom_curve(aes(x = -0.4*moustache_scale, xend = 0, y = -0.35, yend = -0.35), curvature = -0.6, size = 1.5*moustache_scale)
    p <- p +
      geom_curve(aes(x = 0, xend = 0.6*moustache_scale, y = -0.3, yend = -0.3), curvature = -0.6, size = 1.5*moustache_scale) +
      geom_curve(aes(x = 0, xend = 0.5*moustache_scale, y = -0.325, yend = -0.325), curvature = -0.6, size = 1.5*moustache_scale) +
      geom_curve(aes(x = 0, xend = 0.4*moustache_scale, y = -0.35, yend = -0.35), curvature = -0.6, size = 1.5*moustache_scale)
  }
  
  if (is.na(headphones.size)) headphones.size <- 0
  if (headphones.size > visibility_threshold) {
    headphone_scale <- rescale(headphones.size, 0.8, 1.2)
    p <- p +
      ggforce::geom_ellipse(aes(x0 = -1.025, y0 = 0, a = 0.25*headphone_scale, b = 0.3*headphone_scale, angle = 0), fill = "gray40", color = "black", size = 1) +
      ggforce::geom_ellipse(aes(x0 = 1.025, y0 = 0, a = 0.25*headphone_scale, b = 0.3*headphone_scale, angle = 0), fill = "gray40", color = "black", size = 1) +
      geom_curve(aes(x = -1.025, xend = 1.025, y = 0.3*headphone_scale, yend = 0.3*headphone_scale), curvature = -1.2, size = 3, color = "gray20")
  }
  
  if (is.na(rollerblades.size)) rollerblades.size <- 0
  if (rollerblades.size > visibility_threshold) {
    rollerblade_scale <- rescale(rollerblades.size, 0.8, 1.2)
    p <- p +
      geom_rect(aes(xmin = -0.65*rollerblade_scale, xmax = -0.25*rollerblade_scale, ymin = -1.3*rollerblade_scale, ymax = -1.0*rollerblade_scale), fill = "gray50", color = "black", size = 1) +
      geom_rect(aes(xmin = 0.25*rollerblade_scale, xmax = 0.65*rollerblade_scale, ymin = -1.3*rollerblade_scale, ymax = -1.0*rollerblade_scale), fill = "gray50", color = "black", size = 1) +
      ggforce::geom_circle(aes(x0 = -0.55*rollerblade_scale, y0 = -1.3*rollerblade_scale, r = 0.15*rollerblade_scale), fill = "black") +
      ggforce::geom_circle(aes(x0 = -0.35*rollerblade_scale, y0 = -1.3*rollerblade_scale, r = 0.15*rollerblade_scale), fill = "black") +
      ggforce::geom_circle(aes(x0 = 0.35*rollerblade_scale, y0 = -1.3*rollerblade_scale, r = 0.15*rollerblade_scale), fill = "black") +
      ggforce::geom_circle(aes(x0 = 0.55*rollerblade_scale, y0 = -1.3*rollerblade_scale, r = 0.15*rollerblade_scale), fill = "black")
  }
  
  if (is.na(umbrella.size)) umbrella.size <- 0
  if (umbrella.size > visibility_threshold) {
    umbrella_scale <- rescale(umbrella.size, 0.8, 1.2)
    p <- p +
      ggforce::geom_arc_bar(aes(x0 = 0.8*umbrella_scale, y0 = 1.8*umbrella_scale, r0 = 0, r = 1.2*umbrella_scale, start = -pi/2, end = pi/2),
                            fill = final_umbrella_color, color = "black", size = 1.5) +
      geom_segment(aes(x = 0.8*umbrella_scale, xend = 0.8*umbrella_scale, y = (1.8+1.2)*umbrella_scale, yend = (1.8+1.2+0.2)*umbrella_scale),
                   size = 1.5, color = "black")
  }
  
  return(p)
}

# ==============================================================================
#  2. DATA GENERATION FUNCTION (generate_cell_data)
# ==============================================================================
generate_cell_data <- function(n_cells, sub_cell_type, batch_name) {
  df <- data.frame(
    cell.color.value = sample(300:700, n_cells, replace = TRUE),
    mouth.expression.value = sample(300:700, n_cells, replace = TRUE),
    sunglasses.size = 0,
    sunglass.shape.value = 0,
    moustache.size = 0,
    headphones.size = 0,
    rollerblades.size = 0,
    umbrella.size = 0,
    umbrella.color.value = 500,
    hairstyle.value = 0,
    hair.length = 0
  )
  
  # Use sub_cell_type to define features
  if (sub_cell_type == "Helper T-Cell") {
    df$moustache.size <- sample(100:500, n_cells, replace = TRUE)
    df$mouth.expression.value <- sample(0:500, n_cells, replace = TRUE)
    df$hairstyle.value <- sample(100:300, n_cells, replace = TRUE)
    df$hair.length <- sample(0:300, n_cells, replace = TRUE)
    df$cell.color.value <- sample(800:1000, n_cells, replace = TRUE)
  } else if (sub_cell_type == "Cytotoxic T-Cell") {
    df$moustache.size <- sample(300:800, n_cells, replace = TRUE)
    df$mouth.expression.value <- sample(900:1000, n_cells, replace = TRUE)
    df$hairstyle.value <- sample(100:300, n_cells, replace = TRUE)
    df$hair.length <- sample(0:300, n_cells, replace = TRUE)
    df$cell.color.value <- sample(800:1000, n_cells, replace = TRUE)
  } else if (sub_cell_type == "B-Cell") {
    df$moustache.size <- sample(200:500, n_cells, replace = TRUE)
    df$headphones.size <- sample(700:1000, n_cells, replace = TRUE)
    df$mouth.expression.value <- sample(0:600, n_cells, replace = TRUE)
    df$cell.color.value <- sample(200:700, n_cells, replace = TRUE)
    df$hairstyle.value <- sample(0:100, n_cells, replace = TRUE)
  } else if (sub_cell_type == "Macrophage") {
    df$moustache.size <- sample(400:1000, n_cells, replace = TRUE)
    df$cell.color.value <- sample(0:400, n_cells, replace = TRUE)
    df$hairstyle.value <- sample(400:600, n_cells, replace = TRUE)
    df$mouth.expression.value <- sample(400:600, n_cells, replace = TRUE)
    df$umbrella.color.value <- sample(100:500, n_cells, replace = TRUE)
  } else if (sub_cell_type == "Dendritic-Cell") {
    df$sunglasses.size <- sample(50:400, n_cells, replace = TRUE)
    df$sunglass.shape.value <- sample(0:200, n_cells, replace = TRUE)
    df$hairstyle.value <- sample(800:1000, n_cells, replace = TRUE)
    df$hair.length <- sample(700:1000, n_cells, replace = TRUE)
    df$mouth.expression.value <- sample(400:600, n_cells, replace = TRUE)
    df$cell.color.value <- sample(400:800, n_cells, replace = TRUE)
  }
  
  has_rollerblade_noise <- sample(c(TRUE, FALSE), n_cells, replace = TRUE, prob = c(0.1, 0.9))
  has_umbrella_noise <- sample(c(TRUE, FALSE), n_cells, replace = TRUE, prob = c(0.1, 0.9))
  df$rollerblades.size[has_rollerblade_noise] <- sample(1:200, sum(has_rollerblade_noise), replace = TRUE)
  df$umbrella.size[has_umbrella_noise] <- sample(1:200, sum(has_umbrella_noise), replace = TRUE)
  
  if (batch_name == "Batch_2") {
    effect_type <- sample(c("high", "low"), n_cells, replace = TRUE, prob = c(0.9, 0.1))
    high_values <- sample(900:1000, sum(effect_type == "high"), replace = TRUE)
    low_values <- sample(600:899, sum(effect_type == "low"), replace = TRUE)
    df$rollerblades.size[effect_type == "high"] <- high_values
    df$rollerblades.size[effect_type == "low"] <- low_values
  }
  
  if (batch_name == "Batch_3") {
    effect_type <- sample(c("high", "low"), n_cells, replace = TRUE, prob = c(0.9, 0.1))
    high_values <- sample(900:1000, sum(effect_type == "high"), replace = TRUE)
    low_values <- sample(600:899, sum(effect_type == "low"), replace = TRUE)
    df$umbrella.size[effect_type == "high"] <- high_values
    df$umbrella.size[effect_type == "low"] <- low_values
  }
  
  feature_cols <- setdiff(names(df), c("cell_type", "batch", "sub_cell_type"))
  noise_matrix <- matrix(sample(0:50, size = n_cells * length(feature_cols), replace = TRUE), nrow = n_cells)
  df[, feature_cols] <- df[, feature_cols] + noise_matrix
  df[, feature_cols][df < 0] <- 0
  df[, feature_cols][df > 1000] <- 1000
  
  # Create both sub_cell_type and main cell_type columns
  df$sub_cell_type <- sub_cell_type
  df$cell_type <- ifelse(sub_cell_type %in% c("Helper T-Cell", "Cytotoxic T-Cell"), "T-Cell", sub_cell_type)
  df$batch <- batch_name
  
  return(df)
}

# ==============================================================================
#  2. DYNAMIC DATA GENERATION FUNCTION (BLUEPRINT LOGIC)
# ==============================================================================
create_dynamic_cell_data <- function(n_total_cells, cell_type_structure, batch_names) {
  
  # --- 1. Define the Visual Feature Catalogue ---
  feature_catalogue <- list(
    # Hairstyles
    "Spiky_Hair" = list(feature = "hairstyle.value", range = c(100, 300)),
    "Afro_Hair" = list(feature = "hairstyle.value", range = c(400, 600)),
    "Long_Hair" = list(feature = "hairstyle.value", range = c(800, 1000)),
    "Any_Hair" = list(feature = "hairstyle.value", range = c(100, 1000)),
    
    # Hair Length
    "Short_Hair_Length" = list(feature = "hair.length", range = c(100, 400)),
    "Long_Hair_Length" = list(feature = "hair.length", range = c(700, 1000)),
    
    # Accessories
    "Headphones" = list(feature = "headphones.size", range = c(700, 1000)),
    "Sunglasses" = list(feature = "sunglasses.size", range = c(400, 800)),
    "Moustache" = list(feature = "moustache.size", range = c(400, 1000)),
    
    # Colors
    "Reddish_Color" = list(feature = "cell.color.value", range = c(850, 1000)),
    "Orangish_Color" = list(feature = "cell.color.value", range = c(700, 850)),
    "Yellowish_Color" = list(feature = "cell.color.value", range = c(550, 700)),
    "Greenish_Color" = list(feature = "cell.color.value", range = c(400, 550)),
    "Cyannish_Color" = list(feature = "cell.color.value", range = c(250, 400)),
    "Bluish_Color" = list(feature = "cell.color.value", range = c(100, 250)),
    "Purplish_Color" = list(feature = "cell.color.value", range = c(0, 100)),
    "Any_Color" = list(feature = "cell.color.value", range = c(0, 1000)),
    
    # Subtle Features
    "Happy_Mouth" = list(feature = "mouth.expression.value", range = c(0, 400)),
    "Sad_Mouth" = list(feature = "mouth.expression.value", range = c(600, 1000)),
    "Round_Glasses" = list(feature = "sunglass.shape.value", range = c(201, 400)),
    "Aviator_Glasses" = list(feature = "sunglass.shape.value", range = c(401, 800))
  )
  
  # --- 2. Auto-generate Blueprints ---
  cell_blueprints <- list()
  main_type_names <- names(cell_type_structure)
  
  # Create pools of available features to ensure uniqueness
  available_defining_features <- c("Spiky_Hair", "Afro_Hair", "Long_Hair", "Headphones", "Sunglasses", "Moustache", 
                                   "Reddish_Color", "Orangish_Color", "Yellowish_Color", "Greenish_Color", 
                                   "Cyannish_Color", "Bluish_Color", "Purplish_Color")
  available_subtle_features <- c("Happy_Mouth", "Sad_Mouth", "Round_Glasses", "Aviator_Glasses", "Short_Hair_Length", "Long_Hair_Length")
  
  for (main_type in main_type_names) {
    n_features_to_sample <- min(2, length(available_defining_features))
    chosen_defining <- sample(available_defining_features, n_features_to_sample)
    available_defining_features <- setdiff(available_defining_features, chosen_defining)
    
    chosen_subtle <- sample(available_subtle_features, 1)
    available_subtle_features <- setdiff(available_subtle_features, chosen_subtle)
    
    cell_blueprints[[main_type]] <- list(
      defining_features = chosen_defining,
      subtle_feature = chosen_subtle
    )
  }
  
  # --- 3. Create the base metadata dataframe ---
  all_sub_types <- unlist(lapply(names(cell_type_structure), function(ct) {
    if (length(cell_type_structure[[ct]]) == 0) return(ct) else return(cell_type_structure[[ct]])
  }))
  
  cells_per_subtype <- floor(n_total_cells / length(all_sub_types))
  
  metadata <- tibble(sub_cell_type = rep(all_sub_types, each = cells_per_subtype)) %>%
    mutate(cell_type = sapply(sub_cell_type, function(sct) {
      for (main_type in names(cell_type_structure)) {
        subtypes_in_main_type <- cell_type_structure[[main_type]]
        if (length(subtypes_in_main_type) == 0 && sct == main_type) return(main_type)
        if (sct %in% subtypes_in_main_type) return(main_type)
      }
    })) %>%
    sample_frac(1) %>%
    mutate(batch = sample(batch_names, n(), replace = TRUE))
  
  # --- 4. Generate feature data based on blueprints and catalogue ---
  all_features <- names(formals(plotCellComic))
  feature_data <- matrix(0, nrow = nrow(metadata), ncol = length(all_features), 
                         dimnames = list(NULL, all_features))
  
  for(i in 1:nrow(metadata)) {
    subtype <- metadata$sub_cell_type[i]
    maintype <- metadata$cell_type[i]
    blueprint <- cell_blueprints[[maintype]]
    
    for (def_feature_name in blueprint$defining_features) {
      catalogue_entry <- feature_catalogue[[def_feature_name]]
      feature_col <- catalogue_entry$feature
      val_range <- catalogue_entry$range
      feature_data[i, feature_col] <- sample(val_range[1]:val_range[2], 1)
    }
    
    subtypes_in_main <- if(length(cell_type_structure[[maintype]]) == 0) maintype else cell_type_structure[[maintype]]
    sub_idx <- which(subtypes_in_main == subtype)
    
    subtle_feature_name <- blueprint$subtle_feature
    catalogue_entry <- feature_catalogue[[subtle_feature_name]]
    feature_col <- catalogue_entry$feature
    full_range <- catalogue_entry$range
    
    sub_range_size <- floor((full_range[2] - full_range[1] + 1) / length(subtypes_in_main))
    sub_range_low <- full_range[1] + (sub_idx - 1) * sub_range_size
    sub_range_high <- sub_range_low + sub_range_size - 1
    
    feature_data[i, feature_col] <- sample(sub_range_low:sub_range_high, 1)
  }
  
  feature_df <- as_tibble(feature_data)
  
  # --- 5. Add batch effects ---
  batch_effect_features <- c("rollerblades.size", "umbrella.size")
  other_batches <- setdiff(batch_names, batch_names[1])
  
  for (i in seq_along(other_batches)) {
    batch_to_affect <- other_batches[i]
    feature_to_use <- batch_effect_features[i %% length(batch_effect_features)]
    feature_df[metadata$batch == batch_to_affect, feature_to_use] <- sample(900:1000, sum(metadata$batch == batch_to_affect), replace = TRUE)
  }
  
  # --- 6. Add random noise ---
  noise_matrix <- matrix(sample(0:50, size = nrow(feature_df) * ncol(feature_df), replace = TRUE), nrow = nrow(feature_df))
  feature_df <- feature_df + noise_matrix
  
  feature_df[feature_df < 0] <- 0
  feature_df[feature_df > 1000] <- 1000
  
  # --- 7. Format for return ---
  metadata <- as.data.frame(metadata)
  n_cells <- nrow(metadata)
  cell_ids <- paste0("cell_", 1:n_cells)
  
  rownames(metadata) <- cell_ids
  counts <- t(as.matrix(feature_df))
  colnames(counts) <- cell_ids
  
  return(list(counts = counts, metadata = metadata))
}

# ==============================================================================
#  3. HELPER FUNCTION TO CREATE SEURAT OBJECT
# ==============================================================================
create_seurat_from_data <- function(data_list) {
  
  seurat_obj <- CreateSeuratObject(
    counts = data_list$counts,
    meta.data = data_list$metadata
  )
  
  return(seurat_obj)
}

# ==============================================================================
#  4. HELPER FUNCTION TO PLOT CELL EXAMPLES FROM SEURAT OBJECT
# ==============================================================================
plot_cell_type_examples <- function(seurat_obj, n_cells_per_type) {
  
  metadata <- seurat_obj@meta.data
  unique_cell_types <- unique(metadata$cell_type)
  
  feature_data <- as.data.frame(t(as.matrix(GetAssayData(seurat_obj, slot = "counts"))))
  
  plot_rows <- lapply(unique_cell_types, function(ct) {
    
    cells_of_type <- rownames(metadata[metadata$cell_type == ct, ])
    
    n_to_sample <- min(n_cells_per_type, length(cells_of_type))
    if (n_to_sample == 0) return(NULL)
    sampled_cell_names <- sample(cells_of_type, n_to_sample)
    
    sampled_feature_data <- feature_data[sampled_cell_names, , drop = FALSE]
    
    comic_plots <- purrr::pmap(sampled_feature_data, plotCellComic)
    
    patchwork::wrap_plots(comic_plots, nrow = 1) +
      patchwork::plot_annotation(title = paste("Examples of:", ct))
  })
  
  plot_rows <- plot_rows[!sapply(plot_rows, is.null)]
  
  final_plot <- patchwork::wrap_plots(plot_rows, ncol = 1)
  
  return(final_plot)
}
