library(magrittr)

interpolate_pw <- function(from,
                           to,
                           from_id = "GEOID",
                           to_id = "GEOID",
                           extensive,
                           weights,
                           weight_column,
                           crs = NULL) {
  
  # If CRS is given, transform all the objects
  if (!is.null(crs)) {
    from <- sf::st_transform(from, crs)
    to <- sf::st_transform(to, crs)
    weights <- sf::st_transform(weights, crs)
  }
  
  # If from_id and to_id are the same, modify them
  if (from_id == to_id) {
    names(from)[names(from) == from_id] <- paste0(from_id, "_from")
    from_id <- paste0(from_id, "_from")
    names(to)[names(to) == to_id] <- paste0(to_id, "_to")
    to_id <- paste0(to_id, "_to")
  }
  
  # Convert strings to symbols for tidy evaluation
  weight_sym <- rlang::sym(weight_column)
  from_id_sym <- rlang::sym(from_id)
  to_id_sym <- rlang::sym(to_id)
  
  # Convert the input weights to centroids
  weight_centroids <- suppressWarnings(weights %>%
    dplyr::select(!!weight_sym) %>%
    sf::st_centroid())
  
  # Determine the denominator for the weights
  denominators <- from %>%
    sf::st_join(weight_centroids) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!from_id_sym) %>%
    dplyr::summarize(total = sum(!!weight_sym, na.rm = TRUE))
  
  # Calculate the intersections and intersection proportions
  intersections <- suppressWarnings(from %>%
    dplyr::left_join(denominators, by = from_id) %>%
    sf::st_intersection(to) %>%
    dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) %>%
    dplyr::mutate(intersection_id = dplyr::row_number()) %>%
    sf::st_join(weight_centroids) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(intersection_id) %>%
    dplyr::mutate(intersection_value = sum(!!weight_sym, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(intersection_id, .keep_all = TRUE) %>%
    dplyr::mutate(weight_coef = intersection_value / total) %>%
    dplyr::select(!!from_id_sym, !!to_id_sym, weight_coef))
  
  # Merge the weights to the from data and interpolate any numeric columns in from to to
  if (extensive) {
    interpolated <- from %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(intersections, by = from_id) %>%
      dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), 
                                  .fns = ~(.x * weight_coef))) %>%
      dplyr::select(-weight_coef) %>%
      dplyr::group_by(!!to_id_sym) %>%
      dplyr::summarize(dplyr::across(tidyselect:::where(is.numeric), 
                                     .fns = ~sum(.x, na.rm = TRUE))) 
  } else {
    interpolated <- from %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(intersections, by = from_id) %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), 
                                  .fns = ~(.x * weight_coef))) %>%
      dplyr::select(-weight_coef) %>%
      dplyr::group_by(!!to_id_sym) %>%
      dplyr::summarize(dplyr::across(tidyselect::where(is.numeric), 
                                     .fns = ~mean(.x, na.rm = TRUE))) %>%
      dplyr::rename_with(!!to_id_sym, ~stringr::str_remove(.x, "_to"))
  }
  
  # Merge back to the original "to" shapes
  output_shapes <- to %>%
    dplyr::select(!!to_id_sym) %>%
    dplyr::left_join(interpolated, by = to_id) %>%
    dplyr::rename_with(.cols = !!to_id_sym, 
                       .fn = ~stringr::str_remove(.x, "_to"))
  
  return(output_shapes)
  
  
}