#' Dynamic buffering projections
#'
#' @param input_sf Required. Input sf data.frame
#' @param buff_dist Required. Buffer distance
#'
#' @return Data.frame sf object with buffered geometry.
#' @export
#'
#' @examples
st_dynamic_buffer <- function(input_sf, buff_dist){

  # Assign UTM at the centroid of the shape.
  sf::st_agr(input_sf) = 'constant'
  df <- input_sf %>%
    dplyr::mutate(L1 = sf::st_coordinates(sf::st_centroid(.))[, 1],
                  L2 = sf::st_coordinates(sf::st_centroid(.))[, 2],
                  zone = dplyr::case_when(
                    (L2 >= 56 & L2 < 64 & L1 >= 3 & L1 < 12) ~ 32,
                     (L2 >= 72 & L2 < 84 & L1 >= 0 & L1 < 9) ~ 31,
                     (L2 >= 72 & L2 < 84 & L1 >= 9 & L1 < 21) ~ 33,
                     (L2 >= 72 & L2 < 84 & L1 >= 21 & L1 < 33) ~ 35,
                     (L2 >= 72 & L2 < 84 & L1 >= 33 & L1 < 42) ~ 37,
                     TRUE ~ (floor((L1 + 180)/6) %% 60) + 1
                    ),
                  prj = dplyr::case_when(
                    L2 >= 0 ~ paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs"),
                    TRUE ~ paste0("+proj=utm +zone=", zone, " +south", " +datum=WGS84 +units=m +no_defs")
                  )) %>%
    dplyr::group_by(prj)

  # Transform projection, buffer, transform back to 4326, merge
  dfs <- purrr::map2(1:dim(dplyr::group_keys(df))[1],
                     unlist(dplyr::group_keys(df)),
                     function(x, y){
    sf::st_transform(group_split(df)[[x]], y)
  }) %>%
    purrr::map(., ~ sf::st_buffer(., 1609.344*buff_dist) %>%
          sf::st_transform(4326) %>% sf::st_cast("MULTIPOLYGON")) %>%
    data.table::rbindlist() %>%
    sf::st_sf() %>%
    dplyr::select(-L1,-L2,-zone,-prj)

  return(dfs)
}
