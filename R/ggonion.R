
#' Draw onion diagram
#'
#' @param x vector of labels for onion diagramm
#' @param ratio (optional) rmodificator of nested circle size, default is 2
#' @param bias (optional) vertical bias for nested circles, default is 0
#' @param gameofthrones gameofthrones theme
#' @param color (optional) vector of colors for circles
#'
#' @return
#' @export
#' @import ggplot2 dplyr
#'
#' @examples
ggonion <- function(x, ratio = 2, bias = 0, gameofthrones = NA, color = NA){
  
  df <- prepare_circles_df(x, ratio, bias)
  cdata <- make_circle_polygons(df, gameofthrones, color)

  # Behold some circles
  ggplot(cdata, aes(x = x, y = y, group=desc(r))) +
    geom_polygon(fill=cdata$clrs) +
    geom_path(aes(group = label)) +
    ggplot2::theme_void() +
    ggplot2::geom_text(aes(x = x_label, y = y_label, label = label))+
    scale_size(range = c(1, -1))
  #ggplot2::theme(legend.position="none") #+
  #scale_fill_manual(values = clrs)
  
  #  return(res)
}

#' Prepare dataframe with circles parameters
#'
#' @param x vector of classes for onion diagramm
#' @param bias vertical bias of nested circles
#' @param ratio modificator of nested circle size
#'
#' @return df[label,x0,y0,r,x_label,y_label]
#' @import dplyr
#'
#' @examples
prepare_circles_df <- function(x, ratio, bias){
  n_x <- length(x)
  
  big_radius <- 1
  x_0 <- 0
  y_0 <- 0
  
  min_r <- ((2 * big_radius)/((2*n_x) - 1)) * 0.5
  
  cr <- seq(min_r * ratio, big_radius, length.out = n_x)
  
  
  df <- data.frame(
    label = x,
    x0 = rep(x_0, n_x),
    y0 = rep(y_0, n_x),
    r = cr,
    x_label = rep(x_0, n_x),
    y_label = rep(y_0, n_x)
  )
  
  df <- df %>%
    arrange(desc(r))
  
  df <- iterative_get_y0(df, bias)
  df <- iterative_get_y_label(df)
  
  
  return(df)
}

make_circle_polygons <- function(prep_circles_df, gameofthrones, color){
  
  clrs <- make_color(prep_circles_df, gameofthrones, color)
  
  prep_circles_df$clrs <- clrs
  
  nms <- colnames(prep_circles_df)
  
  res_list <- prep_circles_df %>%
    select(x0, y0, r) %>%
    purrr::pmap(circleFun) %>%
    setNames(prep_circles_df$label) %>%
    bind_rows(.id = "label")
  
  res_df <- left_join(res_list, prep_circles_df, by = "label")
  
  return(res_df)
}

#' Hlpr to iterative calculate y coord bias for circles center using previous circle precalculated value
#'
#' @param df
#' @param bias bias [0:1]
#'
#' @return
#'
#' @examples
iterative_get_y0 <- function(df, bias){
  
  nr <- nrow(df)
  if(nr<2){
    return(df)
  }
  
  for (i_row in c(2:nr)) {
    df$y0[i_row] <- df$y0[i_row-1] - ((df$r[i_row-1] - df$r[i_row])*bias)
  }
  
  return(df)
}


#' Calculate labels y coord as middle between upper half of circles
#'
#' @param df
#'
#' @return df with y_label coord
#' @import dplyr
#'
#' @examples
iterative_get_y_label <- function(df){
  nr <- nrow(df)
  
  df <- df %>%
    mutate(y_label = (((y0+r)+(lead(y0)+lead(r)))/2))
  
  df$y_label[nr] <- df$y0[nr]
  
  return(df)
}

#' Draw circle https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
#'
#' @param center
#' @param diameter
#' @param npoints
#'
#' @return
#'
#' @examples
circleFun <- function(x0, y0, r = 1, npoints = 100){
  
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- x0 + r * cos(tt)
  yy <- y0 + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#' Check color vector if provided, if not - generate colors
#'
#' @param prep_circles_df df from prepare_circles_df function
#' @param gameofthrones gameofthrones theme
#' @param color color vector
#'
#' @return color vector
#' @import cli
#'
#' @examples
make_color <- function(prep_circles_df, gameofthrones, color){
  if(!is.na(gameofthrones)){
    if(tolower(gameofthrones) %in% names(got_palettes)){
      nclrs <- nrow(prep_circles_df)
      clrs <- got(nclrs, option = gameofthrones)
    } else {
      cli::cli_alert_warning(paste0("You provided gameofthrones value of ",gameofthrones," There is no such palette "))
      cli::cli_alert_info("Using default palette Daenerys")
      nclrs <- nrow(prep_circles_df)
      clrs <- got(nclrs, option = "Daenerys")
    }
  } else {
    if(all(is.na(color))){
      nclrs <- nrow(prep_circles_df)
      clrs <- rainbow(nclrs)
    } else {
      if(length(color)!=nrow(prep_circles_df)){
        cli::cli_alert_warning(paste0("You provided color vector of length ",length(color)," while yor data is length of ", nrow(prep_circles_df)))
        cli::cli_alert_info("Using default palette")
        nclrs <- nrow(prep_circles_df)
        clrs <- rainbow(nclrs)
      } else {
        clrs <- color
      }
    }
  }
  clrs <- rev(clrs) # due to vector of circles is reversed cause the biggest circle is at the lowest visual layer
  return(clrs)
}

