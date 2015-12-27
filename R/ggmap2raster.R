#' Rasterize a 'ggmap'
#' 
#' @description 
#' \code{ggmap2raster} converts a 'ggmap' object (created via 
#' \code{\link{get_map}}) to a multi-layered (i.e., red-green-blue) 
#' 'RasterStack' object.
#' 
#' @param map A 'ggmap' object.
#' @param crs Optional output coordinate reference system (CRS) passed on to 
#' \code{\link{projectRaster}}.
#' 
#' @return A red-green-blue 'RasterStack' object.
#' 
#' @seealso \code{\link{get_map}}, \code{\link{projectRaster}}.
#' 
#' @author Florian Detsch (\email{florian.detsch@staff.uni-marburg.de})
#' 
#' @examples 
#' ## sample data
#' map_world <- get_map(location = "world")
#' 
#' ## convert 'ggmap' to 'RasterStack'
#' ggmap2raster(map_world)
#' 
#' @export ggmap2raster
#' @aliases ggmap2raster
ggmap2raster <- function(map, crs = NULL) {
  
  ## create raster template 
  crd <- as.numeric(attr(map, "bb")[c(2, 4, 1, 3)])
  crd <- data.frame(lon = crd[1:2], lat = crd[4:3], corner = c("ul", "lr"))
  sp::coordinates(crd) <- ~ lon + lat
  sp::proj4string(crd) <- "+init=epsg:4326"
  crd <- sp::spTransform(crd, CRS = sp::CRS("+init=epsg:3857"))
  ext <- raster::extent(crd)
  
  mat <- as.matrix(map)
  rst <- raster::raster(nrows = nrow(mat), ncols = ncol(mat), ext = ext, 
                        crs = "+init=epsg:3857")
  
  rst_r <- rst_g <- rst_b <- rst
  
  ## convert hexadecimal colors to rgb format
  lst <- lapply(1:nrow(mat), function(i) {
    lst_sub <- lapply(1:ncol(mat), function(j) t(col2rgb(mat[i, j])))
    do.call("rbind", lst_sub)
  })
  
  dat <- do.call("rbind", lst)
  
  ## insert values
  rst_r <- raster::setValues(rst_r, dat[, 1])
  rst_g <- raster::setValues(rst_g, dat[, 2])
  rst_b <- raster::setValues(rst_b, dat[, 3])
  
  ## stack bands
  rgb <- raster::stack(rst_r, rst_g, rst_b) 
  names(rgb) <- c("red", "green", "blue")
  
  ## reproject (optional) and return rgb stack
  if (!is.null(crs)) {
    rgb <- raster::projectRaster(rgb, crs = crs, method = "ngb")
    rgb <- raster::trim(rgb)
    rgb[is.na(rgb[])] <- 0
  }
  
  return(rgb)
}
