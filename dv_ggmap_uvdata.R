# sessionInfo()
# R version 3.3.2 (2016-10-31)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: macOS Sierra 10.12.5
# 
# locale:
#   [1] zh_TW.UTF-8/zh_TW.UTF-8/zh_TW.UTF-8/C/zh_TW.UTF-8/zh_TW.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] dplyr_0.5.0    downloader_0.4 mapproj_1.2-5  maps_3.2.0    
# [5] ggmap_2.6.1    ggplot2_2.2.1 
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.11      plyr_1.8.4        bitops_1.0-6     
# [4] tools_3.3.2       digest_0.6.12     tibble_1.3.1     
# [7] gtable_0.2.0      lattice_0.20-34   png_0.1-7        
# [10] rlang_0.1.1       DBI_0.6-1         binman_0.1.0     
# [13] proto_1.0.0       stringr_1.2.0     RgoogleMaps_1.4.1
# [16] caTools_1.17.1    grid_3.3.2        R6_2.2.1         
# [19] jpeg_0.1-8        XML_3.98-1.7      RSelenium_1.7.1  
# [22] sp_1.2-4          reshape2_1.4.2    semver_0.2.0     
# [25] magrittr_1.5      scales_0.4.1      assertthat_0.2.0 
# [28] geosphere_1.5-5   colorspace_1.3-2  stringi_1.1.2    
# [31] lazyeval_0.2.0    openssl_0.9.6     munsell_0.4.3    
# [34] wdman_0.2.2       rjson_0.2.15
# install packages
pkgs.needs <- c("ggmap", "mapproj", "downloader", "dplyr")
pkgs.installed <- installed.packages()[,"Package"]
new.pkgs <- pkgs.needs[!(pkgs.needs %in% pkgs.installed)]
if(length(new.packages)) install.packages(new.pkgs)  
# install.packages("ggmap", type = "source")
## run this line if you see error message like below:
## GeomRasterAnn was built with an incompatible version of ggproto.
## Please reinstall the package that provides this extension.
library(ggmap)
library(mapproj)
library(downloader) # download
library(dplyr)

# define function
rm.quot <- function(x) {x %>% gsub('"', '', .) %>% return}

# main prog
## load data
url <- "https://blog.gtwang.org/wp-content/uploads/2015/11/UV_20151116152215.csv"
fileName <- "UV_20151116152215.csv"
download(url, fileName, mode = "wb")
uv <- readLines("UV_20151116152215.csv", encoding = "utf8")
if (file.exists(fileName)) file.remove(fileName)

## tidy data
uv = uv %>% iconv("utf8", "utf8")
for (i in c(2:length(uv))) {
  t <- uv[i] %>% gregexpr('\"', .)
  s1 <- t[[1]][1]
  e1 <- t[[1]][2]
  s2 <- t[[1]][3]
  e2 <- t[[1]][4]
  rep1 = substr(uv[i], s1, e1) %>% gsub(",", ".", .)
  rep2 = substr(uv[i], s2, e2) %>% gsub(",", ".", .)
  uv[i] <- paste0(substr(uv[i], 1, s1), rep1, ",", rep2, substr(uv[i], e2, nchar(uv[i])))
}
df = uv %>% strsplit(., split=',', fixed=TRUE)
df = data.frame(matrix(unlist(df), nrow=length(df), byrow=T)) %>% lapply(., rm.quot) %>% as.data.frame
colnames(df) <- unlist(df[1,]) %>% as.character
df = df[-1, ]
df$UVI <- df$UVI %>% as.character %>% as.numeric
lon.deg <- sapply((strsplit(as.character(df$WGS84Lon), ".", fixed = TRUE)), as.numeric)
for (i in c(1:length(lon.deg))) {
  df$lon[i] <- lon.deg[[i]][1] + lon.deg[[i]][2]/60 + lon.deg[[i]][3]/3600
}
lat.deg <- sapply((strsplit(as.character(df$WGS84Lat), ".", fixed = TRUE)), as.numeric)
for (i in c(1:length(lat.deg))) {
  df$lat[i] <- lat.deg[[i]][1] + lat.deg[[i]][2]/60 + lat.deg[[i]][3]/3600
}

## ploting
map <- get_map(location = 'Taiwan', zoom = 7)
ggmap(map) + geom_point(aes(x = lon, y = lat, size = UVI), data = df)
