#Load packages
library(rinat)

# Argument configuration
arguments <- list()
arguments$species <- c("Gyps coprotheres", "Torgos tracheliotos", "Gyps rueppelli")
arguments$maxresults <- 10000
arguments$quality <- NULL
arguments$year <- NULL
arguments$month <- NULL
arguments$day <- NULL
arguments$bounds <- c(-25.362,-50.019, 77.603,  37.560) # Africa's bounding box

# Create image folder from working directory
images_folder <- "./Images"
if (!dir.exists(images_folder)) { dir.create(images_folder) }

### Get observations
get_observations <- function(species = c(), maxresults = 10000,
                             quality = arguments$quality, year = arguments$year,
                             month = arguments$month, day = arguments$day,
                             bounds = arguments$bounds) {
  message(sprintf("Fetching data for %s", species))
  tryCatch(
    {
      inat_out <- get_inat_obs(taxon_name = species, maxresults = maxresults,
                               quality = quality, year = year,
                               month = month,  day = day,
                               bounds = bounds)
      Sys.sleep(2)
    },
    error = function(e) { print(paste("Couldn't find a match for", species)) }
  )
  return(inat_out)
}

inat_data <- sapply(X = arguments$species, FUN = get_observations, simplify = FALSE)
## Drop NA or "" url if exist
droped <- sapply(X = inat_data, FUN = function(x){
  x[which(!is.na(x$image_url) & x$image_url != ""), ]
}, simplify = F)
wrangled_inat_data <- do.call(rbind, droped)
## Sepcies scientific name
spc_scientific_name <- unique(wrangled_inat_data$scientific_name)

## Download image
for (spc in spc_scientific_name) {
  spc_obs_data <- base::subset(wrangled_inat_data, scientific_name == spc)

  image_url <- spc_obs_data$image_url
  photo_name <- paste(
    spc, spc_obs_data$user_login, spc_obs_data$license,
    spc_obs_data$id,
    gsub(" ", "_", gsub(":", "", substr(spc_obs_data$datetime, 1, 19))),
    sep = "_"
  )

  spc_img_folder <- paste0(images_folder, "/", spc)
  if (!dir.exists(spc_img_folder)) { dir.create(spc_img_folder) }

  for (i in 1:length(image_url)) {
    message(paste0("Downloading progress for " , spc, " : ", round(i*100/(length(image_url)), 2), "%"))
    dest_file <- paste0(spc_img_folder, "/", photo_name[i], ".jpeg")
    tryCatch({
      download.file(url = image_url[i], destfile = dest_file ,method = "curl")},
      error = function(e)print("WARNING: couldn't find the url"))
  }

}
