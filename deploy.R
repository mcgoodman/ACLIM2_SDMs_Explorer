
# API keys
nmfs_api_key <- Sys.getenv("NMFS_API_KEY")
shinyapps.io_api_token <- Sys.getenv("SHINYAPPSIO_API_TOKEN")
shinyapps.io_api_secret <- Sys.getenv("SHINYAPPSIO_API_SECRET")

# Register NMFS server
rsconnect::addServer(
  url = "https://connect.fisheries.noaa.gov/",
  name = "nmfspositconnect"
)

# Connect to NMFS server
rsconnect::connectApiUser(
  server = "nmfspositconnect",
  apiKey = nmfs_api_key
)

# Deploy to NMFS server
rsconnect::deployApp(
  appDir = ".",
  appName = "ACLIM2 SDMs Explorer",
  account = "maurice.goodman",
  server = "nmfspositconnect",
  forceUpdate = TRUE
)

# Connect to shinyapps.io
rsconnect::setAccountInfo(
  name = "mgoodman",
  token = shinyapps.io_api_token,
  secret = shinyapps.io_api_secret
)

# Deploy to shinyapps.io
rsconnect::deployApp(
  appDir = ".",
  appName = "aclim2_sdms_explorer",
  account = "mgoodman",
  server = "shinyapps.io",
  forceUpdate = TRUE
)

