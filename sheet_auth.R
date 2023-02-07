library(googlesheets4)

# Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = "SCAMP-screentime-Gform/.secrets/")

# Authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
list.files("SCAMP-screentime-Gform/.secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = "SCAMP-screentime-Gform/.secrets/", email = "advspiers@gmail.com")