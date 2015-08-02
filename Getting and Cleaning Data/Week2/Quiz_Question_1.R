library(httr)
library(httpuv)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("Getting_Cleaning_Data",
                   key = "1bf8dc667aa288b53643",
                   secret = "8b9c81a8c3854ac60b9d84a54b663e81f47880a9")
# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
library(jsonlite)

jsondata1 <- content(req)
jsondata2 <- fromJSON(toJSON(jsondata1))

jsondata2$created_at[jsondata2$name == "datasharing"]
