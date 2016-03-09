library(httr)

img <- "https://avatars2.githubusercontent.com/u/1356007?v=3&s=420"

# The free Betaface API key
betaface <- function(img, key="d45fd466-51e2-4701-8da8-04351c872236",
                      secret="171e8465-f548-401d-b63b-caf0dc28df5f"){
  res <- POST("http://betafaceapi.com/service_json.svc/UploadNewImage_Url", body=list(
    api_key = key,
    api_secret = secret,
    detection_flags = "bestface classifiers",
    image_url = img
  ))
  
  stop_for_status(res)
  
  uid <- content(res)$img_uid
  
  res <- POST("http://betafaceapi.com/service_json.svc/GetImageInfo", body=list(
    api_key = key,
    api_secret = secret,
    img_uid = uid
  ))
  
  stop_for_status(res)
  
  cont <- content(res)
  if (is.null(cont$faces) || length(cont$faces) == 0){
    stop("No face found")
  }
  
  face <- cont$faces[[1]]
  do.call(rbind.data.frame, face$tags)
}


ms_key <- readLines("ms_key.txt")
msface <- function(img, key){
  url <- "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=0&returnFaceLandmarks=0&returnFaceAttributes=age,gender,facialHair,smile"  
  res <- POST(url, add_headers("Ocp-Apim-Subscription-Key"=key), body=list(url=img), encode="json")
  
  while (res$status_code == 429){
    # Rate limit. Sleep and try again
    message("Hit rate limit. Sleeping for 60 seconds")
    Sys.sleep(60)
    res <- POST(url, add_headers("Ocp-Apim-Subscription-Key"=key), body=list(url=img), encode="json")
  }
  
  stop_for_status(res)
  
  cont <- content(res)
  if (length(cont) < 1 || is.null(cont[[1]]$faceAttributes) || length(cont[[1]]$faceAttributes) == 0){
    stop("No face attributes")
  }
  
  att <- cont[[1]]$faceAttributes
}