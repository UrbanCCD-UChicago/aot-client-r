
#' Timestamped message -- primarily used to push error output to user
#'
#' @param msg - The message to be logged
#' @return None (invisible NULL) as per cat
#' @noRd
log_msg <- function (msg) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3 "), ": ", msg, "\n", sep="")
}


#' Sends a request to the API, ensures 200 response and returns the response
#'
#' Given a URL and optional filters/query params, this sends an HTTP GET request
#' to the URL. The response"s status is checked -- if it isn"t 200 then an
#' error message is logged and the process halts; it it"s 200 then the entire
#' response object is returned.
#'
#' @param url - The URL to send the request to
#' @param filters - A list of tuples to build filters/query params
#' @return The entire response
#' @importFrom httr GET
#' @noRd
send_request <- function (url, filters = NULL) {
  # send request; get response
  if (!is.null(filters)) {
    resp <- httr::GET(url, query=filters)
  } else {
    resp <- httr::GET(url)
  }

  # if not 200, log error
  if (resp$status_code != 200) {
    msg <- paste("Error in httr GET:", resp$status_code, resp$headers$statusmessage, url)
    if(!is.null(resp$headers$`content-length`) && (resp$headers$`content-length` > 0)) {
      details <- httr::content(resp)
      msg <- paste(msg, details)
    }
    log_msg(msg)
  }

  # stop or return
  httr::stop_for_status(resp)
  return(resp)
}


#' Parses a response object as JSON and returns the `data` object
#'
#' @param resp - The response object
#' @return The parsed JSON body
#' @importFrom jsonlite fromJSON
#' @noRd
parse_content <- function (resp) {
  content <- httr::content(resp, as="text")
  json <- jsonlite::fromJSON(content)
  data <- json$data
  return(data)
}


#' Sends a request and parses the result as a single map object
#'
#' Given a URL and optional filters, a request is sent and the response
#' is processed as a single map object -- the response content has a
#' `data` key that maps an object representing details for the metadata
#' record requested.
#'
#' @param url - The URL to send the request to
#' @param filters - A list of tuples to build query params
#' @return The metadata details
#' @noRd
stat <- function (url, filters) {
  resp <- send_request(url, filters)
  details <- parse_content(resp)
  return(details)
}


#' Gets a data frame of `project` metadata
#'
#' Projects are the highest entity in the hierarchy of the Array of
#' Things system. They are generally ambiguous geographic regions used
#' to roughly group nodes.
#'
#' @param filters - A list of tuples to create filters/query params
#' @return A data frame of project metadata
#' @export
ls.projects <- function (filters = NULL) {
  # build url, send request, get response
  url <- "https://api.arrayofthings.org/api/projects"
  resp <- send_request(url, filters)

  # build data frame
  data <- parse_content(resp)
  df <- as.data.frame.list(data)
  attr(df, "name") <- data$name
  attr(df, "slug") <- data$slug
  attr(df, "hull") <- data$hull

  # return data frame
  return(df)
}


#' Gets the details for a single `project` metadata record
#'
#' Projects are the highest entity in the hierarchy of the Array of
#' Things system. They are generally ambiguous geographic regions used
#' to roughly group nodes.
#'
#' @param slug - The project"s unique identifier
#' @param filters - A list of tuples to create filters/query params
#' @return A list representing the project
#' @export
stat.project <- function (slug, filters = NULL) {
  # build url, send request, get response
  url <- paste("https://api.arrayofthings.org/api/projects/", slug, sep="")
  details <- stat(url, filters)
  return(details)
}


#' Gets a data frame of `node` metadata
#'
#' Nodes are the physical devices deployed to collect observations.
#' The are comprised of multiple sensors and are grouped by
#' projects.
#'
#' @param filters - A list of tuples to create filters/query params
#' @return A data frame of node metadata
#' @export
ls.nodes <- function (filters = NULL) {
  # build url, send request, get response
  url <- "https://api.arrayofthings.org/api/nodes"
  resp <- send_request(url, filters)

  # build data frame
  data <- parse_content(resp)
  df <- as.data.frame.list(data)
  attr(df, "vsn") <- data$vsn
  attr(df, "location") <- data$location
  attr(df, "address") <- data$human_address
  attr(df, "description") <- data$description

  # return data frame
  return(df)
}


#' Gets the details for a single `node` metadata record
#'
#' Nodes are the physical devices deployed to collect observations.
#' The are comprised of multiple sensors and are grouped by
#' projects.
#'
#' @param vsn - The node"s unique identifier
#' @param filters - A list of tuples to create filters/query params
#' @return A list representing the node
#' @export
stat.node <- function (vsn, filters = NULL) {
  url <- paste("https://api.arrayofthings.org/api/nodes/", vsn, sep="")
  details <- stat(url, filters)
  return(details)
}


#' Gets a data frame of `sensor` metadata
#'
#' Sensors are the physical boards inside the nodes that record
#' the observations. Sensors are in various states of being tuned
#' and therefor some of their observations are considered to be
#' experimental. Trustworthy data is listed under the _observations_
#' endpoint and experimental data is under _raw-observations_.
#'
#' @param filters - A list of tuples to create filters/query params
#' @return A data frame of sensor metadata
#' @export
ls.sensors <- function (filters = NULL) {
  # build url, send request, get response
  url <- "https://api.arrayofthings.org/api/sensors"
  resp <- send_request(url, filters)

  # build data frame
  data <- parse_content(resp)
  df <- as.data.frame.list(data)
  attr(df, "path") <- data$path
  attr(df, "uom") <- data$uom
  attr(df, "min") <- data$min
  attr(df, "max") <- data$max
  attr(df, "data_sheet") <- data$data_sheet

  # return data frame
  return(df)
}


#' Gets the details for a single `sensor` metadata record
#'
#' Sensors are the physical boards inside the nodes that record
#' the observations. Sensors are in various states of being tuned
#' and therefor some of their observations are considered to be
#' experimental. Trustworthy data is listed under the _observations_
#' endpoint and experimental data is under _raw-observations_.
#'
#' @param path - The sensor"s unique identifier
#' @param filters - A list of tuples to create filters/query params
#' @return A list representing the sensor
#' @export
stat.sensor <- function (path, filters = NULL) {
  url <- paste("https://api.arrayofthings.org/api/sensors/", path, sep="")
  details <- stat(url, filters)
  return(details)
}


#' Gets a data frame of `obserations` data.
#'
#' Observation data are the environmental measurements made
#' by the sensors. Data listed here is more or less tuned
#' and trustworthy.
#'
#' @param filters - A list of tuples to create filters/query params
#' @return A data frame of observation data
#' @export
ls.observations <- function (filters = NULL) {
  # build url, send request, get response
  url <- "https://api.arrayofthings.org/api/observations"
  resp <- send_request(url, filters)

  # build data frame
  data <- parse_content(resp)
  df <- as.data.frame.list(data)
  attr(df, "node_vsn") <- data$node_vsn
  attr(df, "sensor_path") <- data$sensor_path
  attr(df, "timestamp") <- as.POSIXlt(data$timestamp)
  attr(df, "value") <- data$value
  attr(df, "uom") <- data$uom
  attr(df, "location") <- data$location

  # return data frame
  return(df)
}
