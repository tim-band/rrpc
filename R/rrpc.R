globals <- new.env(parent = emptyenv())

#' Sends a progress update to the client.
#'
#' During a slow remote procedure call, call this to inform the client of
#' progress.
#' @param numerator The progress, out of [denominator]
#' @param denominator What the progress is out of. You could use this for the
#' number of known items to be completed so that each call increases either
#' the numerator (for more items done) and/or the denominator (for more items
#' discovered that need to be done). However, it is not necessary to do this,
#' you can reduce the numerator if you want.
#' @export
sendProgress <- function(numerator, denominator=1) {
  globals$ws$send(jsonlite::toJSON(list(
    type = "progress",
    id = globals$id,
    numerator = numerator,
    denominator = denominator
  )))
}

#' Sends informational text to the client.
#'
#' During a slow remote procedure call, call this to inform the client of
#' progress.
#' @param text The text to send
#' @export
sendInfoText <- function(text) {
  globals$ws$send(jsonlite::toJSON(list(
    type = "info",
    id = globals$id,
    text = text
  )))
}

# Calls fn with parameters params. rparams is a list of
# parameters that are not passed to fn (like controlling
# the plot format).
processMessage <- function(fn, params, rparams) {
  tryCatch({
    if ("rrpc.resultformat" %in% names(rparams)) {
      validateAndEncodePlotAs(rparams$rrpc.resultformat, function() {
        do.call(fn, params)
      })
    } else {
      list(
        error = NULL,
        result = list(
          data = do.call(fn, params),
          plot = NULL
        )
      )
    }
  },
  error = function(e) {
    print(paste("Error:", e$message))
    print(paste("call:", format(e$call)))
    list(
      error = list(message = e$message, code = -32000),
      result = NULL
    )
  })
}

rrpc <- function(interface) { function(ws) {
  ws$onMessage(function(binary, message) {
    envelope <- tryCatch({
      df <- jsonlite::fromJSON(message)
      method <- df$method
      params <- df$params
      pnames <- names(params)
      rnames <- pnames[grep("^rrpc\\.", pnames)]
      # all parameters whos names begin with "rrpc."
      rparams <- params[rnames]
      # remove names beginning "rrpc." from params
      params[rnames] <- NULL
      fn <- interface[[method]]
      if (is.null(fn)) {
        list(
          jsonrpc = "2.0",
          id = df$id,
          error <- list(message = "no such method", code = -32601)
        )
      } else {
        globals$ws <- ws
        globals$id <- df$id
        r <- processMessage(fn, params, rparams)
        env <- list(
          jsonrpc = "2.0",
          id = df$id,
          error = r$error
        )
        # Set result only if r$result is not NULL
        env$result <- r$result
        env
      }
    }, error = function(e) {
      list(
        jsonrpc = "2.0",
        id = NA,
        error = list(message = "JSON parse error", code = -32700)
      )
    })
    ws$send(jsonlite::toJSON(envelope, force = TRUE, digits = NA))
  })
}}

#' Makes and starts a server for serving R calculations
#'
#' @param interface List of functions to be served. The names of the elements
#' are the names that the client will use to call them.
#' @param host Interface to listen on (default is '0.0.0.0', that is, all
#' interfaces)
#' @param port Port to listen on
#' @param appDirs List of directories in which to find static files to serve
#' @param root Root of the app on the server (with trailing slash)
#' @return The server object, can be passed to [slStop]
#' @export
rrpcServer <- function(
  interface,
  host = '0.0.0.0',
  port = NULL,
  appDirs = NULL,
  root = "/"
) {
  paths <- list()
  paths[[paste0(root, "lang")]] <- httpuv::excludeStaticPath()
  existingFiles <- list()
  for(appDir in appDirs) {
    files <- list.files(appDir, recursive = TRUE)
    for (file in setdiff(files, existingFiles)) {
      paths[[paste0(root,file)]] <- file.path(appDir, file)
      if (file == "index.html" && !(root %in% names(paths))) {
        paths[[root]] <- file.path(appDir, file)
      }
    }
    existingFiles <- union(existingFiles, files)
  }
  app <- list(onWSOpen = rrpc(interface))
  app$staticPaths <- paths
  langs <- list.dirs(path = file.path(appDir, "locales"),
    full.names = FALSE,
    recursive = FALSE
  )
  app$call <- function(req) {
    al <- req$HTTP_ACCEPT_LANGUAGE
    als <- strsplit(al, ",", fixed=TRUE)[[1]]
    langPath <- c(sub(";.*", "", als), "en", langs[1])
    lang <- intersect(langPath, langs)[1]
    host <- req$HTTP_HOST
    path <- sub("^/lang/", paste0("/locales/", lang, "/"), req$PATH_INFO)
    list(
      status = 307L,
      headers = list("Location"=paste0(
        req$rook.url_scheme, "://",
        host,
        req$HTTP_SCRIPT_NAME, path
      )),
      body = ""
    )
  }
  if (is.null(port)) {
    port <- httpuv::randomPort(min = 8192, max = 40000, host = host)
  }
  httpuv::startServer(host = host, port = port, app = app)
}

#' Obtains the address that the server is listening on
#' 
#' @return protocol://address:port
getAddress <- function(server) {
  host <- server$getHost()
  port <- server$getPort()
  protocol <- "http://"
  if (grepl("://", host, fixed=TRUE)) {
    protocol <- ""
  }
  paste0(protocol, host, ":", port)
}

#' Opens a browser to look at the server
#'
#' @param server The server to browse to
browseTo <- function(server) {
  utils::browseURL(getAddress(server))
}

#' Renders a plot as a base64-encoded image
#'
#' @param device Graphics device function, such as [grDevices::png]
#'   or [grDevices::pdf]
#' @param mimeType Mime type for the data produced by `device`
#' @param width Width of the plot in units applicable to `device`
#' @param height Height of the plot in units applicable to `device`
#' @param plotFn Function to call to perform the plot
#' @return list with two keys, whose values can each be NULL:
#' 'plot' is a plot in HTML img src form and 'data' is a data frame or other
#' non-plot result.
#' @seealso [encodePlotAsPng()]
#' @seealso [encodePlotAsPdf()]
#' @export
encodePlot <- function(device, mimeType, width, height, plotFn) {
  tempFilename <- tempfile(pattern='plot', fileext='.tmp')
  device(file=tempFilename, width=as.numeric(width), height=as.numeric(height))
  data <- plotFn()
  plot <- NULL
  grDevices::dev.off()
  fileSize <- file.size(tempFilename)
  if (!is.na(fileSize)) {
    raw <- readBin(tempFilename, what="raw", n=fileSize)
    plot <- paste0("data:", mimeType, ";base64,", jsonlite::base64_enc(raw))
  }
  list(plot=plot, data=data)
}

validateAndEncodePlotAs <- function(format, plotFn) {
  if (!is.list(format)) {
    list(result=NULL, error="rrpc.resultformat specified but not as {type=[,height=,width=]}")
  } else {
    valid <- c('pdf', 'png', 'svg', 'csv')
    if (format$type %in% valid) {
      r <- encodePlotAs(format, plotFn)
      list(result=r, error=NULL)
    } else {
      validCount <- length(valid)
      errorText <- paste(
        "rrpc.resultformat type should be",
        paste(valid[1:validCount-1]),
        "or", valid[validCount]
      )
      list(result=NULL, error=errorText)
    }
  }
}

#' Renders a plot as a base64-encoded PNG
#'
#' The result can be set as the `src` attribute of an `img` element in HTML.
#'
#' @param format An object specifying the output, with the following members:
#' format$type is "png", "pdf" or "csv", and format$width and format$height are
#' the dimensions of the PDF (in inches) or PNG (in pixels) if appropriate.
#' @param plotFn Function to call to perform the plot
#' @return list with two keys, whose values can each be NULL:
#' 'plot' is a plot in HTML img src form and 'data' is a data frame or other
#' non-plot result.
#' @seealso [rrpcServer()]
#' @export
encodePlotAs <- function(format, plotFn) {
  type <- format$type
  if (is.null(type)) {
    stop("plot type not defined")
  }
  if (format$type == "csv") {
    downloadCsv(plotFn())
  } else if (format$type == "png") {
    encodePlot(grDevices::png, "image/png",
        format$width, format$height, plotFn)
  } else if (format$type == "svg") {
    encodePlot(grDevices::svg, "image/svg+xml",
        format$width, format$height, plotFn)
  } else if (format$type == "pdf") {
    encodePlot(grDevices::pdf, "application/pdf",
        format$width, format$height, plotFn)
  } else {
    stop(paste("Did not understand plot type", type))
  }
}

#' Encodes a data frame as a CSV file to be downloaded
#' @export
downloadCsv <- function(results) {
    forJson <- list()
    forJson$action <- "download"
    forJson$filename <- paste0(name, ".csv")
    raw <- utils::capture.output(utils::write.csv(results, stdout()))
    forJson$data <- paste0(
        "data:text/csv;base64,",
        jsonlite::base64_enc(raw))
    forJson
}
