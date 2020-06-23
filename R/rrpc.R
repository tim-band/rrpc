#' Returns the function that sets up the WebSocket
#' 
#' If calling the `httpuv::startServer` function manually, `rrpc`
#' returns the function to provde as the onWSOpen parameter.
#' @param interface A list of named functions that can will be
#' able to be called over RPC. Each function has parameters
#' that will be fed from the params object passed over RPC,
#' the key in the params object matching the parameter name.
#' @export
#' @examples
#' httpuv::startServer(host='127.0.0.1', port=8888, app=list(
#'   onWSOpen=rrpc(list(
#'     add=function(a,b) {
#'       a + b
#'     },
#'     multiply=function(a,b) {
#'       a * b
#'     }
#'   ))
#' ))
rrpc <- function(interface) { function(ws) {
    ws$onMessage(function(binary, message) {
        df <- jsonlite::fromJSON(message);
        method <- df$method
        envelope <- list()
        envelope$jsonrpc <- "2.0"
        envelope$id <- df$id
        if (is.null(interface[[method]])) {
            envelope$error <- "no such method"
            envelope$result <- NULL
        } else {
            envelope$result <- do.call(interface[[method]], df$params)
        }
        ws$send(jsonlite::toJSON(envelope))
    })
}}

#' Starts a server that provices a JsonRPC interface to your R functions
#' 
#' Sets up a WebSocket interface to your R functions, and also optionally
#' serves some static files as a normal HTTP server. In this way a simple
#' web interface can be made over your R functions.
#' 
#' You must repeatedly call `later::run_now` after calling this function
#' to drain the queue of messages.
#' @param interface The list of functions that the web interface gives
#' access to.
#' @param host The interface to listen on (for example `127.0.0.1`).
#' Defaults to all interfaces.
#' @param port The port to listen on.
#' @param appDir The directory containing static files to serve, if desired.
#' @param root The path for the static files to appear on.
#' @export
#' @examples
#' rrpcServer(host=host, port=port, interface=list(
#'   run=function(data) {
#'       run(data)
#'   },
#'   plot=function(data, width, height) {
#'        obj <- list()
#'        obj$results <- run(data)
#'        tempFilename <- tempfile(pattern='plot', fileext='png')
#'        device(file=tempFilename, width=width, height=height)
#'        scatterplot(obj$results)
#'        dev.off()
#'        fileSize <- file.size(tempFilename)
#'        raw <- readBin(tempFilename, what="raw", n=fileSize)
#'        obj$src <- paste0("data:", mimeType, ";base64,", jsonlite::base64_enc(raw))
#'        obj
#'   }
#' ))
#' cat(sprintf("Listening on %s:%d\n", host, port))
#' while (TRUE) {
#'   later::run_now(9999)
#' }
rrpcServer <- function(interface, host='0.0.0.0', port=8080, appDir=NULL, root="/") {
    app <- list(onWSOpen=rrpc(interface))
    if (!is.null(appDir)) {
        paths <- list()
        paths[[root]] <- appDir
        app$staticPaths <- paths
    }
    httpuv::startServer(host=host, port=port, app=app)
}

#' Renders a plot as a base64-encoded png
#' 
#' @param width Width of the plot in pixels
#' @param height Height of the plot in pixels
#' @param plotFn Function to call to perform the plot
encodePlot <- function(width, height, plotFn) {
    tempFilename <- tempfile(pattern='plot', fileext='png')
    device(file=tempFilename, width=width, height=height)
    plotFn()
    dev.off()
    fileSize <- file.size(tempFilename)
    raw <- readBin(tempFilename, what="raw", n=fileSize)
    paste0("data:", mimeType, ";base64,", jsonlite::base64_enc(raw))
}
