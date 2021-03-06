# RRPC

RRPC is an R package that allows other languages to call your R functions.

It does this by setting up a WebSocket server. Optionally this server can
also serve static files, providing a lightweight alternative to Shiny. This
is particularly useful if you want to build your user interface in HTML and
JavaScript but use R for the calculation engine.

To provide the R functions to be called, write R code like the following:

```R
rrpcServer(host=host, port=port, interface=list(
  run=function(data) {
      run(data)
  },
  plot=function(data, width, height) {
    obj <- list()
    obj$src <- encodePlotAsPng(width, height, function() {
      results <- run(data)
      plot(x=results$x, y=results$y)
    })
    obj
  }
))
cat(sprintf("Listening on %s:%d\n", host, port))
while (TRUE) {
  later::run_now(9999)
}
```

To call your functions from a browser, get hold of a recent version of
`rrpc.min.js` from [the releases page on GitHub](https://github.com/tim-band/rrpc-js/releases/)
and write JavaScript a little like this:

```html
<script src='rrpc.min.js'>
<script>
rrpc.initialize(openCallback=function() {
  console.log('RRPC initialized!');
},
errorCallback=function() {
  console.error('RRPC failed to initialize!');
});
function getPlot() {
  var myplot = document.getElementById("myplot");
  var w = myplot.getAttribute("width");
  var h = myplot.getAttribute("height");
  // Provide the function name as a string and the parameters as an object
  rrpc.call("plot", {width: w, height: h, data: [1,3,3,4]}, function(result) {
    // result.src[0] is what the R code set as obj$src above.
    // The [0] is necessary because converting from R to JSON data
    // structures always results in arrays (or objects), never scalars.
    myplot.setAttribute("src", result.src[0]);
  });
}
</script>
<img id="myplot" onclick="getPlot()">
```
