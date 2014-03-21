require(quantmod)
require(PerformanceAnalytics)
require(rCharts)

tckrs <- c("SPY","EEM","VEA")
getSymbols(tckrs)

annRet <- t(na.omit(data.frame(
  table.CalendarReturns(
    do.call(merge,
      lapply(
        tckrs,
        function(tckr){
          return(ROC(to.monthly(get(tckr))[,6],type="discrete",n=1))
        }
      )
    )
  )[,-(1:12)]
)))

rownames(annRet) <- tckrs
annRet <- cbind.data.frame(
  tckrs,
  annRet,
  stringsAsFactors = FALSE
)



brew <- RColorBrewer::brewer.pal(name="RdBu",n=5)
p1 <- rCharts$new()
p1$setLib(system.file('parcoords', package = 'rCharts'))
p1$templates$script = "./chart_more.html"
p1$set(
  padding = list(top = 24, left = 100, bottom = 12, right = 100),
  height = "400",
  width = "800",
  color = "#!d3.scale.category10()!#"
)

p1$set(
  data = toJSONArray(annRet, json = F),
  colorby = 'tckrs'
)
p1$setTemplate(
  afterScript = '
 <script>
 d3.selectAll("svg").selectAll("text")
 .style("font-size","10px")
 </script>
 '
)
p1



#now let's add some 3y, 5y numbers

moreRet <- data.frame(t(
  table.AnnualizedReturns(
    do.call(
      merge,
      lapply(
        tckrs,
        function(tckr){
          return(ROC(to.monthly(get(tckr))[,6],type="discrete",n=1))
        }
      )
    )
  )[1,] * 100
))
colnames(moreRet) <- paste0("since ",colnames(annRet)[2])

p1$params$data = toJSONArray(
  cbind.data.frame(
    moreRet,annRet)[order(moreRet[,1]),],
  json=F)
p1