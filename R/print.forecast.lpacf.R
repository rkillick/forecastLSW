print.forecast.lpacf <-
function (x, ...) 
{
    cat("Class 'forecast.lpacf' : Forecast from Locally Stationary Time Series:\n")
    cat("       ~~~~  : List with", length(x), "components with names\n")
    cat("             ", names(x), "\n\n")
    cat("\nsummary(.):\n----------\n")
    forecastLSW::summary.forecast.lpacf(x)
}
