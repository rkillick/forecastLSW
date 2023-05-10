print.forecastlpacf <-
function (x, ...) 
{
    cat("Class 'forecastlpacf' : Forecast from Locally Stationary Time Series:\n")
    cat("       ~~~~  : List with", length(x), "components with names\n")
    cat("             ", names(x), "\n\n")
    cat("\nsummary(.):\n----------\n")
    forecastLSW::summary.forecastlpacf(x)
}
