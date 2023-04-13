\name{abmld2}
\docType{data}
\alias{abmld2}
\alias{abml}
\title{
Gross Value Added (GVA, Average) at basis prices: CP SA time series / second differenced series
}
\description{
 Essentially GVA is a component in the estimator for UK Gross Domestic Product (GDP) an important economic time series. The series can be downloaded from the UK Office of National Statistics website, see below for references.
}
\usage{
data("abml")
data("abmld2")
}
\format{
	The GVA series that we obtain are the quarterly reports from Q1 1955 until Q4 2020. This is a series of 264 observations. The series has a strong mean trend which we have removed using twice differencing (diff(abml, diff=2)) to obtain the series abmld2. This vector is of length 262.
}
\source{www.statistics.gov.uk/statbase/TSDtables1.asp and www.statistics.gov.uk/cci/nugget.asp?id=254}

\keyword{datasets}
