% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{load_data}
\alias{load_data}
\title{Load the data found in data/.}
\usage{
load_data(update = FALSE, project.name = "urinaryDBP")
}
\arguments{
\item{update}{Whether you want to update the dataset using the fetch_data
function.}

\item{project.name}{Name of your project. Set the default to keep it always
working.}
}
\value{
Outputs the datasets in the data/ folder.
}
\description{
Load the data found in data/.
}
\examples{
load_data()
load_data(update = TRUE)

}
