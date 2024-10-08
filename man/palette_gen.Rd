% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{palette_gen}
\alias{palette_gen}
\title{A function to generate a colour palette of n_pal hexcodes based upon an input
colour (or colours) \code{x}}
\usage{
palette_gen(x, n_pal)
}
\arguments{
\item{x}{Either the character name of an RColorBrewer palette (e.g.
\code{fill_palette = "YlOrRd"}), a character containing a single rgb
colour code, hexcode, or colour name that will be used to generate
a colour range (e.g. \code{fill_palette = "#007C91"}), or a character
vector containing multiple rgb codes, hexcodes, or colour names that
will be used to generate a colour range (e.g. \code{c("#007C91","purple",
"red")}). Defaults to the RColorBrewer "Blues" palette.}

\item{n_pal}{numeric The number of desired colours in the returned palette.}
}
\value{
A character vector of hex codes for use in a colour palette.
}
\description{
A function to generate a colour palette of n_pal hexcodes based upon an input
colour (or colours) \code{x}
}
\examples{
\dontrun{
palette_gen(c("#007C91","purple"),7)

palette_gen("#007C91",5)

palette_gen("YlOrRd",5)
}
}
