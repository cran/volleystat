% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sets}
\alias{sets}
\title{Sets data}
\format{Data frame with 12660 rows and 9 columns.
\describe{
  \item{league_gender}{Men or women's league.}
  \item{season_id}{ID for each season unique within league_gender. It consists of the last two digits of the season
  years, e.g., the season_id of season 2014/2015 equals 1415.}
  \item{match_id}{Official VBL match id. Unique within seasons.}
  \item{match}{Factor variable identifying whether observation is from the home team's perspective or
  from the away team's perspective.}
  \item{team_id}{Team identifier which is unique across all teams and seasons.}
  \item{team_name}{Official name of the team. Note that VCO Berlin has a team in men's and women's league.}
  \item{set}{Set identifier.}
  \item{set_duration}{Length of set in minutes.}
  \item{pt_set}{Points scored in set.}
}}
\source{
\url{https://www.volleyball-bundesliga.de/cms/home/1blf/1blf_archiv/1blf_tabellen_ergebnisse.xhtml}
        \url{https://www.volleyball-bundesliga.de/cms/home/1blm/1blm_archiv/1blm_tabellen_ergebnisse.xhtml}
}
\usage{
sets
}
\description{
A dataset containing all matches of the German first division volleyball league for each season on set level.
starting in 2013/2014 and ending in 2018/2019. Note that all sets are included twice in the
dataset, i.e., from the perspective of the home team and from the perspective of the away team.
}
\examples{
matchstats
}
\keyword{datasets}
