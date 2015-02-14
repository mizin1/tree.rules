\name{tr.rate}
\alias{tr.rate}
\title{
	Ocena jakości zbioru reguł decyzyjnych
}
\description{
	Zwraca odsetek przykładów w zadanym zbiorze błędnie zaklasyfikowanych przez podany model
}
\usage{
tr.rate(model, frm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	model w postaci zbioru reguł decyzyjnych
}
 \item{frm}{
	Data frame zawierajacy dane, na podstawie których wykonane zostanie ocena jakowści
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Odsetek błędnie zaklasyfikowanych przykładów
}

\examples{
	model.rpart <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
	model.tr <- tr.rules(model.rpart)
	model.tr.pruned <- prune(model.tr, kyphosis, 0.01)
	tr.rate(model.tr, kyphosis)
	tr.rate(model.tr.pruned, kyphosis)
}

