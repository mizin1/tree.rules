\name{tr.prune}
\alias{tr.prune}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Przycinanie zbioru regu³
}
\description{
	Przycina podany zbiór regu³ decyzyjnych na podstawie podanego zbioru do przycinania oraz parametru z³o¿ono¶ci
}
\usage{
prune(model, frm, cp = 0, prune.all.rules = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	model w postaci zbioru regu³ decyzyjnych
}
  \item{frm}{
	Data frame zawierajacy dane, na podstawie których wykonane zostanie przycinanie
}
  \item{cp}{
	parametr z³o¿ono¶ci z przedzia³u [0:1], im wiêkszy tym bardziej zbiór regu³ zostanie przyciêty
}
  \item{prune.all.rules}{
	zezwolenie na usuwanie ca³ych regu³ ze zbioru
}
}
\value{
	przyciêty zbiór regu³
}
\examples{
	model.rpart <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
	model.tr <- tr.rules(model.rpart)
	model.tr.pruned <- prune(model.tr, kyphosis, 0.01)
}
