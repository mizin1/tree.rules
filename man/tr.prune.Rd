\name{tr.prune}
\alias{tr.prune}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Przycinanie zbioru regu�
}
\description{
	Przycina podany zbi�r regu� decyzyjnych na podstawie podanego zbioru do przycinania oraz parametru z�o�ono�ci
}
\usage{
prune(model, frm, cp = 0, prune.all.rules = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	model w postaci zbioru regu� decyzyjnych
}
  \item{frm}{
	Data frame zawierajacy dane, na podstawie kt�rych wykonane zostanie przycinanie
}
  \item{cp}{
	parametr z�o�ono�ci z przedzia�u [0:1], im wi�kszy tym bardziej zbi�r regu� zostanie przyci�ty
}
  \item{prune.all.rules}{
	zezwolenie na usuwanie ca�ych regu� ze zbioru
}
}
\value{
	przyci�ty zbi�r regu�
}
\examples{
	model.rpart <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
	model.tr <- tr.rules(model.rpart)
	model.tr.pruned <- prune(model.tr, kyphosis, 0.01)
}
