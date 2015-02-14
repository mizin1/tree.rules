\name{tr.predict}
\alias{tr.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Predykcja na podstawie regu³ decyzyjnych
}
\description{
	Zwraca wektor klas otrzymany na podstawie podanego zbioru regu³ decyzyjnych
}
\usage{
predict(model, frm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	model w postaci zbioru regu³ decyzyjnych
}
  \item{frm}{
	Data frame zawieraj±cy dane do predykcji
}
}
\examples{
	model.rpart <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
	model.tr <- tr.rules(model.rpart)
	kyp <- predict(model.tr, kyphosis)
} 

