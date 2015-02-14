tr.err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

tr.rate <- function(model, frm)
{
	tr.err(frm[,model@target.name], tr.predict.all(model, frm))
}

tr.length <- function(model) {
	ret = 0	
	for(rule in model@rules)
		ret <- ret + length(rule@rule.parts)
	ret
}
