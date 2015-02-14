tr.predict.all <- function(model, frm)
{
	ret = c()
	for (i in 1:nrow(frm))	 {
		ret = c (ret, tr.predict(model, frm[i,]))		
	}
	ret
}

tr.predict <- function(model, ff)
{
	ret <- c()
	for(rule in model@rules) 
	{
		if (tr.apply.rule(rule, ff))
		{
			ret = c(ret, rule@target.value)
		}
	}
	if (length(ret) == 0) {
		return (model@target.default.value)
	}
	ret <- sort(ret)
	rl = rle(ret)
	ret = rl$values[rl$lengths == max(rl$lengths)]
	if (length(ret) == 1)
	{
		return(ret)	
	}
	if (any(ret == model@target.default.value))
	{
		return(model@target.default.value)
	}
	return(ret[1])
}

tr.apply.rule <- function(rule, ff)
{
	for (part in rule@rule.parts)
	{
		if (!tr.apply.rule.part(part, ff))
		{
			return(FALSE)
		}
	}
	return(TRUE)
}

tr.apply.rule.part <- function(part, ff) 
{	
	var = ff[,part@var.name]
	if (is.na(var))
	{
		return(FALSE)
	}
	if (part@operand == '=')
	{
		return(any(part@var.value == var))
	}
	if (part@operand == '>')
	{
		return(var > as.numeric(part@var.value))	
	}
	if (part@operand == '<')
	{
		return(var < as.numeric(part@var.value))	
	}
	if (part@operand == '>=')
	{
		return(var >= as.numeric(part@var.value))	
	}
	if (part@operand == '<=')
	{
		return(var <= as.numeric(part@var.value))	
	}
	warning('Unknown operand!')
	return(FALSE)
}
