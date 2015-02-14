tr.prune <- function(model, frm, cp=0, prune.all.rules=TRUE)
{	
	last.length <- tr.length(model) + 1
	cur.length <- tr.length(model) 
	while(cur.length < last.length)
	{
		last.length <- cur.length
		model = tr.prune.rule.part(model, frm, cp, prune.all.rules)
		cur.length <- tr.length(model)
	}
	model
}

tr.prune.rule.part <- function(model, frm, cp=0, prune.all.rules=FALSE)
{
	if (tr.length(model) == 0)
	{
		return(model)
	}
	best.model <- model
	best.rate <- tr.rate(model, frm) + cp
	for(i in 1:length(model@rules))
	{
		n <- length(model@rules[[i]]@rule.parts)
		if (n ==  1 && prune.all.rules) {
			model2 <- model
			model2@rules <- model2@rules[-i]
			rate <- tr.rate(model2, frm)
			if (rate <= best.rate) 
			{
				best.model <- model2
				best.rate <- rate
			}
		} 
		if (n >  1)
		{
			for (j in 1:n)
			{
				model2 <- model
				model2@rules[[i]]@rule.parts = model2@rules[[i]]@rule.parts[-j]
				rate <- tr.rate(model2, frm)
				if (rate <= best.rate) 
				{
					best.model <- model2
					best.rate <- rate
				}
			}
		}
	}
	best.model
}
