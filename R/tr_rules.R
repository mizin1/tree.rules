tr.rules <- function(model)
{	
	#atrybut docelowy
	target <- as.character(attr(model$terms, "variables")[2])
	#dziedzina atrybutu docelowego
	ylevels = attr(model, "ylevels")
	#ramka obiektu
	model.frame <- model$frame

	if (nrow(model.frame) == 1)	#tylko korzeń
	{
		return(tr.rule.set(target.name = target, target.default.value= ylevels[model.frame[1,]$yval]))
	}
	
	#wszystkie reguły z drzewa
	rule.part.list.global <- tr.rule.parts(model)
	#pozostawiamy tylko liscie
	leaves <- model.frame[model.frame$var == '<leaf>',]
	
	rules = c()
	#petla po lisciach
	for (i in 1:nrow(leaves))
	{
		#kolejny lisc w drzewie
		leaf <- leaves[i,]
	
		#kolejne czesci reguly:
		rule.part.list <- c()

		#nazwa (numer) liscia
		row.name <- row.names(leaf)
		row.name <- as.numeric(row.name)
		
		#petla do korzenia drzewa
		while (row.name > 1)
		{
			cur.rule.part <- rule.part.list.global[row.names(model.frame) == row.name]
			rule.part.list <- c(rule.part.list, cur.rule.part)
			row.name <- floor(row.name/2)
		}

		rule1 <- tr.rule(rule.parts=rule.part.list, target.value = ylevels[leaf$yval])
		rules = c (rules, rule1)
	
	}
	tr.rule.set(rules = rules,  target.name = target, target.default.value= ylevels[model.frame[1,]$yval])
}
