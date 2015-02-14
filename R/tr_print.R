tr.print <- function(object, ...)
{
	for(rule in object@rules)
	{
		tr.print.rule(object, rule)
	}
	print(paste("default: ",  object@target.name, "=", object@target.default.value, sep=""))
}

tr.print.rule <- function(object, rule)
{
	rule.string = ""
	for (part in rule@rule.parts)
	{
		rule.string.part = paste0(part@var.name, part@operand)
		rule.string.part = paste0(rule.string.part, paste(part@var.value, collapse=","))
		rule.string = ifelse(rule.string == "", rule.string.part, paste(rule.string, rule.string.part, sep =" & "))
	}
	rule.string = paste(rule.string, " => ", object@target.name, "=", rule@target.value, sep="")
	print(rule.string)
}
