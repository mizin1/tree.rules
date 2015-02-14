tr.rule.part <- setClass("tr.rule.part", slots = c(var.name="character", var.value="character", operand="character"))

tr.rule <- setClass("tr.rule", slots = c(rule.parts="list", target.value="character") )
tr.rule.set <- setClass("tr.rule.set", slots = c(rules="list", target.name="character", target.default.value="character"))
