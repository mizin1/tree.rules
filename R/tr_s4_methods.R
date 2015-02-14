setMethod('show', "tr.rule.set", function(object) {tr.print(object)} )

setMethod('predict', "tr.rule.set", function(object, frm, ...) {tr.predict.all(object, frm, ...)} )

setMethod('prune', "tr.rule.set", function(tree, frm, ...) {tr.prune(tree, frm, ...)} )

