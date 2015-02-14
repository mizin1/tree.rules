tr.rule.parts <- function(object)
{
	ff <- object$frame
	n <- nrow(ff)
	if (n == 1L) return(tr.rule.part())			# special case of no splits

	is.leaf <- (ff$var == "<leaf>")
	whichrow <- !is.leaf
	vnames <- ff$var[whichrow] # the variable names for the primary splits

	index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + !is.leaf))
	irow <- index[c(whichrow, FALSE)] # we only care about the primary split
	ncat <- object$splits[irow, 2L]

	## Now to work: first create labels for the left and right splits,
	##  but not for leaves of course
	##
	lsplit_values <- rep(list(labels=character(0)),length(irow))
	lsplit_operand <- character(length(irow))
	rsplit_values <- rep(list(labels=character(0)),length(irow))
	rsplit_operand <- character(length(irow))


	if (any(ncat < 2L)) {			   # any continuous vars ?
		jrow <- irow[ncat < 2L]
		cutpoint <- object$splits[jrow, 4L]
		temp1 <- (ifelse(ncat < 0, ">=", "<"))[ncat < 2L]
		temp2 <- (ifelse(ncat < 0, "<", ">="))[ncat < 2L]
		lsplit_values[ncat<2L] <- as.character(cutpoint)
		lsplit_operand[ncat<2L] <- temp1
		rsplit_values[ncat<2L] <- as.character(cutpoint)
		rsplit_operand[ncat<2L] <- temp2
	}

	if (any(ncat > 1L)) {			   # any categorical variables ?
		xlevels <- attr(object, "xlevels")
		##
		## jrow will be the row numbers of factors within lsplit and rsplit
		## crow the row number in "csplit"
		## and cindex the index on the "xlevels" list
		##
		jrow <- seq_along(ncat)[ncat > 1L]
		crow <- object$splits[irow[ncat > 1L], 4L] #row number in csplit
		cindex <- (match(vnames, names(xlevels)))[ncat > 1L]

		## Now tuck in the labels
		## I'll let some other clever person vectorize this
		for (i in seq_along(jrow)) {
			j <- jrow[i]
			splits <- object$csplit[crow[i], ]
			lsplit_values[j] <-
				list(labels=(xlevels[[cindex[i]]])[splits == 3L])
			rsplit_values[j] <-
				list(labels=(xlevels[[cindex[i]]])[splits == 1L])
		}
	}

	lsplit_operand[ncat >= 2L] <- "="
	rsplit_operand[ncat >= 2L] <- "="

	## Now match them up to node numbers
	##   The output will have one label per row of object$frame, each
	##   corresponding the the line segement joining this node to its parent
	varname <- (as.character(vnames))
	node <- as.numeric(row.names(ff))
	parent <- match(node %/% 2L, node[whichrow])
	odd <- (as.logical(node %% 2L))

	ret <- lapply( rep("tr.rule.part", n), new )
	for (i in 2:n) {
		if(odd[i]) {
			ret[i] <- tr.rule.part(var.name=varname[parent[i]], var.value=(lsplit_values[parent[i]])$labels, operand=lsplit_operand[parent[i]])
		} else {

			ret[i] <- tr.rule.part(var.name=varname[parent[i]], var.value=(rsplit_values[parent[i]])$labels, operand=rsplit_operand[parent[i]])
		}
	}
ret
}
