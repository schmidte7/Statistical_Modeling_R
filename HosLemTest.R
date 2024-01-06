#######################################################################
#
# Hosmer-Lemeshow Goodness of Fit (GOF) Test for binary logistic regression models
#
# (Source: hoslem.test:::ResourceSelection > http://www.inside-r.org/packages/cran/ResourceSelection/docs/hoslem.test)
# 
#######################################################################
# Last modified: 10.09.2015.
#######################################################################
# Warranty: none.
#######################################################################

HosLem.test = function (x,       # a numeric vector of observations, binary (0/1)
                        y,       # expected values
						g = 10)  # number of bins to use to calculate quantiles.
{
    DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), sep = ", ")
    # METHOD <- "Hosmer and Lemeshow goodness of fit (GOF) test"
    METHOD <- "Hosmer-Lemeshow goodness of fit test"
    yhat <- y
    y <- x
    qq <- unique(quantile(yhat, probs = seq(0, 1, 1/g)))
    cutyhat <- cut(yhat, breaks = qq, include.lowest = TRUE)
    observed <- xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
    expected <- xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ cutyhat)
    chisq <- sum((observed - expected)^2/expected)
    PVAL = 1 - pchisq(chisq, g - 2)
    PARAMETER <- g - 2
    names(chisq) <- "X-squared"
    names(PARAMETER) <- "df"
    structure(list(statistic = chisq,      # chi-squared test statistic, (sum((observed - expected)^2 / expected))
	     parameter = PARAMETER,  # df of approximate chi-squared distribution of the test statistic (g - 2)
         p.value = PVAL,         # p-value for the test
		 method = METHOD,        # type of test
		 data.name = DNAME,      # name(s) of the data
		 observed = observed,    # observed frequencies in a g-by-2 contingency table
         expected = expected),   # expected frequencies in a g-by-2 contingency table
		 class = "htest")
}

#------------------------------

print.htest = function (x, digits = getOption("digits"), prefix = "\t", ...) 
{
    cat("\n")
    cat(strwrap(x$method, prefix = prefix), sep = "\n")
    cat("\n")
    cat("data:  ", x$data.name, "\n", sep = "")
    out <- character()
    if (!is.null(x$statistic)) 
        out <- c(out, paste(names(x$statistic), "=", format(signif(x$statistic, 
            max(1L, digits - 2L)))))
    if (!is.null(x$parameter)) 
        out <- c(out, paste(names(x$parameter), "=", format(signif(x$parameter, 
            max(1L, digits - 2L)))))
    if (!is.null(x$p.value)) {
        fp <- format.pval(x$p.value, digits = max(1L, digits - 
            3L))
        out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == 
            "<") fp else paste("=", fp)))
    }
    cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
    if (!is.null(x$alternative)) {
        cat("alternative hypothesis: ")
        if (!is.null(x$null.value)) {
            if (length(x$null.value) == 1L) {
                alt.char <- switch(x$alternative, two.sided = "not equal to", 
                  less = "less than", greater = "greater than")
                cat("true ", names(x$null.value), " is ", alt.char, 
                  " ", x$null.value, "\n", sep = "")
            }
            else {
                cat(x$alternative, "\nnull values:\n", sep = "")
                print(x$null.value, digits = digits, ...)
            }
        }
        else cat(x$alternative, "\n", sep = "")
    }
    if (!is.null(x$conf.int)) {
        cat(format(100 * attr(x$conf.int, "conf.level")), " percent confidence interval:\n", 
            " ", paste(format(c(x$conf.int[1L], x$conf.int[2L])), 
                collapse = " "), "\n", sep = "")
    }
    if (!is.null(x$estimate)) {
        cat("sample estimates:\n")
        print(x$estimate, digits = digits, ...)
    }
    cat("\n")
    invisible(x)
}

#------------------------------

# set.seed(123)
# n <- 500
# x <- rnorm(n)
# y <- rbinom(n, 1, plogis(0.1 + 0.5*x))
# m <- glm(y ~ x, family=binomial)
# HosLem.test(m$y, fitted(m))

#######################################################################
