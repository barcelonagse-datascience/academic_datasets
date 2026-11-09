checkSubmission <- function(file = file.choose()){
  # Runs your script in a separate environment, checks that all the required
  # variables exist, and looks for common problems in Part 1 that can affect
  # the questions that come after them.
  sol.env <- new.env()
  capture.output( run <- suppressWarnings(tryCatch({
                   source(file, local = sol.env)
                   TRUE },
                   error = function(cond) {
                     message(sprintf('Your code does not seem to run. I got the following message from R: \n %s',
                                     conditionMessage(cond) ))
                     FALSE
                   })) )
  envVars <- ls(envir = sol.env)
  solutionVars <- c(
    'nRows', 'nCols',                                        # Q1
    'year', 'month', 'data',                                 # Q2
    'naCols', 'nComplete',                                   # Q3
    'N',                                                     # Q4
    'naRowCount', 'lastRow', 'lastYear', 'lastMonth',        # Q5
    'monthMeans',                                            # Q7
    'highMonth', 'lowMonth',                                 # Q8
    'y', 'X', 'y.year', 'y.month',                           # Q9
    'train', 'nTrain', 'nTest', 'X.train', 'X.test', 'y.train', 'y.test', # Q10
    'meanPred', 'arMdl', 'arPred',                           # Q11
    'eaMdl', 'eaPred',                                       # Q12
    'fullMdl', 'fmPred',                                     # Q13
    'X.train.mat', 'X.test.mat', 'lasso', 'lassoPred', 'lassoVars', 'nLasso', # Q14
    'elnet', 'elnetPred',                                    # Q15
    'ridge', 'ridgePred',                                    # Q16
    'pred', 'mse', 'relMse', 'bestModel',                    # Q17
    'nLambdaRidge', 'posMin',                                # Q18
    'ridge2', 'posMin2', 'ridge2Pred', 'mseRidge2',          # Q19
    'monthMeanForecast', 'monthPred', 'mseMonth')            # Q20

  ##### 1. Required variables #####
  whichMiss <- solutionVars[ !(solutionVars %in% envVars) ]
  if (length(whichMiss) == 0) {
    cat('All required variables are defined.\n')
  } else {
    cat('I cant seem to find the following variables: \n')
    cat(whichMiss)
    cat('\nYou can submit the code as is, but you will not get any points for the questions that request these variables.\n')
  }

  ##### 2. Part 1 checks (problems that can affect later questions) #####
  # These checks only compare your variables with each other. Passing them does
  # NOT mean that your answers are correct.
  e <- sol.env
  has <- function(...) all(c(...) %in% envVars)
  problems <- c()
  add <- function(msg) problems <<- c(problems, msg)
  chk <- function(expr) tryCatch(expr, error = function(cond) NULL)

  chk(if (has('data') && any(c('year', 'month') %in% colnames(e$data)))
    add('Q2: data still contains the columns year and/or month. They should be removed from data.'))

  chk(if (has('year', 'month') && (!is.numeric(e$year) || !is.numeric(e$month)))
    add('Q2: year and month should be numeric vectors.'))

  chk(if (has('data', 'nCols') && ncol(e$data) >= e$nCols - 2)
    add('Q4: data has as many columns as the original dataset (minus year and month). It looks like the columns with more than 5% of missing values were not removed.'))

  chk(if (has('data', 'N') && e$N != ncol(e$data))
    add('Q4: N is not equal to the number of columns of data.'))

  chk(if (has('data', 'lastRow') && nrow(e$data) != e$lastRow)
    add('Q5: the number of rows of data is not equal to lastRow. It looks like the rows after lastRow were not dropped from data.'))

  chk(if (has('data', 'year') && length(e$year) != nrow(e$data))
    add('Q5: year does not have as many elements as data has rows. Remember to drop the same rows from data, year and month.'))

  chk(if (has('data', 'month') && length(e$month) != nrow(e$data))
    add('Q5: month does not have as many elements as data has rows. Remember to drop the same rows from data, year and month.'))

  chk(if (has('year', 'month', 'lastYear', 'lastMonth') &&
          (tail(e$year, 1) != e$lastYear || tail(e$month, 1) != e$lastMonth))
    add('Q5: the last elements of year and month are not equal to lastYear and lastMonth.'))

  chk(if (has('data') && anyNA(e$data))
    add('Q6: data still contains missing values. Check that you completed the loop in Q6.'))

  chk(if (has('monthMeans') && (length(e$monthMeans) != 12 || anyNA(e$monthMeans) || !is.numeric(e$monthMeans)))
    add('Q7: monthMeans should be a numeric vector with 12 elements and no missing values.'))

  chk(if (has('highMonth', 'lowMonth') &&
          (length(e$highMonth) != 1 || length(e$lowMonth) != 1 ||
           !(e$highMonth %in% 1:12) || !(e$lowMonth %in% 1:12)))
    add('Q8: highMonth and lowMonth should each be a single number between 1 and 12.'))

  chk(if (has('y', 'data') && (length(e$y) != nrow(e$data) - 1 ||
                               !isTRUE(all.equal(as.numeric(e$y), as.numeric(e$data$TOTALES[-1])))))
    add('Q9: y is not equal to TOTALES from the second row until the last row of data.'))

  chk(if (has('X', 'data') && (nrow(e$X) != nrow(e$data) - 1 ||
                               !isTRUE(all.equal(as.numeric(e$X$TOTALES), as.numeric(e$data$TOTALES[-nrow(e$data)])))))
    add('Q9: X is not equal to the rows of data from the first until the second to last.'))

  chk(if (has('y', 'y.year', 'y.month') && (length(e$y.year) != length(e$y) || length(e$y.month) != length(e$y)))
    add('Q9: y.year and y.month should have as many elements as y.'))

  chk(if (has('y.year', 'y.month', 'year', 'month') &&
          (!isTRUE(all.equal(as.numeric(e$y.year), as.numeric(e$year[-1]))) ||
           !isTRUE(all.equal(as.numeric(e$y.month), as.numeric(e$month[-1])))))
    add('Q9: y.year and y.month do not match the year and month of each element of y.'))

  chk(if (has('train', 'y') && (!is.logical(e$train) || length(e$train) != length(e$y)))
    add('Q10: train should be a logical vector with as many elements as y.'))

  chk(if (has('train', 'y.year') && (any(e$y.year[e$train] > 2014) || any(e$y.year[!e$train] <= 2014)))
    add('Q10: train is not TRUE exactly when y.year is smaller than or equal to 2014.'))

  chk(if (has('nTrain', 'nTest', 'y') && e$nTrain + e$nTest != length(e$y))
    add('Q10: nTrain plus nTest is not equal to the number of elements of y.'))

  chk(if (has('X.train', 'y.train', 'nTrain') && (nrow(e$X.train) != e$nTrain || length(e$y.train) != e$nTrain))
    add('Q10: X.train and y.train should have nTrain rows (elements).'))

  chk(if (has('X.test', 'y.test', 'nTest') && (nrow(e$X.test) != e$nTest || length(e$y.test) != e$nTest))
    add('Q10: X.test and y.test should have nTest rows (elements).'))

  if (length(problems) > 0) {
    cat('\nPart 1 checks: I found possible problems that can also affect later questions:\n')
    cat(paste0(' - ', problems, collapse = '\n'), '\n')
  }

  if (isTRUE(run) && length(whichMiss) == 0 && length(problems) == 0) {
    cat('\nYour submission runs and contains all required variables. Thanks and have a great day!\n')
  }
  invisible(NULL)
}
