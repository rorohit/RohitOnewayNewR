#' Oneway: An Alternative for Anova.
#'  @param z a list of named components with factors
#'  @param z A list of factors
#'  @param y A list of values
#'  @return object an Object of class \code{oneway}, basically a list of all elements:
#'  \item{x}{ A list of values }
#'
#'  \item{y}{ A list of Factors }
#'
#'  \item{SSB}{ Sum of Squares Between }
#'
#'  \item{SSW}{ Sum of Squares Within }
#'
#'  \item{Degree_of_freedomB}{ Degrees of Freedom Between }
#'
#'  \item{Degree_of_freedomW}{ Degrees of Freedom Within }
#'
#'  \item{Total_mean}{ Total Mean of all elements }
#'
#'  \item{Group_mean_vector}{ A list of mean of each group }
#'
#'  \item{Group_length_vector}{ A list of length of each group }
#'
#'  \item{Group_var_vector}{ A list of Variance of each Group }
#'
#'  \item{names_groups}{ Names of all groups }
#'
#'  \item{F_value}{ F value }
#'    library(faraway)
#' data(coagulation)
#' coagulation[1:4,]
#' Defaultdata <- oneway.default(unstack(coagulation))
#' print.oneway(Defaultdata)
# factors <- coagulation$diet
#' Values1 <- coagulation$coag
#' FactorData <- oneway.factor(factors, Values1)
#' print.oneway(FactorData)
#' FormulaData <- oneway.formula(as.formula("coag ~ diet"),data = as.list(coagulation))
#' print.oneway(FormulaData)
#' Defaultdatasummary <- summary.oneway(Defaultdata)

#' print.summary.oneway(Defaultdatasummary)

#' lsmeansdata <- lsmeans.oneway(Defaultdata)

#' plot.oneway(Defaultdata)
#' Default anova.
#'
#' @param z a named list for \code{"anova"}
#' @param \dots not used.

#' @export
  oneway <- function(z, ...) UseMethod("oneway")

#' @describeIn oneway oneway.default(z, ...)
#' @param z a named list for \code{"anova"}
#' @examples
#' {
#' RohitOnewayNewR:::oneway.default(c(A=c(30,31,32,31,30),B=c(34,33,31,32),C=c(32,33,34,35,31,30)))
#' }
#' @export
    oneway.default <- function(z, ...) {
    ## Your code here
    if(length(z) != length(unique(stack(z)$ind)))
    {
      for(i in 1:length(z))
      {
        names(z)[i] <- i
      }
    }
    names_groups <- unique(stack(z)$ind)
    groups <- length(z)
    number_of_observations <- length(stack(z)$ind)
    Total_mean <- mean(stack(z)$values)
    Group_mean_vector <- as.vector(sapply(z, mean))
    Group_var_vector <- as.vector(sapply(z, var))
    Group_length_vector <- as.vector(sapply(z,length))



    SSB <- sum(Group_length_vector * (Group_mean_vector - Total_mean)^2)
    SSW <- sum((Group_length_vector-1) * Group_var_vector)


    FinalList <- structure(list(x = stack(z)$values, y = as.factor(stack(z)$ind), SSB = SSB, SSW = SSW, Degree_of_freedomB = groups - 1, Degree_of_freedomW = number_of_observations - groups, Total_mean = Total_mean, Group_mean_vector = Group_mean_vector, Group_length_vector = Group_length_vector, Group_var_vector = Group_var_vector, names_groups = names_groups, F_value = NA, MSB = NA, MSW = NA, p_value = NA), class = "oneway")
    #class(FinalList) <- c("Oneway")

  }

    #### 2. This method uses the more standard input of a factor representing groups (or samples) and a numeric response.
  #' @describeIn oneway oneway.factor(z,y, ...)
  #' @examples
  #' RohitOnewayNewR:::oneway.factor(as.factor(c("A","A","A","B","B","C","C","C","C")), c(31,32,31,30,34,32,34,35,30))
  #' @export
      oneway.factor <- function(z, y, ...) {
    ## Your code here
    newList1 <- cbind(y,z)
    newList1 <- as.data.frame(newList1)
    names(newList1) <- c("List1", "List2")
    DFF <- unstack(newList1)
    Result <- oneway.default(DFF)
    return(Result)

  }

  #### 3. The model formula is the standard for R models, but do not use **model.matrix** to implement **oneway**.

      #' @describeIn oneway oneway.formula(formula, data = list(), ...)
      #' @examples
      #' RohitOnewayNewR:::oneway.formula(as.formula(coag ~ diet),coagulation)
      #' @export

    oneway.formula <- function(formula, data=list(), ...) {
    ## Your code here
    newList <- model.frame(formula, data = data)

    response1 <- newList[,1]
    factor1 <- newList[,2]

    newList1 <- oneway.factor(factor1, response1)
    return(newList1)

  }

    #### 4. The default **print** method should be short and provide essential information.

  #' Default print for anova.
  #'
  #' @param x an object of class \code{"anova"}, i.e., anova table.
  #' @param \dots not used.
  #' @export

  print.oneway <- function(x, ...) {

    Table_aov <- with(x, rbind(SS = c(SSB, SSW),
                               DF = c(Degree_of_freedomB , Degree_of_freedomW)))
    rownames(Table_aov) <- c("Sum of Squares", "Deg. of Freedom")
    colnames(Table_aov) <- c("diet", "Residuals")

    print(Table_aov)
    ## Your code here
  }

  #### 5. The summary method should create a summary object---not print directly.
  #' @describeIn oneway summary.oneway(object, ...)
  #' @export
    summary.oneway <- function(object, ...) {
    ## Your code here
    x <- object
    MSB <- x$SSB / x$Degree_of_freedomB
    MSW <- x$SSW / x$Degree_of_freedomW

    F_value <- MSB / MSW

    p_value <- pf(F_value, x$Degree_of_freedomB, x$Degree_of_freedomW, lower.tail = FALSE)

    x <- structure(c(list(F_value = F_value, MSB = MSB, MSW = MSW, p_value = p_value),x), class = c("oneway","summary"))
    #class(x) <- c("onewaySummary")
  }

  #### 6. The print method for the summary object should provide more detailed information about the summary object.

  #' Print method for the summary method.
  #'
  #' @param x an object of class \code{"summary.anova"}, i.e., a fitted model.
  #' @param \dots not used.
  #' @export
  print.summary.oneway <- function(x, ...) {
    ## Your code here

    Summary_table <- with(x, cbind(DF = c(Degree_of_freedomB , Degree_of_freedomW),
                                   SS = c(SSB, SSW),
                                   MS = c(MSB, MSW),
                                   F = c(F_value, NA),
                                   "Pr(>F" = c(p_value, NA)
    ))

    Least_sq_mean <- t(as.data.frame(with(x,Group_mean_vector)))
    colnames(Least_sq_mean) <- x$names_groups
    rownames(Least_sq_mean) <- c("[1]")
    tvalue <- qt(0.975,df=unlist(x$Degree_of_freedomW))

    colnames(Summary_table) <- c("Df", "Sum Sq", "Mean Sq","F Value", "Pr(>F)")
    rownames(Summary_table) <- c("ind", "Residuals")
    cat("\nAnalysis of Variance Table:\n")

    printCoefmat(Summary_table, P.values = TRUE, has.Pvalue = TRUE, signif.stars = TRUE, na.print = " ")

    cat("\n\n\n\n Least Square Means\n")

    print(Least_sq_mean)

    cat("\n Least Square Mean Total-: ")

    print(mean(x$Group_mean_vector))

    cat("\n")


  }


  #### 7. Implement Fisher's LSD multiple comparison procedure for your oneway.

  #' Print method for the Fishers LSD.
  #'
  #' @param object an object of class \code{"anova"}, i.e., a fitted model.
  #' @param \dots not used.
  #' @export
  lsmeans.oneway <- function(object, ...) {
    ## Your code here

    x <- object
    dataframe <- NULL
    tvalue <- qt(0.975,df=as.numeric(object$Degree_of_freedomW))
    MSW <- x$SSW / x$Degree_of_freedomW
    for(i in 1:(length(x$Group_length_vector)-1))
    {
      xx <- i +1
      for(j in xx:length(x$Group_mean_vector))
      {
        significance <- as.character("")
        group <- as.character(paste("Group ", as.character(i) , " and Group ", as.character(j)))
        value123 <- tvalue * (sqrt(MSW*((1/x$Group_length_vector[i])+(1/x$Group_length_vector[i]))))

        meandiff <- abs(x$Group_mean_vector[i] - x$Group_mean_vector[j])
        if(meandiff < value123)
          significance <- as.character("No")
        else
          significance <- as.character("Yes")
        dataframe <- rbind(dataframe,c(Group = group, FishersLSD=value123, MeanDiff = meandiff, Significance = significance))

      }
    }

    print(dataframe)

  }

  #### 8. A plot generic function should be implemented for *oneway* objects.

  #' Plot method for the summary method.
  #'
  #' @param x an object of class \code{"summary.anova"}, i.e., a fitted model.
  #' @param \dots not used.
  #' @export

  plot.oneway <- function(x, ...) {
  ## Your code here
  FinalList <- as.data.frame(cbind(as.numeric(x$x),as.character(x$y)))

  colnames(FinalList) <- c("values","ind")
  FinalList <- model.frame(as.formula("values ~ ind"), data = FinalList)
  FinalList <- stack(unstack(FinalList))

  colnames(FinalList) <- c("values","ind")
  FinalList <- transform(FinalList, values = as.numeric(values))


  boxplot(as.formula("values ~ ind"),data = FinalList,xlab = "Factors", ylab = "Values", col = rainbow(length(unique(unlist(FinalList$values)))))
  title("Comparision of Distribution of Groups")
  }

  #### 9. Your S3 class implementation should be illustrated with the *coagulation* data set. The data consists of blood coagulation times for 24 animals randomly assigned to four different diets.
  library(faraway)
  data(coagulation)
  coagulation[1:4,]
  ## Your implementation code here
  Defaultdata <- oneway.default(unstack(coagulation))

  print.oneway(Defaultdata)
  factors <- coagulation$diet
  Values1 <- coagulation$coag
  FactorData <- oneway.factor(factors, Values1)
  print.oneway(FactorData)
  #print.oneway(FactorData)
  FormulaData <- oneway.formula(as.formula("coag ~ diet"),data = as.list(coagulation))
  print.oneway(FormulaData)
  Defaultdatasummary <- summary.oneway(Defaultdata)

  #summaryObj <- print.oneway(unstack(coagulation))

  print.summary.oneway(Defaultdatasummary)

  lsmeansdata <- lsmeans.oneway(Defaultdata)

  plot.oneway(Defaultdata)


