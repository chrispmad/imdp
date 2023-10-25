#' Title Check user input to make sure it is one of a vector of acceptable answers, or 'Q' to quit.
#'
#' @param acceptable_answers vector of acceptable answers, e.g. c("Y","N")
#' @param user_input Don't add anything to this, for internal function use.
#'
#' @return 'Y', 'N' or break from function.
#'
#' @examples \dontrun
check_users_input = function(acceptable_answers,user_input = NULL){
  
  if(is.null(user_input)){
    user_input = readline()
  }
  
  if(!user_input %in% c(acceptable_answers,'Q')){
    cat(paste0("\nNote: you entered '",user_input,"', which is not one of the acceptable answers. They are: "))
    cat(paste0(acceptable_answers, collapse = ', '))
    cat("\nPlease enter one of the acceptable answers, or Q to quit.")
    
    user_input = readline()
    print('new input: ')
    cat(user_input)
    
    check_users_input(acceptable_answers,user_input)
  } else {
    if(user_input == 'Q'){
      stop('Summarising IMDP data to waterbody aborted.')
    } else {
      return(user_input)
    }
  }
}