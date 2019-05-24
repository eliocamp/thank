#' Send an email
#'
#' Opens a prepopulated email for you to send to the maintainer of a package.
#'
#' @param package, name of the package you want to say thanks to.
#'
#' @export
to <- function(package) {
  pkg_author <- utils::packageDescription(package, field = "Maintainer")

  pkg_mail <- stringi::stri_match_all(pkg_author, regex = "<(.*?)>")[[1]][2]
  pkg_author <- stringi::stri_replace_all(pkg_author, replacement = "", regex = "<(.*?)>")
  pkg_author <- stringi::stri_trim_both(pkg_author)

  subject <- paste0("Thanks for the ", package, " package!")

  body <- paste0("Hi, ", pkg_author, ",\n",
                 "I'm an R user and just wanted to tell you that I love your ", package, " R package.\n",
                 "That's all. Have a nice day!")

  mail <- callr::r_vanilla(function(pkg_mail, subject, body) {

    mailto <- paste0("mailto:", pkg_mail,
                     "?subject=", subject,
                     "?body=", body)
    utils::browseURL(mailto)
  }, args = list(pkg_mail = pkg_mail,
                 subject = subject,
                 body = body))


}

