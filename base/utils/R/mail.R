
#' Sends email. This assumes the program sendmail is installed.
#'
#' @param from the sender of the mail message
#' @param to the receipient of the mail message
#' @param subject the subject of the mail message
#' @param body the body of the mail message
#' @author Rob Kooper
#' @return nothing
#' @export
#' @examples
#' \dontrun{
#'   # Check if 'sendmail' is installed before running.
#'   # This prevents errors in environments like Docker where the utility is missing.
#'   if (nzchar(Sys.which("sendmail"))) {
#'     sendmail("bob@@example.com", "joe@@example.com", "Hi", "This is R.")
#'   } else {
#'     message("System 'sendmail' utility not found. Skipping example.")
#'   }
#' }
sendmail <- function(from, to, subject, body) {
  if (is.null(to)) {
    PEcAn.logger::logger.error("No receipient specified, mail is not send.")
  } else {
    if (is.null(from)) {
      from <- to
    }
    sendmail <- Sys.which("sendmail")
    mailfile <- tempfile("mail")
    cat(paste0("From: ", from, "\n", 
               "Subject: ", subject, "\n", 
               "To: ", to, "\n", "\n", 
               body, "\n"), file = mailfile)
    system2(sendmail, c("-f", paste0("\"", from, "\""), 
                        paste0("\"", to, "\""), "<", mailfile))
    unlink(mailfile)
  }
} # sendmail
