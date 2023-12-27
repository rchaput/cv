# Helper methods to print contact information.

cv.contact <- function(icon, url, label = "", alt = "") {
    return(htmltools::tags$a(
        htmltools::tags$i(class=icon),
        label,
        href=url,
        alt=alt,
        title=alt,
        target="_blank",
    ))
}

cv.contact.home <- function (url, label = "") {
    cv.contact("fa fa-home", url, label, alt = "Homepage")
}

cv.contact.github <- function (username, show_username = FALSE) {
    url <- paste0("https://github.com/", username, "/")
    label <- if (show_username) username else ""
    cv.contact("fa fa-github", url, label, alt = "GitHub account")
}

cv.contact.email <- function (address, show_address = FALSE) {
    url <- paste0("mailto:", address)
    label <- if (show_address) address else ""
    cv.contact("fa fa-envelope", url, label, alt = "E-Mail")
}

cv.contact.twitter <- function (username, show_username = FALSE) {
    url <- paste0("https://twitter.com/", username)
    label <- if (show_username) username else ""
    cv.contact("fa fa-twitter", url, label, alt = "Twitter")
}

cv.contact.orcid <- function (orcid, show_orcid = FALSE) {
    url <- paste0("https://orcid.org/", orcid)
    label <- if (show_orcid) orcid else ""
    cv.contact("fab fa-orcid", url, label, alt = "ORCID")
}

cv.contact.linkedin <- function (username, show_username = FALSE) {
    url <- paste0("https://www.linkedin.com/in/", username)
    label <- if (show_username) username else ""
    cv.contact("fab fa-linkedin", url, label, alt = "Linkedin")
}

cv.contact.researchgate <- function (username, show_username = FALSE) {
    url <- paste0("https://www.researchgate.net/profile/", username)
    label <- if (show_username) username else ""
    cv.contact("ai ai-researchgate", url, label, alt = "Research Gate")
}

cv.contact.zotero <- function (username, show_username = FALSE) {
    url <- paste0("https://www.zotero.org/", username)
    label <- if (show_username) username else ""
    cv.contact("ai ai-zotero", url, label, alt = "Zotero")
}

cv.contact.dblp <- function (username) {
    url <- paste0("https://dblp.uni-trier.de/pid/", username, ".html")
    cv.contact("ai ai-dblp", url, alt = "DBLP")
}
