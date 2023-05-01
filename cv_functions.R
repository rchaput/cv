#' Create a new (HTML) entry.
#'
#' This function allows controlling many details about the entry (see the
#' parameters).
#' Note: most parameters follow `vitae`'s convention (what, with, where, when,
#' why). This should be intuitive enough for most of them.
#'
#'
#' @param what The title of the entry.
#' @param with The company or organization of the entry. Might also serve as
#'  a sub-title.
#' @param where The location of the entry. Pre-pended with a map-marker icon.
#' @param when The date(s) of the entry. Can be a single date (`2019`), or a
#'  period (`2019 - 2022`). See also `when_start`, `when_end` and the
#'  `.create_date_div` documentation.
#' @param when_start The starting date of the entry. Must be used in conjunction
#'  with `when_end`, but not with `when` (i.e., specify only `when` OR
#'  `when_start` and `when_end`).
#' @param when_end The ending date of the entry. Must be used in conjunction
#'  with `when_start`.
#' @param why The details (additional information) about the entry. Either a
#'  single element, which will be represented as a paragraph; or a (potentially
#'  nested) list, which will be represented as bullet points.
#' @param tags Optional tags about the entry.
#' @param tag_class The CSS class to use for the tags.
#'
#' @return The HTML elements that correspond to the entry.
detailed_entry <- function (
  what,
  with = NULL,
  where = NULL,
  when = NULL,
  when_start = NULL,
  when_end = NULL,
  why = NULL,
  tags = NULL,
  tag_class = "chip1"
) {

  id <- .sanitize_id(what)

  result <- htmltools::div(
    id = id,
    class = 'section level3 blocks',
    .create_date_div(
      when, when_start, when_end
    ),
    htmltools::div(
      class = 'decorator',
    ),
    htmltools::div(
      class = 'details',
      .create_header(
        what, with, where
      ),
      .create_desc(
        why
      ),
      .create_tags(
        tags,
        tag_class
      ),
    )
  )

  result
}


#' @keywords internal
#' Sanitize a title into an HTML-valid ID.
#'
#' In particular, an ID *must not* contain any whitespace. It is also better
#' to start them with a letter.
#' We also set the ID to lowercase, purely by convention.
#' For example, `My Super Title` will be transformed to `entry-my-super-title`.
#'
#' For more details on HTML IDs, see
#' https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id
#'
#' @param title The title of the entry to transform into a valid ID.
#'
#' @return The title, transformed to lowercase, with all whitespaces replaced
#'  by hyphens (`-`), and beginning with `entry-` to ensure it begins with
#'  a letter.
.sanitize_id <- function (title) {
  paste0(
    "entry-",
    stringr::str_replace_all(
      tolower(title),
      pattern = stringr::regex("\\s"),
      replacement = "-"
    )
  )
}


#' @keywords internal
#' Create the date div of an entry.
#'
#' @param when The *full* date to use. It may be a single point of time, e.g.,
#'   `"2022"`, or a composed date of two points, separated by a hyphen, e.g.,
#'   `"2022 - 2019"`. Note that, when `when` is specified, it takes precedence
#'   over `end_date` and `start_date`.
#' @param start_date The starting point of time when using a composed date.
#'   This point will appear below the `end_date` (e.g., "2022" on top and "2019"
#'   below), so that the entries can be listed in a reverse chronological order.
#' @param end_date The ending point of time when using a composed date.
#'   This point will appear on top of the `start_date` (e.g., "2022" on top and
#'   "2019" below), so that entries can be listed in a reverse chronological order.
#'   `end_date` *should* be used in conjunction with `start_date`, although it
#'   would work alone. However, it has in this case the same effect of passing
#'   the same value to `when`, which is the recommended way. In other words,
#'   `end_date = "2022"` and `when = "2022"` have the same result. `end_date`
#'   and `start_date` should be used when the two data are already available in
#'   separate variables (e.g., because of the dataset's structure).
#'
#' @return The HTML element (div) representing the date(s).
#'
#' @examples
#' .create_date_div("2022")
#'
#' .create_date_div("2022 - 2019")
#'
#' .create_date_div(end_date = "2022", start_date = "2019")
.create_date_div <- function (
  when = NULL,
  start_date = NULL,
  end_date = NULL
) {

  if (!is.null(when)) {
    # Split by a hyphen `-`, potentially surrounded by spaces ` *`.
    dates <- unlist(strsplit(when, " *- *"))
    end_date <- dates[1]
    # Note that `start_date` can be `NA` if there was no hyphen in `when`!
    start_date <- dates[2]
  }

  if (is.null(end_date) || is.na(end_date)) {
    warning("At least `when` or `end_date` should be specified!")
  }

  element <- htmltools::div(
    class = 'date',
    htmltools::span(end_date),
  )

  if (!is.na(start_date) && !is.null(start_date)) {
    element <- htmltools::tagAppendChild(
      element,
      htmltools::span(start_date),
    )
  }

  element
}

#' @keywords internal
#' Create the header HTML element of an entry.
#'
#' @param title The title of the entry. Typically a string, but can be HTML code.
#' @param with Originally the "place" of the entry, or the "with" in vitae's
#'   terminology. Acts as a sub-title (just below the title). By default, `NULL`,
#'   in which case the element is skipped. An empty value (`""`) can be used
#'   to force creating the element, even if it visually does not contain anything.
#' @param where The location of the entry, typically a geographical place. By
#'   default, `NULL`, in which case the element is skipped. An empty value
#'   (`""`) can be used to force creating the element, but is not recommended:
#'   this will result in a FontAwesome map-marker icon next to nothing, which
#'   can be confusing.
#'
#' @return The header element, optionnally containing spans for the with and
#'   where, if they were provided.
.create_header <- function (
  title,
  with = NULL,
  where = NULL
) {

  element <- htmltools::tags$header(
    htmltools::h3(title)
  )

  if (!is.null(with)) {
    element <- htmltools::tagAppendChild(
      element,
      htmltools::span(
        class = 'place',
        with,
      )
    )
  }

  if (!is.null(where)) {
    element <- htmltools::tagAppendChild(
      element,
      htmltools::span(
        class = 'location',
        htmltools::tags$i(
          class = 'fa fa-map-marker-alt',
          `aria-hidden` = 'true',
        ),
        where
      )
    )
  }

  element
}

#' @keywords internal
#' Create the description div of an entry.
#'
#' This function does not much, see `.parse_content_to_list` for the details;
#' it is kept for future flexibility and clarity in the main function.
.create_desc <- function (
  content
) {
  htmltools::div(
    class = 'desc',
    .parse_content_to_list(content)
  )
}


#' @keywords internal
#' Parse the "details" content ("why") into a list or paragraph.
#'
#' This function works recursively to ensure we handle nested lists.
#' Single elements (e.g., `"A paragraph"`) are transformed into paragraphs
#' (`<p>`); lists of multiple elements are transformed into unordered lists
#' (`<ul>` and `<li>`).
#'
#' @param content The content to parse: might be a single element (paragraph)
#'  or a (potentially nested) list of bullet-points.
#'
#' @return A HTML tag, either `<p>` in the case of a single element, or `<ul>`
#'  otherwise. The `<ul>` contains as many `<li>` elements as the content
#'  contains elements, and the `<li>` may contain nested `<ul>` if necessary.
#'
#' @examples
#' .parse_content_to_list("A paragraph")
#' > <p>A paragraph</p>
#'
#' .parse_content_to_list(c("A", "list"))
#' > <ul><li><p>A</p></li><li><p>list</p></li></ul>
#'
#' .parse_content_to_list(list("A", list("nested", "list")))
#' > <ul><li><p>A</p></li><li><ul><li><p>nested</p></li><li><p>list</p></li></ul></li></ul>
#'
.parse_content_to_list <- function (content) {

  if (length(content) == 1) {
    # Single element, simply return a paragraph.
    result <- htmltools::p(
      content
    )
  } else {
    # List of multiple elements (potentially nested!)
    result <- htmltools::tags$ul()
    # We need to parse each of the elements into a list item first, and add
    # them as children.
    for (element in content) {
      result <- htmltools::tagAppendChild(
        result,
        htmltools::tags$li(
          .parse_content_to_list(element)
        )
      )
    }
  }

  result
}


#' @keywords internal
#' Create the (div) list of tags of an entry.
#'
#' @param tags The character vector of tags to display. For exemple,
#'   `c("Tag 1", "Tag 2")`.
#' @param tag_class The CSS class to use for each individual tag. This allows
#'   creating new classes and choosing which one to use, instead of overriding
#'   a default class. In addition, different entries may use different classes.
#'
#' @return The HTML div containing all tags.
.create_tags <- function (
  tags,
  tag_class = "chip1"
) {

  # Helper function to create a single tag.
  # For example, `"Tag 1"` becomes `<span class='chip1'>Tag 1</span>`.
  create_tag <- function (tag_name) {
    htmltools::span(class = tag_class, tag_name)
  }

  # Transform the list of tag names into a list of HTML tags.
  tags <- lapply(tags, create_tag)

  htmltools::div(
    class = 'tags',
    tags
  )
}
