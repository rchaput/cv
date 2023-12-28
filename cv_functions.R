#' Read a YAML data as a tibble
#'
#' It is important that each element in the YAML have the same keys.
#' For example:
#' ```
#' - title: "My element"
#'   date: "2023"
#'   details: "Some details about (...)"
#' - title: "My second element"
#'   date: "2021"
#'   details: "Other details about (...)"
#' ```
#' This will result in a 2x3 tibble, with 2 rows, and 3 columns (title, date,
#' and details).
#'
#' @param filepath The path to the YAML file.
#' @param tags_column (Optional) The name of the tags column, which will be
#'  nested: it will be a list of elements. If an element in the YAML is a list,
#'  the resulting Tibble would, by default, duplicate rows to have each list
#'  element in a separate row. This is not what we want for tags, which should
#'  be a column whose values are lists.
#'
#' @return A tibble in which each row is an element of the YAML, and the
#'  columns are the keys of the elements.
read_data <- function (filepath, tags_column = NULL) {
  yaml_data <- yaml::read_yaml(filepath)
  # `yaml_data` is a list of lists. The nested lists have names,
  # but the outer list does not.
  # We first want to convert each of the nested lists to a tibble.
  tibbles_list <- lapply(yaml_data, tibble::as_tibble)
  # We have a list of tibbles; they may contain several rows if one of the
  # columns was a list.
  # We first merge them together with `bind_rows` (equivalent to
  # `do.call(rbind, tibbles_list)` but faster).
  df <- dplyr::bind_rows(tibbles_list)
  # Then, if we must group values from one of the column, we use `nest` to
  # group values back into a list (for this specific column), grouping by
  # all other columns. This results in a tibble of N lines, for N elements in
  # the original YAML file.
  if (!is.null(tags_column)) {
    df <- df %>% tidyr::nest(tags = dplyr::all_of(tags_column))
  }
  df
}


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
#' @param url The (optional) URL to link the header to.
#' @param compact Set to `TRUE` to produce a shorter entry, by removing empty
#'  sections, such as details (`why`) and tags. By default, sections are always
#'  created, even if they are empty: this takes place in the resulting HTML,
#'  especially because of CSS rules. If only the `what` is specified, and
#'  `compact` is set to `TRUE`, this creates a one-line entry. If `with`,
#'  `where` are specified, they appear below the title (`what`), as expected.
#'  The `when` still appear as expected, but to ensure a compact format, only
#'  a single year should be provided (not a time span).
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
  url = NULL,
  compact = FALSE,
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
        what, with, where, url
      ),
      if (!(compact && is.null(why))) {
        .create_desc(
          why
        )
      } else {
        NULL
      },
      if (!(compact && is.null(tags))) {
        .create_tags(
          tags,
          tag_class
        )
      } else {
        NULL
      },
    )
  )

  result
}


#' Create several HTML entries from a data set.
#'
#' This function builds upon `detailed_entry` to produce entries for each
#' row, e.g., of a tibble.
#' The parameters have the same role as in `detailed_entry` except that,
#' instead of being the values themselves, they should be strings that
#' refer to the data set's column in which the values can be found.
#' For example, `what = "title"` means that the `what` value should be
#' obtained by looking at the `title` column (`data[["title"]]`).
#'
#' @return The HTML elements that correspond to the entries.
#'
#' @seealso detailed_entry
detailed_entries <- function (
  data,
  what = NULL,
  when = NULL,
  when_start = NULL,
  when_end = NULL,
  with = NULL,
  where = NULL,
  why = NULL,
  url = NULL,
  compact = FALSE,
  tags = NULL,
  tag_class = "chip1"
) {

  # Returns either the content of a column in the data, or NULL if no column
  # was specified (this allows to handle the default `NULL` arguments).
  get_column_or_default <- function (row, column, default = NULL) {
    if (is.null(column) || is.na(column)) {
      default
    } else {
      row[[column]]
    }
  }
  # Special handling for tags because it might be a tibble internally...
  get_tags <- function (row, column) {
    tags <- get_column_or_default(row, column, NULL)
    if (!is.null(tags) && typeof(tags) == "list") {
      # Return the first element of this list, which must be a tibble
      tags[[1]]
    } else {
      tags
    }
  }

  # `rowwise` will group data by row (each group is a single row)
  # `group_map` will apply a map operator on each group (= on each row)
  # `.x` represents the group (= the row)
  # The result is a list, containing as many elements as there are rows
  entries <- data %>%
    dplyr::rowwise() %>%
    dplyr::group_map(~ detailed_entry(
      what = get_column_or_default(.x, what),
      with = get_column_or_default(.x, with),
      where = get_column_or_default(.x, where),
      when = get_column_or_default(.x, when),
      when_start = get_column_or_default(.x, when_start),
      when_end = get_column_or_default(.x, when_end),
      why = get_column_or_default(.x, why),
      url = get_column_or_default(.x, url),
      compact = compact,
      tags = get_tags(.x, tags),
      tag_class = tag_class
    ))

  htmltools::tagList(
    entries
  )
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
    dates <- unlist(strsplit(as.character(when), " *- *"))
    end_date <- dates[1]
    # Note that `start_date` can be `NA` if there was no hyphen in `when`!
    start_date <- dates[2]
  }

  if (is.null(end_date) || is.na(end_date)) {
    end_date <- ""
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
#' @param url The optional URL that the header should link to.
#'
#' @return The header element, optionnally containing spans for the with and
#'   where, if they were provided.
.create_header <- function (
  title,
  with = NULL,
  where = NULL,
  url = NULL
) {

  element <- htmltools::tags$header(
    htmltools::h3(
      if(!is.null(url)) {
        htmltools::tags$a(title, href=url, target="_blank")
      } else {
        title
      }
    )
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
  if (typeof(content) == "list" && length(content) == 1) {
    # Special case, we might have imbricated lists, we want to handle the
    # single-element in the list as the content itself instead.
    # For example: `list(c("1st", "2nd"))` => `c("1st", "2nd")`.
    # This typically happens when using tibbles.
    result <- .parse_content_to_list(content[[1]])
  }
  else if (length(content) == 1) {
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
  if (is(tags, "tbl_df")) {
    # If it is a tibble, we must iterate over rows
    tags <- tags %>%
      dplyr::rowwise() %>%
      dplyr::group_map(~ create_tag(.x))
  } else {
    # If it is a list, we can simply use `lapply`
    tags <- lapply(tags, create_tag)
  }

  htmltools::div(
    class = 'tags',
    tags
  )
}
