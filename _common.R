library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(fontawesome)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

gscholar_stats <- function(url) {
  cites <- get_cites(url)
  return(glue::glue(
    'Citations: {cites$citations} | h-index: {cites$hindex} | i10-index: {cites$i10index}'
  ))
}

get_cites <- function(url) {
  html <- xml2::read_html(url)
  node <- rvest::html_nodes(html, xpath='//*[@id="gsc_rsb_st"]')
  cites_df <- rvest::html_table(node)[[1]]
  cites <- data.frame(t(as.data.frame(cites_df)[,2]))
  names(cites) <- c('citations', 'hindex', 'i10index')
  return(cites)
}

get_pubs <- function() {
    pubs <- gsheet::gsheet2tbl(
        url = 'https://docs.google.com/spreadsheets/d/1xyzgW5h1rVkmtO1rduLsoNRF9vszwfFZPd72zrNmhmU')
    pubs <- make_citations(pubs)
    pubs$summary <- ifelse(is.na(pubs$summary), FALSE, pubs$summary)
    pubs$stub <- make_stubs(pubs)
    pubs$url_summary <- file.path('research', pubs$stub, "index.html")
    pubs$url_scholar <- ifelse(
      is.na(pubs$id_scholar), NA, 
      glue::glue('https://scholar.google.com/citations?view_op=view_citation&hl=en&user=DY2D56IAAAAJ&citation_for_view=DY2D56IAAAAJ:{pubs$id_scholar}')
    )
    return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

make_citation <- function(pub) {
  if (!is.na(pub$journal)) {
    pub$journal <- glue::glue('_{pub$journal}_.')
  }
  if (!is.na(pub$number)) {
    pub$number <- glue::glue('{pub$number}.')
  }
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }
  pub$year <- glue::glue("({pub$year})")
  pub$title <- glue::glue('"{pub$title}"')
  pub[,which(is.na(pub))] <- ''
  return(paste(
    pub$author, pub$year, pub$title, pub$journal, 
    pub$number, pub$doi
  ))
}

make_doi <- function(doi) {
  return(glue::glue('DOI: [{doi}](https://doi.org/{doi})'))
}

make_stubs <- function(pubs) {
    journal <- str_to_lower(pubs$journal)
    journal <- str_replace_all(journal, ':', '')
    journal <- str_replace_all(journal, '`', '')
    journal <- str_replace_all(journal, "'", '')
    journal <- str_replace_all(journal, "\\.", '')
    journal <- str_replace_all(journal, "&", '')
    journal <- str_replace_all(journal, ',', '')
    journal <- str_replace_all(journal, '  ', '-')
    journal <- str_replace_all(journal, ' ', '-')
    return(paste0(pubs$year, '-', journal))
}

make_pub_list <- function(pubs, category) {
    x <- pubs[which(pubs$category == category),]
    pub_list <- list()
    for (i in 1:nrow(x)) {
      pub_list[[i]] <- make_pub(x[i,], index = i)
    }
    return(htmltools::HTML(paste(unlist(pub_list), collapse = "")))
}

make_pub <- function(pub, index = NULL) {
  header <- FALSE
  altmetric <- make_altmetric(pub)
  if (is.null(index)) {
    cite <- pub$citation
    icons <- make_icons(pub)
  } else {
    cite <- glue::glue('{index}) {pub$citation}')
    icons <- glue::glue('<ul style="list-style: none;"><li>{make_icons(pub)}</li></ul>')
    if (index == 1) { header <- TRUE }
  }
  # return(markdown_to_html(cite))
  return(htmltools::HTML(glue::glue(
    '<div class="pub">
    <div class="grid">
    <div class="g-col-11"> {markdown_to_html(cite)} </div>
    <div class="g-col-1"> {altmetric} </div>
    </div>
    {icons}
    </div>{make_haiku(pub, header)}'
  )))
}

make_altmetric <- function(pub) {
  altmetric <- ""
  if (pub$category == 'peer_reviewed') {
    altmetric <- glue::glue('<div data-badge-type="donut" data-doi="{pub$doi}" data-hide-no-mentions="true" class="altmetric-embed"></div>')
  }
  return(altmetric)
}

make_haiku <- function(pub, header = FALSE) {
  html <- ""
  haiku <- em(
    pub$haiku1, HTML("&#8226;"), 
    pub$haiku2, HTML("&#8226;"), 
    pub$haiku3
  )
  if (!is.na(pub$haiku1)) {
    if (header) {
      html <- as.character(aside_center(list(
        HTML("<b>Haiku Summary</b>"), br(), haiku))
      )
    } else {
      html <- as.character(aside_center(list(haiku)))
    }
  }
  return(html)
}

aside <- function(text) {
  return(tag("aside", list(text)))
}

center <- function(text) {
  return(tag("center", list(text)))
}

aside_center <- function(text) {
  return(aside(center(list(text))))
}

aside_center_b <- function(text) {
  return(aside(center(list(tag("b", text)))))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  
  # Replace the author names with underlined last names
  text <- gsub(
    pattern = "\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\1</u>, \\2", 
    text
  )
  text <- gsub(
    pattern = "\\\\\\*\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\\\*\\1</u>, \\2", 
    text
  )
  
  # Render the text as HTML
  return(HTML(markdown::renderMarkdown(text = text)))
}

make_icons <- function(pub) {
  html <- c()
  if (pub$summary) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "Summary",
      url  = pub$url_summary, 
      class = "icon-link-summary", 
      target = "_self"
    )))      
  }
  if (!is.na(pub$url_pub)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "View",
      url  = pub$url_pub
    )))
  }
  if (!is.na(pub$url_pdf)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-file-pdf",
      text = "PDF",
      url  = pub$url_pdf
    )))
  }
  if (!is.na(pub$url_repo)) {
    html <- c(html, as.character(icon_link(
      icon = "fab fa-github",
      text = "Code & Data",
      url  = pub$url_repo
    )))
  }
  if (!is.na(pub$url_other)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = pub$other_label,
      url  = pub$url_other
    )))
  }
  if (!is.na(pub$url_rg)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-researchgate",
      # text = "&nbsp;",
      text = "RG",
      url  = pub$url_rg
    )))
  }
  if (!is.na(pub$url_scholar)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-google-scholar",
      # text = "&nbsp;",
      text = "Scholar",
      url  = pub$url_scholar
    )))
  }
  return(paste(html, collapse = ""))
}

# The icon_link() function is in {distilltools}, but I've modified this
# one to include  a custom class to be able to have more control over the
# CSS and an optional target argument

icon_link <- function(
  icon = NULL,
  text = NULL,
  url = NULL,
  class = "icon-link",
  target = "_blank"
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(
    href = url, text, class = class, target = target, rel = "noopener"
  ))
}

make_icon_text <- function(icon, text) {
  return(HTML(paste0(make_icon(icon), " ", text)))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

last_updated <- function() {
  return(span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}

make_media_list <- function() {
  media <- gsheet::gsheet2tbl(
    url = 'https://docs.google.com/spreadsheets/d/1xyzgW5h1rVkmtO1rduLsoNRF9vszwfFZPd72zrNmhmU/edit#gid=2088158801')
  temp <- media %>% 
    mutate(
      date = format(date, format = "%b %d, %Y"), 
      outlet = paste0("**", outlet, "**"),
      post = paste0("- ", date, " - ", outlet, ": ", post)
    )
  return(paste(temp$post, collapse = "\n"))
}
