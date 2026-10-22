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

# Teaching page: build the collapsible "Past semesters" pill list for one
# course from content/courses.csv. Semesters with no url (no archived site)
# render as unlinked pills. Returned as a single line of html -- pandoc ends
# a raw html block at the first blank line.
make_semesters <- function(stub, file = file.path('content', 'courses.csv')) {
  d <- readr::read_csv(file, show_col_types = FALSE) %>%
    filter(course == stub) %>%
    # reverse chronological, Fall ahead of Spring within a year
    arrange(desc(year), desc(term == "Fall"))
  pills <- ifelse(
    is.na(d$url),
    glue::glue('<span>{d$term} {d$year}</span>'),
    glue::glue('<a href="{d$url}">{d$term} {d$year}</a>')
  )
  return(paste0(
    '<details class="semesters"><summary>Semesters Taught (', nrow(d),
    ')</summary><div class="semester-pills">', paste(pills, collapse = ''),
    '</div></details>'
  ))
}

# Lab page: build the People section from content/lab.csv. Current members
# are grouped by position; alumni follow in one group, in csv order. Returned
# as markdown; wrap the call in knitr::asis_output().
make_lab_people <- function(file = file.path('content', 'lab.csv')) {
  people <- readr::read_csv(file, show_col_types = FALSE)
  sections <- c(
    faculty   = "Faculty",
    postdoc   = "Post-Docs",
    phd       = "Ph.D. Students",
    masters   = "Masters Students",
    undergrad = "Undergraduate Students"
  )
  out <- c()
  for (pos in names(sections)) {
    d <- filter(people, status == "current", position == pos)
    if (nrow(d) > 0) {
      out <- c(out, paste("##", sections[[pos]]), make_lab_group(d))
    }
  }
  alumni <- filter(people, status == "alumni")
  if (nrow(alumni) > 0) {
    out <- c(out, "## Alumni", make_lab_group(alumni))
  }
  return(paste(out, collapse = "\n\n"))
}

make_lab_group <- function(d) {
  return(vapply(split(d, seq_len(nrow(d))), make_lab_person, character(1)))
}

# One card per person, styled by css/lab.css. The ### heading must be the
# first block in the card or pandoc drops the name from the TOC (see the
# note in css/teaching.css); css places the photo back in the left column.
make_lab_person <- function(p) {
  name <- p$name
  if (!is.na(p$name_native)) { name <- glue::glue("{name} ({p$name_native})") }
  if (!is.na(p$credentials)) { name <- glue::glue("{name}, {p$credentials}") }
  # Multiple degrees are separated by "; " in the csv
  meta <- NULL
  if (!is.na(p$degree)) {
    meta <- c(
      "::: {.person-meta}",
      str_replace_all(p$degree, "; ", " &middot; "),
      ":::"
    )
  }
  return(paste(c(
    ":::: {.person-card}",
    glue::glue("### {name} {{#{lab_person_id(p$name)}}}"),
    "::: {.person-photo}",
    glue::glue('<img src="images/lab/{p$image}" alt="{p$name}">'),
    ":::",
    meta,
    p$bio,
    "::::"
  ), collapse = "\n\n"))
}

# Anchor id for a person's card, shared by the cards and the map's click links
lab_person_id <- function(name) {
  slug <- str_replace_all(str_to_lower(name), "[^a-z0-9]+", "-")
  return(paste0("person-", str_replace_all(slug, "^-|-$", "")))
}

# Lab page: interactive world map of where lab members are from. This packs
# the lat / lng and home_* columns of content/lab.csv into the page as json;
# js/lab-map.js draws the map (d3, Equal Earth projection) and css/lab.css
# styles it. Hovering a dot shows the person; clicking jumps to their card.
make_lab_map <- function(file = file.path('content', 'lab.csv')) {
  role_labels <- c(
    faculty   = "Faculty",
    postdoc   = "Post-Doc",
    phd       = "Ph.D. Student",
    masters   = "Masters Student",
    undergrad = "Undergraduate Student"
  )
  people <- readr::read_csv(file, show_col_types = FALSE) %>%
    filter(!is.na(lat), !is.na(lng)) %>%
    mutate(
      id = lab_person_id(name),
      # Alumni show their degrees (still "; "-separated, one line each)
      role = ifelse(
        status == "alumni" & !is.na(degree),
        degree,
        unname(role_labels[position])
      ),
      home = ifelse(
        is.na(home_city), home_country, paste0(home_city, ", ", home_country)
      ),
      image = file.path("images", "lab", image)
    ) %>%
    select(name, id, status, role, home, image, lat, lng)

  zoom_button <- function(action, label, text) {
    tags$button(
      type = "button", `data-zoom` = action, `aria-label` = label,
      title = label, text
    )
  }

  return(tagList(
    div(
      class = "lab-map",
      div(
        class = "lab-map-legend",
        span(class = "lab-map-key current"), "Current members",
        span(class = "lab-map-key alumni"), "Alumni"
      ),
      div(
        class = "lab-map-canvas",
        div(
          class = "lab-map-controls",
          zoom_button("in", "Zoom in", "+"),
          zoom_button("out", "Zoom out", HTML("&minus;")),
          zoom_button("reset", "Reset map", HTML("&#8634;"))
        )
      ),
      tags$script(
        type = "application/json", class = "lab-map-data",
        HTML(jsonlite::toJSON(people))
      )
    ),
    tags$script(src = "https://cdn.jsdelivr.net/npm/d3@7.9.0/dist/d3.min.js"),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/topojson-client@3.1.0/dist/topojson-client.min.js"
    ),
    tags$script(src = "js/lab-map.js")
  ))
}
