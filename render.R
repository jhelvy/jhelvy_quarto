source(file.path("R", "functions.R"))

# Fist build the footer to capture today's date
create_footer()

# Build pages for "research" folder
make_research_pages()

# Then render the site
rmarkdown::render_site(encoding = 'UTF-8')
