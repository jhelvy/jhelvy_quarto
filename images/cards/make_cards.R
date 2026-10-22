# Social-media preview cards for the main sections of the site (the image
# that shows up when a page is shared on Bluesky, LinkedIn, etc.). Each card
# is a small html page screenshotted with webshot2, then saved as a jpg:
# 1200 x 630 is the size the platforms expect for large cards, and jpgs stay
# well under Bluesky's ~1MB limit. Each section's .qmd points at its card in
# its open-graph and twitter-card front matter.
#
# Re-run from the project root after changing a card:
# source(file.path('images', 'cards', 'make_cards.R'))

library(glue)

cards <- list(
  about = list(
    title = "About",
    tagline = "Associate Professor of Engineering Management &amp; Systems Engineering at GW",
    visual = '<img class="fill" src="../john_dancing.jpg">'
  ),
  lab = list(
    title = "Lab",
    tagline = "The Technology Change Lab studies how consumers, markets, and policy shape technological change",
    visual = '<img class="contain" src="../lab/logo/logo.png">'
  ),
  research = list(
    title = "Research",
    tagline = "Technological change, consumer preferences, and clean energy policy",
    visual = paste0(
      '<div class="figs">',
      '<img src="../../research/2022-nature/2022-nature.png">',
      '<img src="../../research/2023-joule/2023-joule.png">',
      '</div>'
    )
  ),
  teaching = list(
    title = "Teaching",
    tagline = "Courses on programming and data analysis in R at GW",
    visual = paste0(
      '<div class="hexes three">',
      '<img src="../hex/p4a.png"><img src="../hex/eda.png"><img src="../hex/madd.png">',
      '</div>'
    )
  ),
  projects = list(
    title = "Projects",
    tagline = "R packages, software, and other side projects",
    visual = paste0(
      '<div class="hexes six">',
      '<img src="../hex/logitr.png"><img src="../hex/cbcTools.png">',
      '<img src="../hex/surveydown.png"><img src="../hex/renderthis.png">',
      '<img src="../hex/earmark.png"><img src="../hex/vehicletrends.png">',
      '</div>'
    )
  ),
  blog = list(
    title = "Blog",
    tagline = "Writing on R, data, research, and the tools I use",
    visual = '<img class="portrait" src="../lab/john_helveston_square.png">'
  )
)

# Site colors from css/theme.scss. Placeholders are [[ ]] since the css
# is full of { }
template <- '<!doctype html>
<html><head><meta charset="utf-8">
<link href="https://fonts.googleapis.com/css2?family=Raleway:wght@500;600;800&display=block" rel="stylesheet">
<style>
  html, body { margin: 0; }
  body { width: 1200px; height: 630px; background: #f2f6f9; font-family: Raleway, sans-serif; }
  .wrap { display: flex; align-items: center; gap: 64px; height: 100%; box-sizing: border-box; padding: 0 72px; }
  .text { flex: 1; }
  .site { font-size: 30px; font-weight: 600; color: #5480a7; }
  .bar { width: 88px; height: 8px; border-radius: 4px; background: #56a754; margin: 34px 0 30px; }
  h1 { margin: 0; font-size: 100px; font-weight: 800; line-height: 1; color: #2c3f51; }
  .tagline { margin-top: 26px; font-size: 32px; font-weight: 500; line-height: 1.35; color: #5480a7; }
  .byline { margin-top: 40px; font-size: 24px; font-weight: 600; color: #6b7c8c; }
  .visual { flex: none; width: 450px; height: 450px; box-sizing: border-box; padding: 28px;
    display: flex; align-items: center; justify-content: center; overflow: hidden;
    background: #fff; border: 1px solid rgba(0, 0, 0, 0.1); border-radius: 10px;
    box-shadow: 0 0 16px 1px rgba(84, 128, 167, 0.22), 0 6px 16px rgba(84, 128, 167, 0.16); }
  .visual img { display: block; }
  .visual:has(.fill) { padding: 0; }
  .fill { width: 100%; height: 100%; object-fit: cover; }
  .contain { max-width: 100%; max-height: 100%; }
  .portrait { width: 100%; border-radius: 50%; }
  .figs { display: flex; flex-direction: column; gap: 22px; width: 100%; }
  .figs img { width: 100%; }
  .hexes { display: flex; flex-wrap: wrap; justify-content: center; }
  .hexes.three img { width: 172px; margin: 0 4px -16px; }
  .hexes.six img { width: 120px; margin: 0 3px -8px; }
</style></head>
<body><div class="wrap">
  <div class="text">
    <div class="site">jhelvy.com</div>
    <div class="bar"></div>
    <h1>[[title]]</h1>
    <div class="tagline">[[tagline]]</div>
    <div class="byline">John Paul Helveston</div>
  </div>
  <div class="visual">[[visual]]</div>
</div></body></html>'

dir <- file.path('images', 'cards')
for (name in names(cards)) {
  card <- cards[[name]]
  html <- file.path(dir, paste0(name, '.html'))
  png <- tempfile(fileext = '.png')
  writeLines(glue_data(card, template, .open = '[[', .close = ']]'), html)
  webshot2::webshot(html, png, vwidth = 1200, vheight = 630, zoom = 2, delay = 1.5)
  magick::image_read(png) |>
    magick::image_resize('1200x630') |>
    magick::image_flatten() |>
    magick::image_write(file.path(dir, paste0(name, '.jpg')), quality = 85)
  unlink(c(html, png))
}
