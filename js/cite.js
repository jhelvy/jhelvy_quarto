// "Cite" buttons on the research pages open a pop-up, like Google Scholar's,
// with the pub's citation in a few styles and its BibTeX, each with its own
// copy button. The styles come from make_cite_styles() in _common.R, as json
// in the button's data-cite: {"APA": html, ..., "BibTeX": text}. Styles are
// copied as both rich text (so italics survive a paste into Word) and plain
// text. Styled by the .cite-dialog rules in css/theme.scss.
(function () {
  let dialog;

  function build() {
    dialog = document.createElement('dialog');
    dialog.className = 'cite-dialog';
    dialog.setAttribute('aria-labelledby', 'cite-dialog-title');
    // Focusable, so open() can focus the pop-up itself, not its close button
    dialog.tabIndex = -1;
    dialog.innerHTML =
      '<div class="cite-dialog-head">' +
      '<h2 id="cite-dialog-title">Cite</h2>' +
      '<button type="button" class="cite-dialog-close" aria-label="Close">&times;</button>' +
      '</div><div class="cite-dialog-body"></div>';
    dialog.querySelector('.cite-dialog-close').addEventListener('click', () => dialog.close());
    // A click on the backdrop lands on the dialog element itself
    dialog.addEventListener('click', event => {
      if (event.target === dialog) dialog.close();
    });
    document.body.appendChild(dialog);
  }

  function copy(html, text) {
    if (navigator.clipboard && window.ClipboardItem && html) {
      return navigator.clipboard.write([new ClipboardItem({
        'text/html': new Blob([html], { type: 'text/html' }),
        'text/plain': new Blob([text], { type: 'text/plain' })
      })]).catch(() => navigator.clipboard.writeText(text));
    }
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text);
    }
    // Older browsers, and pages served over plain http
    const area = document.createElement('textarea');
    area.value = text;
    area.style.position = 'fixed';
    area.style.opacity = '0';
    dialog.appendChild(area);
    area.select();
    document.execCommand('copy');
    area.remove();
    return Promise.resolve();
  }

  function row(label, content, isBibtex) {
    const div = document.createElement('div');
    div.className = 'cite-row';
    const name = document.createElement('div');
    name.className = 'cite-label';
    name.textContent = label;
    // The styles are html built (and escaped) in _common.R; BibTeX is text
    const text = document.createElement(isBibtex ? 'pre' : 'div');
    text.className = isBibtex ? 'cite-bibtex' : 'cite-text';
    if (isBibtex) text.textContent = content; else text.innerHTML = content;
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'cite-copy';
    button.textContent = 'Copy';
    button.addEventListener('click', () => {
      copy(isBibtex ? null : content, text.textContent.trim()).then(() => {
        button.textContent = 'Copied';
        button.classList.add('is-copied');
        setTimeout(() => {
          button.textContent = 'Copy';
          button.classList.remove('is-copied');
        }, 1600);
      });
    });
    div.append(name, text, button);
    return div;
  }

  function open(styles) {
    if (!dialog) build();
    const body = dialog.querySelector('.cite-dialog-body');
    body.replaceChildren(...Object.entries(styles).map(
      ([label, content]) => row(label, content, label === 'BibTeX')
    ));
    dialog.showModal();
    dialog.focus();
  }

  document.addEventListener('click', event => {
    const button = event.target.closest('.cite-button');
    if (button) open(JSON.parse(button.dataset.cite));
  });
})();
