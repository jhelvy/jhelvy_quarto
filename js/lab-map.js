// Map of where lab members are from, on lab.qmd. The data comes from
// make_lab_map() in _common.R (as json inside .lab-map); styles are in
// css/lab.css. Equal Earth projection; drag to pan; zoom with the buttons,
// double-click, pinch, or ctrl / cmd + scroll. Dots keep their size as the
// map zooms, so people from the same place separate as you zoom in.
(function () {
  const WORLD_URL = 'https://cdn.jsdelivr.net/npm/world-atlas@2.0.2/countries-50m.json';
  const ANTARCTICA = '010';
  const WIDTH = 1000;
  const DOT_R = 5.5;
  // Spacing (map units at zoom 1) of the spiral that fans out people who
  // share a hometown; it grows with the zoom while the dots don't
  const SPREAD = 3.2;
  const MAX_ZOOM = 16;

  function init(el) {
    const people = JSON.parse(el.querySelector('.lab-map-data').textContent);
    const canvas = el.querySelector('.lab-map-canvas');

    // Fit the whole globe to the width, then crop to the inhabited latitudes
    const projection = d3.geoEqualEarth().fitWidth(WIDTH, { type: 'Sphere' });
    const path = d3.geoPath(projection);
    const top = projection([0, 84])[1];
    const bottom = projection([0, -56])[1];
    const extent = [[0, top], [WIDTH, bottom]];

    const svg = d3.select(canvas)
      .append('svg')
      .attr('viewBox', [0, top, WIDTH, bottom - top].join(' '))
      .attr('role', 'img')
      .attr('aria-label', 'World map of where lab members are from');
    const world = svg.append('g');
    world.append('path')
      .attr('class', 'lab-map-ocean')
      .attr('d', path({ type: 'Sphere' }));
    const land = world.append('g');

    // Sunflower spiral around each shared hometown
    const counts = d3.rollup(people, v => v.length, d => d.lat + ',' + d.lng);
    const seen = new Map();
    people.forEach(d => {
      const key = d.lat + ',' + d.lng;
      const i = seen.get(key) || 0;
      seen.set(key, i + 1);
      const [x, y] = projection([d.lng, d.lat]);
      const r = counts.get(key) > 1 ? SPREAD * Math.sqrt(i + 0.5) : 0;
      d.px = x + r * Math.cos(i * 2.39996);
      d.py = y + r * Math.sin(i * 2.39996);
    });

    const tip = d3.select(canvas).append('div').attr('class', 'lab-map-tip');
    const dots = svg.append('g')
      .selectAll('circle')
      .data(people)
      .join('circle')
      .attr('class', d => 'lab-map-dot ' + d.status)
      .attr('r', DOT_R)
      .attr('cx', d => d.px)
      .attr('cy', d => d.py)
      .attr('tabindex', 0)
      .attr('aria-label', d => d.name + ', ' + d.home)
      .on('mouseenter focus', (event, d) => showTip(event, d))
      .on('mousemove', (event) => placeTip(event))
      .on('mouseleave blur', hideTip)
      .on('click', (event, d) => { window.location.hash = d.id; })
      .on('keydown', (event, d) => {
        if (event.key === 'Enter') window.location.hash = d.id;
      });

    // Built from text nodes, not html, so names and places are never parsed
    function showTip(event, d) {
      tip.selectAll('*').remove();
      tip.append('img')
        .attr('src', d.image)
        .attr('alt', '')
        .attr('width', 48)
        .attr('height', 48);
      const text = tip.append('div');
      text.append('strong').text(d.name);
      d.role.split('; ').forEach(line => text.append('div').text(line));
      text.append('div').attr('class', 'lab-map-tip-home')
        .call(div => div.append('i').attr('class', 'fas fa-location-dot'))
        .call(div => div.append('span').text(' ' + d.home));
      tip.classed('is-visible', true);
      svg.classed('has-hover', true);
      dots.classed('is-active', p => p === d);
      placeTip(event);
    }

    function placeTip(event) {
      const box = canvas.getBoundingClientRect();
      const node = tip.node();
      // Keyboard focus has no pointer position, so anchor on the dot itself
      let x = event.clientX, y = event.clientY;
      if (x === undefined) {
        const dot = event.target.getBoundingClientRect();
        x = dot.x + dot.width / 2;
        y = dot.y + dot.height / 2;
      }
      let left = x - box.left + 14;
      let topPos = y - box.top + 14;
      // Flip to the other side of the pointer rather than run off the map
      if (left + node.offsetWidth > box.width) left = x - box.left - node.offsetWidth - 14;
      if (topPos + node.offsetHeight > box.height) topPos = y - box.top - node.offsetHeight - 14;
      tip.style('left', Math.max(0, left) + 'px').style('top', Math.max(0, topPos) + 'px');
    }

    function hideTip() {
      tip.classed('is-visible', false);
      svg.classed('has-hover', false);
      dots.classed('is-active', false);
    }

    // So the map never traps page scrolling: a plain scroll wheel and a
    // one-finger swipe scroll the page; ctrl / cmd + wheel (and a trackpad
    // pinch, which browsers send as ctrl + wheel) zooms, and on touch screens
    // two fingers pan and pinch the map
    const zoom = d3.zoom()
      .scaleExtent([1, MAX_ZOOM])
      .extent(extent)
      .translateExtent(extent)
      .filter(event => {
        if (event.type === 'wheel') return event.ctrlKey || event.metaKey;
        if (event.type === 'touchstart') return event.touches.length > 1;
        return !event.ctrlKey && !event.button;
      })
      .on('start', hideTip)
      .on('zoom', ({ transform }) => {
        world.attr('transform', transform);
        dots.attr('cx', d => transform.applyX(d.px)).attr('cy', d => transform.applyY(d.py));
      });
    svg.call(zoom);

    el.querySelectorAll('.lab-map-controls button').forEach(button => {
      button.addEventListener('click', () => {
        const t = svg.transition().duration(350);
        const action = button.dataset.zoom;
        if (action === 'in') t.call(zoom.scaleBy, 2);
        if (action === 'out') t.call(zoom.scaleBy, 0.5);
        if (action === 'reset') t.call(zoom.transform, d3.zoomIdentity);
      });
    });

    // The country outlines load after the dots, which work without them
    d3.json(WORLD_URL).then(topo => {
      const countries = topojson.feature(topo, topo.objects.countries).features
        .filter(f => f.id !== ANTARCTICA);
      land.selectAll('path')
        .data(countries)
        .join('path')
        .attr('class', 'lab-map-country')
        .attr('d', path);
    });
  }

  function start() {
    document.querySelectorAll('.lab-map').forEach(init);
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
