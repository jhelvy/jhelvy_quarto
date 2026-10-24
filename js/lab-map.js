// Globe of where lab members are from, on lab.qmd. The data comes from
// make_lab_map() in _common.R (as json inside .lab-map); styles are in
// css/lab.css. Orthographic projection: drag to spin the globe; zoom with
// the scroll wheel, the buttons, double-click, or pinch. The land is drawn
// on a canvas (fast enough to redraw on every frame of a drag) and the dots
// are an svg on top of it, so they keep their tooltips, focus, and clicks.
// Dots keep their size as the globe zooms, so people from the same place
// separate as you zoom in.
(function () {
  const WORLD_URL = 'https://cdn.jsdelivr.net/npm/world-atlas@2.0.2/';
  const DOT_R = 5.5;
  // Spacing (px at zoom 1) of the spiral that fans out people who share a
  // hometown; it grows with the zoom while the dots don't
  const SPREAD = 2.6;
  const MAX_ZOOM = 12;
  const MAX_HEIGHT = 520;
  // Degrees of spin per pixel of drag at zoom 1
  const SPIN = 0.3;

  function init(el) {
    const people = JSON.parse(el.querySelector('.lab-map-data').textContent);
    const canvasBox = el.querySelector('.lab-map-canvas');

    // Start centered on the middle of where everyone is from, tilted no
    // further than 30 degrees so the globe never opens on a pole
    const [cLng, cLat] = d3.geoCentroid({
      type: 'MultiPoint',
      coordinates: people.map(d => [d.lng, d.lat])
    });
    const home = [-cLng, -Math.max(-30, Math.min(30, cLat))];
    let rotation = home.slice();
    let k = 1;
    let width = 0, height = 0, radius = 0;

    const projection = d3.geoOrthographic().clipAngle(90).precision(0.3);
    const graticule = d3.geoGraticule10();
    let land110, borders110, land50, borders50;

    const canvas = d3.select(canvasBox).append('canvas')
      .attr('class', 'lab-map-globe')
      .attr('aria-hidden', 'true');
    const context = canvas.node().getContext('2d');
    const path = d3.geoPath(projection, context);

    const svg = d3.select(canvasBox).append('svg')
      .attr('role', 'img')
      .attr('aria-label', 'Globe showing where lab members are from');

    // Index of each person among those sharing their hometown, for the spiral
    const counts = d3.rollup(people, v => v.length, d => d.lat + ',' + d.lng);
    const seen = new Map();
    people.forEach(d => {
      const key = d.lat + ',' + d.lng;
      d.i = seen.get(key) || 0;
      d.shared = counts.get(key) > 1;
      seen.set(key, d.i + 1);
    });

    const tip = d3.select(canvasBox).append('div').attr('class', 'lab-map-tip');
    const dots = svg.append('g')
      .selectAll('circle')
      .data(people)
      .join('circle')
      .attr('class', d => 'lab-map-dot ' + d.status)
      .attr('r', DOT_R)
      .attr('tabindex', 0)
      .attr('aria-label', d => d.name + ', ' + d.home)
      .on('mouseenter focus', (event, d) => showTip(event, d))
      .on('mousemove', (event) => placeTip(event))
      .on('mouseleave blur', hideTip)
      .on('click', (event, d) => { window.location.hash = d.id; })
      .on('keydown', (event, d) => {
        if (event.key === 'Enter') window.location.hash = d.id;
      });

    // While the globe moves it draws the light 110m outlines; once it
    // settles it redraws with the detailed 50m ones
    let settle;
    function render(moving) {
      projection.rotate(rotation).scale(radius * k);
      draw(moving);
      placeDots();
      clearTimeout(settle);
      if (moving) settle = setTimeout(() => draw(false), 150);
    }

    function draw(moving) {
      const ratio = window.devicePixelRatio || 1;
      context.setTransform(ratio, 0, 0, ratio, 0, 0);
      context.clearRect(0, 0, width, height);
      const [cx, cy] = projection.translate();
      const r = projection.scale();

      // Ocean, lit from the upper left so it reads as a sphere
      const shade = context.createRadialGradient(
        cx - r * 0.35, cy - r * 0.35, r * 0.1, cx, cy, r
      );
      shade.addColorStop(0, '#fbfcfd');
      shade.addColorStop(1, '#dfe8f0');
      context.beginPath();
      path({ type: 'Sphere' });
      context.fillStyle = shade;
      context.fill();

      context.beginPath();
      path(graticule);
      context.strokeStyle = 'rgba(84, 128, 167, 0.12)';
      context.lineWidth = 0.6;
      context.stroke();

      const detailed = !moving && land50;
      const land = detailed ? land50 : land110;
      const borders = detailed ? borders50 : borders110;
      if (land) {
        context.beginPath();
        path(land);
        context.fillStyle = '#c9d4de';
        context.fill();
        context.beginPath();
        path(borders);
        context.strokeStyle = '#ffffff';
        context.lineWidth = 0.6;
        context.stroke();
      }

      context.beginPath();
      path({ type: 'Sphere' });
      context.strokeStyle = 'rgba(44, 63, 81, 0.25)';
      context.lineWidth = 1;
      context.stroke();
    }

    function placeDots() {
      const center = [-rotation[0], -rotation[1]];
      dots.each(function (d) {
        // Hide anyone on the far side of the globe
        const front = d3.geoDistance([d.lng, d.lat], center) < Math.PI / 2 - 0.02;
        const dot = d3.select(this).style('display', front ? null : 'none');
        if (!front) return;
        const [x, y] = projection([d.lng, d.lat]);
        const r = d.shared ? SPREAD * k * Math.sqrt(d.i + 0.5) : 0;
        dot.attr('cx', x + r * Math.cos(d.i * 2.39996))
          .attr('cy', y + r * Math.sin(d.i * 2.39996));
      });
    }

    function resize() {
      width = canvasBox.clientWidth;
      height = Math.min(width, MAX_HEIGHT);
      radius = height / 2 - 6;
      const ratio = window.devicePixelRatio || 1;
      canvas.attr('width', width * ratio).attr('height', height * ratio)
        .style('width', width + 'px').style('height', height + 'px');
      svg.attr('width', width).attr('height', height)
        .attr('viewBox', [0, 0, width, height].join(' '));
      projection.translate([width / 2, height / 2]);
      zoom.extent([[0, 0], [width, height]]);
      render(false);
    }

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
      const box = canvasBox.getBoundingClientRect();
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
      // Flip to the other side of the pointer rather than run off the globe
      if (left + node.offsetWidth > box.width) left = x - box.left - node.offsetWidth - 14;
      if (topPos + node.offsetHeight > box.height) topPos = y - box.top - node.offsetHeight - 14;
      tip.style('left', Math.max(0, left) + 'px').style('top', Math.max(0, topPos) + 'px');
    }

    function hideTip() {
      tip.classed('is-visible', false);
      svg.classed('has-hover', false);
      dots.classed('is-active', false);
    }

    // d3.zoom handles the gestures; its pan becomes spin and its scale
    // becomes zoom. Zooming is always about the center of the globe, so a
    // change of scale ignores d3's matching pan. At full zoom in or out the
    // scroll wheel goes back to scrolling the page (d3 lets the event through
    // when the scale can't change), so scrolling down past the globe is never
    // trapped. On touch screens one finger scrolls the page and two fingers
    // spin and pinch the globe.
    let last = d3.zoomIdentity;
    const zoom = d3.zoom()
      .scaleExtent([1, MAX_ZOOM])
      .filter(event => {
        if (event.type === 'touchstart') return event.touches.length > 1;
        return !event.button;
      })
      .on('start', hideTip)
      .on('zoom', ({ transform, sourceEvent }) => {
        // Only a drag spins; the buttons' transitions have no source event
        if (sourceEvent && transform.k === last.k) {
          const step = SPIN / transform.k;
          rotation = [
            rotation[0] + (transform.x - last.x) * step,
            Math.max(-90, Math.min(90, rotation[1] - (transform.y - last.y) * step))
          ];
        }
        k = transform.k;
        last = transform;
        render(true);
      });
    svg.call(zoom);

    el.querySelectorAll('.lab-map-controls button').forEach(button => {
      button.addEventListener('click', () => {
        const action = button.dataset.zoom;
        if (action === 'in') svg.transition().duration(350).call(zoom.scaleBy, 2);
        if (action === 'out') svg.transition().duration(350).call(zoom.scaleBy, 0.5);
        if (action === 'reset') {
          const spin = d3.interpolate(rotation, home);
          svg.transition().duration(600)
            .call(zoom.transform, d3.zoomIdentity)
            .tween('spin', () => t => { rotation = spin(t); render(true); });
        }
      });
    });

    new ResizeObserver(resize).observe(canvasBox);

    // The outlines load after the globe and dots, which work without them
    function outlines(topo) {
      return [
        topojson.feature(topo, topo.objects.land),
        topojson.mesh(topo, topo.objects.countries, (a, b) => a !== b)
      ];
    }
    d3.json(WORLD_URL + 'countries-110m.json').then(topo => {
      [land110, borders110] = outlines(topo);
      draw(false);
      return d3.json(WORLD_URL + 'countries-50m.json');
    }).then(topo => {
      [land50, borders50] = outlines(topo);
      draw(false);
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
