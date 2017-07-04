module.exports = {
  config: {
    // default font size in pixels for all tabs
    fontSize: 14.5,

    // font family with optional fallbacks
    fontFamily: '"Roboto Mono for Powerline", "Meslo LG S for Powerline", monospace',

    // terminal cursor background color (hex)
    cursorColor: 'rgba(255,255,255,.4)',

    // color of the text
    foregroundColor: 'rgb(131,148,150)',

    // terminal background color
    backgroundColor: 'rgba(2, 40, 50, 1)',

    // border color (window, tabs)
    borderColor: 'rgba(255,255,255,.1)',

    windowSize: [740, 450],

    // custom css to embed in the main window
    css: `
      .header_header {
        background: transparent!important;
      }
      .tab_tab {
        border: 0;
      }
      .tab_active::before {
        border-bottom: 2px solid rgba(255,255,255,.5);
      }
      .tab_hasActivity {
        color: #42a1e4;
      }
      .tab_hasActivity:hover {
        color: #96d4e4;
      }
    `,

    // custom padding (css format, i.e.: `top right bottom left`)
    termCSS: '',

    // custom padding
    padding: '0px 5px',

    // some color overrides. see http://bit.ly/29k1iU2 for
    // the full list
    colors: [
      '#002834',
      '#dc322f',
      '#859901',
      '#b58901',
      '#268bd2',
      '#d33682',
      '#2aa198',
      '#eee8d5',
      '#839496',
      '#cb4b16',
      '#3d713a',
      '#83773b',
      '#839496',
      '#839496',
      '#93a1a1',
      '#93a1a1'
    ]
  },

  // a list of plugins to fetch and install from npm
  // format: [@org/]project[#version]
  // examples:
  //   `hyperpower`
  //   `@company/project`
  //   `project#1.0.1`
  plugins: [
  ],

  // in development, you can create a directory under
  // `~/.hyperterm_plugins/local/` and include it here
  // to load it and avoid it being `npm install`ed
  localPlugins: []
}
