// require('./styles/reset.css');
// require('materialize-css/sass/materialize.scss')
require('bootstrap/scss/bootstrap.scss')
require('./assets/css/sticky-footer.css')
require('./assets/css/logo.css')
require('./index.html')

var Elm = require('./Main.elm')
var mountNode = document.getElementById('root')

var app = Elm.Main.embed(mountNode)
