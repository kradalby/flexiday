// require('./styles/reset.css');
// require('materialize-css/sass/materialize.scss')
require('bootstrap/scss/bootstrap.scss')
require('./css/sticky-footer.css')
require('./css/logo.css')
require('./index.html')

var Elm = require('./Main.elm')
var mountNode = document.getElementById('root')

var app = Elm.Main.embed(mountNode)
