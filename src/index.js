// require('./styles/reset.css');
// require('materialize-css/sass/materialize.scss')
// require("bootstrap/scss/bootstrap.scss");
// require("./assets/css/sticky-footer.css");
// require("./assets/css/logo.css");

import Elm from "./elm.js";
let mountNode = document.getElementById("root");

Elm.Main.embed(mountNode);
