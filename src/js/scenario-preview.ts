import { Elm } from "../Scenario.elm";
import hljs from "highlight.js/lib/core";
import json from "highlight.js/lib/languages/json";
import "highlight.js/styles/github-dark.css";
declare global {
  interface Window {
    hljs: object;
  }
}

hljs.registerLanguage("json", json);
window.hljs = hljs;

Elm.Scenario.init({
  node: document.body.appendChild(document.createElement("div")),
});
