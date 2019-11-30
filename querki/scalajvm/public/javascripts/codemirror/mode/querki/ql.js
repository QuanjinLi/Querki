(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"), require("../meta"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "../meta"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineSimpleMode("ql", {
  start: [
    {regex: /[a-z$][\w$]*/, token: "variable"}
  ],
  meta: {
  }
});

CodeMirror.defineMIME("text/x-ql", "ql");

});
