// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Seq$TypeAligned = require("./Seq.bs.js");

function Make(F) {
  var singleton = function (x) {
    return /* CCons */[/* tuple */[
              x,
              /* CNil */0
            ]];
  };
  var cons = function (x, xs) {
    return /* CCons */[/* tuple */[
              x,
              xs
            ]];
  };
  var viewLeft = function (param) {
    if (param) {
      var match = param[0];
      return /* ConsL */[/* tuple */[
                match[0],
                match[1]
              ]];
    } else {
      return /* EmptyL */0;
    }
  };
  var consOrSnoc = /* `Cons */[
    748545553,
    cons
  ];
  var view = /* `Left */[
    847852583,
    viewLeft
  ];
  return Seq$TypeAligned.Make({
              F: F,
              empty: /* CNil */0,
              singleton: singleton,
              consOrSnoc: consOrSnoc,
              view: view
            });
}

exports.Make = Make;
/* No side effect */
