"use strict";

var Tg = require('../Test.Gcd');
var Ts = require('../Test.Shout');

exports.runGcd = function(n) {
  return function(m) {
    return Tg.gcd(n)(m);
  };
};

exports.runShout = Ts.shout(require('../Data.Show').showNumber)(42);