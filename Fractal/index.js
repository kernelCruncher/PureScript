// Generated by purs bundle 0.14.1
var PS = {};
(function(exports) {
  "use strict";

  exports.arrayApply = function (fs) {
    return function (xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l*k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  "use strict";

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Function"] = $PS["Data.Function"] || {};
  var exports = $PS["Data.Function"];
  var flip = function (f) {
      return function (b) {
          return function (a) {
              return f(a)(b);
          };
      };
  };
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["flip"] = flip;
  exports["const"] = $$const;
})(PS);
(function(exports) {
  "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Unit"] = $PS["Data.Unit"] || {};
  var exports = $PS["Data.Unit"];
  var $foreign = $PS["Data.Unit"];
  exports["unit"] = $foreign.unit;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];
  var $foreign = $PS["Data.Functor"];
  var Data_Function = $PS["Data.Function"];
  var Data_Unit = $PS["Data.Unit"];                  
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };                                                                                             
  var functorArray = new Functor($foreign.arrayMap);
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
  exports["functorArray"] = functorArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Control.Apply"] = $PS["Control.Apply"] || {};
  var exports = $PS["Control.Apply"];
  var $foreign = $PS["Control.Apply"];
  var Data_Functor = $PS["Data.Functor"];            
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  }; 
  var applyArray = new Apply(function () {
      return Data_Functor.functorArray;
  }, $foreign.arrayApply);
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
  exports["applyArray"] = applyArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Control.Applicative"] = $PS["Control.Applicative"] || {};
  var exports = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];          
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS);
(function(exports) {
  "use strict";

  exports.arrayBind = function (arr) {
    return function (f) {
      var result = [];
      for (var i = 0, l = arr.length; i < l; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }
      return result;
    };
  };
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];
  var $foreign = $PS["Control.Bind"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Function = $PS["Data.Function"];          
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  }; 
  var bindArray = new Bind(function () {
      return Control_Apply.applyArray;
  }, $foreign.arrayBind);
  var bind = function (dict) {
      return dict.bind;
  };
  var bindFlipped = function (dictBind) {
      return Data_Function.flip(bind(dictBind));
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
  exports["bindFlipped"] = bindFlipped;
  exports["bindArray"] = bindArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Control.Monad"] = $PS["Control.Monad"] || {};
  var exports = $PS["Control.Monad"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];                
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (f$prime) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (a$prime) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(f$prime(a$prime));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS);
(function(exports) {
  "use strict";

  //------------------------------------------------------------------------------
  // Non-indexed reads -----------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.unconsImpl = function (empty) {
    return function (next) {
      return function (xs) {
        return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
      };
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Array"] = $PS["Data.Array"] || {};
  var exports = $PS["Data.Array"];
  var $foreign = $PS["Data.Array"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Function = $PS["Data.Function"];
  var foldM = function (dictMonad) {
      return function (f) {
          return function (b) {
              return $foreign.unconsImpl(function (v) {
                  return Control_Applicative.pure(dictMonad.Applicative0())(b);
              })(function (a) {
                  return function (as) {
                      return Control_Bind.bind(dictMonad.Bind1())(f(b)(a))(function (b$prime) {
                          return foldM(dictMonad)(f)(b$prime)(as);
                      });
                  };
              });
          };
      };
  };
  var concatMap = Data_Function.flip(Control_Bind.bind(Control_Bind.bindArray));
  exports["concatMap"] = concatMap;
  exports["foldM"] = foldM;
})(PS);
(function(exports) {
  "use strict";

  exports.intSub = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x - y | 0;
    };
  };
})(PS["Data.Ring"] = PS["Data.Ring"] || {});
(function(exports) {
  "use strict";

  exports.intAdd = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x + y | 0;
    };
  };

  exports.intMul = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x * y | 0;
    };
  };
})(PS["Data.Semiring"] = PS["Data.Semiring"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Semiring"] = $PS["Data.Semiring"] || {};
  var exports = $PS["Data.Semiring"];
  var $foreign = $PS["Data.Semiring"];
  var Semiring = function (add, mul, one, zero) {
      this.add = add;
      this.mul = mul;
      this.one = one;
      this.zero = zero;
  };                                                                            
  var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
  exports["semiringInt"] = semiringInt;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Ring"] = $PS["Data.Ring"] || {};
  var exports = $PS["Data.Ring"];
  var $foreign = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Ring = function (Semiring0, sub) {
      this.Semiring0 = Semiring0;
      this.sub = sub;
  };                  
  var ringInt = new Ring(function () {
      return Data_Semiring.semiringInt;
  }, $foreign.intSub);
  exports["ringInt"] = ringInt;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.CommutativeRing"] = $PS["Data.CommutativeRing"] || {};
  var exports = $PS["Data.CommutativeRing"];
  var Data_Ring = $PS["Data.Ring"];
  var CommutativeRing = function (Ring0) {
      this.Ring0 = Ring0;
  }; 
  var commutativeRingInt = new CommutativeRing(function () {
      return Data_Ring.ringInt;
  });
  exports["commutativeRingInt"] = commutativeRingInt;
})(PS);
(function(exports) {
  "use strict";

  exports.intDegree = function (x) {
    return Math.min(Math.abs(x), 2147483647);
  };

  // See the Euclidean definition in
  // https://en.m.wikipedia.org/wiki/Modulo_operation.
  exports.intDiv = function (x) {
    return function (y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };

  exports.intMod = function (x) {
    return function (y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return ((x % yy) + yy) % yy;
    };
  };
})(PS["Data.EuclideanRing"] = PS["Data.EuclideanRing"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.EuclideanRing"] = $PS["Data.EuclideanRing"] || {};
  var exports = $PS["Data.EuclideanRing"];
  var $foreign = $PS["Data.EuclideanRing"];
  var Data_CommutativeRing = $PS["Data.CommutativeRing"];  
  var EuclideanRing = function (CommutativeRing0, degree, div, mod) {
      this.CommutativeRing0 = CommutativeRing0;
      this.degree = degree;
      this.div = div;
      this.mod = mod;
  };
  var mod = function (dict) {
      return dict.mod;
  }; 
  var euclideanRingInt = new EuclideanRing(function () {
      return Data_CommutativeRing.commutativeRingInt;
  }, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);
  exports["mod"] = mod;
  exports["euclideanRingInt"] = euclideanRingInt;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];                 
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
})(PS);
(function(exports) {
  /* eslint-disable no-eq-null, eqeqeq */

  "use strict";          

  exports.nullable = function (a, r, f) {
    return a == null ? r : f(a);
  };
})(PS["Data.Nullable"] = PS["Data.Nullable"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Nullable"] = $PS["Data.Nullable"] || {};
  var exports = $PS["Data.Nullable"];
  var $foreign = $PS["Data.Nullable"];
  var Data_Maybe = $PS["Data.Maybe"];                                   
  var toMaybe = function (n) {
      return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
  };
  exports["toMaybe"] = toMaybe;
})(PS);
(function(exports) {
  "use strict";

  exports.showStringImpl = function (s) {
    var l = s.length;
    return "\"" + s.replace(
      /[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
      function (c, i) {
        switch (c) {
          case "\"":
          case "\\":
            return "\\" + c;
          case "\x07": return "\\a";
          case "\b": return "\\b";
          case "\f": return "\\f";
          case "\n": return "\\n";
          case "\r": return "\\r";
          case "\t": return "\\t";
          case "\v": return "\\v";
        }
        var k = i + 1;
        var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty;
      }
    ) + "\"";
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Data.Show"] = $PS["Data.Show"] || {};
  var exports = $PS["Data.Show"];
  var $foreign = $PS["Data.Show"];
  var Show = function (show) {
      this.show = show;
  };
  var showString = new Show($foreign.showStringImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["show"] = show;
  exports["showString"] = showString;
})(PS);
(function(exports) {
  "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Effect"] = $PS["Effect"] || {};
  var exports = $PS["Effect"];
  var $foreign = $PS["Effect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];                    
  var monadEffect = new Control_Monad.Monad(function () {
      return applicativeEffect;
  }, function () {
      return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
      return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
      return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
      return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
  exports["monadEffect"] = monadEffect;
})(PS);
(function(exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  var Data_Show = $PS["Data.Show"];
  var logShow = function (dictShow) {
      return function (a) {
          return $foreign.log(Data_Show.show(dictShow)(a));
      };
  };
  exports["logShow"] = logShow;
})(PS);
(function(exports) {
  "use strict";

  exports.new = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.modifyImpl = function (f) {
    return function (ref) {
      return function () {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Effect.Ref"] = $PS["Effect.Ref"] || {};
  var exports = $PS["Effect.Ref"];
  var $foreign = $PS["Effect.Ref"];          
  var modify$prime = $foreign.modifyImpl;
  var modify = function (f) {
      return modify$prime(function (s) {
          var s$prime = f(s);
          return {
              state: s$prime,
              value: s$prime
          };
      });
  };
  exports["modify"] = modify;
  exports["new"] = $foreign["new"];
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Fractal"] = $PS["Fractal"] || {};
  var exports = $PS["Fractal"];
  var Data_Array = $PS["Data.Array"];                
  var L = (function () {
      function L() {

      };
      L.value = new L();
      return L;
  })();
  var R = (function () {
      function R() {

      };
      R.value = new R();
      return R;
  })();
  var F = (function () {
      function F() {

      };
      F.value = new F();
      return F;
  })();
  var productions = function (v) {
      if (v instanceof L) {
          return [ L.value ];
      };
      if (v instanceof R) {
          return [ R.value ];
      };
      if (v instanceof F) {
          return [ F.value, L.value, F.value, R.value, R.value, F.value, L.value, F.value ];
      };
      throw new Error("Failed pattern match at Fractal (line 38, column 1 - line 38, column 34): " + [ v.constructor.name ]);
  };
  var lsystem = function (dictMonad) {
      return function (init) {
          return function (prod) {
              return function (interpret) {
                  return function (n) {
                      return function (state) {
                          var go = function ($copy_s) {
                              return function ($copy_v) {
                                  var $tco_var_s = $copy_s;
                                  var $tco_done = false;
                                  var $tco_result;
                                  function $tco_loop(s, v) {
                                      if (v === 0) {
                                          $tco_done = true;
                                          return Data_Array.foldM(dictMonad)(interpret)(state)(s);
                                      };
                                      $tco_var_s = Data_Array.concatMap(prod)(s);
                                      $copy_v = v - 1 | 0;
                                      return;
                                  };
                                  while (!$tco_done) {
                                      $tco_result = $tco_loop($tco_var_s, $copy_v);
                                  };
                                  return $tco_result;
                              };
                          };
                          return go(init)(n);
                      };
                  };
              };
          };
      };
  };
  var initialState = {
      x: 120.0,
      y: 200.0,
      theta: 0.0
  };
  var initial = [ F.value, R.value, R.value, F.value, R.value, R.value, F.value, R.value, R.value ];
  exports["lsystem"] = lsystem;
  exports["L"] = L;
  exports["R"] = R;
  exports["F"] = F;
  exports["initial"] = initial;
  exports["productions"] = productions;
  exports["initialState"] = initialState;
})(PS);
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.getCanvasWidth = function(canvas) {
      return function() {
          return canvas.width;
      };
  };

  exports.getCanvasHeight = function(canvas) {
      return function() {
          return canvas.height;
      };
  };

  exports.setFillStyle = function(ctx) {
      return function(style) {
          return function() {
              ctx.fillStyle = style;
          };
      };
  };

  exports.setStrokeStyle = function(ctx) {
      return function(style) {
          return function() {
              ctx.strokeStyle = style;
          };
      };
  };

  exports.setShadowColor = function(ctx) {
      return function(color) {
          return function() {
              ctx.shadowColor = color;
          };
      };
  };

  exports.setShadowBlur = function(ctx) {
      return function(blur) {
          return function() {
              ctx.shadowBlur = blur;
          };
      };
  };

  exports.setShadowOffsetX = function(ctx) {
      return function(offsetX) {
          return function() {
              ctx.shadowOffsetX = offsetX;
          };
      };
  };

  exports.setShadowOffsetY = function(ctx) {
      return function(offsetY) {
          return function() {
              ctx.shadowOffsetY = offsetY;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
      };
  };

  exports.stroke = function(ctx) {
      return function() {
          ctx.stroke();
      };
  };

  exports.fill = function(ctx) {
      return function() {
          ctx.fill();
      };
  };

  exports.lineTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.lineTo(x, y);
              };
          };
      };
  };

  exports.moveTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.moveTo(x, y);
              };
          };
      };
  };

  exports.closePath = function(ctx) {
      return function() {
          ctx.closePath();
      };
  };

  exports.clearRect = function(ctx) {
      return function(r) {
          return function() {
              ctx.clearRect(r.x, r.y, r.width, r.height);
          };
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Graphics.Canvas"] = $PS["Graphics.Canvas"] || {};
  var exports = $PS["Graphics.Canvas"];
  var $foreign = $PS["Graphics.Canvas"];
  var Data_Maybe = $PS["Data.Maybe"];
  var strokePath = function (ctx) {
      return function (path) {
          return function __do() {
              $foreign.beginPath(ctx)();
              var a = path();
              $foreign.stroke(ctx)();
              return a;
          };
      };
  };
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  var fillPath = function (ctx) {
      return function (path) {
          return function __do() {
              $foreign.beginPath(ctx)();
              var a = path();
              $foreign.fill(ctx)();
              return a;
          };
      };
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["strokePath"] = strokePath;
  exports["fillPath"] = fillPath;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["getCanvasWidth"] = $foreign.getCanvasWidth;
  exports["getCanvasHeight"] = $foreign.getCanvasHeight;
  exports["setFillStyle"] = $foreign.setFillStyle;
  exports["setStrokeStyle"] = $foreign.setStrokeStyle;
  exports["setShadowBlur"] = $foreign.setShadowBlur;
  exports["setShadowOffsetX"] = $foreign.setShadowOffsetX;
  exports["setShadowOffsetY"] = $foreign.setShadowOffsetY;
  exports["setShadowColor"] = $foreign.setShadowColor;
  exports["lineTo"] = $foreign.lineTo;
  exports["moveTo"] = $foreign.moveTo;
  exports["closePath"] = $foreign.closePath;
  exports["clearRect"] = $foreign.clearRect;
})(PS);
(function(exports) {
  "use strict";            

  exports.cos = Math.cos;    

  function nativeImul(a) {
    return function (b) {
      return Math.imul(a, b);
    };
  }

  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
  function emulatedImul(a) {
    /*jshint bitwise: false*/
    return function (b) {
      var ah = a >>> 16 & 0xffff;
      var al = a & 0xffff;
      var bh = b >>> 16 & 0xffff;
      var bl = b & 0xffff;
      // the shift by 0 fixes the sign on the high part
      // the final |0 converts the unsigned value into a signed value
      return al * bl + (ah * bl + al * bh << 16 >>> 0) | 0;
    };
  }                          

  exports.sin = Math.sin;

  exports.tau = 2 * Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Math"] = $PS["Math"] || {};
  var exports = $PS["Math"];
  var $foreign = $PS["Math"];
  exports["cos"] = $foreign.cos;
  exports["sin"] = $foreign.sin;
  exports["tau"] = $foreign.tau;
})(PS);
(function(exports) {
  "use strict";

  // module Unsafe.Coerce

  exports.unsafeCoerce = function (x) {
    return x;
  };
})(PS["Unsafe.Coerce"] = PS["Unsafe.Coerce"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Unsafe.Coerce"] = $PS["Unsafe.Coerce"] || {};
  var exports = $PS["Unsafe.Coerce"];
  var $foreign = $PS["Unsafe.Coerce"];
  exports["unsafeCoerce"] = $foreign.unsafeCoerce;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Web.DOM.Document"] = $PS["Web.DOM.Document"] || {};
  var exports = $PS["Web.DOM.Document"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];                      
  var toParentNode = Unsafe_Coerce.unsafeCoerce;
  exports["toParentNode"] = toParentNode;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Web.DOM.Element"] = $PS["Web.DOM.Element"] || {};
  var exports = $PS["Web.DOM.Element"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toEventTarget = Unsafe_Coerce.unsafeCoerce;
  exports["toEventTarget"] = toEventTarget;
})(PS);
(function(exports) {
  "use strict";                                               

  exports._querySelector = function (selector) {
    return function (node) {
      return function () {
        return node.querySelector(selector);
      };
    };
  };
})(PS["Web.DOM.ParentNode"] = PS["Web.DOM.ParentNode"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Web.DOM.ParentNode"] = $PS["Web.DOM.ParentNode"] || {};
  var exports = $PS["Web.DOM.ParentNode"];
  var $foreign = $PS["Web.DOM.ParentNode"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];
  var querySelector = function (qs) {
      var $0 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
      var $1 = $foreign["_querySelector"](qs);
      return function ($2) {
          return $0($1($2));
      };
  };
  exports["querySelector"] = querySelector;
})(PS);
(function(exports) {
  "use strict";

  exports.eventListener = function (fn) {
    return function () {
      return function (event) {
        return fn(event)();
      };
    };
  };

  exports.addEventListener = function (type) {
    return function (listener) {
      return function (useCapture) {
        return function (target) {
          return function () {
            return target.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  };
})(PS["Web.Event.EventTarget"] = PS["Web.Event.EventTarget"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Web.Event.EventTarget"] = $PS["Web.Event.EventTarget"] || {};
  var exports = $PS["Web.Event.EventTarget"];
  var $foreign = $PS["Web.Event.EventTarget"];
  exports["eventListener"] = $foreign.eventListener;
  exports["addEventListener"] = $foreign.addEventListener;
})(PS);
(function(exports) {
  "use strict";

  exports.window = function () {
    return window;
  };
})(PS["Web.HTML"] = PS["Web.HTML"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Web.HTML"] = $PS["Web.HTML"] || {};
  var exports = $PS["Web.HTML"];
  var $foreign = $PS["Web.HTML"];
  exports["window"] = $foreign.window;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Web.HTML.HTMLDocument"] = $PS["Web.HTML.HTMLDocument"] || {};
  var exports = $PS["Web.HTML.HTMLDocument"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];      
  var toDocument = Unsafe_Coerce.unsafeCoerce;
  exports["toDocument"] = toDocument;
})(PS);
(function(exports) {
  "use strict";

  exports.document = function (window) {
    return function () {
      return window.document;
    };
  };
})(PS["Web.HTML.Window"] = PS["Web.HTML.Window"] || {});
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Web.HTML.Window"] = $PS["Web.HTML.Window"] || {};
  var exports = $PS["Web.HTML.Window"];
  var $foreign = $PS["Web.HTML.Window"];
  exports["document"] = $foreign.document;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.1
  "use strict";
  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Show = $PS["Data.Show"];
  var Effect = $PS["Effect"];
  var Effect_Console = $PS["Effect.Console"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Fractal = $PS["Fractal"];
  var Graphics_Canvas = $PS["Graphics.Canvas"];
  var $$Math = $PS["Math"];
  var Web_DOM_Document = $PS["Web.DOM.Document"];
  var Web_DOM_Element = $PS["Web.DOM.Element"];
  var Web_DOM_ParentNode = $PS["Web.DOM.ParentNode"];
  var Web_Event_EventTarget = $PS["Web.Event.EventTarget"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_HTMLDocument = $PS["Web.HTML.HTMLDocument"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];                
  var main = Data_Functor["void"](Effect.functorEffect)(function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      if (v instanceof Data_Maybe.Just) {
          var ctx = Graphics_Canvas.getContext2D(v.value0)();
          var width = Graphics_Canvas.getCanvasWidth(v.value0)();
          var height = Graphics_Canvas.getCanvasHeight(v.value0)();
          var clickCount = Effect_Ref["new"](0)();
          var doc = Data_Functor.map(Effect.functorEffect)(function ($19) {
              return Web_DOM_Document.toParentNode(Web_HTML_HTMLDocument.toDocument($19));
          })(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.document)(Web_HTML.window))();
          var v1 = Web_DOM_ParentNode.querySelector("#canvas")(doc)();
          if (v1 instanceof Data_Maybe.Just) {
              Graphics_Canvas.setStrokeStyle(ctx)("#000")();
              Graphics_Canvas.setFillStyle(ctx)("#F00")();
              var interpret = function (state) {
                  return function (v2) {
                      if (v2 instanceof Fractal.L) {
                          return Control_Applicative.pure(Effect.applicativeEffect)({
                              theta: state.theta - $$Math.tau / 6.0,
                              x: state.x,
                              y: state.y
                          });
                      };
                      if (v2 instanceof Fractal.R) {
                          return Control_Applicative.pure(Effect.applicativeEffect)({
                              theta: state.theta + $$Math.tau / 6.0,
                              x: state.x,
                              y: state.y
                          });
                      };
                      if (v2 instanceof Fractal.F) {
                          var y = state.y + $$Math.sin(state.theta) * 1.5;
                          var x = state.x + $$Math.cos(state.theta) * 1.5;
                          return function __do() {
                              Graphics_Canvas.lineTo(ctx)(x)(y)();
                              return {
                                  x: x,
                                  y: y,
                                  theta: state.theta
                              };
                          };
                      };
                      throw new Error("Failed pattern match at Main (line 38, column 5 - line 38, column 49): " + [ state.constructor.name, v2.constructor.name ]);
                  };
              };
              var clickListener = Web_Event_EventTarget.eventListener(function (v2) {
                  return function __do() {
                      Effect_Console.logShow(Data_Show.showString)("Mouse clicked!")();
                      var count = Effect_Ref.modify(function (count) {
                          return Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(count + 1 | 0)(6);
                      })(clickCount)();
                      Graphics_Canvas.clearRect(ctx)({
                          x: 0.0,
                          y: 0.0,
                          width: width,
                          height: height
                      })();
                      Graphics_Canvas.fillPath(ctx)(Graphics_Canvas.strokePath(ctx)(function __do() {
                          Graphics_Canvas.moveTo(ctx)(Fractal.initialState.x)(Fractal.initialState.y)();
                          Fractal.lsystem(Effect.monadEffect)(Fractal.initial)(Fractal.productions)(interpret)(count)(Fractal.initialState)();
                          return Graphics_Canvas.closePath(ctx)();
                      }))();
                      Graphics_Canvas.setShadowOffsetX(ctx)(100.0)();
                      Graphics_Canvas.setShadowOffsetY(ctx)(50.0)();
                      Graphics_Canvas.setShadowBlur(ctx)(25.0)();
                      return Graphics_Canvas.setShadowColor(ctx)("#fa0")();
                  };
              })();
              return Web_Event_EventTarget.addEventListener("click")(clickListener)(true)(Web_DOM_Element.toEventTarget(v1.value0))();
          };
          throw new Error("Failed pattern match at Main (line 32, column 3 - line 32, column 59): " + [ v1.constructor.name ]);
      };
      throw new Error("Failed pattern match at Main (line 24, column 3 - line 24, column 47): " + [ v.constructor.name ]);
  });
  exports["main"] = main;
})(PS);
PS["Main"].main();