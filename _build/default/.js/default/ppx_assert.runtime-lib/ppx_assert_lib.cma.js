// Generated by js_of_ocaml
//# buildInfo:effects=false, kind=cma, use-js-string=true, version=5.6.0

//# unitInfo: Provides: Ppx_assert_lib
(function
  (globalThis){
   "use strict";
   var runtime = globalThis.jsoo_runtime, Ppx_assert_lib = [0];
   runtime.caml_register_global(0, Ppx_assert_lib, "Ppx_assert_lib");
   return;
  }
  (globalThis));

//# unitInfo: Provides: Ppx_assert_lib__Runtime
//# unitInfo: Requires: Assert_failure, Base, Base__Source_code_position, Sexplib0__Sexp_conv
(function
  (globalThis){
   "use strict";
   var
    runtime = globalThis.jsoo_runtime,
    caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace;
   function caml_call1(f, a0){
    return (f.l >= 0 ? f.l : f.l = f.length) == 1
            ? f(a0)
            : runtime.caml_call_gen(f, [a0]);
   }
   function caml_call2(f, a0, a1){
    return (f.l >= 0 ? f.l : f.l = f.length) == 2
            ? f(a0, a1)
            : runtime.caml_call_gen(f, [a0, a1]);
   }
   function caml_call4(f, a0, a1, a2, a3){
    return (f.l >= 0 ? f.l : f.l = f.length) == 4
            ? f(a0, a1, a2, a3)
            : runtime.caml_call_gen(f, [a0, a1, a2, a3]);
   }
   var
    global_data = runtime.caml_get_global_data(),
    cst = ": ",
    Base = global_data.Base,
    Base_Source_code_position = global_data.Base__Source_code_position,
    Assert_failure = global_data.Assert_failure,
    Sexplib0_Sexp_conv = global_data.Sexplib0__Sexp_conv,
    E = [248, "Ppx_assert_lib.Runtime.E", runtime.caml_fresh_oo_id(0)],
    _h_ = [0, "got"],
    _i_ = [0, "expected"],
    cst_got_unexpected_result = "got unexpected result",
    _g_ = [0, "vs"],
    cst_comparison_failed = "comparison failed",
    _f_ = [0, "Value"],
    cst_predicate_failed = "predicate failed",
    _d_ = [0, "Stack"],
    _e_ = [0, "Loc"],
    _b_ = [0, "runtime-lib/runtime.ml.E"],
    _c_ = [0, "_none_", 0, -1];
   function _a_(param){
    if(param[1] !== E)
     throw caml_maybe_attach_backtrace([0, Assert_failure, _c_], 1);
    var
     arg1_002 = param[3],
     arg0_001 = param[2],
     res0_003 = caml_call1(Base[165], arg0_001),
     res1_004 = caml_call1(Base[85][4], arg1_002);
    return [1, [0, _b_, [0, res0_003, [0, res1_004, 0]]]];
   }
   caml_call4(Sexplib0_Sexp_conv[70][1], 0, 0, E, _a_);
   function exn_sexp_style(message, pos, x_005, tag, body){
    if(message)
     var
      s = message[1],
      _k_ = caml_call2(Base[198], cst, tag),
      message$0 = caml_call2(Base[198], s, _k_);
    else
     var message$0 = tag;
    var
     _l_ =
       x_005
        ? [0,
          [1,
           [0,
            _d_,
            [0, caml_call2(Base[140], Base_Source_code_position[3], x_005), 0]]],
          0]
        : 0,
     _m_ = caml_call2(Base[179], [0, [1, [0, _e_, [0, [0, pos], 0]]], 0], _l_),
     sexp = [1, caml_call2(Base[179], body, _m_)];
    return [0, E, message$0, sexp];
   }
   function test_pred(pos, sexpifier, here, message, predicate, t){
    var _j_ = 1 - caml_call1(predicate, t);
    if(_j_)
     throw caml_maybe_attach_backtrace
            (exn_sexp_style
              (message,
               pos,
               here,
               cst_predicate_failed,
               [0, [1, [0, _f_, [0, caml_call1(sexpifier, t), 0]]], 0]),
             1);
    return _j_;
   }
   var r_diff = [0, 0];
   function set_diff_function(f){r_diff[1] = f; return 0;}
   function test_result_or_eq(sexpifier, comparator, equal, expect$0, got$0){
    if(equal)
     var f = equal[1], pass = caml_call2(f, got$0, expect$0);
    else
     var pass = 0 === caml_call2(comparator, got$0, expect$0) ? 1 : 0;
    if(pass) return 892014833;
    var
     got = caml_call1(sexpifier, got$0),
     expect = caml_call1(sexpifier, expect$0),
     match = r_diff[1];
    if(match){
     var
      diff = match[1],
      from = caml_call2(Base[85][13], 0, expect),
      to = caml_call2(Base[85][13], 0, got);
     caml_call2(diff, from, to);
    }
    return [0, 781116926, [0, expect, got]];
   }
   function test_eq(pos, sexpifier, comparator, here, message, equal, t1, t2){
    var match = test_result_or_eq(sexpifier, comparator, equal, t1, t2);
    if(typeof match === "number") return 0;
    var match$0 = match[2], t2$0 = match$0[2], t1$0 = match$0[1];
    throw caml_maybe_attach_backtrace
           (exn_sexp_style
             (message,
              pos,
              here,
              cst_comparison_failed,
              [0, t1$0, [0, _g_, [0, t2$0, 0]]]),
            1);
   }
   function test_result
   (pos, sexpifier, comparator, here, message, equal, expect, got){
    var match = test_result_or_eq(sexpifier, comparator, equal, expect, got);
    if(typeof match === "number") return 0;
    var match$0 = match[2], got$0 = match$0[2], expect$0 = match$0[1];
    throw caml_maybe_attach_backtrace
           (exn_sexp_style
             (message,
              pos,
              here,
              cst_got_unexpected_result,
              [0,
               [1, [0, _i_, [0, expect$0, 0]]],
               [0, [1, [0, _h_, [0, got$0, 0]]], 0]]),
            1);
   }
   var
    Ppx_assert_lib_Runtime =
      [0, test_pred, test_eq, test_result, set_diff_function];
   runtime.caml_register_global
    (17, Ppx_assert_lib_Runtime, "Ppx_assert_lib__Runtime");
   return;
  }
  (globalThis));

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLjAsImZpbGUiOiJwcHhfYXNzZXJ0X2xpYi5jbWEuanMiLCJzb3VyY2VSb290IjoiIiwibmFtZXMiOlsiZXhuX3NleHBfc3R5bGUiLCJtZXNzYWdlIiwicG9zIiwieF8wMDUiLCJ0YWciLCJib2R5IiwicyIsIm1lc3NhZ2UkMCIsInNleHAiLCJ0ZXN0X3ByZWQiLCJzZXhwaWZpZXIiLCJoZXJlIiwicHJlZGljYXRlIiwidCIsInJfZGlmZiIsInNldF9kaWZmX2Z1bmN0aW9uIiwiZiIsInRlc3RfcmVzdWx0X29yX2VxIiwiY29tcGFyYXRvciIsImVxdWFsIiwiZXhwZWN0JDAiLCJnb3QkMCIsInBhc3MiLCJnb3QiLCJleHBlY3QiLCJkaWZmIiwiZnJvbSIsInRvIiwidGVzdF9lcSIsInQxIiwidDIiLCJ0MiQwIiwidDEkMCIsInRlc3RfcmVzdWx0Il0sInNvdXJjZXMiOlsiL1VzZXJzL3N2aXNobnVzLy5vcGFtL2dydi10ZXN0L2xpYi9wcHhfYXNzZXJ0L3J1bnRpbWUtbGliL3J1bnRpbWUubWwiXSwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7RTs7Ozs7Ozs7Ozs7Rzs7Ozs7Rzs7Ozs7Rzs7Ozs7R0F5QkE7Ozs7Ozs7SUFBQTs7Ozs7Ozs7Ozs7OztJOzs7Ozs7Ozs7O1lBRUlBLGVBQWdCQyxTQUFTQyxLQUFLQyxPQUFNQyxLQUFJQztJQUMxQyxHQURrQko7S0FJQTtNQUFUSyxJQUpTTDtNQUlBLE1BQUEsMkJBSm9CRztNQUNsQ0csWUFHVSxzQkFBTEQ7O1NBSExDLFlBRGtDSDtJQU10QztLQUFBO09BTmdDRDs7Ozs7b0VBQUFBOzs7S0FTMUIsTUFBQSwrQ0FUcUJEO0tBTXZCTSxXQUNRLHNCQVA4Qkg7SUFxQjFDLGNBcEJJRSxXQUtBQztHQWVhO1lBT2ZDLFVBQVdQLEtBQUtRLFdBQVdDLE1BQU1WLFNBQVFXLFdBQVVDO0lBQzlDLGNBQUEsV0FEb0NELFdBQVVDOztLQUU3QyxNQUFBO2FBOUJOYjtlQTRCaUNDO2VBQXRCQztlQUFnQlM7O29DQUhHLFdBR2RELFdBQW1DRzs7SUFDOUM7R0FDaUQ7R0FFSyxJQUEzREM7WUFDQUMsa0JBQWtCQyxHQURsQkYsWUFDa0JFLFlBQWU7WUFjakNDLGtCQUFtQlAsV0FBV1EsWUFBWUMsT0FBT0MsVUFBUUM7SUFDM0QsR0FENENGO1NBSW5DSCxJQUptQ0csVUFDeENHLE9BR1UsV0FBTE4sR0FKa0RLLE9BQVJEOztTQUMvQ0UsYUFFUSxXQUhvQkosWUFBMkJHLE9BQVJEO0lBTW5ELEdBTElFLE1BTUM7SUFsQks7S0FBTkMsTUFBTSxXQVdXYixXQUFzQ1c7S0FWdkRHLFNBQVMsV0FVUWQsV0FBOEJVO0tBVG5ELFFBTkVOOztLQVNZO01BRFBXO01BQ0RDLE9BQVEsNEJBSlZGO01BS0VHLEtBQVEsNEJBTlZKO0tBT0YsV0FIS0UsTUFDREMsTUFDQUM7O0lBR04sMEJBUklILFFBREFEO0dBbUJpRDtZQVNuREssUUFBUzFCLEtBQUtRLFdBQVdRLFlBQVlQLE1BQU1WLFNBQVNrQixPQUFNVSxJQUFHQztJQUN6RCxZQWxCSmIsa0JBaUJjUCxXQUFXUSxZQUEyQkMsT0FBTVUsSUFBR0M7a0NBRXBEOzRCQVRvQ0MsbUJBQUpDO0lBVWpCLE1BQUE7WUFuRXhCaEM7Y0FnRTJDQztjQUFsQ0M7Y0FBNEJTOztrQkFQSXFCLG1CQUFJRDs7R0FVb0I7WUFRakRFO0lBQWEvQixLQUFLUSxXQUFXUSxZQUFZUCxNQUFNVixTQUFTa0IsT0FBT0ssUUFBUUQ7SUFDbkYsWUE3QkpOLGtCQTRCa0NQLFdBQVdRLFlBQTJCQyxPQUFPSyxRQUFRRDtrQ0FFOUU7NEJBUjRDRixvQkFBUkQ7SUFTaEIsTUFBQTtZQTlFN0JwQjtjQTJFK0RDO2NBQWxDQztjQUE0QlM7OztnQ0FOWlM7b0NBQVFDOztHQVMwQjs7O1VBbEQvRVosV0FvQ0FtQixTQVdnQkssYUExQ2hCbEI7Ozs7RSIsInNvdXJjZXNDb250ZW50IjpbIm9wZW4gQmFzZVxuXG50eXBlICdhIHRlc3RfcHJlZFxuICA9ID9oZXJlOkxleGluZy5wb3NpdGlvbiBsaXN0XG4gIC0+ID9tZXNzYWdlOnN0cmluZ1xuICAtPiAoJ2EgLT4gYm9vbClcbiAgLT4gJ2FcbiAgLT4gdW5pdFxuXG50eXBlICdhIHRlc3RfZXFcbiAgPSA/aGVyZTpMZXhpbmcucG9zaXRpb24gbGlzdFxuICAtPiA/bWVzc2FnZTpzdHJpbmdcbiAgLT4gP2VxdWFsOignYSAtPiAnYSAtPiBib29sKVxuICAtPiAnYVxuICAtPiAnYVxuICAtPiB1bml0XG5cbnR5cGUgJ2EgdGVzdF9yZXN1bHRcbiAgPSA/aGVyZTpMZXhpbmcucG9zaXRpb24gbGlzdFxuICAtPiA/bWVzc2FnZTpzdHJpbmdcbiAgLT4gP2VxdWFsOignYSAtPiAnYSAtPiBib29sKVxuICAtPiBleHBlY3Q6J2FcbiAgLT4gJ2FcbiAgLT4gdW5pdFxuXG5leGNlcHRpb24gRSBvZiBzdHJpbmcgKiBTZXhwLnQgW0BAZGVyaXZpbmcgc2V4cF1cblxubGV0IGV4bl9zZXhwX3N0eWxlIH5tZXNzYWdlIH5wb3MgfmhlcmUgfnRhZyBib2R5ID1cbiAgbGV0IG1lc3NhZ2UgPVxuICAgIG1hdGNoIG1lc3NhZ2Ugd2l0aFxuICAgIHwgTm9uZSAtPiB0YWdcbiAgICB8IFNvbWUgcyAtPiBzIF4gXCI6IFwiIF4gdGFnXG4gIGluXG4gIGxldCBzZXhwID1cbiAgICBTZXhwLkxpc3QgKFxuICAgICAgYm9keVxuICAgICAgQCBbIFNleHAuTGlzdCBbIFNleHAuQXRvbSBcIkxvY1wiOyBTZXhwLkF0b20gcG9zIF0gXVxuICAgICAgQCBiZWdpbiBtYXRjaCBoZXJlIHdpdGhcbiAgICAgICAgfCBbXSAtPiBbXVxuICAgICAgICB8IF8gLT4gWyBTZXhwLkxpc3QgWyBTZXhwLkF0b20gXCJTdGFja1wiXG4gICAgICAgICAgICAgICAgICAgICAgICAgICA7IFslc2V4cF9vZjogU291cmNlX2NvZGVfcG9zaXRpb24udCBsaXN0XSBoZXJlXG4gICAgICAgICAgICAgICAgICAgICAgICAgICBdIF1cbiAgICAgIGVuZFxuICAgIClcbiAgaW5cbiAgKCogSGVyZSBhbmQgaW4gb3RoZXIgcGxhY2VzIHdlIHJldHVybiBleGNlcHRpb25zLCByYXRoZXIgdGhhbiBkaXJlY3RseSByYWlzaW5nLCBhbmRcbiAgICAgaW5zdGVhZCByYWlzZSBhdCB0aGUgbGF0ZXN0IG1vbWVudCBwb3NzaWJsZSwgc28gYmFja3RyYWNlIGRvbid0IGluY2x1ZGUgbm9pc2UgZnJvbVxuICAgICB0aGVzZSBmdW5jdGlvbnMgdGhhdCBjb25zdHJ1Y3QgZXhjZXB0aW9ucy4gKilcbiAgRSAobWVzc2FnZSwgc2V4cClcblxubGV0IFtAY29sZF0gZXhuX3Rlc3RfcHJlZCB+bWVzc2FnZSB+cG9zIH5oZXJlIH5zZXhwaWZpZXIgdCA9XG4gIGV4bl9zZXhwX3N0eWxlIH5tZXNzYWdlIH5wb3MgfmhlcmUgfnRhZzpcInByZWRpY2F0ZSBmYWlsZWRcIiBbXG4gICAgU2V4cC5MaXN0IFtTZXhwLkF0b20gXCJWYWx1ZVwiOyBzZXhwaWZpZXIgdF1cbiAgXVxuXG5sZXQgdGVzdF9wcmVkIH5wb3MgfnNleHBpZmllciB+aGVyZSA/bWVzc2FnZSBwcmVkaWNhdGUgdCA9XG4gIGlmIG5vdCAocHJlZGljYXRlIHQpIHRoZW5cbiAgICByYWlzZSAoZXhuX3Rlc3RfcHJlZCB+bWVzc2FnZSB+cG9zIH5oZXJlIH5zZXhwaWZpZXIgdClcblxubGV0IHJfZGlmZiA6IChmcm9tXzpzdHJpbmcgLT4gdG9fOnN0cmluZyAtPiB1bml0KSBvcHRpb24gcmVmID0gcmVmICAgTm9uZVxubGV0IHNldF9kaWZmX2Z1bmN0aW9uIGYgPSByX2RpZmYgOj0gZlxuXG5sZXQgW0Bjb2xkXSB0ZXN0X3Jlc3VsdF9vcl9lcV9mYWlsZWQgfnNleHBpZmllciB+ZXhwZWN0IH5nb3QgPVxuICBsZXQgZ290ID0gc2V4cGlmaWVyIGdvdCBpblxuICBsZXQgZXhwZWN0ID0gc2V4cGlmaWVyIGV4cGVjdCBpblxuICBiZWdpbiBtYXRjaCAhcl9kaWZmIHdpdGhcbiAgfCBOb25lIC0+ICgpXG4gIHwgU29tZSBkaWZmIC0+XG4gICAgbGV0IGZyb21fID0gU2V4cC50b19zdHJpbmdfaHVtIGV4cGVjdCBpblxuICAgIGxldCB0b18gICA9IFNleHAudG9fc3RyaW5nX2h1bSBnb3QgaW5cbiAgICBkaWZmIH5mcm9tXyB+dG9fXG4gIGVuZDtcbiAgYEZhaWwgKGV4cGVjdCwgZ290KVxuXG5sZXQgdGVzdF9yZXN1bHRfb3JfZXEgfnNleHBpZmllciB+Y29tcGFyYXRvciB+ZXF1YWwgfmV4cGVjdCB+Z290ID1cbiAgbGV0IHBhc3MgPVxuICAgIG1hdGNoIGVxdWFsIHdpdGhcbiAgICB8IE5vbmUgLT4gY29tcGFyYXRvciBnb3QgZXhwZWN0ID0gMFxuICAgIHwgU29tZSBmIC0+IGYgZ290IGV4cGVjdFxuICBpblxuICBpZiBwYXNzXG4gIHRoZW4gYFBhc3NcbiAgZWxzZSB0ZXN0X3Jlc3VsdF9vcl9lcV9mYWlsZWQgfnNleHBpZmllciB+ZXhwZWN0IH5nb3RcblxubGV0IFtAY29sZF0gZXhuX3Rlc3RfZXEgfm1lc3NhZ2UgfnBvcyB+aGVyZSB+dDEgfnQyID1cbiAgZXhuX3NleHBfc3R5bGUgfm1lc3NhZ2UgfnBvcyB+aGVyZSB+dGFnOlwiY29tcGFyaXNvbiBmYWlsZWRcIiBbXG4gICAgdDE7XG4gICAgU2V4cC5BdG9tIFwidnNcIjtcbiAgICB0MjtcbiAgXVxuXG5sZXQgdGVzdF9lcSB+cG9zIH5zZXhwaWZpZXIgfmNvbXBhcmF0b3IgfmhlcmUgP21lc3NhZ2UgP2VxdWFsIHQxIHQyID1cbiAgbWF0Y2ggdGVzdF9yZXN1bHRfb3JfZXEgfnNleHBpZmllciB+Y29tcGFyYXRvciB+ZXF1YWwgfmV4cGVjdDp0MSB+Z290OnQyIHdpdGhcbiAgfCBgUGFzcyAtPiAoKVxuICB8IGBGYWlsICh0MSwgdDIpIC0+IHJhaXNlIChleG5fdGVzdF9lcSB+bWVzc2FnZSB+cG9zIH5oZXJlIH50MSB+dDIpXG5cbmxldCBbQGNvbGRdIGV4bl90ZXN0X3Jlc3VsdCB+bWVzc2FnZSB+cG9zIH5oZXJlIH5leHBlY3QgfmdvdCA9XG4gIGV4bl9zZXhwX3N0eWxlIH5tZXNzYWdlIH5wb3MgfmhlcmUgfnRhZzpcImdvdCB1bmV4cGVjdGVkIHJlc3VsdFwiIFtcbiAgICBTZXhwLkxpc3QgW1NleHAuQXRvbSBcImV4cGVjdGVkXCI7IGV4cGVjdF07XG4gICAgU2V4cC5MaXN0IFtTZXhwLkF0b20gXCJnb3RcIjsgZ290XTtcbiAgXVxuXG5sZXRbQHdhcm5pbmcgXCItMTZcIl0gdGVzdF9yZXN1bHQgfnBvcyB+c2V4cGlmaWVyIH5jb21wYXJhdG9yIH5oZXJlID9tZXNzYWdlID9lcXVhbCB+ZXhwZWN0IH5nb3QgPVxuICBtYXRjaCB0ZXN0X3Jlc3VsdF9vcl9lcSB+c2V4cGlmaWVyIH5jb21wYXJhdG9yIH5lcXVhbCB+ZXhwZWN0IH5nb3Qgd2l0aFxuICB8IGBQYXNzIC0+ICgpXG4gIHwgYEZhaWwgKGV4cGVjdCwgZ290KSAtPiByYWlzZSAoZXhuX3Rlc3RfcmVzdWx0IH5tZXNzYWdlIH5wb3MgfmhlcmUgfmV4cGVjdCB+Z290KVxuIl19