// Generated by js_of_ocaml
//# buildInfo:effects=false, kind=cma, use-js-string=true, version=5.6.0

//# unitInfo: Provides: Balanced_reducer
//# unitInfo: Requires: Assert_failure, Base, Base__Int, Base__List, Base__Option_array, Expect_test_collector, Ppx_bench_lib__Benchmark_accumulator, Ppx_inline_test_lib__Runtime, Ppx_module_timer_runtime, Sexplib0__Sexp_conv
(function
  (globalThis){
   "use strict";
   var
    runtime = globalThis.jsoo_runtime,
    cst_Balanced_reducer$0 = "Balanced_reducer",
    cst_balanced_reducer$0 = "balanced_reducer",
    cst_balanced_reducer_src_balan =
      "balanced_reducer/src/balanced_reducer.ml",
    cst_index = "index",
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
   function caml_call3(f, a0, a1, a2){
    return (f.l >= 0 ? f.l : f.l = f.length) == 3
            ? f(a0, a1, a2)
            : runtime.caml_call_gen(f, [a0, a1, a2]);
   }
   var
    global_data = runtime.caml_get_global_data(),
    cst = "",
    Base_Option_array = global_data.Base__Option_array,
    Sexplib0_Sexp_conv = global_data.Sexplib0__Sexp_conv,
    Base = global_data.Base,
    Base_Int = global_data.Base__Int,
    Assert_failure = global_data.Assert_failure,
    Base_List = global_data.Base__List,
    Ppx_module_timer_runtime = global_data.Ppx_module_timer_runtime,
    Ppx_bench_lib_Benchmark_accumu =
      global_data.Ppx_bench_lib__Benchmark_accumulator,
    Expect_test_collector = global_data.Expect_test_collector,
    Ppx_inline_test_lib_Runtime = global_data.Ppx_inline_test_lib__Runtime;
   caml_call1(Ppx_module_timer_runtime[4], cst_Balanced_reducer$0);
   caml_call1(Ppx_bench_lib_Benchmark_accumu[1][1], cst_balanced_reducer$0);
   caml_call1(Expect_test_collector[5][1], cst_balanced_reducer_src_balan);
   caml_call2(Ppx_inline_test_lib_Runtime[2], cst_balanced_reducer$0, cst);
   var
    _i_ = [0, cst_balanced_reducer$0],
    cst_attempt_to_compute_balance =
      "attempt to compute balanced reducer with unset elements",
    _e_ = [0, cst_index],
    cst_attempt_to_access_negative =
      "attempt to access negative index in balanced reducer",
    _f_ = [0, "length"],
    _g_ = [0, cst_index],
    cst_attempt_to_access_out_of_b =
      "attempt to access out of bounds index in balanced reducer",
    _d_ = [0, "_"],
    _c_ = [0, "num_leaves"],
    cst_non_positive_number_of_lea =
      "non-positive number of leaves in balanced reducer",
    _a_ = [0, cst_balanced_reducer_src_balan, 76, 9],
    _b_ = [0, cst_balanced_reducer_src_balan, 78, 6],
    cst_balanced_reducer = cst_balanced_reducer$0,
    cst_Balanced_reducer = cst_Balanced_reducer$0;
   function left_child_index(parent_index){return (parent_index * 2 | 0) + 1 | 0;
   }
   function num_branches(t){return t[2] - 1 | 0;}
   function index_is_leaf(t, i){return num_branches(t) <= i ? 1 : 0;}
   function leaf_index(t, i){
    var
     offset_from_start_of_leaves_in = i + t[3] | 0,
     rotated_index =
       offset_from_start_of_leaves_in < t[2]
        ? offset_from_start_of_leaves_in
        : offset_from_start_of_leaves_in - t[2] | 0;
    return rotated_index + num_branches(t) | 0;
   }
   function sexp_of_t(sexp_of_a, t){
    function _C_(i){
     var _E_ = leaf_index(t, i);
     return caml_call2(Base_Option_array[33], t[1], _E_);
    }
    var
     x_001 = caml_call2(Base_List[123], t[2], _C_),
     _D_ = caml_call1(Base[153], sexp_of_a);
    return caml_call2(Base[140], _D_, x_001);
   }
   function invariant(invariant_a, t){
    var
     data = t[1],
     _x_ = caml_call1(Base_Option_array[6], data) - 1 | 0,
     _w_ = 0;
    if(_x_ >= 0){
     var i$0 = _w_;
     for(;;){
      var match = caml_call2(Base_Option_array[33], data, i$0);
      if(match){var a = match[1]; caml_call1(invariant_a, a);}
      var _B_ = i$0 + 1 | 0;
      if(_x_ === i$0) break;
      var i$0 = _B_;
     }
    }
    var _z_ = num_branches(t) - 1 | 0, _y_ = 0;
    if(_z_ >= 0){
     var i = _y_;
     for(;;){
      var
       left = left_child_index(i),
       right = left + 1 | 0,
       left_is_none = caml_call2(Base_Option_array[35], data, left),
       right_is_none = caml_call2(Base_Option_array[35], data, right);
      a:
      {
       if(caml_call2(Base_Option_array[36], data, i)){
        if(! left_is_none && ! right_is_none) break a;
        throw caml_maybe_attach_backtrace([0, Assert_failure, _a_], 1);
       }
       if
        (!
         index_is_leaf(t, left)
         && ! index_is_leaf(t, right) && ! left_is_none && ! right_is_none)
        throw caml_maybe_attach_backtrace([0, Assert_failure, _b_], 1);
      }
      var _A_ = i + 1 | 0;
      if(_z_ === i) break;
      var i = _A_;
     }
    }
    return 0;
   }
   function create_exn(opt, param, num_leaves, reduce){
    if(opt)
     var sth = opt[1], sexp_of_a = sth;
    else
     var sexp_of_a = function(param){return _d_;};
    if(num_leaves < 1){
     var
      _u_ = [0, [1, [0, _c_, [0, caml_call1(Base[120], num_leaves), 0]]], 0],
      _v_ =
        [1,
         [0,
          caml_call1(Sexplib0_Sexp_conv[7], cst_non_positive_number_of_lea),
          _u_]];
     caml_call1(Base[203], _v_);
    }
    var
     num_branches = num_leaves - 1 | 0,
     num_leaves_not_in_bottom_level =
       caml_call1(Base_Int[63], num_leaves) - num_leaves | 0,
     data = caml_call1(Base_Option_array[5], num_branches + num_leaves | 0);
    return [0,
            data,
            num_leaves,
            num_leaves_not_in_bottom_level,
            reduce,
            sexp_of_a];
   }
   function validate_index(t, i){
    if(i < 0){
     var
      _o_ = [0, [1, [0, _e_, [0, caml_call1(Base[120], i), 0]]], 0],
      _p_ =
        [1,
         [0,
          caml_call1(Sexplib0_Sexp_conv[7], cst_attempt_to_access_negative),
          _o_]];
     caml_call1(Base[203], _p_);
    }
    var length = t[2], _q_ = length <= i ? 1 : 0;
    if(! _q_) return _q_;
    var
     _r_ = [0, [1, [0, _f_, [0, caml_call1(Base[120], length), 0]]], 0],
     _s_ = [0, [1, [0, _g_, [0, caml_call1(Base[120], i), 0]]], _r_],
     _t_ =
       [1,
        [0,
         caml_call1(Sexplib0_Sexp_conv[7], cst_attempt_to_access_out_of_b),
         _s_]];
    return caml_call1(Base[203], _t_);
   }
   function set_exn(t, i, a){
    validate_index(t, i);
    var data = t[1], i$0 = [0, leaf_index(t, i)];
    caml_call3(Base_Option_array[42], data, i$0[1], a);
    for(;;){
     if(0 === i$0[1]) return 0;
     var child_index = i$0[1], parent = (child_index - 1 | 0) / 2 | 0;
     if(caml_call2(Base_Option_array[35], data, parent))
      i$0[1] = 0;
     else{caml_call2(Base_Option_array[50], data, parent); i$0[1] = parent;}
    }
   }
   function get_exn(t, i){
    validate_index(t, i);
    var _n_ = leaf_index(t, i);
    return caml_call2(Base_Option_array[34], t[1], _n_);
   }
   function _h_(x_002, i){
    if(caml_call2(Base_Option_array[36], x_002[1], i))
     return caml_call2(Base_Option_array[38], x_002[1], i);
    var left = left_child_index(i), right = left + 1 | 0;
    if(caml_call1(Base_Option_array[6], x_002[1]) <= left){
     var
      sexp_of_a = x_002[5],
      _j_ = [0, [1, [0, _i_, [0, sexp_of_t(sexp_of_a, x_002), 0]]], 0],
      _k_ =
        [1,
         [0,
          caml_call1(Sexplib0_Sexp_conv[7], cst_attempt_to_compute_balance),
          _j_]];
     caml_call1(Base[203], _k_);
    }
    var
     _l_ = _h_(x_002, right),
     _m_ = _h_(x_002, left),
     a = caml_call2(x_002[4], _m_, _l_);
    caml_call3(Base_Option_array[49], x_002[1], i, a);
    return a;
   }
   function compute_exn(t){return _h_(t, 0);}
   caml_call1(Ppx_inline_test_lib_Runtime[3], cst_balanced_reducer);
   caml_call1(Expect_test_collector[5][2], 0);
   caml_call1(Ppx_bench_lib_Benchmark_accumu[1][2], 0);
   caml_call1(Ppx_module_timer_runtime[5], cst_Balanced_reducer);
   var
    Balanced_reducer =
      [0, sexp_of_t, invariant, create_exn, set_exn, get_exn, compute_exn];
   runtime.caml_register_global(29, Balanced_reducer, cst_Balanced_reducer$0);
   return;
  }
  (globalThis));

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLjAsImZpbGUiOiJiYWxhbmNlZF9yZWR1Y2VyLmNtYS5qcyIsInNvdXJjZVJvb3QiOiIiLCJuYW1lcyI6WyJsZWZ0X2NoaWxkX2luZGV4IiwicGFyZW50X2luZGV4IiwibnVtX2JyYW5jaGVzIiwidCIsImluZGV4X2lzX2xlYWYiLCJpIiwibGVhZl9pbmRleCIsIm9mZnNldF9mcm9tX3N0YXJ0X29mX2xlYXZlc19pbiIsInJvdGF0ZWRfaW5kZXgiLCJzZXhwX29mX3QiLCJzZXhwX29mX2EiLCJ4XzAwMSIsImludmFyaWFudCIsImludmFyaWFudF9hIiwiZGF0YSIsImkkMCIsImEiLCJsZWZ0IiwicmlnaHQiLCJsZWZ0X2lzX25vbmUiLCJyaWdodF9pc19ub25lIiwiY3JlYXRlX2V4biIsIm9wdCIsIm51bV9sZWF2ZXMiLCJyZWR1Y2UiLCJzdGgiLCJudW1fbGVhdmVzX25vdF9pbl9ib3R0b21fbGV2ZWwiLCJ2YWxpZGF0ZV9pbmRleCIsImxlbmd0aCIsInNldF9leG4iLCJjaGlsZF9pbmRleCIsInBhcmVudCIsImdldF9leG4iLCJ4XzAwMiIsImNvbXB1dGVfZXhuIl0sInNvdXJjZXMiOlsiL1VzZXJzL3N2aXNobnVzLy5vcGFtL2dydi10ZXN0L2xpYi9jb3JlX2tlcm5lbC9iYWxhbmNlZF9yZWR1Y2VyL2JhbGFuY2VkX3JlZHVjZXIubWwiXSwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7Ozs7Ozs7Rzs7Ozs7Rzs7Ozs7Rzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O1lBbUNJQSxpQkFBa0JDLGNBQWUsUUFBZkE7R0FBcUM7WUFLdkRDLGFBQWFDLEdBQUksT0FBSkEsYUFBb0I7WUFDakNDLGNBQWNELEdBQUVFLEdBQUksT0FEcEJILGFBQ2NDLE1BQUVFLFVBQXVCO1lBSXZDQyxXQUFXSCxHQUFFRTtJQUdmO0tBQ01FLGlDQUpTRixJQUFGRjtLQUdUSztPQUNFRCxpQ0FKT0o7VUFJUEk7VUFBQUEsaUNBSk9KO1dBR1RLLGdCQVJGTixhQUtXQztHQVVpQjtZQUs1Qk0sVUFBVUMsV0FBVVA7aUJBRHFCRTtLQURGLFVBYnZDQyxXQWVvQkgsR0FEcUJFO0tBREYsT0FBQSxrQ0FFbkJGO0lBRHNDO0lBQXRCO0tBQ1FRLFFBRFIsMkJBQ2hCUjtpQ0FBVk87c0NBQWtDQzs7WUFFNUNDLFVBQVVDLGFBQVlWO0lBQ3hCO0tBQUlXLE9BRG9CWDtLQUVYLE1BQUEsaUNBRFRXOzs7U0FDSkM7O01BQ1EsWUFBQSxrQ0FGSkQsTUFDSkM7b0JBR1NDLGNBQUssV0FMRkgsYUFLSEc7TUFIVCxVQUFBRDtpQkFBQUE7VUFBQUE7OztJQUthLElBQUEsTUE3QlhiLGFBc0JzQkMsWUFPeEI7O1NBQUFFOztNQUNhO09BQVBZLE9BbkNKakIsaUJBa0NGSztPQUVNYSxRQURBRDtPQUVBRSxlQUFlLGtDQVRqQkwsTUFPRUc7T0FHQUcsZ0JBQWdCLGtDQVZsQk4sTUFRRUk7OztPQUdELEdBQUEsa0NBWERKLE1BTUpUO1FBSXNCLEtBRGhCYyxrQkFDQUM7UUFFQyxNQUFBOzs7O1NBbENMaEIsY0FxQnNCRCxHQVFsQmM7Y0E3QkpiLGNBcUJzQkQsR0FTbEJlLFlBQ0FDLGtCQUNBQztRQUlGLE1BQUE7O01BUkosVUFBQWY7aUJBQUFBO1VBQUFBOzs7O0dBVUk7WUFHRmdCLFdBQWFDLFlBQW1DQyxZQUFZQztJQUM5RCxHQURlRjtTQUF1QkcsTUFBdkJILFFBQUFaLFlBQXVCZTs7U0FBdkJmLDRCO09BQW1DYTs7dURBQUFBOzs7Ozs7S0FHaEQ7O0lBQUE7S0FFRXJCLGVBTDhDcUI7S0FNOUNHO09BQWlDLHlCQU5hSCxjQUFBQTtLQU85Q1QsT0FBTyxpQ0FGUFosZUFMOENxQjtJQVFsRDtZQURJVDtZQVA4Q1M7WUFNOUNHO1lBTjBERjtZQUEvQ2Q7R0FRd0Q7WUFHckVpQixlQUFleEIsR0FBRUU7SUFDbkIsR0FEbUJBOzt1REFBQUE7Ozs7OztLQUdqQjs7UUFFRXVCLFNBTGF6QixZQUtieUIsVUFMZXZCOzs7c0RBS2Z1QjtzREFMZXZCOzs7Ozs7O0dBWUk7WUFHckJ3QixRQUFRMUIsR0FBRUUsR0FBRVc7SUFmWlcsZUFlUXhCLEdBQUVFO0lBR0osSUFESlMsT0FGTVgsTUFHTlksVUFsRUZULFdBK0RRSCxHQUFFRTtJQUlaLGtDQUZJUyxNQUNBQyxRQUhVQzs7Y0FHVkQ7U0E3RVllLGNBNkVaZixRQUdFZ0IsVUFoRlVEO0tBaUZYLEdBQUEsa0NBTERoQixNQUlFaUI7TUFIRmhCO1VBT0Esa0NBUkFELE1BSUVpQixTQUhGaEIsU0FHRWdCOztHQU1GO1lBR0ZDLFFBQVE3QixHQUFFRTtJQTlCVnNCLGVBOEJReEIsR0FBRUU7SUFFcUIsVUFoRi9CQyxXQThFUUgsR0FBRUU7SUFFcUIsT0FBQSxrQ0FGdkJGO0dBRXVDO2dCQUcvQjhCLE9BQUU1QjtJQUNwQixHQUFHLGtDQURlNEIsVUFBRTVCO0tBRWYsT0FBQSxrQ0FGYTRCLFVBQUU1QjtJQUlQLElBQVBZLE9BakdKakIsaUJBNkZrQkssSUFLZGEsUUFEQUQ7T0FFTyxpQ0FOS2dCLGFBSVpoQjs7TUFLRVAsWUFUVXVCO2lDQXBFaEJ4QixVQTZFTUMsV0FUVXVCOzs7Ozs7S0FVZDs7SUFJb0M7S0FBQSxVQWR0QkEsT0FLWmY7S0FTYSxVQWREZSxPQUlaaEI7S0FVQUQsSUFBSSxXQWRRaUI7SUFlaEIsa0NBZmdCQSxVQUFFNUIsR0FjZFc7SUFDSixPQURJQTtHQUVGO1lBR0ZrQixZQUFZL0IsR0FBSSxXQUFKQSxNQUFtQjs7Ozs7OztVQXZGL0JNLFdBRUFHLFdBb0JBUyxZQTBCQVEsU0FlQUcsU0F3QkFFOzs7RSIsInNvdXJjZXNDb250ZW50IjpbIm9wZW4hIEJhc2VcblxuKCogVGhlIFtkYXRhXSBhcnJheSBpcyBhbiBpbXBsaWNpdCBiaW5hcnkgdHJlZSB3aXRoIFtjaGlsZHJlbl9sZW5ndGggKiAyIC0gMV0gbm9kZXMsXG4gICB3aXRoIGVhY2ggbm9kZSBiZWluZyB0aGUgc3VtIG9mIHRoZSB0d28gY2hpbGQgbm9kZXMgYW5kIHRoZSByb290IG5vZGUgYmVpbmcgdGhlIDB0aFxuICAgbm9kZS4gIFRoZSBsZWF2ZXMgb2YgdGhlIHRyZWUgYXJlIHRoZSBsYXN0IFtudW1fbGVhdmVzXSBub2Rlcy5cblxuICAgVGhlIGNoaWxkcmVuIGFyZSBub3QgbmVjZXNzYXJpbHkgYWxsIGF0IHRoZSBzYW1lIGxldmVsIG9mIHRoZSB0cmVlLiBGb3IgaW5zdGFuY2UgaWZcbiAgIHlvdSBoYXZlIDMgY2hpbGRyZW4gW3wgYTsgYjsgYyB8XTpcblxuICAge3ZcbiAgICAgICAgICBvXG4gICAgICAgICAvIFxcXG4gICAgICAgIG8gICBjXG4gICAgICAgLyBcXFxuICAgICAgYSAgIGJcbiAgIHZ9XG5cbiAgIFdlIHdhbnQgdGhpcyB0cmVlIHRvIGJlIHJlcHJlc2VudGF0ZWQgYXMgW3wgbzsgbzsgYzsgYTsgYiB8XSwgaS5lLiB3ZSBuZWVkIHRvIGFwcGx5XG4gICBmaXJzdCBhIHJvdGF0aW9uIHRoZW4gYSB0cmFuc2xhdGlvbiB0byBjb252ZXJ0IGFuIGluZGV4IGluIFt8IGE7IGI7IGMgfF0gdG8gYSAobGVhZilcbiAgIGluZGV4IGluIFt8IG87IG87IGM7IGE7IGIgfF0uICopXG50eXBlICdhIHQgPVxuICB7IGRhdGEgOiAnYSBPcHRpb25fYXJyYXkudFxuICA7IG51bV9sZWF2ZXMgOiBpbnRcbiAgOyBudW1fbGVhdmVzX25vdF9pbl9ib3R0b21fbGV2ZWwgOiBpbnRcbiAgOyByZWR1Y2UgOiAnYSAtPiAnYSAtPiAnYVxuICA7IHNleHBfb2ZfYSA6ICdhIC0+IFNleHAudFxuICB9XG5cbmxldCBsZW5ndGggdCA9IHQubnVtX2xlYXZlc1xuXG4oKiB7dlxuICAgICBwYXJlbnQ6ICAgICAgMCAgMSAgMiAgMyAgNCAgNSAgNiAgNyAgOCAgOSAxMCAxMSAxMiAxMyAxNCAxNSAxNiAxNyAxOCAxOSAuLi5cbiAgICAgbGVmdCBjaGlsZDogIDEgIDMgIDUgIDcgIDkgMTEgMTMgMTUgMTcgMTkgMjEgMjMgMjUgMjcgMjkgMzEgMzMgMzUgMzcgMzkgLi4uXG4gICAgIHJpZ2h0IGNoaWxkOiAyICA0ICA2ICA4IDEwIDEyIDE0IDE2IDE4IDIwIDIyIDI0IDI2IDI4IDMwIDMyIDM0IDM2IDM4IDQwIC4uLiB2fSAqKVxubGV0IHBhcmVudF9pbmRleCB+Y2hpbGRfaW5kZXggPSAoY2hpbGRfaW5kZXggLSAxKSAvIDJcbmxldCBsZWZ0X2NoaWxkX2luZGV4IH5wYXJlbnRfaW5kZXggPSAocGFyZW50X2luZGV4ICogMikgKyAxXG5sZXQgcmlnaHRfY2hpbGRfaW5kZXggfmxlZnRfY2hpbGRfaW5kZXggPSBsZWZ0X2NoaWxkX2luZGV4ICsgMVxuXG4oKiBUaGUgZmlyc3QgW251bV9sZWF2ZXMtMV0gZWxlbWVudHMgYXJlIGludGVybmFsIG5vZGVzIG9mIHRoZSB0cmVlLiAgVGhlIG5leHRcbiAgIFtudW1fbGVhdmVzXSBlbGVtZW50cyBhcmUgdGhlIGxlYXZlcy4gKilcbmxldCBudW1fYnJhbmNoZXMgdCA9IHQubnVtX2xlYXZlcyAtIDFcbmxldCBpbmRleF9pc19sZWFmIHQgaSA9IGkgPj0gbnVtX2JyYW5jaGVzIHRcblxuKCogVGhlIHRyZWUgaXMgY29tcGxldGUsIGJ1dCBub3QgbmVjZXNzYXJpbHkgcGVyZmVjdCwgc28gd2UgcGVyZm9ybSBzb21lIHJvdGF0aW9uIG9mIHRoZVxuICAgbGVhdmVzIHRvIGVuc3VyZSB0aGF0IG91ciByZWR1Y3Rpb25zIHByZXNlcnZlIG9yZGVyaW5nLiAqKVxubGV0IGxlYWZfaW5kZXggdCBpID1cbiAgKCogVGhlIHRyZWUgbGF5b3V0IGlzIGxldmVsIG9yZGVyLiAgQW55IGxlYXZlcyBpbiB0aGUgc2Vjb25kIHRvIGxhc3QgbGV2ZWwgbmVlZCB0byBvY2N1clxuICAgICBpbiB0aGUgYXJyYXkgYmVmb3JlIHRoZSBsZWF2ZXMgaW4gdGhlIGJvdHRvbSBsZXZlbC4gKilcbiAgbGV0IHJvdGF0ZWRfaW5kZXggPVxuICAgIGxldCBvZmZzZXRfZnJvbV9zdGFydF9vZl9sZWF2ZXNfaW5fYXJyYXkgPSBpICsgdC5udW1fbGVhdmVzX25vdF9pbl9ib3R0b21fbGV2ZWwgaW5cbiAgICBpZiBvZmZzZXRfZnJvbV9zdGFydF9vZl9sZWF2ZXNfaW5fYXJyYXkgPCB0Lm51bV9sZWF2ZXNcbiAgICB0aGVuIG9mZnNldF9mcm9tX3N0YXJ0X29mX2xlYXZlc19pbl9hcnJheVxuICAgIGVsc2Ugb2Zmc2V0X2Zyb21fc3RhcnRfb2ZfbGVhdmVzX2luX2FycmF5IC0gdC5udW1fbGVhdmVzXG4gIGluXG4gICgqIFRoZSBsZWF2ZXMgb2NjdXIgYWZ0ZXIgdGhlIGJyYW5jaGVzIGluIHRoZSBhcnJheS4gKilcbiAgcm90YXRlZF9pbmRleCArIG51bV9icmFuY2hlcyB0XG47O1xuXG5sZXQgZ2V0X2xlYWYgdCBpID0gT3B0aW9uX2FycmF5LmdldCB0LmRhdGEgKGxlYWZfaW5kZXggdCBpKVxubGV0IHRvX2xpc3QgdCA9IExpc3QuaW5pdCAobGVuZ3RoIHQpIH5mOihmdW4gaSAtPiBnZXRfbGVhZiB0IGkpXG5sZXQgc2V4cF9vZl90IHNleHBfb2ZfYSB0ID0gWyVzZXhwICh0b19saXN0IHQgOiBhIG9wdGlvbiBsaXN0KV1cblxubGV0IGludmFyaWFudCBpbnZhcmlhbnRfYSB0ID1cbiAgbGV0IGRhdGEgPSB0LmRhdGEgaW5cbiAgZm9yIGkgPSAwIHRvIE9wdGlvbl9hcnJheS5sZW5ndGggZGF0YSAtIDEgZG9cbiAgICBtYXRjaCBPcHRpb25fYXJyYXkuZ2V0IGRhdGEgaSB3aXRoXG4gICAgfCBOb25lIC0+ICgpXG4gICAgfCBTb21lIGEgLT4gaW52YXJpYW50X2EgYVxuICBkb25lO1xuICBmb3IgaSA9IDAgdG8gbnVtX2JyYW5jaGVzIHQgLSAxIGRvXG4gICAgbGV0IGxlZnQgPSBsZWZ0X2NoaWxkX2luZGV4IH5wYXJlbnRfaW5kZXg6aSBpblxuICAgIGxldCByaWdodCA9IHJpZ2h0X2NoaWxkX2luZGV4IH5sZWZ0X2NoaWxkX2luZGV4OmxlZnQgaW5cbiAgICBsZXQgbGVmdF9pc19ub25lID0gT3B0aW9uX2FycmF5LmlzX25vbmUgZGF0YSBsZWZ0IGluXG4gICAgbGV0IHJpZ2h0X2lzX25vbmUgPSBPcHRpb25fYXJyYXkuaXNfbm9uZSBkYXRhIHJpZ2h0IGluXG4gICAgaWYgT3B0aW9uX2FycmF5LmlzX3NvbWUgZGF0YSBpXG4gICAgdGhlbiBhc3NlcnQgKG5vdCAobGVmdF9pc19ub25lIHx8IHJpZ2h0X2lzX25vbmUpKVxuICAgIGVsc2VcbiAgICAgIGFzc2VydCAoXG4gICAgICAgIGluZGV4X2lzX2xlYWYgdCBsZWZ0IHx8IGluZGV4X2lzX2xlYWYgdCByaWdodCB8fCBsZWZ0X2lzX25vbmUgfHwgcmlnaHRfaXNfbm9uZSlcbiAgZG9uZVxuOztcblxubGV0IGNyZWF0ZV9leG4gPyhzZXhwX29mX2EgPSBbJXNleHBfb2Y6IF9dKSAoKSB+bGVuOm51bV9sZWF2ZXMgfnJlZHVjZSA9XG4gIGlmIG51bV9sZWF2ZXMgPCAxXG4gIHRoZW5cbiAgICByYWlzZV9zXG4gICAgICBbJW1lc3NhZ2UgXCJub24tcG9zaXRpdmUgbnVtYmVyIG9mIGxlYXZlcyBpbiBiYWxhbmNlZCByZWR1Y2VyXCIgKG51bV9sZWF2ZXMgOiBpbnQpXTtcbiAgbGV0IG51bV9icmFuY2hlcyA9IG51bV9sZWF2ZXMgLSAxIGluXG4gIGxldCBudW1fbGVhdmVzX25vdF9pbl9ib3R0b21fbGV2ZWwgPSBJbnQuY2VpbF9wb3cyIG51bV9sZWF2ZXMgLSBudW1fbGVhdmVzIGluXG4gIGxldCBkYXRhID0gT3B0aW9uX2FycmF5LmNyZWF0ZSB+bGVuOihudW1fYnJhbmNoZXMgKyBudW1fbGVhdmVzKSBpblxuICB7IGRhdGE7IG51bV9sZWF2ZXM7IG51bV9sZWF2ZXNfbm90X2luX2JvdHRvbV9sZXZlbDsgcmVkdWNlOyBzZXhwX29mX2EgfVxuOztcblxubGV0IHZhbGlkYXRlX2luZGV4IHQgaSA9XG4gIGlmIGkgPCAwXG4gIHRoZW5cbiAgICByYWlzZV9zXG4gICAgICBbJW1lc3NhZ2UgXCJhdHRlbXB0IHRvIGFjY2VzcyBuZWdhdGl2ZSBpbmRleCBpbiBiYWxhbmNlZCByZWR1Y2VyXCIgfmluZGV4OihpIDogaW50KV07XG4gIGxldCBsZW5ndGggPSB0Lm51bV9sZWF2ZXMgaW5cbiAgaWYgaSA+PSBsZW5ndGhcbiAgdGhlblxuICAgIHJhaXNlX3NcbiAgICAgIFslbWVzc2FnZVxuICAgICAgICBcImF0dGVtcHQgdG8gYWNjZXNzIG91dCBvZiBib3VuZHMgaW5kZXggaW4gYmFsYW5jZWQgcmVkdWNlclwiXG4gICAgICAgICAgfmluZGV4OihpIDogaW50KVxuICAgICAgICAgIChsZW5ndGggOiBpbnQpXVxuOztcblxubGV0IHNldF9leG4gdCBpIGEgPVxuICB2YWxpZGF0ZV9pbmRleCB0IGk7XG4gIGxldCBkYXRhID0gdC5kYXRhIGluXG4gIGxldCBpID0gcmVmIChsZWFmX2luZGV4IHQgaSkgaW5cbiAgT3B0aW9uX2FycmF5LnNldF9zb21lIGRhdGEgIWkgYTtcbiAgd2hpbGUgIWkgPD4gMCBkb1xuICAgIGxldCBwYXJlbnQgPSBwYXJlbnRfaW5kZXggfmNoaWxkX2luZGV4OiFpIGluXG4gICAgaWYgT3B0aW9uX2FycmF5LmlzX25vbmUgZGF0YSBwYXJlbnRcbiAgICB0aGVuIGkgOj0gMFxuICAgIGVsc2UgKFxuICAgICAgT3B0aW9uX2FycmF5LnVuc2FmZV9zZXRfbm9uZSBkYXRhIHBhcmVudDtcbiAgICAgIGkgOj0gcGFyZW50KVxuICBkb25lXG47O1xuXG5sZXQgZ2V0X2V4biB0IGkgPVxuICB2YWxpZGF0ZV9pbmRleCB0IGk7XG4gIE9wdGlvbl9hcnJheS5nZXRfc29tZV9leG4gdC5kYXRhIChsZWFmX2luZGV4IHQgaSlcbjs7XG5cbmxldCByZWMgY29tcHV0ZV9leG4gdCBpID1cbiAgaWYgT3B0aW9uX2FycmF5LmlzX3NvbWUgdC5kYXRhIGlcbiAgdGhlbiBPcHRpb25fYXJyYXkudW5zYWZlX2dldF9zb21lX2V4biB0LmRhdGEgaVxuICBlbHNlIChcbiAgICBsZXQgbGVmdCA9IGxlZnRfY2hpbGRfaW5kZXggfnBhcmVudF9pbmRleDppIGluXG4gICAgbGV0IHJpZ2h0ID0gcmlnaHRfY2hpbGRfaW5kZXggfmxlZnRfY2hpbGRfaW5kZXg6bGVmdCBpblxuICAgIGlmIGxlZnQgPj0gT3B0aW9uX2FycmF5Lmxlbmd0aCB0LmRhdGFcbiAgICB0aGVuIChcbiAgICAgICgqIElmIHdlIGdldCBoZXJlLCB0aGUgcGFyZW50IHdhcyBhbiB1bnNldCBsZWFmLiAqKVxuICAgICAgbGV0IHNleHBfb2ZfYSA9IHQuc2V4cF9vZl9hIGluXG4gICAgICByYWlzZV9zXG4gICAgICAgIFslbWVzc2FnZVxuICAgICAgICAgIFwiYXR0ZW1wdCB0byBjb21wdXRlIGJhbGFuY2VkIHJlZHVjZXIgd2l0aCB1bnNldCBlbGVtZW50c1wiXG4gICAgICAgICAgICB+YmFsYW5jZWRfcmVkdWNlcjoodCA6IGEgdCldKTtcbiAgICBsZXQgYSA9IHQucmVkdWNlIChjb21wdXRlX2V4biB0IGxlZnQpIChjb21wdXRlX2V4biB0IHJpZ2h0KSBpblxuICAgIE9wdGlvbl9hcnJheS51bnNhZmVfc2V0X3NvbWUgdC5kYXRhIGkgYTtcbiAgICBhKVxuOztcblxubGV0IGNvbXB1dGVfZXhuIHQgPSBjb21wdXRlX2V4biB0IDBcbiJdfQ==
