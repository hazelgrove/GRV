// Generated by js_of_ocaml
//# buildInfo:effects=false, kind=cma, use-js-string=true, version=5.6.0

//# unitInfo: Provides: Weak_array
//# unitInfo: Requires: Core, Core__Array, Core__Heap_block, Core__Option, Expect_test_collector, Ppx_bench_lib__Benchmark_accumulator, Ppx_inline_test_lib__Runtime, Ppx_module_timer_runtime, Stdlib__Weak
(function
  (globalThis){
   "use strict";
   var
    runtime = globalThis.jsoo_runtime,
    cst_Weak_array$0 = "Weak_array",
    cst_weak_array$0 = "weak_array";
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
   function caml_call5(f, a0, a1, a2, a3, a4){
    return (f.l >= 0 ? f.l : f.l = f.length) == 5
            ? f(a0, a1, a2, a3, a4)
            : runtime.caml_call_gen(f, [a0, a1, a2, a3, a4]);
   }
   var
    global_data = runtime.caml_get_global_data(),
    cst = "",
    Stdlib_Weak = global_data.Stdlib__Weak,
    Core_Heap_block = global_data.Core__Heap_block,
    Core = global_data.Core,
    Core_Array = global_data.Core__Array,
    Core_Option = global_data.Core__Option,
    Ppx_module_timer_runtime = global_data.Ppx_module_timer_runtime,
    Ppx_bench_lib_Benchmark_accumu =
      global_data.Ppx_bench_lib__Benchmark_accumulator,
    Expect_test_collector = global_data.Expect_test_collector,
    Ppx_inline_test_lib_Runtime = global_data.Ppx_inline_test_lib__Runtime;
   caml_call1(Ppx_module_timer_runtime[4], cst_Weak_array$0);
   caml_call1(Ppx_bench_lib_Benchmark_accumu[1][1], cst_weak_array$0);
   caml_call1(Expect_test_collector[5][1], "weak_array/src/weak_array.ml");
   caml_call2(Ppx_inline_test_lib_Runtime[2], cst_weak_array$0, cst);
   var cst_weak_array = cst_weak_array$0, cst_Weak_array = cst_Weak_array$0;
   function create(len){return caml_call1(Stdlib_Weak[1], len);}
   function length(t){return caml_call1(Stdlib_Weak[2], t);}
   var set = Stdlib_Weak[3];
   function set_exn(t, i, x){
    return caml_call3
            (set, t, i, caml_call2(Core_Option[29], x, Core_Heap_block[3]));
   }
   var get = Stdlib_Weak[4];
   function is_some(t, i){return caml_call2(Stdlib_Weak[6], t, i);}
   function is_none(t, i){return 1 - is_some(t, i);}
   function sexp_of_t(sexp_of_a, t){
    function _g_(i){return caml_call2(get, t, i);}
    var
     _h_ = length(t),
     x_001 = caml_call2(Core_Array[40], _h_, _g_),
     _i_ = caml_call1(Core_Heap_block[1], sexp_of_a),
     _j_ = caml_call1(Core[454], _i_);
    return caml_call2(Core[288], _j_, x_001);
   }
   function iter(t, f){
    var _e_ = length(t) - 1 | 0, _d_ = 0;
    if(_e_ >= 0){
     var i = _d_;
     for(;;){
      var match = caml_call2(get, t, i);
      if(match){
       var v = match[1];
       caml_call1(f, caml_call1(Core_Heap_block[4], v));
      }
      var _f_ = i + 1 | 0;
      if(_e_ === i) break;
      var i = _f_;
     }
    }
    return 0;
   }
   function iteri(t, f){
    var _b_ = length(t) - 1 | 0, _a_ = 0;
    if(_b_ >= 0){
     var i = _a_;
     for(;;){
      var match = caml_call2(get, t, i);
      if(match){
       var v = match[1];
       caml_call2(f, i, caml_call1(Core_Heap_block[4], v));
      }
      var _c_ = i + 1 | 0;
      if(_b_ === i) break;
      var i = _c_;
     }
    }
    return 0;
   }
   function blit(src, src_pos, dst, dst_pos, len){
    return caml_call5(Stdlib_Weak[8], src, src_pos, dst, dst_pos, len);
   }
   caml_call1(Ppx_inline_test_lib_Runtime[3], cst_weak_array);
   caml_call1(Expect_test_collector[5][2], 0);
   caml_call1(Ppx_bench_lib_Benchmark_accumu[1][2], 0);
   caml_call1(Ppx_module_timer_runtime[5], cst_Weak_array);
   var
    Weak_array =
      [0,
       sexp_of_t,
       create,
       length,
       set,
       set_exn,
       get,
       is_some,
       is_none,
       iter,
       iteri,
       blit];
   runtime.caml_register_global(16, Weak_array, cst_Weak_array$0);
   return;
  }
  (globalThis));

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLjAsImZpbGUiOiJ3ZWFrX2FycmF5LmNtYS5qcyIsInNvdXJjZVJvb3QiOiIiLCJuYW1lcyI6WyJjcmVhdGUiLCJsZW4iLCJsZW5ndGgiLCJ0Iiwic2V0Iiwic2V0X2V4biIsImkiLCJ4IiwiZ2V0IiwiaXNfc29tZSIsImlzX25vbmUiLCJzZXhwX29mX3QiLCJzZXhwX29mX2EiLCJ4XzAwMSIsIml0ZXIiLCJmIiwidiIsIml0ZXJpIiwiYmxpdCIsInNyYyIsInNyY19wb3MiLCJkc3QiLCJkc3RfcG9zIl0sInNvdXJjZXMiOlsiL1VzZXJzL3N2aXNobnVzLy5vcGFtL2dydi10ZXN0L2xpYi9jb3JlX2tlcm5lbC93ZWFrX2FycmF5L3dlYWtfYXJyYXkubWwiXSwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7OztHOzs7OztHOzs7OztHOzs7OztHOzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7OztZQU1JQSxPQUFRQyxLQUFNLE9BQUEsMkJBQU5BLEtBQXFCO1lBQzdCQyxPQUFPQyxHQUFJLE9BQUEsMkJBQUpBLEdBQWlCO09BQ3hCQztZQUVBQyxRQUFRRixHQUFFRyxHQUFFQztJQUFJLE9BQVE7YUFGeEJILEtBRVFELEdBQUVHLEdBQWMsNEJBQVpDO0dBQW1EO09BRS9EQztZQUNBQyxRQUFRTixHQUFFRyxHQUFJLE9BQUEsMkJBQU5ILEdBQUVHLEdBQWtCO1lBQzVCSSxRQUFRUCxHQUFFRyxHQUFJLFdBRGRHLFFBQ1FOLEdBQUVHLEdBQXFCO1lBRS9CSyxVQUFVQyxXQUFVVDtpQkFEdUJHLEdBQUssT0FBQSxXQUhoREUsS0FJb0JMLEdBRHVCRyxHQUFZO0lBQS9CO0tBQUEsTUFSeEJKLE9BU29CQztLQUFlVSxRQURHOzBDQUM1QkQ7O3NDQUF5QkM7R0FBeUM7WUFFNUVDLEtBQUtYLEdBQUdZO0lBQ1YsSUFBYSxNQVpYYixPQVdLQyxZQUNQOztTQUFBRzs7TUFDUSxZQUFBLFdBUk5FLEtBTUtMLEdBQ1BHOztXQUdTVTtPQUFLLFdBSkpELEdBSU0sK0JBQVBDOztNQUhULFVBQUFWO2lCQUFBQTtVQUFBQTs7OztHQUlJO1lBRUZXLE1BQU1kLEdBQUdZO0lBQ1gsSUFBYSxNQW5CWGIsT0FrQk1DLFlBQ1I7O1NBQUFHOztNQUNRLFlBQUEsV0FmTkUsS0FhTUwsR0FDUkc7O1dBR1NVO09BQUssV0FKSEQsR0FDWFQsR0FHa0IsK0JBQVRVOztNQUhULFVBQUFWO2lCQUFBQTtVQUFBQTs7OztHQUlJO1lBRUZZLEtBQU1DLEtBQUtDLFNBQVNDLEtBQUtDLFNBQVNyQjtJQUNwQyxPQUFBLDJCQURRa0IsS0FBS0MsU0FBU0MsS0FBS0MsU0FBU3JCO0dBQ0M7Ozs7Ozs7O09BakJuQ1U7T0FWQVg7T0FDQUU7T0FDQUU7T0FFQUM7T0FFQUc7T0FDQUM7T0FDQUM7T0FJQUk7T0FPQUc7T0FPQUM7OztFIiwic291cmNlc0NvbnRlbnQiOlsib3BlbiEgQ29yZVxuXG5tb2R1bGUgV2VhayA9IENhbWwuV2Vha1xuXG50eXBlICdhIHQgPSAnYSBIZWFwX2Jsb2NrLnQgV2Vhay50XG5cbmxldCBjcmVhdGUgfmxlbiA9IFdlYWsuY3JlYXRlIGxlblxubGV0IGxlbmd0aCB0ID0gV2Vhay5sZW5ndGggdFxubGV0IHNldCA9IFdlYWsuc2V0XG5cbmxldCBzZXRfZXhuIHQgaSB4ID0gc2V0IHQgaSAoT3B0aW9uLm1hcCB4IH5mOkhlYXBfYmxvY2suY3JlYXRlX2V4bilcblxubGV0IGdldCA9IFdlYWsuZ2V0XG5sZXQgaXNfc29tZSB0IGkgPSBXZWFrLmNoZWNrIHQgaVxubGV0IGlzX25vbmUgdCBpID0gbm90IChpc19zb21lIHQgaSlcbmxldCB0b19hcnJheSB0ID0gQXJyYXkuaW5pdCAobGVuZ3RoIHQpIH5mOihmdW4gaSAtPiBnZXQgdCBpKVxubGV0IHNleHBfb2ZfdCBzZXhwX29mX2EgdCA9IFslc2V4cF9vZjogYSBIZWFwX2Jsb2NrLnQgb3B0aW9uIGFycmF5XSAodG9fYXJyYXkgdClcblxubGV0IGl0ZXIgdCB+ZiA9XG4gIGZvciBpID0gMCB0byBsZW5ndGggdCAtIDEgZG9cbiAgICBtYXRjaCBnZXQgdCBpIHdpdGhcbiAgICB8IE5vbmUgLT4gKClcbiAgICB8IFNvbWUgdiAtPiBmIChIZWFwX2Jsb2NrLnZhbHVlIHYpXG4gIGRvbmVcblxubGV0IGl0ZXJpIHQgfmYgPVxuICBmb3IgaSA9IDAgdG8gbGVuZ3RoIHQgLSAxIGRvXG4gICAgbWF0Y2ggZ2V0IHQgaSB3aXRoXG4gICAgfCBOb25lIC0+ICgpXG4gICAgfCBTb21lIHYgLT4gZiBpIChIZWFwX2Jsb2NrLnZhbHVlIHYpXG4gIGRvbmVcblxubGV0IGJsaXQgfnNyYyB+c3JjX3BvcyB+ZHN0IH5kc3RfcG9zIH5sZW4gPVxuICBXZWFrLmJsaXQgc3JjIHNyY19wb3MgZHN0IGRzdF9wb3MgbGVuXG4iXX0=
