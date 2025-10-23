// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_LVT_DISTRIBUTEDMULTIPORTRAM__PI31_H_
#define VERILATED_VSMT_RTL_TESTBENCH_LVT_DISTRIBUTEDMULTIPORTRAM__PI31_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi31 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    // Anonymous structures to workaround compiler member-count bugs
    struct {
        VL_IN8(__PVT__clk,0,0);
        IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__unnamedblk7__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
    };
    struct {
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        VL_IN8(__PVT__we[2],0,0);
        VL_IN8(__PVT__wa[2],5,0);
        VL_IN16(__PVT__wv[2],9,0);
        VL_IN8(__PVT__ra[8],5,0);
        VL_OUT16(__PVT__rv[8],9,0);
        VlUnpacked<SData/*9:0*/, 64> debugValue;
        VlUnpacked<VlUnpacked<SData/*9:0*/, 2>, 8> __PVT__genblk1__DOT__rvBank;
        VlUnpacked<CData/*0:0*/, 2> __PVT__genblk1__DOT__lvi;
        VlUnpacked<CData/*0:0*/, 8> __PVT__genblk1__DOT__lvo;
        VlUnpacked<CData/*0:0*/, 2> __PVT__genblk1__DOT__lvt__DOT__rwbWriteValue;
        VlUnpacked<CData/*5:0*/, 2> __PVT__genblk1__DOT__lvt__DOT__wbReadAddr;
        VlUnpacked<VlUnpacked<CData/*0:0*/, 2>, 2> __PVT__genblk1__DOT__lvt__DOT__wbReadValue;
        VlUnpacked<CData/*5:0*/, 8> __PVT__genblk1__DOT__lvt__DOT__rbReadAddr;
        VlUnpacked<VlUnpacked<CData/*0:0*/, 8>, 2> __PVT__genblk1__DOT__lvt__DOT__rbReadValue;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__debugValue;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 64> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__rBank__DOT__array;
        VlUnpacked<SData/*9:0*/, 64> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__rBank__DOT__array;
    };

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi31(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi31();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi31);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
