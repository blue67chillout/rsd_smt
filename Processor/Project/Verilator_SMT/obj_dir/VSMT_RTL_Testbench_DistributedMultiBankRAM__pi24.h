// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DISTRIBUTEDMULTIBANKRAM__PI24_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DISTRIBUTEDMULTIBANKRAM__PI24_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__unnamedblk3__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__2__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__2__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__3__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__3__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__4__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__4__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__5__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__5__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__6__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__6__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__7__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__7__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    VL_IN8(__PVT__we[8],0,0);
    VL_IN8(__PVT__wa[8],3,0);
    VL_IN8(__PVT__wv[8],3,0);
    VL_IN8(__PVT__ra[2],3,0);
    VL_OUT8(__PVT__rv[2],3,0);
    VlUnpacked<CData/*3:0*/, 16> debugValue;
    VlUnpacked<CData/*3:0*/, 8> __PVT__genblk1__DOT__rBank__DOT__waBank;
    VlUnpacked<CData/*3:0*/, 8> __PVT__genblk1__DOT__rBank__DOT__raBank;
    VlUnpacked<CData/*3:0*/, 8> __PVT__genblk1__DOT__rBank__DOT__rvBank;
    VlUnpacked<CData/*3:0*/, 8> __PVT__genblk1__DOT__rBank__DOT__wvBank;
    VlUnpacked<CData/*0:0*/, 8> __PVT__genblk1__DOT__rBank__DOT__weBank;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__array;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__array;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__2__KET____DOT__genblk1__DOT__rBank__DOT__array;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__3__KET____DOT__genblk1__DOT__rBank__DOT__array;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__4__KET____DOT__genblk1__DOT__rBank__DOT__array;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__5__KET____DOT__genblk1__DOT__rBank__DOT__array;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__6__KET____DOT__genblk1__DOT__rBank__DOT__array;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__7__KET____DOT__genblk1__DOT__rBank__DOT__array;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
