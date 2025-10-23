// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_BLOCKMULTIBANKRAM__E800_EB2_H_
#define VERILATED_VSMT_RTL_TESTBENCH_BLOCKMULTIBANKRAM__E800_EB2_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    CData/*1:0*/ genblk1__DOT__rBank__DOT____Vcellout__genblk2__BRA__0__KET____DOT__rBank____pinNumber6;
    CData/*1:0*/ genblk1__DOT__rBank__DOT____Vcellout__genblk2__BRA__1__KET____DOT__rBank____pinNumber6;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__unnamedblk3__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__b;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__b;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b;
    VL_IN8(__PVT__we[2],0,0);
    VL_IN16(__PVT__wa[2],10,0);
    VL_IN8(__PVT__wv[2],1,0);
    VL_IN16(__PVT__ra[2],10,0);
    VL_OUT8(__PVT__rv[2],1,0);
    VlUnpacked<CData/*1:0*/, 2048> debugValue;
    VlUnpacked<CData/*1:0*/, 2> __PVT__rvReg;
    VlUnpacked<SData/*10:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__waBank;
    VlUnpacked<SData/*10:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__raBank;
    VlUnpacked<CData/*1:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__rvBank;
    VlUnpacked<CData/*1:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__wvBank;
    VlUnpacked<CData/*0:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__weBank;
    VlUnpacked<SData/*10:0*/, 2> __PVT__genblk1__DOT__rBank__DOT__raReg;
    VlUnpacked<CData/*1:0*/, 1024> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array;
    VlUnpacked<CData/*1:0*/, 1024> __PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
