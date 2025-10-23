// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DISTRIBUTEDMULTIPORTRAM__PI11_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DISTRIBUTEDMULTIPORTRAM__PI11_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    CData/*0:0*/ genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT____Vlvbound_hbbd7c02b__0;
    CData/*0:0*/ genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT____Vlvbound_hbbd7c02b__0;
    CData/*0:0*/ genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0;
    CData/*0:0*/ genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0;
    CData/*0:0*/ genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0;
    CData/*0:0*/ genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__unnamedblk7__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    VL_IN8(__PVT__we[2],0,0);
    VL_IN8(__PVT__wa[2],3,0);
    VL_INW(__PVT__wv[2],124,0,4);
    VL_IN8(__PVT__ra[2],3,0);
    VL_OUTW(__PVT__rv[2],124,0,4);
    VlUnpacked<VlWide<4>/*124:0*/, 16> debugValue;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __PVT__genblk1__DOT__body__DOT__rwbWriteValue;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__body__DOT__wbReadAddr;
    VlUnpacked<VlUnpacked<VlWide<4>/*124:0*/, 2>, 2> __PVT__genblk1__DOT__body__DOT__wbReadValue;
    VlUnpacked<CData/*3:0*/, 2> __PVT__genblk1__DOT__body__DOT__rbReadAddr;
    VlUnpacked<VlUnpacked<VlWide<4>/*124:0*/, 2>, 2> __PVT__genblk1__DOT__body__DOT__rbReadValue;
    VlUnpacked<VlWide<4>/*124:0*/, 16> __PVT__genblk1__DOT__body__DOT__debugValue;
    VlUnpacked<VlWide<4>/*124:0*/, 16> __PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
    VlUnpacked<VlWide<4>/*124:0*/, 16> __PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
    VlUnpacked<VlWide<4>/*124:0*/, 16> __PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
    VlUnpacked<VlWide<4>/*124:0*/, 16> __PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
    VlUnpacked<VlWide<4>/*124:0*/, 16> __PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
    VlUnpacked<VlWide<4>/*124:0*/, 16> __PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
