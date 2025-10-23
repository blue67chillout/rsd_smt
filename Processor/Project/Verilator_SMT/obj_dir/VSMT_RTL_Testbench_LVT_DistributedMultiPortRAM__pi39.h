// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_LVT_DISTRIBUTEDMULTIPORTRAM__PI39_H_
#define VERILATED_VSMT_RTL_TESTBENCH_LVT_DISTRIBUTEDMULTIPORTRAM__PI39_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    VL_IN8(__PVT__we[1],0,0);
    VL_IN8(__PVT__wa[1],3,0);
    VL_IN64(__PVT__wv[1],37,0);
    VL_IN8(__PVT__ra[2],3,0);
    VL_OUT64(__PVT__rv[2],37,0);
    VlUnpacked<QData/*37:0*/, 16> debugValue;
    VlUnpacked<QData/*37:0*/, 16> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__array;
    VlUnpacked<QData/*37:0*/, 16> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__array;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
