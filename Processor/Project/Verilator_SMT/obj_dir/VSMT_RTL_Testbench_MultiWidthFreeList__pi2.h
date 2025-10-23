// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_MULTIWIDTHFREELIST__PI2_H_
#define VERILATED_VSMT_RTL_TESTBENCH_MULTIWIDTHFREELIST__PI2_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_MultiWidthFreeList__pi2 final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22* freeList;

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    VL_OUT8(__PVT__count,5,0);
    CData/*1:0*/ __PVT__pushCount;
    CData/*1:0*/ __PVT__popCount;
    CData/*4:0*/ __PVT__rstIndex;
    CData/*4:0*/ __PVT__queuePointer__DOT__regHead;
    CData/*4:0*/ __PVT__queuePointer__DOT__nextHead;
    CData/*4:0*/ __PVT__queuePointer__DOT__regTail;
    CData/*4:0*/ __PVT__queuePointer__DOT__nextTail;
    CData/*5:0*/ __PVT__queuePointer__DOT__regCount;
    CData/*5:0*/ __PVT__queuePointer__DOT__nextCount;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__i;
    VL_IN8(__PVT__push[2],0,0);
    VL_IN8(__PVT__pop[2],0,0);
    VL_IN8(__PVT__pushedData[2],6,0);
    VL_OUT8(__PVT__poppedData[2],6,0);
    VlUnpacked<CData/*0:0*/, 2> __PVT__we;
    VlUnpacked<CData/*6:0*/, 2> __PVT__wv;
    VlUnpacked<CData/*4:0*/, 2> __PVT__wa;
    VlUnpacked<CData/*6:0*/, 2> __PVT__rv;
    VlUnpacked<CData/*4:0*/, 2> __PVT__ra;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_MultiWidthFreeList__pi2(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_MultiWidthFreeList__pi2();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_MultiWidthFreeList__pi2);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
