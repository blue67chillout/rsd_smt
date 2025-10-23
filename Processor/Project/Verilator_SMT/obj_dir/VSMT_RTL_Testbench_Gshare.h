// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_GSHARE_H_
#define VERILATED_VSMT_RTL_TESTBENCH_GSHARE_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_Gshare final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2* pht;

    // DESIGN SPECIFIC STATE
    CData/*0:0*/ __PVT__stall;
    CData/*0:0*/ __PVT__clear;
    CData/*0:0*/ __PVT__mispred;
    CData/*0:0*/ __PVT__pushPhtQueue;
    CData/*0:0*/ __PVT__popPhtQueue;
    CData/*0:0*/ __PVT__updatePht;
    CData/*4:0*/ __PVT__phtQueuePointer__DOT__regHeadStorage;
    CData/*4:0*/ __PVT__phtQueuePointer__DOT__nextHeadStorage;
    CData/*4:0*/ __PVT__phtQueuePointer__DOT__regTailStorage;
    CData/*4:0*/ __PVT__phtQueuePointer__DOT__nextTailStorage;
    CData/*5:0*/ __PVT__phtQueuePointer__DOT__regCount;
    CData/*5:0*/ __PVT__phtQueuePointer__DOT__nextCount;
    SData/*9:0*/ __PVT__nextBrGlobalHistory;
    SData/*9:0*/ __PVT__regBrGlobalHistory;
    SData/*10:0*/ __PVT__resetIndex;
    IData/*19:0*/ __PVT__pcIn;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk2__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk3__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk5__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk6__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk7__DOT__i;
    VlUnpacked<CData/*0:0*/, 2> __PVT__brPredTaken;
    VlUnpacked<CData/*0:0*/, 2> __PVT__updateHistory;
    VlUnpacked<CData/*0:0*/, 2> __PVT__phtWE;
    VlUnpacked<SData/*10:0*/, 2> __PVT__phtWA;
    VlUnpacked<CData/*1:0*/, 2> __PVT__phtWV;
    VlUnpacked<CData/*1:0*/, 2> __PVT__phtPrevValue;
    VlUnpacked<SData/*10:0*/, 2> __PVT__phtRA;
    VlUnpacked<CData/*1:0*/, 2> __PVT__phtRV;
    VlUnpacked<SData/*9:0*/, 2> __PVT__brGlobalHistory;
    VlUnpacked<QData/*33:0*/, 32> __PVT__phtQueue;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_Gshare(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_Gshare();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_Gshare);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
