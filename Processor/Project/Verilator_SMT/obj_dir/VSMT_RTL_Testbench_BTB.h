// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_BTB_H_
#define VERILATED_VSMT_RTL_TESTBENCH_BTB_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB14;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_BTB final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB14* btbEntryArray;

    // DESIGN SPECIFIC STATE
    CData/*0:0*/ __PVT__pushBtbQueue;
    CData/*0:0*/ __PVT__popBtbQueue;
    CData/*0:0*/ __PVT__updateBtb;
    CData/*4:0*/ __PVT__btbQueuePointer__DOT__regHeadStorage;
    CData/*4:0*/ __PVT__btbQueuePointer__DOT__nextHeadStorage;
    CData/*4:0*/ __PVT__btbQueuePointer__DOT__regTailStorage;
    CData/*4:0*/ __PVT__btbQueuePointer__DOT__nextTailStorage;
    CData/*5:0*/ __PVT__btbQueuePointer__DOT__regCount;
    CData/*5:0*/ __PVT__btbQueuePointer__DOT__nextCount;
    CData/*3:0*/ __Vfunc_ToBTB_Tag__1__Vfuncout;
    CData/*3:0*/ __Vfunc_ToBTB_Tag__1__tag;
    SData/*9:0*/ __PVT__resetIndex;
    IData/*19:0*/ __PVT__pcIn;
    QData/*39:0*/ __PVT__tagReg;
    QData/*39:0*/ __PVT__nextTagReg;
    IData/*31:0*/ __PVT__unnamedblk5__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk6__DOT__i;
    IData/*19:0*/ __Vfunc_ToBTB_Tag__1__pc;
    VlUnpacked<CData/*0:0*/, 2> __PVT__btbWE;
    VlUnpacked<SData/*9:0*/, 2> __PVT__btbWA;
    VlUnpacked<IData/*19:0*/, 2> __PVT__btbWV;
    VlUnpacked<SData/*9:0*/, 2> __PVT__btbRA;
    VlUnpacked<IData/*19:0*/, 2> __PVT__btbRV;
    VlUnpacked<IData/*19:0*/, 2> __PVT__btbOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__btbHit;
    VlUnpacked<CData/*0:0*/, 2> __PVT__readIsCondBr;
    VlUnpacked<QData/*51:0*/, 32> __PVT__btbQueue;
    VlUnpacked<IData/*19:0*/, 2> __Vcellout__btbEntryArray__rv;
    VlUnpacked<IData/*19:0*/, 2> __Vcellinp__btbEntryArray__wv;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_BTB(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_BTB();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_BTB);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
