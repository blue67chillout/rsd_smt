// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_RMT_H_
#define VERILATED_VSMT_RTL_TESTBENCH_RMT_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi7;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RMT final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi7* regRMT;

    // DESIGN SPECIFIC STATE
    IData/*31:0*/ __PVT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__unnamedblk5__DOT__i;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phySrcRegA;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phySrcRegB;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phySrcRegC;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phyPrevDstReg;
    VlUnpacked<CData/*3:0*/, 2> __PVT__srcIssueQueuePtrRegA;
    VlUnpacked<CData/*3:0*/, 2> __PVT__srcIssueQueuePtrRegB;
    VlUnpacked<CData/*3:0*/, 2> __PVT__srcIssueQueuePtrRegC;
    VlUnpacked<CData/*0:0*/, 2> __PVT__rmtWE;
    VlUnpacked<CData/*5:0*/, 2> __PVT__rmtWA;
    VlUnpacked<SData/*9:0*/, 2> __PVT__rmtWV;
    VlUnpacked<CData/*5:0*/, 8> __PVT__rmtRA;
    VlUnpacked<SData/*9:0*/, 8> __PVT__rmtRV;
    VlUnpacked<SData/*9:0*/, 8> __Vcellout__regRMT__rv;
    VlUnpacked<CData/*5:0*/, 8> __Vcellinp__regRMT__ra;
    VlUnpacked<SData/*9:0*/, 2> __Vcellinp__regRMT__wv;
    VlUnpacked<CData/*5:0*/, 2> __Vcellinp__regRMT__wa;
    VlUnpacked<CData/*5:0*/, 2> __PVT__rstWriteLogRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__rstWritePhyRegNum;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RMT(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RMT();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RMT);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
