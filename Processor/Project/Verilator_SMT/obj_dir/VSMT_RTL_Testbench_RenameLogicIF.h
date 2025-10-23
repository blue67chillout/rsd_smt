// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_RENAMELOGICIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_RENAMELOGICIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RenameLogicIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    CData/*1:0*/ __PVT__updateRMT;
    CData/*0:0*/ __PVT__allocatable;
    CData/*1:0*/ __PVT__rmtWriteReg;
    CData/*1:0*/ __PVT__retRMT_WriteReg;
    CData/*1:0*/ __PVT__watWriteRegFromPipeReg;
    CData/*0:0*/ __PVT__commit;
    CData/*1:0*/ __PVT__commitNum;
    CData/*1:0*/ __PVT__flushNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__logSrcRegA;
    VlUnpacked<CData/*5:0*/, 2> __PVT__logSrcRegB;
    VlUnpacked<CData/*5:0*/, 2> __PVT__logSrcRegC;
    VlUnpacked<CData/*5:0*/, 2> __PVT__logDstReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phySrcRegA;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phySrcRegB;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phySrcRegC;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phyDstReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phyPrevDstReg;
    VlUnpacked<CData/*0:0*/, 2> __PVT__readRegA;
    VlUnpacked<CData/*0:0*/, 2> __PVT__readRegB;
    VlUnpacked<CData/*0:0*/, 2> __PVT__readRegC;
    VlUnpacked<CData/*0:0*/, 2> __PVT__writeReg;
    VlUnpacked<CData/*0:0*/, 2> __PVT__releaseReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__phyReleasedReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__rmtWriteReg_PhyRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__rmtWriteReg_LogRegNum;
    VlUnpacked<CData/*6:0*/, 2> __PVT__retRMT_WriteReg_PhyRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__retRMT_WriteReg_LogRegNum;
    VlUnpacked<CData/*6:0*/, 2> __PVT__retRMT_ReadReg_PhyRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__retRMT_ReadReg_LogRegNum;
    VlUnpacked<CData/*3:0*/, 2> __PVT__watWriteIssueQueuePtrFromPipeReg;
    VlUnpacked<CData/*3:0*/, 2> __PVT__srcIssueQueuePtrRegA;
    VlUnpacked<CData/*3:0*/, 2> __PVT__srcIssueQueuePtrRegB;
    VlUnpacked<CData/*3:0*/, 2> __PVT__srcIssueQueuePtrRegC;
    VlUnpacked<CData/*3:0*/, 2> __PVT__prevDependIssueQueuePtr;
    VlUnpacked<CData/*0:0*/, 2> __PVT__watWriteReg;
    VlUnpacked<CData/*5:0*/, 2> __PVT__watWriteLogRegNum;
    VlUnpacked<CData/*3:0*/, 2> __PVT__watWriteIssueQueuePtr;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RenameLogicIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RenameLogicIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RenameLogicIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_RenameLogicIF* obj);

#endif  // guard
