// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_RENAMELOGIC_H_
#define VERILATED_VSMT_RTL_TESTBENCH_RENAMELOGIC_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_MultiWidthFreeList__pi2;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RenameLogic final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_MultiWidthFreeList__pi2* scalarFPFreeList;
    VSMT_RTL_Testbench_MultiWidthFreeList__pi2* genblk1__BRA__0__KET____DOT__scalarFreeList;
    VSMT_RTL_Testbench_MultiWidthFreeList__pi2* genblk1__BRA__1__KET____DOT__scalarFreeList;

    // DESIGN SPECIFIC STATE
    CData/*5:0*/ __PVT__scalarFreeListCount;
    CData/*5:0*/ __PVT__rmtRecoveryIndex;
    CData/*6:0*/ __PVT__rmtRecoveryCount;
    CData/*0:0*/ __PVT__inRecoveryRMT;
    CData/*1:0*/ __PVT__rmtWriteReg;
    IData/*31:0*/ __PVT__unnamedblk6__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk7__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk10__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk13__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk11__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk12__DOT__i;
    VlUnpacked<CData/*0:0*/, 2> __PVT__allocatePhyReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__allocatedPhyRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__allocatePhyScalarReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__allocatedPhyScalarRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__releasePhyScalarReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__releasedPhyScalarRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__renameTid;
    VlUnpacked<CData/*0:0*/, 2> __PVT__commitTid;
    VlUnpacked<CData/*0:0*/, 2> __PVT__allocatePhyScalarFPReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__allocatedPhyScalarFPRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__releasePhyScalarFPReg;
    VlUnpacked<CData/*6:0*/, 2> __PVT__releasedPhyScalarFPRegNum;
    VlUnpacked<QData/*62:0*/, 2> __PVT__alReadData;
    VlUnpacked<CData/*6:0*/, 2> __PVT__rmtWriteReg_PhyRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__rmtWriteReg_LogRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__watWriteReg;
    VlUnpacked<CData/*5:0*/, 2> __PVT__watWriteLogRegNum;
    VlUnpacked<CData/*3:0*/, 2> __PVT__watWriteIssueQueuePtr;
    VlUnpacked<CData/*5:0*/, 2> __PVT__retRMT_ReadReg_LogRegNum;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RenameLogic(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RenameLogic();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RenameLogic);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
