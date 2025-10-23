// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_RECOVERYMANAGERIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_RECOVERYMANAGERIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RecoveryManagerIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*2:0*/ __PVT__refetchTypeFromCommitStage;
    CData/*2:0*/ __PVT__refetchTypeFromRwStage;
    CData/*0:0*/ __PVT__recoveryOpIndex;
    CData/*0:0*/ __PVT__exceptionDetectedInCommitStage;
    CData/*0:0*/ __PVT__exceptionDetectedInRwStage;
    CData/*0:0*/ __PVT__toCommitPhase;
    CData/*0:0*/ __PVT__flushAllInsns;
    CData/*0:0*/ __PVT__issueQueueReturnIndex;
    CData/*0:0*/ __PVT__inRecoveryAL;
    CData/*0:0*/ __PVT__renameLogicRecoveryRMT;
    CData/*0:0*/ __PVT__unableToStartRecovery;
    CData/*3:0*/ __PVT__recoveryCauseFromCommitStage;
    SData/*15:0*/ __PVT__flushIQ_Entry;
    IData/*31:0*/ __PVT__recoveredPC_FromCommitStage;
    IData/*31:0*/ __PVT__recoveredPC_FromRwStage;
    IData/*31:0*/ __PVT__recoveredPC_FromRwCommit;
    VlUnpacked<CData/*0:0*/, 6> __PVT__selected;
    VlUnpacked<CData/*3:0*/, 6> __PVT__selectedPtr;
    VlUnpacked<CData/*5:0*/, 6> __PVT__selectedActiveListPtr;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RecoveryManagerIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RecoveryManagerIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RecoveryManagerIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_RecoveryManagerIF* obj);

#endif  // guard
