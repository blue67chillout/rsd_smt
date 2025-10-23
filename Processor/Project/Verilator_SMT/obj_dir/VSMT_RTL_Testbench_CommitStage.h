// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_COMMITSTAGE_H_
#define VERILATED_VSMT_RTL_TESTBENCH_COMMITSTAGE_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_CommitStage final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    CData/*0:0*/ __PVT__toRecoveryPhase;
    CData/*0:0*/ __PVT__recoveryOpIndex;
    CData/*2:0*/ __PVT__refetchType;
    CData/*3:0*/ __PVT__recoveryCause;
    CData/*1:0*/ __PVT__commitNum;
    CData/*1:0*/ __PVT__commitLoadNum;
    CData/*1:0*/ __PVT__commitStoreNum;
    CData/*0:0*/ __PVT__fflagsWE;
    CData/*4:0*/ __PVT__fflagsData;
    CData/*1:0*/ __PVT__phase;
    IData/*19:0*/ __PVT__lastCommittedPC;
    IData/*19:0*/ __PVT__prevLastCommittedPC;
    IData/*31:0*/ __PVT__cycles;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk2__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk3__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk5__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk6__DOT__i;
    VlUnpacked<CData/*0:0*/, 2> commit;
    VlUnpacked<CData/*0:0*/, 2> __PVT__last;
    VlUnpacked<CData/*0:0*/, 2> __PVT__isBranch;
    VlUnpacked<CData/*0:0*/, 2> __PVT__isStore;
    VlUnpacked<QData/*62:0*/, 2> alReadData;
    VlUnpacked<CData/*3:0*/, 2> __PVT__execState;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__commit;
    VlUnpacked<CData/*3:0*/, 2> __Vtask_DecideCommit__0__execState;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__isBranch;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__isStore;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__last;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__headOfThisInsn;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__tailOfThisInsn;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__recovery;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_DecideCommit__0__recoveryPoint;
    VlUnpacked<CData/*2:0*/, 2> __Vtask_DecideCommit__0__opRefetchType;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_GetInsnPtr__1__headOfThisInsn;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_GetInsnPtr__1__tailOfThisInsn;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_GetInsnPtr__1__last;
    VlUnpacked<CData/*3:0*/, 2> __Vtask_GetFinishedOpNum__2__execState;
    VlUnpacked<CData/*0:0*/, 2> __Vtask_GetFinishedInsnRange__3__last;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_CommitStage(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_CommitStage();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_CommitStage);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
