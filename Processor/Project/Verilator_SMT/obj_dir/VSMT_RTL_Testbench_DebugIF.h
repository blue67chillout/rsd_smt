// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DEBUGIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DEBUGIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DebugIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*0:0*/ __PVT__recover;
    CData/*0:0*/ __PVT__toRecoveryPhase;
    CData/*1:0*/ __PVT__npStagePipeCtrl;
    CData/*1:0*/ __PVT__ifStagePipeCtrl;
    CData/*1:0*/ __PVT__pdStagePipeCtrl;
    CData/*1:0*/ __PVT__idStagePipeCtrl;
    CData/*1:0*/ __PVT__rnStagePipeCtrl;
    CData/*1:0*/ __PVT__dsStagePipeCtrl;
    CData/*1:0*/ __PVT__backEndPipeCtrl;
    CData/*1:0*/ __PVT__cmStagePipeCtrl;
    CData/*0:0*/ __PVT__stallByDecodeStage;
    IData/*19:0*/ __PVT__lastCommittedPC;
    VlWide<7>/*223:0*/ __PVT__perfCounter;
    VlUnpacked<SData/*10:0*/, 2> __PVT__npReg;
    VlUnpacked<SData/*12:0*/, 2> __PVT__ifReg;
    VlUnpacked<IData/*17:0*/, 2> __PVT__pdReg;
    VlUnpacked<VlWide<3>/*80:0*/, 2> __PVT__idReg;
    VlUnpacked<SData/*12:0*/, 2> __PVT__rnReg;
    VlUnpacked<VlWide<3>/*85:0*/, 2> __PVT__dsReg;
    VlUnpacked<SData/*13:0*/, 2> __PVT__intIsReg;
    VlUnpacked<SData/*13:0*/, 2> __PVT__intRrReg;
    VlUnpacked<VlWide<4>/*117:0*/, 2> __PVT__intExReg;
    VlUnpacked<SData/*13:0*/, 2> __PVT__intRwReg;
    VlUnpacked<SData/*13:0*/, 1> __PVT__complexIsReg;
    VlUnpacked<SData/*13:0*/, 1> __PVT__complexRrReg;
    VlUnpacked<VlWide<5>/*137:0*/, 1> __PVT__complexExReg;
    VlUnpacked<SData/*13:0*/, 1> __PVT__complexRwReg;
    VlUnpacked<SData/*13:0*/, 2> __PVT__memIsReg;
    VlUnpacked<SData/*13:0*/, 2> __PVT__memRrReg;
    VlUnpacked<VlWide<4>/*115:0*/, 2> __PVT__memExReg;
    VlUnpacked<VlWide<9>/*273:0*/, 2> __PVT__mtReg;
    VlUnpacked<VlWide<6>/*174:0*/, 2> __PVT__maReg;
    VlUnpacked<SData/*13:0*/, 2> __PVT__memRwReg;
    VlUnpacked<SData/*13:0*/, 1> __PVT__fpIsReg;
    VlUnpacked<SData/*13:0*/, 1> __PVT__fpRrReg;
    VlUnpacked<VlWide<7>/*197:0*/, 1> __PVT__fpExReg;
    VlUnpacked<SData/*13:0*/, 1> __PVT__fpRwReg;
    VlUnpacked<IData/*21:0*/, 2> __PVT__cmReg;
    VlUnpacked<CData/*0:0*/, 16> __PVT__scheduler;
    VlUnpacked<SData/*12:0*/, 16> __PVT__issueQueue;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DebugIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DebugIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DebugIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_DebugIF* obj);

#endif  // guard
