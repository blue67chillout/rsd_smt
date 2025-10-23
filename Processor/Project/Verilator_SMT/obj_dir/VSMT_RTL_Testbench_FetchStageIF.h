// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_FETCHSTAGEIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_FETCHSTAGEIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_FetchStageIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    IData/*21:0*/ __PVT__icReadAddrIn;
    VlUnpacked<CData/*0:0*/, 2> __PVT__fetchStageIsValid;
    VlUnpacked<IData/*19:0*/, 2> __PVT__fetchStagePC;
    VlUnpacked<IData/*19:0*/, 2> __PVT__btbOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__btbHit;
    VlUnpacked<CData/*0:0*/, 2> __PVT__readIsCondBr;
    VlUnpacked<CData/*0:0*/, 2> __PVT__updateBrHistory;
    VlUnpacked<CData/*0:0*/, 2> __PVT__brPredTaken;
    VlUnpacked<SData/*9:0*/, 2> __PVT__brGlobalHistory;
    VlUnpacked<CData/*1:0*/, 2> __PVT__phtPrevValue;
    VlUnpacked<CData/*0:0*/, 2> __PVT__icReadHit;
    VlUnpacked<IData/*31:0*/, 2> __PVT__icReadDataOut;
    VlUnpacked<VlWide<3>/*95:0*/, 2> __PVT__nextStage;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_FetchStageIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_FetchStageIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_FetchStageIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_FetchStageIF* obj);

#endif  // guard
