// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_NEXTPCSTAGEIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_NEXTPCSTAGEIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_NextPCStageIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    CData/*0:0*/ __PVT__pcWE;
    CData/*0:0*/ __PVT__interruptAddrWE;
    IData/*19:0*/ __PVT__pcOut;
    IData/*19:0*/ __PVT__predNextPC;
    IData/*19:0*/ __PVT__interruptAddrIn;
    IData/*21:0*/ __PVT__icNextReadAddrIn;
    VlUnpacked<IData/*19:0*/, 2> __PVT__pcIn;
    VlUnpacked<QData/*56:0*/, 2> __PVT__brResult;
    VlUnpacked<IData/*30:0*/, 2> __PVT__nextStage;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_NextPCStageIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_NextPCStageIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_NextPCStageIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_NextPCStageIF* obj);

#endif  // guard
