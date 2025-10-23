// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_FPISSUESTAGEIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_FPISSUESTAGEIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_FPIssueStageIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VlUnpacked<VlWide<4>/*106:0*/, 1> __PVT__nextStage;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_FPIssueStageIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_FPIssueStageIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_FPIssueStageIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_FPIssueStageIF* obj);

#endif  // guard
