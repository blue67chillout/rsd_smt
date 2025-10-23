// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_PERFORMANCECOUNTERIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_PERFORMANCECOUNTERIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_PerformanceCounterIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*0:0*/ __PVT__icMiss;
    CData/*0:0*/ __PVT__branchPredMissDetectedOnDecode;
    VlUnpacked<CData/*0:0*/, 1> __PVT__loadMiss;
    VlUnpacked<CData/*0:0*/, 1> __PVT__storeMiss;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_PerformanceCounterIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_PerformanceCounterIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_PerformanceCounterIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_PerformanceCounterIF* obj);

#endif  // guard
