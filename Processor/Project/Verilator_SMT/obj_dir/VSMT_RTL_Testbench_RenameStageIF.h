// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_RENAMESTAGEIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_RENAMESTAGEIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RenameStageIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    VlUnpacked<VlWide<7>/*214:0*/, 2> __PVT__nextStage;
    VlUnpacked<IData/*19:0*/, 2> __PVT__pc;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memDependencyPred;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RenameStageIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RenameStageIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RenameStageIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_RenameStageIF* obj);

#endif  // guard
