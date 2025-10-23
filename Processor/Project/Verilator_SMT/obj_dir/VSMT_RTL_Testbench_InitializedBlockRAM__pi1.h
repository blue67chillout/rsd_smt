// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_INITIALIZEDBLOCKRAM__PI1_H_
#define VERILATED_VSMT_RTL_TESTBENCH_INITIALIZEDBLOCKRAM__PI1_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_InitializedBlockRAM__pi1 final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20* body__DOT__ram;

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__we,0,0);
    VL_IN(__PVT__wa,21,0);
    VL_IN(__PVT__ra,21,0);
    VL_IN64(__PVT__wv,63,0);
    VL_OUT64(__PVT__rv,63,0);

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_InitializedBlockRAM__pi1(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_InitializedBlockRAM__pi1();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_InitializedBlockRAM__pi1);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
