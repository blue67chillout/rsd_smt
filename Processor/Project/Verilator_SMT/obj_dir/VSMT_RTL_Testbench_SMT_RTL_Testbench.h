// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_SMT_RTL_TESTBENCH_H_
#define VERILATED_VSMT_RTL_TESTBENCH_SMT_RTL_TESTBENCH_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_Core;
class VSMT_RTL_Testbench_Memory__Iz1;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_SMT_RTL_Testbench final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_Core* core;
    VSMT_RTL_Testbench_Memory__Iz1* memory;

    // DESIGN SPECIFIC STATE
    CData/*0:0*/ __PVT__clk;
    CData/*0:0*/ __PVT__rst;
    CData/*0:0*/ __PVT__rstStart;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__thread0_x3;
    IData/*31:0*/ __PVT__unnamedblk1__DOT__thread1_x3;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_SMT_RTL_Testbench(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_SMT_RTL_Testbench();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_SMT_RTL_Testbench);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
