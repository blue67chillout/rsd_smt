// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DUMPERTYPES_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DUMPERTYPES_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DumperTypes final : public VerilatedModule {
  public:

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DumperTypes(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DumperTypes();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DumperTypes);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
