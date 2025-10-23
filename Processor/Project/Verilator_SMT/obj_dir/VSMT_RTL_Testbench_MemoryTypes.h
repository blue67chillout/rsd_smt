// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_MEMORYTYPES_H_
#define VERILATED_VSMT_RTL_TESTBENCH_MEMORYTYPES_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_MemoryTypes final : public VerilatedModule {
  public:

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // PARAMETERS
    static constexpr IData/*31:0*/ MEMORY_ENTRY_BIT_NUM = 0x00000040U;
    static constexpr IData/*31:0*/ MEMORY_ENTRY_NUM = 0x00400000U;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_MemoryTypes(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_MemoryTypes();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_MemoryTypes);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
