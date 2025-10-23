// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MemoryTypes.h"
#include "VSMT_RTL_Testbench__Syms.h"

// Parameter definitions for VSMT_RTL_Testbench_MemoryTypes
constexpr IData/*31:0*/ VSMT_RTL_Testbench_MemoryTypes::MEMORY_ENTRY_BIT_NUM;
constexpr IData/*31:0*/ VSMT_RTL_Testbench_MemoryTypes::MEMORY_ENTRY_NUM;


void VSMT_RTL_Testbench_MemoryTypes___ctor_var_reset(VSMT_RTL_Testbench_MemoryTypes* vlSelf);

VSMT_RTL_Testbench_MemoryTypes::VSMT_RTL_Testbench_MemoryTypes(VSMT_RTL_Testbench__Syms* symsp, const char* v__name)
    : VerilatedModule{v__name}
    , vlSymsp{symsp}
 {
    // Reset structure values
    VSMT_RTL_Testbench_MemoryTypes___ctor_var_reset(this);
}

void VSMT_RTL_Testbench_MemoryTypes::__Vconfigure(bool first) {
    (void)first;  // Prevent unused variable warning
}

VSMT_RTL_Testbench_MemoryTypes::~VSMT_RTL_Testbench_MemoryTypes() {
}
