// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MemoryIssueStageIF.h"
#include "VSMT_RTL_Testbench__Syms.h"

void VSMT_RTL_Testbench_MemoryIssueStageIF___ctor_var_reset(VSMT_RTL_Testbench_MemoryIssueStageIF* vlSelf);

VSMT_RTL_Testbench_MemoryIssueStageIF::VSMT_RTL_Testbench_MemoryIssueStageIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name)
    : VerilatedModule{v__name}
    , vlSymsp{symsp}
 {
    // Reset structure values
    VSMT_RTL_Testbench_MemoryIssueStageIF___ctor_var_reset(this);
}

void VSMT_RTL_Testbench_MemoryIssueStageIF::__Vconfigure(bool first) {
    (void)first;  // Prevent unused variable warning
}

VSMT_RTL_Testbench_MemoryIssueStageIF::~VSMT_RTL_Testbench_MemoryIssueStageIF() {
}
