// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MemoryRegisterReadStageIF.h"

std::string VL_TO_STRING(const VSMT_RTL_Testbench_MemoryRegisterReadStageIF* obj) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MemoryRegisterReadStageIF::VL_TO_STRING\n"); );
    // Body
    return (obj ? obj->name() : "null");
}
