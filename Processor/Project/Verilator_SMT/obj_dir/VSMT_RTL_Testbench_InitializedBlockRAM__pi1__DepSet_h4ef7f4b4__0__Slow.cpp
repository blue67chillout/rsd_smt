// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_InitializedBlockRAM__pi1.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_InitializedBlockRAM__pi1___ctor_var_reset(VSMT_RTL_Testbench_InitializedBlockRAM__pi1* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_InitializedBlockRAM__pi1___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__we = VL_RAND_RESET_I(1);
    vlSelf->__PVT__wa = VL_RAND_RESET_I(22);
    vlSelf->__PVT__wv = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__ra = VL_RAND_RESET_I(22);
    vlSelf->__PVT__rv = VL_RAND_RESET_Q(64);
}
