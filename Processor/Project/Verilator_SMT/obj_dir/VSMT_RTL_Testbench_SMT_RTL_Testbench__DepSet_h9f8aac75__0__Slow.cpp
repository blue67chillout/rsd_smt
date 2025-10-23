// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_SMT_RTL_Testbench.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_static__TOP__SMT_RTL_Testbench(VSMT_RTL_Testbench_SMT_RTL_Testbench* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+      VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_static__TOP__SMT_RTL_Testbench\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__clk = 0U;
    vlSelfRef.__PVT__rst = 1U;
    vlSelfRef.__PVT__rstStart = 0U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_SMT_RTL_Testbench___ctor_var_reset(VSMT_RTL_Testbench_SMT_RTL_Testbench* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+      VSMT_RTL_Testbench_SMT_RTL_Testbench___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rstStart = VL_RAND_RESET_I(1);
    vlSelf->__PVT__unnamedblk1__DOT__thread0_x3 = VL_RAND_RESET_I(32);
    vlSelf->__PVT__unnamedblk1__DOT__thread1_x3 = VL_RAND_RESET_I(32);
}
