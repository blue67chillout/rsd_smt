// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15___ctor_var_reset(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__PVT__ra[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__PVT__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->debugValue[__Vi0] = VL_RAND_RESET_I(8);
    }
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
}
