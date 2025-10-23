// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39___ctor_var_reset(VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                      VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wv[__Vi0] = VL_RAND_RESET_Q(38);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ra[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rv[__Vi0] = VL_RAND_RESET_Q(38);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->debugValue[__Vi0] = VL_RAND_RESET_Q(38);
    }
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_Q(38);
    }
    vlSelf->__PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_Q(38);
    }
    vlSelf->__PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
}
