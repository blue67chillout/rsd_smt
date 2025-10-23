// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__pi2.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___ctor_var_reset(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rstStart = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__push[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pop[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pushedData[__Vi0] = VL_RAND_RESET_I(7);
    }
    vlSelf->__PVT__count = VL_RAND_RESET_I(6);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__poppedData[__Vi0] = VL_RAND_RESET_I(7);
    }
    vlSelf->__PVT__pushCount = VL_RAND_RESET_I(2);
    vlSelf->__PVT__popCount = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wv[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wa[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rv[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ra[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__rstIndex = VL_RAND_RESET_I(5);
    vlSelf->__PVT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__queuePointer__DOT__regHead = VL_RAND_RESET_I(5);
    vlSelf->__PVT__queuePointer__DOT__nextHead = VL_RAND_RESET_I(5);
    vlSelf->__PVT__queuePointer__DOT__regTail = VL_RAND_RESET_I(5);
    vlSelf->__PVT__queuePointer__DOT__nextTail = VL_RAND_RESET_I(5);
    vlSelf->__PVT__queuePointer__DOT__regCount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__queuePointer__DOT__nextCount = VL_RAND_RESET_I(6);
}
