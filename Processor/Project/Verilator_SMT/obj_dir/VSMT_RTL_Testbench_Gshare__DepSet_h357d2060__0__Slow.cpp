// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Gshare.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_Gshare___ctor_var_reset(VSMT_RTL_Testbench_Gshare* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_Gshare___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__clear = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pcIn = VL_RAND_RESET_I(20);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__brPredTaken[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__updateHistory[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__phtWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__phtWA[__Vi0] = VL_RAND_RESET_I(11);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__phtWV[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__phtPrevValue[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__phtRA[__Vi0] = VL_RAND_RESET_I(11);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__phtRV[__Vi0] = VL_RAND_RESET_I(2);
    }
    vlSelf->__PVT__nextBrGlobalHistory = VL_RAND_RESET_I(10);
    vlSelf->__PVT__regBrGlobalHistory = VL_RAND_RESET_I(10);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__brGlobalHistory[__Vi0] = VL_RAND_RESET_I(10);
    }
    vlSelf->__PVT__mispred = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pushPhtQueue = VL_RAND_RESET_I(1);
    vlSelf->__PVT__popPhtQueue = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 32; ++__Vi0) {
        vlSelf->__PVT__phtQueue[__Vi0] = VL_RAND_RESET_Q(34);
    }
    vlSelf->__PVT__updatePht = VL_RAND_RESET_I(1);
    vlSelf->__PVT__resetIndex = VL_RAND_RESET_I(11);
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__phtQueuePointer__DOT__regHeadStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__phtQueuePointer__DOT__nextHeadStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__phtQueuePointer__DOT__regTailStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__phtQueuePointer__DOT__nextTailStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__phtQueuePointer__DOT__regCount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__phtQueuePointer__DOT__nextCount = VL_RAND_RESET_I(6);
}
