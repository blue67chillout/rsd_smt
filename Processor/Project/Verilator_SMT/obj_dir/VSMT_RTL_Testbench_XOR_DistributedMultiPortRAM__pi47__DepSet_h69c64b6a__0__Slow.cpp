// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47___ctor_var_reset(VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                        VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__wa[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__wv[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ra[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rv[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__rwbWriteValue[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__wbReadAddr[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 8; ++__Vi1) {
            vlSelf->__PVT__wbReadValue[__Vi0][__Vi1] = VL_RAND_RESET_I(3);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rbReadAddr[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__rbReadValue[__Vi0][__Vi1] = VL_RAND_RESET_I(3);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__debugValue[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__unnamedblk7__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__0__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__1__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__2__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__3__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__4__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__5__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__6__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wj__BRA__7__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__2__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__2__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__2__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__2__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__2__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__2__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__3__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__3__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__3__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__3__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__3__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__3__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__4__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__4__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__4__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__4__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__4__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__4__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__5__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__5__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__5__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__5__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__5__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__5__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__6__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__6__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__6__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__6__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__6__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__6__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__7__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__7__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__7__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 64; ++__Vi0) {
        vlSelf->__PVT__rj__BRA__7__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__rj__BRA__7__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rj__BRA__7__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
}
