// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_BTB.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_BTB___ctor_var_reset(VSMT_RTL_Testbench_BTB* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_BTB___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__btbWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__btbWA[__Vi0] = VL_RAND_RESET_I(10);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__btbWV[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__btbRA[__Vi0] = VL_RAND_RESET_I(10);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__btbRV[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__btbOut[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__btbHit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__readIsCondBr[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__pcIn = VL_RAND_RESET_I(20);
    vlSelf->__PVT__tagReg = VL_RAND_RESET_Q(40);
    vlSelf->__PVT__nextTagReg = VL_RAND_RESET_Q(40);
    vlSelf->__PVT__pushBtbQueue = VL_RAND_RESET_I(1);
    vlSelf->__PVT__popBtbQueue = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 32; ++__Vi0) {
        vlSelf->__PVT__btbQueue[__Vi0] = VL_RAND_RESET_Q(52);
    }
    vlSelf->__PVT__updateBtb = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellout__btbEntryArray__rv[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__btbEntryArray__wv[__Vi0] = VL_RAND_RESET_I(20);
    }
    vlSelf->__PVT__resetIndex = VL_RAND_RESET_I(10);
    vlSelf->__PVT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__btbQueuePointer__DOT__regHeadStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__btbQueuePointer__DOT__nextHeadStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__btbQueuePointer__DOT__regTailStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__btbQueuePointer__DOT__nextTailStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__btbQueuePointer__DOT__regCount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__btbQueuePointer__DOT__nextCount = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_ToBTB_Tag__1__Vfuncout = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_ToBTB_Tag__1__pc = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToBTB_Tag__1__tag = VL_RAND_RESET_I(4);
}
