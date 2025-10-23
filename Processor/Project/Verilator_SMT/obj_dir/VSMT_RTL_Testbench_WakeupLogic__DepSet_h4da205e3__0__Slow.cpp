// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_WakeupLogic.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_WakeupLogic___ctor_var_reset(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__PVT__dispatchedSrcRegValid[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__PVT__dispatchedSrcRegNum[__Vi0][__Vi1] = VL_RAND_RESET_I(7);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dispatchedDstRegValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dispatchedDstRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__PVT__wakeupDstRegValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__PVT__wakeupDstRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__PVT__dispatchedSrcRegReady[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__wakeupDstVector[__Vi0] = VL_RAND_RESET_I(16);
    }
    vlSelf->__PVT__dispatchedSrcRegPtr = VL_RAND_RESET_I(24);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__opMatrixReady[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dependStoreBitVector[__Vi0] = VL_RAND_RESET_I(16);
    }
    vlSelf->__PVT__storeBitVector = VL_RAND_RESET_I(16);
    vlSelf->__PVT__storeBitVectorReg = VL_RAND_RESET_I(16);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dispatchStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dispatchLoad[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__notIssued = VL_RAND_RESET_I(16);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memDependencyPred[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum[__Vi0][__Vi1] = VL_RAND_RESET_I(7);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__regReadyBitTbl__dispatchedDstRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__regReadyBitTbl__dispatch[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__Vcellinp__regReadyBitTbl__wakeupDstRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__Vcellinp__regReadyBitTbl__wakeup[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__producerMatrix__dispatchPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__producerMatrix__dispatch[__Vi0] = VL_RAND_RESET_I(1);
    }
    VL_RAND_RESET_W(256, vlSelf->__PVT__producerMatrix__DOT__nextMatrix);
    VL_RAND_RESET_W(256, vlSelf->__PVT__producerMatrix__DOT__matrix);
    vlSelf->__PVT__producerMatrix__DOT__dispatchVector = VL_RAND_RESET_I(32);
    vlSelf->__PVT__producerMatrix__DOT__wakeupVector = VL_RAND_RESET_I(16);
}
