// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_ActiveList.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_ActiveList___ctor_var_reset(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->headPtr = VL_RAND_RESET_I(6);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__headPtrList[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__tailPtrList[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__readPtrList[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pushedTailPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->__PVT__pushNum = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pushTail[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pushedTailData[__Vi0] = VL_RAND_RESET_Q(63);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__readData[__Vi0] = VL_RAND_RESET_Q(63);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellout__activeList__rv[__Vi0] = VL_RAND_RESET_Q(63);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__activeList__wv[__Vi0] = VL_RAND_RESET_Q(63);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        VL_RAND_RESET_W(72, vlSelf->__PVT__writeData[__Vi0]);
    }
    vlSelf->__PVT__oldestAge = VL_RAND_RESET_I(7);
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__writeAge[__Vi0] = VL_RAND_RESET_I(7);
    }
    vlSelf->__PVT__exceptionDetected = VL_RAND_RESET_I(1);
    vlSelf->__PVT__refetchType = VL_RAND_RESET_I(3);
    vlSelf->__PVT__exceptionIndex = VL_RAND_RESET_I(3);
    vlSelf->__PVT__startRecoveryAtCommit = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(71, vlSelf->__PVT__recoveryReg);
    VL_RAND_RESET_W(71, vlSelf->__PVT__nextRecoveryReg);
    vlSelf->__PVT__recoveryEntryNum = VL_RAND_RESET_I(7);
    vlSelf->__PVT__nextRecoveryEntryNum = VL_RAND_RESET_I(7);
    vlSelf->__PVT__flushRangeHeadPtr = VL_RAND_RESET_I(6);
    vlSelf->__PVT__flushRangeTailPtr = VL_RAND_RESET_I(6);
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__esWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__esWA[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__esRA[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__esWV[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__esRV[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__headExecState[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellout__execState__rv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__Vcellinp__execState__wv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 3; ++__Vi0) {
        vlSelf->__PVT__ffsWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 3; ++__Vi0) {
        vlSelf->__PVT__ffsWA[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ffsRA[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 3; ++__Vi0) {
        vlSelf->__PVT__ffsWV[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ffsRV[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellout__fflagsState__rv[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 3; ++__Vi0) {
        vlSelf->__Vcellinp__fflagsState__wv[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__esRefWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__esRefWA[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__esRefRA[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__esRefWV[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__esRefRV[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__execStateIsDifferentFromRef = VL_RAND_RESET_I(1);
    vlSelf->__PVT__regInRecovery = VL_RAND_RESET_I(1);
    vlSelf->__PVT__nextInRecovery = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__headExecStateRef[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__unnamedblk9__DOT__i = 0;
    vlSelf->__PVT__unnamedblk10__DOT__i = 0;
    vlSelf->__PVT__unnamedblk11__DOT__i = 0;
    vlSelf->__PVT__unnamedblk13__DOT__i = 0;
    vlSelf->__PVT__unnamedblk14__DOT__i = 0;
    vlSelf->__PVT__unnamedblk15__DOT__i = 0;
    vlSelf->__PVT__unnamedblk16__DOT__i = 0;
    vlSelf->__PVT__unnamedblk17__DOT__i = 0;
    vlSelf->__PVT__unnamedblk18__DOT__i = 0;
    vlSelf->__PVT__unnamedblk19__DOT__i = 0;
    vlSelf->__Vlvbound_h46781e09__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vlvbound_h0432b08f__0 = VL_RAND_RESET_I(6);
    vlSelf->__Vlvbound_h6be00a99__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vlvbound_h7dcaa4bf__0 = VL_RAND_RESET_I(6);
    vlSelf->__Vlvbound_hd8cbcf9b__0 = VL_RAND_RESET_I(5);
    vlSelf->__PVT__activeListPointer__DOT__regHead = VL_RAND_RESET_I(6);
    vlSelf->__PVT__activeListPointer__DOT__nextHead = VL_RAND_RESET_I(6);
    vlSelf->__PVT__activeListPointer__DOT__regTail = VL_RAND_RESET_I(6);
    vlSelf->__PVT__activeListPointer__DOT__nextTail = VL_RAND_RESET_I(6);
    vlSelf->__PVT__activeListPointer__DOT__regCount = VL_RAND_RESET_I(7);
    vlSelf->__PVT__activeListPointer__DOT__nextCount = VL_RAND_RESET_I(7);
}
