// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_StoreQueue.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_StoreQueue___ctor_var_reset(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__headAddrEntry = VL_RAND_RESET_I(27);
    vlSelf->__PVT__headDataEntry = VL_RAND_RESET_Q(38);
    vlSelf->__PVT__releasedStoreQueuePtr = VL_RAND_RESET_I(4);
    vlSelf->__PVT__pushCount = VL_RAND_RESET_I(2);
    vlSelf->__PVT__push = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__storeQueue[__Vi0] = VL_RAND_RESET_I(27);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executeStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedStoreAddr[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedStoreWordWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedStoreByteWE[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedStoreCondEnabled[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedStoreRegValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedStoreQueuePtrByStore[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__forwardedDataEntry[__Vi0] = VL_RAND_RESET_Q(38);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedLoadWordRE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedLoadByteRE[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__addrMatch[__Vi0] = VL_RAND_RESET_I(16);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__pickedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__picked[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__executedStoreQueuePtrByLoad[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__sqReadPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__sqReadData[__Vi0] = VL_RAND_RESET_Q(38);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__sqWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__sqWriteData[__Vi0] = VL_RAND_RESET_Q(38);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__sqWriteStoreData[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellout__storeQueueData__rv[__Vi0] = VL_RAND_RESET_Q(38);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vcellinp__storeQueueData__wv[__Vi0] = VL_RAND_RESET_Q(38);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__storeLoadForwarded[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__forwardedLoadData[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__forwardMiss[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__unnamedblk4__DOT__i = 0;
    vlSelf->__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = VL_RAND_RESET_I(1);
    vlSelf->__Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr = VL_RAND_RESET_I(4);
    vlSelf->__PVT__storeQueuePointer__DOT__regHead = VL_RAND_RESET_I(4);
    vlSelf->__PVT__storeQueuePointer__DOT__nextHead = VL_RAND_RESET_I(4);
    vlSelf->__PVT__storeQueuePointer__DOT__regTail = VL_RAND_RESET_I(4);
    vlSelf->__PVT__storeQueuePointer__DOT__nextTail = VL_RAND_RESET_I(4);
    vlSelf->__PVT__storeQueuePointer__DOT__roundedSetTailPtr = VL_RAND_RESET_I(4);
    vlSelf->__PVT__storeQueuePointer__DOT__regCount = VL_RAND_RESET_I(5);
    vlSelf->__PVT__storeQueuePointer__DOT__nextCount = VL_RAND_RESET_I(5);
    vlSelf->__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq = VL_RAND_RESET_I(16);
    vlSelf->__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = VL_RAND_RESET_I(4);
    vlSelf->__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_LSQ_SelectBits__2__Vfuncout = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__2__data = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__2__offset = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__2__width = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__2__ret = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__6__Vfuncout = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__6__data = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__6__offset = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__6__width = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__6__ret = 0;
    vlSelf->__Vfunc_LSQ_ToBlockAddr__8__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_LSQ_ToBlockAddr__8__addr = VL_RAND_RESET_I(22);
}
