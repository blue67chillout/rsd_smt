// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_CommitStage.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_CommitStage___ctor_var_reset(VSMT_RTL_Testbench_CommitStage* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_CommitStage___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__toRecoveryPhase = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->commit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__last[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__isBranch[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__isStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->alReadData[__Vi0] = VL_RAND_RESET_Q(63);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__execState[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__recoveryOpIndex = VL_RAND_RESET_I(1);
    vlSelf->__PVT__refetchType = VL_RAND_RESET_I(3);
    vlSelf->__PVT__recoveryCause = VL_RAND_RESET_I(4);
    vlSelf->__PVT__commitNum = VL_RAND_RESET_I(2);
    vlSelf->__PVT__commitLoadNum = VL_RAND_RESET_I(2);
    vlSelf->__PVT__commitStoreNum = VL_RAND_RESET_I(2);
    vlSelf->__PVT__fflagsWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fflagsData = VL_RAND_RESET_I(5);
    vlSelf->__PVT__phase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__lastCommittedPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__prevLastCommittedPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__cycles = VL_RAND_RESET_I(32);
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__unnamedblk6__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__commit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__execState[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__isBranch[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__isStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__last[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__headOfThisInsn[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__tailOfThisInsn[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__recovery[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__recoveryPoint[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__opRefetchType[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_GetInsnPtr__1__headOfThisInsn[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_GetInsnPtr__1__tailOfThisInsn[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_GetInsnPtr__1__last[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_GetFinishedOpNum__2__execState[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_GetFinishedInsnRange__3__last[__Vi0] = VL_RAND_RESET_I(1);
    }
}
