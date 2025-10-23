// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RecoveryManagerIF.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_RecoveryManagerIF___ctor_var_reset(VSMT_RTL_Testbench_RecoveryManagerIF* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_RecoveryManagerIF___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__refetchTypeFromCommitStage = VL_RAND_RESET_I(3);
    vlSelf->__PVT__refetchTypeFromRwStage = VL_RAND_RESET_I(3);
    vlSelf->__PVT__recoveryOpIndex = VL_RAND_RESET_I(1);
    vlSelf->__PVT__exceptionDetectedInCommitStage = VL_RAND_RESET_I(1);
    vlSelf->__PVT__exceptionDetectedInRwStage = VL_RAND_RESET_I(1);
    vlSelf->__PVT__toCommitPhase = VL_RAND_RESET_I(1);
    vlSelf->__PVT__recoveredPC_FromCommitStage = VL_RAND_RESET_I(32);
    vlSelf->__PVT__recoveredPC_FromRwStage = VL_RAND_RESET_I(32);
    vlSelf->__PVT__recoveredPC_FromRwCommit = VL_RAND_RESET_I(32);
    vlSelf->__PVT__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__PVT__flushIQ_Entry = VL_RAND_RESET_I(16);
    vlSelf->__PVT__issueQueueReturnIndex = VL_RAND_RESET_I(1);
    vlSelf->__PVT__inRecoveryAL = VL_RAND_RESET_I(1);
    vlSelf->__PVT__renameLogicRecoveryRMT = VL_RAND_RESET_I(1);
    vlSelf->__PVT__unableToStartRecovery = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectedActiveListPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->__PVT__recoveryCauseFromCommitStage = VL_RAND_RESET_I(4);
}
