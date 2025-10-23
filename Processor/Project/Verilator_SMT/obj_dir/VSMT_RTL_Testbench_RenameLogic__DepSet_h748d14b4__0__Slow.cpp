// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RenameLogic.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_RenameLogic___ctor_var_reset(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__allocatePhyReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__allocatedPhyRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__allocatePhyScalarReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__allocatedPhyScalarRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__releasePhyScalarReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__releasedPhyScalarRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    vlSelf->__PVT__scalarFreeListCount = VL_RAND_RESET_I(6);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__renameTid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__commitTid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__allocatePhyScalarFPReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__allocatedPhyScalarFPRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__releasePhyScalarFPReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__releasedPhyScalarFPRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__alReadData[__Vi0] = VL_RAND_RESET_Q(63);
    }
    vlSelf->__PVT__rmtRecoveryIndex = VL_RAND_RESET_I(6);
    vlSelf->__PVT__rmtRecoveryCount = VL_RAND_RESET_I(7);
    vlSelf->__PVT__inRecoveryRMT = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rmtWriteReg = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rmtWriteReg_PhyRegNum[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rmtWriteReg_LogRegNum[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__watWriteReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__watWriteLogRegNum[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__watWriteIssueQueuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__retRMT_ReadReg_LogRegNum[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->__PVT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__unnamedblk10__DOT__i = 0;
    vlSelf->__PVT__unnamedblk13__DOT__i = 0;
    vlSelf->__PVT__unnamedblk11__DOT__i = 0;
    vlSelf->__PVT__unnamedblk12__DOT__i = 0;
}
