// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DebugIF.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_DebugIF___ctor_var_reset(VSMT_RTL_Testbench_DebugIF* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_DebugIF___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__npReg[__Vi0] = VL_RAND_RESET_I(11);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ifReg[__Vi0] = VL_RAND_RESET_I(13);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pdReg[__Vi0] = VL_RAND_RESET_I(18);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(81, vlSelf->__PVT__idReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rnReg[__Vi0] = VL_RAND_RESET_I(13);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(86, vlSelf->__PVT__dsReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intIsReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(118, vlSelf->__PVT__intExReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRwReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexIsReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRrReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(138, vlSelf->__PVT__complexExReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRwReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memIsReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(116, vlSelf->__PVT__memExReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(274, vlSelf->__PVT__mtReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(175, vlSelf->__PVT__maReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRwReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpIsReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(198, vlSelf->__PVT__fpExReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRwReg[__Vi0] = VL_RAND_RESET_I(14);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__cmReg[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__scheduler[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__issueQueue[__Vi0] = VL_RAND_RESET_I(13);
    }
    vlSelf->__PVT__lastCommittedPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__recover = VL_RAND_RESET_I(1);
    vlSelf->__PVT__toRecoveryPhase = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStagePipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__ifStagePipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__pdStagePipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__idStagePipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__rnStagePipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__dsStagePipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__backEndPipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__cmStagePipeCtrl = VL_RAND_RESET_I(2);
    vlSelf->__PVT__stallByDecodeStage = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(224, vlSelf->__PVT__perfCounter);
}
