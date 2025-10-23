// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_ControllerIF.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_ControllerIF___ctor_var_reset(VSMT_RTL_Testbench_ControllerIF* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_ControllerIF___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__ifStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__pdStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__idStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__rnStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__dsStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__scStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__isStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__backEnd = VL_RAND_RESET_I(2);
    vlSelf->__PVT__cmStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__ifStageEmpty = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pdStageEmpty = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStageEmpty = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rnStageEmpty = VL_RAND_RESET_I(1);
    vlSelf->__PVT__wholePipelineEmpty = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStageSendBubbleLowerForInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__ifStageSendBubbleLower = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStageStallUpper = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rnStageSendBubbleLower = VL_RAND_RESET_I(1);
    vlSelf->__PVT__isStageStallUpper = VL_RAND_RESET_I(1);
    vlSelf->__PVT__stallByDecodeStage = VL_RAND_RESET_I(1);
}
