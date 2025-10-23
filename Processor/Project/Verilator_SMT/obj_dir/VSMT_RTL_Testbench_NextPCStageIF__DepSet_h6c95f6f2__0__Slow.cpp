// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_NextPCStageIF.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_NextPCStageIF___ctor_var_reset(VSMT_RTL_Testbench_NextPCStageIF* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_NextPCStageIF___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rstStart = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pcWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pcOut = VL_RAND_RESET_I(20);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pcIn[__Vi0] = VL_RAND_RESET_I(20);
    }
    vlSelf->__PVT__predNextPC = VL_RAND_RESET_I(20);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__brResult[__Vi0] = VL_RAND_RESET_Q(57);
    }
    vlSelf->__PVT__interruptAddrIn = VL_RAND_RESET_I(20);
    vlSelf->__PVT__interruptAddrWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__icNextReadAddrIn = VL_RAND_RESET_I(22);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__nextStage[__Vi0] = VL_RAND_RESET_I(31);
    }
}
