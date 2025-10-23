// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_CacheSystemIF.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_CacheSystemIF___ctor_var_reset(VSMT_RTL_Testbench_CacheSystemIF* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_CacheSystemIF___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__icMemAccessReq = VL_RAND_RESET_I(23);
    vlSelf->__PVT__icMemAccessReqAck = VL_RAND_RESET_I(4);
    VL_RAND_RESET_W(67, vlSelf->__PVT__icMemAccessResult);
    VL_RAND_RESET_W(88, vlSelf->__PVT__dcMemAccessReq);
    vlSelf->__PVT__dcMemAccessReqAck = VL_RAND_RESET_I(4);
    vlSelf->__PVT__icFlushComplete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__icFlushReq = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dcFlushReq = VL_RAND_RESET_I(1);
    vlSelf->__PVT__flushComplete = VL_RAND_RESET_I(1);
}
