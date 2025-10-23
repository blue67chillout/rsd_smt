// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__0(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__ra[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__ra[1U] 
        = vlSelfRef.__PVT__ra[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__1(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__rv
        [0U];
    vlSelfRef.__PVT__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__rv
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__0(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__wa[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__we[0U] 
        = vlSelfRef.__PVT__we[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__wv[0U] 
        = vlSelfRef.__PVT__wv[0U];
}
