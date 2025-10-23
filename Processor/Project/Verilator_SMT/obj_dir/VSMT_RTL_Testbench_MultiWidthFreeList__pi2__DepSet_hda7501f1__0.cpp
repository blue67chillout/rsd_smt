// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__pi2.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rv[0U] = vlSelf->freeList->__PVT__rv
        [0U];
    vlSelfRef.__PVT__rv[1U] = vlSelf->freeList->__PVT__rv
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__1(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->freeList->__PVT__ra[0U] = vlSelfRef.__PVT__ra
        [0U];
    vlSelf->freeList->__PVT__ra[1U] = vlSelfRef.__PVT__ra
        [1U];
}
