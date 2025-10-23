// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___stl_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___stl_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__ra[0U] = vlSelfRef.__PVT__queuePointer__DOT__regHead;
    vlSelfRef.__PVT__ra[1U] = (0xfU & ((0x10U <= ((IData)(1U) 
                                                  + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead)))
                                        ? ((IData)(1U) 
                                           + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead))
                                        : ((IData)(1U) 
                                           + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__ra[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__ra[1U] 
        = vlSelfRef.__PVT__ra[1U];
}
