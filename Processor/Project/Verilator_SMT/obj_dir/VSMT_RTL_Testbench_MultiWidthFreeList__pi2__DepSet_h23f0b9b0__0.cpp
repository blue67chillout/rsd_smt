// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__pi2.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__1(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__popCount = 0U;
    vlSelfRef.__PVT__poppedData[0U] = vlSelfRef.__PVT__rv
        [0U];
    if (vlSelfRef.__PVT__pop[0U]) {
        vlSelfRef.__PVT__popCount = (3U & ((IData)(1U) 
                                           + (IData)(vlSelfRef.__PVT__popCount)));
    }
    vlSelfRef.__PVT__poppedData[1U] = vlSelfRef.__PVT__rv
        [(1U & (IData)(vlSelfRef.__PVT__popCount))];
    if (vlSelfRef.__PVT__pop[1U]) {
        vlSelfRef.__PVT__popCount = (3U & ((IData)(1U) 
                                           + (IData)(vlSelfRef.__PVT__popCount)));
    }
    vlSelfRef.__PVT__queuePointer__DOT__nextHead = vlSelfRef.__PVT__queuePointer__DOT__regHead;
    vlSelfRef.__PVT__queuePointer__DOT__nextCount = vlSelfRef.__PVT__queuePointer__DOT__regCount;
    if ((0U != (IData)(vlSelfRef.__PVT__pushCount))) {
        vlSelfRef.__PVT__queuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextCount) 
                        + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    if ((0U != (IData)(vlSelfRef.__PVT__popCount))) {
        vlSelfRef.__PVT__queuePointer__DOT__nextHead 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextHead) 
                        + (IData)(vlSelfRef.__PVT__popCount)));
        vlSelfRef.__PVT__queuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextCount) 
                        - (IData)(vlSelfRef.__PVT__popCount)));
    }
}
