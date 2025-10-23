// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__pi2.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (VL_UNLIKELY(((1U & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                               | (0x20U >= (IData)(vlSelfRef.__PVT__queuePointer__DOT__regCount)))))))) {
        VL_WRITEF_NX("The count of a queue exceeds its size.\n",0);
    }
    vlSelfRef.__PVT__rstIndex = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart)
                                  ? 0U : ((0x1eU <= (IData)(vlSelfRef.__PVT__rstIndex))
                                           ? 0U : (0x1fU 
                                                   & ((IData)(2U) 
                                                      + (IData)(vlSelfRef.__PVT__rstIndex)))));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__queuePointer__DOT__regHead = 0U;
        vlSelfRef.__PVT__queuePointer__DOT__regTail = 0U;
        vlSelfRef.__PVT__queuePointer__DOT__regCount = 0x20U;
    } else {
        vlSelfRef.__PVT__queuePointer__DOT__regHead 
            = vlSelfRef.__PVT__queuePointer__DOT__nextHead;
        vlSelfRef.__PVT__queuePointer__DOT__regTail 
            = vlSelfRef.__PVT__queuePointer__DOT__nextTail;
        vlSelfRef.__PVT__queuePointer__DOT__regCount 
            = vlSelfRef.__PVT__queuePointer__DOT__nextCount;
    }
    vlSelfRef.__PVT__ra[0U] = vlSelfRef.__PVT__queuePointer__DOT__regHead;
    vlSelfRef.__PVT__ra[1U] = (0x1fU & ((0x20U <= ((IData)(1U) 
                                                   + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead)))
                                         ? ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead))
                                         : ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__2(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
    }
}
