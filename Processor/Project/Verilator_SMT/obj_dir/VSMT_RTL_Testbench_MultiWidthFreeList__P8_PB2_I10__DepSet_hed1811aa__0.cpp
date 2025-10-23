// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__wv[0U] = vlSelfRef.__PVT__pushedData
        [0U];
    vlSelfRef.__PVT__wv[1U] = vlSelfRef.__PVT__pushedData
        [1U];
    vlSelfRef.__PVT__wv[2U] = vlSelfRef.__PVT__pushedData
        [2U];
    vlSelfRef.__PVT__wv[3U] = vlSelfRef.__PVT__pushedData
        [3U];
    vlSelfRef.__PVT__wv[4U] = vlSelfRef.__PVT__pushedData
        [4U];
    vlSelfRef.__PVT__wv[5U] = vlSelfRef.__PVT__pushedData
        [5U];
    vlSelfRef.__PVT__wv[6U] = vlSelfRef.__PVT__pushedData
        [6U];
    vlSelfRef.__PVT__wv[7U] = vlSelfRef.__PVT__pushedData
        [7U];
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__Vcellinp__issueQueueFreeList__rst) {
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 8U;
        vlSelfRef.__PVT__wv[0U] = vlSelfRef.__PVT__rstIndex;
        vlSelfRef.__PVT__wv[1U] = (0xfU & ((IData)(1U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wv[2U] = (0xfU & ((IData)(2U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wv[3U] = (0xfU & ((IData)(3U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wv[4U] = (0xfU & ((IData)(4U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wv[5U] = (0xfU & ((IData)(5U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wv[6U] = (0xfU & ((IData)(6U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wv[7U] = (0xfU & ((IData)(7U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[0U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[1U] 
        = vlSelfRef.__PVT__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[2U] 
        = vlSelfRef.__PVT__wv[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[3U] 
        = vlSelfRef.__PVT__wv[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[4U] 
        = vlSelfRef.__PVT__wv[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[5U] 
        = vlSelfRef.__PVT__wv[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[6U] 
        = vlSelfRef.__PVT__wv[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wv[7U] 
        = vlSelfRef.__PVT__wv[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__pushCount = 0U;
    vlSelfRef.__PVT__wa[0U] = vlSelfRef.__PVT__queuePointer__DOT__regTail;
    if (vlSelfRef.__PVT__push[0U]) {
        vlSelfRef.__PVT__we[0U] = 1U;
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    } else {
        vlSelfRef.__PVT__we[0U] = 0U;
    }
    vlSelfRef.__PVT__we[1U] = vlSelfRef.__PVT__push
        [1U];
    vlSelfRef.__PVT__wa[1U] = (0xfU & ((0x10U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                  + (IData)(vlSelfRef.__PVT__pushCount)))
                                        ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))
                                        : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))));
    if (vlSelfRef.__PVT__push[1U]) {
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelfRef.__PVT__we[2U] = vlSelfRef.__PVT__push
        [2U];
    vlSelfRef.__PVT__wa[2U] = (0xfU & ((0x10U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                  + (IData)(vlSelfRef.__PVT__pushCount)))
                                        ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))
                                        : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))));
    if (vlSelfRef.__PVT__push[2U]) {
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelfRef.__PVT__we[3U] = vlSelfRef.__PVT__push
        [3U];
    vlSelfRef.__PVT__wa[3U] = (0xfU & ((0x10U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                  + (IData)(vlSelfRef.__PVT__pushCount)))
                                        ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))
                                        : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))));
    if (vlSelfRef.__PVT__push[3U]) {
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelfRef.__PVT__we[4U] = vlSelfRef.__PVT__push
        [4U];
    vlSelfRef.__PVT__wa[4U] = (0xfU & ((0x10U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                  + (IData)(vlSelfRef.__PVT__pushCount)))
                                        ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))
                                        : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))));
    if (vlSelfRef.__PVT__push[4U]) {
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelfRef.__PVT__we[5U] = vlSelfRef.__PVT__push
        [5U];
    vlSelfRef.__PVT__wa[5U] = (0xfU & ((0x10U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                  + (IData)(vlSelfRef.__PVT__pushCount)))
                                        ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))
                                        : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))));
    if (vlSelfRef.__PVT__push[5U]) {
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelfRef.__PVT__we[6U] = vlSelfRef.__PVT__push
        [6U];
    vlSelfRef.__PVT__wa[6U] = (0xfU & ((0x10U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                  + (IData)(vlSelfRef.__PVT__pushCount)))
                                        ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))
                                        : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))));
    if (vlSelfRef.__PVT__push[6U]) {
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelfRef.__PVT__wa[7U] = (0xfU & ((0x10U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                  + (IData)(vlSelfRef.__PVT__pushCount)))
                                        ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))
                                        : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                           + (IData)(vlSelfRef.__PVT__pushCount))));
    if (vlSelfRef.__PVT__push[7U]) {
        vlSelfRef.__PVT__we[7U] = 1U;
        vlSelfRef.__PVT__pushCount = (0xfU & ((IData)(1U) 
                                              + (IData)(vlSelfRef.__PVT__pushCount)));
    } else {
        vlSelfRef.__PVT__we[7U] = 0U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__Vcellinp__issueQueueFreeList__rst) {
        vlSelfRef.__PVT__wa[0U] = vlSelfRef.__PVT__rstIndex;
        vlSelfRef.__PVT__we[0U] = (vlSelfRef.__PVT__wa
                                   [0U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[1U] = (0xfU & ((IData)(1U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[1U] = (vlSelfRef.__PVT__wa
                                   [1U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[2U] = (0xfU & ((IData)(2U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[2U] = (vlSelfRef.__PVT__wa
                                   [2U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[3U] = (0xfU & ((IData)(3U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[3U] = (vlSelfRef.__PVT__wa
                                   [3U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[4U] = (0xfU & ((IData)(4U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[4U] = (vlSelfRef.__PVT__wa
                                   [4U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[5U] = (0xfU & ((IData)(5U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[5U] = (vlSelfRef.__PVT__wa
                                   [5U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[6U] = (0xfU & ((IData)(6U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[6U] = (vlSelfRef.__PVT__wa
                                   [6U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[7U] = (0xfU & ((IData)(7U) 
                                           + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[7U] = (vlSelfRef.__PVT__wa
                                   [7U] >= (IData)(vlSelfRef.__PVT__rstIndex));
    }
    vlSelfRef.__PVT__queuePointer__DOT__nextTail = vlSelfRef.__PVT__queuePointer__DOT__regTail;
    if ((0U != (IData)(vlSelfRef.__PVT__pushCount))) {
        vlSelfRef.__PVT__queuePointer__DOT__nextTail 
            = (0xfU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextTail) 
                       + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[1U] 
        = vlSelfRef.__PVT__wa[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[2U] 
        = vlSelfRef.__PVT__wa[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[3U] 
        = vlSelfRef.__PVT__wa[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[4U] 
        = vlSelfRef.__PVT__wa[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[5U] 
        = vlSelfRef.__PVT__wa[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[6U] 
        = vlSelfRef.__PVT__wa[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[7U] 
        = vlSelfRef.__PVT__wa[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[0U] 
        = vlSelfRef.__PVT__we[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[1U] 
        = vlSelfRef.__PVT__we[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[2U] 
        = vlSelfRef.__PVT__we[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[3U] 
        = vlSelfRef.__PVT__we[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[4U] 
        = vlSelfRef.__PVT__we[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[5U] 
        = vlSelfRef.__PVT__we[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[6U] 
        = vlSelfRef.__PVT__we[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[7U] 
        = vlSelfRef.__PVT__we[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__1(VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__rv
        [0U];
    vlSelfRef.__PVT__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__rv
        [1U];
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
            = (0x1fU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextCount) 
                        + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    if ((0U != (IData)(vlSelfRef.__PVT__popCount))) {
        vlSelfRef.__PVT__queuePointer__DOT__nextHead 
            = (0xfU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextHead) 
                       + (IData)(vlSelfRef.__PVT__popCount)));
        vlSelfRef.__PVT__queuePointer__DOT__nextCount 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextCount) 
                        - (IData)(vlSelfRef.__PVT__popCount)));
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (VL_UNLIKELY(((1U & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__Vcellinp__issueQueueFreeList__rst) 
                               | (0x10U >= (IData)(vlSelfRef.__PVT__queuePointer__DOT__regCount)))))))) {
        VL_WRITEF_NX("The count of a queue exceeds its size.\n",0);
    }
    vlSelfRef.__PVT__rstIndex = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart)
                                  ? 0U : ((8U <= (IData)(vlSelfRef.__PVT__rstIndex))
                                           ? 0U : (0xfU 
                                                   & ((IData)(8U) 
                                                      + (IData)(vlSelfRef.__PVT__rstIndex)))));
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__Vcellinp__issueQueueFreeList__rst) {
        vlSelfRef.__PVT__queuePointer__DOT__regHead = 0U;
        vlSelfRef.__PVT__queuePointer__DOT__regTail = 0U;
        vlSelfRef.__PVT__queuePointer__DOT__regCount = 0x10U;
    } else {
        vlSelfRef.__PVT__queuePointer__DOT__regHead 
            = vlSelfRef.__PVT__queuePointer__DOT__nextHead;
        vlSelfRef.__PVT__queuePointer__DOT__regTail 
            = vlSelfRef.__PVT__queuePointer__DOT__nextTail;
        vlSelfRef.__PVT__queuePointer__DOT__regCount 
            = vlSelfRef.__PVT__queuePointer__DOT__nextCount;
    }
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
