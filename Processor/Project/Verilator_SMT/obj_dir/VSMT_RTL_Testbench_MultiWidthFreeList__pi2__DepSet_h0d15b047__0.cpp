// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__pi2.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___act_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___act_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__wv[0U] = vlSelfRef.__PVT__pushedData
        [0U];
    vlSelfRef.__PVT__wv[1U] = vlSelfRef.__PVT__pushedData
        [1U];
    vlSelfRef.__PVT__pushCount = 0U;
    vlSelfRef.__PVT__wa[0U] = vlSelfRef.__PVT__queuePointer__DOT__regTail;
    if (vlSelfRef.__PVT__push[0U]) {
        vlSelfRef.__PVT__we[0U] = 1U;
        vlSelfRef.__PVT__pushCount = (3U & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__pushCount)));
    } else {
        vlSelfRef.__PVT__we[0U] = 0U;
    }
    vlSelfRef.__PVT__wa[1U] = (0x1fU & ((0x20U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                   + (IData)(vlSelfRef.__PVT__pushCount)))
                                         ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                            + (IData)(vlSelfRef.__PVT__pushCount))
                                         : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                            + (IData)(vlSelfRef.__PVT__pushCount))));
    vlSelfRef.__PVT__we[1U] = vlSelfRef.__PVT__push
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
        vlSelfRef.__PVT__wv[0U] = (0x7fU & (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wv[1U] = (0x7fU & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wa[0U] = vlSelfRef.__PVT__rstIndex;
        vlSelfRef.__PVT__we[0U] = (vlSelfRef.__PVT__wa
                                   [0U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[1U] = (0x1fU & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[1U] = (vlSelfRef.__PVT__wa
                                   [1U] >= (IData)(vlSelfRef.__PVT__rstIndex));
    }
    if (vlSelfRef.__PVT__push[1U]) {
        vlSelfRef.__PVT__pushCount = (3U & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelf->freeList->__PVT__wv[0U] = vlSelfRef.__PVT__wv
        [0U];
    vlSelf->freeList->__PVT__wv[1U] = vlSelfRef.__PVT__wv
        [1U];
    vlSelfRef.__PVT__queuePointer__DOT__nextTail = vlSelfRef.__PVT__queuePointer__DOT__regTail;
    if ((0U != (IData)(vlSelfRef.__PVT__pushCount))) {
        vlSelfRef.__PVT__queuePointer__DOT__nextTail 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextTail) 
                        + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelf->freeList->__PVT__wa[0U] = vlSelfRef.__PVT__wa
        [0U];
    vlSelf->freeList->__PVT__wa[1U] = vlSelfRef.__PVT__wa
        [1U];
    vlSelf->freeList->__PVT__we[0U] = vlSelfRef.__PVT__we
        [0U];
    vlSelf->freeList->__PVT__we[1U] = vlSelfRef.__PVT__we
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___nba_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__wv[0U] = vlSelfRef.__PVT__pushedData
        [0U];
    vlSelfRef.__PVT__wv[1U] = vlSelfRef.__PVT__pushedData
        [1U];
    vlSelfRef.__PVT__pushCount = 0U;
    vlSelfRef.__PVT__wa[0U] = vlSelfRef.__PVT__queuePointer__DOT__regTail;
    if (vlSelfRef.__PVT__push[0U]) {
        vlSelfRef.__PVT__we[0U] = 1U;
        vlSelfRef.__PVT__pushCount = (3U & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__pushCount)));
    } else {
        vlSelfRef.__PVT__we[0U] = 0U;
    }
    vlSelfRef.__PVT__wa[1U] = (0x1fU & ((0x20U <= ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                                   + (IData)(vlSelfRef.__PVT__pushCount)))
                                         ? ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                            + (IData)(vlSelfRef.__PVT__pushCount))
                                         : ((IData)(vlSelfRef.__PVT__queuePointer__DOT__regTail) 
                                            + (IData)(vlSelfRef.__PVT__pushCount))));
    vlSelfRef.__PVT__we[1U] = vlSelfRef.__PVT__push
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__wv[0U] = (0x7fU & (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wv[1U] = (0x7fU & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__wa[0U] = vlSelfRef.__PVT__rstIndex;
        vlSelfRef.__PVT__we[0U] = (vlSelfRef.__PVT__wa
                                   [0U] >= (IData)(vlSelfRef.__PVT__rstIndex));
        vlSelfRef.__PVT__wa[1U] = (0x1fU & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__rstIndex)));
        vlSelfRef.__PVT__we[1U] = (vlSelfRef.__PVT__wa
                                   [1U] >= (IData)(vlSelfRef.__PVT__rstIndex));
    }
    if (vlSelfRef.__PVT__push[1U]) {
        vlSelfRef.__PVT__pushCount = (3U & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelf->freeList->__PVT__wv[0U] = vlSelfRef.__PVT__wv
        [0U];
    vlSelf->freeList->__PVT__wv[1U] = vlSelfRef.__PVT__wv
        [1U];
    vlSelfRef.__PVT__queuePointer__DOT__nextTail = vlSelfRef.__PVT__queuePointer__DOT__regTail;
    if ((0U != (IData)(vlSelfRef.__PVT__pushCount))) {
        vlSelfRef.__PVT__queuePointer__DOT__nextTail 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__queuePointer__DOT__nextTail) 
                        + (IData)(vlSelfRef.__PVT__pushCount)));
    }
    vlSelf->freeList->__PVT__wa[0U] = vlSelfRef.__PVT__wa
        [0U];
    vlSelf->freeList->__PVT__wa[1U] = vlSelfRef.__PVT__wa
        [1U];
    vlSelf->freeList->__PVT__we[0U] = vlSelfRef.__PVT__we
        [0U];
    vlSelf->freeList->__PVT__we[1U] = vlSelfRef.__PVT__we
        [1U];
}
