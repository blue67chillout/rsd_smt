// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Gshare.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_Gshare___act_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__0(VSMT_RTL_Testbench_Gshare* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_Gshare___act_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__0__Vfuncout;
    __Vfunc_ToPHT_Index_Global__0__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToPHT_Index_Global__0__pc;
    __Vfunc_ToPHT_Index_Global__0__pc = 0;
    SData/*9:0*/ __Vfunc_ToPHT_Index_Global__0__gh;
    __Vfunc_ToPHT_Index_Global__0__gh = 0;
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__0__phtIndex;
    __Vfunc_ToPHT_Index_Global__0__phtIndex = 0;
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__1__Vfuncout;
    __Vfunc_ToPHT_Index_Global__1__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToPHT_Index_Global__1__pc;
    __Vfunc_ToPHT_Index_Global__1__pc = 0;
    SData/*9:0*/ __Vfunc_ToPHT_Index_Global__1__gh;
    __Vfunc_ToPHT_Index_Global__1__gh = 0;
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__1__phtIndex;
    __Vfunc_ToPHT_Index_Global__1__phtIndex = 0;
    // Body
    vlSelfRef.__PVT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage) 
                                    >> 1U));
    vlSelfRef.__PVT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage));
    vlSelfRef.__PVT__pcIn = vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__predNextPC;
    vlSelfRef.__PVT__nextBrGlobalHistory = vlSelfRef.__PVT__regBrGlobalHistory;
    vlSelfRef.__PVT__brPredTaken[0U] = 0U;
    vlSelfRef.__PVT__brGlobalHistory[0U] = vlSelfRef.__PVT__regBrGlobalHistory;
    vlSelfRef.__PVT__updateHistory[0U] = 0U;
    vlSelfRef.__PVT__brPredTaken[1U] = 0U;
    vlSelfRef.__PVT__brGlobalHistory[1U] = vlSelfRef.__PVT__regBrGlobalHistory;
    vlSelfRef.__PVT__updateHistory[1U] = 0U;
    vlSelfRef.__PVT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__unnamedblk2__DOT__i)) {
            vlSelfRef.__PVT__brPredTaken[(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                = ((vlSelfRef.__PVT__phtRV[(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                    >> 1U) & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit
                   [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]);
            vlSelfRef.__PVT__updateHistory[(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                = ((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit
                    [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                    & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__readIsCondBr
                    [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]) 
                   & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__updateBrHistory
                   [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]);
            if (vlSelfRef.__PVT__updateHistory[(1U 
                                                & vlSelfRef.__PVT__unnamedblk2__DOT__i)]) {
                vlSelfRef.__PVT__nextBrGlobalHistory 
                    = (0x3ffU & (VL_SHIFTL_III(10,10,32, (IData)(vlSelfRef.__PVT__nextBrGlobalHistory), 1U) 
                                 | vlSelfRef.__PVT__brPredTaken
                                 [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]));
                if (vlSelfRef.__PVT__brPredTaken[(1U 
                                                  & vlSelfRef.__PVT__unnamedblk2__DOT__i)]) {
                    goto __Vlabel1;
                }
            }
            vlSelfRef.__PVT__unnamedblk2__DOT__i = 
                ((IData)(1U) + vlSelfRef.__PVT__unnamedblk2__DOT__i);
        }
        __Vlabel1: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue[0U] 
        = vlSelfRef.__PVT__phtRV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue[1U] 
        = vlSelfRef.__PVT__phtRV[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken[0U] 
        = vlSelfRef.__PVT__brPredTaken[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken[1U] 
        = vlSelfRef.__PVT__brPredTaken[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory[0U] 
        = vlSelfRef.__PVT__brGlobalHistory[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory[1U] 
        = vlSelfRef.__PVT__brGlobalHistory[1U];
    vlSelfRef.__PVT__phtWE[0U] = 0U;
    vlSelfRef.__PVT__phtWV[0U] = 0U;
    vlSelfRef.__PVT__phtPrevValue[0U] = (3U & (IData)(
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [0U]));
    __Vfunc_ToPHT_Index_Global__0__gh = (0x3ffU & (IData)(
                                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                           [0U] 
                                                           >> 2U)));
    __Vfunc_ToPHT_Index_Global__0__pc = (0xfffffU & (IData)(
                                                            (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                             [0U] 
                                                             >> 0x25U)));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__0__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__0__Vfuncout = __Vfunc_ToPHT_Index_Global__0__phtIndex;
    vlSelfRef.__PVT__phtWA[0U] = __Vfunc_ToPHT_Index_Global__0__Vfuncout;
    vlSelfRef.__PVT__phtWE[1U] = 0U;
    vlSelfRef.__PVT__phtWV[1U] = 0U;
    vlSelfRef.__PVT__phtPrevValue[1U] = (3U & (IData)(
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [1U]));
    __Vfunc_ToPHT_Index_Global__0__gh = (0x3ffU & (IData)(
                                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                           [1U] 
                                                           >> 2U)));
    __Vfunc_ToPHT_Index_Global__0__pc = (0xfffffU & (IData)(
                                                            (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                             [1U] 
                                                             >> 0x25U)));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__0__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__0__Vfuncout = __Vfunc_ToPHT_Index_Global__0__phtIndex;
    vlSelfRef.__PVT__phtWA[1U] = __Vfunc_ToPHT_Index_Global__0__Vfuncout;
    vlSelfRef.__PVT__unnamedblk3__DOT__i = 2U;
    vlSelfRef.__PVT__pushPhtQueue = 0U;
    vlSelfRef.__PVT__phtWE[0U] = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                [0U] 
                                                >> 0xcU)));
    vlSelfRef.__PVT__updatePht = vlSelfRef.__PVT__phtWE
        [0U];
    vlSelfRef.__PVT__mispred = (1U & ((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                               [0U] 
                                               >> 0xdU)) 
                                      & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                 [0U] 
                                                 >> 0xcU))));
    vlSelfRef.__PVT__phtWV[0U] = (3U & ((1U & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [0U] 
                                                       >> 0x10U)))
                                         ? ((3U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [0U]) ? 3U
                                             : ((IData)(1U) 
                                                + vlSelfRef.__PVT__phtPrevValue
                                                [0U]))
                                         : ((0U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [0U]) ? 0U
                                             : (vlSelfRef.__PVT__phtPrevValue
                                                [0U] 
                                                - (IData)(1U)))));
    if (vlSelfRef.__PVT__updatePht) {
        vlSelfRef.__PVT__pushPhtQueue = (1U & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0xcU)));
    } else {
        vlSelfRef.__PVT__phtWE[1U] = (1U & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                    [1U] 
                                                    >> 0xcU)));
        vlSelfRef.__PVT__updatePht = ((IData)(vlSelfRef.__PVT__updatePht) 
                                      | vlSelfRef.__PVT__phtWE
                                      [1U]);
    }
    if (vlSelfRef.__PVT__mispred) {
        vlSelfRef.__PVT__nextBrGlobalHistory = (0x3ffU 
                                                & ((1U 
                                                    & (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [0U] 
                                                               >> 0xeU)))
                                                    ? 
                                                   (VL_SHIFTL_III(10,10,32, (IData)(
                                                                                (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                                [0U] 
                                                                                >> 2U)), 1U) 
                                                    | (1U 
                                                       & (IData)(
                                                                 (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                  [0U] 
                                                                  >> 0x10U))))
                                                    : (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [0U] 
                                                               >> 2U))));
    }
    vlSelfRef.__PVT__mispred = (1U & ((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                               [1U] 
                                               >> 0xdU)) 
                                      & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                 [1U] 
                                                 >> 0xcU))));
    if (vlSelfRef.__PVT__mispred) {
        vlSelfRef.__PVT__nextBrGlobalHistory = (0x3ffU 
                                                & ((1U 
                                                    & (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [1U] 
                                                               >> 0xeU)))
                                                    ? 
                                                   (VL_SHIFTL_III(10,10,32, (IData)(
                                                                                (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                                [1U] 
                                                                                >> 2U)), 1U) 
                                                    | (1U 
                                                       & (IData)(
                                                                 (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                  [1U] 
                                                                  >> 0x10U))))
                                                    : (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [1U] 
                                                               >> 2U))));
    }
    vlSelfRef.__PVT__phtWV[1U] = (3U & ((1U & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0x10U)))
                                         ? ((3U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [1U]) ? 3U
                                             : ((IData)(1U) 
                                                + vlSelfRef.__PVT__phtPrevValue
                                                [1U]))
                                         : ((0U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [1U]) ? 0U
                                             : (vlSelfRef.__PVT__phtPrevValue
                                                [1U] 
                                                - (IData)(1U)))));
    vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
    __Vfunc_ToPHT_Index_Global__1__gh = vlSelfRef.__PVT__nextBrGlobalHistory;
    __Vfunc_ToPHT_Index_Global__1__pc = vlSelfRef.__PVT__pcIn;
    __Vfunc_ToPHT_Index_Global__1__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__1__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__1__Vfuncout = __Vfunc_ToPHT_Index_Global__1__phtIndex;
    vlSelfRef.__PVT__phtRA[0U] = __Vfunc_ToPHT_Index_Global__1__Vfuncout;
    __Vfunc_ToPHT_Index_Global__1__gh = vlSelfRef.__PVT__nextBrGlobalHistory;
    __Vfunc_ToPHT_Index_Global__1__pc = (0xfffffU & 
                                         ((IData)(4U) 
                                          + vlSelfRef.__PVT__pcIn));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__1__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__1__Vfuncout = __Vfunc_ToPHT_Index_Global__1__phtIndex;
    vlSelfRef.__PVT__phtRA[1U] = __Vfunc_ToPHT_Index_Global__1__Vfuncout;
    vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
    if (((0U != (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regCount)) 
         & (~ (IData)(vlSelfRef.__PVT__updatePht)))) {
        vlSelfRef.__PVT__popPhtQueue = 1U;
        vlSelfRef.__PVT__phtWE[0U] = 1U;
        vlSelfRef.__PVT__phtWA[0U] = (0x7ffU & (IData)(
                                                       (vlSelfRef.__PVT__phtQueue
                                                        [vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage] 
                                                        >> 2U)));
        vlSelfRef.__PVT__phtWV[0U] = (3U & (IData)(
                                                   vlSelfRef.__PVT__phtQueue
                                                   [vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage]));
    } else {
        vlSelfRef.__PVT__popPhtQueue = 0U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__phtWE[0U] = 1U;
        vlSelfRef.__PVT__phtWA[0U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__phtWV[0U] = 2U;
        vlSelfRef.__PVT__phtWE[1U] = 0U;
        vlSelfRef.__PVT__phtWA[1U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__phtWV[1U] = 2U;
        vlSelfRef.__PVT__unnamedblk6__DOT__i = 2U;
        vlSelfRef.__PVT__phtRA[0U] = 0U;
        vlSelfRef.__PVT__phtRA[1U] = 1U;
        vlSelfRef.__PVT__unnamedblk7__DOT__i = 2U;
        vlSelfRef.__PVT__pushPhtQueue = 0U;
        vlSelfRef.__PVT__popPhtQueue = 0U;
    }
    vlSelfRef.__PVT__phtQueuePointer__DOT__nextTailStorage 
        = vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage;
    vlSelfRef.__PVT__phtQueuePointer__DOT__nextHeadStorage 
        = vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage;
    vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount 
        = vlSelfRef.__PVT__phtQueuePointer__DOT__regCount;
    if (vlSelfRef.__PVT__pushPhtQueue) {
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextTailStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextTailStorage))));
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount)));
    }
    if (vlSelfRef.__PVT__popPhtQueue) {
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextHeadStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextHeadStorage))));
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount) 
                        - (IData)(1U)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__we[0U] 
        = vlSelfRef.__PVT__phtWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__we[1U] 
        = vlSelfRef.__PVT__phtWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wv[0U] 
        = vlSelfRef.__PVT__phtWV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wv[1U] 
        = vlSelfRef.__PVT__phtWV[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wa[0U] 
        = vlSelfRef.__PVT__phtWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wa[1U] 
        = vlSelfRef.__PVT__phtWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__ra[0U] 
        = vlSelfRef.__PVT__phtRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__ra[1U] 
        = vlSelfRef.__PVT__phtRA[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Gshare___act_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__1(VSMT_RTL_Testbench_Gshare* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_Gshare___act_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__phtRV[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__rv
        [0U];
    vlSelfRef.__PVT__phtRV[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__rv
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Gshare___nba_sequent__TOP__SMT_RTL_Testbench__core__brPred__predictor__0(VSMT_RTL_Testbench_Gshare* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_Gshare___nba_sequent__TOP__SMT_RTL_Testbench__core__brPred__predictor__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*10:0*/ __Vdly__resetIndex;
    __Vdly__resetIndex = 0;
    CData/*4:0*/ __VdlyDim0__phtQueue__v0;
    __VdlyDim0__phtQueue__v0 = 0;
    CData/*0:0*/ __VdlySet__phtQueue__v0;
    __VdlySet__phtQueue__v0 = 0;
    CData/*4:0*/ __VdlyDim0__phtQueue__v1;
    __VdlyDim0__phtQueue__v1 = 0;
    IData/*31:0*/ __VdlyVal__phtQueue__v2;
    __VdlyVal__phtQueue__v2 = 0;
    CData/*4:0*/ __VdlyDim0__phtQueue__v2;
    __VdlyDim0__phtQueue__v2 = 0;
    CData/*0:0*/ __VdlySet__phtQueue__v2;
    __VdlySet__phtQueue__v2 = 0;
    CData/*1:0*/ __VdlyVal__phtQueue__v3;
    __VdlyVal__phtQueue__v3 = 0;
    CData/*4:0*/ __VdlyDim0__phtQueue__v3;
    __VdlyDim0__phtQueue__v3 = 0;
    // Body
    __Vdly__resetIndex = vlSelfRef.__PVT__resetIndex;
    if (VL_UNLIKELY((((0U == (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regCount)) 
                      & (IData)(vlSelfRef.__PVT__popPhtQueue))))) {
        VL_WRITEF_NX("Pop from an empty queue.\n",0);
    }
    if (VL_UNLIKELY((((0x20U == (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regCount)) 
                      & (IData)(vlSelfRef.__PVT__pushPhtQueue))))) {
        VL_WRITEF_NX("Push to a full queue.\n",0);
    }
    __VdlySet__phtQueue__v0 = 0U;
    __VdlySet__phtQueue__v2 = 0U;
    __Vdly__resetIndex = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart)
                           ? 0U : (0x7ffU & ((IData)(1U) 
                                             + (IData)(vlSelfRef.__PVT__resetIndex))));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        __VdlyDim0__phtQueue__v0 = (0x1fU & (IData)(vlSelfRef.__PVT__resetIndex));
        __VdlySet__phtQueue__v0 = 1U;
        __VdlyDim0__phtQueue__v1 = (0x1fU & (IData)(vlSelfRef.__PVT__resetIndex));
        vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage = 0U;
        vlSelfRef.__PVT__regBrGlobalHistory = 0U;
        vlSelfRef.__PVT__phtQueuePointer__DOT__regCount = 0U;
        vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage = 0U;
    } else {
        if (vlSelfRef.__PVT__pushPhtQueue) {
            __VdlyVal__phtQueue__v2 = vlSelfRef.__PVT__phtWA
                [1U];
            __VdlyDim0__phtQueue__v2 = vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage;
            __VdlySet__phtQueue__v2 = 1U;
            __VdlyVal__phtQueue__v3 = vlSelfRef.__PVT__phtWV
                [1U];
            __VdlyDim0__phtQueue__v3 = vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage;
        }
        vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage 
            = vlSelfRef.__PVT__phtQueuePointer__DOT__nextTailStorage;
        vlSelfRef.__PVT__regBrGlobalHistory = vlSelfRef.__PVT__nextBrGlobalHistory;
        vlSelfRef.__PVT__phtQueuePointer__DOT__regCount 
            = vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount;
        vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage 
            = vlSelfRef.__PVT__phtQueuePointer__DOT__nextHeadStorage;
    }
    vlSelfRef.__PVT__resetIndex = __Vdly__resetIndex;
    if (__VdlySet__phtQueue__v0) {
        vlSelfRef.__PVT__phtQueue[__VdlyDim0__phtQueue__v0] 
            = (3ULL & vlSelfRef.__PVT__phtQueue[__VdlyDim0__phtQueue__v0]);
        vlSelfRef.__PVT__phtQueue[__VdlyDim0__phtQueue__v1] 
            = (2ULL | (0x3fffffffcULL & vlSelfRef.__PVT__phtQueue
                       [__VdlyDim0__phtQueue__v1]));
    }
    if (__VdlySet__phtQueue__v2) {
        vlSelfRef.__PVT__phtQueue[__VdlyDim0__phtQueue__v2] 
            = ((3ULL & vlSelfRef.__PVT__phtQueue[__VdlyDim0__phtQueue__v2]) 
               | ((QData)((IData)(__VdlyVal__phtQueue__v2)) 
                  << 2U));
        vlSelfRef.__PVT__phtQueue[__VdlyDim0__phtQueue__v3] 
            = ((0x3fffffffcULL & vlSelfRef.__PVT__phtQueue
                [__VdlyDim0__phtQueue__v3]) | (IData)((IData)(__VdlyVal__phtQueue__v3)));
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Gshare___nba_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__0(VSMT_RTL_Testbench_Gshare* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_Gshare___nba_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__0__Vfuncout;
    __Vfunc_ToPHT_Index_Global__0__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToPHT_Index_Global__0__pc;
    __Vfunc_ToPHT_Index_Global__0__pc = 0;
    SData/*9:0*/ __Vfunc_ToPHT_Index_Global__0__gh;
    __Vfunc_ToPHT_Index_Global__0__gh = 0;
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__0__phtIndex;
    __Vfunc_ToPHT_Index_Global__0__phtIndex = 0;
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__1__Vfuncout;
    __Vfunc_ToPHT_Index_Global__1__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToPHT_Index_Global__1__pc;
    __Vfunc_ToPHT_Index_Global__1__pc = 0;
    SData/*9:0*/ __Vfunc_ToPHT_Index_Global__1__gh;
    __Vfunc_ToPHT_Index_Global__1__gh = 0;
    SData/*10:0*/ __Vfunc_ToPHT_Index_Global__1__phtIndex;
    __Vfunc_ToPHT_Index_Global__1__phtIndex = 0;
    // Body
    vlSelfRef.__PVT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage) 
                                    >> 1U));
    vlSelfRef.__PVT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage));
    vlSelfRef.__PVT__pcIn = vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__predNextPC;
    vlSelfRef.__PVT__nextBrGlobalHistory = vlSelfRef.__PVT__regBrGlobalHistory;
    vlSelfRef.__PVT__brPredTaken[0U] = 0U;
    vlSelfRef.__PVT__brGlobalHistory[0U] = vlSelfRef.__PVT__regBrGlobalHistory;
    vlSelfRef.__PVT__updateHistory[0U] = 0U;
    vlSelfRef.__PVT__brPredTaken[1U] = 0U;
    vlSelfRef.__PVT__brGlobalHistory[1U] = vlSelfRef.__PVT__regBrGlobalHistory;
    vlSelfRef.__PVT__updateHistory[1U] = 0U;
    vlSelfRef.__PVT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__unnamedblk2__DOT__i)) {
            vlSelfRef.__PVT__brPredTaken[(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                = ((vlSelfRef.__PVT__phtRV[(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                    >> 1U) & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit
                   [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]);
            vlSelfRef.__PVT__updateHistory[(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                = ((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit
                    [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)] 
                    & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__readIsCondBr
                    [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]) 
                   & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__updateBrHistory
                   [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]);
            if (vlSelfRef.__PVT__updateHistory[(1U 
                                                & vlSelfRef.__PVT__unnamedblk2__DOT__i)]) {
                vlSelfRef.__PVT__nextBrGlobalHistory 
                    = (0x3ffU & (VL_SHIFTL_III(10,10,32, (IData)(vlSelfRef.__PVT__nextBrGlobalHistory), 1U) 
                                 | vlSelfRef.__PVT__brPredTaken
                                 [(1U & vlSelfRef.__PVT__unnamedblk2__DOT__i)]));
                if (vlSelfRef.__PVT__brPredTaken[(1U 
                                                  & vlSelfRef.__PVT__unnamedblk2__DOT__i)]) {
                    goto __Vlabel2;
                }
            }
            vlSelfRef.__PVT__unnamedblk2__DOT__i = 
                ((IData)(1U) + vlSelfRef.__PVT__unnamedblk2__DOT__i);
        }
        __Vlabel2: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue[0U] 
        = vlSelfRef.__PVT__phtRV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue[1U] 
        = vlSelfRef.__PVT__phtRV[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken[0U] 
        = vlSelfRef.__PVT__brPredTaken[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken[1U] 
        = vlSelfRef.__PVT__brPredTaken[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory[0U] 
        = vlSelfRef.__PVT__brGlobalHistory[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory[1U] 
        = vlSelfRef.__PVT__brGlobalHistory[1U];
    vlSelfRef.__PVT__phtWE[0U] = 0U;
    vlSelfRef.__PVT__phtWV[0U] = 0U;
    vlSelfRef.__PVT__phtPrevValue[0U] = (3U & (IData)(
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [0U]));
    __Vfunc_ToPHT_Index_Global__0__gh = (0x3ffU & (IData)(
                                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                           [0U] 
                                                           >> 2U)));
    __Vfunc_ToPHT_Index_Global__0__pc = (0xfffffU & (IData)(
                                                            (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                             [0U] 
                                                             >> 0x25U)));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__0__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__0__Vfuncout = __Vfunc_ToPHT_Index_Global__0__phtIndex;
    vlSelfRef.__PVT__phtWA[0U] = __Vfunc_ToPHT_Index_Global__0__Vfuncout;
    vlSelfRef.__PVT__phtWE[1U] = 0U;
    vlSelfRef.__PVT__phtWV[1U] = 0U;
    vlSelfRef.__PVT__phtPrevValue[1U] = (3U & (IData)(
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [1U]));
    __Vfunc_ToPHT_Index_Global__0__gh = (0x3ffU & (IData)(
                                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                           [1U] 
                                                           >> 2U)));
    __Vfunc_ToPHT_Index_Global__0__pc = (0xfffffU & (IData)(
                                                            (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                             [1U] 
                                                             >> 0x25U)));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__0__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__0__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__0__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__0__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__0__Vfuncout = __Vfunc_ToPHT_Index_Global__0__phtIndex;
    vlSelfRef.__PVT__phtWA[1U] = __Vfunc_ToPHT_Index_Global__0__Vfuncout;
    vlSelfRef.__PVT__unnamedblk3__DOT__i = 2U;
    vlSelfRef.__PVT__pushPhtQueue = 0U;
    vlSelfRef.__PVT__phtWE[0U] = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                [0U] 
                                                >> 0xcU)));
    vlSelfRef.__PVT__updatePht = vlSelfRef.__PVT__phtWE
        [0U];
    vlSelfRef.__PVT__mispred = (1U & ((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                               [0U] 
                                               >> 0xdU)) 
                                      & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                 [0U] 
                                                 >> 0xcU))));
    vlSelfRef.__PVT__phtWV[0U] = (3U & ((1U & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [0U] 
                                                       >> 0x10U)))
                                         ? ((3U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [0U]) ? 3U
                                             : ((IData)(1U) 
                                                + vlSelfRef.__PVT__phtPrevValue
                                                [0U]))
                                         : ((0U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [0U]) ? 0U
                                             : (vlSelfRef.__PVT__phtPrevValue
                                                [0U] 
                                                - (IData)(1U)))));
    if (vlSelfRef.__PVT__updatePht) {
        vlSelfRef.__PVT__pushPhtQueue = (1U & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0xcU)));
    } else {
        vlSelfRef.__PVT__phtWE[1U] = (1U & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                    [1U] 
                                                    >> 0xcU)));
        vlSelfRef.__PVT__updatePht = ((IData)(vlSelfRef.__PVT__updatePht) 
                                      | vlSelfRef.__PVT__phtWE
                                      [1U]);
    }
    if (vlSelfRef.__PVT__mispred) {
        vlSelfRef.__PVT__nextBrGlobalHistory = (0x3ffU 
                                                & ((1U 
                                                    & (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [0U] 
                                                               >> 0xeU)))
                                                    ? 
                                                   (VL_SHIFTL_III(10,10,32, (IData)(
                                                                                (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                                [0U] 
                                                                                >> 2U)), 1U) 
                                                    | (1U 
                                                       & (IData)(
                                                                 (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                  [0U] 
                                                                  >> 0x10U))))
                                                    : (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [0U] 
                                                               >> 2U))));
    }
    vlSelfRef.__PVT__mispred = (1U & ((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                               [1U] 
                                               >> 0xdU)) 
                                      & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                 [1U] 
                                                 >> 0xcU))));
    if (vlSelfRef.__PVT__mispred) {
        vlSelfRef.__PVT__nextBrGlobalHistory = (0x3ffU 
                                                & ((1U 
                                                    & (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [1U] 
                                                               >> 0xeU)))
                                                    ? 
                                                   (VL_SHIFTL_III(10,10,32, (IData)(
                                                                                (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                                [1U] 
                                                                                >> 2U)), 1U) 
                                                    | (1U 
                                                       & (IData)(
                                                                 (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                                  [1U] 
                                                                  >> 0x10U))))
                                                    : (IData)(
                                                              (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                               [1U] 
                                                               >> 2U))));
    }
    vlSelfRef.__PVT__phtWV[1U] = (3U & ((1U & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0x10U)))
                                         ? ((3U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [1U]) ? 3U
                                             : ((IData)(1U) 
                                                + vlSelfRef.__PVT__phtPrevValue
                                                [1U]))
                                         : ((0U == 
                                             vlSelfRef.__PVT__phtPrevValue
                                             [1U]) ? 0U
                                             : (vlSelfRef.__PVT__phtPrevValue
                                                [1U] 
                                                - (IData)(1U)))));
    vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
    __Vfunc_ToPHT_Index_Global__1__gh = vlSelfRef.__PVT__nextBrGlobalHistory;
    __Vfunc_ToPHT_Index_Global__1__pc = vlSelfRef.__PVT__pcIn;
    __Vfunc_ToPHT_Index_Global__1__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__1__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__1__Vfuncout = __Vfunc_ToPHT_Index_Global__1__phtIndex;
    vlSelfRef.__PVT__phtRA[0U] = __Vfunc_ToPHT_Index_Global__1__Vfuncout;
    __Vfunc_ToPHT_Index_Global__1__gh = vlSelfRef.__PVT__nextBrGlobalHistory;
    __Vfunc_ToPHT_Index_Global__1__pc = (0xfffffU & 
                                         ((IData)(4U) 
                                          + vlSelfRef.__PVT__pcIn));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = (0x7ffU 
                                               & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                  >> 2U));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((1U 
                                                & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                               | (0x7feU 
                                                  & ((0xfffffffeU 
                                                      & (IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex)) 
                                                     ^ 
                                                     ((IData)(__Vfunc_ToPHT_Index_Global__1__gh) 
                                                      << 1U))));
    __Vfunc_ToPHT_Index_Global__1__phtIndex = ((IData)(__Vfunc_ToPHT_Index_Global__1__phtIndex) 
                                               ^ (1U 
                                                  & (__Vfunc_ToPHT_Index_Global__1__pc 
                                                     >> 0x13U)));
    __Vfunc_ToPHT_Index_Global__1__Vfuncout = __Vfunc_ToPHT_Index_Global__1__phtIndex;
    vlSelfRef.__PVT__phtRA[1U] = __Vfunc_ToPHT_Index_Global__1__Vfuncout;
    vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
    if (((0U != (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regCount)) 
         & (~ (IData)(vlSelfRef.__PVT__updatePht)))) {
        vlSelfRef.__PVT__popPhtQueue = 1U;
        vlSelfRef.__PVT__phtWE[0U] = 1U;
        vlSelfRef.__PVT__phtWA[0U] = (0x7ffU & (IData)(
                                                       (vlSelfRef.__PVT__phtQueue
                                                        [vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage] 
                                                        >> 2U)));
        vlSelfRef.__PVT__phtWV[0U] = (3U & (IData)(
                                                   vlSelfRef.__PVT__phtQueue
                                                   [vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage]));
    } else {
        vlSelfRef.__PVT__popPhtQueue = 0U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__phtWE[0U] = 1U;
        vlSelfRef.__PVT__phtWA[0U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__phtWV[0U] = 2U;
        vlSelfRef.__PVT__phtWE[1U] = 0U;
        vlSelfRef.__PVT__phtWA[1U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__phtWV[1U] = 2U;
        vlSelfRef.__PVT__unnamedblk6__DOT__i = 2U;
        vlSelfRef.__PVT__phtRA[0U] = 0U;
        vlSelfRef.__PVT__phtRA[1U] = 1U;
        vlSelfRef.__PVT__unnamedblk7__DOT__i = 2U;
        vlSelfRef.__PVT__pushPhtQueue = 0U;
        vlSelfRef.__PVT__popPhtQueue = 0U;
    }
    vlSelfRef.__PVT__phtQueuePointer__DOT__nextTailStorage 
        = vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage;
    vlSelfRef.__PVT__phtQueuePointer__DOT__nextHeadStorage 
        = vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage;
    vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount 
        = vlSelfRef.__PVT__phtQueuePointer__DOT__regCount;
    if (vlSelfRef.__PVT__pushPhtQueue) {
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextTailStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regTailStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextTailStorage))));
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount)));
    }
    if (vlSelfRef.__PVT__popPhtQueue) {
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextHeadStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__regHeadStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextHeadStorage))));
        vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__phtQueuePointer__DOT__nextCount) 
                        - (IData)(1U)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__we[0U] 
        = vlSelfRef.__PVT__phtWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__we[1U] 
        = vlSelfRef.__PVT__phtWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wv[0U] 
        = vlSelfRef.__PVT__phtWV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wv[1U] 
        = vlSelfRef.__PVT__phtWV[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wa[0U] 
        = vlSelfRef.__PVT__phtWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wa[1U] 
        = vlSelfRef.__PVT__phtWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__ra[0U] 
        = vlSelfRef.__PVT__phtRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__ra[1U] 
        = vlSelfRef.__PVT__phtRA[1U];
}
