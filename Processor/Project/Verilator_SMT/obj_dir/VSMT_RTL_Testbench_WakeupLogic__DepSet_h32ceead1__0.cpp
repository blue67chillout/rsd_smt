// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_WakeupLogic.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___act_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__0(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___act_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellinp__producerMatrix__dispatch[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [0U];
    vlSelfRef.__Vcellinp__producerMatrix__dispatch[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [1U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatch[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [0U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatch[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatch[0U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatch
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatch[1U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatch
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___act_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__1(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___act_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady 
        = ((0xfff8U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady)) 
           | ((vlSelfRef.__PVT__opMatrixReady[2U] << 2U) 
              | ((vlSelfRef.__PVT__opMatrixReady[1U] 
                  << 1U) | vlSelfRef.__PVT__opMatrixReady
                 [0U])));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady 
        = ((0xffc7U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady)) 
           | ((vlSelfRef.__PVT__opMatrixReady[5U] << 5U) 
              | ((vlSelfRef.__PVT__opMatrixReady[4U] 
                  << 4U) | (vlSelfRef.__PVT__opMatrixReady
                            [3U] << 3U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady 
        = ((0xfe3fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady)) 
           | ((vlSelfRef.__PVT__opMatrixReady[8U] << 8U) 
              | ((vlSelfRef.__PVT__opMatrixReady[7U] 
                  << 7U) | (vlSelfRef.__PVT__opMatrixReady
                            [6U] << 6U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady 
        = ((0xf1ffU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady)) 
           | ((vlSelfRef.__PVT__opMatrixReady[0xbU] 
               << 0xbU) | ((vlSelfRef.__PVT__opMatrixReady
                            [0xaU] << 0xaU) | (vlSelfRef.__PVT__opMatrixReady
                                               [9U] 
                                               << 9U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady 
        = ((0x8fffU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady)) 
           | ((vlSelfRef.__PVT__opMatrixReady[0xeU] 
               << 0xeU) | ((vlSelfRef.__PVT__opMatrixReady
                            [0xdU] << 0xdU) | (vlSelfRef.__PVT__opMatrixReady
                                               [0xcU] 
                                               << 0xcU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady 
        = ((0x7fffU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady)) 
           | (vlSelfRef.__PVT__opMatrixReady[0xfU] 
              << 0xfU));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__0(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__dispatchStore[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchStore
        [0U];
    vlSelfRef.__PVT__dispatchStore[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchStore
        [1U];
    vlSelfRef.__PVT__dispatchLoad[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchLoad
        [0U];
    vlSelfRef.__PVT__dispatchLoad[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchLoad
        [1U];
    vlSelfRef.__PVT__notIssued = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued;
    vlSelfRef.__PVT__storeBitVector = ((IData)(vlSelfRef.__PVT__storeBitVectorReg) 
                                       & (IData)(vlSelfRef.__PVT__notIssued));
    vlSelfRef.__PVT__memDependencyPred[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__memDependencyPred
        [0U];
    vlSelfRef.__PVT__memDependencyPred[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__memDependencyPred
        [1U];
    if (vlSelfRef.__PVT__dispatchStore[0U]) {
        vlSelfRef.__PVT__dependStoreBitVector[0U] = 0U;
        vlSelfRef.__PVT__storeBitVector = ((IData)(vlSelfRef.__PVT__storeBitVector) 
                                           | (0xffffU 
                                              & ((IData)(1U) 
                                                 << 
                                                 vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
                                                 [0U])));
    } else {
        vlSelfRef.__PVT__dependStoreBitVector[0U] = 
            ((vlSelfRef.__PVT__dispatchLoad[0U] & vlSelfRef.__PVT__memDependencyPred
              [0U]) ? (IData)(vlSelfRef.__PVT__storeBitVector)
              : 0U);
    }
    if (vlSelfRef.__PVT__dispatchStore[1U]) {
        vlSelfRef.__PVT__dependStoreBitVector[1U] = 0U;
        vlSelfRef.__PVT__storeBitVector = ((IData)(vlSelfRef.__PVT__storeBitVector) 
                                           | (0xffffU 
                                              & ((IData)(1U) 
                                                 << 
                                                 vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
                                                 [1U])));
    } else {
        vlSelfRef.__PVT__dependStoreBitVector[1U] = 
            ((vlSelfRef.__PVT__dispatchLoad[1U] & vlSelfRef.__PVT__memDependencyPred
              [1U]) ? (IData)(vlSelfRef.__PVT__storeBitVector)
              : 0U);
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__1(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*15:0*/ __Vlvbound_hd5093e65__0;
    __Vlvbound_hd5093e65__0 = 0;
    // Body
    __Vlvbound_hd5093e65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector
        [0U];
    vlSelfRef.__PVT__wakeupDstVector[0U] = __Vlvbound_hd5093e65__0;
    __Vlvbound_hd5093e65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector
        [1U];
    vlSelfRef.__PVT__wakeupDstVector[1U] = __Vlvbound_hd5093e65__0;
    __Vlvbound_hd5093e65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector
        [2U];
    vlSelfRef.__PVT__wakeupDstVector[2U] = __Vlvbound_hd5093e65__0;
    __Vlvbound_hd5093e65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector
        [3U];
    vlSelfRef.__PVT__wakeupDstVector[3U] = __Vlvbound_hd5093e65__0;
    __Vlvbound_hd5093e65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector
        [4U];
    vlSelfRef.__PVT__wakeupDstVector[4U] = __Vlvbound_hd5093e65__0;
    __Vlvbound_hd5093e65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector
        [5U];
    vlSelfRef.__PVT__wakeupDstVector[5U] = __Vlvbound_hd5093e65__0;
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup
        [0U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup
        [1U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup[2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup
        [2U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup[3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup
        [3U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup[4U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup
        [4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[0U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[1U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[2U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[3U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup
        [3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[4U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeup
        [4U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__2(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vlvbound_h5a6f2d9b__0;
    __Vlvbound_h5a6f2d9b__0 = 0;
    CData/*6:0*/ __Vlvbound_h0d734082__0;
    __Vlvbound_h0d734082__0 = 0;
    // Body
    __Vlvbound_h5a6f2d9b__0 = (1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                               [0U]);
    vlSelfRef.__PVT__wakeupDstRegValid[0U] = __Vlvbound_h5a6f2d9b__0;
    __Vlvbound_h5a6f2d9b__0 = (1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                               [1U]);
    vlSelfRef.__PVT__wakeupDstRegValid[1U] = __Vlvbound_h5a6f2d9b__0;
    __Vlvbound_h5a6f2d9b__0 = (1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                               [2U]);
    vlSelfRef.__PVT__wakeupDstRegValid[2U] = __Vlvbound_h5a6f2d9b__0;
    __Vlvbound_h5a6f2d9b__0 = (1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                               [3U]);
    vlSelfRef.__PVT__wakeupDstRegValid[3U] = __Vlvbound_h5a6f2d9b__0;
    __Vlvbound_h5a6f2d9b__0 = (1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                               [4U]);
    vlSelfRef.__PVT__wakeupDstRegValid[4U] = __Vlvbound_h5a6f2d9b__0;
    __Vlvbound_h0d734082__0 = (0x7fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                        [0U] >> 1U));
    vlSelfRef.__PVT__wakeupDstRegNum[0U] = __Vlvbound_h0d734082__0;
    __Vlvbound_h0d734082__0 = (0x7fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                        [1U] >> 1U));
    vlSelfRef.__PVT__wakeupDstRegNum[1U] = __Vlvbound_h0d734082__0;
    __Vlvbound_h0d734082__0 = (0x7fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                        [2U] >> 1U));
    vlSelfRef.__PVT__wakeupDstRegNum[2U] = __Vlvbound_h0d734082__0;
    __Vlvbound_h0d734082__0 = (0x7fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                        [3U] >> 1U));
    vlSelfRef.__PVT__wakeupDstRegNum[3U] = __Vlvbound_h0d734082__0;
    __Vlvbound_h0d734082__0 = (0x7fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                        [4U] >> 1U));
    vlSelfRef.__PVT__wakeupDstRegNum[4U] = __Vlvbound_h0d734082__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[0U] 
        = vlSelfRef.__PVT__wakeupDstRegValid[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[1U] 
        = vlSelfRef.__PVT__wakeupDstRegValid[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[2U] 
        = vlSelfRef.__PVT__wakeupDstRegValid[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[3U] 
        = vlSelfRef.__PVT__wakeupDstRegValid[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[4U] 
        = vlSelfRef.__PVT__wakeupDstRegValid[4U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum[0U] 
        = vlSelfRef.__PVT__wakeupDstRegNum[0U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum[1U] 
        = vlSelfRef.__PVT__wakeupDstRegNum[1U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum[2U] 
        = vlSelfRef.__PVT__wakeupDstRegNum[2U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum[3U] 
        = vlSelfRef.__PVT__wakeupDstRegNum[3U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum[4U] 
        = vlSelfRef.__PVT__wakeupDstRegNum[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[0U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[1U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[2U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[3U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum
        [3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[4U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__wakeupDstRegNum
        [4U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__3(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__dispatchedSrcRegReady[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
        [0U][0U];
    vlSelfRef.__PVT__dispatchedSrcRegReady[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
        [0U][1U];
    vlSelfRef.__PVT__dispatchedSrcRegReady[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
        [0U][2U];
    vlSelfRef.__PVT__dispatchedSrcRegReady[1U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
        [1U][0U];
    vlSelfRef.__PVT__dispatchedSrcRegReady[1U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
        [1U][1U];
    vlSelfRef.__PVT__dispatchedSrcRegReady[1U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
        [1U][2U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__0(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellinp__producerMatrix__dispatchPtr[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [0U];
    vlSelfRef.__Vcellinp__producerMatrix__dispatchPtr[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [1U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[0U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[1U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[2U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[3U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[4U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[5U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[6U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[7U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U];
    vlSelfRef.__PVT__storeBitVectorReg = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)
                                           ? 0U : (IData)(vlSelfRef.__PVT__storeBitVector));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__1(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ __Vlvbound_h46c53e00__0;
    __Vlvbound_h46c53e00__0 = 0;
    CData/*0:0*/ __Vlvbound_h281446bb__0;
    __Vlvbound_h281446bb__0 = 0;
    CData/*3:0*/ __Vlvbound_he24bb13d__0;
    __Vlvbound_he24bb13d__0 = 0;
    // Body
    vlSelfRef.__PVT__dispatchedDstRegValid[0U] = (1U 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
                                                  [0U]);
    vlSelfRef.__PVT__dispatchedDstRegValid[1U] = (1U 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
                                                  [1U]);
    vlSelfRef.__PVT__dispatchedDstRegNum[0U] = (0x7fU 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
                                                   [0U] 
                                                   >> 1U));
    vlSelfRef.__PVT__dispatchedDstRegNum[1U] = (0x7fU 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
                                                   [1U] 
                                                   >> 1U));
    __Vlvbound_he24bb13d__0 = (0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                               [0U] 
                                               >> 1U)));
    vlSelfRef.__PVT__dispatchedSrcRegPtr = ((0xfffff0U 
                                             & vlSelfRef.__PVT__dispatchedSrcRegPtr) 
                                            | (IData)(__Vlvbound_he24bb13d__0));
    __Vlvbound_he24bb13d__0 = (0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                               [0U] 
                                               >> 6U)));
    vlSelfRef.__PVT__dispatchedSrcRegPtr = ((0xffff0fU 
                                             & vlSelfRef.__PVT__dispatchedSrcRegPtr) 
                                            | ((IData)(__Vlvbound_he24bb13d__0) 
                                               << 4U));
    __Vlvbound_he24bb13d__0 = (0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                               [0U] 
                                               >> 0xbU)));
    vlSelfRef.__PVT__dispatchedSrcRegPtr = ((0xfff0ffU 
                                             & vlSelfRef.__PVT__dispatchedSrcRegPtr) 
                                            | ((IData)(__Vlvbound_he24bb13d__0) 
                                               << 8U));
    __Vlvbound_he24bb13d__0 = (0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                               [1U] 
                                               >> 1U)));
    vlSelfRef.__PVT__dispatchedSrcRegPtr = ((0xff0fffU 
                                             & vlSelfRef.__PVT__dispatchedSrcRegPtr) 
                                            | ((IData)(__Vlvbound_he24bb13d__0) 
                                               << 0xcU));
    __Vlvbound_he24bb13d__0 = (0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                               [1U] 
                                               >> 6U)));
    vlSelfRef.__PVT__dispatchedSrcRegPtr = ((0xf0ffffU 
                                             & vlSelfRef.__PVT__dispatchedSrcRegPtr) 
                                            | ((IData)(__Vlvbound_he24bb13d__0) 
                                               << 0x10U));
    __Vlvbound_he24bb13d__0 = (0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                               [1U] 
                                               >> 0xbU)));
    vlSelfRef.__PVT__dispatchedSrcRegPtr = ((0xfffffU 
                                             & vlSelfRef.__PVT__dispatchedSrcRegPtr) 
                                            | ((IData)(__Vlvbound_he24bb13d__0) 
                                               << 0x14U));
    __Vlvbound_h281446bb__0 = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                             [0U] >> 0xfU)));
    vlSelfRef.__PVT__dispatchedSrcRegValid[0U][0U] 
        = __Vlvbound_h281446bb__0;
    __Vlvbound_h281446bb__0 = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                             [0U] >> 0x17U)));
    vlSelfRef.__PVT__dispatchedSrcRegValid[0U][1U] 
        = __Vlvbound_h281446bb__0;
    __Vlvbound_h281446bb__0 = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                             [0U] >> 0x1fU)));
    vlSelfRef.__PVT__dispatchedSrcRegValid[0U][2U] 
        = __Vlvbound_h281446bb__0;
    __Vlvbound_h281446bb__0 = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                             [1U] >> 0xfU)));
    vlSelfRef.__PVT__dispatchedSrcRegValid[1U][0U] 
        = __Vlvbound_h281446bb__0;
    __Vlvbound_h281446bb__0 = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                             [1U] >> 0x17U)));
    vlSelfRef.__PVT__dispatchedSrcRegValid[1U][1U] 
        = __Vlvbound_h281446bb__0;
    __Vlvbound_h281446bb__0 = (1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                             [1U] >> 0x1fU)));
    vlSelfRef.__PVT__dispatchedSrcRegValid[1U][2U] 
        = __Vlvbound_h281446bb__0;
    __Vlvbound_h46c53e00__0 = (0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                                [0U] 
                                                >> 0x10U)));
    vlSelfRef.__PVT__dispatchedSrcRegNum[0U][0U] = __Vlvbound_h46c53e00__0;
    __Vlvbound_h46c53e00__0 = (0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                                [0U] 
                                                >> 0x18U)));
    vlSelfRef.__PVT__dispatchedSrcRegNum[0U][1U] = __Vlvbound_h46c53e00__0;
    __Vlvbound_h46c53e00__0 = (0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                                [0U] 
                                                >> 0x20U)));
    vlSelfRef.__PVT__dispatchedSrcRegNum[0U][2U] = __Vlvbound_h46c53e00__0;
    __Vlvbound_h46c53e00__0 = (0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                                [1U] 
                                                >> 0x10U)));
    vlSelfRef.__PVT__dispatchedSrcRegNum[1U][0U] = __Vlvbound_h46c53e00__0;
    __Vlvbound_h46c53e00__0 = (0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                                [1U] 
                                                >> 0x18U)));
    vlSelfRef.__PVT__dispatchedSrcRegNum[1U][1U] = __Vlvbound_h46c53e00__0;
    __Vlvbound_h46c53e00__0 = (0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeSrcTag
                                                [1U] 
                                                >> 0x20U)));
    vlSelfRef.__PVT__dispatchedSrcRegNum[1U][2U] = __Vlvbound_h46c53e00__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedDstValid[0U] 
        = vlSelfRef.__PVT__dispatchedDstRegValid[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedDstValid[1U] 
        = vlSelfRef.__PVT__dispatchedDstRegValid[1U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedDstRegNum[0U] 
        = vlSelfRef.__PVT__dispatchedDstRegNum[0U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedDstRegNum[1U] 
        = vlSelfRef.__PVT__dispatchedDstRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcValid[0U][0U] 
        = vlSelfRef.__PVT__dispatchedSrcRegValid[0U]
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcValid[0U][1U] 
        = vlSelfRef.__PVT__dispatchedSrcRegValid[0U]
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcValid[0U][2U] 
        = vlSelfRef.__PVT__dispatchedSrcRegValid[0U]
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcValid[1U][0U] 
        = vlSelfRef.__PVT__dispatchedSrcRegValid[1U]
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcValid[1U][1U] 
        = vlSelfRef.__PVT__dispatchedSrcRegValid[1U]
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcValid[1U][2U] 
        = vlSelfRef.__PVT__dispatchedSrcRegValid[1U]
        [2U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum[0U][0U] 
        = vlSelfRef.__PVT__dispatchedSrcRegNum[0U][0U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum[0U][1U] 
        = vlSelfRef.__PVT__dispatchedSrcRegNum[0U][1U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum[0U][2U] 
        = vlSelfRef.__PVT__dispatchedSrcRegNum[0U][2U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum[1U][0U] 
        = vlSelfRef.__PVT__dispatchedSrcRegNum[1U][0U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum[1U][1U] 
        = vlSelfRef.__PVT__dispatchedSrcRegNum[1U][1U];
    vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum[1U][2U] 
        = vlSelfRef.__PVT__dispatchedSrcRegNum[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedDstRegNum[0U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedDstRegNum
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedDstRegNum[1U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedDstRegNum
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcRegNum[0U][0U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcRegNum[0U][1U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcRegNum[0U][2U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcRegNum[1U][0U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcRegNum[1U][1U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcRegNum[1U][2U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatchedSrcRegNum
        [1U][2U];
}
