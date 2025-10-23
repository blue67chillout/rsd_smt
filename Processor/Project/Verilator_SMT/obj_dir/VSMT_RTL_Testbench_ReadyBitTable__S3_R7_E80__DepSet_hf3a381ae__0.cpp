// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___act_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__0(VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___act_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__readyWV[0U] = 1U;
    vlSelfRef.__PVT__readyWV[1U] = 1U;
    vlSelfRef.__PVT__readyWV[2U] = 1U;
    vlSelfRef.__PVT__readyWV[3U] = 1U;
    vlSelfRef.__PVT__readyWV[4U] = 1U;
    vlSelfRef.__PVT__readyWV[5U] = 0U;
    vlSelfRef.__PVT__readyWV[6U] = 0U;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk3__DOT__i = 7U;
        vlSelfRef.__PVT__readyWV[0U] = 1U;
    }
    vlSelfRef.__Vcellinp__radyBitTable____pinNumber4[0U] 
        = vlSelfRef.__PVT__readyWV[0U];
    vlSelfRef.__Vcellinp__radyBitTable____pinNumber4[1U] 
        = vlSelfRef.__PVT__readyWV[1U];
    vlSelfRef.__Vcellinp__radyBitTable____pinNumber4[2U] 
        = vlSelfRef.__PVT__readyWV[2U];
    vlSelfRef.__Vcellinp__radyBitTable____pinNumber4[3U] 
        = vlSelfRef.__PVT__readyWV[3U];
    vlSelfRef.__Vcellinp__radyBitTable____pinNumber4[4U] 
        = vlSelfRef.__PVT__readyWV[4U];
    vlSelfRef.__Vcellinp__radyBitTable____pinNumber4[5U] 
        = vlSelfRef.__PVT__readyWV[5U];
    vlSelfRef.__Vcellinp__radyBitTable____pinNumber4[6U] 
        = vlSelfRef.__PVT__readyWV[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__radyBitTable____pinNumber4
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__radyBitTable____pinNumber4
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wv[2U] 
        = vlSelfRef.__Vcellinp__radyBitTable____pinNumber4
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wv[3U] 
        = vlSelfRef.__Vcellinp__radyBitTable____pinNumber4
        [3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wv[4U] 
        = vlSelfRef.__Vcellinp__radyBitTable____pinNumber4
        [4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wv[5U] 
        = vlSelfRef.__Vcellinp__radyBitTable____pinNumber4
        [5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wv[6U] 
        = vlSelfRef.__Vcellinp__radyBitTable____pinNumber4
        [6U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__0(VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ __Vlvbound_hb0278605__0;
    __Vlvbound_hb0278605__0 = 0;
    CData/*0:0*/ __Vlvbound_h691fef8d__0;
    __Vlvbound_h691fef8d__0 = 0;
    CData/*6:0*/ __Vlvbound_h1a9bf5a5__0;
    __Vlvbound_h1a9bf5a5__0 = 0;
    CData/*0:0*/ __Vlvbound_hb5b3a32d__0;
    __Vlvbound_hb5b3a32d__0 = 0;
    // Body
    __Vlvbound_h691fef8d__0 = (vlSelfRef.__PVT__wakeup
                               [0U] & vlSelfRef.__PVT__wakeupDstValid
                               [0U]);
    vlSelfRef.__PVT__readyWE[0U] = __Vlvbound_h691fef8d__0;
    __Vlvbound_h691fef8d__0 = (vlSelfRef.__PVT__wakeup
                               [1U] & vlSelfRef.__PVT__wakeupDstValid
                               [1U]);
    vlSelfRef.__PVT__readyWE[1U] = __Vlvbound_h691fef8d__0;
    __Vlvbound_h691fef8d__0 = (vlSelfRef.__PVT__wakeup
                               [2U] & vlSelfRef.__PVT__wakeupDstValid
                               [2U]);
    vlSelfRef.__PVT__readyWE[2U] = __Vlvbound_h691fef8d__0;
    __Vlvbound_h691fef8d__0 = (vlSelfRef.__PVT__wakeup
                               [3U] & vlSelfRef.__PVT__wakeupDstValid
                               [3U]);
    vlSelfRef.__PVT__readyWE[3U] = __Vlvbound_h691fef8d__0;
    __Vlvbound_h691fef8d__0 = (vlSelfRef.__PVT__wakeup
                               [4U] & vlSelfRef.__PVT__wakeupDstValid
                               [4U]);
    vlSelfRef.__PVT__readyWE[4U] = __Vlvbound_h691fef8d__0;
    __Vlvbound_hb5b3a32d__0 = (vlSelfRef.__PVT__dispatch
                               [0U] & vlSelfRef.__PVT__dispatchedDstValid
                               [0U]);
    vlSelfRef.__PVT__readyWE[5U] = __Vlvbound_hb5b3a32d__0;
    __Vlvbound_hb5b3a32d__0 = (vlSelfRef.__PVT__dispatch
                               [1U] & vlSelfRef.__PVT__dispatchedDstValid
                               [1U]);
    vlSelfRef.__PVT__readyWE[6U] = __Vlvbound_hb5b3a32d__0;
    if (vlSelfRef.__PVT__dispatchedSrcValid[0U][0U]) {
        vlSelfRef.__Vlvbound_hdb6a0e9e__0 = vlSelfRef.__PVT__readyRV
            [0U];
        vlSelfRef.__PVT__dispatchedSrcReady[0U][0U] 
            = vlSelfRef.__Vlvbound_hdb6a0e9e__0;
        if (((vlSelfRef.__PVT__wakeup[0U] & vlSelfRef.__PVT__wakeupDstValid
              [0U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [0U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[1U] & vlSelfRef.__PVT__wakeupDstValid
              [1U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [1U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[2U] & vlSelfRef.__PVT__wakeupDstValid
              [2U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [2U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[3U] & vlSelfRef.__PVT__wakeupDstValid
              [3U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [3U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[4U] & vlSelfRef.__PVT__wakeupDstValid
              [4U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [4U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][0U] = 1U;
        }
    } else {
        vlSelfRef.__PVT__dispatchedSrcReady[0U][0U] = 1U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[0U][1U]) {
        vlSelfRef.__Vlvbound_hdb6a0e9e__0 = vlSelfRef.__PVT__readyRV
            [1U];
        vlSelfRef.__PVT__dispatchedSrcReady[0U][1U] 
            = vlSelfRef.__Vlvbound_hdb6a0e9e__0;
        if (((vlSelfRef.__PVT__wakeup[0U] & vlSelfRef.__PVT__wakeupDstValid
              [0U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [0U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[1U] & vlSelfRef.__PVT__wakeupDstValid
              [1U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [1U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[2U] & vlSelfRef.__PVT__wakeupDstValid
              [2U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [2U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[3U] & vlSelfRef.__PVT__wakeupDstValid
              [3U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [3U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[4U] & vlSelfRef.__PVT__wakeupDstValid
              [4U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [4U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][1U] = 1U;
        }
    } else {
        vlSelfRef.__PVT__dispatchedSrcReady[0U][1U] = 1U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[0U][2U]) {
        vlSelfRef.__Vlvbound_hdb6a0e9e__0 = vlSelfRef.__PVT__readyRV
            [2U];
        vlSelfRef.__PVT__dispatchedSrcReady[0U][2U] 
            = vlSelfRef.__Vlvbound_hdb6a0e9e__0;
        if (((vlSelfRef.__PVT__wakeup[0U] & vlSelfRef.__PVT__wakeupDstValid
              [0U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [0U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[1U] & vlSelfRef.__PVT__wakeupDstValid
              [1U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [1U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[2U] & vlSelfRef.__PVT__wakeupDstValid
              [2U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [2U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[3U] & vlSelfRef.__PVT__wakeupDstValid
              [3U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [3U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[4U] & vlSelfRef.__PVT__wakeupDstValid
              [4U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [4U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [0U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[0U][2U] = 1U;
        }
    } else {
        vlSelfRef.__PVT__dispatchedSrcReady[0U][2U] = 1U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[1U][0U]) {
        vlSelfRef.__Vlvbound_hdb6a0e9e__0 = vlSelfRef.__PVT__readyRV
            [3U];
        vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] 
            = vlSelfRef.__Vlvbound_hdb6a0e9e__0;
        if (((vlSelfRef.__PVT__wakeup[0U] & vlSelfRef.__PVT__wakeupDstValid
              [0U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [0U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[1U] & vlSelfRef.__PVT__wakeupDstValid
              [1U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [1U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[2U] & vlSelfRef.__PVT__wakeupDstValid
              [2U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [2U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[3U] & vlSelfRef.__PVT__wakeupDstValid
              [3U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [3U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[4U] & vlSelfRef.__PVT__wakeupDstValid
              [4U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [4U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] = 1U;
        }
        if (((vlSelfRef.__PVT__dispatch[0U] & vlSelfRef.__PVT__dispatchedDstValid
              [0U]) & (vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][0U] == vlSelfRef.__PVT__dispatchedDstRegNum
                       [0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] = 0U;
        }
    } else {
        vlSelfRef.__PVT__dispatchedSrcReady[1U][0U] = 1U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[1U][1U]) {
        vlSelfRef.__Vlvbound_hdb6a0e9e__0 = vlSelfRef.__PVT__readyRV
            [4U];
        vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] 
            = vlSelfRef.__Vlvbound_hdb6a0e9e__0;
        if (((vlSelfRef.__PVT__wakeup[0U] & vlSelfRef.__PVT__wakeupDstValid
              [0U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [0U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[1U] & vlSelfRef.__PVT__wakeupDstValid
              [1U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [1U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[2U] & vlSelfRef.__PVT__wakeupDstValid
              [2U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [2U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[3U] & vlSelfRef.__PVT__wakeupDstValid
              [3U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [3U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[4U] & vlSelfRef.__PVT__wakeupDstValid
              [4U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [4U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][1U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] = 1U;
        }
        if (((vlSelfRef.__PVT__dispatch[0U] & vlSelfRef.__PVT__dispatchedDstValid
              [0U]) & (vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][1U] == vlSelfRef.__PVT__dispatchedDstRegNum
                       [0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] = 0U;
        }
    } else {
        vlSelfRef.__PVT__dispatchedSrcReady[1U][1U] = 1U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[1U][2U]) {
        vlSelfRef.__Vlvbound_hdb6a0e9e__0 = vlSelfRef.__PVT__readyRV
            [5U];
        vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] 
            = vlSelfRef.__Vlvbound_hdb6a0e9e__0;
        if (((vlSelfRef.__PVT__wakeup[0U] & vlSelfRef.__PVT__wakeupDstValid
              [0U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [0U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[1U] & vlSelfRef.__PVT__wakeupDstValid
              [1U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [1U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[2U] & vlSelfRef.__PVT__wakeupDstValid
              [2U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [2U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[3U] & vlSelfRef.__PVT__wakeupDstValid
              [3U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [3U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__wakeup[4U] & vlSelfRef.__PVT__wakeupDstValid
              [4U]) & (vlSelfRef.__PVT__wakeupDstRegNum
                       [4U] == vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][2U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] = 1U;
        }
        if (((vlSelfRef.__PVT__dispatch[0U] & vlSelfRef.__PVT__dispatchedDstValid
              [0U]) & (vlSelfRef.__PVT__dispatchedSrcRegNum
                       [1U][2U] == vlSelfRef.__PVT__dispatchedDstRegNum
                       [0U]))) {
            vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] = 0U;
        }
    } else {
        vlSelfRef.__PVT__dispatchedSrcReady[1U][2U] = 1U;
    }
    __Vlvbound_hb0278605__0 = vlSelfRef.__PVT__wakeupDstRegNum
        [0U];
    vlSelfRef.__PVT__readyWA[0U] = __Vlvbound_hb0278605__0;
    __Vlvbound_hb0278605__0 = vlSelfRef.__PVT__wakeupDstRegNum
        [1U];
    vlSelfRef.__PVT__readyWA[1U] = __Vlvbound_hb0278605__0;
    __Vlvbound_hb0278605__0 = vlSelfRef.__PVT__wakeupDstRegNum
        [2U];
    vlSelfRef.__PVT__readyWA[2U] = __Vlvbound_hb0278605__0;
    __Vlvbound_hb0278605__0 = vlSelfRef.__PVT__wakeupDstRegNum
        [3U];
    vlSelfRef.__PVT__readyWA[3U] = __Vlvbound_hb0278605__0;
    __Vlvbound_hb0278605__0 = vlSelfRef.__PVT__wakeupDstRegNum
        [4U];
    vlSelfRef.__PVT__readyWA[4U] = __Vlvbound_hb0278605__0;
    __Vlvbound_h1a9bf5a5__0 = vlSelfRef.__PVT__dispatchedDstRegNum
        [0U];
    vlSelfRef.__PVT__readyWA[5U] = __Vlvbound_h1a9bf5a5__0;
    __Vlvbound_h1a9bf5a5__0 = vlSelfRef.__PVT__dispatchedDstRegNum
        [1U];
    vlSelfRef.__PVT__readyWA[6U] = __Vlvbound_h1a9bf5a5__0;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__readyWE[0U] = 0U;
        vlSelfRef.__PVT__readyWE[1U] = 0U;
        vlSelfRef.__PVT__readyWE[2U] = 0U;
        vlSelfRef.__PVT__readyWE[3U] = 0U;
        vlSelfRef.__PVT__readyWE[4U] = 0U;
        vlSelfRef.__PVT__readyWE[5U] = 0U;
        vlSelfRef.__PVT__readyWE[6U] = 0U;
        vlSelfRef.__PVT__readyWE[0U] = 1U;
        vlSelfRef.__PVT__readyWA[0U] = vlSelfRef.__PVT__resetIndex;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[0U] 
        = vlSelfRef.__PVT__readyWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[1U] 
        = vlSelfRef.__PVT__readyWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[2U] 
        = vlSelfRef.__PVT__readyWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[3U] 
        = vlSelfRef.__PVT__readyWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[4U] 
        = vlSelfRef.__PVT__readyWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[5U] 
        = vlSelfRef.__PVT__readyWE[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[6U] 
        = vlSelfRef.__PVT__readyWE[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[0U] 
        = vlSelfRef.__PVT__readyWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[1U] 
        = vlSelfRef.__PVT__readyWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[2U] 
        = vlSelfRef.__PVT__readyWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[3U] 
        = vlSelfRef.__PVT__readyWA[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[4U] 
        = vlSelfRef.__PVT__readyWA[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[5U] 
        = vlSelfRef.__PVT__readyWA[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[6U] 
        = vlSelfRef.__PVT__readyWA[6U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__0(VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__resetIndex = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart)
                                    ? 0U : (0x7fU & 
                                            ((IData)(1U) 
                                             + (IData)(vlSelfRef.__PVT__resetIndex))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__1(VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ __Vlvbound_he4e9cc9d__0;
    __Vlvbound_he4e9cc9d__0 = 0;
    // Body
    if (vlSelfRef.__PVT__dispatchedSrcValid[0U][0U]) {
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk6__DOT__k = 5U;
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk7__DOT__k = 0U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[0U][1U]) {
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk6__DOT__k = 5U;
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk7__DOT__k = 0U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[0U][2U]) {
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk6__DOT__k = 5U;
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk7__DOT__k = 0U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[1U][0U]) {
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk6__DOT__k = 5U;
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk7__DOT__k = 1U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[1U][1U]) {
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk6__DOT__k = 5U;
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk7__DOT__k = 1U;
    }
    if (vlSelfRef.__PVT__dispatchedSrcValid[1U][2U]) {
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk6__DOT__k = 5U;
        vlSelfRef.__PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk7__DOT__k = 1U;
    }
    __Vlvbound_he4e9cc9d__0 = vlSelfRef.__PVT__dispatchedSrcRegNum
        [0U][0U];
    vlSelfRef.__PVT__readyRA[0U] = __Vlvbound_he4e9cc9d__0;
    __Vlvbound_he4e9cc9d__0 = vlSelfRef.__PVT__dispatchedSrcRegNum
        [0U][1U];
    vlSelfRef.__PVT__readyRA[1U] = __Vlvbound_he4e9cc9d__0;
    __Vlvbound_he4e9cc9d__0 = vlSelfRef.__PVT__dispatchedSrcRegNum
        [0U][2U];
    vlSelfRef.__PVT__readyRA[2U] = __Vlvbound_he4e9cc9d__0;
    __Vlvbound_he4e9cc9d__0 = vlSelfRef.__PVT__dispatchedSrcRegNum
        [1U][0U];
    vlSelfRef.__PVT__readyRA[3U] = __Vlvbound_he4e9cc9d__0;
    __Vlvbound_he4e9cc9d__0 = vlSelfRef.__PVT__dispatchedSrcRegNum
        [1U][1U];
    vlSelfRef.__PVT__readyRA[4U] = __Vlvbound_he4e9cc9d__0;
    __Vlvbound_he4e9cc9d__0 = vlSelfRef.__PVT__dispatchedSrcRegNum
        [1U][2U];
    vlSelfRef.__PVT__readyRA[5U] = __Vlvbound_he4e9cc9d__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__ra[0U] 
        = vlSelfRef.__PVT__readyRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__ra[1U] 
        = vlSelfRef.__PVT__readyRA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__ra[2U] 
        = vlSelfRef.__PVT__readyRA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__ra[3U] 
        = vlSelfRef.__PVT__readyRA[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__ra[4U] 
        = vlSelfRef.__PVT__readyRA[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__ra[5U] 
        = vlSelfRef.__PVT__readyRA[5U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__2(VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__radyBitTable____pinNumber6[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__radyBitTable____pinNumber6[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__rv
        [1U];
    vlSelfRef.__Vcellout__radyBitTable____pinNumber6[2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__rv
        [2U];
    vlSelfRef.__Vcellout__radyBitTable____pinNumber6[3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__rv
        [3U];
    vlSelfRef.__Vcellout__radyBitTable____pinNumber6[4U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__rv
        [4U];
    vlSelfRef.__Vcellout__radyBitTable____pinNumber6[5U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__rv
        [5U];
    vlSelfRef.__PVT__readyRV[0U] = vlSelfRef.__Vcellout__radyBitTable____pinNumber6
        [0U];
    vlSelfRef.__PVT__readyRV[1U] = vlSelfRef.__Vcellout__radyBitTable____pinNumber6
        [1U];
    vlSelfRef.__PVT__readyRV[2U] = vlSelfRef.__Vcellout__radyBitTable____pinNumber6
        [2U];
    vlSelfRef.__PVT__readyRV[3U] = vlSelfRef.__Vcellout__radyBitTable____pinNumber6
        [3U];
    vlSelfRef.__PVT__readyRV[4U] = vlSelfRef.__Vcellout__radyBitTable____pinNumber6
        [4U];
    vlSelfRef.__PVT__readyRV[5U] = vlSelfRef.__Vcellout__radyBitTable____pinNumber6
        [5U];
}
