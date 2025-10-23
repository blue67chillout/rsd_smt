// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Memory__Iz1.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_Memory__Iz1___ico_sequent__TOP__SMT_RTL_Testbench__memory__0(VSMT_RTL_Testbench_Memory__Iz1* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Memory__Iz1___ico_sequent__TOP__SMT_RTL_Testbench__memory__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__nextMemPipeReg[0U] = ((0xfU & 
                                            vlSelfRef.__PVT__nextMemPipeReg[0U]) 
                                           | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv) 
                                              << 4U));
    vlSelfRef.__PVT__nextMemPipeReg[1U] = (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv) 
                                            >> 0x1cU) 
                                           | ((IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv 
                                                       >> 0x20U)) 
                                              << 4U));
    vlSelfRef.__PVT__nextMemPipeReg[2U] = (0x1fU & 
                                           (((IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv 
                                                      >> 0x20U)) 
                                             >> 0x1cU) 
                                            | (0x7ffffff0U 
                                               & (((IData)(vlSelfRef.__PVT__hasRequestReg) 
                                                   << 4U) 
                                                  & (vlSelfRef.__PVT__requestDataReg[3U] 
                                                     >> 1U)))));
    vlSelfRef.__PVT__nextMemPipeReg[0U] = ((0xfffffff0U 
                                            & vlSelfRef.__PVT__nextMemPipeReg[0U]) 
                                           | (0xfU 
                                              & vlSelfRef.__PVT__requestDataReg[0U]));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Memory__Iz1___act_comb__TOP__SMT_RTL_Testbench__memory__0(VSMT_RTL_Testbench_Memory__Iz1* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Memory__Iz1___act_comb__TOP__SMT_RTL_Testbench__memory__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__nextProcessLatencyCount = (3U 
                                                & ((0U 
                                                    != (IData)(vlSelfRef.__PVT__processLatencyCount))
                                                    ? 
                                                   ((IData)(vlSelfRef.__PVT__processLatencyCount) 
                                                    - (IData)(1U))
                                                    : 
                                                   ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessRE)
                                                     ? 2U
                                                     : 
                                                    ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWE)
                                                      ? 2U
                                                      : 0U))));
    vlSelfRef.__PVT__nextNextMemReadSerial = (3U & 
                                              ((IData)(vlSelfRef.__PVT__prevMemReadAccessAck)
                                                ? ((IData)(1U) 
                                                   + (IData)(vlSelfRef.__PVT__nextMemReadSerial))
                                                : (IData)(vlSelfRef.__PVT__nextMemReadSerial)));
    vlSelfRef.__PVT__nextNextMemWriteSerial = (1U & 
                                               ((IData)(vlSelfRef.__PVT__prevMemWriteAccessAck)
                                                 ? 
                                                ((IData)(1U) 
                                                 + (IData)(vlSelfRef.__PVT__nextMemWriteSerial))
                                                 : (IData)(vlSelfRef.__PVT__nextMemWriteSerial)));
    vlSelfRef.__PVT__memReadAccessAck = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessRE) 
                                         & (0U == (IData)(vlSelfRef.__PVT__processLatencyCount)));
    vlSelfRef.__PVT__memWriteAccessAck = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWE) 
                                          & (0U == (IData)(vlSelfRef.__PVT__processLatencyCount)));
    vlSelfRef.__PVT__pushRequestQueue = ((IData)(vlSelfRef.__PVT__memReadAccessAck) 
                                         | (IData)(vlSelfRef.__PVT__memWriteAccessAck));
    vlSelfRef.__PVT__pushedData[0U] = ((0xfU & vlSelfRef.__PVT__pushedData[0U]) 
                                       | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWriteData) 
                                          << 4U));
    vlSelfRef.__PVT__pushedData[1U] = (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWriteData) 
                                        >> 0x1cU) | 
                                       ((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWriteData 
                                                 >> 0x20U)) 
                                        << 4U));
    vlSelfRef.__PVT__pushedData[2U] = (((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWriteData 
                                                 >> 0x20U)) 
                                        >> 0x1cU) | 
                                       (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessAddr 
                                        << 4U));
    vlSelfRef.__PVT__pushedData[3U] = (0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessAddr 
                                                 >> 0x1cU) 
                                                | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessRE) 
                                                    << 5U) 
                                                   | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWE) 
                                                      << 4U))));
    vlSelfRef.__PVT__pushedData[0U] = ((0xfffffff0U 
                                        & vlSelfRef.__PVT__pushedData[0U]) 
                                       | (((IData)(vlSelfRef.__PVT__nextNextMemReadSerial) 
                                           << 2U) | 
                                          (((IData)(vlSelfRef.__PVT__nextNextMemWriteSerial) 
                                            << 1U) 
                                           | (IData)(vlSelfRef.__PVT__memWriteAccessAck))));
    vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextTailStorage 
        = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage;
    vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextCount 
        = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount;
    if (vlSelfRef.__PVT__pushRequestQueue) {
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextTailStorage 
            = ((0x7fU == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage))
                ? 0U : (0x7fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextTailStorage))));
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextCount 
            = (0xffU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextCount)));
    }
    if (vlSelfRef.__PVT__memReqQueue__DOT__pop) {
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextCount 
            = (0xffU & ((IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextCount) 
                        - (IData)(1U)));
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Memory__Iz1___nba_sequent__TOP__SMT_RTL_Testbench__memory__0(VSMT_RTL_Testbench_Memory__Iz1* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Memory__Iz1___nba_sequent__TOP__SMT_RTL_Testbench__memory__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __VdlySet__memPipeReg__v0;
    __VdlySet__memPipeReg__v0 = 0;
    VlWide<3>/*68:0*/ __VdlyVal__memPipeReg__v5;
    VL_ZERO_W(69, __VdlyVal__memPipeReg__v5);
    CData/*0:0*/ __VdlySet__memPipeReg__v5;
    __VdlySet__memPipeReg__v5 = 0;
    VlWide<3>/*68:0*/ __VdlyVal__memPipeReg__v6;
    VL_ZERO_W(69, __VdlyVal__memPipeReg__v6);
    CData/*0:0*/ __VdlySet__memPipeReg__v6;
    __VdlySet__memPipeReg__v6 = 0;
    VlWide<3>/*68:0*/ __VdlyVal__memPipeReg__v7;
    VL_ZERO_W(69, __VdlyVal__memPipeReg__v7);
    CData/*0:0*/ __VdlySet__memPipeReg__v7;
    __VdlySet__memPipeReg__v7 = 0;
    VlWide<3>/*68:0*/ __VdlyVal__memPipeReg__v8;
    VL_ZERO_W(69, __VdlyVal__memPipeReg__v8);
    CData/*0:0*/ __VdlySet__memPipeReg__v8;
    __VdlySet__memPipeReg__v8 = 0;
    VlWide<3>/*68:0*/ __VdlyVal__memPipeReg__v9;
    VL_ZERO_W(69, __VdlyVal__memPipeReg__v9);
    CData/*0:0*/ __VdlySet__memPipeReg__v9;
    __VdlySet__memPipeReg__v9 = 0;
    VlWide<4>/*101:0*/ __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0;
    VL_ZERO_W(102, __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0);
    CData/*6:0*/ __VdlyDim0__memReqQueue__DOT__memoryRequestQueue__v0;
    __VdlyDim0__memReqQueue__DOT__memoryRequestQueue__v0 = 0;
    CData/*0:0*/ __VdlySet__memReqQueue__DOT__memoryRequestQueue__v0;
    __VdlySet__memReqQueue__DOT__memoryRequestQueue__v0 = 0;
    // Body
    if (VL_UNLIKELY((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessRE) 
                      & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWE))))) {
        VL_WRITEF_NX("Cannot read and write the memory in the same cycle!\n",0);
    }
    if (VL_UNLIKELY(((0x80U == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount))))) {
        VL_WRITEF_NX("Cannot response so many memory request.\n",0);
    }
    if (VL_UNLIKELY((((0U == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount)) 
                      & (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pop))))) {
        VL_WRITEF_NX("Pop from an empty queue.\n",0);
    }
    if (VL_UNLIKELY((((0x80U == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount)) 
                      & (IData)(vlSelfRef.__PVT__pushRequestQueue))))) {
        VL_WRITEF_NX("Push to a full queue.\n",0);
    }
    __VdlySet__memReqQueue__DOT__memoryRequestQueue__v0 = 0U;
    __VdlySet__memPipeReg__v0 = 0U;
    __VdlySet__memPipeReg__v5 = 0U;
    __VdlySet__memPipeReg__v6 = 0U;
    __VdlySet__memPipeReg__v7 = 0U;
    __VdlySet__memPipeReg__v8 = 0U;
    __VdlySet__memPipeReg__v9 = 0U;
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk2__DOT__i = 4U;
    }
    if (vlSelfRef.__PVT__pushRequestQueue) {
        __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[0U] 
            = vlSelfRef.__PVT__pushedData[0U];
        __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[1U] 
            = vlSelfRef.__PVT__pushedData[1U];
        __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[2U] 
            = vlSelfRef.__PVT__pushedData[2U];
        __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[3U] 
            = vlSelfRef.__PVT__pushedData[3U];
        __VdlyDim0__memReqQueue__DOT__memoryRequestQueue__v0 
            = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage;
        __VdlySet__memReqQueue__DOT__memoryRequestQueue__v0 = 1U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk1__DOT__i = 5U;
        __VdlySet__memPipeReg__v0 = 1U;
        vlSelfRef.__PVT__requestDataReg[0U] = 0U;
        vlSelfRef.__PVT__requestDataReg[1U] = 0U;
        vlSelfRef.__PVT__requestDataReg[2U] = 0U;
        vlSelfRef.__PVT__requestDataReg[3U] = 0U;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage = 0U;
        vlSelfRef.__PVT__memReqQueue__DOT__randReg = 0x76775da2U;
        vlSelfRef.__PVT__memReqQueue__DOT__countReg = 0U;
        vlSelfRef.__PVT__processLatencyCount = 0U;
        vlSelfRef.__PVT__nextMemReadSerial = 0U;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount = 0U;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage = 0U;
    } else {
        __VdlyVal__memPipeReg__v5[0U] = vlSelfRef.__PVT__nextMemPipeReg[0U];
        __VdlyVal__memPipeReg__v5[1U] = vlSelfRef.__PVT__nextMemPipeReg[1U];
        __VdlyVal__memPipeReg__v5[2U] = vlSelfRef.__PVT__nextMemPipeReg[2U];
        __VdlySet__memPipeReg__v5 = 1U;
        vlSelfRef.__Vlvbound_hd17915af__0[0U] = vlSelfRef.__PVT__memPipeReg
            [0U][0U];
        vlSelfRef.__Vlvbound_hd17915af__0[1U] = vlSelfRef.__PVT__memPipeReg
            [0U][1U];
        vlSelfRef.__Vlvbound_hd17915af__0[2U] = vlSelfRef.__PVT__memPipeReg
            [0U][2U];
        __VdlyVal__memPipeReg__v6[0U] = vlSelfRef.__Vlvbound_hd17915af__0[0U];
        __VdlyVal__memPipeReg__v6[1U] = vlSelfRef.__Vlvbound_hd17915af__0[1U];
        __VdlyVal__memPipeReg__v6[2U] = vlSelfRef.__Vlvbound_hd17915af__0[2U];
        __VdlySet__memPipeReg__v6 = 1U;
        vlSelfRef.__Vlvbound_hd17915af__0[0U] = vlSelfRef.__PVT__memPipeReg
            [1U][0U];
        vlSelfRef.__Vlvbound_hd17915af__0[1U] = vlSelfRef.__PVT__memPipeReg
            [1U][1U];
        vlSelfRef.__Vlvbound_hd17915af__0[2U] = vlSelfRef.__PVT__memPipeReg
            [1U][2U];
        __VdlyVal__memPipeReg__v7[0U] = vlSelfRef.__Vlvbound_hd17915af__0[0U];
        __VdlyVal__memPipeReg__v7[1U] = vlSelfRef.__Vlvbound_hd17915af__0[1U];
        __VdlyVal__memPipeReg__v7[2U] = vlSelfRef.__Vlvbound_hd17915af__0[2U];
        __VdlySet__memPipeReg__v7 = 1U;
        vlSelfRef.__Vlvbound_hd17915af__0[0U] = vlSelfRef.__PVT__memPipeReg
            [2U][0U];
        vlSelfRef.__Vlvbound_hd17915af__0[1U] = vlSelfRef.__PVT__memPipeReg
            [2U][1U];
        vlSelfRef.__Vlvbound_hd17915af__0[2U] = vlSelfRef.__PVT__memPipeReg
            [2U][2U];
        __VdlyVal__memPipeReg__v8[0U] = vlSelfRef.__Vlvbound_hd17915af__0[0U];
        __VdlyVal__memPipeReg__v8[1U] = vlSelfRef.__Vlvbound_hd17915af__0[1U];
        __VdlyVal__memPipeReg__v8[2U] = vlSelfRef.__Vlvbound_hd17915af__0[2U];
        __VdlySet__memPipeReg__v8 = 1U;
        vlSelfRef.__Vlvbound_hd17915af__0[0U] = vlSelfRef.__PVT__memPipeReg
            [3U][0U];
        vlSelfRef.__Vlvbound_hd17915af__0[1U] = vlSelfRef.__PVT__memPipeReg
            [3U][1U];
        vlSelfRef.__Vlvbound_hd17915af__0[2U] = vlSelfRef.__PVT__memPipeReg
            [3U][2U];
        __VdlyVal__memPipeReg__v9[0U] = vlSelfRef.__Vlvbound_hd17915af__0[0U];
        __VdlyVal__memPipeReg__v9[1U] = vlSelfRef.__Vlvbound_hd17915af__0[1U];
        __VdlyVal__memPipeReg__v9[2U] = vlSelfRef.__Vlvbound_hd17915af__0[2U];
        __VdlySet__memPipeReg__v9 = 1U;
        vlSelfRef.__PVT__requestDataReg[0U] = vlSelfRef.__PVT__requestData[0U];
        vlSelfRef.__PVT__requestDataReg[1U] = vlSelfRef.__PVT__requestData[1U];
        vlSelfRef.__PVT__requestDataReg[2U] = vlSelfRef.__PVT__requestData[2U];
        vlSelfRef.__PVT__requestDataReg[3U] = vlSelfRef.__PVT__requestData[3U];
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage 
            = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage;
        vlSelfRef.__PVT__memReqQueue__DOT__randReg 
            = vlSelfRef.__PVT__memReqQueue__DOT__randNext;
        vlSelfRef.__PVT__memReqQueue__DOT__countReg 
            = vlSelfRef.__PVT__memReqQueue__DOT__count;
        vlSelfRef.__PVT__processLatencyCount = vlSelfRef.__PVT__nextProcessLatencyCount;
        vlSelfRef.__PVT__nextMemReadSerial = vlSelfRef.__PVT__nextNextMemReadSerial;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount 
            = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextCount;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage 
            = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextTailStorage;
    }
    vlSelfRef.__PVT__hasRequestReg = ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                      && (IData)(vlSelfRef.__PVT__hasRequest));
    vlSelfRef.__PVT__prevMemReadAccessAck = ((1U & 
                                              (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                             && (IData)(vlSelfRef.__PVT__memReadAccessAck));
    vlSelfRef.__PVT__prevMemWriteAccessAck = ((1U & 
                                               (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                              && (IData)(vlSelfRef.__PVT__memWriteAccessAck));
    vlSelfRef.__PVT__nextMemWriteSerial = ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                           && (IData)(vlSelfRef.__PVT__nextNextMemWriteSerial));
    if (__VdlySet__memReqQueue__DOT__memoryRequestQueue__v0) {
        vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue[__VdlyDim0__memReqQueue__DOT__memoryRequestQueue__v0][0U] 
            = __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[0U];
        vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue[__VdlyDim0__memReqQueue__DOT__memoryRequestQueue__v0][1U] 
            = __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[1U];
        vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue[__VdlyDim0__memReqQueue__DOT__memoryRequestQueue__v0][2U] 
            = __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[2U];
        vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue[__VdlyDim0__memReqQueue__DOT__memoryRequestQueue__v0][3U] 
            = __VdlyVal__memReqQueue__DOT__memoryRequestQueue__v0[3U];
    }
    if (__VdlySet__memPipeReg__v0) {
        vlSelfRef.__PVT__memPipeReg[0U][0U] = 0U;
        vlSelfRef.__PVT__memPipeReg[0U][1U] = 0U;
        vlSelfRef.__PVT__memPipeReg[0U][2U] = 0U;
        vlSelfRef.__PVT__memPipeReg[1U][0U] = 0U;
        vlSelfRef.__PVT__memPipeReg[1U][1U] = 0U;
        vlSelfRef.__PVT__memPipeReg[1U][2U] = 0U;
        vlSelfRef.__PVT__memPipeReg[2U][0U] = 0U;
        vlSelfRef.__PVT__memPipeReg[2U][1U] = 0U;
        vlSelfRef.__PVT__memPipeReg[2U][2U] = 0U;
        vlSelfRef.__PVT__memPipeReg[3U][0U] = 0U;
        vlSelfRef.__PVT__memPipeReg[3U][1U] = 0U;
        vlSelfRef.__PVT__memPipeReg[3U][2U] = 0U;
        vlSelfRef.__PVT__memPipeReg[4U][0U] = 0U;
        vlSelfRef.__PVT__memPipeReg[4U][1U] = 0U;
        vlSelfRef.__PVT__memPipeReg[4U][2U] = 0U;
    }
    if (__VdlySet__memPipeReg__v5) {
        vlSelfRef.__PVT__memPipeReg[0U][0U] = __VdlyVal__memPipeReg__v5[0U];
        vlSelfRef.__PVT__memPipeReg[0U][1U] = __VdlyVal__memPipeReg__v5[1U];
        vlSelfRef.__PVT__memPipeReg[0U][2U] = __VdlyVal__memPipeReg__v5[2U];
    }
    if (__VdlySet__memPipeReg__v6) {
        vlSelfRef.__PVT__memPipeReg[1U][0U] = __VdlyVal__memPipeReg__v6[0U];
        vlSelfRef.__PVT__memPipeReg[1U][1U] = __VdlyVal__memPipeReg__v6[1U];
        vlSelfRef.__PVT__memPipeReg[1U][2U] = __VdlyVal__memPipeReg__v6[2U];
    }
    if (__VdlySet__memPipeReg__v7) {
        vlSelfRef.__PVT__memPipeReg[2U][0U] = __VdlyVal__memPipeReg__v7[0U];
        vlSelfRef.__PVT__memPipeReg[2U][1U] = __VdlyVal__memPipeReg__v7[1U];
        vlSelfRef.__PVT__memPipeReg[2U][2U] = __VdlyVal__memPipeReg__v7[2U];
    }
    if (__VdlySet__memPipeReg__v8) {
        vlSelfRef.__PVT__memPipeReg[3U][0U] = __VdlyVal__memPipeReg__v8[0U];
        vlSelfRef.__PVT__memPipeReg[3U][1U] = __VdlyVal__memPipeReg__v8[1U];
        vlSelfRef.__PVT__memPipeReg[3U][2U] = __VdlyVal__memPipeReg__v8[2U];
    }
    if (__VdlySet__memPipeReg__v9) {
        vlSelfRef.__PVT__memPipeReg[4U][0U] = __VdlyVal__memPipeReg__v9[0U];
        vlSelfRef.__PVT__memPipeReg[4U][1U] = __VdlyVal__memPipeReg__v9[1U];
        vlSelfRef.__PVT__memPipeReg[4U][2U] = __VdlyVal__memPipeReg__v9[2U];
    }
    vlSelfRef.__PVT__nextMemPipeReg[0U] = ((0xfU & 
                                            vlSelfRef.__PVT__nextMemPipeReg[0U]) 
                                           | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv) 
                                              << 4U));
    vlSelfRef.__PVT__nextMemPipeReg[1U] = (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv) 
                                            >> 0x1cU) 
                                           | ((IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv 
                                                       >> 0x20U)) 
                                              << 4U));
    vlSelfRef.__PVT__nextMemPipeReg[2U] = (0x1fU & 
                                           (((IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram.__PVT__rv 
                                                      >> 0x20U)) 
                                             >> 0x1cU) 
                                            | (0x7ffffff0U 
                                               & (((IData)(vlSelfRef.__PVT__hasRequestReg) 
                                                   << 4U) 
                                                  & (vlSelfRef.__PVT__requestDataReg[3U] 
                                                     >> 1U)))));
    vlSelfRef.__PVT__nextMemPipeReg[0U] = ((0xfffffff0U 
                                            & vlSelfRef.__PVT__nextMemPipeReg[0U]) 
                                           | (0xfU 
                                              & vlSelfRef.__PVT__requestDataReg[0U]));
    vlSelfRef.__PVT__requestData[0U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][0U];
    vlSelfRef.__PVT__requestData[1U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][1U];
    vlSelfRef.__PVT__requestData[2U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][2U];
    vlSelfRef.__PVT__requestData[3U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][3U];
    vlSelfRef.__PVT__memReqQueue__DOT__randNext = vlSelfRef.__PVT__memReqQueue__DOT__randReg;
    vlSelfRef.__PVT__memReqQueue__DOT__count = vlSelfRef.__PVT__memReqQueue__DOT__countReg;
    if ((0U == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount))) {
        vlSelfRef.__PVT__memReqQueue__DOT__pop = 0U;
    } else if (((IData)(vlSelfRef.__PVT__memReqQueue__DOT__count) 
                == VL_MODDIV_III(32, vlSelfRef.__PVT__memReqQueue__DOT__randReg, (IData)(0xaU)))) {
        vlSelfRef.__PVT__memReqQueue__DOT__pop = 1U;
        vlSelfRef.__PVT__memReqQueue__DOT__count = 0U;
        vlSelfRef.__PVT__memReqQueue__DOT__randNext 
            = (vlSelfRef.__PVT__memReqQueue__DOT__randNext 
               ^ VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__memReqQueue__DOT__randNext, 0xdU));
        vlSelfRef.__PVT__memReqQueue__DOT__randNext 
            = (vlSelfRef.__PVT__memReqQueue__DOT__randNext 
               ^ VL_SHIFTR_III(32,32,32, vlSelfRef.__PVT__memReqQueue__DOT__randNext, 0x11U));
        vlSelfRef.__PVT__memReqQueue__DOT__randNext 
            = (vlSelfRef.__PVT__memReqQueue__DOT__randNext 
               ^ VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__memReqQueue__DOT__randNext, 5U));
    } else {
        vlSelfRef.__PVT__memReqQueue__DOT__count = 
            (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__memReqQueue__DOT__count)));
        vlSelfRef.__PVT__memReqQueue__DOT__pop = 0U;
    }
    if (vlSelfRef.__PVT__memReqQueue__DOT__pop) {
        vlSelfRef.__PVT__hasRequest = 1U;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage 
            = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage 
            = ((0x7fU == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage))
                ? 0U : (0x7fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage))));
    } else {
        vlSelfRef.__PVT__hasRequest = 0U;
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage 
            = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage;
    }
}
