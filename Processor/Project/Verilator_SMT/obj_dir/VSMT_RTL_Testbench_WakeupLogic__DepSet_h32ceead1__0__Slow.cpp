// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_WakeupLogic.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_WakeupLogic___stl_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__0(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___stl_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__0\n"); );
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
    vlSelfRef.__Vcellinp__producerMatrix__dispatchPtr[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [0U];
    vlSelfRef.__Vcellinp__producerMatrix__dispatchPtr[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [1U];
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
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatch[0U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatch
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatch[1U] 
        = vlSelfRef.__Vcellinp__regReadyBitTbl__dispatch
        [1U];
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
