// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_sequent__TOP__SMT_RTL_Testbench__core__4(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_sequent__TOP__SMT_RTL_Testbench__core__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<5>/*138:0*/ complexRwStage__DOT____Vlvbound_hfc50245b__0;
    VL_ZERO_W(139, complexRwStage__DOT____Vlvbound_hfc50245b__0);
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h0285a07a__0;
    complexRwStage__DOT____Vlvbound_h0285a07a__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h144dd5b6__0;
    complexRwStage__DOT____Vlvbound_h144dd5b6__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h046b483c__0;
    complexRwStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h8546e2b5__0;
    complexRwStage__DOT____Vlvbound_h8546e2b5__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h1619d9fe__0;
    complexRwStage__DOT____Vlvbound_h1619d9fe__0 = 0;
    CData/*6:0*/ complexRwStage__DOT____Vlvbound_h9f79b102__0;
    complexRwStage__DOT____Vlvbound_h9f79b102__0 = 0;
    QData/*32:0*/ complexRwStage__DOT____Vlvbound_h7efdbe27__0;
    complexRwStage__DOT____Vlvbound_h7efdbe27__0 = 0;
    CData/*5:0*/ complexRwStage__DOT____Vlvbound_h8cf597d3__0;
    complexRwStage__DOT____Vlvbound_h8cf597d3__0 = 0;
    CData/*3:0*/ complexRwStage__DOT____Vlvbound_h851d8249__0;
    complexRwStage__DOT____Vlvbound_h851d8249__0 = 0;
    CData/*3:0*/ complexRwStage__DOT____Vlvbound_h851db172__0;
    complexRwStage__DOT____Vlvbound_h851db172__0 = 0;
    CData/*5:0*/ complexRwStage__DOT____Vlvbound_h8cf597d3__1;
    complexRwStage__DOT____Vlvbound_h8cf597d3__1 = 0;
    IData/*19:0*/ complexRwStage__DOT____Vlvbound_h7f5b6f62__0;
    complexRwStage__DOT____Vlvbound_h7f5b6f62__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h32678e71__0;
    complexRwStage__DOT____Vlvbound_h32678e71__0 = 0;
    VlWide<3>/*71:0*/ complexRwStage__DOT____Vlvbound_haccc5680__0;
    VL_ZERO_W(72, complexRwStage__DOT____Vlvbound_haccc5680__0);
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h59c1814f__0;
    complexRwStage__DOT____Vlvbound_h59c1814f__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h59ed0fc0__0;
    complexRwStage__DOT____Vlvbound_h59ed0fc0__0 = 0;
    SData/*11:0*/ complexRwStage__DOT____Vlvbound_h8bf3b354__0;
    complexRwStage__DOT____Vlvbound_h8bf3b354__0 = 0;
    CData/*0:0*/ memRwStage__DOT____Vlvbound_hb4b12956__0;
    memRwStage__DOT____Vlvbound_hb4b12956__0 = 0;
    CData/*6:0*/ memRwStage__DOT____Vlvbound_hca7847bb__0;
    memRwStage__DOT____Vlvbound_hca7847bb__0 = 0;
    QData/*32:0*/ memRwStage__DOT____Vlvbound_h6f0537ce__0;
    memRwStage__DOT____Vlvbound_h6f0537ce__0 = 0;
    VlWide<3>/*92:0*/ fpRwStage__DOT____Vlvbound_hb611356e__0;
    VL_ZERO_W(93, fpRwStage__DOT____Vlvbound_hb611356e__0);
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h0285a07a__0;
    fpRwStage__DOT____Vlvbound_h0285a07a__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h144dd5b6__0;
    fpRwStage__DOT____Vlvbound_h144dd5b6__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h046b483c__0;
    fpRwStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h8546e2b5__0;
    fpRwStage__DOT____Vlvbound_h8546e2b5__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h21fb6822__0;
    fpRwStage__DOT____Vlvbound_h21fb6822__0 = 0;
    CData/*6:0*/ fpRwStage__DOT____Vlvbound_h6f02ee4c__0;
    fpRwStage__DOT____Vlvbound_h6f02ee4c__0 = 0;
    QData/*32:0*/ fpRwStage__DOT____Vlvbound_he6dc3282__0;
    fpRwStage__DOT____Vlvbound_he6dc3282__0 = 0;
    CData/*5:0*/ fpRwStage__DOT____Vlvbound_h8cf597d3__0;
    fpRwStage__DOT____Vlvbound_h8cf597d3__0 = 0;
    CData/*3:0*/ fpRwStage__DOT____Vlvbound_h851d8249__0;
    fpRwStage__DOT____Vlvbound_h851d8249__0 = 0;
    CData/*3:0*/ fpRwStage__DOT____Vlvbound_h851db172__0;
    fpRwStage__DOT____Vlvbound_h851db172__0 = 0;
    IData/*19:0*/ fpRwStage__DOT____Vlvbound_h7f5b6f62__0;
    fpRwStage__DOT____Vlvbound_h7f5b6f62__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_he995c148__0;
    fpRwStage__DOT____Vlvbound_he995c148__0 = 0;
    VlWide<3>/*71:0*/ fpRwStage__DOT____Vlvbound_h8af695fc__0;
    VL_ZERO_W(72, fpRwStage__DOT____Vlvbound_h8af695fc__0);
    CData/*4:0*/ fpRwStage__DOT____Vlvbound_h1d55e97c__0;
    fpRwStage__DOT____Vlvbound_h1d55e97c__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h48155c9e__0;
    fpRwStage__DOT____Vlvbound_h48155c9e__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h48154c0c__0;
    fpRwStage__DOT____Vlvbound_h48154c0c__0 = 0;
    SData/*11:0*/ fpRwStage__DOT____Vlvbound_h5e9ebd83__0;
    fpRwStage__DOT____Vlvbound_h5e9ebd83__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__548__detectRange;
    __Vfunc_SelectiveFlushDetector__548__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__548__headPtr;
    __Vfunc_SelectiveFlushDetector__548__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__548__tailPtr;
    __Vfunc_SelectiveFlushDetector__548__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__548__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__548__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__548__opPtr;
    __Vfunc_SelectiveFlushDetector__548__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__554__detectRange;
    __Vfunc_SelectiveFlushDetector__554__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__554__headPtr;
    __Vfunc_SelectiveFlushDetector__554__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__554__tailPtr;
    __Vfunc_SelectiveFlushDetector__554__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__554__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__554__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__554__opPtr;
    __Vfunc_SelectiveFlushDetector__554__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__618__detectRange;
    __Vfunc_SelectiveFlushDetector__618__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__618__headPtr;
    __Vfunc_SelectiveFlushDetector__618__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__618__tailPtr;
    __Vfunc_SelectiveFlushDetector__618__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__618__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__618__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__618__opPtr;
    __Vfunc_SelectiveFlushDetector__618__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__630__detectRange;
    __Vfunc_SelectiveFlushDetector__630__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__630__headPtr;
    __Vfunc_SelectiveFlushDetector__630__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__630__tailPtr;
    __Vfunc_SelectiveFlushDetector__630__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__630__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__630__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__630__opPtr;
    __Vfunc_SelectiveFlushDetector__630__opPtr = 0;
    // Body
    vlSelfRef.__PVT__complexRwStage__DOT__stall = (1U 
                                                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                      >> 1U));
    vlSelfRef.__PVT__complexRwStage__DOT__clear = (1U 
                                                   & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[0U] 
        = ((vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
            [0U][2U] << 0x1fU) | (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                  [0U][1U] >> 1U));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[1U] 
        = ((vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
            [0U][3U] << 0x1fU) | (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                  [0U][2U] >> 1U));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[2U] 
        = (0x3ffffU & (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                       [0U][3U] >> 1U));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[3U] = 0U;
    complexRwStage__DOT____Vlvbound_hfc50245b__0[4U] = 0U;
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][0U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[0U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][1U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[1U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][2U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[2U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][3U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[3U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][4U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[4U];
    complexRwStage__DOT____Vlvbound_h0285a07a__0 = 
        (1U & vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
         [0U][1U]);
    vlSelfRef.__PVT__complexRwStage__DOT__regValid[0U] 
        = complexRwStage__DOT____Vlvbound_h0285a07a__0;
    complexRwStage__DOT____Vlvbound_h144dd5b6__0 = 
        (1U & (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
               [0U][3U] >> 0x13U));
    vlSelfRef.__PVT__complexRwStage__DOT__valid[0U] 
        = complexRwStage__DOT____Vlvbound_h144dd5b6__0;
    __Vfunc_SelectiveFlushDetector__554__opPtr = (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__554__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__554__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__554__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__554__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__554__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__554__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                goto __Vlabel163;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__554__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                    goto __Vlabel163;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
                    goto __Vlabel163;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__554__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                    goto __Vlabel163;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                    goto __Vlabel163;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
                    goto __Vlabel163;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
                goto __Vlabel163;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
        }
        __Vlabel163: ;
    }
    complexRwStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout;
    vlSelfRef.__PVT__complexRwStage__DOT__flush[0U] 
        = complexRwStage__DOT____Vlvbound_h046b483c__0;
    complexRwStage__DOT____Vlvbound_h8546e2b5__0 = 
        ((((~ (IData)(vlSelfRef.__PVT__complexRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__complexRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__complexRwStage__DOT__valid
          [0U]) & (~ vlSelfRef.__PVT__complexRwStage__DOT__flush
                   [0U]));
    vlSelfRef.__PVT__complexRwStage__DOT__update[0U] 
        = complexRwStage__DOT____Vlvbound_h8546e2b5__0;
    complexRwStage__DOT____Vlvbound_h1619d9fe__0 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__update
         [0U] & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                 [0U][0U] >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegWE[0U] 
        = complexRwStage__DOT____Vlvbound_h1619d9fe__0;
    complexRwStage__DOT____Vlvbound_h9f79b102__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                  [0U][0U] >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegNum[0U] 
        = complexRwStage__DOT____Vlvbound_h9f79b102__0;
    complexRwStage__DOT____Vlvbound_h7efdbe27__0 = 
        (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                            [0U][1U])) 
                            << 0x20U) | (QData)((IData)(
                                                        vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                                        [0U][0U]))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegData[0U] 
        = complexRwStage__DOT____Vlvbound_h7efdbe27__0;
    complexRwStage__DOT____Vlvbound_h8cf597d3__0 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__iqData
         [0U][1U] >> 0x1aU);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(complexRwStage__DOT____Vlvbound_h8cf597d3__0) 
                                  << 2U)));
    complexRwStage__DOT____Vlvbound_h851d8249__0 = 
        (0xfU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                 [0U][1U] >> 0x16U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(complexRwStage__DOT____Vlvbound_h851d8249__0) 
                         << 0x1eU));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(complexRwStage__DOT____Vlvbound_h851d8249__0) 
                                  >> 2U)));
    complexRwStage__DOT____Vlvbound_h851db172__0 = 
        (0xfU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                 [0U][1U] >> 0x12U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(complexRwStage__DOT____Vlvbound_h851db172__0) 
                         << 0x1aU));
    complexRwStage__DOT____Vlvbound_h8cf597d3__1 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__iqData
         [0U][1U] >> 0x1aU);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(complexRwStage__DOT____Vlvbound_h8cf597d3__1) 
                                  << 2U)));
    complexRwStage__DOT____Vlvbound_h7f5b6f62__0 = 
        (0xfffffU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                     [0U][0U] >> 1U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | (complexRwStage__DOT____Vlvbound_h7f5b6f62__0 
                         << 2U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][0U] 
        = (3U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][1U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | (((vlSelfRef.__PVT__complexRwStage__DOT__update
                           [0U] & vlSelfRef.__PVT__complexRwStage__DOT__regValid
                           [0U]) ? 1U : 0U) << 0x16U));
    complexRwStage__DOT____Vlvbound_h32678e71__0 = 
        vlSelfRef.__PVT__complexRwStage__DOT__update
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWrite[0U] 
        = complexRwStage__DOT____Vlvbound_h32678e71__0;
    complexRwStage__DOT____Vlvbound_haccc5680__0[0U] 
        = vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
        [0U][0U];
    complexRwStage__DOT____Vlvbound_haccc5680__0[1U] 
        = vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
        [0U][1U];
    complexRwStage__DOT____Vlvbound_haccc5680__0[2U] 
        = vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData[0U][0U] 
        = complexRwStage__DOT____Vlvbound_haccc5680__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData[0U][1U] 
        = complexRwStage__DOT____Vlvbound_haccc5680__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData[0U][2U] 
        = complexRwStage__DOT____Vlvbound_haccc5680__0[2U];
    vlSelfRef.__PVT__complexRwStage__DOT__unnamedblk3__DOT__i = 1U;
    complexRwStage__DOT____Vlvbound_h59c1814f__0 = 
        vlSelfRef.__PVT__complexRwStage__DOT__valid
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg
            [0U]) | ((IData)(complexRwStage__DOT____Vlvbound_h59c1814f__0) 
                     << 0xdU));
    complexRwStage__DOT____Vlvbound_h59ed0fc0__0 = 
        vlSelfRef.__PVT__complexRwStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg
            [0U]) | ((IData)(complexRwStage__DOT____Vlvbound_h59ed0fc0__0) 
                     << 0xcU));
    complexRwStage__DOT____Vlvbound_h8bf3b354__0 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
         [0U][3U] >> 0x14U);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg
            [0U]) | (IData)(complexRwStage__DOT____Vlvbound_h8bf3b354__0));
    vlSelfRef.__PVT__complexRwStage__DOT__unnamedblk4__DOT__i = 1U;
    vlSelfRef.__PVT__fpRwStage__DOT__stall = (1U & 
                                              ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                               >> 1U));
    vlSelfRef.__PVT__fpRwStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    fpRwStage__DOT____Vlvbound_hb611356e__0[0U] = (
                                                   (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                    [0U][2U] 
                                                    << 0x1aU) 
                                                   | (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                      [0U][1U] 
                                                      >> 6U));
    fpRwStage__DOT____Vlvbound_hb611356e__0[1U] = (
                                                   (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                    [0U][3U] 
                                                    << 0x1aU) 
                                                   | (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                      [0U][2U] 
                                                      >> 6U));
    fpRwStage__DOT____Vlvbound_hb611356e__0[2U] = (0x1fffffffU 
                                                   & ((vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                       [0U][4U] 
                                                       << 0x1aU) 
                                                      | (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                         [0U][3U] 
                                                         >> 6U)));
    vlSelfRef.__PVT__fpRwStage__DOT__iqData[0U][0U] 
        = fpRwStage__DOT____Vlvbound_hb611356e__0[0U];
    vlSelfRef.__PVT__fpRwStage__DOT__iqData[0U][1U] 
        = fpRwStage__DOT____Vlvbound_hb611356e__0[1U];
    vlSelfRef.__PVT__fpRwStage__DOT__iqData[0U][2U] 
        = fpRwStage__DOT____Vlvbound_hb611356e__0[2U];
    fpRwStage__DOT____Vlvbound_h0285a07a__0 = (1U & 
                                               (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                [0U][1U] 
                                                >> 5U));
    vlSelfRef.__PVT__fpRwStage__DOT__regValid[0U] = fpRwStage__DOT____Vlvbound_h0285a07a__0;
    fpRwStage__DOT____Vlvbound_h144dd5b6__0 = (1U & 
                                               (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                [0U][4U] 
                                                >> 3U));
    vlSelfRef.__PVT__fpRwStage__DOT__valid[0U] = fpRwStage__DOT____Vlvbound_h144dd5b6__0;
    __Vfunc_SelectiveFlushDetector__630__opPtr = (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__630__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__630__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__630__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__630__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__630__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__630__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                goto __Vlabel164;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__630__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                    goto __Vlabel164;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
                    goto __Vlabel164;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__630__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                    goto __Vlabel164;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                    goto __Vlabel164;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
                    goto __Vlabel164;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
                goto __Vlabel164;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
        }
        __Vlabel164: ;
    }
    fpRwStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout;
    vlSelfRef.__PVT__fpRwStage__DOT__flush[0U] = fpRwStage__DOT____Vlvbound_h046b483c__0;
    fpRwStage__DOT____Vlvbound_h8546e2b5__0 = ((((~ (IData)(vlSelfRef.__PVT__fpRwStage__DOT__stall)) 
                                                 & (~ (IData)(vlSelfRef.__PVT__fpRwStage__DOT__clear))) 
                                                & vlSelfRef.__PVT__fpRwStage__DOT__valid
                                                [0U]) 
                                               & (~ 
                                                  vlSelfRef.__PVT__fpRwStage__DOT__flush
                                                  [0U]));
    vlSelfRef.__PVT__fpRwStage__DOT__update[0U] = fpRwStage__DOT____Vlvbound_h8546e2b5__0;
    fpRwStage__DOT____Vlvbound_h21fb6822__0 = (vlSelfRef.__PVT__fpRwStage__DOT__update
                                               [0U] 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][0U] 
                                                  >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegWE[0U] 
        = fpRwStage__DOT____Vlvbound_h21fb6822__0;
    fpRwStage__DOT____Vlvbound_h6f02ee4c__0 = (0x7fU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][0U] 
                                                  >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum[0U] 
        = fpRwStage__DOT____Vlvbound_h6f02ee4c__0;
    fpRwStage__DOT____Vlvbound_he6dc3282__0 = (0x1ffffffffULL 
                                               & (((QData)((IData)(
                                                                   vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                                   [0U][1U])) 
                                                   << 0x1bU) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                                     [0U][0U])) 
                                                     >> 5U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegData[0U] 
        = fpRwStage__DOT____Vlvbound_he6dc3282__0;
    fpRwStage__DOT____Vlvbound_h8cf597d3__0 = (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                               [0U][1U] 
                                               >> 0x1aU);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(fpRwStage__DOT____Vlvbound_h8cf597d3__0) 
                                  << 2U)));
    fpRwStage__DOT____Vlvbound_h851d8249__0 = (0xfU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x16U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(fpRwStage__DOT____Vlvbound_h851d8249__0) 
                         << 0x1eU));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(fpRwStage__DOT____Vlvbound_h851d8249__0) 
                                  >> 2U)));
    fpRwStage__DOT____Vlvbound_h851db172__0 = (0xfU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x12U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(fpRwStage__DOT____Vlvbound_h851db172__0) 
                         << 0x1aU));
    fpRwStage__DOT____Vlvbound_h7f5b6f62__0 = (0xfffffU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][0U] 
                                                  >> 1U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | (fpRwStage__DOT____Vlvbound_h7f5b6f62__0 
                         << 2U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][0U] 
        = (3U & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][1U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | (((vlSelfRef.__PVT__fpRwStage__DOT__update
                           [0U] & vlSelfRef.__PVT__fpRwStage__DOT__regValid
                           [0U]) ? 1U : 0U) << 0x16U));
    fpRwStage__DOT____Vlvbound_he995c148__0 = vlSelfRef.__PVT__fpRwStage__DOT__update
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWrite[0U] 
        = fpRwStage__DOT____Vlvbound_he995c148__0;
    fpRwStage__DOT____Vlvbound_h8af695fc__0[0U] = vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
        [0U][0U];
    fpRwStage__DOT____Vlvbound_h8af695fc__0[1U] = vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
        [0U][1U];
    fpRwStage__DOT____Vlvbound_h8af695fc__0[2U] = vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData[0U][0U] 
        = fpRwStage__DOT____Vlvbound_h8af695fc__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData[0U][1U] 
        = fpRwStage__DOT____Vlvbound_h8af695fc__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData[0U][2U] 
        = fpRwStage__DOT____Vlvbound_h8af695fc__0[2U];
    fpRwStage__DOT____Vlvbound_h1d55e97c__0 = (0x1fU 
                                               & vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                               [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpFFlagsData[0U] 
        = fpRwStage__DOT____Vlvbound_h1d55e97c__0;
    vlSelfRef.__PVT__fpRwStage__DOT__unnamedblk3__DOT__i = 1U;
    fpRwStage__DOT____Vlvbound_h48155c9e__0 = vlSelfRef.__PVT__fpRwStage__DOT__valid
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg
            [0U]) | ((IData)(fpRwStage__DOT____Vlvbound_h48155c9e__0) 
                     << 0xdU));
    fpRwStage__DOT____Vlvbound_h48154c0c__0 = vlSelfRef.__PVT__fpRwStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg
            [0U]) | ((IData)(fpRwStage__DOT____Vlvbound_h48154c0c__0) 
                     << 0xcU));
    fpRwStage__DOT____Vlvbound_h5e9ebd83__0 = (0xfffU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                  [0U][4U] 
                                                  >> 4U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg
            [0U]) | (IData)(fpRwStage__DOT____Vlvbound_h5e9ebd83__0));
    vlSelfRef.__PVT__fpRwStage__DOT__unnamedblk4__DOT__i = 1U;
    vlSelfRef.__PVT__intRwStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__intRwStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][2U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][3U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][4U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][5U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [0U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [0U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__regValid[0U] 
        = (1U & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                 [0U][2U] >> 0x1aU));
    vlSelfRef.__PVT__intRwStage__DOT__valid[0U] = (1U 
                                                   & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                      [0U][7U] 
                                                      >> 6U));
    __Vfunc_SelectiveFlushDetector__548__opPtr = (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__548__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__548__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__548__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__548__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__548__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__548__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                goto __Vlabel165;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel165;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel165;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel165;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel165;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel165;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                goto __Vlabel165;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
        }
        __Vlabel165: ;
    }
    vlSelfRef.__PVT__intRwStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout;
    vlSelfRef.__PVT__intRwStage__DOT__update[0U] = 
        ((((~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__intRwStage__DOT__valid
          [0U]) & (~ vlSelfRef.__PVT__intRwStage__DOT__flush
                   [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE[0U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [0U] & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                   [0U][0U] >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                    [0U][0U] >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData[0U] 
        = (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                              [0U][2U])) 
                              << 6U) | ((QData)((IData)(
                                                        vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                        [0U][1U])) 
                                        >> 0x1aU)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][2U]) | (0xfcU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                  [0U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [0U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][2U]) | (3U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                               [0U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [0U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                       [0U][1U] << 0x11U) 
                                      | (0x1fffcU & 
                                         (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                          [0U][0U] 
                                          >> 0xfU)))));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
        = (3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [0U][1U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][0U]) | (((2U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [0U][2U] >> 3U))) 
                          | (3U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                          [0U][2U] 
                                          >> 3U)))) 
                         << 1U));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__brResult[0U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                                [0U][0U]))));
    vlSelfRef.__PVT__intRwStage__DOT__brResult[0U] 
        = ((0x1ffffffffffefffULL & vlSelfRef.__PVT__intRwStage__DOT__brResult
            [0U]) | ((QData)((IData)((((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                        [0U][0U] >> 0xcU) 
                                       & vlSelfRef.__PVT__intRwStage__DOT__update
                                       [0U]) & vlSelfRef.__PVT__intRwStage__DOT__regValid
                                      [0U]))) << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [1U];
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | ((vlSelfRef.__PVT__intRwStage__DOT__update
                          [0U] ? (vlSelfRef.__PVT__intRwStage__DOT__regValid
                                  [0U] ? ((0x2000000U 
                                           & vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                           [0U][1U])
                                           ? 3U : 1U)
                                   : 0U) : 0U) << 0x16U));
    if (((3U == (0xfU & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                         [0U][1U] >> 0x16U))) | (1U 
                                                 == 
                                                 (0xfU 
                                                  & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                                     [0U][1U] 
                                                     >> 0x16U))))) {
        if (((0U != (3U & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                                   [0U] >> 0x11U)))) 
             & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                        [0U] >> 0xcU)))) {
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
                = (0x3800000U | (0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                 [0U][1U]));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
                = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                    [0U][0U]) | (0x3ffffcU & ((IData)(
                                                      (vlSelfRef.__PVT__intRwStage__DOT__brResult
                                                       [0U] 
                                                       >> 0x11U)) 
                                              << 2U)));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
                = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                   [0U][1U]);
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite[0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__update[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[0U][0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[0U][1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[0U][2U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordEntry[0U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [0U] & (~ vlSelfRef.__PVT__intRwStage__DOT__regValid
                   [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][2U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][3U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][4U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][5U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [0U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [0U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][2U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][3U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][4U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][5U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [1U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [1U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__regValid[1U] 
        = (1U & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                 [1U][2U] >> 0x1aU));
    vlSelfRef.__PVT__intRwStage__DOT__valid[1U] = (1U 
                                                   & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                      [1U][7U] 
                                                      >> 6U));
    __Vfunc_SelectiveFlushDetector__548__opPtr = (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__548__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__548__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__548__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__548__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__548__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__548__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                goto __Vlabel166;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel166;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel166;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel166;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel166;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel166;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                goto __Vlabel166;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
        }
        __Vlabel166: ;
    }
    vlSelfRef.__PVT__intRwStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout;
    vlSelfRef.__PVT__intRwStage__DOT__update[1U] = 
        ((((~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__intRwStage__DOT__valid
          [1U]) & (~ vlSelfRef.__PVT__intRwStage__DOT__flush
                   [1U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE[1U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [1U] & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                   [1U][0U] >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                    [1U][0U] >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData[1U] 
        = (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                              [1U][2U])) 
                              << 6U) | ((QData)((IData)(
                                                        vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                        [1U][1U])) 
                                        >> 0x1aU)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][2U] 
        = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][2U]) | (0xfcU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                  [1U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [1U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][2U]) | (3U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                               [1U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [1U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                       [1U][1U] << 0x11U) 
                                      | (0x1fffcU & 
                                         (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                          [1U][0U] 
                                          >> 0xfU)))));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
        = (3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [1U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [1U][1U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][0U]) | (((2U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [1U][2U] >> 3U))) 
                          | (3U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                          [1U][2U] 
                                          >> 3U)))) 
                         << 1U));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [1U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__brResult[1U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                                [1U][0U]))));
    vlSelfRef.__PVT__intRwStage__DOT__brResult[1U] 
        = ((0x1ffffffffffefffULL & vlSelfRef.__PVT__intRwStage__DOT__brResult
            [1U]) | ((QData)((IData)((((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                        [1U][0U] >> 0xcU) 
                                       & vlSelfRef.__PVT__intRwStage__DOT__update
                                       [1U]) & vlSelfRef.__PVT__intRwStage__DOT__regValid
                                      [1U]))) << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [1U];
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | ((vlSelfRef.__PVT__intRwStage__DOT__update
                          [1U] ? (vlSelfRef.__PVT__intRwStage__DOT__regValid
                                  [1U] ? ((0x2000000U 
                                           & vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                           [1U][1U])
                                           ? 3U : 1U)
                                   : 0U) : 0U) << 0x16U));
    if (((3U == (0xfU & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                         [1U][1U] >> 0x16U))) | (1U 
                                                 == 
                                                 (0xfU 
                                                  & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                                     [1U][1U] 
                                                     >> 0x16U))))) {
        if (((0U != (3U & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                                   [1U] >> 0x11U)))) 
             & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                        [1U] >> 0xcU)))) {
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
                = (0x3800000U | (0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                 [1U][1U]));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
                = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                    [1U][0U]) | (0x3ffffcU & ((IData)(
                                                      (vlSelfRef.__PVT__intRwStage__DOT__brResult
                                                       [1U] 
                                                       >> 0x11U)) 
                                              << 2U)));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
                = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                   [1U][1U]);
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite[1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__update[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[1U][0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[1U][1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[1U][2U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordEntry[1U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [1U] & (~ vlSelfRef.__PVT__intRwStage__DOT__regValid
                   [1U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][2U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][3U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][4U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][5U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [1U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [1U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [0U]) | (vlSelfRef.__PVT__intRwStage__DOT__valid
                     [0U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [0U]) | (vlSelfRef.__PVT__intRwStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][7U] >> 7U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [1U]) | (vlSelfRef.__PVT__intRwStage__DOT__valid
                     [1U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [1U]) | (vlSelfRef.__PVT__intRwStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][7U] >> 7U)));
    vlSelfRef.__PVT__intRwStage__DOT__unnamedblk4__DOT__i = 2U;
    vlSelfRef.__PVT__memRwStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__memRwStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[0U] = 0U;
    if ((1U & ((((~ (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                     [0U][0U] >> 1U)) & (((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                           [0U][1U] 
                                           >> 3U) & 
                                          (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                           [0U][0U] 
                                           >> 2U)) 
                                         | vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [0U][0U])) 
                & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                [0U][4U]) & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                             [0U][1U] >> 4U)))) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[0U] = 1U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[1U] = 0U;
    if ((1U & ((((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                  [0U][0U] >> 1U) & (((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                       [0U][1U] >> 3U) 
                                      & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [0U][0U] >> 2U)) 
                                     | vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                     [0U][0U])) & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                [0U][4U]) & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                             [0U][1U] >> 4U)))) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[1U] = 1U;
    }
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 1U;
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk3__DOT__j = 2U;
    vlSelfRef.__PVT__memRwStage__DOT__valid[0U] = (1U 
                                                   & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [0U][4U]);
    __Vfunc_SelectiveFlushDetector__618__opPtr = (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                  [0U][3U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__618__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__618__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__618__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__618__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__618__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__618__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                goto __Vlabel167;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel167;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel167;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel167;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel167;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel167;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                goto __Vlabel167;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
        }
        __Vlabel167: ;
    }
    vlSelfRef.__PVT__memRwStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout;
    vlSelfRef.__PVT__memRwStage__DOT__update[0U] = 
        ((((~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__memRwStage__DOT__valid
          [0U]) & (~ vlSelfRef.__PVT__memRwStage__DOT__flush
                   [0U]));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][2U]) | (0xfcU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                  [0U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [0U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][2U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [0U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][0U]) | (1U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][1U] >> 5U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | ((vlSelfRef.__PVT__memRwStage__DOT__update
                          [0U] ? (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [0U][1U] 
                                          >> 6U)) : 0U) 
                         << 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__execState[0U] 
        = (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__alWriteData
                   [0U][1U] >> 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                       [0U][3U] << 0x10U) 
                                      | (0xfffcU & 
                                         (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [0U][2U] 
                                          >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][0U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [0U][2U] << 0x10U) 
                                        | (0xfffcU 
                                           & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                              [0U][1U] 
                                              >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][2U] >> 0x10U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite[0U] 
        = vlSelfRef.__PVT__memRwStage__DOT__update[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[0U][0U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[0U][1U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[0U][2U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [0U][2U];
    vlSelfRef.__PVT__memRwStage__DOT__valid[1U] = (1U 
                                                   & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [1U][4U]);
    __Vfunc_SelectiveFlushDetector__618__opPtr = (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                  [1U][3U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__618__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__618__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__618__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__618__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__618__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__618__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                goto __Vlabel168;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel168;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel168;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel168;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel168;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel168;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                goto __Vlabel168;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
        }
        __Vlabel168: ;
    }
    vlSelfRef.__PVT__memRwStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout;
    vlSelfRef.__PVT__memRwStage__DOT__update[1U] = 
        ((((~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__memRwStage__DOT__valid
          [1U]) & (~ vlSelfRef.__PVT__memRwStage__DOT__flush
                   [1U]));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][2U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][2U]) | (0xfcU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                  [1U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [1U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][2U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [1U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
           [1U][0U]);
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][0U]) | (1U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][1U] >> 5U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | ((vlSelfRef.__PVT__memRwStage__DOT__update
                          [1U] ? (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [1U][1U] 
                                          >> 6U)) : 0U) 
                         << 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__execState[1U] 
        = (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__alWriteData
                   [1U][1U] >> 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                       [1U][3U] << 0x10U) 
                                      | (0xfffcU & 
                                         (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [1U][2U] 
                                          >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][0U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [1U][2U] << 0x10U) 
                                        | (0xfffcU 
                                           & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                              [1U][1U] 
                                              >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][2U] >> 0x10U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite[1U] 
        = vlSelfRef.__PVT__memRwStage__DOT__update[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[1U][0U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[1U][1U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[1U][2U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [1U][2U];
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk5__DOT__i = 2U;
    memRwStage__DOT____Vlvbound_hb4b12956__0 = (vlSelfRef.__PVT__memRwStage__DOT__update
                                                [0U] 
                                                & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [0U][1U] 
                                                   >> 0x11U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegWE[0U] 
        = memRwStage__DOT____Vlvbound_hb4b12956__0;
    memRwStage__DOT____Vlvbound_hca7847bb__0 = (0x7fU 
                                                & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [0U][1U] 
                                                   >> 0xaU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum[0U] 
        = memRwStage__DOT____Vlvbound_hca7847bb__0;
    memRwStage__DOT____Vlvbound_h6f0537ce__0 = (0x1ffffffffULL 
                                                & (((QData)((IData)(
                                                                    vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                                    [0U][1U])) 
                                                    << 0x1dU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                                      [0U][0U])) 
                                                      >> 3U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegData[0U] 
        = memRwStage__DOT____Vlvbound_h6f0537ce__0;
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk6__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [0U]) | (vlSelfRef.__PVT__memRwStage__DOT__valid
                     [0U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [0U]) | (vlSelfRef.__PVT__memRwStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][4U] >> 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [1U]) | (vlSelfRef.__PVT__memRwStage__DOT__valid
                     [1U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [1U]) | (vlSelfRef.__PVT__memRwStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][4U] >> 1U)));
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk7__DOT__i = 2U;
    vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect
        [0U];
    vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCanBeInvalidDirect[0U] 
        = vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCanBeInvalidDirect[1U] 
        = vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_sequent__TOP__SMT_RTL_Testbench__core__5(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_sequent__TOP__SMT_RTL_Testbench__core__5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk17__DOT__i = 8U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk19__DOT__way = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__p = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk20__DOT__way = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__unnamedblk15__DOT__way = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk21__DOT__i = 1U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__unnamedblk15__DOT__unnamedblk16__DOT__i = 8U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk17__DOT__unnamedblk18__DOT__way = 2U;
        vlSelfRef.__PVT__controller__DOT__cmStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__cmStage = 0U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__cmStage 
        = vlSelfRef.__PVT__controller__DOT__cmStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__cmStage;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__0(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__array__rst 
            = vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst;
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__array__rst 
            = vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst;
        vlSelfRef.iCache__DOT____Vcellinp__nruStateArray__rst 
            = vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst;
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase = 0U;
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount = 0U;
        vlSelfRef.__PVT__controller__DOT__dsStage = 1U;
    } else {
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__array__rst 
            = vlSelfRef.__PVT__iCache__DOT__regFlush;
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__array__rst 
            = vlSelfRef.__PVT__iCache__DOT__regFlush;
        vlSelfRef.iCache__DOT____Vcellinp__nruStateArray__rst 
            = vlSelfRef.__PVT__iCache__DOT__regFlush;
        vlSelfRef.__PVT__controller__DOT__dsStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase = 1U;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount = 0U;
            vlSelfRef.__PVT__controller__DOT__dsStage = 1U;
        } else if ((1U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__phase))) {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase = 2U;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount 
                = (0x7fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryEntryNum));
        } else if ((2U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__phase))) {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase 
                = ((0U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount))
                    ? 0U : 2U);
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount 
                = (0x7fU & ((2U < (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount))
                             ? ((IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount) 
                                - (IData)(2U)) : 0U));
        } else {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase 
                = vlSelfRef.__PVT__renameLogicCommitter__DOT__phase;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount 
                = (0x7fU & (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount));
        }
    }
    vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady 
        = ((2U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady)) 
           | ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
              && (0x11U > vlSelfRef.__PVT__replayQueue__DOT__mshrPhase
                  [vlSelfRef.__PVT__replayQueue__DOT__mshrID
                  [0U]])));
    vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady 
        = ((1U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady)) 
           | (((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
               && (0x11U > vlSelfRef.__PVT__replayQueue__DOT__mshrPhase
                   [vlSelfRef.__PVT__replayQueue__DOT__mshrID
                   [1U]])) << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid 
        = ((2U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid)) 
           | ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
              && vlSelfRef.__PVT__replayQueue__DOT__mshrValid
              [vlSelfRef.__PVT__replayQueue__DOT__mshrID
              [0U]]));
    vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid 
        = ((1U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid)) 
           | (((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
               && vlSelfRef.__PVT__replayQueue__DOT__mshrValid
               [vlSelfRef.__PVT__replayQueue__DOT__mshrID
               [1U]]) << 1U));
    vlSelfRef.__PVT__storeCommitter__DOT__nextPhase 
        = ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
           && ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                             >> 0x15U))) || ((1U & 
                                              (~ (IData)(vlSelfRef.__PVT__storeCommitter__DOT__phase))) 
                                             && (IData)(vlSelfRef.__PVT__storeCommitter__DOT__phase))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__dsStage 
        = vlSelfRef.__PVT__controller__DOT__dsStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__dsStage;
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__writeWayData[0U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            << 0xcU) | ((0x800U & ((~ (IData)(vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__array__rst)) 
                                   << 0xbU)) | (IData)(vlSelfRef.__PVT__iCache__DOT__regMissTag)));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__writeWayData[1U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            >> 0x14U) | ((IData)(((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                                  >> 0x20U)) << 0xcU));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__writeWayData[2U] 
        = ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                    >> 0x20U)) >> 0x14U);
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__writeWayData[0U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            << 0xcU) | ((0x800U & ((~ (IData)(vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__array__rst)) 
                                   << 0xbU)) | (IData)(vlSelfRef.__PVT__iCache__DOT__regMissTag)));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__writeWayData[1U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            >> 0x14U) | ((IData)(((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                                  >> 0x20U)) << 0xcU));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__writeWayData[2U] 
        = ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                    >> 0x20U)) >> 0x14U);
    vlSelfRef.__PVT__dsStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__dsStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__dsStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__dsStage));
    vlSelfRef.__PVT__dsStage__DOT__update[0U] = (1U 
                                                 & (((~ (IData)(vlSelfRef.__PVT__dsStage__DOT__stall)) 
                                                     & (~ (IData)(vlSelfRef.__PVT__dsStage__DOT__clear))) 
                                                    & (vlSelfRef.__PVT__dsStage__DOT__pipeReg
                                                       [0U][6U] 
                                                       >> 0xaU)));
    vlSelfRef.__PVT__dsStage__DOT__update[1U] = (1U 
                                                 & (((~ (IData)(vlSelfRef.__PVT__dsStage__DOT__stall)) 
                                                     & (~ (IData)(vlSelfRef.__PVT__dsStage__DOT__clear))) 
                                                    & (vlSelfRef.__PVT__dsStage__DOT__pipeReg
                                                       [1U][6U] 
                                                       >> 0xaU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write[0U] 
        = vlSelfRef.__PVT__dsStage__DOT__update[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write[1U] 
        = vlSelfRef.__PVT__dsStage__DOT__update[1U];
    vlSelfRef.__PVT__scheduler__DOT__dispatchStore[0U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [0U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [0U] >> 0x2fU)))) 
                    && (1U == (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [0U] >> 0x2cU))))));
    vlSelfRef.__PVT__scheduler__DOT__dispatchStore[1U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [1U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [1U] >> 0x2fU)))) 
                    && (1U == (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [1U] >> 0x2cU))))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchStore[0U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchStore
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchStore[1U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchStore
        [1U];
    vlSelfRef.__PVT__scheduler__DOT__dispatchLoad[0U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [0U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [0U] >> 0x2fU)))) 
                    && (1U != (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [0U] >> 0x2cU))))));
    vlSelfRef.__PVT__scheduler__DOT__dispatchLoad[1U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [1U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [1U] >> 0x2fU)))) 
                    && (1U != (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [1U] >> 0x2cU))))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchLoad[0U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchLoad
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchLoad[1U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchLoad
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__3(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ complexIsStage__DOT____Vlvbound_h83df4b47__0;
    complexIsStage__DOT____Vlvbound_h83df4b47__0 = 0;
    // Body
    if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__0 
            = (1U & ((vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
                      [0U] >> 4U) & (~ ([&]() {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr 
                                = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData
                                   [0U][1U] >> 0x1aU);
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__flushAllInsns 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 4U));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 0xaU));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange 
                                = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                >> 0x15U)));
                            {
                                if (vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange) {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__549__flushAllInsns) {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                        goto __Vlabel169;
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr) 
                                                   >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                            goto __Vlabel169;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                            goto __Vlabel169;
                                        }
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr) 
                                                   < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                            goto __Vlabel169;
                                        } else if (
                                                   (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                     < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                            goto __Vlabel169;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                            goto __Vlabel169;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                        goto __Vlabel169;
                                    }
                                } else {
                                    vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                }
                                __Vlabel169: ;
                            }
                        }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout)))));
        vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__0) 
                         << 4U));
    } else {
        vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__1 
            = (1U & (vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
                     [0U] >> 4U));
        vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__1) 
                         << 4U));
    }
    complexIsStage__DOT____Vlvbound_h83df4b47__0 = 
        (0xfU & vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
         [0U]);
    vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg
            [0U]) | (IData)(complexIsStage__DOT____Vlvbound_h83df4b47__0));
    vlSelfRef.__PVT__complexIsStage__DOT__unnamedblk2__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__4(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ fpIsStage__DOT____Vlvbound_h83df4b47__0;
    fpIsStage__DOT____Vlvbound_h83df4b47__0 = 0;
    // Body
    if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__0 
            = (1U & ((vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                      [0U] >> 4U) & (~ ([&]() {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr 
                                = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData
                                   [0U][1U] >> 0x1aU);
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__flushAllInsns 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 4U));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 0xaU));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange 
                                = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                >> 0x15U)));
                            {
                                if (vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange) {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__619__flushAllInsns) {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                        goto __Vlabel170;
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr) 
                                                   >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                            goto __Vlabel170;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                            goto __Vlabel170;
                                        }
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr) 
                                                   < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                            goto __Vlabel170;
                                        } else if (
                                                   (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                     < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                            goto __Vlabel170;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                            goto __Vlabel170;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                        goto __Vlabel170;
                                    }
                                } else {
                                    vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                }
                                __Vlabel170: ;
                            }
                        }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout)))));
        vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__0) 
                         << 4U));
    } else {
        vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__1 
            = (1U & (vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                     [0U] >> 4U));
        vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__1) 
                         << 4U));
    }
    fpIsStage__DOT____Vlvbound_h83df4b47__0 = (0xfU 
                                               & vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                                               [0U]);
    vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg
            [0U]) | (IData)(fpIsStage__DOT____Vlvbound_h83df4b47__0));
    vlSelfRef.__PVT__fpIsStage__DOT__unnamedblk2__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__5(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
                    [0U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                      [0U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
                                       [0U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                            goto __Vlabel171;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel171;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel171;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel171;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel171;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel171;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                            goto __Vlabel171;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                    }
                                    __Vlabel171: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout))))
                                  : (vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                     [0U] >> 4U))));
    vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
            [0U]) | (0xfU & vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                     [0U]));
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
                    [1U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                      [1U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
                                       [1U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                            goto __Vlabel172;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel172;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel172;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel172;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel172;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel172;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                            goto __Vlabel172;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                    }
                                    __Vlabel172: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout))))
                                  : (vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                     [1U] >> 4U))));
    vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg[1U] 
        = ((0x10U & vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
            [1U]) | (0xfU & vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                     [1U]));
    vlSelfRef.__PVT__intIsStage__DOT__unnamedblk2__DOT__i = 2U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__6(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__6\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
                    [0U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                      [0U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
                                       [0U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                            goto __Vlabel173;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel173;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel173;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel173;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel173;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel173;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                            goto __Vlabel173;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                    }
                                    __Vlabel173: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout))))
                                  : (vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                     [0U] >> 4U))));
    vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
            [0U]) | (0xfU & vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                     [0U]));
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
                    [1U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                      [1U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
                                       [1U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                            goto __Vlabel174;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel174;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel174;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel174;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel174;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel174;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                            goto __Vlabel174;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                    }
                                    __Vlabel174: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout))))
                                  : (vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                     [1U] >> 4U))));
    vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg[1U] 
        = ((0x10U & vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
            [1U]) | (0xfU & vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                     [1U]));
    vlSelfRef.__PVT__memIsStage__DOT__unnamedblk2__DOT__i = 2U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__8(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__8\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<3>/*81:0*/ complexRrStage__DOT____Vlvbound_h51bf7b14__0;
    VL_ZERO_W(82, complexRrStage__DOT____Vlvbound_h51bf7b14__0);
    CData/*2:0*/ complexRrStage__DOT____Vlvbound_h4f6daee4__0;
    complexRrStage__DOT____Vlvbound_h4f6daee4__0 = 0;
    IData/*20:0*/ complexRrStage__DOT____Vlvbound_h79f92e5a__0;
    complexRrStage__DOT____Vlvbound_h79f92e5a__0 = 0;
    CData/*7:0*/ complexRrStage__DOT____Vlvbound_h9435fd27__0;
    complexRrStage__DOT____Vlvbound_h9435fd27__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h02c11781__0;
    complexRrStage__DOT____Vlvbound_h02c11781__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_ha2604c03__0;
    complexRrStage__DOT____Vlvbound_ha2604c03__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h04fb3afd__0;
    complexRrStage__DOT____Vlvbound_h04fb3afd__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h738e9198__0;
    complexRrStage__DOT____Vlvbound_h738e9198__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h09578847__0;
    complexRrStage__DOT____Vlvbound_h09578847__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h095fb107__0;
    complexRrStage__DOT____Vlvbound_h095fb107__0 = 0;
    SData/*11:0*/ complexRrStage__DOT____Vlvbound_h022db8e7__0;
    complexRrStage__DOT____Vlvbound_h022db8e7__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h046b483c__0;
    complexRrStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_ha5bb9e4e__0;
    complexRrStage__DOT____Vlvbound_ha5bb9e4e__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_ha630ed7f__0;
    complexRrStage__DOT____Vlvbound_ha630ed7f__0 = 0;
    QData/*32:0*/ complexRrStage__DOT____Vlvbound_h0a98cdc0__0;
    complexRrStage__DOT____Vlvbound_h0a98cdc0__0 = 0;
    QData/*32:0*/ complexRrStage__DOT____Vlvbound_h0a827eb3__0;
    complexRrStage__DOT____Vlvbound_h0a827eb3__0 = 0;
    VlWide<3>/*81:0*/ complexRrStage__DOT____Vlvbound_hb64b5082__0;
    VL_ZERO_W(82, complexRrStage__DOT____Vlvbound_hb64b5082__0);
    IData/*20:0*/ complexRrStage__DOT____Vlvbound_h094f5bee__0;
    complexRrStage__DOT____Vlvbound_h094f5bee__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h31a1399f__0;
    complexRrStage__DOT____Vlvbound_h31a1399f__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h31a147f0__0;
    complexRrStage__DOT____Vlvbound_h31a147f0__0 = 0;
    SData/*11:0*/ complexRrStage__DOT____Vlvbound_hbb48d9af__0;
    complexRrStage__DOT____Vlvbound_hbb48d9af__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__551__detectRange;
    __Vfunc_SelectiveFlushDetector__551__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__551__headPtr;
    __Vfunc_SelectiveFlushDetector__551__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__551__tailPtr;
    __Vfunc_SelectiveFlushDetector__551__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__551__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__551__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__551__opPtr;
    __Vfunc_SelectiveFlushDetector__551__opPtr = 0;
    // Body
    vlSelfRef.__PVT__complexRrStage__DOT__stall = (1U 
                                                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                      >> 1U));
    vlSelfRef.__PVT__complexRrStage__DOT__clear = (1U 
                                                   & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    complexRrStage__DOT____Vlvbound_h51bf7b14__0[0U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][0U];
    complexRrStage__DOT____Vlvbound_h51bf7b14__0[1U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][1U];
    complexRrStage__DOT____Vlvbound_h51bf7b14__0[2U] 
        = (0x3ffffU & vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
           [0U][2U]);
    vlSelfRef.__PVT__complexRrStage__DOT__iqData[0U][0U] 
        = complexRrStage__DOT____Vlvbound_h51bf7b14__0[0U];
    vlSelfRef.__PVT__complexRrStage__DOT__iqData[0U][1U] 
        = complexRrStage__DOT____Vlvbound_h51bf7b14__0[1U];
    vlSelfRef.__PVT__complexRrStage__DOT__iqData[0U][2U] 
        = complexRrStage__DOT____Vlvbound_h51bf7b14__0[2U];
    complexRrStage__DOT____Vlvbound_h4f6daee4__0 = 
        (7U & (vlSelfRef.__PVT__complexRrStage__DOT__iqData
               [0U][2U] >> 3U));
    vlSelfRef.__PVT__complexRrStage__DOT__mulOpInfo[0U] 
        = complexRrStage__DOT____Vlvbound_h4f6daee4__0;
    complexRrStage__DOT____Vlvbound_h79f92e5a__0 = 
        (0x1fffffU & ((vlSelfRef.__PVT__complexRrStage__DOT__iqData
                       [0U][1U] << 3U) | (vlSelfRef.__PVT__complexRrStage__DOT__iqData
                                          [0U][0U] 
                                          >> 0x1dU)));
    vlSelfRef.__PVT__complexRrStage__DOT__opSrc[0U] 
        = complexRrStage__DOT____Vlvbound_h79f92e5a__0;
    complexRrStage__DOT____Vlvbound_h9435fd27__0 = 
        (0xffU & (vlSelfRef.__PVT__complexRrStage__DOT__iqData
                  [0U][0U] >> 0x15U));
    vlSelfRef.__PVT__complexRrStage__DOT__opDst[0U] 
        = complexRrStage__DOT____Vlvbound_h9435fd27__0;
    complexRrStage__DOT____Vlvbound_h02c11781__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumA[0U] 
        = complexRrStage__DOT____Vlvbound_h02c11781__0;
    complexRrStage__DOT____Vlvbound_ha2604c03__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumB[0U] 
        = complexRrStage__DOT____Vlvbound_ha2604c03__0;
    complexRrStage__DOT____Vlvbound_h04fb3afd__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumA[0U] 
        = complexRrStage__DOT____Vlvbound_h04fb3afd__0;
    complexRrStage__DOT____Vlvbound_h738e9198__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumB[0U] 
        = complexRrStage__DOT____Vlvbound_h738e9198__0;
    complexRrStage__DOT____Vlvbound_h09578847__0 = 
        (1U & ((vlSelfRef.__PVT__complexRrStage__DOT__opDst
                [0U] >> 7U) & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
                               [0U][2U] >> 0x13U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexWriteReg[0U] 
        = complexRrStage__DOT____Vlvbound_h09578847__0;
    complexRrStage__DOT____Vlvbound_h095fb107__0 = 
        (0x7fU & vlSelfRef.__PVT__complexRrStage__DOT__opDst
         [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhyDstRegNum[0U] 
        = complexRrStage__DOT____Vlvbound_h095fb107__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegA[0U] = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegB[0U] = 1U;
    complexRrStage__DOT____Vlvbound_h022db8e7__0 = 
        (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
         [0U][2U] >> 0x14U);
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & ((IData)(complexRrStage__DOT____Vlvbound_h022db8e7__0) 
                                      << 0xcU)));
    __Vfunc_SelectiveFlushDetector__551__opPtr = (vlSelfRef.__PVT__complexRrStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__551__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__551__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__551__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__551__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__551__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__551__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                goto __Vlabel175;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__551__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                    goto __Vlabel175;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
                    goto __Vlabel175;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__551__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                    goto __Vlabel175;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                    goto __Vlabel175;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
                    goto __Vlabel175;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
                goto __Vlabel175;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
        }
        __Vlabel175: ;
    }
    complexRrStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout;
    vlSelfRef.__PVT__complexRrStage__DOT__flush[0U] 
        = complexRrStage__DOT____Vlvbound_h046b483c__0;
    complexRrStage__DOT____Vlvbound_ha5bb9e4e__0 = 
        (1U & ((~ ((((IData)(vlSelfRef.__PVT__complexRrStage__DOT__stall) 
                     | (IData)(vlSelfRef.__PVT__complexRrStage__DOT__clear)) 
                    | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                   | vlSelfRef.__PVT__complexRrStage__DOT__flush
                   [0U])) & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
                             [0U][2U] >> 0x13U)));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfff7ffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & ((IData)(complexRrStage__DOT____Vlvbound_ha5bb9e4e__0) 
                                      << 0xbU)));
    complexRrStage__DOT____Vlvbound_ha630ed7f__0 = 
        (1U & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
               [0U][2U] >> 0x12U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfffbffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & ((IData)(complexRrStage__DOT____Vlvbound_ha630ed7f__0) 
                                      << 0xaU)));
    if ((1U == (7U & vlSelfRef.__PVT__complexRrStage__DOT__iqData
                [0U][2U]))) {
        vlSelfRef.complexRrStage__DOT____Vlvbound_ha632c82d__0 
            = ((vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
                [0U][2U] >> 0x13U) & vlSelfRef.__PVT__complexRrStage__DOT__flush
               [0U]);
        vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
            = ((0xfffdffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
                [0U][5U]) | (0xffffffU & ((IData)(vlSelfRef.complexRrStage__DOT____Vlvbound_ha632c82d__0) 
                                          << 9U)));
    } else {
        vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
            = (0xfffdffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
               [0U][5U]);
    }
    complexRrStage__DOT____Vlvbound_h0a98cdc0__0 = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegDataA
        [0U];
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][1U] 
        = ((0x3fffffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][1U]) | ((IData)(complexRrStage__DOT____Vlvbound_h0a98cdc0__0) 
                         << 0x16U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][2U] 
        = ((0xff800000U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][2U]) | (((IData)(complexRrStage__DOT____Vlvbound_h0a98cdc0__0) 
                          >> 0xaU) | ((IData)((complexRrStage__DOT____Vlvbound_h0a98cdc0__0 
                                               >> 0x20U)) 
                                      << 0x16U)));
    complexRrStage__DOT____Vlvbound_h0a827eb3__0 = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegDataB
        [0U];
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][0U] 
        = ((0x1fffffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][0U]) | ((IData)(complexRrStage__DOT____Vlvbound_h0a827eb3__0) 
                         << 0x15U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][1U] 
        = ((0xffc00000U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][1U]) | (((IData)(complexRrStage__DOT____Vlvbound_h0a827eb3__0) 
                          >> 0xbU) | ((IData)((complexRrStage__DOT____Vlvbound_h0a827eb3__0 
                                               >> 0x20U)) 
                                      << 0x15U)));
    complexRrStage__DOT____Vlvbound_hb64b5082__0[0U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][0U];
    complexRrStage__DOT____Vlvbound_hb64b5082__0[1U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][1U];
    complexRrStage__DOT____Vlvbound_hb64b5082__0[2U] 
        = (0x3ffffU & vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
           [0U][2U]);
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][2U] 
        = ((0x7fffffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][2U]) | (complexRrStage__DOT____Vlvbound_hb64b5082__0[0U] 
                         << 0x17U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][3U] 
        = ((complexRrStage__DOT____Vlvbound_hb64b5082__0[0U] 
            >> 9U) | (complexRrStage__DOT____Vlvbound_hb64b5082__0[1U] 
                      << 0x17U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][4U] 
        = ((complexRrStage__DOT____Vlvbound_hb64b5082__0[1U] 
            >> 9U) | (complexRrStage__DOT____Vlvbound_hb64b5082__0[2U] 
                      << 0x17U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfffe00U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & (complexRrStage__DOT____Vlvbound_hb64b5082__0[2U] 
                                      >> 9U)));
    complexRrStage__DOT____Vlvbound_h094f5bee__0 = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
        [0U];
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][0U] 
        = ((0xffe00000U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][0U]) | complexRrStage__DOT____Vlvbound_h094f5bee__0);
    vlSelfRef.__PVT__complexRrStage__DOT__unnamedblk3__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][5U];
    complexRrStage__DOT____Vlvbound_h31a1399f__0 = 
        (1U & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
               [0U][2U] >> 0x13U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
            [0U]) | ((IData)(complexRrStage__DOT____Vlvbound_h31a1399f__0) 
                     << 0xdU));
    complexRrStage__DOT____Vlvbound_h31a147f0__0 = 
        vlSelfRef.__PVT__complexRrStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
            [0U]) | ((IData)(complexRrStage__DOT____Vlvbound_h31a147f0__0) 
                     << 0xcU));
    complexRrStage__DOT____Vlvbound_hbb48d9af__0 = 
        (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
         [0U][2U] >> 0x14U);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
            [0U]) | (IData)(complexRrStage__DOT____Vlvbound_hbb48d9af__0));
    vlSelfRef.__PVT__complexRrStage__DOT__unnamedblk4__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__9(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__9\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*31:0*/ __Vfunc_ToAddrFromPC__532__Vfuncout;
    __Vfunc_ToAddrFromPC__532__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToAddrFromPC__532__pc;
    __Vfunc_ToAddrFromPC__532__pc = 0;
    IData/*31:0*/ __Vfunc_RISCV_OpImm__533__Vfuncout;
    __Vfunc_RISCV_OpImm__533__Vfuncout = 0;
    IData/*29:0*/ __Vfunc_RISCV_OpImm__533__intOperandImm;
    __Vfunc_RISCV_OpImm__533__intOperandImm = 0;
    IData/*31:0*/ __Vfunc_RISCV_OpImm__533__result;
    __Vfunc_RISCV_OpImm__533__result = 0;
    CData/*1:0*/ __Vfunc_SelectOperandIntReg__534__opType;
    __Vfunc_SelectOperandIntReg__534__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__534__regV;
    __Vfunc_SelectOperandIntReg__534__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__534__immV;
    __Vfunc_SelectOperandIntReg__534__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__534__pcV;
    __Vfunc_SelectOperandIntReg__534__pcV = 0;
    CData/*1:0*/ __Vfunc_SelectOperandIntReg__535__opType;
    __Vfunc_SelectOperandIntReg__535__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__535__regV;
    __Vfunc_SelectOperandIntReg__535__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__535__immV;
    __Vfunc_SelectOperandIntReg__535__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__535__pcV;
    __Vfunc_SelectOperandIntReg__535__pcV = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__536__detectRange;
    __Vfunc_SelectiveFlushDetector__536__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__536__headPtr;
    __Vfunc_SelectiveFlushDetector__536__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__536__tailPtr;
    __Vfunc_SelectiveFlushDetector__536__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__536__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__536__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__536__opPtr;
    __Vfunc_SelectiveFlushDetector__536__opPtr = 0;
    // Body
    vlSelfRef.__PVT__intRrStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__intRrStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][0U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][1U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][2U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][3U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][4U] 
        = (0x7ffU & vlSelfRef.__PVT__intRrStage__DOT__pipeReg
           [0U][4U]);
    vlSelfRef.__PVT__intRrStage__DOT__intSubInfo[0U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                    [0U][3U])) 
                                    << 0x1aU) | ((QData)((IData)(
                                                                 vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                                 [0U][2U])) 
                                                 >> 6U)));
    vlSelfRef.__PVT__intRrStage__DOT__opSrc[0U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                       [0U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                         [0U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__intRrStage__DOT__opDst[0U] = (0xffU 
                                                   & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                      [0U][0U] 
                                                      >> 0x15U));
    __Vfunc_ToAddrFromPC__532__pc = (0xfffffU & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                 [0U][0U] 
                                                 >> 1U));
    __Vfunc_ToAddrFromPC__532__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__532__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__532__pc));
    vlSelfRef.__PVT__intRrStage__DOT__pc[0U] = __Vfunc_ToAddrFromPC__532__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg[0U] 
        = (1U & ((vlSelfRef.__PVT__intRrStage__DOT__opDst
                  [0U] >> 7U) & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                 [0U][4U] >> 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum[0U] 
        = (0x7fU & vlSelfRef.__PVT__intRrStage__DOT__opDst
           [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [0U] >> 0x37U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [0U] >> 0x35U))));
    __Vfunc_RISCV_OpImm__533__intOperandImm = (0x3fffffffU 
                                               & (IData)(
                                                         (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                          [0U] 
                                                          >> 0x12U)));
    __Vfunc_RISCV_OpImm__533__result = ((3U == (3U 
                                                & __Vfunc_RISCV_OpImm__533__intOperandImm))
                                         ? (0xfffff000U 
                                            & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                               << 0xaU))
                                         : (((- (IData)(
                                                        (1U 
                                                         & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                            >> 0x15U)))) 
                                             << 0x14U) 
                                            | (0xfffffU 
                                               & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                  >> 2U))));
    __Vfunc_RISCV_OpImm__533__Vfuncout = __Vfunc_RISCV_OpImm__533__result;
    vlSelfRef.__PVT__intRrStage__DOT__immOut[0U] = __Vfunc_RISCV_OpImm__533__Vfuncout;
    __Vfunc_SelectOperandIntReg__534__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperandIntReg__534__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperandIntReg__534__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                     [0U]);
    __Vfunc_SelectOperandIntReg__534__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [0U] 
                                                           >> 0x37U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__pcV;
            goto __Vlabel176;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__regV;
            goto __Vlabel176;
        }
        __Vlabel176: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandA[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout)));
    __Vfunc_SelectOperandIntReg__535__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperandIntReg__535__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperandIntReg__535__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                     [0U]);
    __Vfunc_SelectOperandIntReg__535__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [0U] 
                                                           >> 0x35U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__pcV;
            goto __Vlabel177;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__regV;
            goto __Vlabel177;
        }
        __Vlabel177: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandB[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout)));
    vlSelfRef.__PVT__intRrStage__DOT__operandA[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [0U] 
                                                            >> 0x37U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__operandB[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [0U] 
                                                            >> 0x35U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][7U] 
        = ((7U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][7U]) | (0x7ff8U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                    [0U][4U] >> 9U)));
    __Vfunc_SelectiveFlushDetector__536__opPtr = (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__536__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__536__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__536__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__536__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__536__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__536__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                goto __Vlabel178;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel178;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel178;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel178;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel178;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel178;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                goto __Vlabel178;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
        }
        __Vlabel178: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout;
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][7U] 
        = ((0x7ffbU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][7U]) | (4U & (((~ ((((IData)(vlSelfRef.__PVT__intRrStage__DOT__stall) 
                                      | (IData)(vlSelfRef.__PVT__intRrStage__DOT__clear)) 
                                     | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                    | vlSelfRef.__PVT__intRrStage__DOT__flush
                                    [0U])) << 2U) & 
                               (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [0U][4U] >> 9U))));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][1U] 
        = ((0x3fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][1U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                 [0U]) << 0x16U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][2U] 
        = ((0xff800000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][2U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                  [0U]) >> 0xaU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandA
                                   [0U] >> 0x20U)) 
                          << 0x16U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][0U] 
        = ((0x1fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][0U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                 [0U]) << 0x15U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][1U] 
        = ((0xffc00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][1U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                  [0U]) >> 0xbU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandB
                                   [0U] >> 0x20U)) 
                          << 0x15U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][2U] 
        = ((0x7fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][2U]) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                         [0U][0U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][3U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][0U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][1U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][4U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][1U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][2U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][5U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][2U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][3U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][6U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][3U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][4U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][7U] 
        = ((0x7ffcU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][7U]) | (3U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][4U] >> 9U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][0U] 
        = ((0xffe00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
           [0U]);
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][0U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][1U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][2U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][3U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][4U] 
        = (0x7ffU & vlSelfRef.__PVT__intRrStage__DOT__pipeReg
           [1U][4U]);
    vlSelfRef.__PVT__intRrStage__DOT__intSubInfo[1U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                    [1U][3U])) 
                                    << 0x1aU) | ((QData)((IData)(
                                                                 vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                                 [1U][2U])) 
                                                 >> 6U)));
    vlSelfRef.__PVT__intRrStage__DOT__opSrc[1U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                       [1U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                         [1U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__intRrStage__DOT__opDst[1U] = (0xffU 
                                                   & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                      [1U][0U] 
                                                      >> 0x15U));
    __Vfunc_ToAddrFromPC__532__pc = (0xfffffU & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                 [1U][0U] 
                                                 >> 1U));
    __Vfunc_ToAddrFromPC__532__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__532__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__532__pc));
    vlSelfRef.__PVT__intRrStage__DOT__pc[1U] = __Vfunc_ToAddrFromPC__532__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg[1U] 
        = (1U & ((vlSelfRef.__PVT__intRrStage__DOT__opDst
                  [1U] >> 7U) & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                 [1U][4U] >> 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum[1U] 
        = (0x7fU & vlSelfRef.__PVT__intRrStage__DOT__opDst
           [1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [1U] >> 0x37U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [1U] >> 0x35U))));
    __Vfunc_RISCV_OpImm__533__intOperandImm = (0x3fffffffU 
                                               & (IData)(
                                                         (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                          [1U] 
                                                          >> 0x12U)));
    __Vfunc_RISCV_OpImm__533__result = ((3U == (3U 
                                                & __Vfunc_RISCV_OpImm__533__intOperandImm))
                                         ? (0xfffff000U 
                                            & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                               << 0xaU))
                                         : (((- (IData)(
                                                        (1U 
                                                         & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                            >> 0x15U)))) 
                                             << 0x14U) 
                                            | (0xfffffU 
                                               & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                  >> 2U))));
    __Vfunc_RISCV_OpImm__533__Vfuncout = __Vfunc_RISCV_OpImm__533__result;
    vlSelfRef.__PVT__intRrStage__DOT__immOut[1U] = __Vfunc_RISCV_OpImm__533__Vfuncout;
    __Vfunc_SelectOperandIntReg__534__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperandIntReg__534__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperandIntReg__534__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                     [1U]);
    __Vfunc_SelectOperandIntReg__534__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [1U] 
                                                           >> 0x37U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__pcV;
            goto __Vlabel179;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__regV;
            goto __Vlabel179;
        }
        __Vlabel179: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandA[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout)));
    __Vfunc_SelectOperandIntReg__535__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperandIntReg__535__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperandIntReg__535__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                     [1U]);
    __Vfunc_SelectOperandIntReg__535__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [1U] 
                                                           >> 0x35U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__pcV;
            goto __Vlabel180;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__regV;
            goto __Vlabel180;
        }
        __Vlabel180: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandB[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout)));
    vlSelfRef.__PVT__intRrStage__DOT__operandA[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [1U] 
                                                            >> 0x37U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__operandB[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [1U] 
                                                            >> 0x35U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][7U] 
        = ((7U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][7U]) | (0x7ff8U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                    [1U][4U] >> 9U)));
    __Vfunc_SelectiveFlushDetector__536__opPtr = (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__536__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__536__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__536__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__536__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__536__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__536__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                goto __Vlabel181;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel181;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel181;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel181;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel181;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel181;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                goto __Vlabel181;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
        }
        __Vlabel181: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout;
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][7U] 
        = ((0x7ffbU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][7U]) | (4U & (((~ ((((IData)(vlSelfRef.__PVT__intRrStage__DOT__stall) 
                                      | (IData)(vlSelfRef.__PVT__intRrStage__DOT__clear)) 
                                     | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                    | vlSelfRef.__PVT__intRrStage__DOT__flush
                                    [1U])) << 2U) & 
                               (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [1U][4U] >> 9U))));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][1U] 
        = ((0x3fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][1U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                 [1U]) << 0x16U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][2U] 
        = ((0xff800000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][2U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                  [1U]) >> 0xaU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandA
                                   [1U] >> 0x20U)) 
                          << 0x16U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][0U] 
        = ((0x1fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][0U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                 [1U]) << 0x15U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][1U] 
        = ((0xffc00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][1U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                  [1U]) >> 0xbU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandB
                                   [1U] >> 0x20U)) 
                          << 0x15U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][2U] 
        = ((0x7fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][2U]) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                         [1U][0U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][3U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][0U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][1U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][4U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][1U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][2U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][5U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][2U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][3U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][6U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][3U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][4U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][7U] 
        = ((0x7ffcU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][7U]) | (3U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][4U] >> 9U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][0U] 
        = ((0xffe00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
           [1U]);
    vlSelfRef.__PVT__intRrStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][7U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][5U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][6U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][7U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [0U]) | (0x2000U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [0U][4U] << 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [0U]) | (vlSelfRef.__PVT__intRrStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][4U] >> 0xcU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [1U]) | (0x2000U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [1U][4U] << 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [1U]) | (vlSelfRef.__PVT__intRrStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][4U] >> 0xcU)));
    vlSelfRef.__PVT__intRrStage__DOT__unnamedblk4__DOT__i = 2U;
}

extern const VlWide<8>/*255:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0;

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__11(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__11\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*2:0*/ __Vfunc_idStage__DOT__ToInsnLane__512__mopLane;
    __Vfunc_idStage__DOT__ToInsnLane__512__mopLane = 0;
    IData/*31:0*/ __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 0;
    IData/*31:0*/ __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 0;
    IData/*31:0*/ __Vfunc_ToAddrFromPC__513__Vfuncout;
    __Vfunc_ToAddrFromPC__513__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToAddrFromPC__513__pc;
    __Vfunc_ToAddrFromPC__513__pc = 0;
    VlWide<3>/*95:0*/ __Vtemp_6;
    VlWide<3>/*95:0*/ __Vtemp_13;
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__idStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__idStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__controller__DOT__idStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__idStage = 2U;
        } else if (vlSelfRef.__PVT__rnStage__DOT__regFlush) {
            vlSelfRef.__PVT__controller__DOT__idStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
            vlSelfRef.__PVT__controller__DOT__idStage = 2U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage 
        = vlSelfRef.__PVT__controller__DOT__idStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__idStage;
    vlSelfRef.__PVT__idStage__DOT__nextValidMOps = vlSelfRef.__PVT__idStage__DOT__pickedValidMOps;
    vlSelfRef.__PVT__idStage__DOT__complete = 1U;
    if (((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
         & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [0U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 1U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [0U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 2U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [0U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((1U & (~ vlSelfRef.__PVT__idStage__DOT__insnValidOut
               [0U]))) {
        vlSelfRef.__PVT__idStage__DOT__nextValidMOps 
            = (0x38U & (IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps));
        vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk13__DOT__j = 3U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 3U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [1U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 4U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [1U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 5U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [1U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk12__DOT__j = 3U;
    if ((1U & (~ vlSelfRef.__PVT__idStage__DOT__insnValidOut
               [1U]))) {
        vlSelfRef.__PVT__idStage__DOT__nextValidMOps 
            = (7U & (IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps));
        vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk13__DOT__j = 3U;
    }
    vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper 
        = (1U & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__complete)));
    vlSelfRef.__PVT__idStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__idStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage));
    __Vfunc_idStage__DOT__ToInsnLane__512__mopLane 
        = vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
        [0U];
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 0;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 0;
    {
        if ((0U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel182;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((1U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel182;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((2U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel182;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 1U;
        if ((3U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel182;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((4U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel182;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((5U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel182;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 2U;
        vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
        __Vlabel182: ;
    }
    vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane 
        = vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout;
    if ((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                              [0U])))) {
        __Vtemp_6[1U] = (((0U == (0x1fU & ((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                           [0U]))) ? 0U
                           : (vlSelfRef.__PVT__idStage__DOT__microOps[
                              ((IData)(2U) + (0xfU 
                                              & (((IData)(0x4cU) 
                                                  * 
                                                  vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                  [0U]) 
                                                 >> 5U)))] 
                              << ((IData)(0x20U) - 
                                  (0x1fU & ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [0U]))))) 
                         | (vlSelfRef.__PVT__idStage__DOT__microOps[
                            ((IData)(1U) + (0xfU & 
                                            (((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [0U]) 
                                             >> 5U)))] 
                            >> (0x1fU & ((IData)(0x4cU) 
                                         * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                         [0U]))));
        __Vtemp_6[2U] = (0xfffU & (((0U == (0x1fU & 
                                            ((IData)(0x4cU) 
                                             * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                             [0U])))
                                     ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                             ((IData)(3U) 
                                              + (0xfU 
                                                 & (((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [0U]) 
                                                    >> 5U)))] 
                                             << ((IData)(0x20U) 
                                                 - 
                                                 (0x1fU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [0U]))))) 
                                   | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                      ((IData)(2U) 
                                       + (0xfU & (((IData)(0x4cU) 
                                                   * 
                                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                   [0U]) 
                                                  >> 5U)))] 
                                      >> (0x1fU & ((IData)(0x4cU) 
                                                   * 
                                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                   [0U])))));
    } else {
        __Vtemp_6[1U] = 0U;
        __Vtemp_6[2U] = 0U;
    }
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U] 
        = ((0x1fffffU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][1U]) | (((0x1c7U >= (0x1ffU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [0U])))
                           ? (((0U == (0x1fU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [0U])))
                                ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        ((IData)(1U) 
                                         + (0xfU & 
                                            (((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [0U]) 
                                             >> 5U)))] 
                                        << ((IData)(0x20U) 
                                            - (0x1fU 
                                               & ((IData)(0x4cU) 
                                                  * 
                                                  vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                  [0U]))))) 
                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                 (0xfU & (((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                           [0U]) >> 5U))] 
                                 >> (0x1fU & ((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [0U]))))
                           : 0U) << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][2U] 
        = ((((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * 
                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                   [0U]))) ? (((0U 
                                                == 
                                                (0x1fU 
                                                 & ((IData)(0x4cU) 
                                                    * 
                                                    vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                    [0U])))
                                                ? 0U
                                                : (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[
                                                   ((IData)(1U) 
                                                    + 
                                                    (0xfU 
                                                     & (((IData)(0x4cU) 
                                                         * 
                                                         vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                         [0U]) 
                                                        >> 5U)))] 
                                                   << 
                                                   ((IData)(0x20U) 
                                                    - 
                                                    (0x1fU 
                                                     & ((IData)(0x4cU) 
                                                        * 
                                                        vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                        [0U]))))) 
                                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                                 (0xfU 
                                                  & (((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [0U]) 
                                                     >> 5U))] 
                                                 >> 
                                                 (0x1fU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [0U]))))
              : 0U) >> 0xbU) | (__Vtemp_6[1U] << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][3U] 
        = ((__Vtemp_6[1U] >> 0xbU) | (__Vtemp_6[2U] 
                                      << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0x3ffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0x3fffU & (__Vtemp_6[2U] >> 0xbU)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0x3ffdU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0x3fffU & (((vlSelfRef.__PVT__idStage__DOT__insnValidOut
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                      & vlSelfRef.__PVT__idStage__DOT__mopPicked
                                      [0U]) & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__clear))) 
                                    << 1U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U] 
        = ((0xffe00001U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][1U]) | (0x1ffffeU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][8U] 
                                      >> 9U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][0U] 
        = (IData)(vlSelfRef.__PVT__idStage__DOT__brPredOut
                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane]);
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][1U]) | (IData)((vlSelfRef.__PVT__idStage__DOT__brPredOut
                                 [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                 >> 0x20U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0xfU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][0xaU] 
                                     << 5U) | (0x10U 
                                               & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][9U] 
                                                  >> 0x1bU)))));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0x3ff3U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0xcU & (vlSelfRef.__PVT__idStage__DOT__nextStage
                                 [0U][1U] >> 0x14U)));
    __Vfunc_idStage__DOT__ToInsnLane__512__mopLane 
        = vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
        [1U];
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 0;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 0;
    {
        if ((0U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel183;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((1U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel183;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((2U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel183;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 1U;
        if ((3U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel183;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((4U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel183;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((5U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel183;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 2U;
        vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
        __Vlabel183: ;
    }
    vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane 
        = vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout;
    if ((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                              [1U])))) {
        __Vtemp_13[1U] = (((0U == (0x1fU & ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [1U])))
                            ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                    ((IData)(2U) + 
                                     (0xfU & (((IData)(0x4cU) 
                                               * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                               [1U]) 
                                              >> 5U)))] 
                                    << ((IData)(0x20U) 
                                        - (0x1fU & 
                                           ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [1U]))))) 
                          | (vlSelfRef.__PVT__idStage__DOT__microOps[
                             ((IData)(1U) + (0xfU & 
                                             (((IData)(0x4cU) 
                                               * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                               [1U]) 
                                              >> 5U)))] 
                             >> (0x1fU & ((IData)(0x4cU) 
                                          * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                          [1U]))));
        __Vtemp_13[2U] = (0xfffU & (((0U == (0x1fU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [1U])))
                                      ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                              ((IData)(3U) 
                                               + (0xfU 
                                                  & (((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [1U]) 
                                                     >> 5U)))] 
                                              << ((IData)(0x20U) 
                                                  - 
                                                  (0x1fU 
                                                   & ((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [1U]))))) 
                                    | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                       ((IData)(2U) 
                                        + (0xfU & (
                                                   ((IData)(0x4cU) 
                                                    * 
                                                    vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                    [1U]) 
                                                   >> 5U)))] 
                                       >> (0x1fU & 
                                           ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [1U])))));
    } else {
        __Vtemp_13[1U] = 0U;
        __Vtemp_13[2U] = 0U;
    }
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U] 
        = ((0x1fffffU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][1U]) | (((0x1c7U >= (0x1ffU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [1U])))
                           ? (((0U == (0x1fU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [1U])))
                                ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        ((IData)(1U) 
                                         + (0xfU & 
                                            (((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [1U]) 
                                             >> 5U)))] 
                                        << ((IData)(0x20U) 
                                            - (0x1fU 
                                               & ((IData)(0x4cU) 
                                                  * 
                                                  vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                  [1U]))))) 
                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                 (0xfU & (((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                           [1U]) >> 5U))] 
                                 >> (0x1fU & ((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [1U]))))
                           : 0U) << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][2U] 
        = ((((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * 
                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                   [1U]))) ? (((0U 
                                                == 
                                                (0x1fU 
                                                 & ((IData)(0x4cU) 
                                                    * 
                                                    vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                    [1U])))
                                                ? 0U
                                                : (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[
                                                   ((IData)(1U) 
                                                    + 
                                                    (0xfU 
                                                     & (((IData)(0x4cU) 
                                                         * 
                                                         vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                         [1U]) 
                                                        >> 5U)))] 
                                                   << 
                                                   ((IData)(0x20U) 
                                                    - 
                                                    (0x1fU 
                                                     & ((IData)(0x4cU) 
                                                        * 
                                                        vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                        [1U]))))) 
                                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                                 (0xfU 
                                                  & (((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [1U]) 
                                                     >> 5U))] 
                                                 >> 
                                                 (0x1fU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [1U]))))
              : 0U) >> 0xbU) | (__Vtemp_13[1U] << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][3U] 
        = ((__Vtemp_13[1U] >> 0xbU) | (__Vtemp_13[2U] 
                                       << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0x3ffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0x3fffU & (__Vtemp_13[2U] 
                                    >> 0xbU)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0x3ffdU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0x3fffU & (((vlSelfRef.__PVT__idStage__DOT__insnValidOut
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                      & vlSelfRef.__PVT__idStage__DOT__mopPicked
                                      [1U]) & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__clear))) 
                                    << 1U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U] 
        = ((0xffe00001U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][1U]) | (0x1ffffeU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][8U] 
                                      >> 9U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][0U] 
        = (IData)(vlSelfRef.__PVT__idStage__DOT__brPredOut
                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane]);
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][1U]) | (IData)((vlSelfRef.__PVT__idStage__DOT__brPredOut
                                 [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                 >> 0x20U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0xfU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][0xaU] 
                                     << 5U) | (0x10U 
                                               & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][9U] 
                                                  >> 0x1bU)))));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0x3ff3U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0xcU & (vlSelfRef.__PVT__idStage__DOT__nextStage
                                 [1U][1U] >> 0x14U)));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk14__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__perfCounterIF.__PVT__branchPredMissDetectedOnDecode 
        = (((IData)(vlSelfRef.__PVT__idStage__DOT__complete) 
            & (IData)(vlSelfRef.__PVT__idStage__DOT__flushTriggered)) 
           & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__clear)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x10000U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [0U][9U] >> 0xeU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x17fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushed
                                     [0U] << 0xfU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x1bfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushTriggering
                                     [0U] << 0xeU)));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j = 0U;
    {
        while (VL_GTS_III(32, 3U, vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)) {
            if ((((0x1c7U >= ((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                  && (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[
                            (((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                             >> 5U)] >> (0x1fU & ((IData)(4U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))) 
                 & (0U == ((0x1c7U >= ((IData)(1U) 
                                       + (0x1ffU & 
                                          ((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))
                            ? (3U & (((0U == (0x1fU 
                                              & ((IData)(1U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))
                                       ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                               (((IData)(2U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                                                >> 5U)] 
                                               << ((IData)(0x20U) 
                                                   - 
                                                   (0x1fU 
                                                    & ((IData)(1U) 
                                                       + 
                                                       (0x1ffU 
                                                        & ((IData)(0x4cU) 
                                                           * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))) 
                                     | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        (((IData)(1U) 
                                          + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                                         >> 5U)] >> 
                                        (0x1fU & ((IData)(1U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))
                            : 0U)))) {
                if ((1U & (~ ((5U >= (7U & vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)) 
                              && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps) 
                                        >> (7U & vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))) {
                    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
                        = (0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                           [0U][2U]);
                }
                goto __Vlabel184;
            }
            vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j 
                = ((IData)(1U) + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j);
        }
        __Vlabel184: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x1c00fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [0U][0xaU] << 5U) 
                                    | (0x10U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                [0U][9U] 
                                                >> 0x1bU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = (0x1fff3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [0U][2U]);
    __Vfunc_ToAddrFromPC__513__pc = (0xfffffU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                 [0U][8U] 
                                                 >> 0xaU));
    __Vfunc_ToAddrFromPC__513__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__513__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__513__pc));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][1U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][1U]) | (__Vfunc_ToAddrFromPC__513__Vfuncout 
                         << 2U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x1fffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x1ffffU & (__Vfunc_ToAddrFromPC__513__Vfuncout 
                                     >> 0x1eU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                         [0U][9U] << 4U) 
                                        | (0xcU & (
                                                   vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                   [0U][8U] 
                                                   >> 0x1cU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][1U] 
        = ((0xfffffffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][1U]) | (3U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                               [0U][9U] >> 0x1cU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [0U][0U]) | (0x3fffffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[0U] 
                                                >> 6U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [0U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[0U] 
                                           >> 6U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [0U][0U]) | (0x3ffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[2U] 
                                                >> 0x12U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [0U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[2U] 
                                           >> 0x12U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [0U][0U]) | (2U & (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[4U] 
                                                   >> 0x1eU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [0U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[4U] 
                                           >> 0x1eU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x10000U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [1U][9U] >> 0xeU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x17fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushed
                                     [1U] << 0xfU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x1bfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushTriggering
                                     [1U] << 0xeU)));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j = 0U;
    {
        while (VL_GTS_III(32, 3U, vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)) {
            if ((((0x1c7U >= ((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * ((IData)(3U) 
                                                   + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))) 
                  && (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[
                            (((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * ((IData)(3U) 
                                                   + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                             >> 5U)] >> (0x1fU & ((IData)(4U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * 
                                                      ((IData)(3U) 
                                                       + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))) 
                 & (0U == ((0x1c7U >= ((IData)(1U) 
                                       + (0x1ffU & 
                                          ((IData)(0x4cU) 
                                           * ((IData)(3U) 
                                              + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))
                            ? (3U & (((0U == (0x1fU 
                                              & ((IData)(1U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     ((IData)(3U) 
                                                      + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))
                                       ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                               (((IData)(2U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     ((IData)(3U) 
                                                      + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                                                >> 5U)] 
                                               << ((IData)(0x20U) 
                                                   - 
                                                   (0x1fU 
                                                    & ((IData)(1U) 
                                                       + 
                                                       (0x1ffU 
                                                        & ((IData)(0x4cU) 
                                                           * 
                                                           ((IData)(3U) 
                                                            + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))) 
                                     | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        (((IData)(1U) 
                                          + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * ((IData)(3U) 
                                                   + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                                         >> 5U)] >> 
                                        (0x1fU & ((IData)(1U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * 
                                                      ((IData)(3U) 
                                                       + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))))
                            : 0U)))) {
                if ((1U & (~ ((5U >= (7U & ((IData)(3U) 
                                            + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                              && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps) 
                                        >> (7U & ((IData)(3U) 
                                                  + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))) {
                    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
                        = (0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                           [1U][2U]);
                }
                goto __Vlabel185;
            }
            vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j 
                = ((IData)(1U) + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j);
        }
        __Vlabel185: ;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__pdStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__pdStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 2U;
        } else if (vlSelfRef.__PVT__rnStage__DOT__regFlush) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 2U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x1c00fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [1U][0xaU] << 5U) 
                                    | (0x10U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                [1U][9U] 
                                                >> 0x1bU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = (0x1fff3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [1U][2U]);
    __Vfunc_ToAddrFromPC__513__pc = (0xfffffU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                 [1U][8U] 
                                                 >> 0xaU));
    __Vfunc_ToAddrFromPC__513__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__513__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__513__pc));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][1U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][1U]) | (__Vfunc_ToAddrFromPC__513__Vfuncout 
                         << 2U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x1fffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x1ffffU & (__Vfunc_ToAddrFromPC__513__Vfuncout 
                                     >> 0x1eU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                         [1U][9U] << 4U) 
                                        | (0xcU & (
                                                   vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                   [1U][8U] 
                                                   >> 0x1cU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][1U] 
        = ((0xfffffffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][1U]) | (3U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                               [1U][9U] >> 0x1cU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [1U][0U]) | (0x3ffffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[7U] 
                                                >> 0xaU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [1U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[7U] 
                                           >> 0xaU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [1U][0U]) | (0x3feU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[9U] 
                                                >> 0x16U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [1U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[9U] 
                                           >> 0x16U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [1U][0U]) | (0x3ffffffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[0xcU] 
                                                >> 2U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [1U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[0xcU] 
                                           >> 2U))));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk17__DOT__j = 3U;
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__i = 2U;
    vlSelfRef.__PVT__controller__DOT__stallByDecodeStage = 0U;
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        if ((1U != (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower)))) {
                if ((1U & (~ (IData)(vlSelfRef.__PVT__rnStage__DOT__regFlush)))) {
                    if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
                        vlSelfRef.__PVT__controller__DOT__stallByDecodeStage = 1U;
                    }
                }
            }
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__stallByDecodeStage 
        = vlSelfRef.__PVT__controller__DOT__stallByDecodeStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__stallByDecodeStage 
        = vlSelfRef.__PVT__controller__DOT__stallByDecodeStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage 
        = vlSelfRef.__PVT__controller__DOT__pdStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__pdStage;
    vlSelfRef.__PVT__idStage__DOT__stallBranchResolver 
        = (IData)((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage) 
                    >> 1U) & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__stallByDecodeStage))));
    vlSelfRef.__PVT__pdStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__pdStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][9U]) | (0x40000000U & (((~ (((IData)(vlSelfRef.__PVT__pdStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__pdStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                         << 0x1eU) 
                                        & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [0U][2U] 
                                           << 9U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][8U]) | (0xc0000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [0U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U] 
        = ((0xc0000000U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][9U]) | (0x3fffffffU & ((0x3ffffe00U 
                                         & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                            [0U][2U] 
                                            << 9U)) 
                                        | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [0U][1U] 
                                           >> 0x17U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U] 
        = ((0xc00003ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][8U]) | (0x3ffffc00U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [0U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][7U] 
        = ((0x1ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][7U]) | ((IData)((0x1ffffffffULL & 
                                  (((QData)((IData)(
                                                    vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                [0U][0U]))))) 
                         << 9U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U] 
        = ((0xfffffc00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][8U]) | (((IData)((0x1ffffffffULL & 
                                   (((QData)((IData)(
                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                     [0U][1U])) 
                                     << 0x20U) | (QData)((IData)(
                                                                 vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                 [0U][0U]))))) 
                          >> 0x17U) | ((IData)(((0x1ffffffffULL 
                                                 & (((QData)((IData)(
                                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                     [0U][1U])) 
                                                     << 0x20U) 
                                                    | (QData)((IData)(
                                                                      vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                      [0U][0U])))) 
                                                >> 0x20U)) 
                                       << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0U] 
        = ((0xffffffe0U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][0U]) | (0x1fU & (IData)(vlSelfRef.__PVT__pdStage__DOT__insnInfo)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0U] 
        = ((0x1fU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][0U]) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[0U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][1U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[0U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[1U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][2U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[1U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[2U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][3U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[2U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[3U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][4U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[3U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[4U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][5U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[4U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[5U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][6U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[5U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[6U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][7U] 
        = ((0xfffffe00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][7U]) | (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                           & vlSelfRef.__PVT__pdStage__DOT__microOps[6U]) 
                          >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[7U] 
                                        & vlSelfRef.__PVT__pdStage__DOT__microOps[7U]) 
                                       << 5U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][9U]) | (0x80000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [0U][2U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0xaU] 
        = (vlSelfRef.__PVT__pdStage__DOT__pipeReg[0U][2U] 
           >> 0x17U);
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][9U]) | (0x40000000U & (((~ (((IData)(vlSelfRef.__PVT__pdStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__pdStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                         << 0x1eU) 
                                        & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [1U][2U] 
                                           << 9U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][8U]) | (0xc0000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [1U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U] 
        = ((0xc0000000U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][9U]) | (0x3fffffffU & ((0x3ffffe00U 
                                         & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                            [1U][2U] 
                                            << 9U)) 
                                        | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [1U][1U] 
                                           >> 0x17U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U] 
        = ((0xc00003ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][8U]) | (0x3ffffc00U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [1U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][7U] 
        = ((0x1ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][7U]) | ((IData)((0x1ffffffffULL & 
                                  (((QData)((IData)(
                                                    vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                [1U][0U]))))) 
                         << 9U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U] 
        = ((0xfffffc00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][8U]) | (((IData)((0x1ffffffffULL & 
                                   (((QData)((IData)(
                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                     [1U][1U])) 
                                     << 0x20U) | (QData)((IData)(
                                                                 vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                 [1U][0U]))))) 
                          >> 0x17U) | ((IData)(((0x1ffffffffULL 
                                                 & (((QData)((IData)(
                                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                     [1U][1U])) 
                                                     << 0x20U) 
                                                    | (QData)((IData)(
                                                                      vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                      [1U][0U])))) 
                                                >> 0x20U)) 
                                       << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0U] 
        = ((0xffffffe0U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][0U]) | (0x1fU & ((IData)(vlSelfRef.__PVT__pdStage__DOT__insnInfo) 
                                  >> 5U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0U] 
        = ((0x1fU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][0U]) | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
                                         << 5U) & (
                                                   vlSelfRef.__PVT__pdStage__DOT__microOps[7U] 
                                                   << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][1U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[8U] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[7U] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[8U] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][2U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[9U] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[8U] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[9U] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][3U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xaU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[9U] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xaU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][4U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xbU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xaU] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xbU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][5U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xcU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xbU] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xcU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][6U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xdU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xcU] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xdU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][7U] 
        = ((0xfffffe00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][7U]) | (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                           >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xeU] 
                                                  << 1U)) 
                                        | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xdU] 
                                           >> 0x1fU))) 
                         | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[7U] 
                                            << 5U) 
                                           & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xeU] 
                                              << 1U)))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][9U]) | (0x80000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [1U][2U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0xaU] 
        = (vlSelfRef.__PVT__pdStage__DOT__pipeReg[1U][2U] 
           >> 0x17U);
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][7U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][8U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][9U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][0xaU] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0xaU];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][5U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][6U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][7U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][8U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][9U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][0xaU] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0xaU];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__15(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_comb__TOP__SMT_RTL_Testbench__core__15\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*1:0*/ __Vfunc_SelectOperand__559__opType;
    __Vfunc_SelectOperand__559__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__559__regV;
    __Vfunc_SelectOperand__559__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__559__immV;
    __Vfunc_SelectOperand__559__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__559__pcV;
    __Vfunc_SelectOperand__559__pcV = 0;
    CData/*1:0*/ __Vfunc_SelectOperand__560__opType;
    __Vfunc_SelectOperand__560__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__560__regV;
    __Vfunc_SelectOperand__560__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__560__immV;
    __Vfunc_SelectOperand__560__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__560__pcV;
    __Vfunc_SelectOperand__560__pcV = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__561__detectRange;
    __Vfunc_SelectiveFlushDetector__561__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__561__headPtr;
    __Vfunc_SelectiveFlushDetector__561__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__561__tailPtr;
    __Vfunc_SelectiveFlushDetector__561__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__561__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__561__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__561__opPtr;
    __Vfunc_SelectiveFlushDetector__561__opPtr = 0;
    // Body
    vlSelfRef.__PVT__memRrStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__memRrStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][0U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [0U][1U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [0U][0U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][1U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [0U][2U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [0U][1U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][2U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [0U][3U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [0U][2U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][3U] 
        = (0x1fffffffU & ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][4U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                 [0U][3U] 
                                                 >> 5U)));
    vlSelfRef.__PVT__memRrStage__DOT__memOpInfo[0U] 
        = (0x7fffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__memRrStage__DOT__iqData
                                               [0U][3U])) 
                               << 0x16U) | ((QData)((IData)(
                                                            vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                            [0U][2U])) 
                                            >> 0xaU)));
    vlSelfRef.__PVT__memRrStage__DOT__opSrc[0U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                       [0U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                         [0U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__memRrStage__DOT__opDst[0U] = (0xffU 
                                                   & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                      [0U][0U] 
                                                      >> 0x15U));
    vlSelfRef.__PVT__memRrStage__DOT__pc[0U] = (0xfffffU 
                                                & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                   [0U][0U] 
                                                   >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memWriteReg[0U] 
        = (1U & ((vlSelfRef.__PVT__memRrStage__DOT__opDst
                  [0U] >> 7U) & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                 [0U][4U] >> 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum[0U] 
        = (0x7fU & vlSelfRef.__PVT__memRrStage__DOT__opDst
           [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [0U] >> 0x1fU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [0U] >> 0x1dU))));
    vlSelfRef.__PVT__memRrStage__DOT__immOut[0U] = 0U;
    __Vfunc_SelectOperand__559__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperand__559__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperand__559__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                               [0U]);
    __Vfunc_SelectOperand__559__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [0U] 
                                                        >> 0x1fU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__pcV;
            goto __Vlabel186;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__regV;
            goto __Vlabel186;
        }
        __Vlabel186: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandA[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout)));
    __Vfunc_SelectOperand__560__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperand__560__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperand__560__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                               [0U]);
    __Vfunc_SelectOperand__560__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [0U] 
                                                        >> 0x1dU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__pcV;
            goto __Vlabel187;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__regV;
            goto __Vlabel187;
        }
        __Vlabel187: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandB[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout)));
    vlSelfRef.__PVT__memRrStage__DOT__operandA[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [0U] 
                                                            >> 0x1fU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__operandB[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [0U] 
                                                            >> 0x1dU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][6U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][6U]) | (0xfc000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [0U][4U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][7U] 
        = (0x3fU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                    [0U][4U] >> 9U));
    __Vfunc_SelectiveFlushDetector__561__opPtr = (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__561__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__561__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__561__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__561__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__561__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__561__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                goto __Vlabel188;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel188;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel188;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel188;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel188;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel188;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                goto __Vlabel188;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
        }
        __Vlabel188: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout;
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][6U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][6U]) | (0x2000000U & (((~ ((((IData)(vlSelfRef.__PVT__memRrStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__memRrStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                            | vlSelfRef.__PVT__memRrStage__DOT__flush
                                            [0U])) 
                                        << 0x19U) & 
                                       (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [0U][4U] << 0x17U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][2U] 
        = ((0xfffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][2U]) | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [0U][0U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][3U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][1U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [0U][0U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [0U][1U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][4U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][2U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [0U][1U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [0U][2U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][5U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][3U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [0U][2U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [0U][3U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][6U] 
        = ((0xfe000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][6U]) | (0x1ffffffU & ((0xf800000U 
                                        & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [0U][4U] 
                                           << 0x17U)) 
                                       | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                          [0U][3U] 
                                          >> 9U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][1U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][1U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                 [0U]) << 0x1bU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][2U] 
        = ((0xf0000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][2U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                  [0U]) >> 5U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandA
                                                           [0U] 
                                                           >> 0x20U)) 
                                                  << 0x1bU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                 [0U]) << 0x1aU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][1U] 
        = ((0xf8000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][1U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                  [0U]) >> 6U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandB
                                                           [0U] 
                                                           >> 0x20U)) 
                                                  << 0x1aU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0xfc00001fU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                         [0U] << 5U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | (1U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [0U][0U] & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [0U][4U] 
                                           >> 2U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0xffffffe1U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | (0x1eU & vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                         [0U][0U]));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][0U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [1U][1U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [1U][0U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][1U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [1U][2U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [1U][1U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][2U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [1U][3U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [1U][2U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][3U] 
        = (0x1fffffffU & ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][4U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                 [1U][3U] 
                                                 >> 5U)));
    vlSelfRef.__PVT__memRrStage__DOT__memOpInfo[1U] 
        = (0x7fffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__memRrStage__DOT__iqData
                                               [1U][3U])) 
                               << 0x16U) | ((QData)((IData)(
                                                            vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                            [1U][2U])) 
                                            >> 0xaU)));
    vlSelfRef.__PVT__memRrStage__DOT__opSrc[1U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                       [1U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                         [1U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__memRrStage__DOT__opDst[1U] = (0xffU 
                                                   & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                      [1U][0U] 
                                                      >> 0x15U));
    vlSelfRef.__PVT__memRrStage__DOT__pc[1U] = (0xfffffU 
                                                & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                   [1U][0U] 
                                                   >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memWriteReg[1U] 
        = (1U & ((vlSelfRef.__PVT__memRrStage__DOT__opDst
                  [1U] >> 7U) & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                 [1U][4U] >> 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum[1U] 
        = (0x7fU & vlSelfRef.__PVT__memRrStage__DOT__opDst
           [1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [1U] >> 0x1fU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [1U] >> 0x1dU))));
    vlSelfRef.__PVT__memRrStage__DOT__immOut[1U] = 0U;
    __Vfunc_SelectOperand__559__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperand__559__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperand__559__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                               [1U]);
    __Vfunc_SelectOperand__559__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [1U] 
                                                        >> 0x1fU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__pcV;
            goto __Vlabel189;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__regV;
            goto __Vlabel189;
        }
        __Vlabel189: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandA[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout)));
    __Vfunc_SelectOperand__560__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperand__560__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperand__560__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                               [1U]);
    __Vfunc_SelectOperand__560__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [1U] 
                                                        >> 0x1dU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__pcV;
            goto __Vlabel190;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__regV;
            goto __Vlabel190;
        }
        __Vlabel190: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandB[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout)));
    vlSelfRef.__PVT__memRrStage__DOT__operandA[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [1U] 
                                                            >> 0x1fU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__operandB[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [1U] 
                                                            >> 0x1dU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][6U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][6U]) | (0xfc000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [1U][4U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][7U] 
        = (0x3fU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                    [1U][4U] >> 9U));
    __Vfunc_SelectiveFlushDetector__561__opPtr = (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__561__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__561__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__561__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__561__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__561__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__561__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                goto __Vlabel191;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel191;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel191;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel191;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel191;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel191;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                goto __Vlabel191;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
        }
        __Vlabel191: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout;
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][6U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][6U]) | (0x2000000U & (((~ ((((IData)(vlSelfRef.__PVT__memRrStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__memRrStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                            | vlSelfRef.__PVT__memRrStage__DOT__flush
                                            [1U])) 
                                        << 0x19U) & 
                                       (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [1U][4U] << 0x17U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][2U] 
        = ((0xfffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][2U]) | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [1U][0U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][3U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][1U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [1U][0U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [1U][1U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][4U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][2U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [1U][1U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [1U][2U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][5U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][3U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [1U][2U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [1U][3U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][6U] 
        = ((0xfe000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][6U]) | (0x1ffffffU & ((0xf800000U 
                                        & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [1U][4U] 
                                           << 0x17U)) 
                                       | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                          [1U][3U] 
                                          >> 9U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][1U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][1U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                 [1U]) << 0x1bU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][2U] 
        = ((0xf0000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][2U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                  [1U]) >> 5U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandA
                                                           [1U] 
                                                           >> 0x20U)) 
                                                  << 0x1bU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                 [1U]) << 0x1aU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][1U] 
        = ((0xf8000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][1U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                  [1U]) >> 6U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandB
                                                           [1U] 
                                                           >> 0x20U)) 
                                                  << 0x1aU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0xfc00001fU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                         [1U] << 5U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | (1U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [1U][0U] & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [1U][4U] 
                                           >> 2U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0xffffffe1U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | (0x1eU & vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                         [1U][0U]));
    vlSelfRef.__PVT__memRrStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][7U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][5U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][6U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][7U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [0U]) | (0x2000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                [0U][4U] << 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [0U]) | (vlSelfRef.__PVT__memRrStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [0U][4U] >> 3U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [1U]) | (0x2000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                [1U][4U] << 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [1U]) | (vlSelfRef.__PVT__memRrStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [1U][4U] >> 3U)));
    vlSelfRef.__PVT__memRrStage__DOT__unnamedblk4__DOT__i = 2U;
}
