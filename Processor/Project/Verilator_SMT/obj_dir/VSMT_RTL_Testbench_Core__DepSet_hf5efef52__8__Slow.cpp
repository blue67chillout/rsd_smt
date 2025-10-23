// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__54(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__54\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0xfffcU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xfffffffeU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [1U] << 1U))) | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                                  [0U])));
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0xfff3U & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xfffffff8U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [3U] << 3U))) | (0xfffffffcU 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                                        [2U] 
                                                        << 2U)))));
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0xffcfU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xffffffe0U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [5U] << 5U))) | (0xfffffff0U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                                        [4U] 
                                                        << 4U)))));
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0xff3fU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xffffff80U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [7U] << 7U))) | (0xffffffc0U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                                        [6U] 
                                                        << 6U)))));
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0xfcffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xfffffe00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [9U] << 9U))) | (0xffffff00U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                                        [8U] 
                                                        << 8U)))));
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0xf3ffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xfffff800U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [0xbU] << 0xbU))) 
              | (0xfffffc00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                   [0xaU] << 0xaU)))));
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0xcfffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xffffe000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [0xdU] << 0xdU))) 
              | (0xfffff000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                   [0xcU] << 0xcU)))));
    vlSelfRef.__PVT__selectLogic__DOT__compRequest 
        = ((0x3fffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compRequest)) 
           | ((0xffff8000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                 [0xfU] << 0xfU))) 
              | (0xffffc000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq
                                   [0xeU] << 0xeU)))));
    vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__reqTmp 
        = vlSelfRef.__PVT__selectLogic__DOT__compRequest;
    vlSelfRef.__PVT__selectLogic__DOT__compGrant = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__compSelected[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__compSelectedPtr[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0U;
    {
        while (VL_GTS_III(32, 0x10U, vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__reqTmp) 
                       >> (0xfU & vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)))) {
                vlSelfRef.__PVT__selectLogic__DOT__compGrant 
                    = ((IData)(vlSelfRef.__PVT__selectLogic__DOT__compGrant) 
                       | (0xffffU & ((IData)(1U) << 
                                     (0xfU & vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))));
                vlSelfRef.selectLogic__DOT__compPicker__DOT____Vlvbound_h1f89c34b__1 = 1U;
                vlSelfRef.__PVT__selectLogic__DOT__compSelected[0U] 
                    = vlSelfRef.selectLogic__DOT__compPicker__DOT____Vlvbound_h1f89c34b__1;
                vlSelfRef.selectLogic__DOT__compPicker__DOT____Vlvbound_h54e39f63__1 
                    = (0xfU & vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
                vlSelfRef.__PVT__selectLogic__DOT__compSelectedPtr[0U] 
                    = vlSelfRef.selectLogic__DOT__compPicker__DOT____Vlvbound_h54e39f63__1;
                vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__reqTmp 
                    = ((~ ((IData)(1U) << (0xfU & vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))) 
                       & (IData)(vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__reqTmp));
                goto __Vlabel152;
            }
            vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e 
                = ((IData)(1U) + vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
        }
        __Vlabel152: ;
    }
    vlSelfRef.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__p = 1U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__55(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__55\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0xfffcU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xfffffffeU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [1U] << 1U))) | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                                                [0U])));
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0xfff3U & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xfffffff8U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [3U] << 3U))) | (0xfffffffcU 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                                                      [2U] 
                                                      << 2U)))));
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0xffcfU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xffffffe0U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [5U] << 5U))) | (0xfffffff0U 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                                                      [4U] 
                                                      << 4U)))));
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0xff3fU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xffffff80U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [7U] << 7U))) | (0xffffffc0U 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                                                      [6U] 
                                                      << 6U)))));
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0xfcffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xfffffe00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [9U] << 9U))) | (0xffffff00U 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                                                      [8U] 
                                                      << 8U)))));
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0xf3ffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xfffff800U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [0xbU] << 0xbU))) | 
            (0xfffffc00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [0xaU] << 0xaU)))));
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0xcfffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xffffe000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [0xdU] << 0xdU))) | 
            (0xfffff000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [0xcU] << 0xcU)))));
    vlSelfRef.__PVT__selectLogic__DOT__fpRequest = 
        ((0x3fffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpRequest)) 
         | ((0xffff8000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [0xfU] << 0xfU))) | 
            (0xffffc000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq
                               [0xeU] << 0xeU)))));
    vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__reqTmp 
        = vlSelfRef.__PVT__selectLogic__DOT__fpRequest;
    vlSelfRef.__PVT__selectLogic__DOT__fpGrant = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__fpSelected[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__fpSelectedPtr[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0U;
    {
        while (VL_GTS_III(32, 0x10U, vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__reqTmp) 
                       >> (0xfU & vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)))) {
                vlSelfRef.__PVT__selectLogic__DOT__fpGrant 
                    = ((IData)(vlSelfRef.__PVT__selectLogic__DOT__fpGrant) 
                       | (0xffffU & ((IData)(1U) << 
                                     (0xfU & vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))));
                vlSelfRef.selectLogic__DOT__fpPicker__DOT____Vlvbound_h1f89c34b__1 = 1U;
                vlSelfRef.__PVT__selectLogic__DOT__fpSelected[0U] 
                    = vlSelfRef.selectLogic__DOT__fpPicker__DOT____Vlvbound_h1f89c34b__1;
                vlSelfRef.selectLogic__DOT__fpPicker__DOT____Vlvbound_h54e39f63__1 
                    = (0xfU & vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
                vlSelfRef.__PVT__selectLogic__DOT__fpSelectedPtr[0U] 
                    = vlSelfRef.selectLogic__DOT__fpPicker__DOT____Vlvbound_h54e39f63__1;
                vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__reqTmp 
                    = ((~ ((IData)(1U) << (0xfU & vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))) 
                       & (IData)(vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__reqTmp));
                goto __Vlabel153;
            }
            vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e 
                = ((IData)(1U) + vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
        }
        __Vlabel153: ;
    }
    vlSelfRef.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__p = 1U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__58(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__58\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ scStage__DOT____Vlvbound_h0ac9b13f__0;
    scStage__DOT____Vlvbound_h0ac9b13f__0 = 0;
    CData/*3:0*/ scStage__DOT____Vlvbound_h353f532c__0;
    scStage__DOT____Vlvbound_h353f532c__0 = 0;
    CData/*0:0*/ scStage__DOT____Vlvbound_h08e85d10__0;
    scStage__DOT____Vlvbound_h08e85d10__0 = 0;
    CData/*0:0*/ scStage__DOT____Vlvbound_h6b1ccd1c__0;
    scStage__DOT____Vlvbound_h6b1ccd1c__0 = 0;
    CData/*0:0*/ scStage__DOT____Vlvbound_he0235765__0;
    scStage__DOT____Vlvbound_he0235765__0 = 0;
    CData/*3:0*/ scStage__DOT____Vlvbound_hf53c3fad__0;
    scStage__DOT____Vlvbound_hf53c3fad__0 = 0;
    CData/*4:0*/ scStage__DOT____Vlvbound_h345671f9__0;
    scStage__DOT____Vlvbound_h345671f9__0 = 0;
    CData/*4:0*/ scStage__DOT____Vlvbound_h8e4acef0__0;
    scStage__DOT____Vlvbound_h8e4acef0__0 = 0;
    CData/*0:0*/ scheduler__DOT____Vlvbound_h5f98ca24__0;
    scheduler__DOT____Vlvbound_h5f98ca24__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_hdb2d82a3__0;
    wakeupPipelineRegister__DOT____Vlvbound_hdb2d82a3__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_h9f74ea64__0;
    wakeupPipelineRegister__DOT____Vlvbound_h9f74ea64__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_h0c642a40__0;
    wakeupPipelineRegister__DOT____Vlvbound_h0c642a40__0 = 0;
    SData/*15:0*/ wakeupPipelineRegister__DOT____Vlvbound_h08ae36e2__0;
    wakeupPipelineRegister__DOT____Vlvbound_h08ae36e2__0 = 0;
    CData/*5:0*/ wakeupPipelineRegister__DOT____Vlvbound_h120d9f38__0;
    wakeupPipelineRegister__DOT____Vlvbound_h120d9f38__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_ha6fd7b38__0;
    wakeupPipelineRegister__DOT____Vlvbound_ha6fd7b38__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_hef721fdd__0;
    wakeupPipelineRegister__DOT____Vlvbound_hef721fdd__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_ha9d6caef__0;
    wakeupPipelineRegister__DOT____Vlvbound_ha9d6caef__0 = 0;
    SData/*15:0*/ wakeupPipelineRegister__DOT____Vlvbound_hec96f25a__0;
    wakeupPipelineRegister__DOT____Vlvbound_hec96f25a__0 = 0;
    CData/*5:0*/ wakeupPipelineRegister__DOT____Vlvbound_ha3c87944__0;
    wakeupPipelineRegister__DOT____Vlvbound_ha3c87944__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_h87bbb49c__0;
    wakeupPipelineRegister__DOT____Vlvbound_h87bbb49c__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_hd219707c__0;
    wakeupPipelineRegister__DOT____Vlvbound_hd219707c__0 = 0;
    SData/*15:0*/ wakeupPipelineRegister__DOT____Vlvbound_h89f1c5a3__0;
    wakeupPipelineRegister__DOT____Vlvbound_h89f1c5a3__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_hf246e9eb__0;
    wakeupPipelineRegister__DOT____Vlvbound_hf246e9eb__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_h71c4a121__0;
    wakeupPipelineRegister__DOT____Vlvbound_h71c4a121__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_h1c2242f7__0;
    wakeupPipelineRegister__DOT____Vlvbound_h1c2242f7__0 = 0;
    SData/*15:0*/ wakeupPipelineRegister__DOT____Vlvbound_h3fe92220__0;
    wakeupPipelineRegister__DOT____Vlvbound_h3fe92220__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_h966cd435__0;
    wakeupPipelineRegister__DOT____Vlvbound_h966cd435__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_hac0d33d7__0;
    wakeupPipelineRegister__DOT____Vlvbound_hac0d33d7__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_hb609efdd__0;
    wakeupPipelineRegister__DOT____Vlvbound_hb609efdd__0 = 0;
    SData/*15:0*/ wakeupPipelineRegister__DOT____Vlvbound_hadc244c6__0;
    wakeupPipelineRegister__DOT____Vlvbound_hadc244c6__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_hdaf3aedd__0;
    wakeupPipelineRegister__DOT____Vlvbound_hdaf3aedd__0 = 0;
    SData/*15:0*/ wakeupPipelineRegister__DOT____Vlvbound_h811685c6__0;
    wakeupPipelineRegister__DOT____Vlvbound_h811685c6__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_he223bad4__0;
    wakeupPipelineRegister__DOT____Vlvbound_he223bad4__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_h793cefab__0;
    wakeupPipelineRegister__DOT____Vlvbound_h793cefab__0 = 0;
    CData/*3:0*/ wakeupPipelineRegister__DOT____Vlvbound_h0399b589__0;
    wakeupPipelineRegister__DOT____Vlvbound_h0399b589__0 = 0;
    SData/*15:0*/ wakeupPipelineRegister__DOT____Vlvbound_h5770f0b2__0;
    wakeupPipelineRegister__DOT____Vlvbound_h5770f0b2__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_h031eb0bd__0;
    wakeupPipelineRegister__DOT____Vlvbound_h031eb0bd__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_hac5d62af__0;
    wakeupPipelineRegister__DOT____Vlvbound_hac5d62af__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_ha62a6d55__0;
    wakeupPipelineRegister__DOT____Vlvbound_ha62a6d55__0 = 0;
    CData/*0:0*/ wakeupPipelineRegister__DOT____Vlvbound_hc332c5d7__0;
    wakeupPipelineRegister__DOT____Vlvbound_hc332c5d7__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__526__detectRange;
    __Vfunc_SelectiveFlushDetector__526__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__526__headPtr;
    __Vfunc_SelectiveFlushDetector__526__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__526__tailPtr;
    __Vfunc_SelectiveFlushDetector__526__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__526__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__526__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__526__opPtr;
    __Vfunc_SelectiveFlushDetector__526__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__527__detectRange;
    __Vfunc_SelectiveFlushDetector__527__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__527__headPtr;
    __Vfunc_SelectiveFlushDetector__527__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__527__tailPtr;
    __Vfunc_SelectiveFlushDetector__527__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__527__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__527__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__527__opPtr;
    __Vfunc_SelectiveFlushDetector__527__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__528__detectRange;
    __Vfunc_SelectiveFlushDetector__528__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__528__headPtr;
    __Vfunc_SelectiveFlushDetector__528__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__528__tailPtr;
    __Vfunc_SelectiveFlushDetector__528__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__528__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__528__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__528__opPtr;
    __Vfunc_SelectiveFlushDetector__528__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__529__detectRange;
    __Vfunc_SelectiveFlushDetector__529__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__529__headPtr;
    __Vfunc_SelectiveFlushDetector__529__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__529__tailPtr;
    __Vfunc_SelectiveFlushDetector__529__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__529__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__529__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__529__opPtr;
    __Vfunc_SelectiveFlushDetector__529__opPtr = 0;
    // Body
    vlSelfRef.__PVT__scStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__scStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__scStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__scStage));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall 
        = vlSelfRef.__PVT__scStage__DOT__stall;
    vlSelfRef.__PVT__scStage__DOT__flushIQ_Entry = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushIQ_Entry;
    scStage__DOT____Vlvbound_h0ac9b13f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected
        [0U];
    vlSelfRef.__PVT__scStage__DOT__valid[0U] = scStage__DOT____Vlvbound_h0ac9b13f__0;
    scStage__DOT____Vlvbound_h353f532c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [0U];
    vlSelfRef.__PVT__scStage__DOT__issueQueuePtr[0U] 
        = scStage__DOT____Vlvbound_h353f532c__0;
    scStage__DOT____Vlvbound_h08e85d10__0 = (1U & ((IData)(vlSelfRef.__PVT__scStage__DOT__flushIQ_Entry) 
                                                   >> 
                                                   vlSelfRef.__PVT__scStage__DOT__issueQueuePtr
                                                   [0U]));
    vlSelfRef.__PVT__scStage__DOT__flush[0U] = scStage__DOT____Vlvbound_h08e85d10__0;
    scStage__DOT____Vlvbound_h6b1ccd1c__0 = ((((~ (IData)(vlSelfRef.__PVT__scStage__DOT__stall)) 
                                               & (~ (IData)(vlSelfRef.__PVT__scStage__DOT__clear))) 
                                              & vlSelfRef.__PVT__scStage__DOT__valid
                                              [0U]) 
                                             & (~ vlSelfRef.__PVT__scStage__DOT__flush
                                                [0U]));
    vlSelfRef.__PVT__scStage__DOT__update[0U] = scStage__DOT____Vlvbound_h6b1ccd1c__0;
    scStage__DOT____Vlvbound_he0235765__0 = ((~ ((((IData)(vlSelfRef.__PVT__scStage__DOT__stall) 
                                                   | (IData)(vlSelfRef.__PVT__scStage__DOT__clear)) 
                                                  | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                 | vlSelfRef.__PVT__scStage__DOT__flush
                                                 [0U])) 
                                             & vlSelfRef.__PVT__scStage__DOT__valid
                                             [0U]);
    vlSelfRef.__PVT__scStage__DOT__nextStage[0U] = 
        ((0xfU & vlSelfRef.__PVT__scStage__DOT__nextStage
          [0U]) | ((IData)(scStage__DOT____Vlvbound_he0235765__0) 
                   << 4U));
    scStage__DOT____Vlvbound_hf53c3fad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [0U];
    vlSelfRef.__PVT__scStage__DOT__nextStage[0U] = 
        ((0x10U & vlSelfRef.__PVT__scStage__DOT__nextStage
          [0U]) | (IData)(scStage__DOT____Vlvbound_hf53c3fad__0));
    vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__intNextStage[0U] 
        = vlSelfRef.__PVT__scStage__DOT__nextStage[0U];
    scStage__DOT____Vlvbound_h0ac9b13f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected
        [1U];
    vlSelfRef.__PVT__scStage__DOT__valid[1U] = scStage__DOT____Vlvbound_h0ac9b13f__0;
    scStage__DOT____Vlvbound_h353f532c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [1U];
    vlSelfRef.__PVT__scStage__DOT__issueQueuePtr[1U] 
        = scStage__DOT____Vlvbound_h353f532c__0;
    scStage__DOT____Vlvbound_h08e85d10__0 = (1U & ((IData)(vlSelfRef.__PVT__scStage__DOT__flushIQ_Entry) 
                                                   >> 
                                                   vlSelfRef.__PVT__scStage__DOT__issueQueuePtr
                                                   [1U]));
    vlSelfRef.__PVT__scStage__DOT__flush[1U] = scStage__DOT____Vlvbound_h08e85d10__0;
    scStage__DOT____Vlvbound_h6b1ccd1c__0 = ((((~ (IData)(vlSelfRef.__PVT__scStage__DOT__stall)) 
                                               & (~ (IData)(vlSelfRef.__PVT__scStage__DOT__clear))) 
                                              & vlSelfRef.__PVT__scStage__DOT__valid
                                              [1U]) 
                                             & (~ vlSelfRef.__PVT__scStage__DOT__flush
                                                [1U]));
    vlSelfRef.__PVT__scStage__DOT__update[1U] = scStage__DOT____Vlvbound_h6b1ccd1c__0;
    scStage__DOT____Vlvbound_he0235765__0 = ((~ ((((IData)(vlSelfRef.__PVT__scStage__DOT__stall) 
                                                   | (IData)(vlSelfRef.__PVT__scStage__DOT__clear)) 
                                                  | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                 | vlSelfRef.__PVT__scStage__DOT__flush
                                                 [1U])) 
                                             & vlSelfRef.__PVT__scStage__DOT__valid
                                             [1U]);
    vlSelfRef.__PVT__scStage__DOT__nextStage[1U] = 
        ((0xfU & vlSelfRef.__PVT__scStage__DOT__nextStage
          [1U]) | ((IData)(scStage__DOT____Vlvbound_he0235765__0) 
                   << 4U));
    scStage__DOT____Vlvbound_hf53c3fad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [1U];
    vlSelfRef.__PVT__scStage__DOT__nextStage[1U] = 
        ((0x10U & vlSelfRef.__PVT__scStage__DOT__nextStage
          [1U]) | (IData)(scStage__DOT____Vlvbound_hf53c3fad__0));
    vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__intNextStage[1U] 
        = vlSelfRef.__PVT__scStage__DOT__nextStage[1U];
    scStage__DOT____Vlvbound_h0ac9b13f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected
        [2U];
    vlSelfRef.__PVT__scStage__DOT__valid[2U] = scStage__DOT____Vlvbound_h0ac9b13f__0;
    scStage__DOT____Vlvbound_h353f532c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [2U];
    vlSelfRef.__PVT__scStage__DOT__issueQueuePtr[2U] 
        = scStage__DOT____Vlvbound_h353f532c__0;
    scStage__DOT____Vlvbound_h08e85d10__0 = (1U & ((IData)(vlSelfRef.__PVT__scStage__DOT__flushIQ_Entry) 
                                                   >> 
                                                   vlSelfRef.__PVT__scStage__DOT__issueQueuePtr
                                                   [2U]));
    vlSelfRef.__PVT__scStage__DOT__flush[2U] = scStage__DOT____Vlvbound_h08e85d10__0;
    scStage__DOT____Vlvbound_h6b1ccd1c__0 = ((((~ (IData)(vlSelfRef.__PVT__scStage__DOT__stall)) 
                                               & (~ (IData)(vlSelfRef.__PVT__scStage__DOT__clear))) 
                                              & vlSelfRef.__PVT__scStage__DOT__valid
                                              [2U]) 
                                             & (~ vlSelfRef.__PVT__scStage__DOT__flush
                                                [2U]));
    vlSelfRef.__PVT__scStage__DOT__update[2U] = scStage__DOT____Vlvbound_h6b1ccd1c__0;
    scStage__DOT____Vlvbound_he0235765__0 = ((~ ((((IData)(vlSelfRef.__PVT__scStage__DOT__stall) 
                                                   | (IData)(vlSelfRef.__PVT__scStage__DOT__clear)) 
                                                  | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                 | vlSelfRef.__PVT__scStage__DOT__flush
                                                 [2U])) 
                                             & vlSelfRef.__PVT__scStage__DOT__valid
                                             [2U]);
    vlSelfRef.__PVT__scStage__DOT__nextStage[2U] = 
        ((0xfU & vlSelfRef.__PVT__scStage__DOT__nextStage
          [2U]) | ((IData)(scStage__DOT____Vlvbound_he0235765__0) 
                   << 4U));
    scStage__DOT____Vlvbound_hf53c3fad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [2U];
    vlSelfRef.__PVT__scStage__DOT__nextStage[2U] = 
        ((0x10U & vlSelfRef.__PVT__scStage__DOT__nextStage
          [2U]) | (IData)(scStage__DOT____Vlvbound_hf53c3fad__0));
    scStage__DOT____Vlvbound_h345671f9__0 = vlSelfRef.__PVT__scStage__DOT__nextStage
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__complexNextStage[0U] 
        = scStage__DOT____Vlvbound_h345671f9__0;
    scStage__DOT____Vlvbound_h0ac9b13f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected
        [3U];
    vlSelfRef.__PVT__scStage__DOT__valid[3U] = scStage__DOT____Vlvbound_h0ac9b13f__0;
    scStage__DOT____Vlvbound_h353f532c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [3U];
    vlSelfRef.__PVT__scStage__DOT__issueQueuePtr[3U] 
        = scStage__DOT____Vlvbound_h353f532c__0;
    scStage__DOT____Vlvbound_h08e85d10__0 = (1U & ((IData)(vlSelfRef.__PVT__scStage__DOT__flushIQ_Entry) 
                                                   >> 
                                                   vlSelfRef.__PVT__scStage__DOT__issueQueuePtr
                                                   [3U]));
    vlSelfRef.__PVT__scStage__DOT__flush[3U] = scStage__DOT____Vlvbound_h08e85d10__0;
    scStage__DOT____Vlvbound_h6b1ccd1c__0 = ((((~ (IData)(vlSelfRef.__PVT__scStage__DOT__stall)) 
                                               & (~ (IData)(vlSelfRef.__PVT__scStage__DOT__clear))) 
                                              & vlSelfRef.__PVT__scStage__DOT__valid
                                              [3U]) 
                                             & (~ vlSelfRef.__PVT__scStage__DOT__flush
                                                [3U]));
    vlSelfRef.__PVT__scStage__DOT__update[3U] = scStage__DOT____Vlvbound_h6b1ccd1c__0;
    scStage__DOT____Vlvbound_he0235765__0 = ((~ ((((IData)(vlSelfRef.__PVT__scStage__DOT__stall) 
                                                   | (IData)(vlSelfRef.__PVT__scStage__DOT__clear)) 
                                                  | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                 | vlSelfRef.__PVT__scStage__DOT__flush
                                                 [3U])) 
                                             & vlSelfRef.__PVT__scStage__DOT__valid
                                             [3U]);
    vlSelfRef.__PVT__scStage__DOT__nextStage[3U] = 
        ((0xfU & vlSelfRef.__PVT__scStage__DOT__nextStage
          [3U]) | ((IData)(scStage__DOT____Vlvbound_he0235765__0) 
                   << 4U));
    scStage__DOT____Vlvbound_hf53c3fad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [3U];
    vlSelfRef.__PVT__scStage__DOT__nextStage[3U] = 
        ((0x10U & vlSelfRef.__PVT__scStage__DOT__nextStage
          [3U]) | (IData)(scStage__DOT____Vlvbound_hf53c3fad__0));
    vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__memNextStage[0U] 
        = vlSelfRef.__PVT__scStage__DOT__nextStage[3U];
    scStage__DOT____Vlvbound_h0ac9b13f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected
        [4U];
    vlSelfRef.__PVT__scStage__DOT__valid[4U] = scStage__DOT____Vlvbound_h0ac9b13f__0;
    scStage__DOT____Vlvbound_h353f532c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [4U];
    vlSelfRef.__PVT__scStage__DOT__issueQueuePtr[4U] 
        = scStage__DOT____Vlvbound_h353f532c__0;
    scStage__DOT____Vlvbound_h08e85d10__0 = (1U & ((IData)(vlSelfRef.__PVT__scStage__DOT__flushIQ_Entry) 
                                                   >> 
                                                   vlSelfRef.__PVT__scStage__DOT__issueQueuePtr
                                                   [4U]));
    vlSelfRef.__PVT__scStage__DOT__flush[4U] = scStage__DOT____Vlvbound_h08e85d10__0;
    scStage__DOT____Vlvbound_h6b1ccd1c__0 = ((((~ (IData)(vlSelfRef.__PVT__scStage__DOT__stall)) 
                                               & (~ (IData)(vlSelfRef.__PVT__scStage__DOT__clear))) 
                                              & vlSelfRef.__PVT__scStage__DOT__valid
                                              [4U]) 
                                             & (~ vlSelfRef.__PVT__scStage__DOT__flush
                                                [4U]));
    vlSelfRef.__PVT__scStage__DOT__update[4U] = scStage__DOT____Vlvbound_h6b1ccd1c__0;
    scStage__DOT____Vlvbound_he0235765__0 = ((~ ((((IData)(vlSelfRef.__PVT__scStage__DOT__stall) 
                                                   | (IData)(vlSelfRef.__PVT__scStage__DOT__clear)) 
                                                  | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                 | vlSelfRef.__PVT__scStage__DOT__flush
                                                 [4U])) 
                                             & vlSelfRef.__PVT__scStage__DOT__valid
                                             [4U]);
    vlSelfRef.__PVT__scStage__DOT__nextStage[4U] = 
        ((0xfU & vlSelfRef.__PVT__scStage__DOT__nextStage
          [4U]) | ((IData)(scStage__DOT____Vlvbound_he0235765__0) 
                   << 4U));
    scStage__DOT____Vlvbound_hf53c3fad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [4U];
    vlSelfRef.__PVT__scStage__DOT__nextStage[4U] = 
        ((0x10U & vlSelfRef.__PVT__scStage__DOT__nextStage
          [4U]) | (IData)(scStage__DOT____Vlvbound_hf53c3fad__0));
    vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__memNextStage[1U] 
        = vlSelfRef.__PVT__scStage__DOT__nextStage[4U];
    scStage__DOT____Vlvbound_h0ac9b13f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected
        [5U];
    vlSelfRef.__PVT__scStage__DOT__valid[5U] = scStage__DOT____Vlvbound_h0ac9b13f__0;
    scStage__DOT____Vlvbound_h353f532c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [5U];
    vlSelfRef.__PVT__scStage__DOT__issueQueuePtr[5U] 
        = scStage__DOT____Vlvbound_h353f532c__0;
    scStage__DOT____Vlvbound_h08e85d10__0 = (1U & ((IData)(vlSelfRef.__PVT__scStage__DOT__flushIQ_Entry) 
                                                   >> 
                                                   vlSelfRef.__PVT__scStage__DOT__issueQueuePtr
                                                   [5U]));
    vlSelfRef.__PVT__scStage__DOT__flush[5U] = scStage__DOT____Vlvbound_h08e85d10__0;
    scStage__DOT____Vlvbound_h6b1ccd1c__0 = ((((~ (IData)(vlSelfRef.__PVT__scStage__DOT__stall)) 
                                               & (~ (IData)(vlSelfRef.__PVT__scStage__DOT__clear))) 
                                              & vlSelfRef.__PVT__scStage__DOT__valid
                                              [5U]) 
                                             & (~ vlSelfRef.__PVT__scStage__DOT__flush
                                                [5U]));
    vlSelfRef.__PVT__scStage__DOT__update[5U] = scStage__DOT____Vlvbound_h6b1ccd1c__0;
    scStage__DOT____Vlvbound_he0235765__0 = ((~ ((((IData)(vlSelfRef.__PVT__scStage__DOT__stall) 
                                                   | (IData)(vlSelfRef.__PVT__scStage__DOT__clear)) 
                                                  | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                 | vlSelfRef.__PVT__scStage__DOT__flush
                                                 [5U])) 
                                             & vlSelfRef.__PVT__scStage__DOT__valid
                                             [5U]);
    vlSelfRef.__PVT__scStage__DOT__nextStage[5U] = 
        ((0xfU & vlSelfRef.__PVT__scStage__DOT__nextStage
          [5U]) | ((IData)(scStage__DOT____Vlvbound_he0235765__0) 
                   << 4U));
    scStage__DOT____Vlvbound_hf53c3fad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr
        [5U];
    vlSelfRef.__PVT__scStage__DOT__nextStage[5U] = 
        ((0x10U & vlSelfRef.__PVT__scStage__DOT__nextStage
          [5U]) | (IData)(scStage__DOT____Vlvbound_hf53c3fad__0));
    scStage__DOT____Vlvbound_h8e4acef0__0 = vlSelfRef.__PVT__scStage__DOT__nextStage
        [5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__fpNextStage[0U] 
        = scStage__DOT____Vlvbound_h8e4acef0__0;
    vlSelfRef.__PVT__scheduler__DOT__selectedVector = 0U;
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)))) {
        vlSelfRef.__PVT__scheduler__DOT__unnamedblk10__DOT__i = 6U;
        vlSelfRef.__PVT__scheduler__DOT__selectedVector 
            = ((IData)(vlSelfRef.__PVT__scheduler__DOT__selectedVector) 
               | vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
               [0U]);
        vlSelfRef.__PVT__scheduler__DOT__selectedVector 
            = ((IData)(vlSelfRef.__PVT__scheduler__DOT__selectedVector) 
               | vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
               [1U]);
        vlSelfRef.__PVT__scheduler__DOT__selectedVector 
            = ((IData)(vlSelfRef.__PVT__scheduler__DOT__selectedVector) 
               | vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
               [2U]);
        vlSelfRef.__PVT__scheduler__DOT__selectedVector 
            = ((IData)(vlSelfRef.__PVT__scheduler__DOT__selectedVector) 
               | vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
               [3U]);
        vlSelfRef.__PVT__scheduler__DOT__selectedVector 
            = ((IData)(vlSelfRef.__PVT__scheduler__DOT__selectedVector) 
               | vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
               [4U]);
        vlSelfRef.__PVT__scheduler__DOT__selectedVector 
            = ((IData)(vlSelfRef.__PVT__scheduler__DOT__selectedVector) 
               | vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
               [5U]);
    }
    scheduler__DOT____Vlvbound_h5f98ca24__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                                               [0U] 
                                               & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[0U] 
        = scheduler__DOT____Vlvbound_h5f98ca24__0;
    scheduler__DOT____Vlvbound_h5f98ca24__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                                               [1U] 
                                               & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[1U] 
        = scheduler__DOT____Vlvbound_h5f98ca24__0;
    scheduler__DOT____Vlvbound_h5f98ca24__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                                               [2U] 
                                               & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[2U] 
        = scheduler__DOT____Vlvbound_h5f98ca24__0;
    scheduler__DOT____Vlvbound_h5f98ca24__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                                               [3U] 
                                               & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[3U] 
        = scheduler__DOT____Vlvbound_h5f98ca24__0;
    scheduler__DOT____Vlvbound_h5f98ca24__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                                               [4U] 
                                               & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[4U] 
        = scheduler__DOT____Vlvbound_h5f98ca24__0;
    scheduler__DOT____Vlvbound_h5f98ca24__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                                               [5U] 
                                               & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[5U] 
        = scheduler__DOT____Vlvbound_h5f98ca24__0;
    wakeupPipelineRegister__DOT____Vlvbound_h031eb0bd__0 
        = (1U & ((vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                  [0U][0U] >> 0x1aU) & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h031eb0bd__0;
    wakeupPipelineRegister__DOT____Vlvbound_h031eb0bd__0 
        = (1U & ((vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                  [1U][0U] >> 0x1aU) & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[1U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h031eb0bd__0;
    wakeupPipelineRegister__DOT____Vlvbound_hac5d62af__0 
        = (1U & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
                 [0U][0U] >> 0x1aU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[2U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hac5d62af__0;
    wakeupPipelineRegister__DOT____Vlvbound_ha62a6d55__0 
        = (1U & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                 [0U][0U] >> 0x1aU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[3U] 
        = wakeupPipelineRegister__DOT____Vlvbound_ha62a6d55__0;
    wakeupPipelineRegister__DOT____Vlvbound_ha62a6d55__0 
        = (1U & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                 [1U][0U] >> 0x1aU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[4U] 
        = wakeupPipelineRegister__DOT____Vlvbound_ha62a6d55__0;
    wakeupPipelineRegister__DOT____Vlvbound_hc332c5d7__0 
        = (1U & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
                 [0U][0U] >> 0x1aU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[5U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hc332c5d7__0;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushIQ_Entry;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intSelectedPtr[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [0U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [0U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                      [0U] & (~ ((IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry) 
                                 >> vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intSelectedPtr
                                 [0U]))) << 0x1aU));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[0U] 
        = ((0x43fffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
                     [0U] << 0x16U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[0U] 
        = ((0x7c0003fU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [0U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
                      [0U] & (~ (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry))) 
                     << 6U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[0U] 
        = ((0x7ffffc0U & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr
           [0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intSelectedPtr[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [1U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[1U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [1U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                      [1U] & (~ ((IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry) 
                                 >> vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intSelectedPtr
                                 [1U]))) << 0x1aU));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[1U] 
        = ((0x43fffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
                     [1U] << 0x16U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[1U] 
        = ((0x7c0003fU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [1U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
                      [1U] & (~ (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry))) 
                     << 6U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[1U] 
        = ((0x7ffffc0U & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
            [1U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr
           [1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk31__DOT__i = 2U;
    wakeupPipelineRegister__DOT____Vlvbound_hdb2d82a3__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [2U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexSelectedPtr[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hdb2d82a3__0;
    wakeupPipelineRegister__DOT____Vlvbound_h9f74ea64__0 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
           [2U] & (~ ((IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry) 
                      >> vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexSelectedPtr
                      [0U])));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg[0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
            [0U]) | ((IData)(wakeupPipelineRegister__DOT____Vlvbound_h9f74ea64__0) 
                     << 0x1aU));
    wakeupPipelineRegister__DOT____Vlvbound_h0c642a40__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [2U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg[0U] 
        = ((0x43fffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
            [0U]) | ((IData)(wakeupPipelineRegister__DOT____Vlvbound_h0c642a40__0) 
                     << 0x16U));
    wakeupPipelineRegister__DOT____Vlvbound_h08ae36e2__0 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
           [2U] & (~ (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry)));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg[0U] 
        = ((0x7c0003fU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
            [0U]) | ((IData)(wakeupPipelineRegister__DOT____Vlvbound_h08ae36e2__0) 
                     << 6U));
    wakeupPipelineRegister__DOT____Vlvbound_h120d9f38__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr
        [2U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg[0U] 
        = ((0x7ffffc0U & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
            [0U]) | (IData)(wakeupPipelineRegister__DOT____Vlvbound_h120d9f38__0));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk32__DOT__i = 1U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memSelectedPtr[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [3U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [0U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                      [3U] & (~ ((IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry) 
                                 >> vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memSelectedPtr
                                 [0U]))) << 0x1aU));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[0U] 
        = ((0x43fffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
                     [3U] << 0x16U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[0U] 
        = ((0x7c0003fU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [0U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
                      [3U] & (~ (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry))) 
                     << 6U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[0U] 
        = ((0x7ffffc0U & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr
           [3U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memSelectedPtr[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [4U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[1U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [1U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
                      [4U] & (~ ((IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry) 
                                 >> vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memSelectedPtr
                                 [1U]))) << 0x1aU));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[1U] 
        = ((0x43fffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
                     [4U] << 0x16U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[1U] 
        = ((0x7c0003fU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [1U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
                      [4U] & (~ (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry))) 
                     << 6U));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[1U] 
        = ((0x7ffffc0U & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
            [1U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr
           [4U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk33__DOT__i = 2U;
    wakeupPipelineRegister__DOT____Vlvbound_ha6fd7b38__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [5U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpSelectedPtr[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_ha6fd7b38__0;
    wakeupPipelineRegister__DOT____Vlvbound_hef721fdd__0 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected
           [5U] & (~ ((IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry) 
                      >> vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpSelectedPtr
                      [0U])));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg[0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
            [0U]) | ((IData)(wakeupPipelineRegister__DOT____Vlvbound_hef721fdd__0) 
                     << 0x1aU));
    wakeupPipelineRegister__DOT____Vlvbound_ha9d6caef__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr
        [5U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg[0U] 
        = ((0x43fffffU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
            [0U]) | ((IData)(wakeupPipelineRegister__DOT____Vlvbound_ha9d6caef__0) 
                     << 0x16U));
    wakeupPipelineRegister__DOT____Vlvbound_hec96f25a__0 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector
           [5U] & (~ (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry)));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg[0U] 
        = ((0x7c0003fU & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
            [0U]) | ((IData)(wakeupPipelineRegister__DOT____Vlvbound_hec96f25a__0) 
                     << 6U));
    wakeupPipelineRegister__DOT____Vlvbound_ha3c87944__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr
        [5U];
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg[0U] 
        = ((0x7ffffc0U & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
            [0U]) | (IData)(wakeupPipelineRegister__DOT____Vlvbound_ha3c87944__0));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk34__DOT__i = 1U;
    __Vfunc_SelectiveFlushDetector__526__opPtr = (0x3fU 
                                                  & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                                                  [0U]
                                                  [0U]);
    __Vfunc_SelectiveFlushDetector__526__flushAllInsns 
        = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__526__tailPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr;
    __Vfunc_SelectiveFlushDetector__526__headPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr;
    __Vfunc_SelectiveFlushDetector__526__detectRange 
        = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountInt;
    {
        if (__Vfunc_SelectiveFlushDetector__526__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__526__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                goto __Vlabel154;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__526__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                    goto __Vlabel154;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
                    goto __Vlabel154;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__526__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                    goto __Vlabel154;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                    goto __Vlabel154;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
                    goto __Vlabel154;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
                goto __Vlabel154;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
        }
        __Vlabel154: ;
    }
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushInt[0U] 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout;
    wakeupPipelineRegister__DOT____Vlvbound_h87bbb49c__0 
        = (1U & (((vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                   [0U][0U] >> 0x1aU) & (~ vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushInt
                                         [0U])) & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h87bbb49c__0;
    wakeupPipelineRegister__DOT____Vlvbound_hd219707c__0 
        = (0xfU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                   [0U][0U] >> 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hd219707c__0;
    wakeupPipelineRegister__DOT____Vlvbound_h89f1c5a3__0 
        = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)
            ? 0U : (0xffffU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                               [0U][0U] >> 6U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h89f1c5a3__0;
    __Vfunc_SelectiveFlushDetector__526__opPtr = (0x3fU 
                                                  & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                                                  [1U]
                                                  [0U]);
    __Vfunc_SelectiveFlushDetector__526__flushAllInsns 
        = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__526__tailPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr;
    __Vfunc_SelectiveFlushDetector__526__headPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr;
    __Vfunc_SelectiveFlushDetector__526__detectRange 
        = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountInt;
    {
        if (__Vfunc_SelectiveFlushDetector__526__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__526__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                goto __Vlabel155;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__526__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                    goto __Vlabel155;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
                    goto __Vlabel155;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__526__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                    goto __Vlabel155;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__526__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__526__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__526__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 1U;
                    goto __Vlabel155;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
                    goto __Vlabel155;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
                goto __Vlabel155;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout = 0U;
        }
        __Vlabel155: ;
    }
    wakeupPipelineRegister__DOT____Vlvbound_h89f1c5a3__0 
        = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall)
            ? 0U : (0xffffU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                               [1U][0U] >> 6U)));
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushInt[1U] 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__526__Vfuncout;
    wakeupPipelineRegister__DOT____Vlvbound_h87bbb49c__0 
        = (1U & (((vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                   [1U][0U] >> 0x1aU) & (~ vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushInt
                                         [1U])) & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[1U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h87bbb49c__0;
    wakeupPipelineRegister__DOT____Vlvbound_hd219707c__0 
        = (0xfU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
                   [1U][0U] >> 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[1U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hd219707c__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[1U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h89f1c5a3__0;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk35__DOT__i = 2U;
    __Vfunc_SelectiveFlushDetector__527__opPtr = (0x3fU 
                                                  & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
                                                  [0U]
                                                  [0U]);
    __Vfunc_SelectiveFlushDetector__527__flushAllInsns 
        = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__527__tailPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr;
    __Vfunc_SelectiveFlushDetector__527__headPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr;
    __Vfunc_SelectiveFlushDetector__527__detectRange 
        = (0U != (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountComplex));
    {
        if (__Vfunc_SelectiveFlushDetector__527__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__527__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 1U;
                goto __Vlabel156;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__527__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__527__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__527__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__527__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__527__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__527__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__527__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 1U;
                    goto __Vlabel156;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 0U;
                    goto __Vlabel156;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__527__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__527__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__527__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__527__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__527__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__527__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__527__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 1U;
                    goto __Vlabel156;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__527__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__527__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__527__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__527__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 1U;
                    goto __Vlabel156;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 0U;
                    goto __Vlabel156;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 0U;
                goto __Vlabel156;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout = 0U;
        }
        __Vlabel156: ;
    }
    wakeupPipelineRegister__DOT____Vlvbound_hf246e9eb__0 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__527__Vfuncout;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushComplex[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hf246e9eb__0;
    wakeupPipelineRegister__DOT____Vlvbound_h71c4a121__0 
        = (1U & ((vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
                  [0U][0U] >> 0x1aU) & (~ vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushComplex
                                        [0U])));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[2U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h71c4a121__0;
    wakeupPipelineRegister__DOT____Vlvbound_h1c2242f7__0 
        = (0xfU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
                   [0U][0U] >> 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[2U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h1c2242f7__0;
    wakeupPipelineRegister__DOT____Vlvbound_h3fe92220__0 
        = (0xffffU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
                      [0U][0U] >> 6U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[2U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h3fe92220__0;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk36__DOT__i = 1U;
    __Vfunc_SelectiveFlushDetector__528__opPtr = (0x3fU 
                                                  & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                                                  [0U]
                                                  [0U]);
    __Vfunc_SelectiveFlushDetector__528__flushAllInsns 
        = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__528__tailPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr;
    __Vfunc_SelectiveFlushDetector__528__headPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr;
    __Vfunc_SelectiveFlushDetector__528__detectRange 
        = (0U != (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountMem));
    {
        if (__Vfunc_SelectiveFlushDetector__528__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__528__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 1U;
                goto __Vlabel157;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__528__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__528__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__528__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__528__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__528__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__528__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__528__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 1U;
                    goto __Vlabel157;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 0U;
                    goto __Vlabel157;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__528__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__528__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__528__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__528__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__528__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__528__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__528__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 1U;
                    goto __Vlabel157;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__528__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__528__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__528__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__528__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 1U;
                    goto __Vlabel157;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 0U;
                    goto __Vlabel157;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 0U;
                goto __Vlabel157;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout = 0U;
        }
        __Vlabel157: ;
    }
    wakeupPipelineRegister__DOT____Vlvbound_h966cd435__0 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__528__Vfuncout;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushMem[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h966cd435__0;
    wakeupPipelineRegister__DOT____Vlvbound_hac0d33d7__0 
        = (1U & ((vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                  [0U][0U] >> 0x1aU) & (~ vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushMem
                                        [0U])));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[3U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hac0d33d7__0;
    wakeupPipelineRegister__DOT____Vlvbound_hb609efdd__0 
        = (0xfU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                   [0U][0U] >> 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[3U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hb609efdd__0;
    wakeupPipelineRegister__DOT____Vlvbound_hadc244c6__0 
        = (0xffffU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                      [0U][0U] >> 6U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[3U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hadc244c6__0;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk37__DOT__i = 1U;
    wakeupPipelineRegister__DOT____Vlvbound_hdaf3aedd__0 
        = (0xfU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                   [1U][0U] >> 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[5U] 
        = wakeupPipelineRegister__DOT____Vlvbound_hdaf3aedd__0;
    wakeupPipelineRegister__DOT____Vlvbound_h811685c6__0 
        = (0xffffU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
                      [1U][0U] >> 6U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[5U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h811685c6__0;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk38__DOT__i = 1U;
    __Vfunc_SelectiveFlushDetector__529__opPtr = (0x3fU 
                                                  & vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
                                                  [0U]
                                                  [0U]);
    __Vfunc_SelectiveFlushDetector__529__flushAllInsns 
        = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__529__tailPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr;
    __Vfunc_SelectiveFlushDetector__529__headPtr = vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr;
    __Vfunc_SelectiveFlushDetector__529__detectRange 
        = (0U != (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountFP));
    {
        if (__Vfunc_SelectiveFlushDetector__529__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__529__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 1U;
                goto __Vlabel158;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__529__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__529__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__529__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__529__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__529__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__529__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__529__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 1U;
                    goto __Vlabel158;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 0U;
                    goto __Vlabel158;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__529__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__529__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__529__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__529__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__529__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__529__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__529__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 1U;
                    goto __Vlabel158;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__529__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__529__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__529__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__529__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 1U;
                    goto __Vlabel158;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 0U;
                    goto __Vlabel158;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 0U;
                goto __Vlabel158;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout = 0U;
        }
        __Vlabel158: ;
    }
    wakeupPipelineRegister__DOT____Vlvbound_he223bad4__0 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__529__Vfuncout;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushFP[0U] 
        = wakeupPipelineRegister__DOT____Vlvbound_he223bad4__0;
    wakeupPipelineRegister__DOT____Vlvbound_h793cefab__0 
        = (1U & ((vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
                  [0U][0U] >> 0x1aU) & (~ vlSelfRef.__PVT__wakeupPipelineRegister__DOT__flushFP
                                        [0U])));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[4U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h793cefab__0;
    wakeupPipelineRegister__DOT____Vlvbound_h0399b589__0 
        = (0xfU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
                   [0U][0U] >> 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[4U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h0399b589__0;
    wakeupPipelineRegister__DOT____Vlvbound_h5770f0b2__0 
        = (0xffffU & (vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
                      [0U][0U] >> 6U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[4U] 
        = wakeupPipelineRegister__DOT____Vlvbound_h5770f0b2__0;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk39__DOT__i = 1U;
}
