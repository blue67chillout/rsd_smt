// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RegisterFile.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__0(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vlvbound_h7f3dc019__0;
    __Vlvbound_h7f3dc019__0 = 0;
    CData/*6:0*/ __Vlvbound_h63ac04e7__0;
    __Vlvbound_h63ac04e7__0 = 0;
    QData/*32:0*/ __Vlvbound_h66422b62__0;
    __Vlvbound_h66422b62__0 = 0;
    CData/*0:0*/ __Vlvbound_h6c2b73f0__0;
    __Vlvbound_h6c2b73f0__0 = 0;
    CData/*6:0*/ __Vlvbound_h3802c8b6__0;
    __Vlvbound_h3802c8b6__0 = 0;
    QData/*32:0*/ __Vlvbound_hd2d68753__0;
    __Vlvbound_hd2d68753__0 = 0;
    CData/*0:0*/ __Vlvbound_h3cb25d40__0;
    __Vlvbound_h3cb25d40__0 = 0;
    CData/*6:0*/ __Vlvbound_h4877a006__0;
    __Vlvbound_h4877a006__0 = 0;
    QData/*32:0*/ __Vlvbound_h031d8dc3__0;
    __Vlvbound_h031d8dc3__0 = 0;
    CData/*0:0*/ __Vlvbound_h6356f7e7__0;
    __Vlvbound_h6356f7e7__0 = 0;
    CData/*6:0*/ __Vlvbound_h7fb74c9d__0;
    __Vlvbound_h7fb74c9d__0 = 0;
    QData/*32:0*/ __Vlvbound_hca3b039c__0;
    __Vlvbound_hca3b039c__0 = 0;
    // Body
    vlSelfRef.__PVT__dstFPRegData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegData
        [0U];
    vlSelfRef.__PVT__dstFPRegData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegData
        [0U];
    __Vlvbound_h66422b62__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[0U] = __Vlvbound_h66422b62__0;
    __Vlvbound_h66422b62__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData
        [1U];
    vlSelfRef.__PVT__dstRegData[1U] = __Vlvbound_h66422b62__0;
    __Vlvbound_hd2d68753__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[2U] = __Vlvbound_hd2d68753__0;
    __Vlvbound_h031d8dc3__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[3U] = __Vlvbound_h031d8dc3__0;
    __Vlvbound_hca3b039c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[4U] = __Vlvbound_hca3b039c__0;
    vlSelfRef.__PVT__fpRegWE[0U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegWE
                                    [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                                            [0U] >> 6U));
    vlSelfRef.__PVT__fpRegWE[1U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegWE
                                    [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                            [0U] >> 6U));
    vlSelfRef.__PVT__dstFPRegNum[0U] = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstTid
                                                               [0U], 6U) 
                                                 + 
                                                 (0x3fU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                                                  [0U])));
    vlSelfRef.__PVT__dstFPRegNum[1U] = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstTid
                                                               [0U], 6U) 
                                                 + 
                                                 (0x3fU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                                  [0U])));
    __Vlvbound_h7f3dc019__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[0U] = __Vlvbound_h7f3dc019__0;
    __Vlvbound_h7f3dc019__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE
                               [1U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                          [1U] >> 6U)));
    vlSelfRef.__PVT__regWE[1U] = __Vlvbound_h7f3dc019__0;
    __Vlvbound_h6c2b73f0__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[2U] = __Vlvbound_h6c2b73f0__0;
    __Vlvbound_h3cb25d40__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[3U] = __Vlvbound_h3cb25d40__0;
    __Vlvbound_h6356f7e7__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[4U] = __Vlvbound_h6356f7e7__0;
    __Vlvbound_h63ac04e7__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstTid
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                           [0U])));
    vlSelfRef.__PVT__dstRegNum[0U] = __Vlvbound_h63ac04e7__0;
    __Vlvbound_h63ac04e7__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstTid
                                                      [1U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                           [1U])));
    vlSelfRef.__PVT__dstRegNum[1U] = __Vlvbound_h63ac04e7__0;
    __Vlvbound_h3802c8b6__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstTid
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegNum
                                           [0U])));
    vlSelfRef.__PVT__dstRegNum[2U] = __Vlvbound_h3802c8b6__0;
    __Vlvbound_h4877a006__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstTid
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                           [0U])));
    vlSelfRef.__PVT__dstRegNum[3U] = __Vlvbound_h4877a006__0;
    __Vlvbound_h7fb74c9d__0 = (0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                               [0U]);
    vlSelfRef.__PVT__dstRegNum[4U] = __Vlvbound_h7fb74c9d__0;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk6__DOT__i = 5U;
        vlSelfRef.__PVT__unnamedblk10__DOT__i = 2U;
        vlSelfRef.__PVT__dstFPRegData[0U] = (0x100000000ULL 
                                             & vlSelfRef.__PVT__dstFPRegData
                                             [0U]);
        vlSelfRef.__PVT__dstFPRegData[0U] = (0x100000000ULL 
                                             | vlSelfRef.__PVT__dstFPRegData
                                             [0U]);
        vlSelfRef.__PVT__dstFPRegData[1U] = (0x100000000ULL 
                                             & vlSelfRef.__PVT__dstFPRegData
                                             [1U]);
        vlSelfRef.__PVT__dstFPRegData[1U] = (0x100000000ULL 
                                             | vlSelfRef.__PVT__dstFPRegData
                                             [1U]);
        vlSelfRef.__PVT__dstRegData[0U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [0U]);
        vlSelfRef.__PVT__dstRegData[0U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [0U]);
        vlSelfRef.__PVT__dstRegData[1U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [1U]);
        vlSelfRef.__PVT__dstRegData[1U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [1U]);
        vlSelfRef.__PVT__dstRegData[2U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [2U]);
        vlSelfRef.__PVT__dstRegData[2U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [2U]);
        vlSelfRef.__PVT__dstRegData[3U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [3U]);
        vlSelfRef.__PVT__dstRegData[3U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [3U]);
        vlSelfRef.__PVT__dstRegData[4U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [4U]);
        vlSelfRef.__PVT__dstRegData[4U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [4U]);
        vlSelfRef.__PVT__fpRegWE[0U] = 1U;
        vlSelfRef.__PVT__fpRegWE[1U] = 1U;
        vlSelfRef.__PVT__dstFPRegNum[0U] = vlSelfRef.__PVT__fpRstIndex;
        vlSelfRef.__PVT__dstFPRegNum[1U] = (0x7fU & 
                                            ((IData)(1U) 
                                             + (IData)(vlSelfRef.__PVT__fpRstIndex)));
        vlSelfRef.__PVT__regWE[0U] = 1U;
        vlSelfRef.__PVT__regWE[1U] = 1U;
        vlSelfRef.__PVT__regWE[2U] = 1U;
        vlSelfRef.__PVT__regWE[3U] = 1U;
        vlSelfRef.__PVT__regWE[4U] = 1U;
        vlSelfRef.__Vlvbound_h8b146626__0 = vlSelfRef.__PVT__regRstIndex;
        vlSelfRef.__PVT__dstRegNum[0U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(1U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[1U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(2U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[2U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(3U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[3U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(4U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[4U] = vlSelfRef.__Vlvbound_h8b146626__0;
    }
    vlSelfRef.__Vcellinp__phyFPReg__wv[0U] = vlSelfRef.__PVT__dstFPRegData
        [0U];
    vlSelfRef.__Vcellinp__phyFPReg__wv[1U] = vlSelfRef.__PVT__dstFPRegData
        [1U];
    vlSelfRef.__Vcellinp__phyReg__wv[0U] = vlSelfRef.__PVT__dstRegData
        [0U];
    vlSelfRef.__Vcellinp__phyReg__wv[1U] = vlSelfRef.__PVT__dstRegData
        [1U];
    vlSelfRef.__Vcellinp__phyReg__wv[2U] = vlSelfRef.__PVT__dstRegData
        [2U];
    vlSelfRef.__Vcellinp__phyReg__wv[3U] = vlSelfRef.__PVT__dstRegData
        [3U];
    vlSelfRef.__Vcellinp__phyReg__wv[4U] = vlSelfRef.__PVT__dstRegData
        [4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__we[0U] 
        = vlSelfRef.__PVT__fpRegWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__we[1U] 
        = vlSelfRef.__PVT__fpRegWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wa[0U] 
        = vlSelfRef.__PVT__dstFPRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wa[1U] 
        = vlSelfRef.__PVT__dstFPRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[0U] 
        = vlSelfRef.__PVT__regWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[1U] 
        = vlSelfRef.__PVT__regWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[2U] 
        = vlSelfRef.__PVT__regWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[3U] 
        = vlSelfRef.__PVT__regWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[4U] 
        = vlSelfRef.__PVT__regWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[0U] 
        = vlSelfRef.__PVT__dstRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[1U] 
        = vlSelfRef.__PVT__dstRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[2U] 
        = vlSelfRef.__PVT__dstRegNum[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[3U] 
        = vlSelfRef.__PVT__dstRegNum[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[4U] 
        = vlSelfRef.__PVT__dstRegNum[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__phyFPReg__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__phyFPReg__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[2U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[3U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[4U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[4U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__1(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__ra[0U] 
        = vlSelfRef.__PVT__srcFPRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__ra[1U] 
        = vlSelfRef.__PVT__srcFPRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__ra[2U] 
        = vlSelfRef.__PVT__srcFPRegNum[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__ra[3U] 
        = vlSelfRef.__PVT__srcFPRegNum[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__ra[4U] 
        = vlSelfRef.__PVT__srcFPRegNum[4U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__2(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    QData/*32:0*/ __Vlvbound_h741f6621__0;
    __Vlvbound_h741f6621__0 = 0;
    QData/*32:0*/ __Vlvbound_hbc238481__0;
    __Vlvbound_hbc238481__0 = 0;
    // Body
    vlSelfRef.__Vcellout__phyFPReg__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__phyFPReg__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [1U];
    vlSelfRef.__Vcellout__phyFPReg__rv[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [2U];
    vlSelfRef.__Vcellout__phyFPReg__rv[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [3U];
    vlSelfRef.__Vcellout__phyFPReg__rv[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [4U];
    vlSelfRef.__PVT__srcFPRegData[0U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [0U];
    vlSelfRef.__PVT__srcFPRegData[1U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [1U];
    vlSelfRef.__PVT__srcFPRegData[2U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [2U];
    vlSelfRef.__PVT__srcFPRegData[3U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [3U];
    vlSelfRef.__PVT__srcFPRegData[4U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [4U];
    __Vlvbound_h741f6621__0 = vlSelfRef.__PVT__srcFPRegData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataB[0U] 
        = __Vlvbound_h741f6621__0;
    __Vlvbound_hbc238481__0 = vlSelfRef.__PVT__srcFPRegData
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataC[0U] 
        = __Vlvbound_hbc238481__0;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__3(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[0U] 
        = vlSelfRef.__PVT__srcRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[1U] 
        = vlSelfRef.__PVT__srcRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[2U] 
        = vlSelfRef.__PVT__srcRegNum[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[3U] 
        = vlSelfRef.__PVT__srcRegNum[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[4U] 
        = vlSelfRef.__PVT__srcRegNum[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[5U] 
        = vlSelfRef.__PVT__srcRegNum[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[6U] 
        = vlSelfRef.__PVT__srcRegNum[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[7U] 
        = vlSelfRef.__PVT__srcRegNum[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[8U] 
        = vlSelfRef.__PVT__srcRegNum[8U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[9U] 
        = vlSelfRef.__PVT__srcRegNum[9U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__ra[0xaU] 
        = vlSelfRef.__PVT__srcRegNum[0xaU];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__4(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_sequent__TOP__SMT_RTL_Testbench__core__registerFile__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    QData/*32:0*/ __Vlvbound_h763ce95f__0;
    __Vlvbound_h763ce95f__0 = 0;
    QData/*32:0*/ __Vlvbound_h0494d203__0;
    __Vlvbound_h0494d203__0 = 0;
    // Body
    vlSelfRef.__Vcellout__phyReg__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__phyReg__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [1U];
    vlSelfRef.__Vcellout__phyReg__rv[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [2U];
    vlSelfRef.__Vcellout__phyReg__rv[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [3U];
    vlSelfRef.__Vcellout__phyReg__rv[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [4U];
    vlSelfRef.__Vcellout__phyReg__rv[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [5U];
    vlSelfRef.__Vcellout__phyReg__rv[6U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [6U];
    vlSelfRef.__Vcellout__phyReg__rv[7U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [7U];
    vlSelfRef.__Vcellout__phyReg__rv[8U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [8U];
    vlSelfRef.__Vcellout__phyReg__rv[9U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [9U];
    vlSelfRef.__Vcellout__phyReg__rv[0xaU] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__rv
        [0xaU];
    vlSelfRef.__PVT__srcRegData[0U] = vlSelfRef.__Vcellout__phyReg__rv
        [0U];
    vlSelfRef.__PVT__srcRegData[1U] = vlSelfRef.__Vcellout__phyReg__rv
        [1U];
    vlSelfRef.__PVT__srcRegData[2U] = vlSelfRef.__Vcellout__phyReg__rv
        [2U];
    vlSelfRef.__PVT__srcRegData[3U] = vlSelfRef.__Vcellout__phyReg__rv
        [3U];
    vlSelfRef.__PVT__srcRegData[4U] = vlSelfRef.__Vcellout__phyReg__rv
        [4U];
    vlSelfRef.__PVT__srcRegData[5U] = vlSelfRef.__Vcellout__phyReg__rv
        [5U];
    vlSelfRef.__PVT__srcRegData[6U] = vlSelfRef.__Vcellout__phyReg__rv
        [6U];
    vlSelfRef.__PVT__srcRegData[7U] = vlSelfRef.__Vcellout__phyReg__rv
        [7U];
    vlSelfRef.__PVT__srcRegData[8U] = vlSelfRef.__Vcellout__phyReg__rv
        [8U];
    vlSelfRef.__PVT__srcRegData[9U] = vlSelfRef.__Vcellout__phyReg__rv
        [9U];
    vlSelfRef.__PVT__srcRegData[0xaU] = vlSelfRef.__Vcellout__phyReg__rv
        [0xaU];
    __Vlvbound_h763ce95f__0 = vlSelfRef.__PVT__srcRegData
        [4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegDataA[0U] 
        = __Vlvbound_h763ce95f__0;
    __Vlvbound_h0494d203__0 = vlSelfRef.__PVT__srcRegData
        [5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegDataB[0U] 
        = __Vlvbound_h0494d203__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA[0U] 
        = vlSelfRef.__PVT__srcRegData[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA[1U] 
        = vlSelfRef.__PVT__srcRegData[8U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB[0U] 
        = vlSelfRef.__PVT__srcRegData[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB[1U] 
        = vlSelfRef.__PVT__srcRegData[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA[0U] 
        = vlSelfRef.__PVT__srcRegData[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA[1U] 
        = vlSelfRef.__PVT__srcRegData[2U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__0(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB[0U] 
        = ((0x40U & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
            [0U]) ? vlSelfRef.__PVT__srcFPRegData[3U]
            : vlSelfRef.__PVT__srcRegData[7U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB[1U] 
        = ((0x40U & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
            [1U]) ? vlSelfRef.__PVT__srcFPRegData[4U]
            : vlSelfRef.__PVT__srcRegData[9U]);
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__1(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    QData/*32:0*/ __Vlvbound_h18072dbf__0;
    __Vlvbound_h18072dbf__0 = 0;
    // Body
    __Vlvbound_h18072dbf__0 = ((0x40U & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumA
                                [0U]) ? vlSelfRef.__PVT__srcFPRegData
                               [0U] : vlSelfRef.__PVT__srcRegData
                               [0xaU]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataA[0U] 
        = __Vlvbound_h18072dbf__0;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__2(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ __Vlvbound_h7cfb2dd6__0;
    __Vlvbound_h7cfb2dd6__0 = 0;
    CData/*6:0*/ __Vlvbound_hffe74a2b__0;
    __Vlvbound_hffe74a2b__0 = 0;
    CData/*6:0*/ __Vlvbound_hccbe8e78__0;
    __Vlvbound_hccbe8e78__0 = 0;
    CData/*6:0*/ __Vlvbound_h4bbfad65__0;
    __Vlvbound_h4bbfad65__0 = 0;
    CData/*6:0*/ __Vlvbound_h22506be4__0;
    __Vlvbound_h22506be4__0 = 0;
    CData/*6:0*/ __Vlvbound_h04c01a89__0;
    __Vlvbound_h04c01a89__0 = 0;
    CData/*6:0*/ __Vlvbound_h77da9e2c__0;
    __Vlvbound_h77da9e2c__0 = 0;
    // Body
    __Vlvbound_h7cfb2dd6__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcTidA
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA
                                           [0U])));
    vlSelfRef.__PVT__srcRegNum[0U] = __Vlvbound_h7cfb2dd6__0;
    __Vlvbound_hffe74a2b__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcTidB
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB
                                           [0U])));
    vlSelfRef.__PVT__srcRegNum[1U] = __Vlvbound_hffe74a2b__0;
    __Vlvbound_h7cfb2dd6__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcTidA
                                                      [1U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA
                                           [1U])));
    vlSelfRef.__PVT__srcRegNum[2U] = __Vlvbound_h7cfb2dd6__0;
    __Vlvbound_hffe74a2b__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcTidB
                                                      [1U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB
                                           [1U])));
    vlSelfRef.__PVT__srcRegNum[3U] = __Vlvbound_hffe74a2b__0;
    __Vlvbound_hccbe8e78__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcTidA
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumA
                                           [0U])));
    vlSelfRef.__PVT__srcRegNum[4U] = __Vlvbound_hccbe8e78__0;
    __Vlvbound_h4bbfad65__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcTidB
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumB
                                           [0U])));
    vlSelfRef.__PVT__srcRegNum[5U] = __Vlvbound_h4bbfad65__0;
    __Vlvbound_h22506be4__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcTidA
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA
                                           [0U])));
    vlSelfRef.__PVT__srcRegNum[6U] = __Vlvbound_h22506be4__0;
    __Vlvbound_h04c01a89__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcTidB
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                           [0U])));
    vlSelfRef.__PVT__srcRegNum[7U] = __Vlvbound_h04c01a89__0;
    __Vlvbound_h22506be4__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcTidA
                                                      [1U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA
                                           [1U])));
    vlSelfRef.__PVT__srcRegNum[8U] = __Vlvbound_h22506be4__0;
    __Vlvbound_h04c01a89__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcTidB
                                                      [1U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                           [1U])));
    vlSelfRef.__PVT__srcRegNum[9U] = __Vlvbound_h04c01a89__0;
    __Vlvbound_h77da9e2c__0 = (0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumA
                               [0U]);
    vlSelfRef.__PVT__srcRegNum[0xaU] = __Vlvbound_h77da9e2c__0;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__3(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___act_comb__TOP__SMT_RTL_Testbench__core__registerFile__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ __Vlvbound_h51dec7c1__0;
    __Vlvbound_h51dec7c1__0 = 0;
    CData/*6:0*/ __Vlvbound_hc7bfe2e1__0;
    __Vlvbound_hc7bfe2e1__0 = 0;
    CData/*6:0*/ __Vlvbound_hc7c1b372__0;
    __Vlvbound_hc7c1b372__0 = 0;
    CData/*6:0*/ __Vlvbound_h90022c08__0;
    __Vlvbound_h90022c08__0 = 0;
    // Body
    __Vlvbound_h51dec7c1__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcTidA
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumA
                                           [0U])));
    vlSelfRef.__PVT__srcFPRegNum[0U] = __Vlvbound_h51dec7c1__0;
    __Vlvbound_hc7bfe2e1__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcTidB
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumB
                                           [0U])));
    vlSelfRef.__PVT__srcFPRegNum[1U] = __Vlvbound_hc7bfe2e1__0;
    __Vlvbound_hc7c1b372__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcTidC
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumC
                                           [0U])));
    vlSelfRef.__PVT__srcFPRegNum[2U] = __Vlvbound_hc7c1b372__0;
    __Vlvbound_h90022c08__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcTidB
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                           [0U])));
    vlSelfRef.__PVT__srcFPRegNum[3U] = __Vlvbound_h90022c08__0;
    __Vlvbound_h90022c08__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcTidB
                                                      [1U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                           [1U])));
    vlSelfRef.__PVT__srcFPRegNum[4U] = __Vlvbound_h90022c08__0;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___nba_sequent__TOP__SMT_RTL_Testbench__core__registerFile__0(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___nba_sequent__TOP__SMT_RTL_Testbench__core__registerFile__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    QData/*32:0*/ __Vlvbound_h741f6621__0;
    __Vlvbound_h741f6621__0 = 0;
    QData/*32:0*/ __Vlvbound_hbc238481__0;
    __Vlvbound_hbc238481__0 = 0;
    // Body
    vlSelfRef.__Vcellout__phyFPReg__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__phyFPReg__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [1U];
    vlSelfRef.__Vcellout__phyFPReg__rv[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [2U];
    vlSelfRef.__Vcellout__phyFPReg__rv[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [3U];
    vlSelfRef.__Vcellout__phyFPReg__rv[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__rv
        [4U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart) {
        vlSelfRef.__PVT__fpRstIndex = 0U;
        vlSelfRef.__PVT__regRstIndex = 0U;
    } else {
        vlSelfRef.__PVT__fpRstIndex = (0x7fU & ((IData)(2U) 
                                                + (IData)(vlSelfRef.__PVT__fpRstIndex)));
        vlSelfRef.__PVT__regRstIndex = (0x7fU & ((IData)(5U) 
                                                 + (IData)(vlSelfRef.__PVT__regRstIndex)));
    }
    vlSelfRef.__PVT__srcFPRegData[0U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [0U];
    vlSelfRef.__PVT__srcFPRegData[1U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [1U];
    vlSelfRef.__PVT__srcFPRegData[2U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [2U];
    vlSelfRef.__PVT__srcFPRegData[3U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [3U];
    vlSelfRef.__PVT__srcFPRegData[4U] = vlSelfRef.__Vcellout__phyFPReg__rv
        [4U];
    __Vlvbound_h741f6621__0 = vlSelfRef.__PVT__srcFPRegData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataB[0U] 
        = __Vlvbound_h741f6621__0;
    __Vlvbound_hbc238481__0 = vlSelfRef.__PVT__srcFPRegData
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataC[0U] 
        = __Vlvbound_hbc238481__0;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___nba_sequent__TOP__SMT_RTL_Testbench__core__registerFile__2(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___nba_sequent__TOP__SMT_RTL_Testbench__core__registerFile__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk6__DOT__i = 5U;
        vlSelfRef.__PVT__unnamedblk10__DOT__i = 2U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RegisterFile___nba_comb__TOP__SMT_RTL_Testbench__core__registerFile__0(VSMT_RTL_Testbench_RegisterFile* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RegisterFile___nba_comb__TOP__SMT_RTL_Testbench__core__registerFile__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vlvbound_h7f3dc019__0;
    __Vlvbound_h7f3dc019__0 = 0;
    CData/*6:0*/ __Vlvbound_h63ac04e7__0;
    __Vlvbound_h63ac04e7__0 = 0;
    QData/*32:0*/ __Vlvbound_h66422b62__0;
    __Vlvbound_h66422b62__0 = 0;
    CData/*0:0*/ __Vlvbound_h6c2b73f0__0;
    __Vlvbound_h6c2b73f0__0 = 0;
    CData/*6:0*/ __Vlvbound_h3802c8b6__0;
    __Vlvbound_h3802c8b6__0 = 0;
    QData/*32:0*/ __Vlvbound_hd2d68753__0;
    __Vlvbound_hd2d68753__0 = 0;
    CData/*0:0*/ __Vlvbound_h3cb25d40__0;
    __Vlvbound_h3cb25d40__0 = 0;
    CData/*6:0*/ __Vlvbound_h4877a006__0;
    __Vlvbound_h4877a006__0 = 0;
    QData/*32:0*/ __Vlvbound_h031d8dc3__0;
    __Vlvbound_h031d8dc3__0 = 0;
    CData/*0:0*/ __Vlvbound_h6356f7e7__0;
    __Vlvbound_h6356f7e7__0 = 0;
    CData/*6:0*/ __Vlvbound_h7fb74c9d__0;
    __Vlvbound_h7fb74c9d__0 = 0;
    QData/*32:0*/ __Vlvbound_hca3b039c__0;
    __Vlvbound_hca3b039c__0 = 0;
    // Body
    vlSelfRef.__PVT__dstFPRegData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegData
        [0U];
    vlSelfRef.__PVT__dstFPRegData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegData
        [0U];
    __Vlvbound_h66422b62__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[0U] = __Vlvbound_h66422b62__0;
    __Vlvbound_h66422b62__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData
        [1U];
    vlSelfRef.__PVT__dstRegData[1U] = __Vlvbound_h66422b62__0;
    __Vlvbound_hd2d68753__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[2U] = __Vlvbound_hd2d68753__0;
    __Vlvbound_h031d8dc3__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[3U] = __Vlvbound_h031d8dc3__0;
    __Vlvbound_hca3b039c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegData
        [0U];
    vlSelfRef.__PVT__dstRegData[4U] = __Vlvbound_hca3b039c__0;
    vlSelfRef.__PVT__fpRegWE[0U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegWE
                                    [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                                            [0U] >> 6U));
    vlSelfRef.__PVT__fpRegWE[1U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegWE
                                    [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                            [0U] >> 6U));
    vlSelfRef.__PVT__dstFPRegNum[0U] = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstTid
                                                               [0U], 6U) 
                                                 + 
                                                 (0x3fU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                                                  [0U])));
    vlSelfRef.__PVT__dstFPRegNum[1U] = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstTid
                                                               [0U], 6U) 
                                                 + 
                                                 (0x3fU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                                  [0U])));
    __Vlvbound_h7f3dc019__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[0U] = __Vlvbound_h7f3dc019__0;
    __Vlvbound_h7f3dc019__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE
                               [1U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                          [1U] >> 6U)));
    vlSelfRef.__PVT__regWE[1U] = __Vlvbound_h7f3dc019__0;
    __Vlvbound_h6c2b73f0__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[2U] = __Vlvbound_h6c2b73f0__0;
    __Vlvbound_h3cb25d40__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[3U] = __Vlvbound_h3cb25d40__0;
    __Vlvbound_h6356f7e7__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegWE
                               [0U] & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                                          [0U] >> 6U)));
    vlSelfRef.__PVT__regWE[4U] = __Vlvbound_h6356f7e7__0;
    __Vlvbound_h63ac04e7__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstTid
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                           [0U])));
    vlSelfRef.__PVT__dstRegNum[0U] = __Vlvbound_h63ac04e7__0;
    __Vlvbound_h63ac04e7__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstTid
                                                      [1U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum
                                           [1U])));
    vlSelfRef.__PVT__dstRegNum[1U] = __Vlvbound_h63ac04e7__0;
    __Vlvbound_h3802c8b6__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstTid
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegNum
                                           [0U])));
    vlSelfRef.__PVT__dstRegNum[2U] = __Vlvbound_h3802c8b6__0;
    __Vlvbound_h4877a006__0 = (0x7fU & (VL_SHIFTL_III(7,32,32, 
                                                      vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstTid
                                                      [0U], 6U) 
                                        + (0x3fU & 
                                           vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum
                                           [0U])));
    vlSelfRef.__PVT__dstRegNum[3U] = __Vlvbound_h4877a006__0;
    __Vlvbound_h7fb74c9d__0 = (0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum
                               [0U]);
    vlSelfRef.__PVT__dstRegNum[4U] = __Vlvbound_h7fb74c9d__0;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__dstFPRegData[0U] = (0x100000000ULL 
                                             & vlSelfRef.__PVT__dstFPRegData
                                             [0U]);
        vlSelfRef.__PVT__dstFPRegData[0U] = (0x100000000ULL 
                                             | vlSelfRef.__PVT__dstFPRegData
                                             [0U]);
        vlSelfRef.__PVT__dstFPRegData[1U] = (0x100000000ULL 
                                             & vlSelfRef.__PVT__dstFPRegData
                                             [1U]);
        vlSelfRef.__PVT__dstFPRegData[1U] = (0x100000000ULL 
                                             | vlSelfRef.__PVT__dstFPRegData
                                             [1U]);
        vlSelfRef.__PVT__dstRegData[0U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [0U]);
        vlSelfRef.__PVT__dstRegData[0U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [0U]);
        vlSelfRef.__PVT__dstRegData[1U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [1U]);
        vlSelfRef.__PVT__dstRegData[1U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [1U]);
        vlSelfRef.__PVT__dstRegData[2U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [2U]);
        vlSelfRef.__PVT__dstRegData[2U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [2U]);
        vlSelfRef.__PVT__dstRegData[3U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [3U]);
        vlSelfRef.__PVT__dstRegData[3U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [3U]);
        vlSelfRef.__PVT__dstRegData[4U] = (0x100000000ULL 
                                           & vlSelfRef.__PVT__dstRegData
                                           [4U]);
        vlSelfRef.__PVT__dstRegData[4U] = (0x100000000ULL 
                                           | vlSelfRef.__PVT__dstRegData
                                           [4U]);
        vlSelfRef.__PVT__fpRegWE[0U] = 1U;
        vlSelfRef.__PVT__fpRegWE[1U] = 1U;
        vlSelfRef.__PVT__dstFPRegNum[0U] = vlSelfRef.__PVT__fpRstIndex;
        vlSelfRef.__PVT__dstFPRegNum[1U] = (0x7fU & 
                                            ((IData)(1U) 
                                             + (IData)(vlSelfRef.__PVT__fpRstIndex)));
        vlSelfRef.__PVT__regWE[0U] = 1U;
        vlSelfRef.__PVT__regWE[1U] = 1U;
        vlSelfRef.__PVT__regWE[2U] = 1U;
        vlSelfRef.__PVT__regWE[3U] = 1U;
        vlSelfRef.__PVT__regWE[4U] = 1U;
        vlSelfRef.__Vlvbound_h8b146626__0 = vlSelfRef.__PVT__regRstIndex;
        vlSelfRef.__PVT__dstRegNum[0U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(1U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[1U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(2U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[2U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(3U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[3U] = vlSelfRef.__Vlvbound_h8b146626__0;
        vlSelfRef.__Vlvbound_h8b146626__0 = (0x7fU 
                                             & ((IData)(4U) 
                                                + (IData)(vlSelfRef.__PVT__regRstIndex)));
        vlSelfRef.__PVT__dstRegNum[4U] = vlSelfRef.__Vlvbound_h8b146626__0;
    }
    vlSelfRef.__Vcellinp__phyFPReg__wv[0U] = vlSelfRef.__PVT__dstFPRegData
        [0U];
    vlSelfRef.__Vcellinp__phyFPReg__wv[1U] = vlSelfRef.__PVT__dstFPRegData
        [1U];
    vlSelfRef.__Vcellinp__phyReg__wv[0U] = vlSelfRef.__PVT__dstRegData
        [0U];
    vlSelfRef.__Vcellinp__phyReg__wv[1U] = vlSelfRef.__PVT__dstRegData
        [1U];
    vlSelfRef.__Vcellinp__phyReg__wv[2U] = vlSelfRef.__PVT__dstRegData
        [2U];
    vlSelfRef.__Vcellinp__phyReg__wv[3U] = vlSelfRef.__PVT__dstRegData
        [3U];
    vlSelfRef.__Vcellinp__phyReg__wv[4U] = vlSelfRef.__PVT__dstRegData
        [4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__we[0U] 
        = vlSelfRef.__PVT__fpRegWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__we[1U] 
        = vlSelfRef.__PVT__fpRegWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wa[0U] 
        = vlSelfRef.__PVT__dstFPRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wa[1U] 
        = vlSelfRef.__PVT__dstFPRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[0U] 
        = vlSelfRef.__PVT__regWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[1U] 
        = vlSelfRef.__PVT__regWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[2U] 
        = vlSelfRef.__PVT__regWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[3U] 
        = vlSelfRef.__PVT__regWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__we[4U] 
        = vlSelfRef.__PVT__regWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[0U] 
        = vlSelfRef.__PVT__dstRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[1U] 
        = vlSelfRef.__PVT__dstRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[2U] 
        = vlSelfRef.__PVT__dstRegNum[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[3U] 
        = vlSelfRef.__PVT__dstRegNum[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wa[4U] 
        = vlSelfRef.__PVT__dstRegNum[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__phyFPReg__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__phyFPReg__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[2U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[3U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.__PVT__wv[4U] 
        = vlSelfRef.__Vcellinp__phyReg__wv[4U];
}
