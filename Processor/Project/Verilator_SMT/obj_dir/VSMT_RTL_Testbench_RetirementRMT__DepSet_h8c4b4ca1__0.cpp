// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RetirementRMT.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_RetirementRMT___act_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__0(VSMT_RTL_Testbench_RetirementRMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RetirementRMT___act_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
        vlSelfRef.__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 1U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RetirementRMT___act_comb__TOP__SMT_RTL_Testbench__core__retirementRMT__0(VSMT_RTL_Testbench_RetirementRMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RetirementRMT___act_comb__TOP__SMT_RTL_Testbench__core__retirementRMT__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__writePhyRegNum[0U] = (0x3fU 
                                               & vlSelfRef.__PVT__rstWritePhyRegNum
                                               [0U]);
        vlSelfRef.__PVT__writePhyRegNum[1U] = (0x3fU 
                                               & vlSelfRef.__PVT__rstWritePhyRegNum
                                               [1U]);
        vlSelfRef.__PVT__writeLogRegNum[0U] = vlSelfRef.__PVT__rstWriteLogRegNum
            [0U];
        vlSelfRef.__PVT__we[0U] = 1U;
        vlSelfRef.__PVT__writeLogRegNum[1U] = vlSelfRef.__PVT__rstWriteLogRegNum
            [1U];
        vlSelfRef.__PVT__we[1U] = 0U;
    } else {
        vlSelfRef.__PVT__writePhyRegNum[0U] = (0x3fU 
                                               & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum
                                               [0U]);
        vlSelfRef.__PVT__writePhyRegNum[1U] = (0x3fU 
                                               & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum
                                               [1U]);
        vlSelfRef.__PVT__writeLogRegNum[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum
            [0U];
        vlSelfRef.__PVT__we[0U] = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg));
        vlSelfRef.__PVT__writeLogRegNum[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum
            [1U];
        vlSelfRef.__PVT__we[1U] = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg) 
                                         >> 1U));
        if ((vlSelfRef.__PVT__we[1U] & (vlSelfRef.__PVT__writeLogRegNum
                                        [1U] == vlSelfRef.__PVT__writeLogRegNum
                                        [0U]))) {
            vlSelfRef.__PVT__we[0U] = 0U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wv[0U] 
        = vlSelfRef.__PVT__writePhyRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wv[1U] 
        = vlSelfRef.__PVT__writePhyRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__we[0U] 
        = vlSelfRef.__PVT__we[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__we[1U] 
        = vlSelfRef.__PVT__we[1U];
    vlSelfRef.__Vcellinp__regRMT__wa[0U] = vlSelfRef.__PVT__writeLogRegNum
        [0U];
    vlSelfRef.__Vcellinp__regRMT__wa[1U] = vlSelfRef.__PVT__writeLogRegNum
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wa[0U] 
        = vlSelfRef.__Vcellinp__regRMT__wa[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wa[1U] 
        = vlSelfRef.__Vcellinp__regRMT__wa[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RetirementRMT___nba_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__0(VSMT_RTL_Testbench_RetirementRMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RetirementRMT___nba_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*5:0*/ __VdlyVal__rstWriteLogRegNum__v0;
    __VdlyVal__rstWriteLogRegNum__v0 = 0;
    CData/*5:0*/ __VdlyVal__rstWriteLogRegNum__v1;
    __VdlyVal__rstWriteLogRegNum__v1 = 0;
    // Body
    vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
    vlSelfRef.__PVT__readPhyRegNum[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__rv
        [0U];
    vlSelfRef.__PVT__readPhyRegNum[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__rv
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart) {
        __VdlyVal__rstWriteLogRegNum__v0 = 0U;
        __VdlyVal__rstWriteLogRegNum__v1 = 0U;
    } else {
        __VdlyVal__rstWriteLogRegNum__v0 = (0x3fU & 
                                            ((IData)(1U) 
                                             + vlSelfRef.__PVT__rstWriteLogRegNum
                                             [0U]));
        __VdlyVal__rstWriteLogRegNum__v1 = (0x3fU & 
                                            ((IData)(1U) 
                                             + vlSelfRef.__PVT__rstWriteLogRegNum
                                             [1U]));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum[0U] 
        = ((0x40U & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
            [0U]) | vlSelfRef.__PVT__readPhyRegNum[0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum[0U] 
        = ((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
            [0U]) | (0x40U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_LogRegNum
                              [0U] << 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum[1U] 
        = ((0x40U & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
            [1U]) | vlSelfRef.__PVT__readPhyRegNum[1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum[1U] 
        = ((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
            [1U]) | (0x40U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_LogRegNum
                              [1U] << 1U)));
    vlSelfRef.__PVT__rstWriteLogRegNum[0U] = __VdlyVal__rstWriteLogRegNum__v0;
    vlSelfRef.__PVT__rstWriteLogRegNum[1U] = __VdlyVal__rstWriteLogRegNum__v1;
    vlSelfRef.__PVT__rstWritePhyRegNum[0U] = (0x3fU 
                                              & ((0x20U 
                                                  & vlSelfRef.__PVT__rstWriteLogRegNum
                                                  [0U])
                                                  ? 
                                                 ((IData)(0x20U) 
                                                  + 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__rstWriteLogRegNum
                                                   [0U]))
                                                  : 
                                                 ((IData)(0x20U) 
                                                  + 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__rstWriteLogRegNum
                                                   [0U]))));
    vlSelfRef.__PVT__rstWritePhyRegNum[1U] = (0x3fU 
                                              & ((0x20U 
                                                  & vlSelfRef.__PVT__rstWriteLogRegNum
                                                  [1U])
                                                  ? 
                                                 ((IData)(0x20U) 
                                                  + 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__rstWriteLogRegNum
                                                   [1U]))
                                                  : 
                                                 ((IData)(0x20U) 
                                                  + 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__rstWriteLogRegNum
                                                   [1U]))));
}
