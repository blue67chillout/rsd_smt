// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RetirementRMT.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_RetirementRMT___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__0(VSMT_RTL_Testbench_RetirementRMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RetirementRMT___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
        vlSelfRef.__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 1U;
    }
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
    vlSelfRef.__PVT__readLogRegNum[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_LogRegNum
        [0U];
    vlSelfRef.__PVT__readLogRegNum[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_LogRegNum
        [1U];
    vlSelfRef.__Vcellinp__regRMT__ra[0U] = vlSelfRef.__PVT__readLogRegNum
        [0U];
    vlSelfRef.__Vcellinp__regRMT__ra[1U] = vlSelfRef.__PVT__readLogRegNum
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__ra[0U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__ra[1U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[1U];
}

VL_ATTR_COLD void VSMT_RTL_Testbench_RetirementRMT___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__1(VSMT_RTL_Testbench_RetirementRMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RetirementRMT___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__readPhyRegNum[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__rv
        [0U];
    vlSelfRef.__PVT__readPhyRegNum[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__rv
        [1U];
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
}
