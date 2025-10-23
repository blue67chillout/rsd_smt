// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RenameLogic.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_RenameLogic___stl_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__0(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___stl_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__scalarFreeListCount = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__queuePointer__DOT__regCount;
    vlSelfRef.__PVT__releasedPhyScalarFPRegNum[0U] 
        = (0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
           [0U]);
    vlSelfRef.__PVT__releasedPhyScalarFPRegNum[1U] 
        = (0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
           [1U]);
    vlSelfRef.__PVT__releasePhyScalarFPReg[0U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg
                                                  [0U] 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                                     [0U] 
                                                     >> 6U));
    vlSelfRef.__PVT__releasePhyScalarFPReg[1U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg
                                                  [1U] 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                                     [1U] 
                                                     >> 6U));
    vlSelfRef.__PVT__releasedPhyScalarRegNum[0U] = 
        (0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
         [0U]);
    vlSelfRef.__PVT__releasedPhyScalarRegNum[1U] = 
        (0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
         [1U]);
    vlSelfRef.__PVT__releasePhyScalarReg[0U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg
                                                [0U] 
                                                & (~ 
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                                    [0U] 
                                                    >> 6U)));
    vlSelfRef.__PVT__releasePhyScalarReg[1U] = (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg
                                                [1U] 
                                                & (~ 
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                                    [1U] 
                                                    >> 6U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__pushedData[0U] 
        = vlSelfRef.__PVT__releasedPhyScalarFPRegNum
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__pushedData[1U] 
        = vlSelfRef.__PVT__releasedPhyScalarFPRegNum
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__push[0U] 
        = vlSelfRef.__PVT__releasePhyScalarFPReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__push[1U] 
        = vlSelfRef.__PVT__releasePhyScalarFPReg[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__pushedData[0U] 
        = vlSelfRef.__PVT__releasedPhyScalarRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__pushedData[1U] 
        = vlSelfRef.__PVT__releasedPhyScalarRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__pushedData[0U] 
        = vlSelfRef.__PVT__releasedPhyScalarRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__pushedData[1U] 
        = vlSelfRef.__PVT__releasedPhyScalarRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__push[0U] 
        = vlSelfRef.__PVT__releasePhyScalarReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__push[1U] 
        = vlSelfRef.__PVT__releasePhyScalarReg[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__push[0U] 
        = vlSelfRef.__PVT__releasePhyScalarReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__push[1U] 
        = vlSelfRef.__PVT__releasePhyScalarReg[1U];
}
