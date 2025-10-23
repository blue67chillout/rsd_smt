// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RenameLogic.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_RenameLogic___eval_initial__TOP__SMT_RTL_Testbench__core__renameLogic(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___eval_initial__TOP__SMT_RTL_Testbench__core__renameLogic\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__retRMT_ReadReg_LogRegNum[0U] = 0U;
    vlSelfRef.__PVT__retRMT_ReadReg_LogRegNum[1U] = 0U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_LogRegNum[0U] 
        = vlSelfRef.__PVT__retRMT_ReadReg_LogRegNum
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_LogRegNum[1U] 
        = vlSelfRef.__PVT__retRMT_ReadReg_LogRegNum
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__0(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT))) {
        vlSelfRef.__PVT__unnamedblk6__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk7__DOT__i = 2U;
    }
    vlSelfRef.__PVT__allocatePhyReg[0U] = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT) 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg
                                           [0U]);
    vlSelfRef.__PVT__allocatePhyScalarReg[0U] = (vlSelfRef.__PVT__allocatePhyReg
                                                 [0U] 
                                                 & (~ 
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                     [0U] 
                                                     >> 5U)));
    vlSelfRef.__PVT__allocatePhyScalarFPReg[0U] = (
                                                   vlSelfRef.__PVT__allocatePhyReg
                                                   [0U] 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                      [0U] 
                                                      >> 5U));
    vlSelfRef.__PVT__allocatePhyReg[1U] = (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT) 
                                            >> 1U) 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg
                                           [1U]);
    vlSelfRef.__PVT__allocatePhyScalarReg[1U] = (vlSelfRef.__PVT__allocatePhyReg
                                                 [1U] 
                                                 & (~ 
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                     [1U] 
                                                     >> 5U)));
    vlSelfRef.__PVT__allocatePhyScalarFPReg[1U] = (
                                                   vlSelfRef.__PVT__allocatePhyReg
                                                   [1U] 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                      [1U] 
                                                      >> 5U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__pop[0U] 
        = vlSelfRef.__PVT__allocatePhyScalarFPReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__pop[1U] 
        = vlSelfRef.__PVT__allocatePhyScalarFPReg[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__pop[0U] 
        = vlSelfRef.__PVT__allocatePhyScalarReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__pop[1U] 
        = vlSelfRef.__PVT__allocatePhyScalarReg[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__pop[0U] 
        = vlSelfRef.__PVT__allocatePhyScalarReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__pop[1U] 
        = vlSelfRef.__PVT__allocatePhyScalarReg[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__1(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__allocatedPhyScalarFPRegNum[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__poppedData
        [0U];
    vlSelfRef.__PVT__allocatedPhyScalarFPRegNum[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__poppedData
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__2(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__allocatedPhyScalarRegNum[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__poppedData
        [0U];
    vlSelfRef.__PVT__allocatedPhyScalarRegNum[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__poppedData
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__3(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__allocatedPhyScalarRegNum[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__poppedData
        [0U];
    vlSelfRef.__PVT__allocatedPhyScalarRegNum[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__poppedData
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__4(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___act_comb__TOP__SMT_RTL_Testbench__core__renameLogic__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rmtWriteReg = (2U & (IData)(vlSelfRef.__PVT__rmtWriteReg));
    vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[0U] = 0U;
    vlSelfRef.__PVT__rmtWriteReg_LogRegNum[0U] = 0U;
    vlSelfRef.__PVT__rmtWriteReg = (1U & (IData)(vlSelfRef.__PVT__rmtWriteReg));
    vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[1U] = 0U;
    vlSelfRef.__PVT__rmtWriteReg_LogRegNum[1U] = 0U;
    vlSelfRef.__PVT__allocatedPhyRegNum[0U] = ((0x3fU 
                                                & vlSelfRef.__PVT__allocatedPhyRegNum
                                                [0U]) 
                                               | (0x40U 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                     [0U] 
                                                     << 1U)));
    vlSelfRef.__PVT__allocatedPhyRegNum[0U] = ((0x40U 
                                                & vlSelfRef.__PVT__allocatedPhyRegNum
                                                [0U]) 
                                               | (0x3fU 
                                                  & ((0x20U 
                                                      & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                      [0U])
                                                      ? 
                                                     vlSelfRef.__PVT__allocatedPhyScalarFPRegNum
                                                     [0U]
                                                      : 
                                                     vlSelfRef.__PVT__allocatedPhyScalarRegNum
                                                     [0U])));
    vlSelfRef.__PVT__allocatedPhyRegNum[1U] = ((0x3fU 
                                                & vlSelfRef.__PVT__allocatedPhyRegNum
                                                [1U]) 
                                               | (0x40U 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                     [1U] 
                                                     << 1U)));
    vlSelfRef.__PVT__allocatedPhyRegNum[1U] = ((0x40U 
                                                & vlSelfRef.__PVT__allocatedPhyRegNum
                                                [1U]) 
                                               | (0x3fU 
                                                  & ((0x20U 
                                                      & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                                      [1U])
                                                      ? 
                                                     vlSelfRef.__PVT__allocatedPhyScalarFPRegNum
                                                     [1U]
                                                      : 
                                                     vlSelfRef.__PVT__allocatedPhyScalarRegNum
                                                     [1U])));
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg[0U] 
        = vlSelfRef.__PVT__allocatedPhyRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg[1U] 
        = vlSelfRef.__PVT__allocatedPhyRegNum[1U];
    vlSelfRef.__PVT__inRecoveryRMT = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__inRecoveryAL;
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__renameLogicRecoveryRMT 
        = vlSelfRef.__PVT__inRecoveryRMT;
    vlSelfRef.__PVT__alReadData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
        [0U];
    vlSelfRef.__PVT__alReadData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__allocatable 
        = (((~ (IData)(vlSelfRef.__PVT__inRecoveryRMT)) 
            & (2U <= (IData)(vlSelfRef.__PVT__scalarFreeListCount))) 
           & (2U <= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__queuePointer__DOT__regCount)));
    if ((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT))) {
        vlSelfRef.__PVT__rmtWriteReg = ((2U & (IData)(vlSelfRef.__PVT__rmtWriteReg)) 
                                        | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT) 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg
                                           [0U]));
        vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[0U] 
            = vlSelfRef.__PVT__allocatedPhyRegNum[0U];
        vlSelfRef.__PVT__rmtWriteReg_LogRegNum[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
            [0U];
        vlSelfRef.__PVT__rmtWriteReg = ((1U & (IData)(vlSelfRef.__PVT__rmtWriteReg)) 
                                        | (0xfffffffeU 
                                           & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT) 
                                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg
                                                 [1U] 
                                                 << 1U))));
        vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[1U] 
            = vlSelfRef.__PVT__allocatedPhyRegNum[1U];
        vlSelfRef.__PVT__rmtWriteReg_LogRegNum[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
            [1U];
    } else if (vlSelfRef.__PVT__inRecoveryRMT) {
        vlSelfRef.__PVT__rmtWriteReg = ((2U & (IData)(vlSelfRef.__PVT__rmtWriteReg)) 
                                        | (0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)));
        vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[0U] 
            = (0x7fU & (IData)((vlSelfRef.__PVT__alReadData
                                [0U] >> 4U)));
        vlSelfRef.__PVT__unnamedblk10__DOT__i = 2U;
        vlSelfRef.__PVT__rmtWriteReg_LogRegNum[0U] 
            = (0x3fU & (IData)((vlSelfRef.__PVT__alReadData
                                [0U] >> 0x19U)));
        vlSelfRef.__PVT__rmtWriteReg = ((1U & (IData)(vlSelfRef.__PVT__rmtWriteReg)) 
                                        | ((1U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)) 
                                           << 1U));
        vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[1U] 
            = (0x7fU & (IData)((vlSelfRef.__PVT__alReadData
                                [1U] >> 4U)));
        vlSelfRef.__PVT__rmtWriteReg_LogRegNum[1U] 
            = (0x3fU & (IData)((vlSelfRef.__PVT__alReadData
                                [1U] >> 0x19U)));
    }
    if (vlSelfRef.__PVT__inRecoveryRMT) {
        vlSelfRef.__PVT__watWriteReg[0U] = ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)) 
                                            & (IData)(
                                                      (vlSelfRef.__PVT__alReadData
                                                       [0U] 
                                                       >> 0x18U)));
        vlSelfRef.__PVT__unnamedblk13__DOT__i = 2U;
        vlSelfRef.__PVT__watWriteLogRegNum[0U] = (0x3fU 
                                                  & (IData)(
                                                            (vlSelfRef.__PVT__alReadData
                                                             [0U] 
                                                             >> 0x19U)));
        vlSelfRef.__PVT__watWriteIssueQueuePtr[0U] 
            = (0xfU & (IData)(vlSelfRef.__PVT__alReadData
                              [0U]));
        vlSelfRef.__PVT__watWriteReg[1U] = ((1U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)) 
                                            & (IData)(
                                                      (vlSelfRef.__PVT__alReadData
                                                       [1U] 
                                                       >> 0x18U)));
        vlSelfRef.__PVT__watWriteLogRegNum[1U] = (0x3fU 
                                                  & (IData)(
                                                            (vlSelfRef.__PVT__alReadData
                                                             [1U] 
                                                             >> 0x19U)));
        vlSelfRef.__PVT__watWriteIssueQueuePtr[1U] 
            = (0xfU & (IData)(vlSelfRef.__PVT__alReadData
                              [1U]));
    } else {
        vlSelfRef.__PVT__watWriteReg[0U] = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteRegFromPipeReg));
        vlSelfRef.__PVT__watWriteLogRegNum[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
            [0U];
        vlSelfRef.__PVT__watWriteIssueQueuePtr[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtrFromPipeReg
            [0U];
        vlSelfRef.__PVT__unnamedblk11__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk12__DOT__i = 2U;
        vlSelfRef.__PVT__watWriteReg[1U] = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteRegFromPipeReg) 
                                                  >> 1U));
        vlSelfRef.__PVT__watWriteLogRegNum[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
            [1U];
        vlSelfRef.__PVT__watWriteIssueQueuePtr[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtrFromPipeReg
            [1U];
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg 
        = vlSelfRef.__PVT__rmtWriteReg;
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum[0U] 
        = vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum[1U] 
        = vlSelfRef.__PVT__rmtWriteReg_PhyRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum[0U] 
        = vlSelfRef.__PVT__rmtWriteReg_LogRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum[1U] 
        = vlSelfRef.__PVT__rmtWriteReg_LogRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteReg[0U] 
        = vlSelfRef.__PVT__watWriteReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteReg[1U] 
        = vlSelfRef.__PVT__watWriteReg[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteLogRegNum[0U] 
        = vlSelfRef.__PVT__watWriteLogRegNum[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteLogRegNum[1U] 
        = vlSelfRef.__PVT__watWriteLogRegNum[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr[0U] 
        = vlSelfRef.__PVT__watWriteIssueQueuePtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr[1U] 
        = vlSelfRef.__PVT__watWriteIssueQueuePtr[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RenameLogic___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__0(VSMT_RTL_Testbench_RenameLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RenameLogic___nba_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
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
    vlSelfRef.__PVT__scalarFreeListCount = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__queuePointer__DOT__regCount;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__rmtRecoveryIndex = 0U;
        vlSelfRef.__PVT__rmtRecoveryCount = 0U;
    } else if ((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                             >> 0x15U)))) {
        vlSelfRef.__PVT__rmtRecoveryIndex = 0U;
        vlSelfRef.__PVT__rmtRecoveryCount = 0x40U;
    } else {
        vlSelfRef.__PVT__rmtRecoveryIndex = (0x3fU 
                                             & ((IData)(2U) 
                                                + (IData)(vlSelfRef.__PVT__rmtRecoveryIndex)));
        vlSelfRef.__PVT__rmtRecoveryCount = ((2U < (IData)(vlSelfRef.__PVT__rmtRecoveryCount))
                                              ? (0x7fU 
                                                 & ((IData)(vlSelfRef.__PVT__rmtRecoveryCount) 
                                                    - (IData)(2U)))
                                              : 0U);
    }
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
