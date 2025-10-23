// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RMT.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_RMT___act_sequent__TOP__SMT_RTL_Testbench__core__rmt_wat__0(VSMT_RTL_Testbench_RMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RMT___act_sequent__TOP__SMT_RTL_Testbench__core__rmt_wat__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
        vlSelfRef.__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 1U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RMT___act_comb__TOP__SMT_RTL_Testbench__core__rmt_wat__0(VSMT_RTL_Testbench_RMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RMT___act_comb__TOP__SMT_RTL_Testbench__core__rmt_wat__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rmtRA[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
        [0U];
    vlSelfRef.__PVT__rmtRA[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
        [0U];
    vlSelfRef.__PVT__rmtRA[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
        [0U];
    vlSelfRef.__PVT__rmtRA[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
        [0U];
    vlSelfRef.__PVT__rmtRA[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
        [1U];
    vlSelfRef.__PVT__rmtRA[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
        [1U];
    vlSelfRef.__PVT__rmtRA[6U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
        [1U];
    vlSelfRef.__PVT__rmtRA[7U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
        [1U];
    vlSelfRef.__Vcellinp__regRMT__ra[0U] = vlSelfRef.__PVT__rmtRA
        [0U];
    vlSelfRef.__Vcellinp__regRMT__ra[1U] = vlSelfRef.__PVT__rmtRA
        [1U];
    vlSelfRef.__Vcellinp__regRMT__ra[2U] = vlSelfRef.__PVT__rmtRA
        [2U];
    vlSelfRef.__Vcellinp__regRMT__ra[3U] = vlSelfRef.__PVT__rmtRA
        [3U];
    vlSelfRef.__Vcellinp__regRMT__ra[4U] = vlSelfRef.__PVT__rmtRA
        [4U];
    vlSelfRef.__Vcellinp__regRMT__ra[5U] = vlSelfRef.__PVT__rmtRA
        [5U];
    vlSelfRef.__Vcellinp__regRMT__ra[6U] = vlSelfRef.__PVT__rmtRA
        [6U];
    vlSelfRef.__Vcellinp__regRMT__ra[7U] = vlSelfRef.__PVT__rmtRA
        [7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[0U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[1U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[2U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[3U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[4U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[5U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[6U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[7U] 
        = vlSelfRef.__Vcellinp__regRMT__ra[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RMT___act_comb__TOP__SMT_RTL_Testbench__core__rmt_wat__1(VSMT_RTL_Testbench_RMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RMT___act_comb__TOP__SMT_RTL_Testbench__core__rmt_wat__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__regRMT__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__regRMT__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [1U];
    vlSelfRef.__Vcellout__regRMT__rv[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [2U];
    vlSelfRef.__Vcellout__regRMT__rv[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [3U];
    vlSelfRef.__Vcellout__regRMT__rv[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [4U];
    vlSelfRef.__Vcellout__regRMT__rv[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [5U];
    vlSelfRef.__Vcellout__regRMT__rv[6U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [6U];
    vlSelfRef.__Vcellout__regRMT__rv[7U] = vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv
        [7U];
    vlSelfRef.__PVT__rmtRV[0U] = vlSelfRef.__Vcellout__regRMT__rv
        [0U];
    vlSelfRef.__PVT__rmtRV[1U] = vlSelfRef.__Vcellout__regRMT__rv
        [1U];
    vlSelfRef.__PVT__rmtRV[2U] = vlSelfRef.__Vcellout__regRMT__rv
        [2U];
    vlSelfRef.__PVT__rmtRV[3U] = vlSelfRef.__Vcellout__regRMT__rv
        [3U];
    vlSelfRef.__PVT__rmtRV[4U] = vlSelfRef.__Vcellout__regRMT__rv
        [4U];
    vlSelfRef.__PVT__rmtRV[5U] = vlSelfRef.__Vcellout__regRMT__rv
        [5U];
    vlSelfRef.__PVT__rmtRV[6U] = vlSelfRef.__Vcellout__regRMT__rv
        [6U];
    vlSelfRef.__PVT__rmtRV[7U] = vlSelfRef.__Vcellout__regRMT__rv
        [7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RMT___act_comb__TOP__SMT_RTL_Testbench__core__rmt_wat__2(VSMT_RTL_Testbench_RMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RMT___act_comb__TOP__SMT_RTL_Testbench__core__rmt_wat__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__rmtWV[0U] = ((0xfU & vlSelfRef.__PVT__rmtWV
                                       [0U]) | (vlSelfRef.__PVT__rstWritePhyRegNum
                                                [0U] 
                                                << 4U));
        vlSelfRef.__PVT__rmtWV[0U] = (0x3f0U & vlSelfRef.__PVT__rmtWV
                                      [0U]);
        vlSelfRef.__PVT__rmtWV[1U] = ((0xfU & vlSelfRef.__PVT__rmtWV
                                       [1U]) | (vlSelfRef.__PVT__rstWritePhyRegNum
                                                [1U] 
                                                << 4U));
        vlSelfRef.__PVT__rmtWV[1U] = (0x3f0U & vlSelfRef.__PVT__rmtWV
                                      [1U]);
        vlSelfRef.__PVT__rmtWE[0U] = 1U;
        vlSelfRef.__PVT__rmtWA[0U] = vlSelfRef.__PVT__rstWriteLogRegNum
            [0U];
        vlSelfRef.__PVT__rmtWE[1U] = 0U;
        vlSelfRef.__PVT__rmtWA[1U] = vlSelfRef.__PVT__rstWriteLogRegNum
            [1U];
    } else {
        vlSelfRef.__PVT__rmtWV[0U] = ((0xfU & vlSelfRef.__PVT__rmtWV
                                       [0U]) | (0x3f0U 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                                   [0U] 
                                                   << 4U)));
        vlSelfRef.__PVT__rmtWV[0U] = ((0x3f0U & vlSelfRef.__PVT__rmtWV
                                       [0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr
                                      [0U]);
        vlSelfRef.__PVT__rmtWV[1U] = ((0xfU & vlSelfRef.__PVT__rmtWV
                                       [1U]) | (0x3f0U 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                                   [1U] 
                                                   << 4U)));
        vlSelfRef.__PVT__rmtWV[1U] = ((0x3f0U & vlSelfRef.__PVT__rmtWV
                                       [1U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr
                                      [1U]);
        vlSelfRef.__PVT__rmtWE[0U] = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg));
        vlSelfRef.__PVT__rmtWA[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum
            [0U];
        vlSelfRef.__PVT__rmtWE[1U] = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg) 
                                            >> 1U));
        vlSelfRef.__PVT__rmtWA[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum
            [1U];
        if ((vlSelfRef.__PVT__rmtWE[1U] & (vlSelfRef.__PVT__rmtWA
                                           [1U] == 
                                           vlSelfRef.__PVT__rmtWA
                                           [0U]))) {
            vlSelfRef.__PVT__rmtWE[0U] = 0U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__prevDependIssueQueuePtr[0U] 
        = (0xfU & vlSelfRef.__PVT__rmtRV[2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__prevDependIssueQueuePtr[1U] 
        = (0xfU & vlSelfRef.__PVT__rmtRV[6U]);
    vlSelfRef.__PVT__phyPrevDstReg[0U] = ((0x3fU & 
                                           vlSelfRef.__PVT__phyPrevDstReg
                                           [0U]) | 
                                          (0x40U & 
                                           (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                            [0U] << 1U)));
    vlSelfRef.__PVT__phyPrevDstReg[0U] = ((0x40U & 
                                           vlSelfRef.__PVT__phyPrevDstReg
                                           [0U]) | 
                                          (0x3fU & 
                                           (vlSelfRef.__PVT__rmtRV
                                            [2U] >> 4U)));
    vlSelfRef.__PVT__phyPrevDstReg[1U] = ((0x3fU & 
                                           vlSelfRef.__PVT__phyPrevDstReg
                                           [1U]) | 
                                          (0x40U & 
                                           (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                            [1U] << 1U)));
    vlSelfRef.__PVT__phyPrevDstReg[1U] = ((0x40U & 
                                           vlSelfRef.__PVT__phyPrevDstReg
                                           [1U]) | 
                                          (0x3fU & 
                                           (vlSelfRef.__PVT__rmtRV
                                            [6U] >> 4U)));
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg))) {
        if ((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [1U] == vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [0U])) {
            vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__prevDependIssueQueuePtr[1U] 
                = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr
                [0U];
            vlSelfRef.__PVT__phyPrevDstReg[1U] = ((0x40U 
                                                   & vlSelfRef.__PVT__phyPrevDstReg
                                                   [1U]) 
                                                  | (0x3fU 
                                                     & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                                     [0U]));
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg[0U] 
        = vlSelfRef.__PVT__phyPrevDstReg[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg[1U] 
        = vlSelfRef.__PVT__phyPrevDstReg[1U];
    vlSelfRef.__PVT__phySrcRegB[0U] = ((0x3fU & vlSelfRef.__PVT__phySrcRegB
                                        [0U]) | (0x40U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
                                                    [0U] 
                                                    << 1U)));
    vlSelfRef.__PVT__phySrcRegB[0U] = ((0x40U & vlSelfRef.__PVT__phySrcRegB
                                        [0U]) | (0x3fU 
                                                 & (vlSelfRef.__PVT__rmtRV
                                                    [1U] 
                                                    >> 4U)));
    vlSelfRef.__PVT__phySrcRegB[1U] = ((0x3fU & vlSelfRef.__PVT__phySrcRegB
                                        [1U]) | (0x40U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
                                                    [1U] 
                                                    << 1U)));
    vlSelfRef.__PVT__phySrcRegB[1U] = ((0x40U & vlSelfRef.__PVT__phySrcRegB
                                        [1U]) | (0x3fU 
                                                 & (vlSelfRef.__PVT__rmtRV
                                                    [5U] 
                                                    >> 4U)));
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg))) {
        if ((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
             [1U] == vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [0U])) {
            vlSelfRef.__PVT__phySrcRegB[1U] = ((0x40U 
                                                & vlSelfRef.__PVT__phySrcRegB
                                                [1U]) 
                                               | (0x3fU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                                  [0U]));
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB[0U] 
        = vlSelfRef.__PVT__phySrcRegB[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB[1U] 
        = vlSelfRef.__PVT__phySrcRegB[1U];
    vlSelfRef.__PVT__srcIssueQueuePtrRegC[0U] = (0xfU 
                                                 & vlSelfRef.__PVT__rmtRV
                                                 [3U]);
    vlSelfRef.__PVT__srcIssueQueuePtrRegC[1U] = (0xfU 
                                                 & vlSelfRef.__PVT__rmtRV
                                                 [7U]);
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg))) {
        if ((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
             [1U] == vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [0U])) {
            vlSelfRef.__PVT__srcIssueQueuePtrRegC[1U] 
                = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr
                [0U];
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegC[0U] 
        = vlSelfRef.__PVT__srcIssueQueuePtrRegC[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegC[1U] 
        = vlSelfRef.__PVT__srcIssueQueuePtrRegC[1U];
    vlSelfRef.__PVT__phySrcRegA[0U] = ((0x3fU & vlSelfRef.__PVT__phySrcRegA
                                        [0U]) | (0x40U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
                                                    [0U] 
                                                    << 1U)));
    vlSelfRef.__PVT__phySrcRegA[0U] = ((0x40U & vlSelfRef.__PVT__phySrcRegA
                                        [0U]) | (0x3fU 
                                                 & (vlSelfRef.__PVT__rmtRV
                                                    [0U] 
                                                    >> 4U)));
    vlSelfRef.__PVT__phySrcRegA[1U] = ((0x3fU & vlSelfRef.__PVT__phySrcRegA
                                        [1U]) | (0x40U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
                                                    [1U] 
                                                    << 1U)));
    vlSelfRef.__PVT__phySrcRegA[1U] = ((0x40U & vlSelfRef.__PVT__phySrcRegA
                                        [1U]) | (0x3fU 
                                                 & (vlSelfRef.__PVT__rmtRV
                                                    [4U] 
                                                    >> 4U)));
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg))) {
        if ((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
             [1U] == vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [0U])) {
            vlSelfRef.__PVT__phySrcRegA[1U] = ((0x40U 
                                                & vlSelfRef.__PVT__phySrcRegA
                                                [1U]) 
                                               | (0x3fU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                                  [0U]));
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA[0U] 
        = vlSelfRef.__PVT__phySrcRegA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA[1U] 
        = vlSelfRef.__PVT__phySrcRegA[1U];
    vlSelfRef.__PVT__phySrcRegC[0U] = ((0x3fU & vlSelfRef.__PVT__phySrcRegC
                                        [0U]) | (0x40U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                                    [0U] 
                                                    << 1U)));
    vlSelfRef.__PVT__phySrcRegC[0U] = ((0x40U & vlSelfRef.__PVT__phySrcRegC
                                        [0U]) | (0x3fU 
                                                 & (vlSelfRef.__PVT__rmtRV
                                                    [3U] 
                                                    >> 4U)));
    vlSelfRef.__PVT__phySrcRegC[1U] = ((0x3fU & vlSelfRef.__PVT__phySrcRegC
                                        [1U]) | (0x40U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                                    [1U] 
                                                    << 1U)));
    vlSelfRef.__PVT__phySrcRegC[1U] = ((0x40U & vlSelfRef.__PVT__phySrcRegC
                                        [1U]) | (0x3fU 
                                                 & (vlSelfRef.__PVT__rmtRV
                                                    [7U] 
                                                    >> 4U)));
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg))) {
        if ((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
             [1U] == vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [0U])) {
            vlSelfRef.__PVT__phySrcRegC[1U] = ((0x40U 
                                                & vlSelfRef.__PVT__phySrcRegC
                                                [1U]) 
                                               | (0x3fU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                                  [0U]));
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC[0U] 
        = vlSelfRef.__PVT__phySrcRegC[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC[1U] 
        = vlSelfRef.__PVT__phySrcRegC[1U];
    vlSelfRef.__PVT__srcIssueQueuePtrRegA[0U] = (0xfU 
                                                 & vlSelfRef.__PVT__rmtRV
                                                 [0U]);
    vlSelfRef.__PVT__srcIssueQueuePtrRegA[1U] = (0xfU 
                                                 & vlSelfRef.__PVT__rmtRV
                                                 [4U]);
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg))) {
        if ((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
             [1U] == vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [0U])) {
            vlSelfRef.__PVT__srcIssueQueuePtrRegA[1U] 
                = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr
                [0U];
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegA[0U] 
        = vlSelfRef.__PVT__srcIssueQueuePtrRegA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegA[1U] 
        = vlSelfRef.__PVT__srcIssueQueuePtrRegA[1U];
    vlSelfRef.__PVT__srcIssueQueuePtrRegB[0U] = (0xfU 
                                                 & vlSelfRef.__PVT__rmtRV
                                                 [1U]);
    vlSelfRef.__PVT__srcIssueQueuePtrRegB[1U] = (0xfU 
                                                 & vlSelfRef.__PVT__rmtRV
                                                 [5U]);
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg))) {
        if ((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
             [1U] == vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
             [0U])) {
            vlSelfRef.__PVT__srcIssueQueuePtrRegB[1U] 
                = vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr
                [0U];
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB[0U] 
        = vlSelfRef.__PVT__srcIssueQueuePtrRegB[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB[1U] 
        = vlSelfRef.__PVT__srcIssueQueuePtrRegB[1U];
    vlSelfRef.__Vcellinp__regRMT__wv[0U] = vlSelfRef.__PVT__rmtWV
        [0U];
    vlSelfRef.__Vcellinp__regRMT__wv[1U] = vlSelfRef.__PVT__rmtWV
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__we[0U] 
        = vlSelfRef.__PVT__rmtWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__we[1U] 
        = vlSelfRef.__PVT__rmtWE[1U];
    vlSelfRef.__Vcellinp__regRMT__wa[0U] = vlSelfRef.__PVT__rmtWA
        [0U];
    vlSelfRef.__Vcellinp__regRMT__wa[1U] = vlSelfRef.__PVT__rmtWA
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__regRMT__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__regRMT__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wa[0U] 
        = vlSelfRef.__Vcellinp__regRMT__wa[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wa[1U] 
        = vlSelfRef.__Vcellinp__regRMT__wa[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_RMT___nba_sequent__TOP__SMT_RTL_Testbench__core__rmt_wat__0(VSMT_RTL_Testbench_RMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RMT___nba_sequent__TOP__SMT_RTL_Testbench__core__rmt_wat__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*5:0*/ __VdlyVal__rstWriteLogRegNum__v0;
    __VdlyVal__rstWriteLogRegNum__v0 = 0;
    CData/*5:0*/ __VdlyVal__rstWriteLogRegNum__v1;
    __VdlyVal__rstWriteLogRegNum__v1 = 0;
    // Body
    vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
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
