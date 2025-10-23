// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_RMT.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_RMT___stl_sequent__TOP__SMT_RTL_Testbench__core__rmt_wat__0(VSMT_RTL_Testbench_RMT* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_RMT___stl_sequent__TOP__SMT_RTL_Testbench__core__rmt_wat__0\n"); );
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
}
