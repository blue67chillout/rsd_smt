// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__pi2.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_MultiWidthFreeList__pi2___stl_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0(VSMT_RTL_Testbench_MultiWidthFreeList__pi2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_MultiWidthFreeList__pi2___stl_sequent__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
    }
    vlSelfRef.__PVT__ra[0U] = vlSelfRef.__PVT__queuePointer__DOT__regHead;
    vlSelfRef.__PVT__ra[1U] = (0x1fU & ((0x20U <= ((IData)(1U) 
                                                   + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead)))
                                         ? ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead))
                                         : ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__queuePointer__DOT__regHead))));
    vlSelf->freeList->__PVT__ra[0U] = vlSelfRef.__PVT__ra
        [0U];
    vlSelf->freeList->__PVT__ra[1U] = vlSelfRef.__PVT__ra
        [1U];
}
