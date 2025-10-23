// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__0(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__ra[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__ra[1U] 
        = vlSelfRef.__PVT__ra[1U];
}

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__1(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8___stl_sequent__TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__rv
        [0U];
    vlSelfRef.__PVT__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__rv
        [1U];
}
