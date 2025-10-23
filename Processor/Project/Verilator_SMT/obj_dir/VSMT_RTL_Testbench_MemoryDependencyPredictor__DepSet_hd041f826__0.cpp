// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_MemoryDependencyPredictor.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_MemoryDependencyPredictor___act_sequent__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0(VSMT_RTL_Testbench_MemoryDependencyPredictor* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_MemoryDependencyPredictor___act_sequent__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*9:0*/ __Vfunc_ToMDT_Index__0__Vfuncout;
    __Vfunc_ToMDT_Index__0__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToMDT_Index__0__addr;
    __Vfunc_ToMDT_Index__0__addr = 0;
    // Body
    vlSelfRef.__PVT__mdtWV[0U] = 1U;
    __Vfunc_ToMDT_Index__0__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
        [0U];
    __Vfunc_ToMDT_Index__0__Vfuncout = (0x3ffU & (__Vfunc_ToMDT_Index__0__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__mdtRA[0U] = __Vfunc_ToMDT_Index__0__Vfuncout;
    __Vfunc_ToMDT_Index__0__addr = (0xfffffU & ((IData)(4U) 
                                                + vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                                [0U]));
    __Vfunc_ToMDT_Index__0__Vfuncout = (0x3ffU & (__Vfunc_ToMDT_Index__0__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__mdtRA[1U] = __Vfunc_ToMDT_Index__0__Vfuncout;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 1U;
        vlSelfRef.__PVT__mdtWV[0U] = 0U;
        vlSelfRef.__PVT__mdtRA[0U] = 0U;
        vlSelfRef.__PVT__mdtRA[1U] = 1U;
    }
    vlSelfRef.__Vcellinp__mdt__wv[0U] = vlSelfRef.__PVT__mdtWV
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__ra[0U] 
        = vlSelfRef.__PVT__mdtRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__ra[1U] 
        = vlSelfRef.__PVT__mdtRA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__mdt__wv[0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MemoryDependencyPredictor___act_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0(VSMT_RTL_Testbench_MemoryDependencyPredictor* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_MemoryDependencyPredictor___act_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vlvbound_hf6fb3ae5__0;
    __Vlvbound_hf6fb3ae5__0 = 0;
    // Body
    __Vlvbound_hf6fb3ae5__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__memAccessOrderViolation
        [0U];
    vlSelfRef.__PVT__mdtWE[0U] = __Vlvbound_hf6fb3ae5__0;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__mdtWE[0U] = 1U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__we[0U] 
        = vlSelfRef.__PVT__mdtWE[0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MemoryDependencyPredictor___act_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__1(VSMT_RTL_Testbench_MemoryDependencyPredictor* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_MemoryDependencyPredictor___act_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*9:0*/ __Vlvbound_h68d986fb__0;
    __Vlvbound_h68d986fb__0 = 0;
    SData/*9:0*/ __Vfunc_ToMDT_Index__1__Vfuncout;
    __Vfunc_ToMDT_Index__1__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToMDT_Index__1__addr;
    __Vfunc_ToMDT_Index__1__addr = 0;
    // Body
    __Vfunc_ToMDT_Index__1__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__conflictLoadPC
        [0U];
    __Vfunc_ToMDT_Index__1__Vfuncout = (0x3ffU & (__Vfunc_ToMDT_Index__1__addr 
                                                  >> 2U));
    __Vlvbound_h68d986fb__0 = __Vfunc_ToMDT_Index__1__Vfuncout;
    vlSelfRef.__PVT__mdtWA[0U] = __Vlvbound_h68d986fb__0;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__Vlvbound_hd4b5281c__0 = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__mdtWA[0U] = vlSelfRef.__Vlvbound_hd4b5281c__0;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__wa[0U] 
        = vlSelfRef.__PVT__mdtWA[0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MemoryDependencyPredictor___act_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__2(VSMT_RTL_Testbench_MemoryDependencyPredictor* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_MemoryDependencyPredictor___act_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__mdt__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__mdt__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__rv
        [1U];
    vlSelfRef.__PVT__mdtRV[0U] = vlSelfRef.__Vcellout__mdt__rv
        [0U];
    vlSelfRef.__PVT__mdtRV[1U] = vlSelfRef.__Vcellout__mdt__rv
        [1U];
    vlSelfRef.__PVT__prediction[0U] = vlSelfRef.__PVT__mdtRV
        [0U];
    vlSelfRef.__PVT__prediction[1U] = vlSelfRef.__PVT__mdtRV
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__memDependencyPred[0U] 
        = vlSelfRef.__PVT__prediction[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__memDependencyPred[1U] 
        = vlSelfRef.__PVT__prediction[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MemoryDependencyPredictor___nba_sequent__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0(VSMT_RTL_Testbench_MemoryDependencyPredictor* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_MemoryDependencyPredictor___nba_sequent__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__resetIndex = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart)
                                    ? 0U : (0x3ffU 
                                            & ((IData)(1U) 
                                               + (IData)(vlSelfRef.__PVT__resetIndex))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MemoryDependencyPredictor___nba_sequent__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__1(VSMT_RTL_Testbench_MemoryDependencyPredictor* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_MemoryDependencyPredictor___nba_sequent__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 1U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_MemoryDependencyPredictor___nba_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0(VSMT_RTL_Testbench_MemoryDependencyPredictor* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_MemoryDependencyPredictor___nba_comb__TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*9:0*/ __Vfunc_ToMDT_Index__0__Vfuncout;
    __Vfunc_ToMDT_Index__0__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToMDT_Index__0__addr;
    __Vfunc_ToMDT_Index__0__addr = 0;
    // Body
    __Vfunc_ToMDT_Index__0__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
        [0U];
    __Vfunc_ToMDT_Index__0__Vfuncout = (0x3ffU & (__Vfunc_ToMDT_Index__0__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__mdtRA[0U] = __Vfunc_ToMDT_Index__0__Vfuncout;
    __Vfunc_ToMDT_Index__0__addr = (0xfffffU & ((IData)(4U) 
                                                + vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                                [0U]));
    __Vfunc_ToMDT_Index__0__Vfuncout = (0x3ffU & (__Vfunc_ToMDT_Index__0__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__mdtRA[1U] = __Vfunc_ToMDT_Index__0__Vfuncout;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__mdtRA[0U] = 0U;
        vlSelfRef.__PVT__mdtRA[1U] = 1U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__ra[0U] 
        = vlSelfRef.__PVT__mdtRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__ra[1U] 
        = vlSelfRef.__PVT__mdtRA[1U];
}
