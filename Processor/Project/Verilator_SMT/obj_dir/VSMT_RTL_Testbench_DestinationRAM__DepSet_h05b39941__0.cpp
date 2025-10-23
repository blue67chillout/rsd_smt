// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DestinationRAM.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_DestinationRAM___act_sequent__TOP__SMT_RTL_Testbench__core__destinationRAM__0(VSMT_RTL_Testbench_DestinationRAM* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_DestinationRAM___act_sequent__TOP__SMT_RTL_Testbench__core__destinationRAM__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__writeData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
        [0U];
    vlSelfRef.__PVT__writeData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
        [1U];
    vlSelfRef.__PVT__writePtr[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [0U];
    vlSelfRef.__PVT__writePtr[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [1U];
    vlSelfRef.__PVT__write[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [0U];
    vlSelfRef.__PVT__write[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk7__DOT__i = 2U;
        vlSelfRef.__PVT__writeData[0U] = 0U;
        vlSelfRef.__PVT__writePtr[0U] = vlSelfRef.__PVT__rstIndex;
        vlSelfRef.__PVT__write[0U] = 0U;
        vlSelfRef.__PVT__write[1U] = 0U;
        vlSelfRef.__PVT__write[0U] = 1U;
    }
    vlSelfRef.__Vcellinp__dstRAM__wv[0U] = vlSelfRef.__PVT__writeData
        [0U];
    vlSelfRef.__Vcellinp__dstRAM__wv[1U] = vlSelfRef.__PVT__writeData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wa[0U] 
        = vlSelfRef.__PVT__writePtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wa[1U] 
        = vlSelfRef.__PVT__writePtr[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__we[0U] 
        = vlSelfRef.__PVT__write[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__we[1U] 
        = vlSelfRef.__PVT__write[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__dstRAM__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__dstRAM__wv[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_DestinationRAM___act_comb__TOP__SMT_RTL_Testbench__core__destinationRAM__0(VSMT_RTL_Testbench_DestinationRAM* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_DestinationRAM___act_comb__TOP__SMT_RTL_Testbench__core__destinationRAM__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ __Vlvbound_h649ec659__0;
    __Vlvbound_h649ec659__0 = 0;
    CData/*3:0*/ __Vlvbound_h41a11592__0;
    __Vlvbound_h41a11592__0 = 0;
    CData/*3:0*/ __Vlvbound_h79c28a5b__0;
    __Vlvbound_h79c28a5b__0 = 0;
    CData/*3:0*/ __Vlvbound_hb5b6eaf6__0;
    __Vlvbound_hb5b6eaf6__0 = 0;
    // Body
    __Vlvbound_h649ec659__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr
        [0U];
    vlSelfRef.__PVT__readPtr[0U] = __Vlvbound_h649ec659__0;
    __Vlvbound_h649ec659__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr
        [1U];
    vlSelfRef.__PVT__readPtr[1U] = __Vlvbound_h649ec659__0;
    __Vlvbound_h41a11592__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr
        [2U];
    vlSelfRef.__PVT__readPtr[2U] = __Vlvbound_h41a11592__0;
    __Vlvbound_h79c28a5b__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr
        [3U];
    vlSelfRef.__PVT__readPtr[3U] = __Vlvbound_h79c28a5b__0;
    __Vlvbound_hb5b6eaf6__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr
        [4U];
    vlSelfRef.__PVT__readPtr[4U] = __Vlvbound_hb5b6eaf6__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[0U] 
        = vlSelfRef.__PVT__readPtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[1U] 
        = vlSelfRef.__PVT__readPtr[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[2U] 
        = vlSelfRef.__PVT__readPtr[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[3U] 
        = vlSelfRef.__PVT__readPtr[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[4U] 
        = vlSelfRef.__PVT__readPtr[4U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_DestinationRAM___act_comb__TOP__SMT_RTL_Testbench__core__destinationRAM__1(VSMT_RTL_Testbench_DestinationRAM* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_DestinationRAM___act_comb__TOP__SMT_RTL_Testbench__core__destinationRAM__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*7:0*/ __Vlvbound_h330862a0__0;
    __Vlvbound_h330862a0__0 = 0;
    // Body
    vlSelfRef.__Vcellout__dstRAM__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__dstRAM__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv
        [1U];
    vlSelfRef.__Vcellout__dstRAM__rv[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv
        [2U];
    vlSelfRef.__Vcellout__dstRAM__rv[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv
        [3U];
    vlSelfRef.__Vcellout__dstRAM__rv[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv
        [4U];
    vlSelfRef.__PVT__readData[0U] = vlSelfRef.__Vcellout__dstRAM__rv
        [0U];
    vlSelfRef.__PVT__readData[1U] = vlSelfRef.__Vcellout__dstRAM__rv
        [1U];
    vlSelfRef.__PVT__readData[2U] = vlSelfRef.__Vcellout__dstRAM__rv
        [2U];
    vlSelfRef.__PVT__readData[3U] = vlSelfRef.__Vcellout__dstRAM__rv
        [3U];
    vlSelfRef.__PVT__readData[4U] = vlSelfRef.__Vcellout__dstRAM__rv
        [4U];
    __Vlvbound_h330862a0__0 = vlSelfRef.__PVT__readData
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag[0U] 
        = __Vlvbound_h330862a0__0;
    __Vlvbound_h330862a0__0 = vlSelfRef.__PVT__readData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag[1U] 
        = __Vlvbound_h330862a0__0;
    __Vlvbound_h330862a0__0 = vlSelfRef.__PVT__readData
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag[2U] 
        = __Vlvbound_h330862a0__0;
    __Vlvbound_h330862a0__0 = vlSelfRef.__PVT__readData
        [3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag[3U] 
        = __Vlvbound_h330862a0__0;
    __Vlvbound_h330862a0__0 = vlSelfRef.__PVT__readData
        [4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag[4U] 
        = __Vlvbound_h330862a0__0;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_DestinationRAM___nba_sequent__TOP__SMT_RTL_Testbench__core__destinationRAM__0(VSMT_RTL_Testbench_DestinationRAM* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_DestinationRAM___nba_sequent__TOP__SMT_RTL_Testbench__core__destinationRAM__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rstIndex = (((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart) 
                                  | (0xfU <= (IData)(vlSelfRef.__PVT__rstIndex)))
                                  ? 0U : (0xfU & ((IData)(1U) 
                                                  + (IData)(vlSelfRef.__PVT__rstIndex))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_DestinationRAM___nba_sequent__TOP__SMT_RTL_Testbench__core__destinationRAM__1(VSMT_RTL_Testbench_DestinationRAM* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_DestinationRAM___nba_sequent__TOP__SMT_RTL_Testbench__core__destinationRAM__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk7__DOT__i = 2U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_DestinationRAM___nba_comb__TOP__SMT_RTL_Testbench__core__destinationRAM__0(VSMT_RTL_Testbench_DestinationRAM* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_DestinationRAM___nba_comb__TOP__SMT_RTL_Testbench__core__destinationRAM__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__writePtr[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [0U];
    vlSelfRef.__PVT__writePtr[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writePtr
        [1U];
    vlSelfRef.__PVT__writeData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
        [0U];
    vlSelfRef.__PVT__writeData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__writeDstTag
        [1U];
    vlSelfRef.__PVT__write[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [0U];
    vlSelfRef.__PVT__write[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__writePtr[0U] = vlSelfRef.__PVT__rstIndex;
        vlSelfRef.__PVT__writeData[0U] = 0U;
        vlSelfRef.__PVT__write[0U] = 0U;
        vlSelfRef.__PVT__write[1U] = 0U;
        vlSelfRef.__PVT__write[0U] = 1U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wa[0U] 
        = vlSelfRef.__PVT__writePtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wa[1U] 
        = vlSelfRef.__PVT__writePtr[1U];
    vlSelfRef.__Vcellinp__dstRAM__wv[0U] = vlSelfRef.__PVT__writeData
        [0U];
    vlSelfRef.__Vcellinp__dstRAM__wv[1U] = vlSelfRef.__PVT__writeData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__we[0U] 
        = vlSelfRef.__PVT__write[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__we[1U] 
        = vlSelfRef.__PVT__write[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__dstRAM__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__dstRAM__wv[1U];
}
