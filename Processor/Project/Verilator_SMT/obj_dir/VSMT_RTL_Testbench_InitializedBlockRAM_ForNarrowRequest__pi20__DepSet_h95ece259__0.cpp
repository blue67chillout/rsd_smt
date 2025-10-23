// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___ico_sequent__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram__0(VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___ico_sequent__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*20:0*/ __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout;
    __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetHexArrayIndexFromAddress__0__addr;
    __Vfunc_GetHexArrayIndexFromAddress__0__addr = 0;
    CData/*0:0*/ __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetBlockOffsetFromAddress__1__addr;
    __Vfunc_GetBlockOffsetFromAddress__1__addr = 0;
    IData/*20:0*/ __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout;
    __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetHexArrayIndexFromAddress__2__addr;
    __Vfunc_GetHexArrayIndexFromAddress__2__addr = 0;
    CData/*0:0*/ __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetBlockOffsetFromAddress__3__addr;
    __Vfunc_GetBlockOffsetFromAddress__3__addr = 0;
    // Body
    __Vfunc_GetHexArrayIndexFromAddress__2__addr = 
        (0x3fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                      >> 7U));
    __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout 
        = (0x1fffffU & (__Vfunc_GetHexArrayIndexFromAddress__2__addr 
                        >> 1U));
    vlSelfRef.__PVT__hexFileWA = __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__3__addr = (0x3fffffU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                                                     >> 7U));
    __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout 
        = (1U & __Vfunc_GetBlockOffsetFromAddress__3__addr);
    vlSelfRef.__PVT__hexFileWAOffset = __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout;
    vlSelfRef.__PVT__tmpWriteEntry[0U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][0U];
    vlSelfRef.__PVT__tmpWriteEntry[1U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][1U];
    vlSelfRef.__PVT__tmpWriteEntry[2U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][2U];
    vlSelfRef.__PVT__tmpWriteEntry[3U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][3U];
    VL_ASSIGNSEL_WQ(128,64,(0x7fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileWAOffset), 6U)), vlSelfRef.__PVT__tmpWriteEntry, 
                    (((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U])) 
                      << 0x3cU) | (((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[1U])) 
                                    << 0x1cU) | ((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U])) 
                                                 >> 4U))));
    vlSelfRef.__PVT__hexFileWV[0U] = vlSelfRef.__PVT__tmpWriteEntry[0U];
    vlSelfRef.__PVT__hexFileWV[1U] = vlSelfRef.__PVT__tmpWriteEntry[1U];
    vlSelfRef.__PVT__hexFileWV[2U] = vlSelfRef.__PVT__tmpWriteEntry[2U];
    vlSelfRef.__PVT__hexFileWV[3U] = vlSelfRef.__PVT__tmpWriteEntry[3U];
    __Vfunc_GetHexArrayIndexFromAddress__0__addr = vlSelfRef.__PVT__raReg;
    __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout 
        = (0x1fffffU & (__Vfunc_GetHexArrayIndexFromAddress__0__addr 
                        >> 1U));
    vlSelfRef.__PVT__hexFileRA = __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__1__addr = vlSelfRef.__PVT__raReg;
    __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout 
        = (1U & __Vfunc_GetBlockOffsetFromAddress__1__addr);
    vlSelfRef.__PVT__hexFileRAOffset = __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout;
    vlSelfRef.__PVT__hexFileRV[0U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][0U];
    vlSelfRef.__PVT__hexFileRV[1U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][1U];
    vlSelfRef.__PVT__hexFileRV[2U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][2U];
    vlSelfRef.__PVT__hexFileRV[3U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][3U];
    vlSelfRef.__PVT__rv = (((QData)((IData)(vlSelfRef.__PVT__hexFileRV[
                                            (((IData)(0x3fU) 
                                              + (0x7fU 
                                                 & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))) 
                                             >> 5U)])) 
                            << ((0U == (0x1fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U)))
                                 ? 0x20U : ((IData)(0x40U) 
                                            - (0x1fU 
                                               & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))))) 
                           | (((0U == (0x1fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U)))
                                ? 0ULL : ((QData)((IData)(
                                                          vlSelfRef.__PVT__hexFileRV[
                                                          (((IData)(0x1fU) 
                                                            + 
                                                            (0x7fU 
                                                             & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))) 
                                                           >> 5U)])) 
                                          << ((IData)(0x20U) 
                                              - (0x1fU 
                                                 & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))))) 
                              | ((QData)((IData)(vlSelfRef.__PVT__hexFileRV[
                                                 (3U 
                                                  & (VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U) 
                                                     >> 5U))])) 
                                 >> (0x1fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U)))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___nba_sequent__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram__0(VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___nba_sequent__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*20:0*/ __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout;
    __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetHexArrayIndexFromAddress__0__addr;
    __Vfunc_GetHexArrayIndexFromAddress__0__addr = 0;
    CData/*0:0*/ __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetBlockOffsetFromAddress__1__addr;
    __Vfunc_GetBlockOffsetFromAddress__1__addr = 0;
    VlWide<4>/*127:0*/ __VdlyVal__array__v0;
    VL_ZERO_W(128, __VdlyVal__array__v0);
    IData/*20:0*/ __VdlyDim0__array__v0;
    __VdlyDim0__array__v0 = 0;
    CData/*0:0*/ __VdlySet__array__v0;
    __VdlySet__array__v0 = 0;
    // Body
    __VdlySet__array__v0 = 0U;
    if (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__hasRequest) 
         & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
            >> 4U))) {
        __VdlyVal__array__v0[0U] = vlSelfRef.__PVT__hexFileWV[0U];
        __VdlyVal__array__v0[1U] = vlSelfRef.__PVT__hexFileWV[1U];
        __VdlyVal__array__v0[2U] = vlSelfRef.__PVT__hexFileWV[2U];
        __VdlyVal__array__v0[3U] = vlSelfRef.__PVT__hexFileWV[3U];
        __VdlyDim0__array__v0 = vlSelfRef.__PVT__hexFileWA;
        __VdlySet__array__v0 = 1U;
    }
    vlSelfRef.__PVT__dummyRV[0U] = vlSelfRef.__PVT__hexFileRV[0U];
    vlSelfRef.__PVT__dummyRV[1U] = vlSelfRef.__PVT__hexFileRV[1U];
    vlSelfRef.__PVT__dummyRV[2U] = vlSelfRef.__PVT__hexFileRV[2U];
    vlSelfRef.__PVT__dummyRV[3U] = vlSelfRef.__PVT__hexFileRV[3U];
    vlSelfRef.__PVT__raReg = (0x3fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                                           >> 7U));
    if (__VdlySet__array__v0) {
        vlSelfRef.array[__VdlyDim0__array__v0][0U] 
            = __VdlyVal__array__v0[0U];
        vlSelfRef.array[__VdlyDim0__array__v0][1U] 
            = __VdlyVal__array__v0[1U];
        vlSelfRef.array[__VdlyDim0__array__v0][2U] 
            = __VdlyVal__array__v0[2U];
        vlSelfRef.array[__VdlyDim0__array__v0][3U] 
            = __VdlyVal__array__v0[3U];
    }
    __Vfunc_GetHexArrayIndexFromAddress__0__addr = vlSelfRef.__PVT__raReg;
    __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout 
        = (0x1fffffU & (__Vfunc_GetHexArrayIndexFromAddress__0__addr 
                        >> 1U));
    vlSelfRef.__PVT__hexFileRA = __Vfunc_GetHexArrayIndexFromAddress__0__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__1__addr = vlSelfRef.__PVT__raReg;
    __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout 
        = (1U & __Vfunc_GetBlockOffsetFromAddress__1__addr);
    vlSelfRef.__PVT__hexFileRAOffset = __Vfunc_GetBlockOffsetFromAddress__1__Vfuncout;
    vlSelfRef.__PVT__hexFileRV[0U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][0U];
    vlSelfRef.__PVT__hexFileRV[1U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][1U];
    vlSelfRef.__PVT__hexFileRV[2U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][2U];
    vlSelfRef.__PVT__hexFileRV[3U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileRA][3U];
    vlSelfRef.__PVT__rv = (((QData)((IData)(vlSelfRef.__PVT__hexFileRV[
                                            (((IData)(0x3fU) 
                                              + (0x7fU 
                                                 & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))) 
                                             >> 5U)])) 
                            << ((0U == (0x1fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U)))
                                 ? 0x20U : ((IData)(0x40U) 
                                            - (0x1fU 
                                               & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))))) 
                           | (((0U == (0x1fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U)))
                                ? 0ULL : ((QData)((IData)(
                                                          vlSelfRef.__PVT__hexFileRV[
                                                          (((IData)(0x1fU) 
                                                            + 
                                                            (0x7fU 
                                                             & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))) 
                                                           >> 5U)])) 
                                          << ((IData)(0x20U) 
                                              - (0x1fU 
                                                 & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U))))) 
                              | ((QData)((IData)(vlSelfRef.__PVT__hexFileRV[
                                                 (3U 
                                                  & (VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U) 
                                                     >> 5U))])) 
                                 >> (0x1fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileRAOffset), 6U)))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___nba_sequent__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram__1(VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___nba_sequent__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*20:0*/ __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout;
    __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetHexArrayIndexFromAddress__2__addr;
    __Vfunc_GetHexArrayIndexFromAddress__2__addr = 0;
    CData/*0:0*/ __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_GetBlockOffsetFromAddress__3__addr;
    __Vfunc_GetBlockOffsetFromAddress__3__addr = 0;
    // Body
    __Vfunc_GetHexArrayIndexFromAddress__2__addr = 
        (0x3fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                      >> 7U));
    __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout 
        = (0x1fffffU & (__Vfunc_GetHexArrayIndexFromAddress__2__addr 
                        >> 1U));
    vlSelfRef.__PVT__hexFileWA = __Vfunc_GetHexArrayIndexFromAddress__2__Vfuncout;
    __Vfunc_GetBlockOffsetFromAddress__3__addr = (0x3fffffU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                                                     >> 7U));
    __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout 
        = (1U & __Vfunc_GetBlockOffsetFromAddress__3__addr);
    vlSelfRef.__PVT__hexFileWAOffset = __Vfunc_GetBlockOffsetFromAddress__3__Vfuncout;
    vlSelfRef.__PVT__tmpWriteEntry[0U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][0U];
    vlSelfRef.__PVT__tmpWriteEntry[1U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][1U];
    vlSelfRef.__PVT__tmpWriteEntry[2U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][2U];
    vlSelfRef.__PVT__tmpWriteEntry[3U] = vlSelfRef.array
        [vlSelfRef.__PVT__hexFileWA][3U];
    VL_ASSIGNSEL_WQ(128,64,(0x7fU & VL_SHIFTL_III(7,32,32, (IData)(vlSelfRef.__PVT__hexFileWAOffset), 6U)), vlSelfRef.__PVT__tmpWriteEntry, 
                    (((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U])) 
                      << 0x3cU) | (((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[1U])) 
                                    << 0x1cU) | ((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U])) 
                                                 >> 4U))));
    vlSelfRef.__PVT__hexFileWV[0U] = vlSelfRef.__PVT__tmpWriteEntry[0U];
    vlSelfRef.__PVT__hexFileWV[1U] = vlSelfRef.__PVT__tmpWriteEntry[1U];
    vlSelfRef.__PVT__hexFileWV[2U] = vlSelfRef.__PVT__tmpWriteEntry[2U];
    vlSelfRef.__PVT__hexFileWV[3U] = vlSelfRef.__PVT__tmpWriteEntry[3U];
}
