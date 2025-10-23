// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"

extern const VlWide<11>/*351:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0;
extern const VlWide<23>/*735:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0;
extern const VlWide<8>/*255:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0;
extern const VlWide<8>/*255:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0;
extern const VlWide<8>/*255:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0;
extern const VlWide<8>/*255:0*/ VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0;

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___eval_initial__TOP__SMT_RTL_Testbench__core(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___eval_initial__TOP__SMT_RTL_Testbench__core\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__iCache__DOT__regMissIndex = 0U;
    vlSelfRef.__PVT__iCache__DOT__regMissTag = 0U;
    vlSelfRef.__PVT__iCache__DOT__regSerial = 0U;
    vlSelfRef.__PVT__pdStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__pdStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__pdStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__pdStage__DOT__pipeReg[1U][0U] = 0U;
    vlSelfRef.__PVT__pdStage__DOT__pipeReg[1U][1U] = 0U;
    vlSelfRef.__PVT__pdStage__DOT__pipeReg[1U][2U] = 0U;
    vlSelfRef.__PVT__pdStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[0U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[1U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[2U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[3U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[4U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[5U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[6U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[7U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][8U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[8U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][9U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[9U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[0U][0xaU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[0xaU];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[0U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[1U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[2U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[3U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[4U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[5U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[6U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[7U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][8U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[8U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][9U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[9U];
    vlSelfRef.__PVT__idStage__DOT__pipeReg[1U][0xaU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h2d522d82_0[0xaU];
    vlSelfRef.__PVT__idStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[0U][4U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[1U][0U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[1U][1U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[1U][2U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[1U][3U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__pipeReg[1U][4U] = 0U;
    vlSelfRef.__PVT__rnStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__rnStage__DOT__regRecoveredPC = 0U;
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[1U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[2U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[3U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[4U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[5U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[6U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[7U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[8U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[8U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[9U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[9U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xaU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0xaU];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xbU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0xbU];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xcU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0xcU];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xdU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0xdU];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xeU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0xeU];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xfU] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0xfU];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x10U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0x10U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x11U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0x11U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x12U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0x12U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x13U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0x13U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x14U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0x14U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x15U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0x15U];
    vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x16U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h14c113d2_0[0x16U];
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[1U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [1U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[2U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [2U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[3U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [3U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[4U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [4U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[5U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [5U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[6U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [6U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[7U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [7U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[8U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [8U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[9U][(0x1fU 
                                                                            & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                               >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [9U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                   >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                << 
                                                (0x1fU 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0xaU][(0x1fU 
                                                                              & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0xaU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                     >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                  << 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0xbU][(0x1fU 
                                                                              & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0xbU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                     >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                  << 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0xcU][(0x1fU 
                                                                              & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0xcU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                     >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                  << 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0xdU][(0x1fU 
                                                                              & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0xdU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                     >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                  << 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0xeU][(0x1fU 
                                                                              & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0xeU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                     >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                  << 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0xfU][(0x1fU 
                                                                              & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0xfU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                     >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                  << 
                                                  (0x1fU 
                                                   & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x10U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x10U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x11U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x11U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x12U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x12U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x13U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x13U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x14U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x14U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x15U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x15U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x16U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x16U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x17U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x17U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x18U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x18U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x19U][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x19U][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x1aU][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x1aU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x1bU][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x1bU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x1cU][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x1cU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x1dU][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x1dU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x1eU][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x1eU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0U;
    while (VL_GTS_III(32, 0x2c8U, vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)) {
        vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = 0U;
        if (VL_LIKELY(((0x2c7U >= (0x3ffU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))))) {
            vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array[0x1fU][(0x1fU 
                                                                               & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                                                                >> 5U))] 
                = (((~ ((IData)(1U) << (0x1fU & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j))) 
                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
                    [0x1fU][(0x1fU & (vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
                                      >> 5U))]) | ((IData)(vlSelfRef.replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j)));
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j 
            = ((IData)(1U) + vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j);
    }
    vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__i = 0x20U;
    vlSelfRef.__PVT__scheduler__DOT__isInt = 0U;
    vlSelfRef.__PVT__scheduler__DOT__isComplex = 0U;
    vlSelfRef.__PVT__scheduler__DOT__isDiv = 0U;
    vlSelfRef.__PVT__scheduler__DOT__isLoad = 0U;
    vlSelfRef.__PVT__scheduler__DOT__isStore = 0U;
    vlSelfRef.__PVT__scheduler__DOT__isFP = 0U;
    vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt = 0U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg[0U][0U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg[0U][0U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg[0U][0U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg[1U][0U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
           [1U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg[1U][0U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
           [1U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg[1U][0U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__intPipeReg
           [1U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 1U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][0U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][0U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][0U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][1U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][1U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][1U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][2U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][2U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][2U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][3U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][3U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][3U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][3U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][3U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][3U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][4U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][4U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][4U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][4U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg[0U][4U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__complexPipeReg
           [0U][4U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk3__DOT__unnamedblk4__DOT__j = 5U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk3__DOT__i = 1U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][0U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][0U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][0U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][1U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][1U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][1U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][2U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][2U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[0U][2U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][0U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][0U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][0U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][1U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][1U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][1U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][2U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][2U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg[1U][2U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__memPipeReg
           [1U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk5__DOT__unnamedblk6__DOT__j = 3U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk5__DOT__i = 2U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][0U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][0U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][0U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][0U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][1U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][1U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][1U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][1U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][2U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][2U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][2U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][2U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][3U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][3U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][3U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][3U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][3U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][3U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][4U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][4U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][4U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][4U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][4U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][4U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][5U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][5U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][5U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][5U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][5U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][5U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][6U] 
        = (0x3c00000U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][6U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][6U] 
        = (0x3fffc0U | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][6U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg[0U][6U] 
        = (0x3fU | vlSelfRef.__PVT__wakeupPipelineRegister__DOT__fpPipeReg
           [0U][6U]);
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk7__DOT__unnamedblk8__DOT__j = 7U;
    vlSelfRef.__PVT__wakeupPipelineRegister__DOT__unnamedblk7__DOT__i = 1U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[0U][4U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[1U][0U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[1U][1U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[1U][2U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[1U][3U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__pipeReg[1U][4U] = 0U;
    vlSelfRef.__PVT__intRrStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[0U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[1U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[2U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[3U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[4U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[5U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[6U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[0U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[7U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[0U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[1U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[2U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[3U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[4U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[5U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[6U];
    vlSelfRef.__PVT__intExStage__DOT__pipeReg[1U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h3de9eaa4_0[7U];
    vlSelfRef.__PVT__intExStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[0U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[1U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[2U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[3U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[4U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[5U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[6U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[0U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[7U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[0U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[1U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[2U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[3U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[4U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[5U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[6U];
    vlSelfRef.__PVT__intRwStage__DOT__pipeReg[1U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h36564b92_0[7U];
    vlSelfRef.__PVT__intRwStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__complexRrStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__complexRrStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__complexRrStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__complexRrStage__DOT__unnamedblk1__DOT__i = 1U;
    vlSelfRef.__PVT__complexExStage__DOT__localPipeReg[0U][0U][0U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__localPipeReg[0U][0U][1U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__localPipeReg[0U][0U][2U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__localPipeReg[0U][1U][0U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__localPipeReg[0U][1U][1U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__localPipeReg[0U][1U][2U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 2U;
    vlSelfRef.__PVT__complexExStage__DOT__unnamedblk1__DOT__i = 1U;
    vlSelfRef.__PVT__complexExStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__pipeReg[0U][4U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__pipeReg[0U][5U] = 0U;
    vlSelfRef.__PVT__complexExStage__DOT__unnamedblk5__DOT__i = 1U;
    vlSelfRef.__PVT__complexRwStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__complexRwStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__complexRwStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__complexRwStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__complexRwStage__DOT__unnamedblk1__DOT__i = 1U;
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__pipeReg[0U] = 0U;
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__pipeReg[1U] = 0U;
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk3__DOT__i = 2U;
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk3__DOT__i = 2U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[0U][4U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[1U][0U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[1U][1U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[1U][2U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[1U][3U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__pipeReg[1U][4U] = 0U;
    vlSelfRef.__PVT__memRrStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[0U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[1U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[2U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[3U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[4U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[5U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[6U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[0U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[7U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[0U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[1U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[2U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[3U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[4U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[5U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[6U];
    vlSelfRef.__PVT__memExStage__DOT__pipeReg[1U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h1e479923_0[7U];
    vlSelfRef.__PVT__memExStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[0U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[1U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[2U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[3U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[4U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[5U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[6U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[0U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[7U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[0U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[1U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[2U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[3U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[4U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[5U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[6U];
    vlSelfRef.__PVT__mtStage__DOT__pipeReg[1U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[7U];
    vlSelfRef.__PVT__mtStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[0U][4U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[0U][5U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[1U][0U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[1U][1U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[1U][2U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[1U][3U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[1U][4U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__pipeReg[1U][5U] = 0U;
    vlSelfRef.__PVT__maStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__dCache__DOT__dcReadAddrRegTagStg[0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__dcReadAddrRegDataStg[0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__dcReadUncachableReg[0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__dcReadActiveListPtrReg[0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__unnamedblk1__DOT__i = 1U;
    vlSelfRef.__PVT__dCache__DOT__dcWriteAddrReg = 0U;
    vlSelfRef.__PVT__dCache__DOT__dcWriteUncachableReg = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[0U][0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[0U][1U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[0U][2U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[0U][3U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[0U][4U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[0U][5U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[1U][0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[1U][1U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[1U][2U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[1U][3U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[1U][4U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr[1U][5U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[0U][4U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[1U][0U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[1U][1U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[1U][2U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[1U][3U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__pipeReg[1U][4U] = 0U;
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk1__DOT__i = 2U;
    vlSelfRef.__PVT__fpRrStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__fpRrStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__fpRrStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__fpRrStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__fpRrStage__DOT__unnamedblk1__DOT__i = 1U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][0U][0U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][0U][1U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][0U][2U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][0U][3U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][1U][0U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][1U][1U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][1U][2U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][1U][3U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][2U][0U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][2U][1U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][2U][2U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][2U][3U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][3U][0U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][3U][1U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][3U][2U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__localPipeReg[0U][3U][3U] = 0U;
    vlSelfRef.__PVT__fpExStage__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 4U;
    vlSelfRef.__PVT__fpExStage__DOT__unnamedblk1__DOT__i = 1U;
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[0U];
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[1U];
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[2U];
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[3U];
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[4U];
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[5U];
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[6U];
    vlSelfRef.__PVT__fpExStage__DOT__pipeReg[0U][7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_hee3372ac_0[7U];
    vlSelfRef.__PVT__fpExStage__DOT__unnamedblk5__DOT__i = 1U;
    vlSelfRef.__PVT__fpRwStage__DOT__pipeReg[0U][0U] = 0U;
    vlSelfRef.__PVT__fpRwStage__DOT__pipeReg[0U][1U] = 0U;
    vlSelfRef.__PVT__fpRwStage__DOT__pipeReg[0U][2U] = 0U;
    vlSelfRef.__PVT__fpRwStage__DOT__pipeReg[0U][3U] = 0U;
    vlSelfRef.__PVT__fpRwStage__DOT__pipeReg[0U][4U] = 0U;
    vlSelfRef.__PVT__fpRwStage__DOT__unnamedblk1__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__26(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__26\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__Vfuncout;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__Vfuncout = 0;
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__src;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__src = 0;
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__Vfuncout;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__Vfuncout = 0;
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__src;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__src = 0;
    CData/*0:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__inv;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__inv = 0;
    // Body
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
        = (IData)(vlSelfRef.__PVT__intExStage__DOT__fuOpA
                  [0U]);
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB 
        = (IData)(vlSelfRef.__PVT__intExStage__DOT__fuOpB
                  [0U]);
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpA = 0U;
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpB = 0U;
    if ((8U & vlSelfRef.__PVT__intExStage__DOT__aluCode
         [0U])) {
        if ((4U & vlSelfRef.__PVT__intExStage__DOT__aluCode
             [0U])) {
            if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                 [0U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
                                       & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)));
            } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                        [0U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
                                       & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)));
            } else {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA 
                    = VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA, 3U);
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst;
            }
        } else if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                    [0U])) {
            if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                 [0U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA 
                    = VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA, 2U);
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst;
            } else {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA 
                    = VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA, 1U);
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst;
            }
        } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                    [0U])) {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)(((0U != vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)
                                    ? 0U : vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA)));
        } else {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)(((0U == vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)
                                    ? 0U : vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA)));
        }
    } else if ((4U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                [0U])) {
        if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
             [0U])) {
            if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                 [0U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
                                       & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)));
            } else {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
                                       | vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)));
            }
        } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                    [0U])) {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
                                   & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)));
        } else {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
                                   ^ vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)));
        }
    } else if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                [0U])) {
        if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
             [0U])) {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA 
                                   < vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB)));
        } else {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpA 
                = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpB 
                = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)(VL_LTS_III(32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpA, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpB)));
        }
    } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                [0U])) {
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst;
    } else {
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst;
    }
    vlSelfRef.intExStage__DOT____Vcellout__BlockALU__BRA__0__KET____DOT__intALU__aluDataOut 
        = (IData)(vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst);
    vlSelfRef.__PVT__intExStage__DOT__aluDataOut[0U] 
        = vlSelfRef.intExStage__DOT____Vcellout__BlockALU__BRA__0__KET____DOT__intALU__aluDataOut;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__src 
        = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__Vfuncout 
        = __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__src;
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpA 
        = __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__544__Vfuncout;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__inv 
        = ((1U & (~ (vlSelfRef.__PVT__intExStage__DOT__aluCode
                     [0U] >> 3U))) && ((1U & (~ (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                 [0U] 
                                                 >> 2U))) 
                                       && ((1U & (~ 
                                                  (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                   [0U] 
                                                   >> 1U))) 
                                           && (1U & 
                                               vlSelfRef.__PVT__intExStage__DOT__aluCode
                                               [0U]))));
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__src 
        = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB;
    __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__Vfuncout 
        = ((IData)(__Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__inv)
            ? (~ __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__src)
            : __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__src);
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpB 
        = __Vfunc_intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__Inv__545__Vfuncout;
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst 
        = (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpA)) 
                              + (QData)((IData)(vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpB))) 
                             + (QData)((IData)(((1U 
                                                 & (~ 
                                                    (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                     [0U] 
                                                     >> 3U))) 
                                                && ((1U 
                                                     & (~ 
                                                        (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                         [0U] 
                                                         >> 2U))) 
                                                    && ((1U 
                                                         & (~ 
                                                            (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                             [0U] 
                                                             >> 1U))) 
                                                        && (1U 
                                                            & vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                            [0U]))))))));
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderOutOverflow 
        = (1U & (((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst 
                           >> 0x1fU)) ^ (vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpA 
                                         >> 0x1fU)) 
                 & ((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst 
                             >> 0x1fU)) ^ (vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpB 
                                           >> 0x1fU))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__27(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__27\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__Vfuncout;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__Vfuncout = 0;
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__src;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__src = 0;
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__Vfuncout;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__Vfuncout = 0;
    IData/*31:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__src;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__src = 0;
    CData/*0:0*/ __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__inv;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__inv = 0;
    // Body
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
        = (IData)(vlSelfRef.__PVT__intExStage__DOT__fuOpA
                  [1U]);
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB 
        = (IData)(vlSelfRef.__PVT__intExStage__DOT__fuOpB
                  [1U]);
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpA = 0U;
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpB = 0U;
    if ((8U & vlSelfRef.__PVT__intExStage__DOT__aluCode
         [1U])) {
        if ((4U & vlSelfRef.__PVT__intExStage__DOT__aluCode
             [1U])) {
            if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                 [1U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
                                       & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)));
            } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                        [1U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
                                       & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)));
            } else {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA 
                    = VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA, 3U);
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst;
            }
        } else if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                    [1U])) {
            if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                 [1U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA 
                    = VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA, 2U);
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst;
            } else {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA 
                    = VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA, 1U);
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                    = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst;
            }
        } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                    [1U])) {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)(((0U != vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)
                                    ? 0U : vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA)));
        } else {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)(((0U == vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)
                                    ? 0U : vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA)));
        }
    } else if ((4U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                [1U])) {
        if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
             [1U])) {
            if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                 [1U])) {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
                                       & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)));
            } else {
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
                vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                    = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
                                       | vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)));
            }
        } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                    [1U])) {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
                                   & vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)));
        } else {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
                                   ^ vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)));
        }
    } else if ((2U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                [1U])) {
        if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
             [1U])) {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA 
                                   < vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB)));
        } else {
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpA 
                = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpB 
                = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = 0U;
            vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                = (QData)((IData)(VL_LTS_III(32, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpA, vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpB)));
        }
    } else if ((1U & vlSelfRef.__PVT__intExStage__DOT__aluCode
                [1U])) {
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst;
    } else {
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB;
        vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
            = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst;
    }
    vlSelfRef.intExStage__DOT____Vcellout__BlockALU__BRA__1__KET____DOT__intALU__aluDataOut 
        = (IData)(vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst);
    vlSelfRef.__PVT__intExStage__DOT__aluDataOut[1U] 
        = vlSelfRef.intExStage__DOT____Vcellout__BlockALU__BRA__1__KET____DOT__intALU__aluDataOut;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__src 
        = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__Vfuncout 
        = __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__src;
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpA 
        = __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__546__Vfuncout;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__inv 
        = ((1U & (~ (vlSelfRef.__PVT__intExStage__DOT__aluCode
                     [1U] >> 3U))) && ((1U & (~ (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                 [1U] 
                                                 >> 2U))) 
                                       && ((1U & (~ 
                                                  (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                   [1U] 
                                                   >> 1U))) 
                                           && (1U & 
                                               vlSelfRef.__PVT__intExStage__DOT__aluCode
                                               [1U]))));
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__src 
        = vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB;
    __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__Vfuncout 
        = ((IData)(__Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__inv)
            ? (~ __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__src)
            : __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__src);
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpB 
        = __Vfunc_intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__Inv__547__Vfuncout;
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst 
        = (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpA)) 
                              + (QData)((IData)(vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpB))) 
                             + (QData)((IData)(((1U 
                                                 & (~ 
                                                    (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                     [1U] 
                                                     >> 3U))) 
                                                && ((1U 
                                                     & (~ 
                                                        (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                         [1U] 
                                                         >> 2U))) 
                                                    && ((1U 
                                                         & (~ 
                                                            (vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                             [1U] 
                                                             >> 1U))) 
                                                        && (1U 
                                                            & vlSelfRef.__PVT__intExStage__DOT__aluCode
                                                            [1U]))))))));
    vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderOutOverflow 
        = (1U & (((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst 
                           >> 0x1fU)) ^ (vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpA 
                                         >> 0x1fU)) 
                 & ((IData)((vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst 
                             >> 0x1fU)) ^ (vlSelfRef.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpB 
                                           >> 0x1fU))));
}
