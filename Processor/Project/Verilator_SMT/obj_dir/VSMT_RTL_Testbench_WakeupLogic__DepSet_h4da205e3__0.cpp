// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_WakeupLogic.h"

extern const VlWide<8>/*255:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0;

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___eval_initial__TOP__SMT_RTL_Testbench__core__wakeupLogic(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___eval_initial__TOP__SMT_RTL_Testbench__core__wakeupLogic\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[0U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[0U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[1U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[1U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[2U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[2U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[3U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[3U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[4U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[4U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[5U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[5U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[6U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[6U];
    vlSelfRef.__PVT__producerMatrix__DOT__matrix[7U] 
        = VSMT_RTL_Testbench__ConstPool__CONST_h9e67c271_0[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__4(VSMT_RTL_Testbench_WakeupLogic* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_WakeupLogic___act_comb__TOP__SMT_RTL_Testbench__core__wakeupLogic__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[0U];
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[1U];
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[2U];
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[3U];
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[4U];
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[5U];
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[6U];
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = vlSelfRef.__PVT__producerMatrix__DOT__matrix[7U];
    vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector 
        = vlSelfRef.__PVT__wakeupDstVector[0U];
    vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector 
        = ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
           | vlSelfRef.__PVT__wakeupDstVector[1U]);
    vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector 
        = ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
           | vlSelfRef.__PVT__wakeupDstVector[2U]);
    vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector 
        = ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
           | vlSelfRef.__PVT__wakeupDstVector[3U]);
    vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector 
        = ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
           | vlSelfRef.__PVT__wakeupDstVector[4U]);
    vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector 
        = ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
           | vlSelfRef.__PVT__wakeupDstVector[5U]);
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (1U & ((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                    & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (2U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 1U)) << 1U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffffffbU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (4U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 2U)) << 2U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (8U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                         >> 3U)) << 3U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffffffefU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x10U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 4U)) << 4U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffffffdfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x20U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 5U)) << 5U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffffffbfU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x40U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 6U)) << 6U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffffff7fU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x80U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                            >> 7U)) << 7U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffffeffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x100U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 8U)) << 8U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffffdffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x200U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 9U)) << 9U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffffbffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x400U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xaU)) << 0xaU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffff7ffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x800U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                             >> 0xbU)) << 0xbU) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffffefffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x1000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xcU)) << 0xcU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffffdfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x2000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xdU)) << 0xdU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffffbfffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x4000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xeU)) << 0xeU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffff7fffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x8000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                              >> 0xfU)) << 0xfU) & 
                         vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffeffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x10000U & (((~ (IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector)) 
                           << 0x10U) & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffdffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x20000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 1U)) << 0x11U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x40000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 2U)) << 0x12U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x80000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                               >> 3U)) << 0x13U) & 
                          vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffefffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x100000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 4U)) << 0x14U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffdfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x200000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 5U)) << 0x15U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x400000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 6U)) << 0x16U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x800000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                >> 7U)) << 0x17U) & 
                           vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfeffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x1000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 8U)) << 0x18U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x2000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 9U)) << 0x19U) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xfbffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x4000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xaU)) << 0x1aU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x8000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                 >> 0xbU)) << 0x1bU) 
                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xefffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x10000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xcU)) << 0x1cU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x20000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xdU)) << 0x1dU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | (0x40000000U & (((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                                  >> 0xeU)) << 0x1eU) 
                             & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])));
    vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U]) 
           | ((IData)(((~ ((IData)(vlSelfRef.__PVT__producerMatrix__DOT__wakeupVector) 
                           >> 0xfU)) & (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
                                        >> 0x1fU))) 
              << 0x1fU));
    vlSelfRef.__PVT__opMatrixReady[0U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (0xffffU 
                                                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U])))));
    vlSelfRef.__PVT__opMatrixReady[1U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[0U] 
                                                            >> 0x10U)))));
    vlSelfRef.__PVT__opMatrixReady[2U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (0xffffU 
                                                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U])))));
    vlSelfRef.__PVT__opMatrixReady[3U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[1U] 
                                                            >> 0x10U)))));
    vlSelfRef.__PVT__opMatrixReady[4U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (0xffffU 
                                                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U])))));
    vlSelfRef.__PVT__opMatrixReady[5U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[2U] 
                                                            >> 0x10U)))));
    vlSelfRef.__PVT__opMatrixReady[6U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (0xffffU 
                                                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U])))));
    vlSelfRef.__PVT__opMatrixReady[7U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[3U] 
                                                            >> 0x10U)))));
    vlSelfRef.__PVT__opMatrixReady[8U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (0xffffU 
                                                            & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U])))));
    vlSelfRef.__PVT__opMatrixReady[9U] = (1U & (~ (IData)(
                                                          (0U 
                                                           != 
                                                           (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[4U] 
                                                            >> 0x10U)))));
    vlSelfRef.__PVT__opMatrixReady[0xaU] = (1U & (~ (IData)(
                                                            (0U 
                                                             != 
                                                             (0xffffU 
                                                              & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U])))));
    vlSelfRef.__PVT__opMatrixReady[0xbU] = (1U & (~ (IData)(
                                                            (0U 
                                                             != 
                                                             (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[5U] 
                                                              >> 0x10U)))));
    vlSelfRef.__PVT__opMatrixReady[0xcU] = (1U & (~ (IData)(
                                                            (0U 
                                                             != 
                                                             (0xffffU 
                                                              & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U])))));
    vlSelfRef.__PVT__opMatrixReady[0xdU] = (1U & (~ (IData)(
                                                            (0U 
                                                             != 
                                                             (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[6U] 
                                                              >> 0x10U)))));
    vlSelfRef.__PVT__opMatrixReady[0xeU] = (1U & (~ (IData)(
                                                            (0U 
                                                             != 
                                                             (0xffffU 
                                                              & vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U])))));
    vlSelfRef.__PVT__opMatrixReady[0xfU] = (1U & (~ (IData)(
                                                            (0U 
                                                             != 
                                                             (vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix[7U] 
                                                              >> 0x10U)))));
    vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
        = (0xffff0000U & vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    if ((1U & (~ vlSelfRef.__PVT__dispatchedSrcRegReady
               [0U][0U]))) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
               | (0xffffffffULL & ((IData)(1U) << (0xfU 
                                                   & vlSelfRef.__PVT__dispatchedSrcRegPtr))));
    }
    if ((1U & (~ vlSelfRef.__PVT__dispatchedSrcRegReady
               [0U][1U]))) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
               | (0xffffffffULL & ((IData)(1U) << (0xfU 
                                                   & (vlSelfRef.__PVT__dispatchedSrcRegPtr 
                                                      >> 4U)))));
    }
    if ((1U & (~ vlSelfRef.__PVT__dispatchedSrcRegReady
               [0U][2U]))) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
               | (0xffffffffULL & ((IData)(1U) << (0xfU 
                                                   & (vlSelfRef.__PVT__dispatchedSrcRegPtr 
                                                      >> 8U)))));
    }
    if ((1U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (1U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((2U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (2U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((4U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (4U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((8U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (8U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x10U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x10U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x20U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x20U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x40U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x40U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x80U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x80U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x100U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x100U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x200U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x200U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x400U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x400U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x800U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x800U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x1000U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x1000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x2000U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x2000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x4000U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x4000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x8000U & vlSelfRef.__PVT__dependStoreBitVector
         [0U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x8000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
        = (0xffffU & vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    if ((1U & (~ vlSelfRef.__PVT__dispatchedSrcRegReady
               [1U][0U]))) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
               | (0xffffffffULL & ((IData)(1U) << (0x1fU 
                                                   & ((IData)(0x10U) 
                                                      + 
                                                      (0xfU 
                                                       & (vlSelfRef.__PVT__dispatchedSrcRegPtr 
                                                          >> 0xcU)))))));
    }
    if ((1U & (~ vlSelfRef.__PVT__dispatchedSrcRegReady
               [1U][1U]))) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
               | (0xffffffffULL & ((IData)(1U) << (0x1fU 
                                                   & ((IData)(0x10U) 
                                                      + 
                                                      (0xfU 
                                                       & (vlSelfRef.__PVT__dispatchedSrcRegPtr 
                                                          >> 0x10U)))))));
    }
    if ((1U & (~ vlSelfRef.__PVT__dispatchedSrcRegReady
               [1U][2U]))) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
               | (0xffffffffULL & ((IData)(1U) << (0x1fU 
                                                   & ((IData)(0x10U) 
                                                      + 
                                                      (0xfU 
                                                       & (vlSelfRef.__PVT__dispatchedSrcRegPtr 
                                                          >> 0x14U)))))));
    }
    if ((1U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x10000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((2U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x20000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((4U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x40000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((8U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x80000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x10U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x100000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x20U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x200000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x40U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x400000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x80U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x800000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x100U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x1000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x200U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x2000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x400U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x4000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x800U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x8000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x1000U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x10000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x2000U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x20000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x4000U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x40000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if ((0x8000U & vlSelfRef.__PVT__dependStoreBitVector
         [1U])) {
        vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
            = (0x80000000U | vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector);
    }
    if (vlSelfRef.__Vcellinp__producerMatrix__dispatch
        [0U]) {
        VL_ASSIGNSEL_WI(256,16,(0xffU & VL_SHIFTL_III(8,32,32, 
                                                      vlSelfRef.__Vcellinp__producerMatrix__dispatchPtr
                                                      [0U], 4U)), vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix, 
                        (0xffffU & vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector));
    }
    if (vlSelfRef.__Vcellinp__producerMatrix__dispatch
        [1U]) {
        VL_ASSIGNSEL_WI(256,16,(0xffU & VL_SHIFTL_III(8,32,32, 
                                                      vlSelfRef.__Vcellinp__producerMatrix__dispatchPtr
                                                      [1U], 4U)), vlSelfRef.__PVT__producerMatrix__DOT__nextMatrix, 
                        (vlSelfRef.__PVT__producerMatrix__DOT__dispatchVector 
                         >> 0x10U));
    }
}
