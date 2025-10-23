// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench__Syms.h"

extern const VlWide<8>/*255:0*/ VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0;

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__2(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*2:0*/ __Vfunc_idStage__DOT__ToInsnLane__512__mopLane;
    __Vfunc_idStage__DOT__ToInsnLane__512__mopLane = 0;
    IData/*31:0*/ __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 0;
    IData/*31:0*/ __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 0;
    IData/*31:0*/ __Vfunc_ToAddrFromPC__513__Vfuncout;
    __Vfunc_ToAddrFromPC__513__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToAddrFromPC__513__pc;
    __Vfunc_ToAddrFromPC__513__pc = 0;
    VlWide<3>/*95:0*/ __Vtemp_6;
    VlWide<3>/*95:0*/ __Vtemp_13;
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__idStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__idStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__controller__DOT__idStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__idStage = 2U;
        } else if (vlSelfRef.__PVT__rnStage__DOT__regFlush) {
            vlSelfRef.__PVT__controller__DOT__idStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
            vlSelfRef.__PVT__controller__DOT__idStage = 2U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage 
        = vlSelfRef.__PVT__controller__DOT__idStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__idStage;
    vlSelfRef.__PVT__idStage__DOT__nextValidMOps = vlSelfRef.__PVT__idStage__DOT__pickedValidMOps;
    vlSelfRef.__PVT__idStage__DOT__complete = 1U;
    if (((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
         & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [0U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 1U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [0U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 2U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [0U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((1U & (~ vlSelfRef.__PVT__idStage__DOT__insnValidOut
               [0U]))) {
        vlSelfRef.__PVT__idStage__DOT__nextValidMOps 
            = (0x38U & (IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps));
        vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk13__DOT__j = 3U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 3U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [1U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 4U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [1U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    if ((((IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps) 
          >> 5U) & vlSelfRef.__PVT__idStage__DOT__insnValidIn
         [1U])) {
        vlSelfRef.__PVT__idStage__DOT__complete = 0U;
    }
    vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk12__DOT__j = 3U;
    if ((1U & (~ vlSelfRef.__PVT__idStage__DOT__insnValidOut
               [1U]))) {
        vlSelfRef.__PVT__idStage__DOT__nextValidMOps 
            = (7U & (IData)(vlSelfRef.__PVT__idStage__DOT__nextValidMOps));
        vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk13__DOT__j = 3U;
    }
    vlSelfRef.__PVT__idStage__DOT__unnamedblk11__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper 
        = (1U & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__complete)));
    vlSelfRef.__PVT__idStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__idStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage));
    __Vfunc_idStage__DOT__ToInsnLane__512__mopLane 
        = vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
        [0U];
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 0;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 0;
    {
        if ((0U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel25;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((1U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel25;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((2U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel25;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 1U;
        if ((3U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel25;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((4U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel25;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((5U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel25;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 2U;
        vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
        __Vlabel25: ;
    }
    vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane 
        = vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout;
    if ((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                              [0U])))) {
        __Vtemp_6[1U] = (((0U == (0x1fU & ((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                           [0U]))) ? 0U
                           : (vlSelfRef.__PVT__idStage__DOT__microOps[
                              ((IData)(2U) + (0xfU 
                                              & (((IData)(0x4cU) 
                                                  * 
                                                  vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                  [0U]) 
                                                 >> 5U)))] 
                              << ((IData)(0x20U) - 
                                  (0x1fU & ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [0U]))))) 
                         | (vlSelfRef.__PVT__idStage__DOT__microOps[
                            ((IData)(1U) + (0xfU & 
                                            (((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [0U]) 
                                             >> 5U)))] 
                            >> (0x1fU & ((IData)(0x4cU) 
                                         * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                         [0U]))));
        __Vtemp_6[2U] = (0xfffU & (((0U == (0x1fU & 
                                            ((IData)(0x4cU) 
                                             * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                             [0U])))
                                     ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                             ((IData)(3U) 
                                              + (0xfU 
                                                 & (((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [0U]) 
                                                    >> 5U)))] 
                                             << ((IData)(0x20U) 
                                                 - 
                                                 (0x1fU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [0U]))))) 
                                   | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                      ((IData)(2U) 
                                       + (0xfU & (((IData)(0x4cU) 
                                                   * 
                                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                   [0U]) 
                                                  >> 5U)))] 
                                      >> (0x1fU & ((IData)(0x4cU) 
                                                   * 
                                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                   [0U])))));
    } else {
        __Vtemp_6[1U] = 0U;
        __Vtemp_6[2U] = 0U;
    }
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U] 
        = ((0x1fffffU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][1U]) | (((0x1c7U >= (0x1ffU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [0U])))
                           ? (((0U == (0x1fU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [0U])))
                                ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        ((IData)(1U) 
                                         + (0xfU & 
                                            (((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [0U]) 
                                             >> 5U)))] 
                                        << ((IData)(0x20U) 
                                            - (0x1fU 
                                               & ((IData)(0x4cU) 
                                                  * 
                                                  vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                  [0U]))))) 
                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                 (0xfU & (((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                           [0U]) >> 5U))] 
                                 >> (0x1fU & ((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [0U]))))
                           : 0U) << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][2U] 
        = ((((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * 
                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                   [0U]))) ? (((0U 
                                                == 
                                                (0x1fU 
                                                 & ((IData)(0x4cU) 
                                                    * 
                                                    vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                    [0U])))
                                                ? 0U
                                                : (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[
                                                   ((IData)(1U) 
                                                    + 
                                                    (0xfU 
                                                     & (((IData)(0x4cU) 
                                                         * 
                                                         vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                         [0U]) 
                                                        >> 5U)))] 
                                                   << 
                                                   ((IData)(0x20U) 
                                                    - 
                                                    (0x1fU 
                                                     & ((IData)(0x4cU) 
                                                        * 
                                                        vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                        [0U]))))) 
                                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                                 (0xfU 
                                                  & (((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [0U]) 
                                                     >> 5U))] 
                                                 >> 
                                                 (0x1fU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [0U]))))
              : 0U) >> 0xbU) | (__Vtemp_6[1U] << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][3U] 
        = ((__Vtemp_6[1U] >> 0xbU) | (__Vtemp_6[2U] 
                                      << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0x3ffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0x3fffU & (__Vtemp_6[2U] >> 0xbU)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0x3ffdU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0x3fffU & (((vlSelfRef.__PVT__idStage__DOT__insnValidOut
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                      & vlSelfRef.__PVT__idStage__DOT__mopPicked
                                      [0U]) & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__clear))) 
                                    << 1U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U] 
        = ((0xffe00001U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][1U]) | (0x1ffffeU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][8U] 
                                      >> 9U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][0U] 
        = (IData)(vlSelfRef.__PVT__idStage__DOT__brPredOut
                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane]);
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][1U]) | (IData)((vlSelfRef.__PVT__idStage__DOT__brPredOut
                                 [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                 >> 0x20U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0xfU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][0xaU] 
                                     << 5U) | (0x10U 
                                               & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][9U] 
                                                  >> 0x1bU)))));
    vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U] 
        = ((0x3ff3U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [0U][4U]) | (0xcU & (vlSelfRef.__PVT__idStage__DOT__nextStage
                                 [0U][1U] >> 0x14U)));
    __Vfunc_idStage__DOT__ToInsnLane__512__mopLane 
        = vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
        [1U];
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 0;
    __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 0;
    {
        if ((0U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel26;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((1U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel26;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((2U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
            goto __Vlabel26;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 1U;
        if ((3U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel26;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 1U;
        if ((4U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel26;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 2U;
        if ((5U == (IData)(__Vfunc_idStage__DOT__ToInsnLane__512__mopLane))) {
            vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 1U;
            goto __Vlabel26;
        }
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__unnamedblk8__DOT__j = 3U;
        __Vfunc_idStage__DOT__ToInsnLane__512__unnamedblk7__DOT__i = 2U;
        vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = 0U;
        __Vlabel26: ;
    }
    vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane 
        = vlSelfRef.__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout;
    if ((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                              [1U])))) {
        __Vtemp_13[1U] = (((0U == (0x1fU & ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [1U])))
                            ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                    ((IData)(2U) + 
                                     (0xfU & (((IData)(0x4cU) 
                                               * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                               [1U]) 
                                              >> 5U)))] 
                                    << ((IData)(0x20U) 
                                        - (0x1fU & 
                                           ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [1U]))))) 
                          | (vlSelfRef.__PVT__idStage__DOT__microOps[
                             ((IData)(1U) + (0xfU & 
                                             (((IData)(0x4cU) 
                                               * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                               [1U]) 
                                              >> 5U)))] 
                             >> (0x1fU & ((IData)(0x4cU) 
                                          * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                          [1U]))));
        __Vtemp_13[2U] = (0xfffU & (((0U == (0x1fU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [1U])))
                                      ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                              ((IData)(3U) 
                                               + (0xfU 
                                                  & (((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [1U]) 
                                                     >> 5U)))] 
                                              << ((IData)(0x20U) 
                                                  - 
                                                  (0x1fU 
                                                   & ((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [1U]))))) 
                                    | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                       ((IData)(2U) 
                                        + (0xfU & (
                                                   ((IData)(0x4cU) 
                                                    * 
                                                    vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                    [1U]) 
                                                   >> 5U)))] 
                                       >> (0x1fU & 
                                           ((IData)(0x4cU) 
                                            * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                            [1U])))));
    } else {
        __Vtemp_13[1U] = 0U;
        __Vtemp_13[2U] = 0U;
    }
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U] 
        = ((0x1fffffU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][1U]) | (((0x1c7U >= (0x1ffU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [1U])))
                           ? (((0U == (0x1fU & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                [1U])))
                                ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        ((IData)(1U) 
                                         + (0xfU & 
                                            (((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [1U]) 
                                             >> 5U)))] 
                                        << ((IData)(0x20U) 
                                            - (0x1fU 
                                               & ((IData)(0x4cU) 
                                                  * 
                                                  vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                  [1U]))))) 
                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                 (0xfU & (((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                           [1U]) >> 5U))] 
                                 >> (0x1fU & ((IData)(0x4cU) 
                                              * vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                              [1U]))))
                           : 0U) << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][2U] 
        = ((((0x1c7U >= (0x1ffU & ((IData)(0x4cU) * 
                                   vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                   [1U]))) ? (((0U 
                                                == 
                                                (0x1fU 
                                                 & ((IData)(0x4cU) 
                                                    * 
                                                    vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                    [1U])))
                                                ? 0U
                                                : (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[
                                                   ((IData)(1U) 
                                                    + 
                                                    (0xfU 
                                                     & (((IData)(0x4cU) 
                                                         * 
                                                         vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                         [1U]) 
                                                        >> 5U)))] 
                                                   << 
                                                   ((IData)(0x20U) 
                                                    - 
                                                    (0x1fU 
                                                     & ((IData)(0x4cU) 
                                                        * 
                                                        vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                        [1U]))))) 
                                              | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                                 (0xfU 
                                                  & (((IData)(0x4cU) 
                                                      * 
                                                      vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                      [1U]) 
                                                     >> 5U))] 
                                                 >> 
                                                 (0x1fU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     vlSelfRef.__PVT__idStage__DOT__mopPickedIndex
                                                     [1U]))))
              : 0U) >> 0xbU) | (__Vtemp_13[1U] << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][3U] 
        = ((__Vtemp_13[1U] >> 0xbU) | (__Vtemp_13[2U] 
                                       << 0x15U));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0x3ffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0x3fffU & (__Vtemp_13[2U] 
                                    >> 0xbU)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0x3ffdU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0x3fffU & (((vlSelfRef.__PVT__idStage__DOT__insnValidOut
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                      & vlSelfRef.__PVT__idStage__DOT__mopPicked
                                      [1U]) & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__clear))) 
                                    << 1U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U] 
        = ((0xffe00001U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][1U]) | (0x1ffffeU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                      [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][8U] 
                                      >> 9U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][0U] 
        = (IData)(vlSelfRef.__PVT__idStage__DOT__brPredOut
                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane]);
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][1U]) | (IData)((vlSelfRef.__PVT__idStage__DOT__brPredOut
                                 [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane] 
                                 >> 0x20U)));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0xfU & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][0xaU] 
                                     << 5U) | (0x10U 
                                               & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                  [vlSelfRef.__PVT__idStage__DOT__orgPickedInsnLane][9U] 
                                                  >> 0x1bU)))));
    vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U] 
        = ((0x3ff3U & vlSelfRef.__PVT__idStage__DOT__nextStage
            [1U][4U]) | (0xcU & (vlSelfRef.__PVT__idStage__DOT__nextStage
                                 [1U][1U] >> 0x14U)));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk14__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__idStage__DOT__nextStage[1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__perfCounterIF.__PVT__branchPredMissDetectedOnDecode 
        = (((IData)(vlSelfRef.__PVT__idStage__DOT__complete) 
            & (IData)(vlSelfRef.__PVT__idStage__DOT__flushTriggered)) 
           & (~ (IData)(vlSelfRef.__PVT__idStage__DOT__clear)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x10000U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [0U][9U] >> 0xeU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x17fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushed
                                     [0U] << 0xfU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x1bfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushTriggering
                                     [0U] << 0xeU)));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j = 0U;
    {
        while (VL_GTS_III(32, 3U, vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)) {
            if ((((0x1c7U >= ((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                  && (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[
                            (((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                             >> 5U)] >> (0x1fU & ((IData)(4U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))) 
                 & (0U == ((0x1c7U >= ((IData)(1U) 
                                       + (0x1ffU & 
                                          ((IData)(0x4cU) 
                                           * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))
                            ? (3U & (((0U == (0x1fU 
                                              & ((IData)(1U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))
                                       ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                               (((IData)(2U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                                                >> 5U)] 
                                               << ((IData)(0x20U) 
                                                   - 
                                                   (0x1fU 
                                                    & ((IData)(1U) 
                                                       + 
                                                       (0x1ffU 
                                                        & ((IData)(0x4cU) 
                                                           * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))) 
                                     | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        (((IData)(1U) 
                                          + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                                         >> 5U)] >> 
                                        (0x1fU & ((IData)(1U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))
                            : 0U)))) {
                if ((1U & (~ ((5U >= (7U & vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)) 
                              && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps) 
                                        >> (7U & vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))) {
                    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
                        = (0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                           [0U][2U]);
                }
                goto __Vlabel27;
            }
            vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j 
                = ((IData)(1U) + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j);
        }
        __Vlabel27: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x1c00fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [0U][0xaU] << 5U) 
                                    | (0x10U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                [0U][9U] 
                                                >> 0x1bU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = (0x1fff3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [0U][2U]);
    __Vfunc_ToAddrFromPC__513__pc = (0xfffffU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                 [0U][8U] 
                                                 >> 0xaU));
    __Vfunc_ToAddrFromPC__513__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__513__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__513__pc));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][1U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][1U]) | (__Vfunc_ToAddrFromPC__513__Vfuncout 
                         << 2U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][2U] 
        = ((0x1fffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][2U]) | (0x1ffffU & (__Vfunc_ToAddrFromPC__513__Vfuncout 
                                     >> 0x1eU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                         [0U][9U] << 4U) 
                                        | (0xcU & (
                                                   vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                   [0U][8U] 
                                                   >> 0x1cU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][1U] 
        = ((0xfffffffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][1U]) | (3U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                               [0U][9U] >> 0x1cU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [0U][0U]) | (0x3fffffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[0U] 
                                                >> 6U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [0U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[0U] 
                                           >> 6U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [0U][0U]) | (0x3ffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[2U] 
                                                >> 0x12U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [0U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[2U] 
                                           >> 0x12U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [0U][0U]) | (2U & (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[4U] 
                                                   >> 0x1eU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[0U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [0U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [0U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[4U] 
                                           >> 0x1eU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x10000U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [1U][9U] >> 0xeU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x17fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushed
                                     [1U] << 0xfU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x1bfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x1ffffU & (vlSelfRef.__PVT__idStage__DOT__insnFlushTriggering
                                     [1U] << 0xeU)));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j = 0U;
    {
        while (VL_GTS_III(32, 3U, vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)) {
            if ((((0x1c7U >= ((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * ((IData)(3U) 
                                                   + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))) 
                  && (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[
                            (((IData)(4U) + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * ((IData)(3U) 
                                                   + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                             >> 5U)] >> (0x1fU & ((IData)(4U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * 
                                                      ((IData)(3U) 
                                                       + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))) 
                 & (0U == ((0x1c7U >= ((IData)(1U) 
                                       + (0x1ffU & 
                                          ((IData)(0x4cU) 
                                           * ((IData)(3U) 
                                              + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))
                            ? (3U & (((0U == (0x1fU 
                                              & ((IData)(1U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     ((IData)(3U) 
                                                      + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))
                                       ? 0U : (vlSelfRef.__PVT__idStage__DOT__microOps[
                                               (((IData)(2U) 
                                                 + 
                                                 (0x1ffU 
                                                  & ((IData)(0x4cU) 
                                                     * 
                                                     ((IData)(3U) 
                                                      + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                                                >> 5U)] 
                                               << ((IData)(0x20U) 
                                                   - 
                                                   (0x1fU 
                                                    & ((IData)(1U) 
                                                       + 
                                                       (0x1ffU 
                                                        & ((IData)(0x4cU) 
                                                           * 
                                                           ((IData)(3U) 
                                                            + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))) 
                                     | (vlSelfRef.__PVT__idStage__DOT__microOps[
                                        (((IData)(1U) 
                                          + (0x1ffU 
                                             & ((IData)(0x4cU) 
                                                * ((IData)(3U) 
                                                   + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))) 
                                         >> 5U)] >> 
                                        (0x1fU & ((IData)(1U) 
                                                  + 
                                                  (0x1ffU 
                                                   & ((IData)(0x4cU) 
                                                      * 
                                                      ((IData)(3U) 
                                                       + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))))))))
                            : 0U)))) {
                if ((1U & (~ ((5U >= (7U & ((IData)(3U) 
                                            + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j))) 
                              && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps) 
                                        >> (7U & ((IData)(3U) 
                                                  + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j)))))))) {
                    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
                        = (0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                           [1U][2U]);
                }
                goto __Vlabel28;
            }
            vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j 
                = ((IData)(1U) + vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j);
        }
        __Vlabel28: ;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__pdStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__pdStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 2U;
        } else if (vlSelfRef.__PVT__rnStage__DOT__regFlush) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
            vlSelfRef.__PVT__controller__DOT__pdStage = 2U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x1c00fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x3ff0U & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                     [1U][0xaU] << 5U) 
                                    | (0x10U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                [1U][9U] 
                                                >> 0x1bU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = (0x1fff3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [1U][2U]);
    __Vfunc_ToAddrFromPC__513__pc = (0xfffffU & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                 [1U][8U] 
                                                 >> 0xaU));
    __Vfunc_ToAddrFromPC__513__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__513__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__513__pc));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][1U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][1U]) | (__Vfunc_ToAddrFromPC__513__Vfuncout 
                         << 2U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][2U] 
        = ((0x1fffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][2U]) | (0x1ffffU & (__Vfunc_ToAddrFromPC__513__Vfuncout 
                                     >> 0x1eU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__idStage__DOT__pipeReg
                                         [1U][9U] << 4U) 
                                        | (0xcU & (
                                                   vlSelfRef.__PVT__idStage__DOT__pipeReg
                                                   [1U][8U] 
                                                   >> 0x1cU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][1U] 
        = ((0xfffffffcU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][1U]) | (3U & (vlSelfRef.__PVT__idStage__DOT__pipeReg
                               [1U][9U] >> 0x1cU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [1U][0U]) | (0x3ffffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[7U] 
                                                >> 0xaU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [1U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[7U] 
                                           >> 0xaU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [1U][0U]) | (0x3feU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[9U] 
                                                >> 0x16U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [1U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[9U] 
                                           >> 0x16U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (2U & ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                [1U][0U]) | (0x3ffffffeU 
                                             & (vlSelfRef.__PVT__idStage__DOT__microOps[0xcU] 
                                                >> 2U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg[1U][0U] 
        = ((0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
            [1U][0U]) | (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                               [1U][0U] | (vlSelfRef.__PVT__idStage__DOT__microOps[0xcU] 
                                           >> 2U))));
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk17__DOT__j = 3U;
    vlSelfRef.__PVT__idStage__DOT__unnamedblk15__DOT__i = 2U;
    vlSelfRef.__PVT__controller__DOT__stallByDecodeStage = 0U;
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        if ((1U != (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower)))) {
                if ((1U & (~ (IData)(vlSelfRef.__PVT__rnStage__DOT__regFlush)))) {
                    if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
                        vlSelfRef.__PVT__controller__DOT__stallByDecodeStage = 1U;
                    }
                }
            }
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__stallByDecodeStage 
        = vlSelfRef.__PVT__controller__DOT__stallByDecodeStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__stallByDecodeStage 
        = vlSelfRef.__PVT__controller__DOT__stallByDecodeStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage 
        = vlSelfRef.__PVT__controller__DOT__pdStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__pdStage;
    vlSelfRef.__PVT__idStage__DOT__stallBranchResolver 
        = (IData)((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage) 
                    >> 1U) & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__stallByDecodeStage))));
    vlSelfRef.__PVT__pdStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__pdStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][9U]) | (0x40000000U & (((~ (((IData)(vlSelfRef.__PVT__pdStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__pdStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                         << 0x1eU) 
                                        & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [0U][2U] 
                                           << 9U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][8U]) | (0xc0000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [0U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U] 
        = ((0xc0000000U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][9U]) | (0x3fffffffU & ((0x3ffffe00U 
                                         & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                            [0U][2U] 
                                            << 9U)) 
                                        | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [0U][1U] 
                                           >> 0x17U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U] 
        = ((0xc00003ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][8U]) | (0x3ffffc00U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [0U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][7U] 
        = ((0x1ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][7U]) | ((IData)((0x1ffffffffULL & 
                                  (((QData)((IData)(
                                                    vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                [0U][0U]))))) 
                         << 9U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U] 
        = ((0xfffffc00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][8U]) | (((IData)((0x1ffffffffULL & 
                                   (((QData)((IData)(
                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                     [0U][1U])) 
                                     << 0x20U) | (QData)((IData)(
                                                                 vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                 [0U][0U]))))) 
                          >> 0x17U) | ((IData)(((0x1ffffffffULL 
                                                 & (((QData)((IData)(
                                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                     [0U][1U])) 
                                                     << 0x20U) 
                                                    | (QData)((IData)(
                                                                      vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                      [0U][0U])))) 
                                                >> 0x20U)) 
                                       << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0U] 
        = ((0xffffffe0U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][0U]) | (0x1fU & (IData)(vlSelfRef.__PVT__pdStage__DOT__insnInfo)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0U] 
        = ((0x1fU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][0U]) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[0U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][1U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[0U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[1U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][2U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[1U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[2U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][3U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[2U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[3U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][4U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[3U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[4U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][5U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[4U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[5U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][6U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
             & vlSelfRef.__PVT__pdStage__DOT__microOps[5U]) 
            >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                          & vlSelfRef.__PVT__pdStage__DOT__microOps[6U]) 
                         << 5U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][7U] 
        = ((0xfffffe00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][7U]) | (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                           & vlSelfRef.__PVT__pdStage__DOT__microOps[6U]) 
                          >> 0x1bU) | ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[7U] 
                                        & vlSelfRef.__PVT__pdStage__DOT__microOps[7U]) 
                                       << 5U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [0U][9U]) | (0x80000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [0U][2U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0xaU] 
        = (vlSelfRef.__PVT__pdStage__DOT__pipeReg[0U][2U] 
           >> 0x17U);
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][9U]) | (0x40000000U & (((~ (((IData)(vlSelfRef.__PVT__pdStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__pdStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                         << 0x1eU) 
                                        & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [1U][2U] 
                                           << 9U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][8U]) | (0xc0000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [1U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U] 
        = ((0xc0000000U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][9U]) | (0x3fffffffU & ((0x3ffffe00U 
                                         & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                            [1U][2U] 
                                            << 9U)) 
                                        | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                           [1U][1U] 
                                           >> 0x17U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U] 
        = ((0xc00003ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][8U]) | (0x3ffffc00U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [1U][1U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][7U] 
        = ((0x1ffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][7U]) | ((IData)((0x1ffffffffULL & 
                                  (((QData)((IData)(
                                                    vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                [1U][0U]))))) 
                         << 9U));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U] 
        = ((0xfffffc00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][8U]) | (((IData)((0x1ffffffffULL & 
                                   (((QData)((IData)(
                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                     [1U][1U])) 
                                     << 0x20U) | (QData)((IData)(
                                                                 vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                 [1U][0U]))))) 
                          >> 0x17U) | ((IData)(((0x1ffffffffULL 
                                                 & (((QData)((IData)(
                                                                     vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                     [1U][1U])) 
                                                     << 0x20U) 
                                                    | (QData)((IData)(
                                                                      vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                                      [1U][0U])))) 
                                                >> 0x20U)) 
                                       << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0U] 
        = ((0xffffffe0U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][0U]) | (0x1fU & ((IData)(vlSelfRef.__PVT__pdStage__DOT__insnInfo) 
                                  >> 5U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0U] 
        = ((0x1fU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][0U]) | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
                                         << 5U) & (
                                                   vlSelfRef.__PVT__pdStage__DOT__microOps[7U] 
                                                   << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][1U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[0U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[8U] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[7U] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[8U] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][2U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[1U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[9U] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[8U] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[9U] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][3U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[2U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xaU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[9U] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xaU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][4U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[3U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xbU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xaU] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xbU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][5U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[4U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xcU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xbU] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xcU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][6U] 
        = (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[5U] 
             >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xdU] 
                                    << 1U)) | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xcU] 
                                               >> 0x1fU))) 
           | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                              << 5U) & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xdU] 
                                        << 1U))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][7U] 
        = ((0xfffffe00U & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][7U]) | (((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[6U] 
                           >> 0x1bU) & ((0x1eU & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xeU] 
                                                  << 1U)) 
                                        | (vlSelfRef.__PVT__pdStage__DOT__microOps[0xdU] 
                                           >> 0x1fU))) 
                         | (0xffffffe0U & ((VSMT_RTL_Testbench__ConstPool__CONST_h5b979017_0[7U] 
                                            << 5U) 
                                           & (vlSelfRef.__PVT__pdStage__DOT__microOps[0xeU] 
                                              << 1U)))));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__pdStage__DOT__nextStage
            [1U][9U]) | (0x80000000U & (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                        [1U][2U] << 9U)));
    vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0xaU] 
        = (vlSelfRef.__PVT__pdStage__DOT__pipeReg[1U][2U] 
           >> 0x17U);
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][7U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][8U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][8U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][9U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][9U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[0U][0xaU] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[0U][0xaU];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][5U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][6U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][7U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][8U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][8U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][9U] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][9U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage[1U][0xaU] 
        = vlSelfRef.__PVT__pdStage__DOT__nextStage[1U][0xaU];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__3(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__ifStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__ifStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__controller__DOT__ifStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__ifStage = 2U;
        } else if (vlSelfRef.__PVT__rnStage__DOT__regFlush) {
            vlSelfRef.__PVT__controller__DOT__ifStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
            vlSelfRef.__PVT__controller__DOT__ifStage = 2U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__ifStage = 3U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage 
        = vlSelfRef.__PVT__controller__DOT__ifStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__ifStage;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__4(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__npStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__npStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__controller__DOT__npStage = 0U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__npStage = 2U;
        } else if (vlSelfRef.__PVT__rnStage__DOT__regFlush) {
            vlSelfRef.__PVT__controller__DOT__npStage = 0U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper) {
            vlSelfRef.__PVT__controller__DOT__npStage = 2U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__npStage = 3U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStageSendBubbleLowerForInterrupt) {
            vlSelfRef.__PVT__controller__DOT__npStage = 3U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStage 
        = vlSelfRef.__PVT__controller__DOT__npStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__wholePipelineEmpty 
        = ((((((IData)(vlSelfRef.__PVT__controller__DOT__npStage) 
               & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStageEmpty)) 
              & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStageEmpty)) 
             & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageEmpty)) 
            & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageEmpty)) 
           & (0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__npStage;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__5(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__rnStage = 1U;
    } else {
        vlSelfRef.__PVT__controller__DOT__rnStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__controller__DOT__rnStage = 1U;
        } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower) {
            vlSelfRef.__PVT__controller__DOT__rnStage = 3U;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage 
        = vlSelfRef.__PVT__controller__DOT__rnStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__rnStage;
    vlSelfRef.__PVT__rnStage__DOT__serializer__DOT__nextPhase = 0U;
    if ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage))) {
        vlSelfRef.__PVT__rnStage__DOT__serializer__DOT__nextPhase = 0U;
    }
    if ((0U == (IData)(vlSelfRef.__PVT__rnStage__DOT__serializer__DOT__regPhase))) {
        if ((1U & (vlSelfRef.__PVT__rnStage__DOT__opInfo[0U] 
                   & (IData)(vlSelfRef.__PVT__rnStage__DOT__valid)))) {
            vlSelfRef.__PVT__rnStage__DOT__serializer__DOT__nextPhase 
                = ((0x20000U & vlSelfRef.__PVT__rnStage__DOT__opInfo[1U])
                    ? (((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount)) 
                        | (0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount)))
                        ? 0U : 2U) : ((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount))
                                       ? 2U : 0U));
        }
    } else {
        vlSelfRef.__PVT__rnStage__DOT__serializer__DOT__nextPhase 
            = (((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount)) 
                | (0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount)))
                ? 2U : 0U);
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__6(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__6\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*1:0*/ __Vfunc_SelectOperand__559__opType;
    __Vfunc_SelectOperand__559__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__559__regV;
    __Vfunc_SelectOperand__559__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__559__immV;
    __Vfunc_SelectOperand__559__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__559__pcV;
    __Vfunc_SelectOperand__559__pcV = 0;
    CData/*1:0*/ __Vfunc_SelectOperand__560__opType;
    __Vfunc_SelectOperand__560__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__560__regV;
    __Vfunc_SelectOperand__560__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__560__immV;
    __Vfunc_SelectOperand__560__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperand__560__pcV;
    __Vfunc_SelectOperand__560__pcV = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__561__detectRange;
    __Vfunc_SelectiveFlushDetector__561__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__561__headPtr;
    __Vfunc_SelectiveFlushDetector__561__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__561__tailPtr;
    __Vfunc_SelectiveFlushDetector__561__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__561__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__561__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__561__opPtr;
    __Vfunc_SelectiveFlushDetector__561__opPtr = 0;
    // Body
    vlSelfRef.__PVT__memRrStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__memRrStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][0U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [0U][1U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [0U][0U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][1U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [0U][2U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [0U][1U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][2U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [0U][3U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [0U][2U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[0U][3U] 
        = (0x1fffffffU & ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][4U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                 [0U][3U] 
                                                 >> 5U)));
    vlSelfRef.__PVT__memRrStage__DOT__memOpInfo[0U] 
        = (0x7fffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__memRrStage__DOT__iqData
                                               [0U][3U])) 
                               << 0x16U) | ((QData)((IData)(
                                                            vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                            [0U][2U])) 
                                            >> 0xaU)));
    vlSelfRef.__PVT__memRrStage__DOT__opSrc[0U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                       [0U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                         [0U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__memRrStage__DOT__opDst[0U] = (0xffU 
                                                   & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                      [0U][0U] 
                                                      >> 0x15U));
    vlSelfRef.__PVT__memRrStage__DOT__pc[0U] = (0xfffffU 
                                                & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                   [0U][0U] 
                                                   >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memWriteReg[0U] 
        = (1U & ((vlSelfRef.__PVT__memRrStage__DOT__opDst
                  [0U] >> 7U) & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                 [0U][4U] >> 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum[0U] 
        = (0x7fU & vlSelfRef.__PVT__memRrStage__DOT__opDst
           [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [0U] >> 0x1fU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [0U] >> 0x1dU))));
    vlSelfRef.__PVT__memRrStage__DOT__immOut[0U] = 0U;
    __Vfunc_SelectOperand__559__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperand__559__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperand__559__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                               [0U]);
    __Vfunc_SelectOperand__559__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [0U] 
                                                        >> 0x1fU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__pcV;
            goto __Vlabel29;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__regV;
            goto __Vlabel29;
        }
        __Vlabel29: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandA[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout)));
    __Vfunc_SelectOperand__560__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperand__560__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperand__560__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                               [0U]);
    __Vfunc_SelectOperand__560__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [0U] 
                                                        >> 0x1dU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__pcV;
            goto __Vlabel30;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__regV;
            goto __Vlabel30;
        }
        __Vlabel30: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandB[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout)));
    vlSelfRef.__PVT__memRrStage__DOT__operandA[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [0U] 
                                                            >> 0x1fU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__operandB[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [0U] 
                                                            >> 0x1dU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][6U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][6U]) | (0xfc000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [0U][4U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][7U] 
        = (0x3fU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                    [0U][4U] >> 9U));
    __Vfunc_SelectiveFlushDetector__561__opPtr = (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__561__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__561__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__561__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__561__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__561__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__561__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                goto __Vlabel31;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel31;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel31;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel31;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel31;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel31;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                goto __Vlabel31;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
        }
        __Vlabel31: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout;
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][6U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][6U]) | (0x2000000U & (((~ ((((IData)(vlSelfRef.__PVT__memRrStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__memRrStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                            | vlSelfRef.__PVT__memRrStage__DOT__flush
                                            [0U])) 
                                        << 0x19U) & 
                                       (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [0U][4U] << 0x17U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][2U] 
        = ((0xfffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][2U]) | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [0U][0U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][3U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][1U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [0U][0U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [0U][1U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][4U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][2U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [0U][1U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [0U][2U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][5U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [0U][3U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [0U][2U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [0U][3U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][6U] 
        = ((0xfe000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][6U]) | (0x1ffffffU & ((0xf800000U 
                                        & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [0U][4U] 
                                           << 0x17U)) 
                                       | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                          [0U][3U] 
                                          >> 9U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][1U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][1U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                 [0U]) << 0x1bU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][2U] 
        = ((0xf0000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][2U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                  [0U]) >> 5U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandA
                                                           [0U] 
                                                           >> 0x20U)) 
                                                  << 0x1bU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                 [0U]) << 0x1aU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][1U] 
        = ((0xf8000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][1U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                  [0U]) >> 6U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandB
                                                           [0U] 
                                                           >> 0x20U)) 
                                                  << 0x1aU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0xfc00001fU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                         [0U] << 5U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | (1U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [0U][0U] & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [0U][4U] 
                                           >> 2U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[0U][0U] 
        = ((0xffffffe1U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [0U][0U]) | (0x1eU & vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                         [0U][0U]));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][0U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [1U][1U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [1U][0U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][1U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [1U][2U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [1U][1U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][2U] 
        = ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
            [1U][3U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                  [1U][2U] >> 5U));
    vlSelfRef.__PVT__memRrStage__DOT__iqData[1U][3U] 
        = (0x1fffffffU & ((vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][4U] << 0x1bU) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                 [1U][3U] 
                                                 >> 5U)));
    vlSelfRef.__PVT__memRrStage__DOT__memOpInfo[1U] 
        = (0x7fffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__memRrStage__DOT__iqData
                                               [1U][3U])) 
                               << 0x16U) | ((QData)((IData)(
                                                            vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                            [1U][2U])) 
                                            >> 0xaU)));
    vlSelfRef.__PVT__memRrStage__DOT__opSrc[1U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                       [1U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                         [1U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__memRrStage__DOT__opDst[1U] = (0xffU 
                                                   & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                      [1U][0U] 
                                                      >> 0x15U));
    vlSelfRef.__PVT__memRrStage__DOT__pc[1U] = (0xfffffU 
                                                & (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                   [1U][0U] 
                                                   >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__memRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memWriteReg[1U] 
        = (1U & ((vlSelfRef.__PVT__memRrStage__DOT__opDst
                  [1U] >> 7U) & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                 [1U][4U] >> 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum[1U] 
        = (0x7fU & vlSelfRef.__PVT__memRrStage__DOT__opDst
           [1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [1U] >> 0x1fU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                [1U] >> 0x1dU))));
    vlSelfRef.__PVT__memRrStage__DOT__immOut[1U] = 0U;
    __Vfunc_SelectOperand__559__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperand__559__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperand__559__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                               [1U]);
    __Vfunc_SelectOperand__559__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [1U] 
                                                        >> 0x1fU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__559__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__pcV;
            goto __Vlabel32;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout 
                = __Vfunc_SelectOperand__559__regV;
            goto __Vlabel32;
        }
        __Vlabel32: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandA[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__559__Vfuncout)));
    __Vfunc_SelectOperand__560__pcV = vlSelfRef.__PVT__memRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperand__560__immV = vlSelfRef.__PVT__memRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperand__560__regV = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                               [1U]);
    __Vfunc_SelectOperand__560__opType = (3U & (IData)(
                                                       (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                        [1U] 
                                                        >> 0x1dU)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperand__560__opType))) {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__pcV;
            goto __Vlabel33;
        } else {
            vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout 
                = __Vfunc_SelectOperand__560__regV;
            goto __Vlabel33;
        }
        __Vlabel33: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__operandB[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperand__560__Vfuncout)));
    vlSelfRef.__PVT__memRrStage__DOT__operandA[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandA
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [1U] 
                                                            >> 0x1fU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataA
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__operandB[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__memRrStage__DOT__operandB
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__memRrStage__DOT__memOpInfo
                                                            [1U] 
                                                            >> 0x1dU)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegDataB
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][6U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][6U]) | (0xfc000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [1U][4U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][7U] 
        = (0x3fU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                    [1U][4U] >> 9U));
    __Vfunc_SelectiveFlushDetector__561__opPtr = (vlSelfRef.__PVT__memRrStage__DOT__iqData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__561__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__561__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__561__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__561__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__561__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__561__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                goto __Vlabel34;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel34;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel34;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__561__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel34;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__561__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__561__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__561__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 1U;
                    goto __Vlabel34;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                    goto __Vlabel34;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
                goto __Vlabel34;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout = 0U;
        }
        __Vlabel34: ;
    }
    vlSelfRef.__PVT__memRrStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__561__Vfuncout;
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][6U] 
        = ((0xfdffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][6U]) | (0x2000000U & (((~ ((((IData)(vlSelfRef.__PVT__memRrStage__DOT__stall) 
                                              | (IData)(vlSelfRef.__PVT__memRrStage__DOT__clear)) 
                                             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                            | vlSelfRef.__PVT__memRrStage__DOT__flush
                                            [1U])) 
                                        << 0x19U) & 
                                       (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [1U][4U] << 0x17U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][2U] 
        = ((0xfffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][2U]) | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                        [1U][0U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][3U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][1U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [1U][0U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [1U][1U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][4U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][2U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [1U][1U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [1U][2U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][5U] 
        = (((0xf800000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                           [1U][3U] << 0x17U)) | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                                  [1U][2U] 
                                                  >> 9U)) 
           | (0xf0000000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                             [1U][3U] << 0x17U)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][6U] 
        = ((0xfe000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][6U]) | (0x1ffffffU & ((0xf800000U 
                                        & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [1U][4U] 
                                           << 0x17U)) 
                                       | (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                          [1U][3U] 
                                          >> 9U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][1U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][1U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                 [1U]) << 0x1bU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][2U] 
        = ((0xf0000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][2U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandA
                                  [1U]) >> 5U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandA
                                                           [1U] 
                                                           >> 0x20U)) 
                                                  << 0x1bU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0x3ffffffU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | ((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                 [1U]) << 0x1aU));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][1U] 
        = ((0xf8000000U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][1U]) | (((IData)(vlSelfRef.__PVT__memRrStage__DOT__operandB
                                  [1U]) >> 6U) | ((IData)(
                                                          (vlSelfRef.__PVT__memRrStage__DOT__operandB
                                                           [1U] 
                                                           >> 0x20U)) 
                                                  << 0x1aU)));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0xfc00001fU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                         [1U] << 5U));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | (1U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [1U][0U] & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                           [1U][4U] 
                                           >> 2U))));
    vlSelfRef.__PVT__memRrStage__DOT__nextStage[1U][0U] 
        = ((0xffffffe1U & vlSelfRef.__PVT__memRrStage__DOT__nextStage
            [1U][0U]) | (0x1eU & vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                         [1U][0U]));
    vlSelfRef.__PVT__memRrStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[0U][7U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [0U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][5U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][6U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage[1U][7U] 
        = vlSelfRef.__PVT__memRrStage__DOT__nextStage
        [1U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [0U]) | (0x2000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                [0U][4U] << 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [0U]) | (vlSelfRef.__PVT__memRrStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [0U][4U] >> 3U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [1U]) | (0x2000U & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                                [1U][4U] << 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [1U]) | (vlSelfRef.__PVT__memRrStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__memRrStage__DOT__pipeReg
                               [1U][4U] >> 3U)));
    vlSelfRef.__PVT__memRrStage__DOT__unnamedblk4__DOT__i = 2U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__7(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__7\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<3>/*92:0*/ fpRrStage__DOT____Vlvbound_hb611356e__0;
    VL_ZERO_W(93, fpRrStage__DOT____Vlvbound_hb611356e__0);
    IData/*16:0*/ fpRrStage__DOT____Vlvbound_h185250e4__0;
    fpRrStage__DOT____Vlvbound_h185250e4__0 = 0;
    IData/*20:0*/ fpRrStage__DOT____Vlvbound_h79f92e5a__0;
    fpRrStage__DOT____Vlvbound_h79f92e5a__0 = 0;
    CData/*7:0*/ fpRrStage__DOT____Vlvbound_h9435fd27__0;
    fpRrStage__DOT____Vlvbound_h9435fd27__0 = 0;
    CData/*6:0*/ fpRrStage__DOT____Vlvbound_h12567129__0;
    fpRrStage__DOT____Vlvbound_h12567129__0 = 0;
    CData/*6:0*/ fpRrStage__DOT____Vlvbound_h81a2ccdc__0;
    fpRrStage__DOT____Vlvbound_h81a2ccdc__0 = 0;
    CData/*6:0*/ fpRrStage__DOT____Vlvbound_h92f189df__0;
    fpRrStage__DOT____Vlvbound_h92f189df__0 = 0;
    CData/*6:0*/ fpRrStage__DOT____Vlvbound_h567d88d1__0;
    fpRrStage__DOT____Vlvbound_h567d88d1__0 = 0;
    CData/*6:0*/ fpRrStage__DOT____Vlvbound_hb478dda8__0;
    fpRrStage__DOT____Vlvbound_hb478dda8__0 = 0;
    CData/*6:0*/ fpRrStage__DOT____Vlvbound_he6a2db1e__0;
    fpRrStage__DOT____Vlvbound_he6a2db1e__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_h796f373a__0;
    fpRrStage__DOT____Vlvbound_h796f373a__0 = 0;
    CData/*6:0*/ fpRrStage__DOT____Vlvbound_hc44258c7__0;
    fpRrStage__DOT____Vlvbound_hc44258c7__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_h6daff5ed__0;
    fpRrStage__DOT____Vlvbound_h6daff5ed__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_h5b677635__0;
    fpRrStage__DOT____Vlvbound_h5b677635__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_hd54d22a7__0;
    fpRrStage__DOT____Vlvbound_hd54d22a7__0 = 0;
    QData/*32:0*/ fpRrStage__DOT____Vlvbound_hde206962__0;
    fpRrStage__DOT____Vlvbound_hde206962__0 = 0;
    QData/*32:0*/ fpRrStage__DOT____Vlvbound_h078a34ed__0;
    fpRrStage__DOT____Vlvbound_h078a34ed__0 = 0;
    QData/*32:0*/ fpRrStage__DOT____Vlvbound_hf0e1184e__0;
    fpRrStage__DOT____Vlvbound_hf0e1184e__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_ha7225fd9__0;
    fpRrStage__DOT____Vlvbound_ha7225fd9__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_h53b5b0e7__0;
    fpRrStage__DOT____Vlvbound_h53b5b0e7__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_ha4222492__0;
    fpRrStage__DOT____Vlvbound_ha4222492__0 = 0;
    SData/*11:0*/ fpRrStage__DOT____Vlvbound_hf208bb28__0;
    fpRrStage__DOT____Vlvbound_hf208bb28__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_h046b483c__0;
    fpRrStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_hc32aa449__0;
    fpRrStage__DOT____Vlvbound_hc32aa449__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_hc328557a__0;
    fpRrStage__DOT____Vlvbound_hc328557a__0 = 0;
    QData/*32:0*/ fpRrStage__DOT____Vlvbound_hf5b92239__0;
    fpRrStage__DOT____Vlvbound_hf5b92239__0 = 0;
    QData/*32:0*/ fpRrStage__DOT____Vlvbound_hf5c3e4e2__0;
    fpRrStage__DOT____Vlvbound_hf5c3e4e2__0 = 0;
    QData/*32:0*/ fpRrStage__DOT____Vlvbound_hf5d1c31f__0;
    fpRrStage__DOT____Vlvbound_hf5d1c31f__0 = 0;
    VlWide<3>/*92:0*/ fpRrStage__DOT____Vlvbound_hd6eeb60a__0;
    VL_ZERO_W(93, fpRrStage__DOT____Vlvbound_hd6eeb60a__0);
    IData/*20:0*/ fpRrStage__DOT____Vlvbound_hd89e9e70__0;
    fpRrStage__DOT____Vlvbound_hd89e9e70__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_h74f50670__0;
    fpRrStage__DOT____Vlvbound_h74f50670__0 = 0;
    CData/*0:0*/ fpRrStage__DOT____Vlvbound_h74f5373d__0;
    fpRrStage__DOT____Vlvbound_h74f5373d__0 = 0;
    SData/*11:0*/ fpRrStage__DOT____Vlvbound_h59ff9539__0;
    fpRrStage__DOT____Vlvbound_h59ff9539__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__621__detectRange;
    __Vfunc_SelectiveFlushDetector__621__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__621__headPtr;
    __Vfunc_SelectiveFlushDetector__621__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__621__tailPtr;
    __Vfunc_SelectiveFlushDetector__621__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__621__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__621__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__621__opPtr;
    __Vfunc_SelectiveFlushDetector__621__opPtr = 0;
    // Body
    vlSelfRef.__PVT__fpRrStage__DOT__stall = (1U & 
                                              ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                               >> 1U));
    vlSelfRef.__PVT__fpRrStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    fpRrStage__DOT____Vlvbound_hb611356e__0[0U] = vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
        [0U][0U];
    fpRrStage__DOT____Vlvbound_hb611356e__0[1U] = vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
        [0U][1U];
    fpRrStage__DOT____Vlvbound_hb611356e__0[2U] = (0x1fffffffU 
                                                   & vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                   [0U][2U]);
    vlSelfRef.__PVT__fpRrStage__DOT__iqData[0U][0U] 
        = fpRrStage__DOT____Vlvbound_hb611356e__0[0U];
    vlSelfRef.__PVT__fpRrStage__DOT__iqData[0U][1U] 
        = fpRrStage__DOT____Vlvbound_hb611356e__0[1U];
    vlSelfRef.__PVT__fpRrStage__DOT__iqData[0U][2U] 
        = fpRrStage__DOT____Vlvbound_hb611356e__0[2U];
    fpRrStage__DOT____Vlvbound_h185250e4__0 = (0x1ffffU 
                                               & vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                               [0U][2U]);
    vlSelfRef.__PVT__fpRrStage__DOT__fpOpInfo[0U] = fpRrStage__DOT____Vlvbound_h185250e4__0;
    fpRrStage__DOT____Vlvbound_h79f92e5a__0 = (0x1fffffU 
                                               & ((vlSelfRef.__PVT__fpRrStage__DOT__iqData
                                                   [0U][1U] 
                                                   << 3U) 
                                                  | (vlSelfRef.__PVT__fpRrStage__DOT__iqData
                                                     [0U][0U] 
                                                     >> 0x1dU)));
    vlSelfRef.__PVT__fpRrStage__DOT__opSrc[0U] = fpRrStage__DOT____Vlvbound_h79f92e5a__0;
    fpRrStage__DOT____Vlvbound_h9435fd27__0 = (0xffU 
                                               & (vlSelfRef.__PVT__fpRrStage__DOT__iqData
                                                  [0U][0U] 
                                                  >> 0x15U));
    vlSelfRef.__PVT__fpRrStage__DOT__opDst[0U] = fpRrStage__DOT____Vlvbound_h9435fd27__0;
    fpRrStage__DOT____Vlvbound_h12567129__0 = (0x7fU 
                                               & (vlSelfRef.__PVT__fpRrStage__DOT__opSrc
                                                  [0U] 
                                                  >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumA[0U] 
        = fpRrStage__DOT____Vlvbound_h12567129__0;
    fpRrStage__DOT____Vlvbound_h81a2ccdc__0 = (0x7fU 
                                               & (vlSelfRef.__PVT__fpRrStage__DOT__opSrc
                                                  [0U] 
                                                  >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumB[0U] 
        = fpRrStage__DOT____Vlvbound_h81a2ccdc__0;
    fpRrStage__DOT____Vlvbound_h92f189df__0 = (0x7fU 
                                               & vlSelfRef.__PVT__fpRrStage__DOT__opSrc
                                               [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumC[0U] 
        = fpRrStage__DOT____Vlvbound_h92f189df__0;
    fpRrStage__DOT____Vlvbound_h567d88d1__0 = (0x7fU 
                                               & (vlSelfRef.__PVT__fpRrStage__DOT__opSrc
                                                  [0U] 
                                                  >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumA[0U] 
        = fpRrStage__DOT____Vlvbound_h567d88d1__0;
    fpRrStage__DOT____Vlvbound_hb478dda8__0 = (0x7fU 
                                               & (vlSelfRef.__PVT__fpRrStage__DOT__opSrc
                                                  [0U] 
                                                  >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumB[0U] 
        = fpRrStage__DOT____Vlvbound_hb478dda8__0;
    fpRrStage__DOT____Vlvbound_he6a2db1e__0 = (0x7fU 
                                               & vlSelfRef.__PVT__fpRrStage__DOT__opSrc
                                               [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumC[0U] 
        = fpRrStage__DOT____Vlvbound_he6a2db1e__0;
    fpRrStage__DOT____Vlvbound_h796f373a__0 = (1U & 
                                               ((vlSelfRef.__PVT__fpRrStage__DOT__opDst
                                                 [0U] 
                                                 >> 7U) 
                                                & (vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                   [0U][2U] 
                                                   >> 0x1eU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpWriteReg[0U] 
        = fpRrStage__DOT____Vlvbound_h796f373a__0;
    fpRrStage__DOT____Vlvbound_hc44258c7__0 = (0x7fU 
                                               & vlSelfRef.__PVT__fpRrStage__DOT__opDst
                                               [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhyDstRegNum[0U] 
        = fpRrStage__DOT____Vlvbound_hc44258c7__0;
    fpRrStage__DOT____Vlvbound_h6daff5ed__0 = (0U == 
                                               (3U 
                                                & (vlSelfRef.__PVT__fpRrStage__DOT__fpOpInfo
                                                   [0U] 
                                                   >> 4U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegA[0U] 
        = fpRrStage__DOT____Vlvbound_h6daff5ed__0;
    fpRrStage__DOT____Vlvbound_h5b677635__0 = (0U == 
                                               (3U 
                                                & (vlSelfRef.__PVT__fpRrStage__DOT__fpOpInfo
                                                   [0U] 
                                                   >> 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegB[0U] 
        = fpRrStage__DOT____Vlvbound_h5b677635__0;
    fpRrStage__DOT____Vlvbound_hd54d22a7__0 = (0U == 
                                               (3U 
                                                & vlSelfRef.__PVT__fpRrStage__DOT__fpOpInfo
                                                [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegC[0U] 
        = fpRrStage__DOT____Vlvbound_hd54d22a7__0;
    fpRrStage__DOT____Vlvbound_hde206962__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataA
        [0U];
    vlSelfRef.__PVT__fpRrStage__DOT__operandA[0U] = fpRrStage__DOT____Vlvbound_hde206962__0;
    fpRrStage__DOT____Vlvbound_h078a34ed__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataB
        [0U];
    vlSelfRef.__PVT__fpRrStage__DOT__operandB[0U] = fpRrStage__DOT____Vlvbound_h078a34ed__0;
    fpRrStage__DOT____Vlvbound_hf0e1184e__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataC
        [0U];
    vlSelfRef.__PVT__fpRrStage__DOT__operandC[0U] = fpRrStage__DOT____Vlvbound_hf0e1184e__0;
    fpRrStage__DOT____Vlvbound_ha7225fd9__0 = (1U & 
                                               ((0U 
                                                 != 
                                                 (3U 
                                                  & (vlSelfRef.__PVT__fpRrStage__DOT__fpOpInfo
                                                     [0U] 
                                                     >> 4U))) 
                                                | (IData)(
                                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataA
                                                           [0U] 
                                                           >> 0x20U))));
    vlSelfRef.__PVT__fpRrStage__DOT__operandA[0U] = 
        ((0xffffffffULL & vlSelfRef.__PVT__fpRrStage__DOT__operandA
          [0U]) | ((QData)((IData)(fpRrStage__DOT____Vlvbound_ha7225fd9__0)) 
                   << 0x20U));
    fpRrStage__DOT____Vlvbound_h53b5b0e7__0 = (1U & 
                                               ((0U 
                                                 != 
                                                 (3U 
                                                  & (vlSelfRef.__PVT__fpRrStage__DOT__fpOpInfo
                                                     [0U] 
                                                     >> 2U))) 
                                                | (IData)(
                                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataB
                                                           [0U] 
                                                           >> 0x20U))));
    vlSelfRef.__PVT__fpRrStage__DOT__operandB[0U] = 
        ((0xffffffffULL & vlSelfRef.__PVT__fpRrStage__DOT__operandB
          [0U]) | ((QData)((IData)(fpRrStage__DOT____Vlvbound_h53b5b0e7__0)) 
                   << 0x20U));
    fpRrStage__DOT____Vlvbound_ha4222492__0 = (1U & 
                                               ((0U 
                                                 != 
                                                 (3U 
                                                  & vlSelfRef.__PVT__fpRrStage__DOT__fpOpInfo
                                                  [0U])) 
                                                | (IData)(
                                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegDataC
                                                           [0U] 
                                                           >> 0x20U))));
    vlSelfRef.__PVT__fpRrStage__DOT__operandC[0U] = 
        ((0xffffffffULL & vlSelfRef.__PVT__fpRrStage__DOT__operandC
          [0U]) | ((QData)((IData)(fpRrStage__DOT____Vlvbound_ha4222492__0)) 
                   << 0x20U));
    fpRrStage__DOT____Vlvbound_hf208bb28__0 = (0xfffU 
                                               & ((vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                   [0U][3U] 
                                                   << 1U) 
                                                  | (vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                     [0U][2U] 
                                                     >> 0x1fU)));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][6U] 
        = ((0xffffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][6U]) | ((IData)(fpRrStage__DOT____Vlvbound_hf208bb28__0) 
                         << 0x18U));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][7U] 
        = (0xfU & ((IData)(fpRrStage__DOT____Vlvbound_hf208bb28__0) 
                   >> 8U));
    __Vfunc_SelectiveFlushDetector__621__opPtr = (vlSelfRef.__PVT__fpRrStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__621__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__621__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__621__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__621__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__621__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__621__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 1U;
                goto __Vlabel35;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__621__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__621__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__621__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__621__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__621__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__621__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__621__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 1U;
                    goto __Vlabel35;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 0U;
                    goto __Vlabel35;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__621__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__621__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__621__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__621__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__621__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__621__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__621__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 1U;
                    goto __Vlabel35;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__621__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__621__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__621__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__621__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 1U;
                    goto __Vlabel35;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 0U;
                    goto __Vlabel35;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 0U;
                goto __Vlabel35;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout = 0U;
        }
        __Vlabel35: ;
    }
    fpRrStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__621__Vfuncout;
    vlSelfRef.__PVT__fpRrStage__DOT__flush[0U] = fpRrStage__DOT____Vlvbound_h046b483c__0;
    fpRrStage__DOT____Vlvbound_hc32aa449__0 = (1U & 
                                               ((~ 
                                                 ((((IData)(vlSelfRef.__PVT__fpRrStage__DOT__stall) 
                                                    | (IData)(vlSelfRef.__PVT__fpRrStage__DOT__clear)) 
                                                   | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                  | vlSelfRef.__PVT__fpRrStage__DOT__flush
                                                  [0U])) 
                                                & (vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                   [0U][2U] 
                                                   >> 0x1eU)));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][6U] 
        = ((0xff7fffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][6U]) | ((IData)(fpRrStage__DOT____Vlvbound_hc32aa449__0) 
                         << 0x17U));
    fpRrStage__DOT____Vlvbound_hc328557a__0 = (1U & 
                                               (vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                [0U][2U] 
                                                >> 0x1dU));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][6U] 
        = ((0xffbfffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][6U]) | ((IData)(fpRrStage__DOT____Vlvbound_hc328557a__0) 
                         << 0x16U));
    if (((2U == (7U & (vlSelfRef.__PVT__fpRrStage__DOT__iqData
                       [0U][2U] >> 0xeU))) | (3U == 
                                              (7U & 
                                               (vlSelfRef.__PVT__fpRrStage__DOT__iqData
                                                [0U][2U] 
                                                >> 0xeU))))) {
        vlSelfRef.fpRrStage__DOT____Vlvbound_hc328d351__0 
            = ((vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                [0U][2U] >> 0x1eU) & vlSelfRef.__PVT__fpRrStage__DOT__flush
               [0U]);
        vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][6U] 
            = ((0xffdfffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
                [0U][6U]) | ((IData)(vlSelfRef.fpRrStage__DOT____Vlvbound_hc328d351__0) 
                             << 0x15U));
    } else {
        vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][6U] 
            = (0xffdfffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
               [0U][6U]);
    }
    fpRrStage__DOT____Vlvbound_hf5b92239__0 = vlSelfRef.__PVT__fpRrStage__DOT__operandA
        [0U];
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][2U] 
        = ((0x7fffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][2U]) | ((IData)(fpRrStage__DOT____Vlvbound_hf5b92239__0) 
                         << 0x17U));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][3U] 
        = ((0xff000000U & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][3U]) | (((IData)(fpRrStage__DOT____Vlvbound_hf5b92239__0) 
                          >> 9U) | ((IData)((fpRrStage__DOT____Vlvbound_hf5b92239__0 
                                             >> 0x20U)) 
                                    << 0x17U)));
    fpRrStage__DOT____Vlvbound_hf5c3e4e2__0 = vlSelfRef.__PVT__fpRrStage__DOT__operandB
        [0U];
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][1U] 
        = ((0x3fffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][1U]) | ((IData)(fpRrStage__DOT____Vlvbound_hf5c3e4e2__0) 
                         << 0x16U));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][2U] 
        = ((0xff800000U & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][2U]) | (((IData)(fpRrStage__DOT____Vlvbound_hf5c3e4e2__0) 
                          >> 0xaU) | ((IData)((fpRrStage__DOT____Vlvbound_hf5c3e4e2__0 
                                               >> 0x20U)) 
                                      << 0x16U)));
    fpRrStage__DOT____Vlvbound_hf5d1c31f__0 = vlSelfRef.__PVT__fpRrStage__DOT__operandC
        [0U];
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][0U] 
        = ((0x1fffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][0U]) | ((IData)(fpRrStage__DOT____Vlvbound_hf5d1c31f__0) 
                         << 0x15U));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][1U] 
        = ((0xffc00000U & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][1U]) | (((IData)(fpRrStage__DOT____Vlvbound_hf5d1c31f__0) 
                          >> 0xbU) | ((IData)((fpRrStage__DOT____Vlvbound_hf5d1c31f__0 
                                               >> 0x20U)) 
                                      << 0x15U)));
    fpRrStage__DOT____Vlvbound_hd6eeb60a__0[0U] = vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
        [0U][0U];
    fpRrStage__DOT____Vlvbound_hd6eeb60a__0[1U] = vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
        [0U][1U];
    fpRrStage__DOT____Vlvbound_hd6eeb60a__0[2U] = (0x1fffffffU 
                                                   & vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                   [0U][2U]);
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][3U] 
        = ((0xffffffU & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][3U]) | (fpRrStage__DOT____Vlvbound_hd6eeb60a__0[0U] 
                         << 0x18U));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][4U] 
        = ((fpRrStage__DOT____Vlvbound_hd6eeb60a__0[0U] 
            >> 8U) | (fpRrStage__DOT____Vlvbound_hd6eeb60a__0[1U] 
                      << 0x18U));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][5U] 
        = ((fpRrStage__DOT____Vlvbound_hd6eeb60a__0[1U] 
            >> 8U) | (fpRrStage__DOT____Vlvbound_hd6eeb60a__0[2U] 
                      << 0x18U));
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][6U] 
        = ((0xffe00000U & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][6U]) | (fpRrStage__DOT____Vlvbound_hd6eeb60a__0[2U] 
                         >> 8U));
    fpRrStage__DOT____Vlvbound_hd89e9e70__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
        [0U];
    vlSelfRef.__PVT__fpRrStage__DOT__nextStage[0U][0U] 
        = ((0xffe00000U & vlSelfRef.__PVT__fpRrStage__DOT__nextStage
            [0U][0U]) | fpRrStage__DOT____Vlvbound_hd89e9e70__0);
    vlSelfRef.__PVT__fpRrStage__DOT__unnamedblk3__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage[0U][7U] 
        = vlSelfRef.__PVT__fpRrStage__DOT__nextStage
        [0U][7U];
    fpRrStage__DOT____Vlvbound_h74f50670__0 = (1U & 
                                               (vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                [0U][2U] 
                                                >> 0x1eU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
            [0U]) | ((IData)(fpRrStage__DOT____Vlvbound_h74f50670__0) 
                     << 0xdU));
    fpRrStage__DOT____Vlvbound_h74f5373d__0 = vlSelfRef.__PVT__fpRrStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
            [0U]) | ((IData)(fpRrStage__DOT____Vlvbound_h74f5373d__0) 
                     << 0xcU));
    fpRrStage__DOT____Vlvbound_h59ff9539__0 = (0xfffU 
                                               & ((vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                   [0U][3U] 
                                                   << 1U) 
                                                  | (vlSelfRef.__PVT__fpRrStage__DOT__pipeReg
                                                     [0U][2U] 
                                                     >> 0x1fU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
            [0U]) | (IData)(fpRrStage__DOT____Vlvbound_h59ff9539__0));
    vlSelfRef.__PVT__fpRrStage__DOT__unnamedblk4__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__8(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__8\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rnStage__DOT__update[0U] = ((IData)(vlSelfRef.__PVT__rnStage__DOT__valid) 
                                                 & (0U 
                                                    == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage)));
    vlSelfRef.__PVT__rnStage__DOT__update[1U] = (1U 
                                                 & (IData)(
                                                           (((IData)(vlSelfRef.__PVT__rnStage__DOT__valid) 
                                                             >> 1U) 
                                                            & (0U 
                                                               == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage)))));
    vlSelfRef.__PVT__rnStage__DOT__isLoad[0U] = (IData)(
                                                        (0x100U 
                                                         == 
                                                         (0x1f0U 
                                                          & vlSelfRef.__PVT__rnStage__DOT__opInfo[2U])));
    vlSelfRef.__PVT__rnStage__DOT__isStore[0U] = (IData)(
                                                         (0x110U 
                                                          == 
                                                          (0x1f0U 
                                                           & vlSelfRef.__PVT__rnStage__DOT__opInfo[2U])));
    vlSelfRef.__PVT__rnStage__DOT__isBranch[0U] = (IData)(
                                                          ((0U 
                                                            == 
                                                            (0x180U 
                                                             & vlSelfRef.__PVT__rnStage__DOT__opInfo[2U])) 
                                                           & ((2U 
                                                               == 
                                                               (7U 
                                                                & (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                                                   >> 4U))) 
                                                              | (3U 
                                                                 == 
                                                                 (7U 
                                                                  & (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                                                     >> 4U))))));
    vlSelfRef.__PVT__rnStage__DOT__isLoad[1U] = (IData)(
                                                        (0x100000U 
                                                         == 
                                                         (0x1f0000U 
                                                          & vlSelfRef.__PVT__rnStage__DOT__opInfo[4U])));
    vlSelfRef.__PVT__rnStage__DOT__isStore[1U] = (IData)(
                                                         (0x110000U 
                                                          == 
                                                          (0x1f0000U 
                                                           & vlSelfRef.__PVT__rnStage__DOT__opInfo[4U])));
    vlSelfRef.__PVT__rnStage__DOT__isBranch[1U] = (IData)(
                                                          ((0U 
                                                            == 
                                                            (0x180000U 
                                                             & vlSelfRef.__PVT__rnStage__DOT__opInfo[4U])) 
                                                           & ((2U 
                                                               == 
                                                               (7U 
                                                                & (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                                                   >> 0x10U))) 
                                                              | (3U 
                                                                 == 
                                                                 (7U 
                                                                  & (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                                                     >> 0x10U))))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT 
        = ((2U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT)) 
           | vlSelfRef.__PVT__rnStage__DOT__update[0U]);
    if (vlSelfRef.__PVT__rnStage__DOT__isBranch[0U]) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA[0U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                         << 8U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                   >> 0x18U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB[0U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                         << 0xeU) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0x12U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg[0U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                         << 2U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                   >> 0x1eU)));
    } else {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA[0U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                         << 8U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                   >> 0x18U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB[0U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                         << 0xeU) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0x12U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg[0U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                         << 2U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                   >> 0x1eU)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteRegFromPipeReg 
        = ((2U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteRegFromPipeReg)) 
           | ((vlSelfRef.__PVT__rnStage__DOT__opInfo[0U] 
               >> 8U) & vlSelfRef.__PVT__rnStage__DOT__update
              [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT 
        = ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT)) 
           | (vlSelfRef.__PVT__rnStage__DOT__update
              [1U] << 1U));
    if (vlSelfRef.__PVT__rnStage__DOT__isBranch[1U]) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA[1U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                         << 0x1cU) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                      >> 4U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB[1U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                         << 2U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[3U] 
                                   >> 0x1eU)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg[1U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                         << 0x16U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                      >> 0xaU)));
    } else {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA[1U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                         << 0x1cU) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                      >> 4U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB[1U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                         << 2U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[3U] 
                                   >> 0x1eU)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg[1U] 
            = (0x3fU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                         << 0x16U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                      >> 0xaU)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteRegFromPipeReg 
        = ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteRegFromPipeReg)) 
           | (0x1ffeU & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                          >> 0x13U) & (vlSelfRef.__PVT__rnStage__DOT__update
                                       [1U] << 1U))));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][2U] 
        = ((0xfffffe03U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA
                         [0U] << 2U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][1U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                         [0U] << 0x1bU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][2U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                         [0U] >> 5U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][1U] 
        = ((0xf80fffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC
                         [0U] << 0x14U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][1U] 
        = ((0xfff01fffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg
                         [0U] << 0xdU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][1U] 
        = ((0xffffe03fU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg
                         [0U] << 6U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][2U] 
        = ((0xfffffe03U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA
                         [1U] << 2U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][1U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                         [1U] << 0x1bU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][2U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                         [1U] >> 5U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][1U] 
        = ((0xf80fffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC
                         [1U] << 0x14U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][1U] 
        = ((0xfff01fffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg
                         [1U] << 0xdU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][1U] 
        = ((0xffffe03fU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg
                         [1U] << 6U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][1U] 
        = ((0xffffffc3U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegA
                         [0U] << 2U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB
                         [0U] << 0x1eU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][1U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB
                         [0U] >> 2U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegC
                         [0U] << 0x1aU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][1U] 
        = ((0xffffffc3U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegA
                         [1U] << 2U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB
                         [1U] << 0x1eU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][1U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB
                         [1U] >> 2U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegC
                         [1U] << 0x1aU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail[0U] 
        = vlSelfRef.__PVT__rnStage__DOT__update[0U];
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ffffffffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (0xfffU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                         [0U][4U] 
                                                                         >> 2U)))) 
                                                     << 0x33U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ff800007fffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (0xfffffU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                         [0U][1U] 
                                                                         >> 1U)))) 
                                                     << 0x1fU));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ffffffffffff80fULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (0x7fU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__nextStage
                                                                         [0U][1U] 
                                                                         >> 6U)))) 
                                                     << 4U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ffffffffffc07ffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (0x7fU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__nextStage
                                                                         [0U][1U] 
                                                                         >> 0xdU)))) 
                                                     << 0xbU));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7fffffff81ffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (0x3fU 
                                                                      & ((vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                                                          << 2U) 
                                                                         | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                                                            >> 0x1eU))))) 
                                                     << 0x19U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ffffffffeffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (1U 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__opInfo[0U] 
                                                                         >> 8U)))) 
                                                     << 0x18U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7fffffffff7fffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__isLoad
                                                                     [0U])) 
                                                     << 0x17U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7fffffffffbfffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__isStore
                                                                     [0U])) 
                                                     << 0x16U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7fffffffffdfffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__isBranch
                                                                     [0U])) 
                                                     << 0x15U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7fffffffffefffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (1U 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                                                                         >> 0xeU)))) 
                                                     << 0x14U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ffffffffffbffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)((IData)(
                                                                             (0U 
                                                                              != 
                                                                              (0xc0U 
                                                                               & vlSelfRef.__PVT__rnStage__DOT__opInfo[0U]))))) 
                                                     << 0x12U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ffffffffff7ffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | ((QData)((IData)(
                                                                     (1U 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__opInfo[0U] 
                                                                         >> 3U)))) 
                                                     << 0x13U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[0U] = ((0x7ffffffffffffff0ULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [0U]) 
                                                  | (IData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__prevDependIssueQueuePtr
                                                                    [0U])));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0xffc0ffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr
                         [0U] << 0x10U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail[1U] 
        = vlSelfRef.__PVT__rnStage__DOT__update[1U];
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ffffffffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (0xfffU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                         [1U][4U] 
                                                                         >> 2U)))) 
                                                     << 0x33U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ff800007fffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (0xfffffU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                         [1U][1U] 
                                                                         >> 1U)))) 
                                                     << 0x1fU));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ffffffffffff80fULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (0x7fU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__nextStage
                                                                         [1U][1U] 
                                                                         >> 6U)))) 
                                                     << 4U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ffffffffffc07ffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (0x7fU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__nextStage
                                                                         [1U][1U] 
                                                                         >> 0xdU)))) 
                                                     << 0xbU));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7fffffff81ffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (0x3fU 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                                                         >> 0xaU)))) 
                                                     << 0x19U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ffffffffeffffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (1U 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                                                         >> 0x14U)))) 
                                                     << 0x18U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7fffffffff7fffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__isLoad
                                                                     [1U])) 
                                                     << 0x17U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7fffffffffbfffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__isStore
                                                                     [1U])) 
                                                     << 0x16U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7fffffffffdfffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__isBranch
                                                                     [1U])) 
                                                     << 0x15U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7fffffffffefffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (1U 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__opInfo[3U] 
                                                                         >> 0x1aU)))) 
                                                     << 0x14U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ffffffffffbffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)((IData)(
                                                                             (0U 
                                                                              != 
                                                                              (0xc0000U 
                                                                               & vlSelfRef.__PVT__rnStage__DOT__opInfo[2U]))))) 
                                                     << 0x12U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ffffffffff7ffffULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | ((QData)((IData)(
                                                                     (1U 
                                                                      & (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                                                         >> 0xfU)))) 
                                                     << 0x13U));
    vlSelfRef.__PVT__rnStage__DOT__alEntry[1U] = ((0x7ffffffffffffff0ULL 
                                                   & vlSelfRef.__PVT__rnStage__DOT__alEntry
                                                   [1U]) 
                                                  | (IData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__prevDependIssueQueuePtr
                                                                    [1U])));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0xffc0ffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr
                         [1U] << 0x10U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData[0U] 
        = vlSelfRef.__PVT__rnStage__DOT__alEntry[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData[1U] 
        = vlSelfRef.__PVT__rnStage__DOT__alEntry[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocate[0U] 
        = vlSelfRef.__PVT__rnStage__DOT__update[0U];
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocatedPtr
                         [0U] << 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocate[1U] 
        = vlSelfRef.__PVT__rnStage__DOT__update[1U];
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocatedPtr
                         [1U] << 0x16U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateLoadQueue[0U] 
        = (vlSelfRef.__PVT__rnStage__DOT__update[0U] 
           & vlSelfRef.__PVT__rnStage__DOT__isLoad[0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue[0U] 
        = (vlSelfRef.__PVT__rnStage__DOT__update[0U] 
           & vlSelfRef.__PVT__rnStage__DOT__isStore
           [0U]);
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0xffff0fffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr
                         [0U] << 0xcU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0xfffff0ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr
                         [0U] << 8U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateLoadQueue[1U] 
        = (vlSelfRef.__PVT__rnStage__DOT__update[1U] 
           & vlSelfRef.__PVT__rnStage__DOT__isLoad[1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue[1U] 
        = (vlSelfRef.__PVT__rnStage__DOT__update[1U] 
           & vlSelfRef.__PVT__rnStage__DOT__isStore
           [1U]);
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0xffff0fffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr
                         [1U] << 0xcU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0xfffff0ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr
                         [1U] << 8U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][6U] 
        = ((0x7ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][6U]) | (0x7ff800U & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                      [0U][4U] << 9U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][6U] 
        = ((0x7ffbffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][6U]) | (0x400U & (((~ ((IData)((0U 
                                                 != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage))) 
                                        | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                    & (IData)(vlSelfRef.__PVT__rnStage__DOT__valid)) 
                                   << 0xaU)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][3U] 
        = ((0xc00003ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][3U]) | (0x3ffffc00U & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                        [0U][1U] << 9U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][2U] 
        = ((0x1ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][2U]) | ((IData)((0x1ffffffffULL & 
                                  (((QData)((IData)(
                                                    vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                [0U][0U]))))) 
                         << 9U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][3U] 
        = ((0xfffffc00U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][3U]) | (((IData)((0x1ffffffffULL & 
                                   (((QData)((IData)(
                                                     vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                     [0U][1U])) 
                                     << 0x20U) | (QData)((IData)(
                                                                 vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                 [0U][0U]))))) 
                          >> 0x17U) | ((IData)(((0x1ffffffffULL 
                                                 & (((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                     [0U][1U])) 
                                                     << 0x20U) 
                                                    | (QData)((IData)(
                                                                      vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                      [0U][0U])))) 
                                                >> 0x20U)) 
                                       << 9U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][3U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][3U]) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[0U] 
                         << 0x1eU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][4U] 
        = ((vlSelfRef.__PVT__rnStage__DOT__opInfo[0U] 
            >> 2U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
                      << 0x1eU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][5U] 
        = ((vlSelfRef.__PVT__rnStage__DOT__opInfo[1U] 
            >> 2U) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                      << 0x1eU));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][6U] 
        = ((0x7ffc00U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][6U]) | (0x3ffU & (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                   >> 2U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0xffffff0fU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr
                         [0U] << 4U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U] 
        = ((0xfffffff0U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr
           [0U]);
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][6U] 
        = ((0x7ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][6U]) | (0x7ff800U & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                      [1U][4U] << 9U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][6U] 
        = ((0x7ffbffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][6U]) | (0x7fffffU & ((IData)((((IData)(vlSelfRef.__PVT__rnStage__DOT__valid) 
                                                >> 1U) 
                                               & (~ 
                                                  ((IData)(
                                                           (0U 
                                                            != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage))) 
                                                   | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))))) 
                                      << 0xaU)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][3U] 
        = ((0xc00003ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][3U]) | (0x3ffffc00U & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                        [1U][1U] << 9U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][2U] 
        = ((0x1ffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][2U]) | ((IData)((0x1ffffffffULL & 
                                  (((QData)((IData)(
                                                    vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                [1U][0U]))))) 
                         << 9U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][3U] 
        = ((0xfffffc00U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][3U]) | (((IData)((0x1ffffffffULL & 
                                   (((QData)((IData)(
                                                     vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                     [1U][1U])) 
                                     << 0x20U) | (QData)((IData)(
                                                                 vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                 [1U][0U]))))) 
                          >> 0x17U) | ((IData)(((0x1ffffffffULL 
                                                 & (((QData)((IData)(
                                                                     vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                     [1U][1U])) 
                                                     << 0x20U) 
                                                    | (QData)((IData)(
                                                                      vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                                      [1U][0U])))) 
                                                >> 0x20U)) 
                                       << 9U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][3U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][3U]) | (0xc0000000U & (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                        << 0x12U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][4U] 
        = (((0x3ffc0000U & (vlSelfRef.__PVT__rnStage__DOT__opInfo[3U] 
                            << 0x12U)) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[2U] 
                                          >> 0xeU)) 
           | (0xc0000000U & (vlSelfRef.__PVT__rnStage__DOT__opInfo[3U] 
                             << 0x12U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][5U] 
        = (((0x3ffc0000U & (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                            << 0x12U)) | (vlSelfRef.__PVT__rnStage__DOT__opInfo[3U] 
                                          >> 0xeU)) 
           | (0xc0000000U & (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                             << 0x12U)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][6U] 
        = ((0x7ffc00U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][6U]) | (0x3ffU & (vlSelfRef.__PVT__rnStage__DOT__opInfo[4U] 
                                   >> 0xeU)));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0xffffff0fU & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr
                         [1U] << 4U));
    vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U] 
        = ((0xfffffff0U & vlSelfRef.__PVT__rnStage__DOT__nextStage
            [1U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr
           [1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[1U][5U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage[1U][6U] 
        = vlSelfRef.__PVT__rnStage__DOT__nextStage[1U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr[0U] 
        = vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail;
    vlSelfRef.__PVT__loadQueue__DOT__pushCount = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateLoadQueue
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr[1U] 
        = (0xfU & ((0x10U > ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail) 
                             + (IData)(vlSelfRef.__PVT__loadQueue__DOT__pushCount)))
                    ? ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail) 
                       + (IData)(vlSelfRef.__PVT__loadQueue__DOT__pushCount))
                    : ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail) 
                       + (IData)(vlSelfRef.__PVT__loadQueue__DOT__pushCount))));
    vlSelfRef.__PVT__loadQueue__DOT__pushCount = (3U 
                                                  & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__pushCount) 
                                                     + 
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateLoadQueue
                                                     [1U]));
    vlSelfRef.__PVT__loadQueue__DOT__push = (0U < (IData)(vlSelfRef.__PVT__loadQueue__DOT__pushCount));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__9(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__9\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ bypassController__DOT____Vlvbound_h536e2a08__0;
    bypassController__DOT____Vlvbound_h536e2a08__0 = 0;
    CData/*6:0*/ bypassController__DOT____Vlvbound_h5380edc5__0;
    bypassController__DOT____Vlvbound_h5380edc5__0 = 0;
    CData/*6:0*/ bypassController__DOT____Vlvbound_h456474b5__0;
    bypassController__DOT____Vlvbound_h456474b5__0 = 0;
    CData/*0:0*/ bypassController__DOT____Vlvbound_hd3b1499b__0;
    bypassController__DOT____Vlvbound_hd3b1499b__0 = 0;
    CData/*6:0*/ bypassController__DOT____Vlvbound_h9a9f4d86__0;
    bypassController__DOT____Vlvbound_h9a9f4d86__0 = 0;
    CData/*6:0*/ bypassController__DOT____Vlvbound_h9a929242__0;
    bypassController__DOT____Vlvbound_h9a929242__0 = 0;
    CData/*6:0*/ bypassController__DOT____Vlvbound_h9a9205ad__0;
    bypassController__DOT____Vlvbound_h9a9205ad__0 = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__637__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__637__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__637__regNum;
    __Vfunc_bypassController__DOT__SelectReg__637__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__637__read;
    __Vfunc_bypassController__DOT__SelectReg__637__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__637__ret;
    __Vfunc_bypassController__DOT__SelectReg__637__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__638__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__638__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__638__regNum;
    __Vfunc_bypassController__DOT__SelectReg__638__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__638__read;
    __Vfunc_bypassController__DOT__SelectReg__638__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__638__ret;
    __Vfunc_bypassController__DOT__SelectReg__638__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__639__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__639__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__639__regNum;
    __Vfunc_bypassController__DOT__SelectReg__639__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__639__read;
    __Vfunc_bypassController__DOT__SelectReg__639__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__639__ret;
    __Vfunc_bypassController__DOT__SelectReg__639__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__640__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__640__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__640__regNum;
    __Vfunc_bypassController__DOT__SelectReg__640__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__640__read;
    __Vfunc_bypassController__DOT__SelectReg__640__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__640__ret;
    __Vfunc_bypassController__DOT__SelectReg__640__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__641__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__641__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__641__regNum;
    __Vfunc_bypassController__DOT__SelectReg__641__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__641__read;
    __Vfunc_bypassController__DOT__SelectReg__641__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__641__ret;
    __Vfunc_bypassController__DOT__SelectReg__641__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__642__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__642__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__642__regNum;
    __Vfunc_bypassController__DOT__SelectReg__642__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__642__read;
    __Vfunc_bypassController__DOT__SelectReg__642__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__642__ret;
    __Vfunc_bypassController__DOT__SelectReg__642__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__643__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__643__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__643__regNum;
    __Vfunc_bypassController__DOT__SelectReg__643__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__643__read;
    __Vfunc_bypassController__DOT__SelectReg__643__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__643__ret;
    __Vfunc_bypassController__DOT__SelectReg__643__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__644__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__644__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__644__regNum;
    __Vfunc_bypassController__DOT__SelectReg__644__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__644__read;
    __Vfunc_bypassController__DOT__SelectReg__644__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__644__ret;
    __Vfunc_bypassController__DOT__SelectReg__644__ret = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__645__Vfuncout;
    __Vfunc_bypassController__DOT__SelectReg__645__Vfuncout = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__645__regNum;
    __Vfunc_bypassController__DOT__SelectReg__645__regNum = 0;
    CData/*0:0*/ __Vfunc_bypassController__DOT__SelectReg__645__read;
    __Vfunc_bypassController__DOT__SelectReg__645__read = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i = 0;
    IData/*31:0*/ __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i;
    __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i = 0;
    CData/*6:0*/ __Vfunc_bypassController__DOT__SelectReg__645__ret;
    __Vfunc_bypassController__DOT__SelectReg__645__ret = 0;
    // Body
    vlSelfRef.__PVT__bypassController__DOT__intRR[0U] 
        = ((1U & vlSelfRef.__PVT__bypassController__DOT__intRR
            [0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum
                     [0U] << 1U));
    vlSelfRef.__PVT__bypassController__DOT__intRR[0U] 
        = ((0xfeU & vlSelfRef.__PVT__bypassController__DOT__intRR
            [0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg
           [0U]);
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__637__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__637__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__637__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel36;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel36;
            }
            __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i);
        }
        __Vlabel36: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)));
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel37;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret));
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel37;
            }
            __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i);
        }
        __Vlabel37: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__637__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__637__ret;
    vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl[0U] 
        = ((0x3fffU & vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl
            [0U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__Vfuncout) 
                     << 0xeU));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__638__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__638__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__638__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel38;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel38;
            }
            __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i);
        }
        __Vlabel38: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)));
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel39;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret));
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel39;
            }
            __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i);
        }
        __Vlabel39: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__638__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__638__ret;
    vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl[0U] 
        = ((0x1fc07fU & vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl
            [0U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__Vfuncout) 
                     << 7U));
    vlSelfRef.__PVT__bypassController__DOT__intRR[1U] 
        = ((1U & vlSelfRef.__PVT__bypassController__DOT__intRR
            [1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum
                     [1U] << 1U));
    vlSelfRef.__PVT__bypassController__DOT__intRR[1U] 
        = ((0xfeU & vlSelfRef.__PVT__bypassController__DOT__intRR
            [1U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg
           [1U]);
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__637__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__637__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__637__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel40;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel40;
            }
            __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk1__DOT__i);
        }
        __Vlabel40: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)));
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel41;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__637__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__637__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret));
                __Vfunc_bypassController__DOT__SelectReg__637__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__637__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel41;
            }
            __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__637__unnamedblk2__DOT__i);
        }
        __Vlabel41: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__637__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__637__ret;
    vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl[1U] 
        = ((0x3fffU & vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl
            [1U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__637__Vfuncout) 
                     << 0xeU));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__638__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__638__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__638__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel42;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel42;
            }
            __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk1__DOT__i);
        }
        __Vlabel42: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)));
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel43;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__638__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__638__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret));
                __Vfunc_bypassController__DOT__SelectReg__638__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__638__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel43;
            }
            __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__638__unnamedblk2__DOT__i);
        }
        __Vlabel43: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__638__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__638__ret;
    vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl[1U] 
        = ((0x1fc07fU & vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl
            [1U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__638__Vfuncout) 
                     << 7U));
    vlSelfRef.__PVT__bypassController__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intBypassCtrl
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__639__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__639__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__639__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__639__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__639__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__639__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__639__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel44;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__639__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__639__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__639__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__639__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel44;
            }
            __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk1__DOT__i);
        }
        __Vlabel44: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__639__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__639__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__639__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__639__ret)));
                __Vfunc_bypassController__DOT__SelectReg__639__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__639__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel45;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__639__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__639__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__639__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__639__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__639__ret));
                __Vfunc_bypassController__DOT__SelectReg__639__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__639__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel45;
            }
            __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__639__unnamedblk2__DOT__i);
        }
        __Vlabel45: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__639__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__639__ret;
    bypassController__DOT____Vlvbound_h536e2a08__0 
        = __Vfunc_bypassController__DOT__SelectReg__639__Vfuncout;
    vlSelfRef.__PVT__bypassController__DOT__complexBypassCtrl[0U] 
        = ((0x3fffU & vlSelfRef.__PVT__bypassController__DOT__complexBypassCtrl
            [0U]) | ((IData)(bypassController__DOT____Vlvbound_h536e2a08__0) 
                     << 0xeU));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__640__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__640__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__640__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__640__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__640__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__640__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__640__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel46;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__640__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__640__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__640__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__640__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel46;
            }
            __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk1__DOT__i);
        }
        __Vlabel46: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__640__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__640__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__640__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__640__ret)));
                __Vfunc_bypassController__DOT__SelectReg__640__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__640__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel47;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__640__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__640__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__640__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__640__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__640__ret));
                __Vfunc_bypassController__DOT__SelectReg__640__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__640__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel47;
            }
            __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__640__unnamedblk2__DOT__i);
        }
        __Vlabel47: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__640__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__640__ret;
    bypassController__DOT____Vlvbound_h5380edc5__0 
        = __Vfunc_bypassController__DOT__SelectReg__640__Vfuncout;
    vlSelfRef.__PVT__bypassController__DOT__complexBypassCtrl[0U] 
        = ((0x1fc07fU & vlSelfRef.__PVT__bypassController__DOT__complexBypassCtrl
            [0U]) | ((IData)(bypassController__DOT____Vlvbound_h5380edc5__0) 
                     << 7U));
    vlSelfRef.__PVT__bypassController__DOT__unnamedblk4__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__complexBypassCtrl
        [0U];
    bypassController__DOT____Vlvbound_h456474b5__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum
        [0U];
    vlSelfRef.__PVT__bypassController__DOT__memRR[0U] 
        = ((1U & vlSelfRef.__PVT__bypassController__DOT__memRR
            [0U]) | ((IData)(bypassController__DOT____Vlvbound_h456474b5__0) 
                     << 1U));
    bypassController__DOT____Vlvbound_hd3b1499b__0 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memWriteReg
        [0U];
    vlSelfRef.__PVT__bypassController__DOT__memRR[0U] 
        = ((0xfeU & vlSelfRef.__PVT__bypassController__DOT__memRR
            [0U]) | (IData)(bypassController__DOT____Vlvbound_hd3b1499b__0));
    vlSelfRef.__PVT__bypassController__DOT__unnamedblk5__DOT__i = 1U;
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__641__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__641__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__641__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel48;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel48;
            }
            __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i);
        }
        __Vlabel48: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)));
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel49;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret));
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel49;
            }
            __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i);
        }
        __Vlabel49: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__641__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__641__ret;
    vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl[0U] 
        = ((0x3fffU & vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl
            [0U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__Vfuncout) 
                     << 0xeU));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__642__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__642__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__642__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel50;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel50;
            }
            __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i);
        }
        __Vlabel50: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)));
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel51;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret));
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel51;
            }
            __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i);
        }
        __Vlabel51: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__642__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__642__ret;
    vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl[0U] 
        = ((0x1fc07fU & vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl
            [0U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__Vfuncout) 
                     << 7U));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__641__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__641__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__641__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel52;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel52;
            }
            __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk1__DOT__i);
        }
        __Vlabel52: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)));
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel53;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__641__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__641__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret));
                __Vfunc_bypassController__DOT__SelectReg__641__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__641__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel53;
            }
            __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__641__unnamedblk2__DOT__i);
        }
        __Vlabel53: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__641__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__641__ret;
    vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl[1U] 
        = ((0x3fffU & vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl
            [1U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__641__Vfuncout) 
                     << 0xeU));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__642__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__642__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__642__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel54;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel54;
            }
            __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk1__DOT__i);
        }
        __Vlabel54: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)));
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel55;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__642__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__642__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret));
                __Vfunc_bypassController__DOT__SelectReg__642__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__642__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel55;
            }
            __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__642__unnamedblk2__DOT__i);
        }
        __Vlabel55: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__642__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__642__ret;
    vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl[1U] 
        = ((0x1fc07fU & vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl
            [1U]) | ((IData)(__Vfunc_bypassController__DOT__SelectReg__642__Vfuncout) 
                     << 7U));
    vlSelfRef.__PVT__bypassController__DOT__unnamedblk6__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__memBypassCtrl
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__643__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__643__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumA
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__643__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__643__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__643__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__643__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__643__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel56;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__643__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__643__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__643__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__643__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel56;
            }
            __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk1__DOT__i);
        }
        __Vlabel56: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__643__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__643__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__643__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__643__ret)));
                __Vfunc_bypassController__DOT__SelectReg__643__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__643__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel57;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__643__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__643__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__643__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__643__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__643__ret));
                __Vfunc_bypassController__DOT__SelectReg__643__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__643__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel57;
            }
            __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__643__unnamedblk2__DOT__i);
        }
        __Vlabel57: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__643__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__643__ret;
    bypassController__DOT____Vlvbound_h9a9f4d86__0 
        = __Vfunc_bypassController__DOT__SelectReg__643__Vfuncout;
    vlSelfRef.__PVT__bypassController__DOT__fpBypassCtrl[0U] 
        = ((0x3fffU & vlSelfRef.__PVT__bypassController__DOT__fpBypassCtrl
            [0U]) | ((IData)(bypassController__DOT____Vlvbound_h9a9f4d86__0) 
                     << 0xeU));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__644__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__644__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumB
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__644__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__644__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__644__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__644__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__644__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel58;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__644__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__644__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__644__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__644__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel58;
            }
            __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk1__DOT__i);
        }
        __Vlabel58: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__644__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__644__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__644__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__644__ret)));
                __Vfunc_bypassController__DOT__SelectReg__644__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__644__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel59;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__644__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__644__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__644__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__644__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__644__ret));
                __Vfunc_bypassController__DOT__SelectReg__644__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__644__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel59;
            }
            __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__644__unnamedblk2__DOT__i);
        }
        __Vlabel59: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__644__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__644__ret;
    bypassController__DOT____Vlvbound_h9a929242__0 
        = __Vfunc_bypassController__DOT__SelectReg__644__Vfuncout;
    vlSelfRef.__PVT__bypassController__DOT__fpBypassCtrl[0U] 
        = ((0x1fc07fU & vlSelfRef.__PVT__bypassController__DOT__fpBypassCtrl
            [0U]) | ((IData)(bypassController__DOT____Vlvbound_h9a929242__0) 
                     << 7U));
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__memWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__memMA[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__memMA
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intWB[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intWB[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intWB
        [1U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intEX[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [0U];
    vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intEX[1U] 
        = vlSelfRef.__PVT__bypassController__DOT__intEX
        [1U];
    __Vfunc_bypassController__DOT__SelectReg__645__read 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegC
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__645__regNum 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumC
        [0U];
    __Vfunc_bypassController__DOT__SelectReg__645__ret = 0U;
    __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__645__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intEX
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__645__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intEX
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__645__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__645__ret)) 
                       | (0x40U | (8U & (__Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel60;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__645__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intWB
                  [(1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i)]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__645__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__intWB
                                 [(1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i)] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__645__ret 
                    = ((7U & (IData)(__Vfunc_bypassController__DOT__SelectReg__645__ret)) 
                       | (0x50U | (8U & (__Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i 
                                         << 3U))));
                goto __Vlabel60;
            }
            __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk1__DOT__i);
        }
        __Vlabel60: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 1U, __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i)) {
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__645__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__memMA
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__645__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__memMA
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__645__ret 
                    = (0x60U | (0xfU & (IData)(__Vfunc_bypassController__DOT__SelectReg__645__ret)));
                __Vfunc_bypassController__DOT__SelectReg__645__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__645__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel61;
            }
            if ((((IData)(__Vfunc_bypassController__DOT__SelectReg__645__read) 
                  & vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__memWB
                  [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i)) 
                    && (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i))]) 
                 & ((IData)(__Vfunc_bypassController__DOT__SelectReg__645__regNum) 
                    == (0x7fU & (vlSelfRef.__Vfunc_bypassController__DOT__SelectReg__645__memWB
                                 [((0U >= (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i)) 
                                   && (1U & __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i))] 
                                 >> 1U))))) {
                __Vfunc_bypassController__DOT__SelectReg__645__ret 
                    = (0x70U | (IData)(__Vfunc_bypassController__DOT__SelectReg__645__ret));
                __Vfunc_bypassController__DOT__SelectReg__645__ret 
                    = ((0x7dU & (IData)(__Vfunc_bypassController__DOT__SelectReg__645__ret)) 
                       | (2U & (__Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i 
                                << 1U)));
                goto __Vlabel61;
            }
            __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vfunc_bypassController__DOT__SelectReg__645__unnamedblk2__DOT__i);
        }
        __Vlabel61: ;
    }
    __Vfunc_bypassController__DOT__SelectReg__645__Vfuncout 
        = __Vfunc_bypassController__DOT__SelectReg__645__ret;
    bypassController__DOT____Vlvbound_h9a9205ad__0 
        = __Vfunc_bypassController__DOT__SelectReg__645__Vfuncout;
    vlSelfRef.__PVT__bypassController__DOT__fpBypassCtrl[0U] 
        = ((0x1fff80U & vlSelfRef.__PVT__bypassController__DOT__fpBypassCtrl
            [0U]) | (IData)(bypassController__DOT____Vlvbound_h9a9205ad__0));
    vlSelfRef.__PVT__bypassController__DOT__unnamedblk7__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut[0U] 
        = vlSelfRef.__PVT__bypassController__DOT__fpBypassCtrl
        [0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__10(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__10\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextHead 
        = vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead;
    vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount 
        = vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regCount;
    vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__roundedSetTailPtr 
        = (0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                   >> 9U));
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueue) {
        vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextHead 
            = (0xfU & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextHead) 
                       + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueueEntryNum)));
    }
    if (vlSelfRef.__PVT__loadQueue__DOT__push) {
        vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount) 
                        + (IData)(vlSelfRef.__PVT__loadQueue__DOT__pushCount)));
    } else if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                             >> 0x15U)))) {
        vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount 
            = (0x1fU & (((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead) 
                         <= (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__roundedSetTailPtr))
                         ? ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__roundedSetTailPtr) 
                            - (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead))
                         : (((IData)(0x10U) + (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__roundedSetTailPtr)) 
                            - (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead))));
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueue) {
        vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueueEntryNum)));
    }
    vlSelfRef.__PVT__recoveryManager__DOT__toRecoveryPhase 
        = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage) 
           | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInRwStage));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | (((IData)(vlSelfRef.__PVT__recoveryManager__DOT__toRecoveryPhase) 
               && (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInRwStage)) 
              << 3U));
    vlSelfRef.__PVT__recoveryManager__DOT__toCommitPhase 
        = (IData)(((0x400000U == (0x600000U & vlSelfRef.__PVT__recoveryManager__DOT__regState[3U])) 
                   & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__renameLogicRecoveryRMT) 
                         | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__issueQueueReturnIndex)))));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xfffffff8U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)
               ? (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromCommitStage)
               : (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromRwStage)));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xffffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | ((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                        << 0x20U) | (QData)((IData)(
                                                    ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                      << 0xdU) 
                                                     | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                        >> 0x13U)))))) 
              << 0x10U));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[1U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                      << 0x20U) | (QData)((IData)((
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                    << 0xdU) 
                                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                      >> 0x13U)))))) 
            >> 0x10U) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromCommitStage 
                          << 0x14U) | ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                                                  << 0x20U) 
                                                 | (QData)((IData)(
                                                                   ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                                     << 0xdU) 
                                                                    | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                                       >> 0x13U))))) 
                                                >> 0x20U)) 
                                       << 0x10U)));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U] 
        = ((0xfff00000U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U]) 
           | (((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromCommitStage 
                           >> 0xcU)) | ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                                                   << 0x20U) 
                                                  | (QData)((IData)(
                                                                    ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                                      << 0xdU) 
                                                                     | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                                        >> 0x13U))))) 
                                                 >> 0x20U)) 
                                        >> 0x10U)) 
              | (0xf0000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromCommitStage 
                             >> 0xcU))));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U] 
        = ((0xfffffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U]) 
           | ((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)) 
                        << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage)))) 
              << 0x14U));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U] 
        = ((0x600000U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U]) 
           | (0x7fffffU & (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)) 
                                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage)))) 
                            >> 0xcU) | ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)) 
                                                   << 0x20U) 
                                                  | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage))) 
                                                 >> 0x20U)) 
                                        << 0x14U))));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U] 
        = ((0x1fffffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U]) 
           | (0x7fffffU & (((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)
                             ? 0U : (3U & ((0U == (3U 
                                                   & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                      >> 0x15U)))
                                            ? ((IData)(vlSelfRef.__PVT__recoveryManager__DOT__toRecoveryPhase)
                                                ? 1U
                                                : 0U)
                                            : ((1U 
                                                == 
                                                (3U 
                                                 & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                    >> 0x15U)))
                                                ? 2U
                                                : ((IData)(vlSelfRef.__PVT__recoveryManager__DOT__toCommitPhase)
                                                    ? 0U
                                                    : 
                                                   ((vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                     << 0xbU) 
                                                    | (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                       >> 0x15U))))))) 
                           << 0x15U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__toCommitPhase 
        = vlSelfRef.__PVT__recoveryManager__DOT__toCommitPhase;
    vlSelfRef.__PVT__recoveryManager__DOT__exceptionOpPtr 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__exceptionOpPtr;
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xffff03ffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | (0xfc00U & ((((0U == (7U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U])) 
                           | (5U == (7U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U])))
                           ? (IData)(vlSelfRef.__PVT__recoveryManager__DOT__exceptionOpPtr)
                           : ((IData)(1U) + (IData)(vlSelfRef.__PVT__recoveryManager__DOT__exceptionOpPtr))) 
                         << 0xaU)));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xfffffc0fU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__detectedFlushRangeTailPtr) 
              << 4U));
    vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
        [0U];
    vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
        [1U];
    vlSelfRef.__PVT__renameLogicCommitter__DOT__releaseNum = 0U;
    vlSelfRef.__PVT__renameLogicCommitter__DOT__flushNum = 0U;
    if ((0U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__phase))) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum 
            = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commit)
                ? (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commitNum)
                : 0U);
        vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum = 0U;
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[0U] 
            = ((0x7fU & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                [0U]) | ((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commit) 
                           & (0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commitNum))) 
                          && (1U & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                            [0U] >> 0x18U)))) 
                         << 7U));
        vlSelfRef.__PVT__renameLogicCommitter__DOT__flushNum = 0U;
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[0U] 
            = ((0x80U & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                [0U]) | (0x7fU & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                          [0U] >> 4U))));
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[1U] 
            = ((0x7fU & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                [1U]) | ((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commit) 
                           & (1U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commitNum))) 
                          && (1U & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                            [1U] >> 0x18U)))) 
                         << 7U));
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[1U] 
            = ((0x80U & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                [1U]) | (0x7fU & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                          [1U] >> 4U))));
    } else {
        vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum = 0U;
        if ((1U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__phase))) {
            vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum = 0U;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[0U] 
                = (0x7fU & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                   [0U]);
            vlSelfRef.__PVT__renameLogicCommitter__DOT__flushNum = 0U;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[0U] 
                = (0x80U & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                   [0U]);
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[1U] 
                = (0x7fU & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                   [1U]);
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[1U] 
                = (0x80U & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                   [1U]);
        } else {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__releaseNum = 2U;
            if ((2U > (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount))) {
                vlSelfRef.__PVT__renameLogicCommitter__DOT__releaseNum 
                    = (3U & (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount));
            }
            vlSelfRef.__PVT__renameLogicCommitter__DOT__flushNum 
                = vlSelfRef.__PVT__renameLogicCommitter__DOT__releaseNum;
            vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum 
                = vlSelfRef.__PVT__renameLogicCommitter__DOT__releaseNum;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[0U] 
                = ((0x7fU & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                    [0U]) | (((0U < (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__releaseNum)) 
                              && (1U & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                                [0U] 
                                                >> 0x18U)))) 
                             << 7U));
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[0U] 
                = ((0x80U & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                    [0U]) | (0x7fU & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                              [0U] 
                                              >> 0xbU))));
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[1U] 
                = ((0x7fU & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                    [1U]) | (((1U < (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__releaseNum)) 
                              && (1U & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                                [1U] 
                                                >> 0x18U)))) 
                             << 7U));
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg[1U] 
                = ((0x80U & vlSelfRef.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                    [1U]) | (0x7fU & (IData)((vlSelfRef.__PVT__renameLogicCommitter__DOT__alReadData
                                              [1U] 
                                              >> 0xbU))));
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__flushNum 
        = vlSelfRef.__PVT__renameLogicCommitter__DOT__flushNum;
    vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextTail 
        = vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail;
    if (vlSelfRef.__PVT__loadQueue__DOT__push) {
        vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextTail 
            = (0xfU & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextTail) 
                       + (IData)(vlSelfRef.__PVT__loadQueue__DOT__pushCount)));
    } else if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                             >> 0x15U)))) {
        vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextTail 
            = vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__roundedSetTailPtr;
    }
}
