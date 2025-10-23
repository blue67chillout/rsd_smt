// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_BTB.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_BTB___act_sequent__TOP__SMT_RTL_Testbench__core__btb__0(VSMT_RTL_Testbench_BTB* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_BTB___act_sequent__TOP__SMT_RTL_Testbench__core__btb__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*9:0*/ __Vfunc_ToBTB_Index__3__Vfuncout;
    __Vfunc_ToBTB_Index__3__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToBTB_Index__3__pc;
    __Vfunc_ToBTB_Index__3__pc = 0;
    SData/*9:0*/ __Vfunc_ToBTB_Index__3__index;
    __Vfunc_ToBTB_Index__3__index = 0;
    CData/*3:0*/ __Vfunc_ToBTB_Tag__4__Vfuncout;
    __Vfunc_ToBTB_Tag__4__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToBTB_Tag__4__pc;
    __Vfunc_ToBTB_Tag__4__pc = 0;
    CData/*3:0*/ __Vfunc_ToBTB_Tag__4__tag;
    __Vfunc_ToBTB_Tag__4__tag = 0;
    SData/*12:0*/ __Vfunc_ToBTB_Addr__5__Vfuncout;
    __Vfunc_ToBTB_Addr__5__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToBTB_Addr__5__addr;
    __Vfunc_ToBTB_Addr__5__addr = 0;
    // Body
    vlSelfRef.__PVT__btbWE[0U] = 0U;
    vlSelfRef.__PVT__btbWE[1U] = 0U;
    vlSelfRef.__PVT__pushBtbQueue = 0U;
    vlSelfRef.__PVT__btbWE[0U] = (1U & ((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                 [0U] 
                                                 >> 0xcU)) 
                                        & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                   [0U] 
                                                   >> 0x10U))));
    vlSelfRef.__PVT__updateBtb = vlSelfRef.__PVT__btbWE
        [0U];
    __Vfunc_ToBTB_Index__3__pc = (0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [0U] 
                                                      >> 0x25U)));
    __Vfunc_ToBTB_Index__3__index = (0x3ffU & (__Vfunc_ToBTB_Index__3__pc 
                                               >> 2U));
    __Vfunc_ToBTB_Index__3__Vfuncout = ((IData)(__Vfunc_ToBTB_Index__3__index) 
                                        ^ (1U & (__Vfunc_ToBTB_Index__3__pc 
                                                 >> 0x13U)));
    vlSelfRef.__PVT__btbWA[0U] = __Vfunc_ToBTB_Index__3__Vfuncout;
    __Vfunc_ToBTB_Tag__4__pc = (0xfffffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                    [0U] 
                                                    >> 0x25U)));
    __Vfunc_ToBTB_Tag__4__tag = (0xfU & (__Vfunc_ToBTB_Tag__4__pc 
                                         >> 0xcU));
    __Vfunc_ToBTB_Tag__4__Vfuncout = ((IData)(__Vfunc_ToBTB_Tag__4__tag) 
                                      ^ (1U & (__Vfunc_ToBTB_Tag__4__pc 
                                               >> 0x13U)));
    vlSelfRef.__PVT__btbWV[0U] = ((0xc3fffU & vlSelfRef.__PVT__btbWV
                                   [0U]) | ((IData)(__Vfunc_ToBTB_Tag__4__Vfuncout) 
                                            << 0xeU));
    vlSelfRef.__PVT__btbWV[0U] = ((0xbffffU & vlSelfRef.__PVT__btbWV
                                   [0U]) | (0x40000U 
                                            & ((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                        [0U] 
                                                        >> 0x38U)) 
                                               << 0x12U)));
    __Vfunc_ToBTB_Addr__5__addr = (0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [0U] 
                                                       >> 0x11U)));
    __Vfunc_ToBTB_Addr__5__Vfuncout = (0x1fffU & (__Vfunc_ToBTB_Addr__5__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__btbWV[0U] = ((0xfc001U & vlSelfRef.__PVT__btbWV
                                   [0U]) | ((IData)(__Vfunc_ToBTB_Addr__5__Vfuncout) 
                                            << 1U));
    vlSelfRef.__PVT__btbWV[0U] = (0x80000U | vlSelfRef.__PVT__btbWV
                                  [0U]);
    vlSelfRef.__PVT__btbWV[0U] = ((0xffffeU & vlSelfRef.__PVT__btbWV
                                   [0U]) | (1U & (IData)(
                                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                          [0U] 
                                                          >> 0xeU))));
    if (vlSelfRef.__PVT__updateBtb) {
        vlSelfRef.__PVT__pushBtbQueue = (1U & ((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                        [1U] 
                                                        >> 0xcU)) 
                                               & (IData)(
                                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                          [1U] 
                                                          >> 0x10U))));
    } else {
        vlSelfRef.__PVT__btbWE[1U] = (1U & ((IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                     [1U] 
                                                     >> 0xcU)) 
                                            & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0x10U))));
        vlSelfRef.__PVT__updateBtb = ((IData)(vlSelfRef.__PVT__updateBtb) 
                                      | vlSelfRef.__PVT__btbWE
                                      [1U]);
    }
    __Vfunc_ToBTB_Index__3__pc = (0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [1U] 
                                                      >> 0x25U)));
    __Vfunc_ToBTB_Index__3__index = (0x3ffU & (__Vfunc_ToBTB_Index__3__pc 
                                               >> 2U));
    __Vfunc_ToBTB_Index__3__Vfuncout = ((IData)(__Vfunc_ToBTB_Index__3__index) 
                                        ^ (1U & (__Vfunc_ToBTB_Index__3__pc 
                                                 >> 0x13U)));
    vlSelfRef.__PVT__btbWA[1U] = __Vfunc_ToBTB_Index__3__Vfuncout;
    __Vfunc_ToBTB_Tag__4__pc = (0xfffffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                    [1U] 
                                                    >> 0x25U)));
    __Vfunc_ToBTB_Tag__4__tag = (0xfU & (__Vfunc_ToBTB_Tag__4__pc 
                                         >> 0xcU));
    __Vfunc_ToBTB_Tag__4__Vfuncout = ((IData)(__Vfunc_ToBTB_Tag__4__tag) 
                                      ^ (1U & (__Vfunc_ToBTB_Tag__4__pc 
                                               >> 0x13U)));
    vlSelfRef.__PVT__btbWV[1U] = ((0xc3fffU & vlSelfRef.__PVT__btbWV
                                   [1U]) | ((IData)(__Vfunc_ToBTB_Tag__4__Vfuncout) 
                                            << 0xeU));
    vlSelfRef.__PVT__btbWV[1U] = ((0xbffffU & vlSelfRef.__PVT__btbWV
                                   [1U]) | (0x40000U 
                                            & ((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                        [1U] 
                                                        >> 0x38U)) 
                                               << 0x12U)));
    __Vfunc_ToBTB_Addr__5__addr = (0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0x11U)));
    __Vfunc_ToBTB_Addr__5__Vfuncout = (0x1fffU & (__Vfunc_ToBTB_Addr__5__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__btbWV[1U] = ((0xfc001U & vlSelfRef.__PVT__btbWV
                                   [1U]) | ((IData)(__Vfunc_ToBTB_Addr__5__Vfuncout) 
                                            << 1U));
    vlSelfRef.__PVT__btbWV[1U] = (0x80000U | vlSelfRef.__PVT__btbWV
                                  [1U]);
    vlSelfRef.__PVT__btbWV[1U] = ((0xffffeU & vlSelfRef.__PVT__btbWV
                                   [1U]) | (1U & (IData)(
                                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                          [1U] 
                                                          >> 0xeU))));
    if (((0U != (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regCount)) 
         & (~ (IData)(vlSelfRef.__PVT__updateBtb)))) {
        vlSelfRef.__PVT__popBtbQueue = 1U;
        vlSelfRef.__PVT__btbWE[0U] = 1U;
        vlSelfRef.__PVT__btbWA[0U] = (0x3ffU & (IData)(
                                                       (vlSelfRef.__PVT__btbQueue
                                                        [vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage] 
                                                        >> 0x14U)));
        vlSelfRef.__PVT__btbWV[0U] = (0xfffffU & (IData)(
                                                         vlSelfRef.__PVT__btbQueue
                                                         [vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage]));
    } else {
        vlSelfRef.__PVT__popBtbQueue = 0U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk6__DOT__i = 2U;
        vlSelfRef.__PVT__btbWE[0U] = 1U;
        vlSelfRef.__PVT__btbWA[0U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__btbWV[0U] = (0xc3fffU & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__pushBtbQueue = 0U;
        vlSelfRef.__PVT__popBtbQueue = 0U;
        vlSelfRef.__PVT__btbWV[0U] = (0xbffffU & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__btbWE[1U] = 0U;
        vlSelfRef.__PVT__btbWA[1U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__btbWV[0U] = (0xfc001U & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__btbWV[0U] = (0x7ffffU & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__btbWV[1U] = (0xc3fffU & vlSelfRef.__PVT__btbWV
                                      [1U]);
        vlSelfRef.__PVT__btbWV[1U] = (0xbffffU & vlSelfRef.__PVT__btbWV
                                      [1U]);
        vlSelfRef.__PVT__btbWV[1U] = (0xfc001U & vlSelfRef.__PVT__btbWV
                                      [1U]);
        vlSelfRef.__PVT__btbWV[1U] = (0x7ffffU & vlSelfRef.__PVT__btbWV
                                      [1U]);
    }
    vlSelfRef.__PVT__btbQueuePointer__DOT__nextTailStorage 
        = vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage;
    vlSelfRef.__PVT__btbQueuePointer__DOT__nextHeadStorage 
        = vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage;
    vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount 
        = vlSelfRef.__PVT__btbQueuePointer__DOT__regCount;
    if (vlSelfRef.__PVT__pushBtbQueue) {
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextTailStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextTailStorage))));
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount)));
    }
    if (vlSelfRef.__PVT__popBtbQueue) {
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextHeadStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextHeadStorage))));
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount) 
                        - (IData)(1U)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__we[0U] 
        = vlSelfRef.__PVT__btbWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__we[1U] 
        = vlSelfRef.__PVT__btbWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wa[0U] 
        = vlSelfRef.__PVT__btbWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wa[1U] 
        = vlSelfRef.__PVT__btbWA[1U];
    vlSelfRef.__Vcellinp__btbEntryArray__wv[0U] = vlSelfRef.__PVT__btbWV
        [0U];
    vlSelfRef.__Vcellinp__btbEntryArray__wv[1U] = vlSelfRef.__PVT__btbWV
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__btbEntryArray__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__btbEntryArray__wv[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BTB___act_comb__TOP__SMT_RTL_Testbench__core__btb__0(VSMT_RTL_Testbench_BTB* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_BTB___act_comb__TOP__SMT_RTL_Testbench__core__btb__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*19:0*/ __Vfunc_ToRawAddrFromBTB_Addr__2__Vfuncout;
    __Vfunc_ToRawAddrFromBTB_Addr__2__Vfuncout = 0;
    SData/*12:0*/ __Vfunc_ToRawAddrFromBTB_Addr__2__addr;
    __Vfunc_ToRawAddrFromBTB_Addr__2__addr = 0;
    IData/*19:0*/ __Vfunc_ToRawAddrFromBTB_Addr__2__pc;
    __Vfunc_ToRawAddrFromBTB_Addr__2__pc = 0;
    // Body
    vlSelfRef.__Vcellout__btbEntryArray__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__btbEntryArray__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__rv
        [1U];
    vlSelfRef.__PVT__btbRV[0U] = vlSelfRef.__Vcellout__btbEntryArray__rv
        [0U];
    vlSelfRef.__PVT__btbRV[1U] = vlSelfRef.__Vcellout__btbEntryArray__rv
        [1U];
    vlSelfRef.__PVT__readIsCondBr[0U] = (1U & vlSelfRef.__PVT__btbRV
                                         [0U]);
    vlSelfRef.__PVT__readIsCondBr[1U] = (1U & vlSelfRef.__PVT__btbRV
                                         [1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__readIsCondBr[0U] 
        = vlSelfRef.__PVT__readIsCondBr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__readIsCondBr[1U] 
        = vlSelfRef.__PVT__readIsCondBr[1U];
    __Vfunc_ToRawAddrFromBTB_Addr__2__pc = (0xfffffU 
                                            & (IData)(vlSelfRef.__PVT__tagReg));
    __Vfunc_ToRawAddrFromBTB_Addr__2__addr = (0x1fffU 
                                              & (vlSelfRef.__PVT__btbRV
                                                 [0U] 
                                                 >> 1U));
    __Vfunc_ToRawAddrFromBTB_Addr__2__Vfuncout = ((0x78000U 
                                                   & __Vfunc_ToRawAddrFromBTB_Addr__2__pc) 
                                                  | ((IData)(__Vfunc_ToRawAddrFromBTB_Addr__2__addr) 
                                                     << 2U));
    vlSelfRef.__PVT__btbOut[0U] = __Vfunc_ToRawAddrFromBTB_Addr__2__Vfuncout;
    __Vfunc_ToRawAddrFromBTB_Addr__2__pc = (0xfffffU 
                                            & (IData)(
                                                      (vlSelfRef.__PVT__tagReg 
                                                       >> 0x14U)));
    __Vfunc_ToRawAddrFromBTB_Addr__2__addr = (0x1fffU 
                                              & (vlSelfRef.__PVT__btbRV
                                                 [1U] 
                                                 >> 1U));
    __Vfunc_ToRawAddrFromBTB_Addr__2__Vfuncout = ((0x78000U 
                                                   & __Vfunc_ToRawAddrFromBTB_Addr__2__pc) 
                                                  | ((IData)(__Vfunc_ToRawAddrFromBTB_Addr__2__addr) 
                                                     << 2U));
    vlSelfRef.__PVT__btbOut[1U] = __Vfunc_ToRawAddrFromBTB_Addr__2__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut[0U] 
        = vlSelfRef.__PVT__btbOut[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut[1U] 
        = vlSelfRef.__PVT__btbOut[1U];
    vlSelfRef.__PVT__btbHit[0U] = (((vlSelfRef.__PVT__btbRV
                                     [0U] >> 0x13U) 
                                    & ((0xfU & (vlSelfRef.__PVT__btbRV
                                                [0U] 
                                                >> 0xeU)) 
                                       == ([&]() {
                        vlSelfRef.__Vfunc_ToBTB_Tag__1__pc 
                            = (0xfffffU & (IData)(vlSelfRef.__PVT__tagReg));
                        vlSelfRef.__Vfunc_ToBTB_Tag__1__tag 
                            = (0xfU & (vlSelfRef.__Vfunc_ToBTB_Tag__1__pc 
                                       >> 0xcU));
                        vlSelfRef.__Vfunc_ToBTB_Tag__1__Vfuncout 
                            = ((IData)(vlSelfRef.__Vfunc_ToBTB_Tag__1__tag) 
                               ^ (1U & (vlSelfRef.__Vfunc_ToBTB_Tag__1__pc 
                                        >> 0x13U)));
                    }(), (IData)(vlSelfRef.__Vfunc_ToBTB_Tag__1__Vfuncout)))) 
                                   & ((1U & (vlSelfRef.__PVT__btbRV
                                             [0U] >> 0x12U)) 
                                      == (1U & (IData)(
                                                       (vlSelfRef.__PVT__tagReg 
                                                        >> 0x13U)))));
    vlSelfRef.__PVT__btbHit[1U] = (((vlSelfRef.__PVT__btbRV
                                     [1U] >> 0x13U) 
                                    & ((0xfU & (vlSelfRef.__PVT__btbRV
                                                [1U] 
                                                >> 0xeU)) 
                                       == ([&]() {
                        vlSelfRef.__Vfunc_ToBTB_Tag__1__pc 
                            = (0xfffffU & (IData)((vlSelfRef.__PVT__tagReg 
                                                   >> 0x14U)));
                        vlSelfRef.__Vfunc_ToBTB_Tag__1__tag 
                            = (0xfU & (vlSelfRef.__Vfunc_ToBTB_Tag__1__pc 
                                       >> 0xcU));
                        vlSelfRef.__Vfunc_ToBTB_Tag__1__Vfuncout 
                            = ((IData)(vlSelfRef.__Vfunc_ToBTB_Tag__1__tag) 
                               ^ (1U & (vlSelfRef.__Vfunc_ToBTB_Tag__1__pc 
                                        >> 0x13U)));
                    }(), (IData)(vlSelfRef.__Vfunc_ToBTB_Tag__1__Vfuncout)))) 
                                   & ((1U & (vlSelfRef.__PVT__btbRV
                                             [1U] >> 0x12U)) 
                                      == (1U & (IData)(
                                                       (vlSelfRef.__PVT__tagReg 
                                                        >> 0x27U)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit[0U] 
        = vlSelfRef.__PVT__btbHit[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit[1U] 
        = vlSelfRef.__PVT__btbHit[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BTB___act_comb__TOP__SMT_RTL_Testbench__core__btb__1(VSMT_RTL_Testbench_BTB* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_BTB___act_comb__TOP__SMT_RTL_Testbench__core__btb__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*19:0*/ __Vlvbound_had9b5299__0;
    __Vlvbound_had9b5299__0 = 0;
    SData/*9:0*/ __Vfunc_ToBTB_Index__0__Vfuncout;
    __Vfunc_ToBTB_Index__0__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToBTB_Index__0__pc;
    __Vfunc_ToBTB_Index__0__pc = 0;
    SData/*9:0*/ __Vfunc_ToBTB_Index__0__index;
    __Vfunc_ToBTB_Index__0__index = 0;
    // Body
    vlSelfRef.__PVT__pcIn = vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__predNextPC;
    __Vfunc_ToBTB_Index__0__pc = vlSelfRef.__PVT__pcIn;
    __Vfunc_ToBTB_Index__0__index = (0x3ffU & (__Vfunc_ToBTB_Index__0__pc 
                                               >> 2U));
    __Vfunc_ToBTB_Index__0__Vfuncout = ((IData)(__Vfunc_ToBTB_Index__0__index) 
                                        ^ (1U & (__Vfunc_ToBTB_Index__0__pc 
                                                 >> 0x13U)));
    vlSelfRef.__PVT__btbRA[0U] = __Vfunc_ToBTB_Index__0__Vfuncout;
    __Vlvbound_had9b5299__0 = vlSelfRef.__PVT__pcIn;
    vlSelfRef.__PVT__nextTagReg = ((0xfffff00000ULL 
                                    & vlSelfRef.__PVT__nextTagReg) 
                                   | (IData)((IData)(__Vlvbound_had9b5299__0)));
    __Vfunc_ToBTB_Index__0__pc = (0xfffffU & ((IData)(4U) 
                                              + vlSelfRef.__PVT__pcIn));
    __Vfunc_ToBTB_Index__0__index = (0x3ffU & (__Vfunc_ToBTB_Index__0__pc 
                                               >> 2U));
    __Vfunc_ToBTB_Index__0__Vfuncout = ((IData)(__Vfunc_ToBTB_Index__0__index) 
                                        ^ (1U & (__Vfunc_ToBTB_Index__0__pc 
                                                 >> 0x13U)));
    vlSelfRef.__PVT__btbRA[1U] = __Vfunc_ToBTB_Index__0__Vfuncout;
    __Vlvbound_had9b5299__0 = (0xfffffU & ((IData)(4U) 
                                           + vlSelfRef.__PVT__pcIn));
    vlSelfRef.__PVT__nextTagReg = ((0xfffffULL & vlSelfRef.__PVT__nextTagReg) 
                                   | ((QData)((IData)(__Vlvbound_had9b5299__0)) 
                                      << 0x14U));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__btbRA[0U] = 0U;
        vlSelfRef.__PVT__btbRA[1U] = 1U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__ra[0U] 
        = vlSelfRef.__PVT__btbRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__ra[1U] 
        = vlSelfRef.__PVT__btbRA[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BTB___nba_sequent__TOP__SMT_RTL_Testbench__core__btb__0(VSMT_RTL_Testbench_BTB* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_BTB___nba_sequent__TOP__SMT_RTL_Testbench__core__btb__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*9:0*/ __Vdly__resetIndex;
    __Vdly__resetIndex = 0;
    CData/*4:0*/ __VdlyDim0__btbQueue__v0;
    __VdlyDim0__btbQueue__v0 = 0;
    CData/*0:0*/ __VdlySet__btbQueue__v0;
    __VdlySet__btbQueue__v0 = 0;
    CData/*4:0*/ __VdlyDim0__btbQueue__v1;
    __VdlyDim0__btbQueue__v1 = 0;
    CData/*4:0*/ __VdlyDim0__btbQueue__v2;
    __VdlyDim0__btbQueue__v2 = 0;
    IData/*31:0*/ __VdlyVal__btbQueue__v3;
    __VdlyVal__btbQueue__v3 = 0;
    CData/*4:0*/ __VdlyDim0__btbQueue__v3;
    __VdlyDim0__btbQueue__v3 = 0;
    CData/*0:0*/ __VdlySet__btbQueue__v3;
    __VdlySet__btbQueue__v3 = 0;
    IData/*19:0*/ __VdlyVal__btbQueue__v4;
    __VdlyVal__btbQueue__v4 = 0;
    CData/*4:0*/ __VdlyDim0__btbQueue__v4;
    __VdlyDim0__btbQueue__v4 = 0;
    // Body
    __Vdly__resetIndex = vlSelfRef.__PVT__resetIndex;
    if (VL_UNLIKELY((((0U == (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regCount)) 
                      & (IData)(vlSelfRef.__PVT__popBtbQueue))))) {
        VL_WRITEF_NX("Pop from an empty queue.\n",0);
    }
    if (VL_UNLIKELY((((0x20U == (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regCount)) 
                      & (IData)(vlSelfRef.__PVT__pushBtbQueue))))) {
        VL_WRITEF_NX("Push to a full queue.\n",0);
    }
    __VdlySet__btbQueue__v0 = 0U;
    __VdlySet__btbQueue__v3 = 0U;
    __Vdly__resetIndex = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rstStart)
                           ? 0U : (0x3ffU & ((IData)(1U) 
                                             + (IData)(vlSelfRef.__PVT__resetIndex))));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        __VdlyDim0__btbQueue__v0 = (0x1fU & (IData)(vlSelfRef.__PVT__resetIndex));
        __VdlySet__btbQueue__v0 = 1U;
        __VdlyDim0__btbQueue__v1 = (0x1fU & (IData)(vlSelfRef.__PVT__resetIndex));
        __VdlyDim0__btbQueue__v2 = (0x1fU & (IData)(vlSelfRef.__PVT__resetIndex));
        vlSelfRef.__PVT__tagReg = 0ULL;
        vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage = 0U;
        vlSelfRef.__PVT__btbQueuePointer__DOT__regCount = 0U;
        vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage = 0U;
    } else {
        if (vlSelfRef.__PVT__pushBtbQueue) {
            __VdlyVal__btbQueue__v3 = vlSelfRef.__PVT__btbWA
                [1U];
            __VdlyDim0__btbQueue__v3 = vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage;
            __VdlySet__btbQueue__v3 = 1U;
            __VdlyVal__btbQueue__v4 = vlSelfRef.__PVT__btbWV
                [1U];
            __VdlyDim0__btbQueue__v4 = vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage;
        }
        vlSelfRef.__PVT__tagReg = vlSelfRef.__PVT__nextTagReg;
        vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage 
            = vlSelfRef.__PVT__btbQueuePointer__DOT__nextTailStorage;
        vlSelfRef.__PVT__btbQueuePointer__DOT__regCount 
            = vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount;
        vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage 
            = vlSelfRef.__PVT__btbQueuePointer__DOT__nextHeadStorage;
    }
    vlSelfRef.__PVT__resetIndex = __Vdly__resetIndex;
    if (__VdlySet__btbQueue__v0) {
        vlSelfRef.__PVT__btbQueue[__VdlyDim0__btbQueue__v0] 
            = (0xfffffULL & vlSelfRef.__PVT__btbQueue
               [__VdlyDim0__btbQueue__v0]);
        vlSelfRef.__PVT__btbQueue[__VdlyDim0__btbQueue__v1] 
            = (0xffffffff00000ULL & vlSelfRef.__PVT__btbQueue
               [__VdlyDim0__btbQueue__v1]);
        vlSelfRef.__PVT__btbQueue[__VdlyDim0__btbQueue__v2] 
            = (0xffffffffbffffULL & vlSelfRef.__PVT__btbQueue
               [__VdlyDim0__btbQueue__v2]);
    }
    if (__VdlySet__btbQueue__v3) {
        vlSelfRef.__PVT__btbQueue[__VdlyDim0__btbQueue__v3] 
            = ((0xfffffULL & vlSelfRef.__PVT__btbQueue
                [__VdlyDim0__btbQueue__v3]) | ((QData)((IData)(__VdlyVal__btbQueue__v3)) 
                                               << 0x14U));
        vlSelfRef.__PVT__btbQueue[__VdlyDim0__btbQueue__v4] 
            = ((0xffffffff00000ULL & vlSelfRef.__PVT__btbQueue
                [__VdlyDim0__btbQueue__v4]) | (IData)((IData)(__VdlyVal__btbQueue__v4)));
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BTB___nba_sequent__TOP__SMT_RTL_Testbench__core__btb__1(VSMT_RTL_Testbench_BTB* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_BTB___nba_sequent__TOP__SMT_RTL_Testbench__core__btb__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk6__DOT__i = 2U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BTB___nba_comb__TOP__SMT_RTL_Testbench__core__btb__0(VSMT_RTL_Testbench_BTB* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_BTB___nba_comb__TOP__SMT_RTL_Testbench__core__btb__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*9:0*/ __Vfunc_ToBTB_Index__3__Vfuncout;
    __Vfunc_ToBTB_Index__3__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToBTB_Index__3__pc;
    __Vfunc_ToBTB_Index__3__pc = 0;
    SData/*9:0*/ __Vfunc_ToBTB_Index__3__index;
    __Vfunc_ToBTB_Index__3__index = 0;
    CData/*3:0*/ __Vfunc_ToBTB_Tag__4__Vfuncout;
    __Vfunc_ToBTB_Tag__4__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToBTB_Tag__4__pc;
    __Vfunc_ToBTB_Tag__4__pc = 0;
    CData/*3:0*/ __Vfunc_ToBTB_Tag__4__tag;
    __Vfunc_ToBTB_Tag__4__tag = 0;
    SData/*12:0*/ __Vfunc_ToBTB_Addr__5__Vfuncout;
    __Vfunc_ToBTB_Addr__5__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToBTB_Addr__5__addr;
    __Vfunc_ToBTB_Addr__5__addr = 0;
    // Body
    vlSelfRef.__PVT__btbWE[0U] = 0U;
    vlSelfRef.__PVT__btbWE[1U] = 0U;
    vlSelfRef.__PVT__pushBtbQueue = 0U;
    vlSelfRef.__PVT__btbWE[0U] = (1U & ((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                 [0U] 
                                                 >> 0xcU)) 
                                        & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                   [0U] 
                                                   >> 0x10U))));
    vlSelfRef.__PVT__updateBtb = vlSelfRef.__PVT__btbWE
        [0U];
    __Vfunc_ToBTB_Index__3__pc = (0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [0U] 
                                                      >> 0x25U)));
    __Vfunc_ToBTB_Index__3__index = (0x3ffU & (__Vfunc_ToBTB_Index__3__pc 
                                               >> 2U));
    __Vfunc_ToBTB_Index__3__Vfuncout = ((IData)(__Vfunc_ToBTB_Index__3__index) 
                                        ^ (1U & (__Vfunc_ToBTB_Index__3__pc 
                                                 >> 0x13U)));
    vlSelfRef.__PVT__btbWA[0U] = __Vfunc_ToBTB_Index__3__Vfuncout;
    __Vfunc_ToBTB_Tag__4__pc = (0xfffffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                    [0U] 
                                                    >> 0x25U)));
    __Vfunc_ToBTB_Tag__4__tag = (0xfU & (__Vfunc_ToBTB_Tag__4__pc 
                                         >> 0xcU));
    __Vfunc_ToBTB_Tag__4__Vfuncout = ((IData)(__Vfunc_ToBTB_Tag__4__tag) 
                                      ^ (1U & (__Vfunc_ToBTB_Tag__4__pc 
                                               >> 0x13U)));
    vlSelfRef.__PVT__btbWV[0U] = ((0xc3fffU & vlSelfRef.__PVT__btbWV
                                   [0U]) | ((IData)(__Vfunc_ToBTB_Tag__4__Vfuncout) 
                                            << 0xeU));
    vlSelfRef.__PVT__btbWV[0U] = ((0xbffffU & vlSelfRef.__PVT__btbWV
                                   [0U]) | (0x40000U 
                                            & ((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                        [0U] 
                                                        >> 0x38U)) 
                                               << 0x12U)));
    __Vfunc_ToBTB_Addr__5__addr = (0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [0U] 
                                                       >> 0x11U)));
    __Vfunc_ToBTB_Addr__5__Vfuncout = (0x1fffU & (__Vfunc_ToBTB_Addr__5__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__btbWV[0U] = ((0xfc001U & vlSelfRef.__PVT__btbWV
                                   [0U]) | ((IData)(__Vfunc_ToBTB_Addr__5__Vfuncout) 
                                            << 1U));
    vlSelfRef.__PVT__btbWV[0U] = (0x80000U | vlSelfRef.__PVT__btbWV
                                  [0U]);
    vlSelfRef.__PVT__btbWV[0U] = ((0xffffeU & vlSelfRef.__PVT__btbWV
                                   [0U]) | (1U & (IData)(
                                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                          [0U] 
                                                          >> 0xeU))));
    if (vlSelfRef.__PVT__updateBtb) {
        vlSelfRef.__PVT__pushBtbQueue = (1U & ((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                        [1U] 
                                                        >> 0xcU)) 
                                               & (IData)(
                                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                          [1U] 
                                                          >> 0x10U))));
    } else {
        vlSelfRef.__PVT__btbWE[1U] = (1U & ((IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                     [1U] 
                                                     >> 0xcU)) 
                                            & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0x10U))));
        vlSelfRef.__PVT__updateBtb = ((IData)(vlSelfRef.__PVT__updateBtb) 
                                      | vlSelfRef.__PVT__btbWE
                                      [1U]);
    }
    __Vfunc_ToBTB_Index__3__pc = (0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                      [1U] 
                                                      >> 0x25U)));
    __Vfunc_ToBTB_Index__3__index = (0x3ffU & (__Vfunc_ToBTB_Index__3__pc 
                                               >> 2U));
    __Vfunc_ToBTB_Index__3__Vfuncout = ((IData)(__Vfunc_ToBTB_Index__3__index) 
                                        ^ (1U & (__Vfunc_ToBTB_Index__3__pc 
                                                 >> 0x13U)));
    vlSelfRef.__PVT__btbWA[1U] = __Vfunc_ToBTB_Index__3__Vfuncout;
    __Vfunc_ToBTB_Tag__4__pc = (0xfffffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                    [1U] 
                                                    >> 0x25U)));
    __Vfunc_ToBTB_Tag__4__tag = (0xfU & (__Vfunc_ToBTB_Tag__4__pc 
                                         >> 0xcU));
    __Vfunc_ToBTB_Tag__4__Vfuncout = ((IData)(__Vfunc_ToBTB_Tag__4__tag) 
                                      ^ (1U & (__Vfunc_ToBTB_Tag__4__pc 
                                               >> 0x13U)));
    vlSelfRef.__PVT__btbWV[1U] = ((0xc3fffU & vlSelfRef.__PVT__btbWV
                                   [1U]) | ((IData)(__Vfunc_ToBTB_Tag__4__Vfuncout) 
                                            << 0xeU));
    vlSelfRef.__PVT__btbWV[1U] = ((0xbffffU & vlSelfRef.__PVT__btbWV
                                   [1U]) | (0x40000U 
                                            & ((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                        [1U] 
                                                        >> 0x38U)) 
                                               << 0x12U)));
    __Vfunc_ToBTB_Addr__5__addr = (0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                       [1U] 
                                                       >> 0x11U)));
    __Vfunc_ToBTB_Addr__5__Vfuncout = (0x1fffU & (__Vfunc_ToBTB_Addr__5__addr 
                                                  >> 2U));
    vlSelfRef.__PVT__btbWV[1U] = ((0xfc001U & vlSelfRef.__PVT__btbWV
                                   [1U]) | ((IData)(__Vfunc_ToBTB_Addr__5__Vfuncout) 
                                            << 1U));
    vlSelfRef.__PVT__btbWV[1U] = (0x80000U | vlSelfRef.__PVT__btbWV
                                  [1U]);
    vlSelfRef.__PVT__btbWV[1U] = ((0xffffeU & vlSelfRef.__PVT__btbWV
                                   [1U]) | (1U & (IData)(
                                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult
                                                          [1U] 
                                                          >> 0xeU))));
    if (((0U != (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regCount)) 
         & (~ (IData)(vlSelfRef.__PVT__updateBtb)))) {
        vlSelfRef.__PVT__popBtbQueue = 1U;
        vlSelfRef.__PVT__btbWE[0U] = 1U;
        vlSelfRef.__PVT__btbWA[0U] = (0x3ffU & (IData)(
                                                       (vlSelfRef.__PVT__btbQueue
                                                        [vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage] 
                                                        >> 0x14U)));
        vlSelfRef.__PVT__btbWV[0U] = (0xfffffU & (IData)(
                                                         vlSelfRef.__PVT__btbQueue
                                                         [vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage]));
    } else {
        vlSelfRef.__PVT__popBtbQueue = 0U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__btbWE[0U] = 1U;
        vlSelfRef.__PVT__btbWA[0U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__btbWV[0U] = (0xc3fffU & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__pushBtbQueue = 0U;
        vlSelfRef.__PVT__popBtbQueue = 0U;
        vlSelfRef.__PVT__btbWV[0U] = (0xbffffU & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__btbWE[1U] = 0U;
        vlSelfRef.__PVT__btbWA[1U] = vlSelfRef.__PVT__resetIndex;
        vlSelfRef.__PVT__btbWV[0U] = (0xfc001U & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__btbWV[0U] = (0x7ffffU & vlSelfRef.__PVT__btbWV
                                      [0U]);
        vlSelfRef.__PVT__btbWV[1U] = (0xc3fffU & vlSelfRef.__PVT__btbWV
                                      [1U]);
        vlSelfRef.__PVT__btbWV[1U] = (0xbffffU & vlSelfRef.__PVT__btbWV
                                      [1U]);
        vlSelfRef.__PVT__btbWV[1U] = (0xfc001U & vlSelfRef.__PVT__btbWV
                                      [1U]);
        vlSelfRef.__PVT__btbWV[1U] = (0x7ffffU & vlSelfRef.__PVT__btbWV
                                      [1U]);
    }
    vlSelfRef.__PVT__btbQueuePointer__DOT__nextTailStorage 
        = vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage;
    vlSelfRef.__PVT__btbQueuePointer__DOT__nextHeadStorage 
        = vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage;
    vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount 
        = vlSelfRef.__PVT__btbQueuePointer__DOT__regCount;
    if (vlSelfRef.__PVT__pushBtbQueue) {
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextTailStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regTailStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextTailStorage))));
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount)));
    }
    if (vlSelfRef.__PVT__popBtbQueue) {
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextHeadStorage 
            = ((0x1fU == (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__regHeadStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextHeadStorage))));
        vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__btbQueuePointer__DOT__nextCount) 
                        - (IData)(1U)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__we[0U] 
        = vlSelfRef.__PVT__btbWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__we[1U] 
        = vlSelfRef.__PVT__btbWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wa[0U] 
        = vlSelfRef.__PVT__btbWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wa[1U] 
        = vlSelfRef.__PVT__btbWA[1U];
    vlSelfRef.__Vcellinp__btbEntryArray__wv[0U] = vlSelfRef.__PVT__btbWV
        [0U];
    vlSelfRef.__Vcellinp__btbEntryArray__wv[1U] = vlSelfRef.__PVT__btbWV
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__btbEntryArray__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__btbEntryArray__wv[1U];
}
