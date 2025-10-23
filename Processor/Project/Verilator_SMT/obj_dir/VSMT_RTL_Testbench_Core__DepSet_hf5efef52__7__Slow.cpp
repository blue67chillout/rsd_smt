// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__33(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__33\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*19:0*/ loadQueue__DOT____Vlvbound_h75038a4e__0;
    loadQueue__DOT____Vlvbound_h75038a4e__0 = 0;
    CData/*0:0*/ loadQueue__DOT____Vlvbound_h70aa6c43__0;
    loadQueue__DOT____Vlvbound_h70aa6c43__0 = 0;
    CData/*3:0*/ loadQueue__DOT____Vlvbound_hd66c37fb__0;
    loadQueue__DOT____Vlvbound_hd66c37fb__0 = 0;
    CData/*0:0*/ loadQueue__DOT____Vlvbound_h2103b133__0;
    loadQueue__DOT____Vlvbound_h2103b133__0 = 0;
    CData/*0:0*/ loadQueue__DOT____Vlvbound_hba8df09a__0;
    loadQueue__DOT____Vlvbound_hba8df09a__0 = 0;
    IData/*19:0*/ __Vfunc_LSQ_ToBlockAddr__578__Vfuncout;
    __Vfunc_LSQ_ToBlockAddr__578__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_LSQ_ToBlockAddr__578__addr;
    __Vfunc_LSQ_ToBlockAddr__578__addr = 0;
    CData/*0:0*/ __Vfunc_LSQ_ToBlockWordEnable__579__Vfuncout;
    __Vfunc_LSQ_ToBlockWordEnable__579__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_LSQ_ToBlockWordEnable__579__addr;
    __Vfunc_LSQ_ToBlockWordEnable__579__addr = 0;
    // Body
    vlSelfRef.__PVT__loadQueue__DOT__reset = vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst;
    vlSelfRef.__PVT__loadQueue__DOT__violation[0U] = 0U;
    vlSelfRef.__PVT__loadQueue__DOT__conflictLoadPC[0U] = 0U;
    __Vfunc_LSQ_ToBlockAddr__578__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
        [0U];
    __Vfunc_LSQ_ToBlockAddr__578__Vfuncout = (0xfffffU 
                                              & (__Vfunc_LSQ_ToBlockAddr__578__addr 
                                                 >> 2U));
    loadQueue__DOT____Vlvbound_h75038a4e__0 = __Vfunc_LSQ_ToBlockAddr__578__Vfuncout;
    vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr[0U] 
        = loadQueue__DOT____Vlvbound_h75038a4e__0;
    __Vfunc_LSQ_ToBlockWordEnable__579__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
        [0U];
    __Vfunc_LSQ_ToBlockWordEnable__579__Vfuncout = 
        (1U & VL_SHIFTL_III(1,1,32, (IData)(1U), ([&]() {
                    vlSelfRef.__Vfunc_LSQ_SelectBits__580__width = 0U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__580__offset = 2U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__580__data 
                        = __Vfunc_LSQ_ToBlockWordEnable__579__addr;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__580__ret = 0U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i = 0U;
                    while (VL_LTS_III(32, vlSelfRef.__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i, vlSelfRef.__Vfunc_LSQ_SelectBits__580__width)) {
                        vlSelfRef.__Vfunc_LSQ_SelectBits__580__ret 
                            = (((~ ((IData)(1U) << 
                                    (0x1fU & vlSelfRef.__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i))) 
                                & vlSelfRef.__Vfunc_LSQ_SelectBits__580__ret) 
                               | (0xffffffffULL & (
                                                   (1U 
                                                    & (vlSelfRef.__Vfunc_LSQ_SelectBits__580__data 
                                                       >> 
                                                       (0x1fU 
                                                        & (vlSelfRef.__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i 
                                                           + vlSelfRef.__Vfunc_LSQ_SelectBits__580__offset)))) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i))));
                        vlSelfRef.__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i 
                            = ((IData)(1U) + vlSelfRef.__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i);
                    }
                    vlSelfRef.__Vfunc_LSQ_SelectBits__580__Vfuncout 
                        = vlSelfRef.__Vfunc_LSQ_SelectBits__580__ret;
                }(), vlSelfRef.__Vfunc_LSQ_SelectBits__580__Vfuncout)));
    loadQueue__DOT____Vlvbound_h70aa6c43__0 = __Vfunc_LSQ_ToBlockWordEnable__579__Vfuncout;
    vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE[0U] 
        = loadQueue__DOT____Vlvbound_h70aa6c43__0;
    loadQueue__DOT____Vlvbound_hd66c37fb__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadQueuePtrByStore
        [0U];
    vlSelfRef.__PVT__loadQueue__DOT__executedLoadQueuePtrByStore[0U] 
        = loadQueue__DOT____Vlvbound_hd66c37fb__0;
    vlSelfRef.__PVT__loadQueue__DOT__unnamedblk6__DOT__si = 1U;
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [0U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [0U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [0U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [0U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xfffeU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | (IData)(loadQueue__DOT____Vlvbound_h2103b133__0));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [1U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [1U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [1U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [1U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xfffdU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 1U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [2U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [2U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [2U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [2U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xfffbU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 2U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [3U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [3U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [3U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [3U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xfff7U & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 3U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [4U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [4U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [4U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [4U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xffefU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 4U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [5U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [5U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [5U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [5U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xffdfU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 5U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [6U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [6U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [6U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [6U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xffbfU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 6U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [7U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [7U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [7U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [7U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xff7fU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 7U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [8U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [8U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [8U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [8U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xfeffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 8U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [9U] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [9U] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [9U] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [9U] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xfdffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 9U));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [0xaU] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [0xaU] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [0xaU] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [0xaU] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xfbffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 0xaU));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [0xbU] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [0xbU] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [0xbU] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [0xbU] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xf7ffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 0xbU));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [0xcU] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [0xcU] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [0xcU] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [0xcU] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xefffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 0xcU));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [0xdU] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [0xdU] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [0xdU] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [0xdU] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xdfffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 0xdU));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [0xeU] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [0xeU] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [0xeU] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [0xeU] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0xbfffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 0xeU));
    loadQueue__DOT____Vlvbound_h2103b133__0 = ((((IData)(
                                                         (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                          [0xfU] 
                                                          >> 0x29U)) 
                                                 & (IData)(
                                                           (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                            [0xfU] 
                                                            >> 0x2aU))) 
                                                & ((0xfffffU 
                                                    & (IData)(
                                                              (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                               [0xfU] 
                                                               >> 0x15U))) 
                                                   == 
                                                   vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
                                                   [0U])) 
                                               & ((IData)(
                                                          (vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                                           [0xfU] 
                                                           >> 0x14U)) 
                                                  & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                                                  [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__addrMatch[0U] 
        = ((0x7fffU & vlSelfRef.__PVT__loadQueue__DOT__addrMatch
            [0U]) | ((IData)(loadQueue__DOT____Vlvbound_h2103b133__0) 
                     << 0xfU));
    vlSelfRef.__PVT__loadQueue__DOT__unnamedblk7__DOT__unnamedblk8__DOT__lqe = 0x10U;
    vlSelfRef.__PVT__loadQueue__DOT__unnamedblk7__DOT__si = 1U;
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeStore
        [0U]) {
        if (vlSelfRef.__PVT__loadQueue__DOT__picked
            [0U]) {
            vlSelfRef.loadQueue__DOT____Vlvbound_h177e4fd5__0 = 1U;
            vlSelfRef.__PVT__loadQueue__DOT__violation[0U] 
                = vlSelfRef.loadQueue__DOT____Vlvbound_h177e4fd5__0;
            vlSelfRef.loadQueue__DOT____Vlvbound_h1a4b3925__0 
                = (0xfffffU & (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueue
                                      [vlSelfRef.__PVT__loadQueue__DOT__pickedPtr
                                      [0U]]));
            vlSelfRef.__PVT__loadQueue__DOT__conflictLoadPC[0U] 
                = vlSelfRef.loadQueue__DOT____Vlvbound_h1a4b3925__0;
        } else {
            vlSelfRef.loadQueue__DOT____Vlvbound_h177e4fd5__0 = 0U;
            vlSelfRef.__PVT__loadQueue__DOT__violation[0U] 
                = vlSelfRef.loadQueue__DOT____Vlvbound_h177e4fd5__0;
        }
    } else {
        vlSelfRef.__PVT__loadQueue__DOT__violation[0U] = 0U;
    }
    vlSelfRef.__PVT__loadQueue__DOT__unnamedblk9__DOT__si = 1U;
    {
        if ((1U & (~ vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeLoad
                   [0U]))) {
            goto __Vlabel124;
        }
        if ((vlSelfRef.__PVT__loadQueue__DOT__executedLoadAddr
             [0U] != vlSelfRef.__PVT__loadQueue__DOT__executedStoreAddr
             [0U])) {
            goto __Vlabel124;
        }
        if ((1U & (~ (vlSelfRef.__PVT__loadQueue__DOT__executedLoadWordRE
                      [0U] & vlSelfRef.__PVT__loadQueue__DOT__executedStoreWordWE
                      [0U])))) {
            goto __Vlabel124;
        }
        if ((([&]() {
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__head 
                            = vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead;
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__ptr 
                            = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadQueuePtrByLoad
                            [0U];
                        vlSelf->__Vfunc_LoadQueuePtrToAge__581__age = VL_RAND_RESET_I(5);
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__age 
                            = vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__ptr;
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__Vfuncout 
                            = (0x1fU & (((IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__ptr) 
                                         < (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__head))
                                         ? ((IData)(0x10U) 
                                            + (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__age))
                                         : (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__age)));
                    }(), (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__581__Vfuncout)) 
             >= ([&]() {
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__head 
                            = vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead;
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__ptr 
                            = vlSelfRef.__PVT__loadQueue__DOT__executedLoadQueuePtrByStore
                            [0U];
                        vlSelf->__Vfunc_LoadQueuePtrToAge__582__age = VL_RAND_RESET_I(5);
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__age 
                            = vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__ptr;
                        vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__Vfuncout 
                            = (0x1fU & (((IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__ptr) 
                                         < (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__head))
                                         ? ((IData)(0x10U) 
                                            + (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__age))
                                         : (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__age)));
                    }(), (IData)(vlSelfRef.__Vfunc_LoadQueuePtrToAge__582__Vfuncout)))) {
            vlSelfRef.loadQueue__DOT____Vlvbound_ha8f38411__0 = 1U;
            vlSelfRef.__PVT__loadQueue__DOT__violation[0U] 
                = vlSelfRef.loadQueue__DOT____Vlvbound_ha8f38411__0;
            vlSelfRef.loadQueue__DOT____Vlvbound_h8fa27459__0 
                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadPC
                [0U];
            vlSelfRef.__PVT__loadQueue__DOT__conflictLoadPC[0U] 
                = vlSelfRef.loadQueue__DOT____Vlvbound_h8fa27459__0;
        }
        __Vlabel124: ;
    }
    vlSelfRef.__PVT__loadQueue__DOT__unnamedblk10__DOT__unnamedblk11__DOT__li = 1U;
    vlSelfRef.__PVT__loadQueue__DOT__unnamedblk10__DOT__si = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__conflictLoadPC[0U] 
        = vlSelfRef.__PVT__loadQueue__DOT__conflictLoadPC
        [0U];
    loadQueue__DOT____Vlvbound_hba8df09a__0 = vlSelfRef.__PVT__loadQueue__DOT__violation
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__conflict[0U] 
        = loadQueue__DOT____Vlvbound_hba8df09a__0;
    vlSelfRef.__PVT__loadQueue__DOT__unnamedblk12__DOT__i = 1U;
    vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
        = (0x7fffffffU & ((vlSelfRef.__PVT__loadQueue__DOT__addrMatch
                           [0U] << 0x10U) | vlSelfRef.__PVT__loadQueue__DOT__addrMatch
                          [0U]));
    vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
        = ((3U == (3U & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail) 
                         >> 2U))) ? (0x7ffffU & (vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                                 >> 0xcU))
            : ((2U == (3U & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail) 
                             >> 2U))) ? (0x7ffffU & 
                                         (vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                          >> 8U)) : 
               ((1U == (3U & ((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail) 
                              >> 2U))) ? (0x7ffffU 
                                          & (vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                             >> 4U))
                 : (0x7ffffU & vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp))));
    vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
        = ((3U == (3U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail)))
            ? (0xffffU & (vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                          >> 3U)) : ((2U == (3U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail)))
                                      ? (0xffffU & 
                                         (vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                          >> 2U)) : 
                                     ((1U == (3U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail)))
                                       ? (0xffffU & 
                                          (vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                           >> 1U)) : 
                                      (0xffffU & vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp))));
    vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq 
        = (0xffffU & vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp);
    vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 0U;
    vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0U;
    if ((1U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((2U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 1U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((4U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 2U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((8U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 3U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x10U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 4U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x20U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 5U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x40U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 6U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x80U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 7U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x100U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 8U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x200U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 9U;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x400U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xaU;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x800U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xbU;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x1000U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xcU;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x2000U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xdU;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x4000U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xeU;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x8000U & (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xfU;
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr 
        = (0xfU & ((0x10U > ((IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant) 
                             + (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail)))
                    ? ((IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant) 
                       + (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail))
                    : ((IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant) 
                       + (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail))));
    if (((IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail) 
         >= vlSelfRef.__PVT__loadQueue__DOT__executedLoadQueuePtrByStore
         [0U])) {
        if (((((IData)(0x10U) + vlSelfRef.__PVT__loadQueue__DOT__executedLoadQueuePtrByStore
               [0U]) - (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail)) 
             > (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant))) {
            vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 0U;
        }
    } else if (((0xfU & (vlSelfRef.__PVT__loadQueue__DOT__executedLoadQueuePtrByStore
                         [0U] - (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail))) 
                > (IData)(vlSelfRef.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant))) {
        vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 0U;
    }
    vlSelfRef.__PVT__loadQueue__DOT__picked[0U] = vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked;
    vlSelfRef.__PVT__loadQueue__DOT__pickedPtr[0U] 
        = vlSelfRef.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__37(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__37\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ complexExStage__DOT____Vlvbound_ha13f6e41__0;
    complexExStage__DOT____Vlvbound_ha13f6e41__0 = 0;
    CData/*0:0*/ complexExStage__DOT____Vlvbound_h1a7582f2__0;
    complexExStage__DOT____Vlvbound_h1a7582f2__0 = 0;
    // Body
    complexExStage__DOT____Vlvbound_ha13f6e41__0 = 
        (1U == (7U & (vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                      [0U][4U] >> 0x17U)));
    vlSelfRef.__PVT__complexExStage__DOT__isDiv[0U] 
        = complexExStage__DOT____Vlvbound_ha13f6e41__0;
    complexExStage__DOT____Vlvbound_h1a7582f2__0 = 
        ((((vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReserved
            [0U] & (vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                    [0U][5U] >> 0xbU)) & vlSelfRef.__PVT__complexExStage__DOT__isDiv
           [0U]) & (IData)((vlSelfRef.__PVT__complexExStage__DOT__fuOpA
                            [0U] >> 0x20U))) & (IData)(
                                                       (vlSelfRef.__PVT__complexExStage__DOT__fuOpB
                                                        [0U] 
                                                        >> 0x20U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReq[0U] 
        = complexExStage__DOT____Vlvbound_h1a7582f2__0;
    vlSelfRef.mulDivUnit__DOT____Vcellout__BlockDivUnit__BRA__0__KET____DOT__divUnit__finished 
        = (0U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase));
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReq
        [0U]) {
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivCode 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divCode
            [0U];
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInA
            [0U];
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextIsSigned 
            = (1U & (~ ((1U == vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divCode
                         [0U]) | (3U == vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divCode
                                  [0U]))));
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInB
            [0U];
    } else {
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivCode 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivCode;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDividend;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextIsSigned 
            = (1U & (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regIsSigned));
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivisor;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReq
        [0U]) {
        if (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextIsSigned) {
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextZ 
                = ((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend 
                    >> 0x1fU) ? (0x100000000ULL | (QData)((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend)))
                    : (QData)((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend)));
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextD 
                = ((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor 
                    >> 0x1fU) ? (0x100000000ULL | (QData)((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor)))
                    : (QData)((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor)));
        } else {
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextZ 
                = (QData)((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend));
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextD 
                = (QData)((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor));
        }
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ = 0ULL;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
            = ((1U & (IData)((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextZ 
                              >> 0x20U))) ? 0x1ffffffffULL
                : 0ULL);
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextSigned 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextIsSigned;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextPhase = 1U;
        vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__dividend 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextZ;
        vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__Vfuncout = 1U;
        vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i = 0x1fU;
        {
            while (VL_LTES_III(32, 0U, vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i)) {
                if (((1U & (IData)((vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__dividend 
                                    >> 0x20U))) != 
                     ((0x20U >= (0x3fU & vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i)) 
                      && (1U & (IData)((vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__dividend 
                                        >> (0x3fU & vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i))))))) {
                    vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__Vfuncout 
                        = (0x3fU & ((IData)(2U) + vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i));
                    goto __Vlabel125;
                }
                vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i 
                    = (vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i 
                       - (IData)(1U));
            }
            __Vlabel125: ;
        }
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter 
            = vlSelfRef.__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__Vfuncout;
    } else {
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextZ 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextD 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextSigned 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regSigned;
        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter 
            = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter;
        if ((1U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase))) {
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                = (0x1ffffffffULL & (VL_SHIFTL_QQI(33,33,32, vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR, 1U) 
                                     | (QData)((IData)(
                                                       ((0x20U 
                                                         >= 
                                                         (0x3fU 
                                                          & ((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter) 
                                                             - (IData)(1U)))) 
                                                        && (1U 
                                                            & (IData)(
                                                                      (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ 
                                                                       >> 
                                                                       (0x3fU 
                                                                        & ((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter) 
                                                                           - (IData)(1U)))))))))));
            if (((1U & (IData)((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR 
                                >> 0x20U))) != (1U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD 
                                                           >> 0x20U))))) {
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                    = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                         + vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ = 0x1ffffffffULL;
            } else {
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                    = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                         - vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ = 0ULL;
            }
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter 
                = (0x3fU & ((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter) 
                            - (IData)(1U)));
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextPhase 
                = ((0U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter))
                    ? 3U : 2U);
        } else if ((2U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase))) {
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                = (0x1ffffffffULL & (VL_SHIFTL_QQI(33,33,32, vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR, 1U) 
                                     | (QData)((IData)(
                                                       ((0x20U 
                                                         >= 
                                                         (0x3fU 
                                                          & ((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter) 
                                                             - (IData)(1U)))) 
                                                        && (1U 
                                                            & (IData)(
                                                                      (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ 
                                                                       >> 
                                                                       (0x3fU 
                                                                        & ((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter) 
                                                                           - (IData)(1U)))))))))));
            if (((1U & (IData)((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR 
                                >> 0x20U))) != (1U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD 
                                                           >> 0x20U))))) {
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                    = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                         + vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                    = (0x1ffffffffULL & VL_SHIFTL_QQI(33,33,32, vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ, 1U));
            } else {
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                    = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                         - vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                    = (0x1ffffffffULL & (1ULL | VL_SHIFTL_QQI(33,33,32, vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ, 1U)));
            }
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter 
                = (0x3fU & ((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter) 
                            - (IData)(1U)));
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextPhase 
                = ((0U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter))
                    ? 3U : 2U);
        } else if ((3U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase))) {
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                = (0x1ffffffffULL & (1ULL + VL_SHIFTL_QQI(33,33,32, vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ, 1U)));
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR;
            if ((0ULL != vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR)) {
                if ((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR 
                     == vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD)) {
                    vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                        = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                             - vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                    vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                        = (0x1ffffffffULL & (1ULL + vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ));
                } else if ((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR 
                            == (0x1ffffffffULL & (- vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD)))) {
                    vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                        = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                             + vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                    vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                        = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                                             - 1ULL));
                } else if (((1U & (IData)((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR 
                                           >> 0x20U))) 
                            != (1U & (IData)((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ 
                                              >> 0x20U))))) {
                    if (((1U & (IData)((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR 
                                        >> 0x20U))) 
                         != (1U & (IData)((vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD 
                                           >> 0x20U))))) {
                        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                            = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                                 + vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                            = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                                                 - 1ULL));
                    } else {
                        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                            = (0x1ffffffffULL & (vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                                                 - vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD));
                        vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                            = (0x1ffffffffULL & (1ULL 
                                                 + vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ));
                    }
                }
            }
            if ((0ULL == vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD)) {
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ = 0x1ffffffffULL;
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR 
                    = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ;
            } else if ((((IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regSigned) 
                         & (0x80000000ULL == vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ)) 
                        & (0xffffffffULL == vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD))) {
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ 
                    = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ;
                vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR = 0ULL;
            }
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextPhase = 0U;
        } else {
            vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextPhase 
                = vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase;
        }
    }
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__quotient 
        = (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ);
    vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__remainder 
        = (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR);
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divDataOut[0U] 
        = (((0U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivCode)) 
            | (1U == (IData)(vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivCode)))
            ? vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__quotient
            : vlSelfRef.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__remainder);
    vlSelfRef.__PVT__mulDivUnit__DOT__finished[0U] 
        = vlSelfRef.mulDivUnit__DOT____Vcellout__BlockDivUnit__BRA__0__KET____DOT__divUnit__finished;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__39(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__39\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.__PVT__npStage__DOT__predNextPC = 
            (0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwCommit);
    } else if (vlSelfRef.__PVT__rnStage__DOT__regFlush) {
        vlSelfRef.__PVT__npStage__DOT__predNextPC = vlSelfRef.__PVT__rnStage__DOT__regRecoveredPC;
    } else {
        vlSelfRef.__PVT__npStage__DOT__predNextPC = vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcOut;
        vlSelfRef.__PVT__npStage__DOT__predNextPC = 
            ((0x7ffffU & vlSelfRef.__PVT__npStage__DOT__predNextPC) 
             | ((IData)(vlSelfRef.__PVT__npStage__DOT__threadCounter) 
                << 0x13U));
        vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i = 0U;
        {
            while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i)) {
                if (((((~ (IData)(vlSelfRef.__PVT__npStage__DOT__regStall)) 
                       & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__fetchStageIsValid
                       [(1U & vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i)]) 
                      & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit
                      [(1U & vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i)]) 
                     & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken
                     [(1U & vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i)])) {
                    vlSelfRef.__PVT__npStage__DOT__predNextPC 
                        = vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut
                        [(1U & vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i)];
                    vlSelfRef.__PVT__npStage__DOT__predNextPC 
                        = ((0x7ffffU & vlSelfRef.__PVT__npStage__DOT__predNextPC) 
                           | ((IData)(vlSelfRef.__PVT__npStage__DOT__threadCounter) 
                              << 0x13U));
                    goto __Vlabel126;
                }
                vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i 
                    = ((IData)(1U) + vlSelfRef.__PVT__npStage__DOT__unnamedblk1__DOT__i);
            }
            __Vlabel126: ;
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__predNextPC 
        = vlSelfRef.__PVT__npStage__DOT__predNextPC;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__41(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__41\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ replayQueue__DOT____Vlvbound_heb5c08ab__0;
    replayQueue__DOT____Vlvbound_heb5c08ab__0 = 0;
    VlWide<5>/*138:0*/ replayQueue__DOT____Vlvbound_hfcb7ab53__0;
    VL_ZERO_W(139, replayQueue__DOT____Vlvbound_hfcb7ab53__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_hb8fbf5e1__0;
    replayQueue__DOT____Vlvbound_hb8fbf5e1__0 = 0;
    VlWide<3>/*81:0*/ replayQueue__DOT____Vlvbound_h33fcaa62__0;
    VL_ZERO_W(82, replayQueue__DOT____Vlvbound_h33fcaa62__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h7f16fea4__0;
    replayQueue__DOT____Vlvbound_h7f16fea4__0 = 0;
    VlWide<4>/*124:0*/ replayQueue__DOT____Vlvbound_he966880c__0;
    VL_ZERO_W(125, replayQueue__DOT____Vlvbound_he966880c__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_hb5870313__0;
    replayQueue__DOT____Vlvbound_hb5870313__0 = 0;
    VlWide<3>/*92:0*/ replayQueue__DOT____Vlvbound_h297fb8fe__0;
    VL_ZERO_W(93, replayQueue__DOT____Vlvbound_h297fb8fe__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h501ddca9__0;
    replayQueue__DOT____Vlvbound_h501ddca9__0 = 0;
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h9df06bd0__0;
    replayQueue__DOT____Vlvbound_h9df06bd0__0 = 0;
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h164510e2__0;
    replayQueue__DOT____Vlvbound_h164510e2__0 = 0;
    VlWide<5>/*138:0*/ replayQueue__DOT____Vlvbound_h2b7bb9d2__0;
    VL_ZERO_W(139, replayQueue__DOT____Vlvbound_h2b7bb9d2__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h26bfef00__0;
    replayQueue__DOT____Vlvbound_h26bfef00__0 = 0;
    VlWide<3>/*81:0*/ replayQueue__DOT____Vlvbound_h01e5c618__0;
    VL_ZERO_W(82, replayQueue__DOT____Vlvbound_h01e5c618__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_hfe3aa704__0;
    replayQueue__DOT____Vlvbound_hfe3aa704__0 = 0;
    VlWide<4>/*124:0*/ replayQueue__DOT____Vlvbound_h858224db__0;
    VL_ZERO_W(125, replayQueue__DOT____Vlvbound_h858224db__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h4b79c4ba__0;
    replayQueue__DOT____Vlvbound_h4b79c4ba__0 = 0;
    VlWide<3>/*92:0*/ replayQueue__DOT____Vlvbound_h3055f0e2__0;
    VL_ZERO_W(93, replayQueue__DOT____Vlvbound_h3055f0e2__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h66b90636__0;
    replayQueue__DOT____Vlvbound_h66b90636__0 = 0;
    VlWide<3>/*81:0*/ replayQueue__DOT____Vlvbound_hb644f52a__0;
    VL_ZERO_W(82, replayQueue__DOT____Vlvbound_hb644f52a__0);
    CData/*0:0*/ replayQueue__DOT____Vlvbound_h43fc87c6__0;
    replayQueue__DOT____Vlvbound_h43fc87c6__0 = 0;
    VlWide<3>/*92:0*/ replayQueue__DOT____Vlvbound_h07b9a337__0;
    VL_ZERO_W(93, replayQueue__DOT____Vlvbound_h07b9a337__0);
    // Body
    replayQueue__DOT____Vlvbound_heb5c08ab__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordEntry
        [0U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U] 
        = ((0xbfU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U]) 
           | (0xffU & ((IData)(replayQueue__DOT____Vlvbound_heb5c08ab__0) 
                       << 6U)));
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [0U][0U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [0U][1U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[2U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [0U][2U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[3U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [0U][3U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[4U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [0U][4U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU] 
        = ((0xffffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU]) 
           | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[0U] 
              << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xeU] 
        = ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[0U] 
            >> 0x10U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[1U] 
                         << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xfU] 
        = ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[1U] 
            >> 0x10U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[2U] 
                         << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x10U] 
        = ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[2U] 
            >> 0x10U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[3U] 
                         << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x11U] 
        = ((0xf8000000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x11U]) 
           | ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[3U] 
               >> 0x10U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[4U] 
                            << 0x10U)));
    replayQueue__DOT____Vlvbound_heb5c08ab__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordEntry
        [1U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U] 
        = ((0x7fU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U]) 
           | (0xffU & ((IData)(replayQueue__DOT____Vlvbound_heb5c08ab__0) 
                       << 7U)));
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [1U][0U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [1U][1U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[2U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [1U][2U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[3U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [1U][3U];
    replayQueue__DOT____Vlvbound_hfcb7ab53__0[4U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData
        [1U][4U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x11U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x11U]) 
           | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[0U] 
              << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x12U] 
        = ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[0U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[1U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x13U] 
        = ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[1U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[2U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x14U] 
        = ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[2U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[3U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x15U] 
        = ((replayQueue__DOT____Vlvbound_hfcb7ab53__0[3U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_hfcb7ab53__0[4U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U] 
        = ((0xc0U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U]) 
           | (0xffU & (replayQueue__DOT____Vlvbound_hfcb7ab53__0[4U] 
                       >> 5U)));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk5__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_hb8fbf5e1__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordEntry
        [0U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU] 
        = ((0xffff7fffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU]) 
           | ((IData)(replayQueue__DOT____Vlvbound_hb8fbf5e1__0) 
              << 0xfU));
    replayQueue__DOT____Vlvbound_h33fcaa62__0[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
        [0U][0U];
    replayQueue__DOT____Vlvbound_h33fcaa62__0[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
        [0U][1U];
    replayQueue__DOT____Vlvbound_h33fcaa62__0[2U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
        [0U][2U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU] 
        = ((0x1fffffffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU]) 
           | (replayQueue__DOT____Vlvbound_h33fcaa62__0[0U] 
              << 0x1dU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xbU] 
        = ((replayQueue__DOT____Vlvbound_h33fcaa62__0[0U] 
            >> 3U) | (replayQueue__DOT____Vlvbound_h33fcaa62__0[1U] 
                      << 0x1dU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xcU] 
        = ((replayQueue__DOT____Vlvbound_h33fcaa62__0[1U] 
            >> 3U) | (replayQueue__DOT____Vlvbound_h33fcaa62__0[2U] 
                      << 0x1dU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU] 
        = ((0xffff8000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU]) 
           | (replayQueue__DOT____Vlvbound_h33fcaa62__0[2U] 
              >> 3U));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk6__DOT__i = 1U;
    replayQueue__DOT____Vlvbound_h7f16fea4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordEntry
        [0U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU]) 
           | ((IData)(replayQueue__DOT____Vlvbound_h7f16fea4__0) 
              << 0x1bU));
    replayQueue__DOT____Vlvbound_he966880c__0[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [0U][0U];
    replayQueue__DOT____Vlvbound_he966880c__0[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [0U][1U];
    replayQueue__DOT____Vlvbound_he966880c__0[2U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [0U][2U];
    replayQueue__DOT____Vlvbound_he966880c__0[3U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [0U][3U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[3U] 
        = ((1U & vlSelfRef.__PVT__replayQueue__DOT__recordData[3U]) 
           | (replayQueue__DOT____Vlvbound_he966880c__0[0U] 
              << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[4U] 
        = ((replayQueue__DOT____Vlvbound_he966880c__0[0U] 
            >> 0x1fU) | (replayQueue__DOT____Vlvbound_he966880c__0[1U] 
                         << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[5U] 
        = ((replayQueue__DOT____Vlvbound_he966880c__0[1U] 
            >> 0x1fU) | (replayQueue__DOT____Vlvbound_he966880c__0[2U] 
                         << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[6U] 
        = ((0xc0000000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[6U]) 
           | ((replayQueue__DOT____Vlvbound_he966880c__0[2U] 
               >> 0x1fU) | (replayQueue__DOT____Vlvbound_he966880c__0[3U] 
                            << 1U)));
    replayQueue__DOT____Vlvbound_h7f16fea4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordEntry
        [1U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU] 
        = ((0xefffffffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU]) 
           | ((IData)(replayQueue__DOT____Vlvbound_h7f16fea4__0) 
              << 0x1cU));
    replayQueue__DOT____Vlvbound_he966880c__0[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [1U][0U];
    replayQueue__DOT____Vlvbound_he966880c__0[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [1U][1U];
    replayQueue__DOT____Vlvbound_he966880c__0[2U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [1U][2U];
    replayQueue__DOT____Vlvbound_he966880c__0[3U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
        [1U][3U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[6U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[6U]) 
           | (replayQueue__DOT____Vlvbound_he966880c__0[0U] 
              << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[7U] 
        = ((replayQueue__DOT____Vlvbound_he966880c__0[0U] 
            >> 2U) | (replayQueue__DOT____Vlvbound_he966880c__0[1U] 
                      << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[8U] 
        = ((replayQueue__DOT____Vlvbound_he966880c__0[1U] 
            >> 2U) | (replayQueue__DOT____Vlvbound_he966880c__0[2U] 
                      << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[9U] 
        = ((replayQueue__DOT____Vlvbound_he966880c__0[2U] 
            >> 2U) | (replayQueue__DOT____Vlvbound_he966880c__0[3U] 
                      << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU] 
        = ((0xf8000000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU]) 
           | (replayQueue__DOT____Vlvbound_he966880c__0[3U] 
              >> 2U));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk7__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_hb5870313__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordEntry
        [0U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[3U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__replayQueue__DOT__recordData[3U]) 
           | (IData)(replayQueue__DOT____Vlvbound_hb5870313__0));
    replayQueue__DOT____Vlvbound_h297fb8fe__0[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
        [0U][0U];
    replayQueue__DOT____Vlvbound_h297fb8fe__0[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
        [0U][1U];
    replayQueue__DOT____Vlvbound_h297fb8fe__0[2U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
        [0U][2U];
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0U] 
        = ((7U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0U]) 
           | (replayQueue__DOT____Vlvbound_h297fb8fe__0[0U] 
              << 3U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[1U] 
        = ((replayQueue__DOT____Vlvbound_h297fb8fe__0[0U] 
            >> 0x1dU) | (replayQueue__DOT____Vlvbound_h297fb8fe__0[1U] 
                         << 3U));
    vlSelfRef.__PVT__replayQueue__DOT__recordData[2U] 
        = ((replayQueue__DOT____Vlvbound_h297fb8fe__0[1U] 
            >> 0x1dU) | (replayQueue__DOT____Vlvbound_h297fb8fe__0[2U] 
                         << 3U));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk8__DOT__i = 1U;
    vlSelfRef.__PVT__replayQueue__DOT__recordData[0U] 
        = ((0xfffffff8U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0U]) 
           | (IData)(vlSelfRef.__PVT__replayQueue__DOT__intervalIn));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U] 
            = (0x3fU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U]);
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk9__DOT__i = 2U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk10__DOT__i = 1U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk11__DOT__i = 2U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk12__DOT__i = 1U;
        vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU] 
            = (0xffff7fffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU]);
        vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU] 
            = (0xe7ffffffU & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU]);
        vlSelfRef.__PVT__replayQueue__DOT__recordData[3U] 
            = (0xfffffffeU & vlSelfRef.__PVT__replayQueue__DOT__recordData[3U]);
        vlSelfRef.__PVT__replayQueue__DOT__recordData[0U] 
            = (0xfffffff8U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0U]);
        vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 0U;
        vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 0U;
    } else {
        vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 0U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk13__DOT__i = 2U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk14__DOT__i = 1U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk15__DOT__i = 2U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk16__DOT__i = 1U;
        if ((0x40U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 1U;
        }
        if ((0x80U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 1U;
        }
        if ((0x8000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 1U;
        }
        if ((0x8000000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 1U;
        }
        if ((0x10000000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 1U;
        }
        if ((1U & vlSelfRef.__PVT__replayQueue__DOT__recordData[3U])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn = 1U;
        }
        vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 0U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk17__DOT__i = 2U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk18__DOT__i = 1U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk19__DOT__i = 2U;
        vlSelfRef.__PVT__replayQueue__DOT__unnamedblk20__DOT__i = 1U;
        if ((0x40U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x16U])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 1U;
        }
        if ((0x80U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x16U])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 1U;
        }
        if ((0x8000U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xdU])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 1U;
        }
        if ((0x8000000U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 1U;
        }
        if ((0x10000000U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 1U;
        }
        if ((1U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[3U])) {
            vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut = 1U;
        }
    }
    vlSelfRef.__PVT__replayQueue__DOT__flushInt[0U] 
        = (([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__flushAllInsns 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeTailPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeHeadPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange 
                    = (0U != (IData)(vlSelfRef.__PVT__replayQueue__DOT__canBeFlushedEntryCount));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__518__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                            goto __Vlabel127;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                                goto __Vlabel127;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                                goto __Vlabel127;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                                goto __Vlabel127;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                                goto __Vlabel127;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                                goto __Vlabel127;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                            goto __Vlabel127;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                    }
                    __Vlabel127: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout)) 
           | ([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__flushAllInsns 
                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 4U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange 
                    = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                    >> 0x15U)));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__519__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                            goto __Vlabel128;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                                goto __Vlabel128;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                                goto __Vlabel128;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                                goto __Vlabel128;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                                goto __Vlabel128;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                                goto __Vlabel128;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                            goto __Vlabel128;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                    }
                    __Vlabel128: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout)));
    vlSelfRef.__PVT__replayQueue__DOT__flushInt[1U] 
        = (([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                >> 0x15U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__flushAllInsns 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeTailPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeHeadPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange 
                    = (0U != (IData)(vlSelfRef.__PVT__replayQueue__DOT__canBeFlushedEntryCount));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__518__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                            goto __Vlabel129;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                                goto __Vlabel129;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                                goto __Vlabel129;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                                goto __Vlabel129;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 1U;
                                goto __Vlabel129;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                                goto __Vlabel129;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                            goto __Vlabel129;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout = 0U;
                    }
                    __Vlabel129: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__518__Vfuncout)) 
           | ([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                >> 0x15U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__flushAllInsns 
                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 4U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange 
                    = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                    >> 0x15U)));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__519__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                            goto __Vlabel130;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                                goto __Vlabel130;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                                goto __Vlabel130;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                                goto __Vlabel130;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 1U;
                                goto __Vlabel130;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                                goto __Vlabel130;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                            goto __Vlabel130;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout = 0U;
                    }
                    __Vlabel130: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__519__Vfuncout)));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk21__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_h501ddca9__0 = (([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                >> 0x17U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__flushAllInsns 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__tailPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeTailPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__headPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeHeadPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__detectRange 
                    = (0U != (IData)(vlSelfRef.__PVT__replayQueue__DOT__canBeFlushedEntryCount));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__520__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__520__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 1U;
                            goto __Vlabel131;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 1U;
                                goto __Vlabel131;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 0U;
                                goto __Vlabel131;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 1U;
                                goto __Vlabel131;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 1U;
                                goto __Vlabel131;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 0U;
                                goto __Vlabel131;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 0U;
                            goto __Vlabel131;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout = 0U;
                    }
                    __Vlabel131: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__520__Vfuncout)) 
                                                 | ([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                >> 0x17U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__flushAllInsns 
                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__tailPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 4U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__headPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__detectRange 
                    = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                    >> 0x15U)));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__521__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__521__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 1U;
                            goto __Vlabel132;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 1U;
                                goto __Vlabel132;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 0U;
                                goto __Vlabel132;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 1U;
                                goto __Vlabel132;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 1U;
                                goto __Vlabel132;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 0U;
                                goto __Vlabel132;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 0U;
                            goto __Vlabel132;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout = 0U;
                    }
                    __Vlabel132: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__521__Vfuncout)));
    vlSelfRef.__PVT__replayQueue__DOT__flushComplex[0U] 
        = replayQueue__DOT____Vlvbound_h501ddca9__0;
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk22__DOT__i = 1U;
    vlSelfRef.__PVT__replayQueue__DOT__flushMem[0U] 
        = (([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr 
                    = (0x3fU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                 << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                           >> 0x1bU)));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__flushAllInsns 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeTailPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeHeadPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange 
                    = (0U != (IData)(vlSelfRef.__PVT__replayQueue__DOT__canBeFlushedEntryCount));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__522__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                            goto __Vlabel133;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                                goto __Vlabel133;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                                goto __Vlabel133;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                                goto __Vlabel133;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                                goto __Vlabel133;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                                goto __Vlabel133;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                            goto __Vlabel133;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                    }
                    __Vlabel133: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout)) 
           | ([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr 
                    = (0x3fU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                 << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                           >> 0x1bU)));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__flushAllInsns 
                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 4U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange 
                    = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                    >> 0x15U)));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__523__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                            goto __Vlabel134;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                                goto __Vlabel134;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                                goto __Vlabel134;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                                goto __Vlabel134;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                                goto __Vlabel134;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                                goto __Vlabel134;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                            goto __Vlabel134;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                    }
                    __Vlabel134: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout)));
    vlSelfRef.__PVT__replayQueue__DOT__flushMem[1U] 
        = (([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                >> 0x18U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__flushAllInsns 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeTailPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeHeadPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange 
                    = (0U != (IData)(vlSelfRef.__PVT__replayQueue__DOT__canBeFlushedEntryCount));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__522__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                            goto __Vlabel135;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                                goto __Vlabel135;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                                goto __Vlabel135;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                                goto __Vlabel135;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 1U;
                                goto __Vlabel135;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                                goto __Vlabel135;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                            goto __Vlabel135;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout = 0U;
                    }
                    __Vlabel135: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__522__Vfuncout)) 
           | ([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr 
                    = (0x3fU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                >> 0x18U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__flushAllInsns 
                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 4U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange 
                    = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                    >> 0x15U)));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__523__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                            goto __Vlabel136;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                                goto __Vlabel136;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                                goto __Vlabel136;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                                goto __Vlabel136;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 1U;
                                goto __Vlabel136;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                                goto __Vlabel136;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                            goto __Vlabel136;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout = 0U;
                    }
                    __Vlabel136: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__523__Vfuncout)));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk23__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_h9df06bd0__0 = (([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__opPtr 
                    = (0x3fU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                 << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                           >> 0x1dU)));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__flushAllInsns 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__tailPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeTailPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__headPtr 
                    = vlSelfRef.__PVT__replayQueue__DOT__flushRangeHeadPtr;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__detectRange 
                    = (0U != (IData)(vlSelfRef.__PVT__replayQueue__DOT__canBeFlushedEntryCount));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__524__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__524__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 1U;
                            goto __Vlabel137;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 1U;
                                goto __Vlabel137;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 0U;
                                goto __Vlabel137;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 1U;
                                goto __Vlabel137;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 1U;
                                goto __Vlabel137;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 0U;
                                goto __Vlabel137;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 0U;
                            goto __Vlabel137;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout = 0U;
                    }
                    __Vlabel137: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__524__Vfuncout)) 
                                                 | ([&]() {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__opPtr 
                    = (0x3fU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                 << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                           >> 0x1dU)));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__flushAllInsns 
                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__tailPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 4U));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__headPtr 
                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                >> 0xaU));
                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__detectRange 
                    = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                    >> 0x15U)));
                {
                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__525__detectRange) {
                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__525__flushAllInsns) {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 1U;
                            goto __Vlabel138;
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__tailPtr) 
                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__opPtr) 
                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 1U;
                                goto __Vlabel138;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 0U;
                                goto __Vlabel138;
                            }
                        } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__detectRange) 
                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__tailPtr) 
                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__headPtr)))) {
                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__opPtr) 
                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__headPtr)) 
                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__opPtr) 
                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 1U;
                                goto __Vlabel138;
                            } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__opPtr) 
                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__headPtr)) 
                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__opPtr) 
                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__tailPtr)))) {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 1U;
                                goto __Vlabel138;
                            } else {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 0U;
                                goto __Vlabel138;
                            }
                        } else {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 0U;
                            goto __Vlabel138;
                        }
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout = 0U;
                    }
                    __Vlabel138: ;
                }
            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__525__Vfuncout)));
    vlSelfRef.__PVT__replayQueue__DOT__flushFP[0U] 
        = replayQueue__DOT____Vlvbound_h9df06bd0__0;
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk24__DOT__i = 1U;
    vlSelfRef.__PVT__replayQueue__DOT__almostFull = 
        (0x11U <= (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regCount));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 0U;
        vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
    } else {
        if ((0x14U == (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regCount))) {
            vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 0U;
        } else if (((~ (IData)(vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn)) 
                    & (0U == (IData)(vlSelfRef.__PVT__replayQueue__DOT__validInstCount)))) {
            vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 0U;
        } else {
            vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 0U;
            if ((0x40U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U])) {
                vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 1U;
            }
            if ((0x80U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0x16U])) {
                vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 1U;
            }
            vlSelfRef.__PVT__replayQueue__DOT__unnamedblk25__DOT__i = 2U;
            if ((0x8000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xdU])) {
                vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 1U;
            }
            vlSelfRef.__PVT__replayQueue__DOT__unnamedblk26__DOT__i = 1U;
            if ((0x8000000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU])) {
                vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 1U;
            }
            if ((0x10000000U & vlSelfRef.__PVT__replayQueue__DOT__recordData[0xaU])) {
                vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 1U;
            }
            vlSelfRef.__PVT__replayQueue__DOT__unnamedblk27__DOT__i = 2U;
            if ((1U & vlSelfRef.__PVT__replayQueue__DOT__recordData[3U])) {
                vlSelfRef.__PVT__replayQueue__DOT__pushEntry = 1U;
            }
            vlSelfRef.__PVT__replayQueue__DOT__unnamedblk28__DOT__i = 1U;
        }
        if ((0U == (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regCount))) {
            vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
        } else if (((IData)(vlSelfRef.__PVT__replayQueue__DOT__intervalCount) 
                    < (7U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0U]))) {
            vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
        } else if (vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut) {
            vlSelfRef.__PVT__replayQueue__DOT__popEntry = 1U;
            if (((((IData)((((0U == (0x38000U & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[6U])) 
                             & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                >> 0x1bU)) & (~ vlSelfRef.__PVT__replayQueue__DOT__flushMem
                                              [0U]))) 
                   & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                      >> 2U)) & (IData)(vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid)) 
                 & (IData)(vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady))) {
                vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
            }
            if (((((IData)(((0x10000000U == (0x10007000U 
                                             & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU])) 
                            & (~ vlSelfRef.__PVT__replayQueue__DOT__flushMem
                               [1U]))) & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                          >> 0x1fU)) 
                  & ((IData)(vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid) 
                     >> 1U)) & ((IData)(vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady) 
                                >> 1U))) {
                vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
            }
            vlSelfRef.__PVT__replayQueue__DOT__unnamedblk29__DOT__i = 2U;
            if ((((IData)((((0x28000U == (0x38000U 
                                          & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[6U])) 
                            & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                               >> 0x1bU)) & (~ vlSelfRef.__PVT__replayQueue__DOT__flushMem
                                             [0U]))) 
                  & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                     >> 0xbU)) & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheFlushManagerIF.__PVT__cacheFlushComplete)))) {
                vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
            }
            if (((((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                    >> 0xfU) & (~ vlSelfRef.__PVT__replayQueue__DOT__flushComplex
                                [0U])) & (1U == (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                                 >> 0x1dU))) 
                 & vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divBusy
                 [0U])) {
                vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
            }
            vlSelfRef.__PVT__replayQueue__DOT__unnamedblk30__DOT__i = 1U;
            if ((((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                   & (~ vlSelfRef.__PVT__replayQueue__DOT__flushFP
                      [0U])) & ((2U == (7U & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                              >> 0x11U))) 
                                | (3U == (7U & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                                >> 0x11U))))) 
                 & vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Busy
                 [0U])) {
                vlSelfRef.__PVT__replayQueue__DOT__popEntry = 0U;
            }
            vlSelfRef.__PVT__replayQueue__DOT__unnamedblk31__DOT__i = 1U;
        } else {
            vlSelfRef.__PVT__replayQueue__DOT__popEntry = 1U;
        }
    }
    replayQueue__DOT____Vlvbound_h164510e2__0 = (((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
                                                  & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x16U] 
                                                     >> 6U)) 
                                                 & (~ 
                                                    vlSelfRef.__PVT__replayQueue__DOT__flushInt
                                                    [0U]));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x16U] 
        = ((0xbfU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x16U]) 
           | (0xffU & ((IData)(replayQueue__DOT____Vlvbound_h164510e2__0) 
                       << 6U)));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
          << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                       >> 0x10U));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
          << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                       >> 0x10U));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[2U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
          << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                       >> 0x10U));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[3U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
          << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                       >> 0x10U));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[4U] = 
        (0x7ffU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                   >> 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
        = ((0xffffU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xdU]) 
           | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[0U] 
              << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
        = ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[0U] 
            >> 0x10U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[1U] 
                         << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
        = ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[1U] 
            >> 0x10U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[2U] 
                         << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
        = ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[2U] 
            >> 0x10U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[3U] 
                         << 0x10U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
        = ((0xf8000000U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x11U]) 
           | ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[3U] 
               >> 0x10U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[4U] 
                            << 0x10U)));
    replayQueue__DOT____Vlvbound_h164510e2__0 = (((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
                                                  & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x16U] 
                                                     >> 7U)) 
                                                 & (~ 
                                                    vlSelfRef.__PVT__replayQueue__DOT__flushInt
                                                    [1U]));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x16U] 
        = ((0x7fU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x16U]) 
           | (0xffU & ((IData)(replayQueue__DOT____Vlvbound_h164510e2__0) 
                       << 7U)));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
          << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                    >> 0x1bU));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
          << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                    >> 0x1bU));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[2U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
          << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                    >> 0x1bU));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[3U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
          << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                    >> 0x1bU));
    replayQueue__DOT____Vlvbound_h2b7bb9d2__0[4U] = 
        (0x7ffU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x16U] 
                    << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                              >> 0x1bU)));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
        = ((0x7ffffffU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x11U]) 
           | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[0U] 
              << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
        = ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[0U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[1U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
        = ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[1U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[2U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x14U] 
        = ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[2U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[3U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
        = ((replayQueue__DOT____Vlvbound_h2b7bb9d2__0[3U] 
            >> 5U) | (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[4U] 
                      << 0x1bU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x16U] 
        = ((0xc0U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0x16U]) 
           | (0xffU & (replayQueue__DOT____Vlvbound_h2b7bb9d2__0[4U] 
                       >> 5U)));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk32__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_h26bfef00__0 = (((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
                                                  & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                                     >> 0xfU)) 
                                                 & (~ 
                                                    vlSelfRef.__PVT__replayQueue__DOT__flushComplex
                                                    [0U]));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
        = ((0xffff7fffU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xdU]) 
           | ((IData)(replayQueue__DOT____Vlvbound_h26bfef00__0) 
              << 0xfU));
    replayQueue__DOT____Vlvbound_h01e5c618__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
          << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                    >> 0x1dU));
    replayQueue__DOT____Vlvbound_h01e5c618__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
          << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                    >> 0x1dU));
    replayQueue__DOT____Vlvbound_h01e5c618__0[2U] = 
        (0x3ffffU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                      << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                >> 0x1dU)));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
        = ((0x1fffffffU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU]) 
           | (replayQueue__DOT____Vlvbound_h01e5c618__0[0U] 
              << 0x1dU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xbU] 
        = ((replayQueue__DOT____Vlvbound_h01e5c618__0[0U] 
            >> 3U) | (replayQueue__DOT____Vlvbound_h01e5c618__0[1U] 
                      << 0x1dU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
        = ((replayQueue__DOT____Vlvbound_h01e5c618__0[1U] 
            >> 3U) | (replayQueue__DOT____Vlvbound_h01e5c618__0[2U] 
                      << 0x1dU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
        = ((0xffff8000U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xdU]) 
           | (replayQueue__DOT____Vlvbound_h01e5c618__0[2U] 
              >> 3U));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk33__DOT__i = 1U;
    replayQueue__DOT____Vlvbound_hfe3aa704__0 = (((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
                                                  & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                                     >> 0x1bU)) 
                                                 & (~ 
                                                    vlSelfRef.__PVT__replayQueue__DOT__flushMem
                                                    [0U]));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
        = ((0xf7ffffffU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU]) 
           | ((IData)(replayQueue__DOT____Vlvbound_hfe3aa704__0) 
              << 0x1bU));
    replayQueue__DOT____Vlvbound_h858224db__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[4U] 
          << 0x1fU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                       >> 1U));
    replayQueue__DOT____Vlvbound_h858224db__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[5U] 
          << 0x1fU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                       >> 1U));
    replayQueue__DOT____Vlvbound_h858224db__0[2U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[6U] 
          << 0x1fU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                       >> 1U));
    replayQueue__DOT____Vlvbound_h858224db__0[3U] = 
        (0x1fffffffU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                        >> 1U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
        = ((1U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[3U]) 
           | (replayQueue__DOT____Vlvbound_h858224db__0[0U] 
              << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
        = ((replayQueue__DOT____Vlvbound_h858224db__0[0U] 
            >> 0x1fU) | (replayQueue__DOT____Vlvbound_h858224db__0[1U] 
                         << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
        = ((replayQueue__DOT____Vlvbound_h858224db__0[1U] 
            >> 0x1fU) | (replayQueue__DOT____Vlvbound_h858224db__0[2U] 
                         << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
        = ((0xc0000000U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[6U]) 
           | ((replayQueue__DOT____Vlvbound_h858224db__0[2U] 
               >> 0x1fU) | (replayQueue__DOT____Vlvbound_h858224db__0[3U] 
                            << 1U)));
    replayQueue__DOT____Vlvbound_hfe3aa704__0 = (((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
                                                  & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                                     >> 0x1cU)) 
                                                 & (~ 
                                                    vlSelfRef.__PVT__replayQueue__DOT__flushMem
                                                    [1U]));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
        = ((0xefffffffU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU]) 
           | ((IData)(replayQueue__DOT____Vlvbound_hfe3aa704__0) 
              << 0x1cU));
    replayQueue__DOT____Vlvbound_h858224db__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[7U] 
          << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                    >> 0x1eU));
    replayQueue__DOT____Vlvbound_h858224db__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[8U] 
          << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                    >> 0x1eU));
    replayQueue__DOT____Vlvbound_h858224db__0[2U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[9U] 
          << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                    >> 0x1eU));
    replayQueue__DOT____Vlvbound_h858224db__0[3U] = 
        (0x1fffffffU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                         << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                   >> 0x1eU)));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[6U]) 
           | (replayQueue__DOT____Vlvbound_h858224db__0[0U] 
              << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[7U] 
        = ((replayQueue__DOT____Vlvbound_h858224db__0[0U] 
            >> 2U) | (replayQueue__DOT____Vlvbound_h858224db__0[1U] 
                      << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
        = ((replayQueue__DOT____Vlvbound_h858224db__0[1U] 
            >> 2U) | (replayQueue__DOT____Vlvbound_h858224db__0[2U] 
                      << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
        = ((replayQueue__DOT____Vlvbound_h858224db__0[2U] 
            >> 2U) | (replayQueue__DOT____Vlvbound_h858224db__0[3U] 
                      << 0x1eU));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
        = ((0xf8000000U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0xaU]) 
           | (replayQueue__DOT____Vlvbound_h858224db__0[3U] 
              >> 2U));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk34__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_h4b79c4ba__0 = (((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
                                                  & vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[3U]) 
                                                 & (~ 
                                                    vlSelfRef.__PVT__replayQueue__DOT__flushFP
                                                    [0U]));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[3U]) 
           | (IData)(replayQueue__DOT____Vlvbound_h4b79c4ba__0));
    replayQueue__DOT____Vlvbound_h3055f0e2__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[1U] 
          << 0x1dU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                       >> 3U));
    replayQueue__DOT____Vlvbound_h3055f0e2__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[2U] 
          << 0x1dU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                       >> 3U));
    replayQueue__DOT____Vlvbound_h3055f0e2__0[2U] = 
        (vlSelfRef.__PVT__replayQueue__DOT__replayEntryOut[2U] 
         >> 3U);
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
        = ((7U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0U]) 
           | (replayQueue__DOT____Vlvbound_h3055f0e2__0[0U] 
              << 3U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
        = ((replayQueue__DOT____Vlvbound_h3055f0e2__0[0U] 
            >> 0x1dU) | (replayQueue__DOT____Vlvbound_h3055f0e2__0[1U] 
                         << 3U));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
        = ((replayQueue__DOT____Vlvbound_h3055f0e2__0[1U] 
            >> 0x1dU) | (replayQueue__DOT____Vlvbound_h3055f0e2__0[2U] 
                         << 3U));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk35__DOT__i = 1U;
    vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
        = ((0xfffffff8U & vlSelfRef.__PVT__replayQueue__DOT__nextReplayEntry[0U]) 
           | (7U & vlSelfRef.__PVT__replayQueue__DOT__replayQueue__DOT__array
              [vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regHeadStorage][0U]));
    vlSelfRef.__PVT__replayQueue__DOT__nextReplay = 
        ((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
         & (IData)(vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayEntry[0U] 
        = (1U & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x16U] 
                 >> 6U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[0U][0U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xeU] 
            << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xdU] 
                         >> 0x10U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[0U][1U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xfU] 
            << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xeU] 
                         >> 0x10U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[0U][2U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x10U] 
            << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xfU] 
                         >> 0x10U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[0U][3U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x11U] 
            << 0x10U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x10U] 
                         >> 0x10U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[0U][4U] 
        = (0x7ffU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x11U] 
                     >> 0x10U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayEntry[1U] 
        = (1U & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x16U] 
                 >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[1U][0U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x12U] 
            << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x11U] 
                      >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[1U][1U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x13U] 
            << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x12U] 
                      >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[1U][2U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x14U] 
            << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x13U] 
                      >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[1U][3U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x15U] 
            << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x14U] 
                      >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData[1U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x16U] 
                      << 5U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0x15U] 
                                >> 0x1bU)));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk36__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_h66b90636__0 = (1U 
                                                 & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xdU] 
                                                    >> 0xfU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayEntry[0U] 
        = replayQueue__DOT____Vlvbound_h66b90636__0;
    replayQueue__DOT____Vlvbound_hb644f52a__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xbU] 
          << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xaU] 
                    >> 0x1dU));
    replayQueue__DOT____Vlvbound_hb644f52a__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xcU] 
          << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xbU] 
                    >> 0x1dU));
    replayQueue__DOT____Vlvbound_hb644f52a__0[2U] = 
        (0x3ffffU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xdU] 
                      << 3U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xcU] 
                                >> 0x1dU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData[0U][0U] 
        = replayQueue__DOT____Vlvbound_hb644f52a__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData[0U][1U] 
        = replayQueue__DOT____Vlvbound_hb644f52a__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData[0U][2U] 
        = replayQueue__DOT____Vlvbound_hb644f52a__0[2U];
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk37__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayEntry[0U] 
        = (1U & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xaU] 
                 >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[0U][0U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[4U] 
            << 0x1fU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[3U] 
                         >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[0U][1U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[5U] 
            << 0x1fU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[4U] 
                         >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[0U][2U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[6U] 
            << 0x1fU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[5U] 
                         >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[0U][3U] 
        = (0x1fffffffU & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[6U] 
                          >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayEntry[1U] 
        = (1U & (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xaU] 
                 >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[1U][0U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[7U] 
            << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[6U] 
                      >> 0x1eU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[1U][1U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[8U] 
            << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[7U] 
                      >> 0x1eU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[1U][2U] 
        = ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[9U] 
            << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[8U] 
                      >> 0x1eU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData[1U][3U] 
        = (0x1fffffffU & ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0xaU] 
                           << 2U) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[9U] 
                                     >> 0x1eU)));
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk38__DOT__i = 2U;
    replayQueue__DOT____Vlvbound_h43fc87c6__0 = (1U 
                                                 & vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[3U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayEntry[0U] 
        = replayQueue__DOT____Vlvbound_h43fc87c6__0;
    replayQueue__DOT____Vlvbound_h07b9a337__0[0U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[1U] 
          << 0x1dU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[0U] 
                       >> 3U));
    replayQueue__DOT____Vlvbound_h07b9a337__0[1U] = 
        ((vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[2U] 
          << 0x1dU) | (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[1U] 
                       >> 3U));
    replayQueue__DOT____Vlvbound_h07b9a337__0[2U] = 
        (vlSelfRef.__PVT__replayQueue__DOT__replayEntryReg[2U] 
         >> 3U);
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData[0U][0U] 
        = replayQueue__DOT____Vlvbound_h07b9a337__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData[0U][1U] 
        = replayQueue__DOT____Vlvbound_h07b9a337__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData[0U][2U] 
        = replayQueue__DOT____Vlvbound_h07b9a337__0[2U];
    vlSelfRef.__PVT__replayQueue__DOT__unnamedblk39__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStageStallUpper 
        = ((IData)(vlSelfRef.__PVT__replayQueue__DOT__replayReg) 
           | (IData)(vlSelfRef.__PVT__replayQueue__DOT__almostFull));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__controller__DOT__scStage = 1U;
        vlSelfRef.__PVT__controller__DOT__isStage = 1U;
    } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStageStallUpper) {
        vlSelfRef.__PVT__controller__DOT__scStage = 2U;
        vlSelfRef.__PVT__controller__DOT__isStage = 2U;
    } else {
        vlSelfRef.__PVT__controller__DOT__scStage = 0U;
        vlSelfRef.__PVT__controller__DOT__isStage = 0U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay 
        = vlSelfRef.__PVT__replayQueue__DOT__replayReg;
    vlSelfRef.__PVT__replayQueue__DOT__nextIntervalIn 
        = vlSelfRef.__PVT__replayQueue__DOT__intervalIn;
    vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextTailStorage 
        = vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regTailStorage;
    vlSelfRef.__PVT__replayQueue__DOT__nextIntervalCount 
        = vlSelfRef.__PVT__replayQueue__DOT__intervalCount;
    vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextHeadStorage 
        = vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regHeadStorage;
    vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextCount 
        = vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regCount;
    if (vlSelfRef.__PVT__replayQueue__DOT__pushEntry) {
        vlSelfRef.__PVT__replayQueue__DOT__nextIntervalIn = 0U;
        vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextTailStorage 
            = ((0x13U == (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regTailStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextTailStorage))));
        vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextCount 
            = (0x3fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextCount)));
    } else if ((7U > (IData)(vlSelfRef.__PVT__replayQueue__DOT__intervalIn))) {
        vlSelfRef.__PVT__replayQueue__DOT__nextIntervalIn 
            = (7U & ((IData)(1U) + (IData)(vlSelfRef.__PVT__replayQueue__DOT__nextIntervalIn)));
    }
    if (vlSelfRef.__PVT__replayQueue__DOT__popEntry) {
        vlSelfRef.__PVT__replayQueue__DOT__nextIntervalCount = 0U;
        vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextHeadStorage 
            = ((0x13U == (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__regHeadStorage))
                ? 0U : (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextHeadStorage))));
        vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextCount 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__replayQueue__DOT__pointer__DOT__nextCount) 
                        - (IData)(1U)));
    } else if ((7U > (IData)(vlSelfRef.__PVT__replayQueue__DOT__intervalCount))) {
        vlSelfRef.__PVT__replayQueue__DOT__nextIntervalCount 
            = (7U & ((IData)(1U) + (IData)(vlSelfRef.__PVT__replayQueue__DOT__nextIntervalCount)));
    }
    vlSelfRef.__PVT__replayQueue__DOT__validInstCountNext 
        = vlSelfRef.__PVT__replayQueue__DOT__validInstCount;
    if (((IData)(vlSelfRef.__PVT__replayQueue__DOT__pushEntry) 
         & (IData)(vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidIn))) {
        vlSelfRef.__PVT__replayQueue__DOT__validInstCountNext 
            = (0x3fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__replayQueue__DOT__validInstCountNext)));
    }
    if (((IData)(vlSelfRef.__PVT__replayQueue__DOT__popEntry) 
         & (IData)(vlSelfRef.__PVT__replayQueue__DOT__replayEntryValidOut))) {
        vlSelfRef.__PVT__replayQueue__DOT__validInstCountNext 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__replayQueue__DOT__validInstCountNext) 
                        - (IData)(1U)));
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__scStage 
        = vlSelfRef.__PVT__controller__DOT__scStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage 
        = vlSelfRef.__PVT__controller__DOT__isStage;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__42(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__42\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__ifStage__DOT__brPred[0U] = ((0x1fffULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [0U]) 
                                                 | ((QData)((IData)(
                                                                    (0xfffffU 
                                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken
                                                                        [0U]
                                                                         ? 
                                                                        vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut
                                                                        [0U]
                                                                         : 
                                                                        ((IData)(4U) 
                                                                         + 
                                                                         (0xfffffU 
                                                                          & vlSelfRef.__PVT__ifStage__DOT__pipeReg
                                                                          [0U])))))) 
                                                    << 0xdU));
    vlSelfRef.__PVT__ifStage__DOT__brPred[0U] = ((0x1ffffefffULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [0U]) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken
                                                                    [0U])) 
                                                    << 0xcU));
    vlSelfRef.__PVT__ifStage__DOT__brPred[0U] = ((0x1fffff003ULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [0U]) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory
                                                                    [0U])) 
                                                    << 2U));
    vlSelfRef.__PVT__ifStage__DOT__brPred[0U] = ((0x1fffffffcULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [0U]) 
                                                 | (IData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue
                                                                   [0U])));
    vlSelfRef.__PVT__ifStage__DOT__brPred[1U] = ((0x1fffULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [1U]) 
                                                 | ((QData)((IData)(
                                                                    (0xfffffU 
                                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken
                                                                        [1U]
                                                                         ? 
                                                                        vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut
                                                                        [1U]
                                                                         : 
                                                                        ((IData)(4U) 
                                                                         + 
                                                                         (0xfffffU 
                                                                          & vlSelfRef.__PVT__ifStage__DOT__pipeReg
                                                                          [1U])))))) 
                                                    << 0xdU));
    vlSelfRef.__PVT__ifStage__DOT__brPred[1U] = ((0x1ffffefffULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [1U]) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken
                                                                    [1U])) 
                                                    << 0xcU));
    vlSelfRef.__PVT__ifStage__DOT__brPred[1U] = ((0x1fffff003ULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [1U]) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory
                                                                    [1U])) 
                                                    << 2U));
    vlSelfRef.__PVT__ifStage__DOT__brPred[1U] = ((0x1fffffffcULL 
                                                  & vlSelfRef.__PVT__ifStage__DOT__brPred
                                                  [1U]) 
                                                 | (IData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue
                                                                   [1U])));
    vlSelfRef.__PVT__ifStage__DOT__unnamedblk5__DOT__i = 2U;
    vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i)) {
            vlSelfRef.__PVT__ifStage__DOT__isFlushed[(1U 
                                                      & vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i)] = 0U;
            if ((1U & (((~ (IData)(vlSelfRef.__PVT__ifStage__DOT__regStall)) 
                        & (vlSelfRef.__PVT__ifStage__DOT__pipeReg
                           [(1U & vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i)] 
                           >> 0x14U)) & (IData)((vlSelfRef.__PVT__ifStage__DOT__brPred
                                                 [(1U 
                                                   & vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i)] 
                                                 >> 0xcU))))) {
                vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j 
                    = ((IData)(1U) + vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i);
                while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j)) {
                    vlSelfRef.__PVT__ifStage__DOT__isFlushed[(1U 
                                                              & vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j)] 
                        = (1U & (vlSelfRef.__PVT__ifStage__DOT__pipeReg
                                 [(1U & vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j)] 
                                 >> 0x14U));
                    vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j 
                        = ((IData)(1U) + vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j);
                }
                goto __Vlabel139;
            }
            vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__ifStage__DOT__unnamedblk6__DOT__i);
        }
        __Vlabel139: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__updateBrHistory[0U] 
        = (1U & ((~ (IData)(vlSelfRef.__PVT__ifStage__DOT__regStall)) 
                 & (vlSelfRef.__PVT__ifStage__DOT__pipeReg
                    [0U] >> 0x14U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__updateBrHistory[1U] 
        = (1U & ((~ (IData)(vlSelfRef.__PVT__ifStage__DOT__regStall)) 
                 & (vlSelfRef.__PVT__ifStage__DOT__pipeReg
                    [1U] >> 0x14U)));
    vlSelfRef.__PVT__ifStage__DOT__unnamedblk8__DOT__i = 2U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__43(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__43\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*21:0*/ __Vfunc_ToPhyAddrFromLogical__4__Vfuncout;
    __Vfunc_ToPhyAddrFromLogical__4__Vfuncout = 0;
    IData/*31:0*/ __Vfunc_ToPhyAddrFromLogical__4__logAddr;
    __Vfunc_ToPhyAddrFromLogical__4__logAddr = 0;
    CData/*31:0*/ __Vtemp_2;
    CData/*31:0*/ __Vtemp_3;
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__interruptAddrWE) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn[vlSelfRef.__PVT__npStage__DOT__threadCounter] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__interruptAddrIn;
        vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn[vlSelfRef.__PVT__npStage__DOT__threadCounter] 
            = ((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn
                [vlSelfRef.__PVT__npStage__DOT__threadCounter]) 
               | ((IData)(vlSelfRef.__PVT__npStage__DOT__threadCounter) 
                  << 0x13U));
    } else if (vlSelfRef.__PVT__npStage__DOT__beginStall) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn[vlSelfRef.__PVT__npStage__DOT__threadCounter] 
            = vlSelfRef.__PVT__npStage__DOT__predNextPC;
    } else {
        vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn[vlSelfRef.__PVT__npStage__DOT__threadCounter] 
            = (0xfffffU & ((IData)(8U) + vlSelfRef.__PVT__npStage__DOT__predNextPC));
        vlSelfRef.__PVT__npStage__DOT__unnamedblk2__DOT__i = 1U;
        {
            while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__npStage__DOT__unnamedblk2__DOT__i)) {
                if (([&]() {
                            vlSelfRef.__Vfunc_StepOverCacheLine__0__pc2 
                                = (0xfffffU & (vlSelfRef.__PVT__npStage__DOT__predNextPC 
                                               + VL_SHIFTL_III(20,32,32, vlSelfRef.__PVT__npStage__DOT__unnamedblk2__DOT__i, 2U)));
                            vlSelfRef.__Vfunc_StepOverCacheLine__0__pc1 
                                = vlSelfRef.__PVT__npStage__DOT__predNextPC;
                            vlSelfRef.__Vfunc_StepOverCacheLine__0__Vfuncout 
                                = ((1U & (vlSelfRef.__Vfunc_StepOverCacheLine__0__pc1 
                                          >> 3U)) != 
                                   (1U & (vlSelfRef.__Vfunc_StepOverCacheLine__0__pc2 
                                          >> 3U)));
                        }(), (IData)(vlSelfRef.__Vfunc_StepOverCacheLine__0__Vfuncout))) {
                    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn[vlSelfRef.__PVT__npStage__DOT__threadCounter] 
                        = (0xfffffU & (vlSelfRef.__PVT__npStage__DOT__predNextPC 
                                       + VL_SHIFTL_III(20,32,32, vlSelfRef.__PVT__npStage__DOT__unnamedblk2__DOT__i, 2U)));
                    goto __Vlabel140;
                }
                vlSelfRef.__PVT__npStage__DOT__unnamedblk2__DOT__i 
                    = ((IData)(1U) + vlSelfRef.__PVT__npStage__DOT__unnamedblk2__DOT__i);
            }
            __Vlabel140: ;
        }
    }
    if (vlSelfRef.__PVT__npStage__DOT__threadCounter) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn[0U] 
            = (1U & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcOut);
    }
    if ((1U & (~ (IData)(vlSelfRef.__PVT__npStage__DOT__threadCounter)))) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn[1U] 
            = (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcOut 
                     >> 1U));
    }
    vlSelfRef.__PVT__npStage__DOT__unnamedblk3__DOT__t = 2U;
    vlSelfRef.__PVT__npStage__DOT__nextStage[0U] = 
        ((0x1fffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
          [0U]) | ((IData)(vlSelfRef.__PVT__npStage__DOT__sidFF__DOT__body) 
                   << 0x15U));
    vlSelfRef.__PVT__npStage__DOT__nextStage[0U] = 
        ((0x7ff00000U & vlSelfRef.__PVT__npStage__DOT__nextStage
          [0U]) | vlSelfRef.__PVT__npStage__DOT__predNextPC);
    vlSelfRef.__PVT__npStage__DOT__nextStage[0U] = 
        ((0x7ff7ffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
          [0U]) | ((IData)(vlSelfRef.__PVT__npStage__DOT__threadCounter) 
                   << 0x13U));
    __Vtemp_2 = (1U & (~ (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__interruptAddrWE) 
                           | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStage)) 
                          | ([&]() {
                        vlSelfRef.__Vfunc_StepOverCacheLine__1__pc2 
                            = (0xfffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
                               [0U]);
                        vlSelfRef.__Vfunc_StepOverCacheLine__1__pc1 
                            = vlSelfRef.__PVT__npStage__DOT__predNextPC;
                        vlSelfRef.__Vfunc_StepOverCacheLine__1__Vfuncout 
                            = ((1U & (vlSelfRef.__Vfunc_StepOverCacheLine__1__pc1 
                                      >> 3U)) != (1U 
                                                  & (vlSelfRef.__Vfunc_StepOverCacheLine__1__pc2 
                                                     >> 3U)));
                    }(), (IData)(vlSelfRef.__Vfunc_StepOverCacheLine__1__Vfuncout)))));
    vlSelfRef.__PVT__npStage__DOT__nextStage[0U] = 
        ((0x7fefffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
          [0U]) | (__Vtemp_2 << 0x14U));
    vlSelfRef.__PVT__npStage__DOT__nextStage[1U] = 
        ((0x1fffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
          [1U]) | (0x7fe00000U & (((IData)(1U) + (IData)(vlSelfRef.__PVT__npStage__DOT__sidFF__DOT__body)) 
                                  << 0x15U)));
    vlSelfRef.__PVT__npStage__DOT__nextStage[1U] = 
        ((0x7ff00000U & vlSelfRef.__PVT__npStage__DOT__nextStage
          [1U]) | (0xfffffU & ((IData)(4U) + vlSelfRef.__PVT__npStage__DOT__predNextPC)));
    vlSelfRef.__PVT__npStage__DOT__nextStage[1U] = 
        ((0x7ff7ffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
          [1U]) | ((IData)(vlSelfRef.__PVT__npStage__DOT__threadCounter) 
                   << 0x13U));
    __Vtemp_3 = (1U & (~ (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__interruptAddrWE) 
                           | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStage)) 
                          | ([&]() {
                        vlSelfRef.__Vfunc_StepOverCacheLine__1__pc2 
                            = (0xfffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
                               [1U]);
                        vlSelfRef.__Vfunc_StepOverCacheLine__1__pc1 
                            = vlSelfRef.__PVT__npStage__DOT__predNextPC;
                        vlSelfRef.__Vfunc_StepOverCacheLine__1__Vfuncout 
                            = ((1U & (vlSelfRef.__Vfunc_StepOverCacheLine__1__pc1 
                                      >> 3U)) != (1U 
                                                  & (vlSelfRef.__Vfunc_StepOverCacheLine__1__pc2 
                                                     >> 3U)));
                    }(), (IData)(vlSelfRef.__Vfunc_StepOverCacheLine__1__Vfuncout)))));
    vlSelfRef.__PVT__npStage__DOT__nextStage[1U] = 
        ((0x7fefffffU & vlSelfRef.__PVT__npStage__DOT__nextStage
          [1U]) | (__Vtemp_3 << 0x14U));
    vlSelfRef.__PVT__npStage__DOT__unnamedblk4__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage[0U] 
        = vlSelfRef.__PVT__npStage__DOT__nextStage[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage[1U] 
        = vlSelfRef.__PVT__npStage__DOT__nextStage[1U];
    vlSelfRef.__PVT__npStage__DOT__fetchAddr = ((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__fetchStageIsValid
                                                 [0U] 
                                                 & (IData)(vlSelfRef.__PVT__npStage__DOT__stall))
                                                 ? 
                                                ([&]() {
                vlSelfRef.__Vfunc_ToAddrFromPC__2__pc 
                    = vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__fetchStagePC
                    [0U];
                vlSelfRef.__Vfunc_ToAddrFromPC__2__Vfuncout 
                    = ((0x80000000U & (vlSelfRef.__Vfunc_ToAddrFromPC__2__pc 
                                       << 0xdU)) | 
                       (0x3ffffU & vlSelfRef.__Vfunc_ToAddrFromPC__2__pc));
            }(), vlSelfRef.__Vfunc_ToAddrFromPC__2__Vfuncout)
                                                 : 
                                                ([&]() {
                vlSelfRef.__Vfunc_ToAddrFromPC__3__pc 
                    = vlSelfRef.__PVT__npStage__DOT__predNextPC;
                vlSelfRef.__Vfunc_ToAddrFromPC__3__Vfuncout 
                    = ((0x80000000U & (vlSelfRef.__Vfunc_ToAddrFromPC__3__pc 
                                       << 0xdU)) | 
                       (0x3ffffU & vlSelfRef.__Vfunc_ToAddrFromPC__3__pc));
            }(), vlSelfRef.__Vfunc_ToAddrFromPC__3__Vfuncout));
    __Vfunc_ToPhyAddrFromLogical__4__logAddr = vlSelfRef.__PVT__npStage__DOT__fetchAddr;
    vlSelf->__Vfunc_ToPhyAddrFromLogical__4__phyAddr = VL_RAND_RESET_I(22);
    if ((0x40002000U == __Vfunc_ToPhyAddrFromLogical__4__logAddr)) {
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr = 0x302000U;
    } else if (((0x40000000U <= __Vfunc_ToPhyAddrFromLogical__4__logAddr) 
                & (0x40000010U > __Vfunc_ToPhyAddrFromLogical__4__logAddr))) {
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr 
            = (0x300000U | (0xfU & __Vfunc_ToPhyAddrFromLogical__4__logAddr));
    } else if (((0x80040000U <= __Vfunc_ToPhyAddrFromLogical__4__logAddr) 
                & (0x80050000U > __Vfunc_ToPhyAddrFromLogical__4__logAddr))) {
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr 
            = (0x200000U | (0xfffffU & vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr));
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr 
            = ((0x300000U & vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr) 
               | (0xfffffU & ((IData)(0x10000U) + (0x7ffffU 
                                                   & __Vfunc_ToPhyAddrFromLogical__4__logAddr))));
    } else if (((0x1000U <= __Vfunc_ToPhyAddrFromLogical__4__logAddr) 
                & (0x10000U > __Vfunc_ToPhyAddrFromLogical__4__logAddr))) {
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr 
            = (0x1ffffU & __Vfunc_ToPhyAddrFromLogical__4__logAddr);
    } else if (((0x80000000U <= __Vfunc_ToPhyAddrFromLogical__4__logAddr) 
                & (0x80040000U > __Vfunc_ToPhyAddrFromLogical__4__logAddr))) {
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr 
            = (0xfffffU & vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr);
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr 
            = ((0x300000U & vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr) 
               | (0xfffffU & ((IData)(0x10000U) + (0x3ffffU 
                                                   & __Vfunc_ToPhyAddrFromLogical__4__logAddr))));
    } else {
        vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr = 0xcccccU;
    }
    __Vfunc_ToPhyAddrFromLogical__4__Vfuncout = vlSelfRef.__Vfunc_ToPhyAddrFromLogical__4__phyAddr;
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__icNextReadAddrIn 
        = __Vfunc_ToPhyAddrFromLogical__4__Vfuncout;
    vlSelfRef.__PVT__npStage__DOT__numValidInsns = 0U;
    vlSelfRef.__PVT__npStage__DOT__unnamedblk5__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__npStage__DOT__unnamedblk5__DOT__i)) {
            if ((0x100000U & vlSelfRef.__PVT__npStage__DOT__nextStage
                 [(1U & vlSelfRef.__PVT__npStage__DOT__unnamedblk5__DOT__i)])) {
                vlSelfRef.__PVT__npStage__DOT__numValidInsns 
                    = (7U & ((IData)(1U) + (IData)(vlSelfRef.__PVT__npStage__DOT__numValidInsns)));
            } else {
                goto __Vlabel141;
            }
            vlSelfRef.__PVT__npStage__DOT__unnamedblk5__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__npStage__DOT__unnamedblk5__DOT__i);
        }
        __Vlabel141: ;
    }
    vlSelfRef.__PVT__npStage__DOT__nextSID = (0x3ffU 
                                              & ((1U 
                                                  & ((IData)(vlSelfRef.__PVT__npStage__DOT__stall) 
                                                     | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStage)))
                                                  ? (IData)(vlSelfRef.__PVT__npStage__DOT__sidFF__DOT__body)
                                                  : 
                                                 ((IData)(vlSelfRef.__PVT__npStage__DOT__sidFF__DOT__body) 
                                                  + (IData)(vlSelfRef.__PVT__npStage__DOT__numValidInsns))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg[0U] 
        = ((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
            [0U]) | (0x400U & (((~ (IData)(vlSelfRef.__PVT__npStage__DOT__stall)) 
                                << 0xaU) & (vlSelfRef.__PVT__npStage__DOT__nextStage
                                            [0U] >> 0xaU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg[0U] 
        = ((0x400U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
            [0U]) | (0x3ffU & (vlSelfRef.__PVT__npStage__DOT__nextStage
                               [0U] >> 0x15U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg[1U] 
        = ((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
            [1U]) | (0x400U & (((~ (IData)(vlSelfRef.__PVT__npStage__DOT__stall)) 
                                << 0xaU) & (vlSelfRef.__PVT__npStage__DOT__nextStage
                                            [1U] >> 0xaU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg[1U] 
        = ((0x400U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
            [1U]) | (0x3ffU & (vlSelfRef.__PVT__npStage__DOT__nextStage
                               [1U] >> 0x15U)));
    vlSelfRef.__PVT__npStage__DOT__unnamedblk6__DOT__i = 2U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__44(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__44\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__531__detectRange;
    __Vfunc_SelectiveFlushDetector__531__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__531__headPtr;
    __Vfunc_SelectiveFlushDetector__531__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__531__tailPtr;
    __Vfunc_SelectiveFlushDetector__531__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__531__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__531__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__531__opPtr;
    __Vfunc_SelectiveFlushDetector__531__opPtr = 0;
    // Body
    vlSelfRef.__PVT__intIsStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage) 
                                                >> 1U));
    vlSelfRef.__PVT__intIsStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage));
    vlSelfRef.__PVT__intIsStage__DOT__issueQueuePtr[0U] 
        = (0xfU & vlSelfRef.__PVT__intIsStage__DOT__pipeReg
           [0U]);
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay) {
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [0U][0U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [0U][1U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [0U][2U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [0U][3U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][4U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [0U][4U];
        vlSelfRef.__PVT__intIsStage__DOT__valid[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayEntry
            [0U];
    } else {
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [0U][0U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [0U][1U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [0U][2U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [0U][3U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[0U][4U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [0U][4U];
        vlSelfRef.__PVT__intIsStage__DOT__valid[0U] 
            = (1U & ((~ (IData)(vlSelfRef.__PVT__intIsStage__DOT__stall)) 
                     & (vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                        [0U] >> 4U)));
    }
    __Vfunc_SelectiveFlushDetector__531__opPtr = (vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__531__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__531__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__531__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__531__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__531__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__531__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                goto __Vlabel142;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__531__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                    goto __Vlabel142;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
                    goto __Vlabel142;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__531__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                    goto __Vlabel142;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                    goto __Vlabel142;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
                    goto __Vlabel142;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
                goto __Vlabel142;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
        }
        __Vlabel142: ;
    }
    vlSelfRef.__PVT__intIsStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssue[0U] 
        = (((~ (IData)(vlSelfRef.__PVT__intIsStage__DOT__clear)) 
            & vlSelfRef.__PVT__intIsStage__DOT__valid
            [0U]) & (~ vlSelfRef.__PVT__intIsStage__DOT__flush
                     [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuePtr[0U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issueQueuePtr
        [0U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[0U][4U] 
        = ((0xfff7ffU & vlSelfRef.__PVT__intIsStage__DOT__nextStage
            [0U][4U]) | (0xffffffU & (((~ (((IData)(vlSelfRef.__PVT__intIsStage__DOT__clear) 
                                            | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                           | vlSelfRef.__PVT__intIsStage__DOT__flush
                                           [0U])) & 
                                       vlSelfRef.__PVT__intIsStage__DOT__valid
                                       [0U]) << 0xbU)));
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [0U][0U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [0U][1U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [0U][2U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [0U][3U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[0U][4U] 
        = ((0xfff800U & vlSelfRef.__PVT__intIsStage__DOT__nextStage
            [0U][4U]) | (0xffffffU & vlSelfRef.__PVT__intIsStage__DOT__issuedData
                         [0U][4U]));
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[0U][4U] 
        = ((0xfffU & vlSelfRef.__PVT__intIsStage__DOT__nextStage
            [0U][4U]) | (0xfff000U & ((vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                       [0U][4U] << 0xdU) 
                                      | (0x1000U & 
                                         (vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                          [0U][3U] 
                                          >> 0x13U)))));
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay) {
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [1U][0U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [1U][1U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [1U][2U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [1U][3U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][4U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
            [1U][4U];
        vlSelfRef.__PVT__intIsStage__DOT__valid[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayEntry
            [1U];
    } else {
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [1U][0U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [1U][1U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [1U][2U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [1U][3U];
        vlSelfRef.__PVT__intIsStage__DOT__issuedData[1U][4U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
            [1U][4U];
        vlSelfRef.__PVT__intIsStage__DOT__valid[1U] 
            = (1U & ((~ (IData)(vlSelfRef.__PVT__intIsStage__DOT__stall)) 
                     & (vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                        [1U] >> 4U)));
    }
    vlSelfRef.__PVT__intIsStage__DOT__issueQueuePtr[1U] 
        = (0xfU & vlSelfRef.__PVT__intIsStage__DOT__pipeReg
           [1U]);
    __Vfunc_SelectiveFlushDetector__531__opPtr = (vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__531__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__531__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__531__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__531__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__531__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__531__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                goto __Vlabel143;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__531__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                    goto __Vlabel143;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
                    goto __Vlabel143;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__531__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                    goto __Vlabel143;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__531__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__531__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__531__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 1U;
                    goto __Vlabel143;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
                    goto __Vlabel143;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
                goto __Vlabel143;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout = 0U;
        }
        __Vlabel143: ;
    }
    vlSelfRef.__PVT__intIsStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__531__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssue[1U] 
        = (((~ (IData)(vlSelfRef.__PVT__intIsStage__DOT__clear)) 
            & vlSelfRef.__PVT__intIsStage__DOT__valid
            [1U]) & (~ vlSelfRef.__PVT__intIsStage__DOT__flush
                     [1U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuePtr[1U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issueQueuePtr
        [1U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[1U][4U] 
        = ((0xfff7ffU & vlSelfRef.__PVT__intIsStage__DOT__nextStage
            [1U][4U]) | (0xffffffU & (((~ (((IData)(vlSelfRef.__PVT__intIsStage__DOT__clear) 
                                            | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                           | vlSelfRef.__PVT__intIsStage__DOT__flush
                                           [1U])) & 
                                       vlSelfRef.__PVT__intIsStage__DOT__valid
                                       [1U]) << 0xbU)));
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [1U][0U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [1U][1U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [1U][2U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__intIsStage__DOT__issuedData
        [1U][3U];
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[1U][4U] 
        = ((0xfff800U & vlSelfRef.__PVT__intIsStage__DOT__nextStage
            [1U][4U]) | (0xffffffU & vlSelfRef.__PVT__intIsStage__DOT__issuedData
                         [1U][4U]));
    vlSelfRef.__PVT__intIsStage__DOT__nextStage[1U][4U] 
        = ((0xfffU & vlSelfRef.__PVT__intIsStage__DOT__nextStage
            [1U][4U]) | (0xfff000U & ((vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                       [1U][4U] << 0xdU) 
                                      | (0x1000U & 
                                         (vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                          [1U][3U] 
                                          >> 0x13U)))));
    vlSelfRef.__PVT__intIsStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__intIsStage__DOT__nextStage
        [1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
            [0U]) | (vlSelfRef.__PVT__intIsStage__DOT__valid
                     [0U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
            [0U]) | (vlSelfRef.__PVT__intIsStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
            [0U]) | (0xfffU & ((vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                [0U][4U] << 1U) | (
                                                   vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                                   [0U][3U] 
                                                   >> 0x1fU))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
            [1U]) | (vlSelfRef.__PVT__intIsStage__DOT__valid
                     [1U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
            [1U]) | (vlSelfRef.__PVT__intIsStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
            [1U]) | (0xfffU & ((vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                [1U][4U] << 1U) | (
                                                   vlSelfRef.__PVT__intIsStage__DOT__issuedData
                                                   [1U][3U] 
                                                   >> 0x1fU))));
    vlSelfRef.__PVT__intIsStage__DOT__unnamedblk4__DOT__i = 2U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__45(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__45\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__558__detectRange;
    __Vfunc_SelectiveFlushDetector__558__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__558__headPtr;
    __Vfunc_SelectiveFlushDetector__558__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__558__tailPtr;
    __Vfunc_SelectiveFlushDetector__558__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__558__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__558__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__558__opPtr;
    __Vfunc_SelectiveFlushDetector__558__opPtr = 0;
    // Body
    vlSelfRef.__PVT__memIsStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage) 
                                                >> 1U));
    vlSelfRef.__PVT__memIsStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage));
    vlSelfRef.__PVT__memIsStage__DOT__issueQueuePtr[0U] 
        = (0xfU & vlSelfRef.__PVT__memIsStage__DOT__pipeReg
           [0U]);
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay) {
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [0U][0U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [0U][1U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [0U][2U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [0U][3U];
        vlSelfRef.__PVT__memIsStage__DOT__valid[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayEntry
            [0U];
    } else {
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [0U][0U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [0U][1U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [0U][2U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[0U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [0U][3U];
        vlSelfRef.__PVT__memIsStage__DOT__valid[0U] 
            = (1U & ((~ (IData)(vlSelfRef.__PVT__memIsStage__DOT__stall)) 
                     & (vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                        [0U] >> 4U)));
    }
    __Vfunc_SelectiveFlushDetector__558__opPtr = (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__558__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__558__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__558__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__558__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__558__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__558__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                goto __Vlabel144;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__558__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                    goto __Vlabel144;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
                    goto __Vlabel144;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__558__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                    goto __Vlabel144;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                    goto __Vlabel144;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
                    goto __Vlabel144;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
                goto __Vlabel144;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
        }
        __Vlabel144: ;
    }
    vlSelfRef.__PVT__memIsStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssue[0U] 
        = (((~ (IData)(vlSelfRef.__PVT__memIsStage__DOT__clear)) 
            & vlSelfRef.__PVT__memIsStage__DOT__valid
            [0U]) & (~ vlSelfRef.__PVT__memIsStage__DOT__flush
                     [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuePtr[0U] 
        = vlSelfRef.__PVT__memIsStage__DOT__issueQueuePtr
        [0U];
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [0U][0U]) | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][0U] 
        = ((0xffffffe1U & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [0U][0U]) | (vlSelfRef.__PVT__memIsStage__DOT__issueQueuePtr
                         [0U] << 1U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][4U] 
        = ((0x7ffbU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [0U][4U]) | (0x7fffU & (((~ (((IData)(vlSelfRef.__PVT__memIsStage__DOT__clear) 
                                          | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                         | vlSelfRef.__PVT__memIsStage__DOT__flush
                                         [0U])) & vlSelfRef.__PVT__memIsStage__DOT__valid
                                     [0U]) << 2U)));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][0U] 
        = ((0x1fU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [0U][0U]) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                         [0U][0U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][1U] 
        = ((vlSelfRef.__PVT__memIsStage__DOT__issuedData
            [0U][0U] >> 0x1bU) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                  [0U][1U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][2U] 
        = ((vlSelfRef.__PVT__memIsStage__DOT__issuedData
            [0U][1U] >> 0x1bU) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                  [0U][2U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][3U] 
        = ((vlSelfRef.__PVT__memIsStage__DOT__issuedData
            [0U][2U] >> 0x1bU) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                  [0U][3U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][4U] 
        = ((0x7ffcU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [0U][4U]) | (0x7fffU & (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                    [0U][3U] >> 0x1bU)));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[0U][4U] 
        = ((7U & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [0U][4U]) | (0x7ff8U & (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                    [0U][3U] >> 0xeU)));
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay) {
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [1U][0U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [1U][1U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [1U][2U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
            [1U][3U];
        vlSelfRef.__PVT__memIsStage__DOT__valid[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayEntry
            [1U];
    } else {
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [1U][0U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [1U][1U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [1U][2U];
        vlSelfRef.__PVT__memIsStage__DOT__issuedData[1U][3U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
            [1U][3U];
        vlSelfRef.__PVT__memIsStage__DOT__valid[1U] 
            = (1U & ((~ (IData)(vlSelfRef.__PVT__memIsStage__DOT__stall)) 
                     & (vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                        [1U] >> 4U)));
    }
    vlSelfRef.__PVT__memIsStage__DOT__issueQueuePtr[1U] 
        = (0xfU & vlSelfRef.__PVT__memIsStage__DOT__pipeReg
           [1U]);
    __Vfunc_SelectiveFlushDetector__558__opPtr = (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__558__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__558__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__558__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__558__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__558__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__558__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                goto __Vlabel145;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__558__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                    goto __Vlabel145;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
                    goto __Vlabel145;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__558__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                    goto __Vlabel145;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__558__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__558__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__558__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 1U;
                    goto __Vlabel145;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
                    goto __Vlabel145;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
                goto __Vlabel145;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout = 0U;
        }
        __Vlabel145: ;
    }
    vlSelfRef.__PVT__memIsStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__558__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssue[1U] 
        = (((~ (IData)(vlSelfRef.__PVT__memIsStage__DOT__clear)) 
            & vlSelfRef.__PVT__memIsStage__DOT__valid
            [1U]) & (~ vlSelfRef.__PVT__memIsStage__DOT__flush
                     [1U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuePtr[1U] 
        = vlSelfRef.__PVT__memIsStage__DOT__issueQueuePtr
        [1U];
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [1U][0U]) | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][0U] 
        = ((0xffffffe1U & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [1U][0U]) | (vlSelfRef.__PVT__memIsStage__DOT__issueQueuePtr
                         [1U] << 1U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][4U] 
        = ((0x7ffbU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [1U][4U]) | (0x7fffU & (((~ (((IData)(vlSelfRef.__PVT__memIsStage__DOT__clear) 
                                          | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                         | vlSelfRef.__PVT__memIsStage__DOT__flush
                                         [1U])) & vlSelfRef.__PVT__memIsStage__DOT__valid
                                     [1U]) << 2U)));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][0U] 
        = ((0x1fU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [1U][0U]) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                         [1U][0U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][1U] 
        = ((vlSelfRef.__PVT__memIsStage__DOT__issuedData
            [1U][0U] >> 0x1bU) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                  [1U][1U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][2U] 
        = ((vlSelfRef.__PVT__memIsStage__DOT__issuedData
            [1U][1U] >> 0x1bU) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                  [1U][2U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][3U] 
        = ((vlSelfRef.__PVT__memIsStage__DOT__issuedData
            [1U][2U] >> 0x1bU) | (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                  [1U][3U] << 5U));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][4U] 
        = ((0x7ffcU & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [1U][4U]) | (0x7fffU & (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                    [1U][3U] >> 0x1bU)));
    vlSelfRef.__PVT__memIsStage__DOT__nextStage[1U][4U] 
        = ((7U & vlSelfRef.__PVT__memIsStage__DOT__nextStage
            [1U][4U]) | (0x7ff8U & (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                                    [1U][3U] >> 0xeU)));
    vlSelfRef.__PVT__memIsStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__memIsStage__DOT__nextStage
        [1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
            [0U]) | (vlSelfRef.__PVT__memIsStage__DOT__valid
                     [0U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
            [0U]) | (vlSelfRef.__PVT__memIsStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                               [0U][3U] >> 0x11U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
            [1U]) | (vlSelfRef.__PVT__memIsStage__DOT__valid
                     [1U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
            [1U]) | (vlSelfRef.__PVT__memIsStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__memIsStage__DOT__issuedData
                               [1U][3U] >> 0x11U)));
    vlSelfRef.__PVT__memIsStage__DOT__unnamedblk4__DOT__i = 2U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__46(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__46\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ complexIsStage__DOT____Vlvbound_h6a845ab6__0;
    complexIsStage__DOT____Vlvbound_h6a845ab6__0 = 0;
    CData/*0:0*/ complexIsStage__DOT____Vlvbound_h046b483c__0;
    complexIsStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ complexIsStage__DOT____Vlvbound_he39064e5__0;
    complexIsStage__DOT____Vlvbound_he39064e5__0 = 0;
    CData/*3:0*/ complexIsStage__DOT____Vlvbound_hdbf2f1a3__0;
    complexIsStage__DOT____Vlvbound_hdbf2f1a3__0 = 0;
    CData/*0:0*/ complexIsStage__DOT____Vlvbound_hc08daecf__0;
    complexIsStage__DOT____Vlvbound_hc08daecf__0 = 0;
    CData/*5:0*/ complexIsStage__DOT____Vlvbound_hed7eddda__0;
    complexIsStage__DOT____Vlvbound_hed7eddda__0 = 0;
    CData/*0:0*/ complexIsStage__DOT____Vlvbound_hbac4ac1f__0;
    complexIsStage__DOT____Vlvbound_hbac4ac1f__0 = 0;
    VlWide<3>/*81:0*/ complexIsStage__DOT____Vlvbound_h2e60d6ba__0;
    VL_ZERO_W(82, complexIsStage__DOT____Vlvbound_h2e60d6ba__0);
    CData/*0:0*/ complexIsStage__DOT____Vlvbound_hbac60129__0;
    complexIsStage__DOT____Vlvbound_hbac60129__0 = 0;
    SData/*11:0*/ complexIsStage__DOT____Vlvbound_h3ccd0758__0;
    complexIsStage__DOT____Vlvbound_h3ccd0758__0 = 0;
    CData/*0:0*/ complexIsStage__DOT____Vlvbound_hdec9e1f2__0;
    complexIsStage__DOT____Vlvbound_hdec9e1f2__0 = 0;
    CData/*0:0*/ complexIsStage__DOT____Vlvbound_hdec9d2a3__0;
    complexIsStage__DOT____Vlvbound_hdec9d2a3__0 = 0;
    SData/*11:0*/ complexIsStage__DOT____Vlvbound_h3cb64271__0;
    complexIsStage__DOT____Vlvbound_h3cb64271__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__550__detectRange;
    __Vfunc_SelectiveFlushDetector__550__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__550__headPtr;
    __Vfunc_SelectiveFlushDetector__550__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__550__tailPtr;
    __Vfunc_SelectiveFlushDetector__550__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__550__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__550__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__550__opPtr;
    __Vfunc_SelectiveFlushDetector__550__opPtr = 0;
    // Body
    vlSelfRef.__PVT__complexIsStage__DOT__stall = (1U 
                                                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage) 
                                                      >> 1U));
    vlSelfRef.__PVT__complexIsStage__DOT__clear = (1U 
                                                   & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage));
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay) {
        vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__0[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
            [0U][0U];
        vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__0[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
            [0U][1U];
        vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__0[2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
            [0U][2U];
        vlSelfRef.complexIsStage__DOT____Vlvbound_h144dd5b6__0 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayEntry
            [0U];
        vlSelfRef.__PVT__complexIsStage__DOT__issuedData[0U][0U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__0[0U];
        vlSelfRef.__PVT__complexIsStage__DOT__issuedData[0U][1U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__0[1U];
        vlSelfRef.__PVT__complexIsStage__DOT__issuedData[0U][2U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__0[2U];
        vlSelfRef.__PVT__complexIsStage__DOT__valid[0U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_h144dd5b6__0;
    } else {
        vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__1[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData
            [0U][0U];
        vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__1[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData
            [0U][1U];
        vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__1[2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData
            [0U][2U];
        vlSelfRef.complexIsStage__DOT____Vlvbound_h144dd5b6__1 
            = (1U & ((~ (IData)(vlSelfRef.__PVT__complexIsStage__DOT__stall)) 
                     & (vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
                        [0U] >> 4U)));
        vlSelfRef.__PVT__complexIsStage__DOT__issuedData[0U][0U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__1[0U];
        vlSelfRef.__PVT__complexIsStage__DOT__issuedData[0U][1U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__1[1U];
        vlSelfRef.__PVT__complexIsStage__DOT__issuedData[0U][2U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_hfff70973__1[2U];
        vlSelfRef.__PVT__complexIsStage__DOT__valid[0U] 
            = vlSelfRef.complexIsStage__DOT____Vlvbound_h144dd5b6__1;
    }
    complexIsStage__DOT____Vlvbound_h6a845ab6__0 = 
        (0xfU & vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
         [0U]);
    vlSelfRef.__PVT__complexIsStage__DOT__issueQueuePtr[0U] 
        = complexIsStage__DOT____Vlvbound_h6a845ab6__0;
    __Vfunc_SelectiveFlushDetector__550__opPtr = (vlSelfRef.__PVT__complexIsStage__DOT__issuedData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__550__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__550__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__550__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__550__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__550__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__550__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 1U;
                goto __Vlabel146;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__550__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__550__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__550__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__550__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__550__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__550__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__550__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 1U;
                    goto __Vlabel146;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 0U;
                    goto __Vlabel146;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__550__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__550__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__550__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__550__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__550__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__550__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__550__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 1U;
                    goto __Vlabel146;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__550__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__550__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__550__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__550__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 1U;
                    goto __Vlabel146;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 0U;
                    goto __Vlabel146;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 0U;
                goto __Vlabel146;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout = 0U;
        }
        __Vlabel146: ;
    }
    complexIsStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__550__Vfuncout;
    vlSelfRef.__PVT__complexIsStage__DOT__flush[0U] 
        = complexIsStage__DOT____Vlvbound_h046b483c__0;
    complexIsStage__DOT____Vlvbound_he39064e5__0 = 
        (((~ (IData)(vlSelfRef.__PVT__complexIsStage__DOT__clear)) 
          & vlSelfRef.__PVT__complexIsStage__DOT__valid
          [0U]) & (~ vlSelfRef.__PVT__complexIsStage__DOT__flush
                   [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssue[0U] 
        = complexIsStage__DOT____Vlvbound_he39064e5__0;
    complexIsStage__DOT____Vlvbound_hdbf2f1a3__0 = 
        vlSelfRef.__PVT__complexIsStage__DOT__issueQueuePtr
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuePtr[0U] 
        = complexIsStage__DOT____Vlvbound_hdbf2f1a3__0;
    complexIsStage__DOT____Vlvbound_hc08daecf__0 = 
        ((((~ (IData)(vlSelfRef.__PVT__complexIsStage__DOT__clear)) 
           & vlSelfRef.__PVT__complexIsStage__DOT__valid
           [0U]) & (~ vlSelfRef.__PVT__complexIsStage__DOT__flush
                    [0U])) & (1U == (7U & vlSelfRef.__PVT__complexIsStage__DOT__issuedData
                                     [0U][2U])));
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divAcquire[0U] 
        = complexIsStage__DOT____Vlvbound_hc08daecf__0;
    complexIsStage__DOT____Vlvbound_hed7eddda__0 = 
        (vlSelfRef.__PVT__complexIsStage__DOT__issuedData
         [0U][1U] >> 0x1aU);
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__acquireActiveListPtr[0U] 
        = complexIsStage__DOT____Vlvbound_hed7eddda__0;
    complexIsStage__DOT____Vlvbound_hbac4ac1f__0 = 
        ((~ (((IData)(vlSelfRef.__PVT__complexIsStage__DOT__clear) 
              | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
             | vlSelfRef.__PVT__complexIsStage__DOT__flush
             [0U])) & vlSelfRef.__PVT__complexIsStage__DOT__valid
         [0U]);
    vlSelfRef.__PVT__complexIsStage__DOT__nextStage[0U][2U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__complexIsStage__DOT__nextStage
            [0U][2U]) | ((IData)(complexIsStage__DOT____Vlvbound_hbac4ac1f__0) 
                         << 0x13U));
    complexIsStage__DOT____Vlvbound_h2e60d6ba__0[0U] 
        = vlSelfRef.__PVT__complexIsStage__DOT__issuedData
        [0U][0U];
    complexIsStage__DOT____Vlvbound_h2e60d6ba__0[1U] 
        = vlSelfRef.__PVT__complexIsStage__DOT__issuedData
        [0U][1U];
    complexIsStage__DOT____Vlvbound_h2e60d6ba__0[2U] 
        = vlSelfRef.__PVT__complexIsStage__DOT__issuedData
        [0U][2U];
    vlSelfRef.__PVT__complexIsStage__DOT__nextStage[0U][0U] 
        = complexIsStage__DOT____Vlvbound_h2e60d6ba__0[0U];
    vlSelfRef.__PVT__complexIsStage__DOT__nextStage[0U][1U] 
        = complexIsStage__DOT____Vlvbound_h2e60d6ba__0[1U];
    vlSelfRef.__PVT__complexIsStage__DOT__nextStage[0U][2U] 
        = ((0xfffc0000U & vlSelfRef.__PVT__complexIsStage__DOT__nextStage
            [0U][2U]) | complexIsStage__DOT____Vlvbound_h2e60d6ba__0[2U]);
    complexIsStage__DOT____Vlvbound_hbac60129__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay;
    vlSelfRef.__PVT__complexIsStage__DOT__nextStage[0U][2U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__complexIsStage__DOT__nextStage
            [0U][2U]) | ((IData)(complexIsStage__DOT____Vlvbound_hbac60129__0) 
                         << 0x12U));
    complexIsStage__DOT____Vlvbound_h3ccd0758__0 = 
        (0xfffU & (vlSelfRef.__PVT__complexIsStage__DOT__issuedData
                   [0U][2U] >> 6U));
    vlSelfRef.__PVT__complexIsStage__DOT__nextStage[0U][2U] 
        = ((0xfffffU & vlSelfRef.__PVT__complexIsStage__DOT__nextStage
            [0U][2U]) | ((IData)(complexIsStage__DOT____Vlvbound_h3ccd0758__0) 
                         << 0x14U));
    vlSelfRef.__PVT__complexIsStage__DOT__unnamedblk3__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__complexIsStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__complexIsStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__complexIsStage__DOT__nextStage
        [0U][2U];
    complexIsStage__DOT____Vlvbound_hdec9e1f2__0 = 
        vlSelfRef.__PVT__complexIsStage__DOT__valid
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
            [0U]) | ((IData)(complexIsStage__DOT____Vlvbound_hdec9e1f2__0) 
                     << 0xdU));
    complexIsStage__DOT____Vlvbound_hdec9d2a3__0 = 
        vlSelfRef.__PVT__complexIsStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
            [0U]) | ((IData)(complexIsStage__DOT____Vlvbound_hdec9d2a3__0) 
                     << 0xcU));
    complexIsStage__DOT____Vlvbound_h3cb64271__0 = 
        (0xfffU & (vlSelfRef.__PVT__complexIsStage__DOT__issuedData
                   [0U][2U] >> 6U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
            [0U]) | (IData)(complexIsStage__DOT____Vlvbound_h3cb64271__0));
    vlSelfRef.__PVT__complexIsStage__DOT__unnamedblk4__DOT__i = 1U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__47(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__47\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ fpIsStage__DOT____Vlvbound_h6a845ab6__0;
    fpIsStage__DOT____Vlvbound_h6a845ab6__0 = 0;
    CData/*0:0*/ fpIsStage__DOT____Vlvbound_h046b483c__0;
    fpIsStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ fpIsStage__DOT____Vlvbound_h573734bf__0;
    fpIsStage__DOT____Vlvbound_h573734bf__0 = 0;
    CData/*3:0*/ fpIsStage__DOT____Vlvbound_h23bfaa6b__0;
    fpIsStage__DOT____Vlvbound_h23bfaa6b__0 = 0;
    CData/*0:0*/ fpIsStage__DOT____Vlvbound_h821591aa__0;
    fpIsStage__DOT____Vlvbound_h821591aa__0 = 0;
    CData/*5:0*/ fpIsStage__DOT____Vlvbound_h2519f420__0;
    fpIsStage__DOT____Vlvbound_h2519f420__0 = 0;
    CData/*0:0*/ fpIsStage__DOT____Vlvbound_h2f13474c__0;
    fpIsStage__DOT____Vlvbound_h2f13474c__0 = 0;
    VlWide<3>/*92:0*/ fpIsStage__DOT____Vlvbound_hc9e0d9cf__0;
    VL_ZERO_W(93, fpIsStage__DOT____Vlvbound_hc9e0d9cf__0);
    CData/*0:0*/ fpIsStage__DOT____Vlvbound_h2f0c8c48__0;
    fpIsStage__DOT____Vlvbound_h2f0c8c48__0 = 0;
    SData/*11:0*/ fpIsStage__DOT____Vlvbound_hbe82b2f3__0;
    fpIsStage__DOT____Vlvbound_hbe82b2f3__0 = 0;
    CData/*0:0*/ fpIsStage__DOT____Vlvbound_hb4b3fdf8__0;
    fpIsStage__DOT____Vlvbound_hb4b3fdf8__0 = 0;
    CData/*0:0*/ fpIsStage__DOT____Vlvbound_hb4b44e87__0;
    fpIsStage__DOT____Vlvbound_hb4b44e87__0 = 0;
    SData/*11:0*/ fpIsStage__DOT____Vlvbound_hfb212655__0;
    fpIsStage__DOT____Vlvbound_hfb212655__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__620__detectRange;
    __Vfunc_SelectiveFlushDetector__620__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__620__headPtr;
    __Vfunc_SelectiveFlushDetector__620__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__620__tailPtr;
    __Vfunc_SelectiveFlushDetector__620__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__620__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__620__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__620__opPtr;
    __Vfunc_SelectiveFlushDetector__620__opPtr = 0;
    // Body
    vlSelfRef.__PVT__fpIsStage__DOT__stall = (1U & 
                                              ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage) 
                                               >> 1U));
    vlSelfRef.__PVT__fpIsStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage));
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay) {
        vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__0[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
            [0U][0U];
        vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__0[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
            [0U][1U];
        vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__0[2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
            [0U][2U];
        vlSelfRef.fpIsStage__DOT____Vlvbound_h144dd5b6__0 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayEntry
            [0U];
        vlSelfRef.__PVT__fpIsStage__DOT__issuedData[0U][0U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__0[0U];
        vlSelfRef.__PVT__fpIsStage__DOT__issuedData[0U][1U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__0[1U];
        vlSelfRef.__PVT__fpIsStage__DOT__issuedData[0U][2U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__0[2U];
        vlSelfRef.__PVT__fpIsStage__DOT__valid[0U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_h144dd5b6__0;
    } else {
        vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__1[0U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData
            [0U][0U];
        vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__1[1U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData
            [0U][1U];
        vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__1[2U] 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData
            [0U][2U];
        vlSelfRef.fpIsStage__DOT____Vlvbound_h144dd5b6__1 
            = (1U & ((~ (IData)(vlSelfRef.__PVT__fpIsStage__DOT__stall)) 
                     & (vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                        [0U] >> 4U)));
        vlSelfRef.__PVT__fpIsStage__DOT__issuedData[0U][0U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__1[0U];
        vlSelfRef.__PVT__fpIsStage__DOT__issuedData[0U][1U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__1[1U];
        vlSelfRef.__PVT__fpIsStage__DOT__issuedData[0U][2U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_hd7fbcd50__1[2U];
        vlSelfRef.__PVT__fpIsStage__DOT__valid[0U] 
            = vlSelfRef.fpIsStage__DOT____Vlvbound_h144dd5b6__1;
    }
    fpIsStage__DOT____Vlvbound_h6a845ab6__0 = (0xfU 
                                               & vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                                               [0U]);
    vlSelfRef.__PVT__fpIsStage__DOT__issueQueuePtr[0U] 
        = fpIsStage__DOT____Vlvbound_h6a845ab6__0;
    __Vfunc_SelectiveFlushDetector__620__opPtr = (vlSelfRef.__PVT__fpIsStage__DOT__issuedData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__620__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__620__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__620__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__620__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__620__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__620__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 1U;
                goto __Vlabel147;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__620__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__620__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__620__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__620__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__620__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__620__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__620__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 1U;
                    goto __Vlabel147;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 0U;
                    goto __Vlabel147;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__620__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__620__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__620__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__620__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__620__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__620__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__620__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 1U;
                    goto __Vlabel147;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__620__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__620__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__620__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__620__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 1U;
                    goto __Vlabel147;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 0U;
                    goto __Vlabel147;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 0U;
                goto __Vlabel147;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout = 0U;
        }
        __Vlabel147: ;
    }
    fpIsStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__620__Vfuncout;
    vlSelfRef.__PVT__fpIsStage__DOT__flush[0U] = fpIsStage__DOT____Vlvbound_h046b483c__0;
    fpIsStage__DOT____Vlvbound_h573734bf__0 = (((~ (IData)(vlSelfRef.__PVT__fpIsStage__DOT__clear)) 
                                                & vlSelfRef.__PVT__fpIsStage__DOT__valid
                                                [0U]) 
                                               & (~ 
                                                  vlSelfRef.__PVT__fpIsStage__DOT__flush
                                                  [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssue[0U] 
        = fpIsStage__DOT____Vlvbound_h573734bf__0;
    fpIsStage__DOT____Vlvbound_h23bfaa6b__0 = vlSelfRef.__PVT__fpIsStage__DOT__issueQueuePtr
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuePtr[0U] 
        = fpIsStage__DOT____Vlvbound_h23bfaa6b__0;
    fpIsStage__DOT____Vlvbound_h821591aa__0 = ((((~ (IData)(vlSelfRef.__PVT__fpIsStage__DOT__clear)) 
                                                 & vlSelfRef.__PVT__fpIsStage__DOT__valid
                                                 [0U]) 
                                                & (~ 
                                                   vlSelfRef.__PVT__fpIsStage__DOT__flush
                                                   [0U])) 
                                               & ((2U 
                                                   == 
                                                   (7U 
                                                    & (vlSelfRef.__PVT__fpIsStage__DOT__issuedData
                                                       [0U][2U] 
                                                       >> 0xeU))) 
                                                  | (3U 
                                                     == 
                                                     (7U 
                                                      & (vlSelfRef.__PVT__fpIsStage__DOT__issuedData
                                                         [0U][2U] 
                                                         >> 0xeU)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Acquire[0U] 
        = fpIsStage__DOT____Vlvbound_h821591aa__0;
    fpIsStage__DOT____Vlvbound_h2519f420__0 = (vlSelfRef.__PVT__fpIsStage__DOT__issuedData
                                               [0U][1U] 
                                               >> 0x1aU);
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__acquireActiveListPtr[0U] 
        = fpIsStage__DOT____Vlvbound_h2519f420__0;
    fpIsStage__DOT____Vlvbound_h2f13474c__0 = ((~ (
                                                   ((IData)(vlSelfRef.__PVT__fpIsStage__DOT__clear) 
                                                    | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                                   | vlSelfRef.__PVT__fpIsStage__DOT__flush
                                                   [0U])) 
                                               & vlSelfRef.__PVT__fpIsStage__DOT__valid
                                               [0U]);
    vlSelfRef.__PVT__fpIsStage__DOT__nextStage[0U][2U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__fpIsStage__DOT__nextStage
            [0U][2U]) | ((IData)(fpIsStage__DOT____Vlvbound_h2f13474c__0) 
                         << 0x1eU));
    fpIsStage__DOT____Vlvbound_hc9e0d9cf__0[0U] = vlSelfRef.__PVT__fpIsStage__DOT__issuedData
        [0U][0U];
    fpIsStage__DOT____Vlvbound_hc9e0d9cf__0[1U] = vlSelfRef.__PVT__fpIsStage__DOT__issuedData
        [0U][1U];
    fpIsStage__DOT____Vlvbound_hc9e0d9cf__0[2U] = vlSelfRef.__PVT__fpIsStage__DOT__issuedData
        [0U][2U];
    vlSelfRef.__PVT__fpIsStage__DOT__nextStage[0U][0U] 
        = fpIsStage__DOT____Vlvbound_hc9e0d9cf__0[0U];
    vlSelfRef.__PVT__fpIsStage__DOT__nextStage[0U][1U] 
        = fpIsStage__DOT____Vlvbound_hc9e0d9cf__0[1U];
    vlSelfRef.__PVT__fpIsStage__DOT__nextStage[0U][2U] 
        = ((0xe0000000U & vlSelfRef.__PVT__fpIsStage__DOT__nextStage
            [0U][2U]) | fpIsStage__DOT____Vlvbound_hc9e0d9cf__0[2U]);
    fpIsStage__DOT____Vlvbound_h2f0c8c48__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay;
    vlSelfRef.__PVT__fpIsStage__DOT__nextStage[0U][2U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__fpIsStage__DOT__nextStage
            [0U][2U]) | ((IData)(fpIsStage__DOT____Vlvbound_h2f0c8c48__0) 
                         << 0x1dU));
    fpIsStage__DOT____Vlvbound_hbe82b2f3__0 = (0xfffU 
                                               & (vlSelfRef.__PVT__fpIsStage__DOT__issuedData
                                                  [0U][2U] 
                                                  >> 0x11U));
    vlSelfRef.__PVT__fpIsStage__DOT__nextStage[0U][2U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__fpIsStage__DOT__nextStage
            [0U][2U]) | ((IData)(fpIsStage__DOT____Vlvbound_hbe82b2f3__0) 
                         << 0x1fU));
    vlSelfRef.__PVT__fpIsStage__DOT__nextStage[0U][3U] 
        = (0x7ffU & ((IData)(fpIsStage__DOT____Vlvbound_hbe82b2f3__0) 
                     >> 1U));
    vlSelfRef.__PVT__fpIsStage__DOT__unnamedblk3__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__fpIsStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__fpIsStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__fpIsStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__fpIsStage__DOT__nextStage
        [0U][3U];
    fpIsStage__DOT____Vlvbound_hb4b3fdf8__0 = vlSelfRef.__PVT__fpIsStage__DOT__valid
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
            [0U]) | ((IData)(fpIsStage__DOT____Vlvbound_hb4b3fdf8__0) 
                     << 0xdU));
    fpIsStage__DOT____Vlvbound_hb4b44e87__0 = vlSelfRef.__PVT__fpIsStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
            [0U]) | ((IData)(fpIsStage__DOT____Vlvbound_hb4b44e87__0) 
                     << 0xcU));
    fpIsStage__DOT____Vlvbound_hfb212655__0 = (0xfffU 
                                               & (vlSelfRef.__PVT__fpIsStage__DOT__issuedData
                                                  [0U][2U] 
                                                  >> 0x11U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
            [0U]) | (IData)(fpIsStage__DOT____Vlvbound_hfb212655__0));
    vlSelfRef.__PVT__fpIsStage__DOT__unnamedblk4__DOT__i = 1U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__48(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__48\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*7:0*/ __Vfunc_iCache__DOT__GetICacheIndex__7__Vfuncout;
    __Vfunc_iCache__DOT__GetICacheIndex__7__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_iCache__DOT__GetICacheIndex__7__addr;
    __Vfunc_iCache__DOT__GetICacheIndex__7__addr = 0;
    SData/*10:0*/ __Vfunc_iCache__DOT__GetICacheTag__8__Vfuncout;
    __Vfunc_iCache__DOT__GetICacheTag__8__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_iCache__DOT__GetICacheTag__8__addr;
    __Vfunc_iCache__DOT__GetICacheTag__8__addr = 0;
    CData/*7:0*/ __Vfunc_iCache__DOT__GetICacheIndex__9__Vfuncout;
    __Vfunc_iCache__DOT__GetICacheIndex__9__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_iCache__DOT__GetICacheIndex__9__addr;
    __Vfunc_iCache__DOT__GetICacheIndex__9__addr = 0;
    CData/*1:0*/ __Vfunc_iCache__DOT__UpdateNRUState__10__Vfuncout;
    __Vfunc_iCache__DOT__UpdateNRUState__10__Vfuncout = 0;
    CData/*1:0*/ __Vfunc_iCache__DOT__UpdateNRUState__10__NRUState;
    __Vfunc_iCache__DOT__UpdateNRUState__10__NRUState = 0;
    CData/*0:0*/ __Vfunc_iCache__DOT__UpdateNRUState__10__way;
    __Vfunc_iCache__DOT__UpdateNRUState__10__way = 0;
    CData/*1:0*/ __Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__Vfuncout;
    __Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__Vfuncout = 0;
    CData/*1:0*/ __Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__NRUState;
    __Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__NRUState = 0;
    CData/*0:0*/ __Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__Vfuncout;
    __Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__addr;
    __Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__addr = 0;
    // Body
    __Vfunc_iCache__DOT__GetICacheIndex__7__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadAddrIn;
    __Vfunc_iCache__DOT__GetICacheIndex__7__Vfuncout 
        = (0xffU & (__Vfunc_iCache__DOT__GetICacheIndex__7__addr 
                    >> 3U));
    vlSelfRef.__PVT__iCache__DOT__readIndex = __Vfunc_iCache__DOT__GetICacheIndex__7__Vfuncout;
    __Vfunc_iCache__DOT__GetICacheTag__8__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadAddrIn;
    __Vfunc_iCache__DOT__GetICacheTag__8__Vfuncout 
        = (0x7ffU & (__Vfunc_iCache__DOT__GetICacheTag__8__addr 
                     >> 0xbU));
    vlSelfRef.__PVT__iCache__DOT__readTag = __Vfunc_iCache__DOT__GetICacheTag__8__Vfuncout;
    __Vfunc_iCache__DOT__GetICacheIndex__9__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__icNextReadAddrIn;
    __Vfunc_iCache__DOT__GetICacheIndex__9__Vfuncout 
        = (0xffU & (__Vfunc_iCache__DOT__GetICacheIndex__9__addr 
                    >> 3U));
    vlSelfRef.__PVT__iCache__DOT__nextReadIndex = __Vfunc_iCache__DOT__GetICacheIndex__9__Vfuncout;
    vlSelfRef.__PVT__iCache__DOT__hit = (0U != (IData)(vlSelfRef.__PVT__iCache__DOT__hitArray));
    vlSelfRef.__PVT__iCache__DOT__hitWay = 0U;
    vlSelfRef.__PVT__iCache__DOT__unnamedblk1__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__iCache__DOT__unnamedblk1__DOT__i)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__iCache__DOT__hitArray) 
                       >> (1U & vlSelfRef.__PVT__iCache__DOT__unnamedblk1__DOT__i)))) {
                vlSelfRef.__PVT__iCache__DOT__hitWay 
                    = (1U & vlSelfRef.__PVT__iCache__DOT__unnamedblk1__DOT__i);
                goto __Vlabel148;
            }
            vlSelfRef.__PVT__iCache__DOT__unnamedblk1__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__iCache__DOT__unnamedblk1__DOT__i);
        }
        __Vlabel148: ;
    }
    vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__valid 
        = (1U & (vlSelfRef.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U] 
                 >> 0xbU));
    vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__hit 
        = ((IData)(vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__valid) 
           & ((0x7ffU & vlSelfRef.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U]) 
              == (IData)(vlSelfRef.__PVT__iCache__DOT__readTag)));
    vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__valid 
        = (1U & (vlSelfRef.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U] 
                 >> 0xbU));
    vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__hit 
        = ((IData)(vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__valid) 
           & ((0x7ffU & vlSelfRef.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U]) 
              == (IData)(vlSelfRef.__PVT__iCache__DOT__readTag)));
    __Vfunc_iCache__DOT__UpdateNRUState__10__way = vlSelfRef.__PVT__iCache__DOT__hitWay;
    __Vfunc_iCache__DOT__UpdateNRUState__10__NRUState 
        = vlSelfRef.__PVT__iCache__DOT__readNRUState;
    __Vfunc_iCache__DOT__UpdateNRUState__10__Vfuncout 
        = (3U & ((3U == ((IData)(__Vfunc_iCache__DOT__UpdateNRUState__10__NRUState) 
                         | ((IData)(1U) << (IData)(__Vfunc_iCache__DOT__UpdateNRUState__10__way))))
                  ? ((IData)(1U) << (IData)(__Vfunc_iCache__DOT__UpdateNRUState__10__way))
                  : ((IData)(__Vfunc_iCache__DOT__UpdateNRUState__10__NRUState) 
                     | ((IData)(1U) << (IData)(__Vfunc_iCache__DOT__UpdateNRUState__10__way)))));
    vlSelfRef.__PVT__iCache__DOT__updatedNRUState = __Vfunc_iCache__DOT__UpdateNRUState__10__Vfuncout;
    __Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__NRUState 
        = vlSelfRef.__PVT__iCache__DOT__readNRUState;
    __Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__Vfuncout 
        = (3U & (((IData)(__Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__NRUState) 
                  | ((IData)(1U) + (IData)(__Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__NRUState))) 
                 ^ (IData)(__Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__NRUState)));
    vlSelfRef.__PVT__iCache__DOT__wayToEvictOneHot 
        = __Vfunc_iCache__DOT__DecideWayToEvictByNRUState__11__Vfuncout;
    vlSelfRef.__PVT__iCache__DOT__wayToEvict = 0U;
    vlSelfRef.__PVT__iCache__DOT__unnamedblk2__DOT__i = 0U;
    vlSelfRef.__PVT__iCache__DOT__nruStateArray__DOT__writeNRUStateIndex 
        = ((IData)(vlSelfRef.iCache__DOT____Vcellinp__nruStateArray__rst)
            ? (IData)(vlSelfRef.__PVT__iCache__DOT__rstIndex)
            : (IData)(vlSelfRef.__PVT__iCache__DOT__readIndex));
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__iCache__DOT__unnamedblk2__DOT__i)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__iCache__DOT__wayToEvictOneHot) 
                       >> (1U & vlSelfRef.__PVT__iCache__DOT__unnamedblk2__DOT__i)))) {
                vlSelfRef.__PVT__iCache__DOT__wayToEvict 
                    = (1U & vlSelfRef.__PVT__iCache__DOT__unnamedblk2__DOT__i);
                goto __Vlabel149;
            }
            vlSelfRef.__PVT__iCache__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__iCache__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel149: ;
    }
    vlSelfRef.__PVT__iCache__DOT__nruStateArray__DOT__writeNRUStateData 
        = ((IData)(vlSelfRef.iCache__DOT____Vcellinp__nruStateArray__rst)
            ? 0U : (IData)(vlSelfRef.__PVT__iCache__DOT__updatedNRUState));
    __Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__addr 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadAddrIn;
    __Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__Vfuncout 
        = (1U & (__Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__addr 
                 >> 2U));
    vlSelfRef.__PVT__iCache__DOT__wordPtr[0U] = __Vfunc_iCache__DOT__GetICacheLineInsnIndex__12__Vfuncout;
    vlSelfRef.__PVT__iCache__DOT__wordPtr[1U] = (1U 
                                                 & ((IData)(1U) 
                                                    + 
                                                    vlSelfRef.__PVT__iCache__DOT__wordPtr
                                                    [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadDataOut[0U] 
        = (IData)((vlSelfRef.__PVT__iCache__DOT__readLineInsnList
                   [vlSelfRef.__PVT__iCache__DOT__hitWay] 
                   >> (0x3fU & VL_SHIFTL_III(6,32,32, 
                                             vlSelfRef.__PVT__iCache__DOT__wordPtr
                                             [0U], 5U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadDataOut[1U] 
        = (IData)((vlSelfRef.__PVT__iCache__DOT__readLineInsnList
                   [vlSelfRef.__PVT__iCache__DOT__hitWay] 
                   >> (0x3fU & VL_SHIFTL_III(6,32,32, 
                                             vlSelfRef.__PVT__iCache__DOT__wordPtr
                                             [1U], 5U))));
    vlSelfRef.iCache__DOT____Vcellinp__iCacheHitLogic__hitIn 
        = (((IData)(vlSelfRef.__PVT__iCache__DOT__hit) 
            & (0U == (IData)(vlSelfRef.__PVT__iCache__DOT__regPhase))) 
           & (vlSelfRef.__PVT__ifStage__DOT__pipeReg
              [0U] >> 0x14U));
    vlSelfRef.__PVT__iCache__DOT__valid[0U] = vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__valid;
    vlSelfRef.__PVT__iCache__DOT__valid[1U] = vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__valid;
    vlSelfRef.__PVT__iCache__DOT__hitArray = (((IData)(vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__hit) 
                                               << 1U) 
                                              | (IData)(vlSelfRef.iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__hit));
    vlSelfRef.__PVT__iCache__DOT__we[0U] = 0U;
    vlSelfRef.__PVT__iCache__DOT__we[1U] = 0U;
    if ((1U & (~ ((IData)(vlSelfRef.__PVT__iCache__DOT__regPhase) 
                  >> 2U)))) {
        if ((2U & (IData)(vlSelfRef.__PVT__iCache__DOT__regPhase))) {
            if ((1U & (~ (IData)(vlSelfRef.__PVT__iCache__DOT__regPhase)))) {
                if (((vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[2U] 
                      >> 2U) & ((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[2U]) 
                                == (IData)(vlSelfRef.__PVT__iCache__DOT__regSerial)))) {
                    vlSelfRef.__PVT__iCache__DOT__we[vlSelfRef.__PVT__iCache__DOT__wayToEvict] = 1U;
                }
            }
        }
    }
    vlSelfRef.__PVT__iCache__DOT__iCacheHitLogic__DOT__wordPtr[0U] 
        = (3U & vlSelfRef.__PVT__iCache__DOT__wordPtr
           [0U]);
    vlSelfRef.iCache__DOT____Vcellout__iCacheHitLogic__hitOut[0U] 
        = ((1U & (~ (vlSelfRef.__PVT__iCache__DOT__iCacheHitLogic__DOT__wordPtr
                     [0U] >> 1U))) && (IData)(vlSelfRef.iCache__DOT____Vcellinp__iCacheHitLogic__hitIn));
    vlSelfRef.__PVT__iCache__DOT__iCacheHitLogic__DOT__wordPtr[1U] 
        = (3U & ((IData)(1U) + vlSelfRef.__PVT__iCache__DOT__wordPtr
                 [0U]));
    vlSelfRef.iCache__DOT____Vcellout__iCacheHitLogic__hitOut[1U] 
        = ((1U & (~ (vlSelfRef.__PVT__iCache__DOT__iCacheHitLogic__DOT__wordPtr
                     [1U] >> 1U))) && (IData)(vlSelfRef.iCache__DOT____Vcellinp__iCacheHitLogic__hitIn));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadHit[0U] 
        = vlSelfRef.iCache__DOT____Vcellout__iCacheHitLogic__hitOut
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadHit[1U] 
        = vlSelfRef.iCache__DOT____Vcellout__iCacheHitLogic__hitOut
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStageSendBubbleLower 
        = (1U & ((vlSelfRef.__PVT__ifStage__DOT__pipeReg
                  [0U] >> 0x14U) & (~ vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadHit
                                    [0U])));
    vlSelfRef.__PVT__ifStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__ifStage__DOT__beginStall = ((~ (IData)(vlSelfRef.__PVT__ifStage__DOT__regStall)) 
                                                 & (IData)(vlSelfRef.__PVT__ifStage__DOT__stall));
    vlSymsp->TOP__SMT_RTL_Testbench__core__perfCounterIF.__PVT__icMiss 
        = (((IData)(vlSelfRef.__PVT__ifStage__DOT__beginStall) 
            & (vlSelfRef.__PVT__ifStage__DOT__pipeReg
               [0U] >> 0x14U)) & (~ vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadHit
                                  [0U]));
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__49(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__49\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*11:0*/ complexExStage__DOT____Vlvbound_hd30db1a5__0;
    complexExStage__DOT____Vlvbound_hd30db1a5__0 = 0;
    CData/*0:0*/ complexExStage__DOT____Vlvbound_h7bb3cb4c__0;
    complexExStage__DOT____Vlvbound_h7bb3cb4c__0 = 0;
    VlWide<3>/*81:0*/ complexExStage__DOT____Vlvbound_hc34d7a17__0;
    VL_ZERO_W(82, complexExStage__DOT____Vlvbound_hc34d7a17__0);
    SData/*11:0*/ complexExStage__DOT____Vlvbound_h59117768__0;
    complexExStage__DOT____Vlvbound_h59117768__0 = 0;
    CData/*0:0*/ complexExStage__DOT____Vlvbound_h00cce467__0;
    complexExStage__DOT____Vlvbound_h00cce467__0 = 0;
    CData/*0:0*/ complexExStage__DOT____Vlvbound_h00cc54d8__0;
    complexExStage__DOT____Vlvbound_h00cc54d8__0 = 0;
    VlWide<3>/*81:0*/ complexExStage__DOT____Vlvbound_h51aca2a4__0;
    VL_ZERO_W(82, complexExStage__DOT____Vlvbound_h51aca2a4__0);
    CData/*0:0*/ mulDivUnit__DOT____Vlvbound_h4d794342__0;
    mulDivUnit__DOT____Vlvbound_h4d794342__0 = 0;
    CData/*0:0*/ mulDivUnit__DOT____Vlvbound_h7d0655ac__0;
    mulDivUnit__DOT____Vlvbound_h7d0655ac__0 = 0;
    CData/*0:0*/ mulDivUnit__DOT____Vlvbound_h49e4d866__0;
    mulDivUnit__DOT____Vlvbound_h49e4d866__0 = 0;
    CData/*0:0*/ mulDivUnit__DOT____Vlvbound_h14f7d4db__0;
    mulDivUnit__DOT____Vlvbound_h14f7d4db__0 = 0;
    CData/*0:0*/ mulDivUnit__DOT____Vlvbound_he06413ab__0;
    mulDivUnit__DOT____Vlvbound_he06413ab__0 = 0;
    CData/*0:0*/ mulDivUnit__DOT____Vlvbound_h86b5a7bb__0;
    mulDivUnit__DOT____Vlvbound_h86b5a7bb__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__555__detectRange;
    __Vfunc_SelectiveFlushDetector__555__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__555__headPtr;
    __Vfunc_SelectiveFlushDetector__555__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__555__tailPtr;
    __Vfunc_SelectiveFlushDetector__555__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__555__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__555__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__555__opPtr;
    __Vfunc_SelectiveFlushDetector__555__opPtr = 0;
    // Body
    vlSelfRef.__PVT__mulDivUnit__DOT__nextPhase[0U] 
        = vlSelfRef.__PVT__mulDivUnit__DOT__regPhase
        [0U];
    vlSelfRef.__PVT__mulDivUnit__DOT__nextActiveListPtr[0U] 
        = vlSelfRef.__PVT__mulDivUnit__DOT__regActiveListPtr
        [0U];
    if ((2U & vlSelfRef.__PVT__mulDivUnit__DOT__regPhase
         [0U])) {
        if ((1U & vlSelfRef.__PVT__mulDivUnit__DOT__regPhase
             [0U])) {
            if (vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divRelease
                [0U]) {
                vlSelfRef.__PVT__mulDivUnit__DOT__nextPhase[0U] = 0U;
            }
        } else if (vlSelfRef.__PVT__mulDivUnit__DOT__finished
                   [0U]) {
            vlSelfRef.__PVT__mulDivUnit__DOT__nextPhase[0U] = 3U;
        }
    } else if ((1U & vlSelfRef.__PVT__mulDivUnit__DOT__regPhase
                [0U])) {
        if (vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReq
            [0U]) {
            vlSelfRef.__PVT__mulDivUnit__DOT__nextPhase[0U] = 2U;
        }
    } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divAcquire
               [0U]) {
        vlSelfRef.mulDivUnit__DOT____Vlvbound_ha7e1d1c3__0 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__acquireActiveListPtr
            [0U];
        vlSelfRef.__PVT__mulDivUnit__DOT__nextPhase[0U] = 1U;
        vlSelfRef.__PVT__mulDivUnit__DOT__nextActiveListPtr[0U] 
            = vlSelfRef.mulDivUnit__DOT____Vlvbound_ha7e1d1c3__0;
    }
    __Vfunc_SelectiveFlushDetector__555__opPtr = vlSelfRef.__PVT__mulDivUnit__DOT__regActiveListPtr
        [0U];
    __Vfunc_SelectiveFlushDetector__555__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__555__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__555__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__555__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__555__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__555__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 1U;
                goto __Vlabel150;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__555__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__555__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__555__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__555__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__555__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__555__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__555__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 1U;
                    goto __Vlabel150;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 0U;
                    goto __Vlabel150;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__555__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__555__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__555__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__555__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__555__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__555__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__555__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 1U;
                    goto __Vlabel150;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__555__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__555__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__555__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__555__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 1U;
                    goto __Vlabel150;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 0U;
                    goto __Vlabel150;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 0U;
                goto __Vlabel150;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout = 0U;
        }
        __Vlabel150: ;
    }
    mulDivUnit__DOT____Vlvbound_h4d794342__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__555__Vfuncout;
    vlSelfRef.__PVT__mulDivUnit__DOT__flush[0U] = mulDivUnit__DOT____Vlvbound_h4d794342__0;
    if (vlSelfRef.__PVT__mulDivUnit__DOT__flush[0U]) {
        vlSelfRef.__PVT__mulDivUnit__DOT__nextPhase[0U] = 0U;
    }
    mulDivUnit__DOT____Vlvbound_h7d0655ac__0 = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                                                | vlSelfRef.__PVT__mulDivUnit__DOT__flush
                                                [0U]);
    vlSelfRef.__PVT__mulDivUnit__DOT__rst_divider[0U] 
        = mulDivUnit__DOT____Vlvbound_h7d0655ac__0;
    mulDivUnit__DOT____Vlvbound_h49e4d866__0 = (0U 
                                                == 
                                                vlSelfRef.__PVT__mulDivUnit__DOT__nextPhase
                                                [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divFree[0U] 
        = mulDivUnit__DOT____Vlvbound_h49e4d866__0;
    mulDivUnit__DOT____Vlvbound_h14f7d4db__0 = (3U 
                                                == 
                                                vlSelfRef.__PVT__mulDivUnit__DOT__regPhase
                                                [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divFinished[0U] 
        = mulDivUnit__DOT____Vlvbound_h14f7d4db__0;
    mulDivUnit__DOT____Vlvbound_he06413ab__0 = (2U 
                                                == 
                                                vlSelfRef.__PVT__mulDivUnit__DOT__regPhase
                                                [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divBusy[0U] 
        = mulDivUnit__DOT____Vlvbound_he06413ab__0;
    mulDivUnit__DOT____Vlvbound_h86b5a7bb__0 = (1U 
                                                == 
                                                vlSelfRef.__PVT__mulDivUnit__DOT__regPhase
                                                [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReserved[0U] 
        = mulDivUnit__DOT____Vlvbound_h86b5a7bb__0;
    vlSelfRef.__PVT__mulDivUnit__DOT__unnamedblk2__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divRelease[0U] 
        = (((vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divFinished
             [0U] & (vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
                     [0U][1U][2U] >> 0x13U)) & (1U 
                                                == 
                                                (7U 
                                                 & vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
                                                 [0U]
                                                 [1U][2U]))) 
           & (vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
              [0U][1U][2U] >> 0x12U));
    complexExStage__DOT____Vlvbound_hd30db1a5__0 = 
        (0xfffU & (vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                   [0U][5U] >> 0xcU));
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
        = ((0xfffffU & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
            [0U][0U][2U]) | ((IData)(complexExStage__DOT____Vlvbound_hd30db1a5__0) 
                             << 0x14U));
    complexExStage__DOT____Vlvbound_h7bb3cb4c__0 = 
        (1U & ((~ vlSelfRef.__PVT__complexExStage__DOT__flush
                [0U][0U]) & (vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                             [0U][5U] >> 0xbU)));
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
            [0U][0U][2U]) | ((IData)(complexExStage__DOT____Vlvbound_h7bb3cb4c__0) 
                             << 0x13U));
    complexExStage__DOT____Vlvbound_hc34d7a17__0[0U] 
        = ((vlSelfRef.__PVT__complexExStage__DOT__pipeReg
            [0U][3U] << 9U) | (vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                               [0U][2U] >> 0x17U));
    complexExStage__DOT____Vlvbound_hc34d7a17__0[1U] 
        = ((vlSelfRef.__PVT__complexExStage__DOT__pipeReg
            [0U][4U] << 9U) | (vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                               [0U][3U] >> 0x17U));
    complexExStage__DOT____Vlvbound_hc34d7a17__0[2U] 
        = (0x3ffffU & ((vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                        [0U][5U] << 9U) | (vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                                           [0U][4U] 
                                           >> 0x17U)));
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][0U][0U] 
        = complexExStage__DOT____Vlvbound_hc34d7a17__0[0U];
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][0U][1U] 
        = complexExStage__DOT____Vlvbound_hc34d7a17__0[1U];
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
        = ((0xfffc0000U & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
            [0U][0U][2U]) | complexExStage__DOT____Vlvbound_hc34d7a17__0[2U]);
    if (vlSelfRef.__PVT__complexExStage__DOT__isDiv
        [0U]) {
        vlSelfRef.complexExStage__DOT____Vlvbound_h7bb5401b__0 
            = ((vlSelfRef.__PVT__complexExStage__DOT__pipeReg
                [0U][5U] >> 0xaU) & vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divFinished
               [0U]);
        vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
            = ((0xfffbffffU & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
                [0U][0U][2U]) | ((IData)(vlSelfRef.complexExStage__DOT____Vlvbound_h7bb5401b__0) 
                                 << 0x12U));
    } else {
        vlSelfRef.complexExStage__DOT____Vlvbound_h7bb5401b__1 
            = vlSelfRef.__PVT__complexExStage__DOT__regValid
            [0U];
        vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
            = ((0xfffbffffU & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
                [0U][0U][2U]) | ((IData)(vlSelfRef.complexExStage__DOT____Vlvbound_h7bb5401b__1) 
                                 << 0x12U));
    }
    complexExStage__DOT____Vlvbound_h59117768__0 = 
        (vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
         [0U][0U][2U] >> 0x14U);
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0xfffffU & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | ((IData)(complexExStage__DOT____Vlvbound_h59117768__0) 
                             << 0x14U));
    complexExStage__DOT____Vlvbound_h00cce467__0 = 
        (1U & ((~ vlSelfRef.__PVT__complexExStage__DOT__flush
                [0U][1U]) & (vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
                             [0U][0U][2U] >> 0x13U)));
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0xfff7ffffU & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | ((IData)(complexExStage__DOT____Vlvbound_h00cce467__0) 
                             << 0x13U));
    complexExStage__DOT____Vlvbound_h00cc54d8__0 = 
        (1U & (vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
               [0U][0U][2U] >> 0x12U));
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0xfffbffffU & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | ((IData)(complexExStage__DOT____Vlvbound_h00cc54d8__0) 
                             << 0x12U));
    complexExStage__DOT____Vlvbound_h51aca2a4__0[0U] 
        = vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
        [0U][0U][0U];
    complexExStage__DOT____Vlvbound_h51aca2a4__0[1U] 
        = vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
        [0U][0U][1U];
    complexExStage__DOT____Vlvbound_h51aca2a4__0[2U] 
        = (0x3ffffU & vlSelfRef.__PVT__complexExStage__DOT__localPipeReg
           [0U][0U][2U]);
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][1U][0U] 
        = complexExStage__DOT____Vlvbound_h51aca2a4__0[0U];
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][1U][1U] 
        = complexExStage__DOT____Vlvbound_h51aca2a4__0[1U];
    vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0xfffc0000U & vlSelfRef.__PVT__complexExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | complexExStage__DOT____Vlvbound_h51aca2a4__0[2U]);
    vlSelfRef.__PVT__scheduler__DOT__canIssueDiv = 1U;
    if ((1U & (~ vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divFree
               [0U]))) {
        vlSelfRef.__PVT__scheduler__DOT__canIssueDiv = 0U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0U] 
        = (1U & (((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                  & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                 & ((~ (IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv)) 
                    | ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                       & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[1U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 1U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 1U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 1U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[2U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 2U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 2U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 2U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[3U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 3U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 3U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 3U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[4U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 4U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 4U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 4U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[5U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 5U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 5U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 5U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[6U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 6U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 6U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 6U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[7U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 7U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 7U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 7U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[8U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 8U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 8U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 8U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[9U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 9U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                >> 9U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                            >> 9U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0xaU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 0xaU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                  >> 0xaU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                                >> 0xaU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0xbU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 0xbU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                  >> 0xbU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                                >> 0xbU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0xcU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 0xcU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                  >> 0xcU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                                >> 0xcU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0xdU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 0xdU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                  >> 0xdU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                                >> 0xdU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0xeU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 0xeU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                  >> 0xeU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                                >> 0xeU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0xfU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isComplex)) 
                  >> 0xfU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                  >> 0xfU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isDiv) 
                                                >> 0xfU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueDiv)))));
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__50(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___stl_comb__TOP__SMT_RTL_Testbench__core__50\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*11:0*/ fpExStage__DOT____Vlvbound_h033cc2ef__0;
    fpExStage__DOT____Vlvbound_h033cc2ef__0 = 0;
    CData/*0:0*/ fpExStage__DOT____Vlvbound_h00d27605__0;
    fpExStage__DOT____Vlvbound_h00d27605__0 = 0;
    VlWide<3>/*92:0*/ fpExStage__DOT____Vlvbound_h19911eea__0;
    VL_ZERO_W(93, fpExStage__DOT____Vlvbound_h19911eea__0);
    SData/*11:0*/ fpExStage__DOT____Vlvbound_hf054b8c4__0;
    fpExStage__DOT____Vlvbound_hf054b8c4__0 = 0;
    CData/*0:0*/ fpExStage__DOT____Vlvbound_hb707da19__0;
    fpExStage__DOT____Vlvbound_hb707da19__0 = 0;
    CData/*0:0*/ fpExStage__DOT____Vlvbound_hb70d5785__0;
    fpExStage__DOT____Vlvbound_hb70d5785__0 = 0;
    VlWide<3>/*92:0*/ fpExStage__DOT____Vlvbound_hde270e27__0;
    VL_ZERO_W(93, fpExStage__DOT____Vlvbound_hde270e27__0);
    CData/*0:0*/ fpDivSqrtUnit__DOT____Vlvbound_h4d794342__0;
    fpDivSqrtUnit__DOT____Vlvbound_h4d794342__0 = 0;
    CData/*0:0*/ fpDivSqrtUnit__DOT____Vlvbound_h7d0655ac__0;
    fpDivSqrtUnit__DOT____Vlvbound_h7d0655ac__0 = 0;
    CData/*0:0*/ fpDivSqrtUnit__DOT____Vlvbound_he40ef903__0;
    fpDivSqrtUnit__DOT____Vlvbound_he40ef903__0 = 0;
    CData/*0:0*/ fpDivSqrtUnit__DOT____Vlvbound_h7d81fd01__0;
    fpDivSqrtUnit__DOT____Vlvbound_h7d81fd01__0 = 0;
    CData/*0:0*/ fpDivSqrtUnit__DOT____Vlvbound_h6d7d750a__0;
    fpDivSqrtUnit__DOT____Vlvbound_h6d7d750a__0 = 0;
    CData/*0:0*/ fpDivSqrtUnit__DOT____Vlvbound_h08df1577__0;
    fpDivSqrtUnit__DOT____Vlvbound_h08df1577__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__631__detectRange;
    __Vfunc_SelectiveFlushDetector__631__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__631__headPtr;
    __Vfunc_SelectiveFlushDetector__631__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__631__tailPtr;
    __Vfunc_SelectiveFlushDetector__631__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__631__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__631__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__631__opPtr;
    __Vfunc_SelectiveFlushDetector__631__opPtr = 0;
    // Body
    vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextPhase[0U] 
        = vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regPhase
        [0U];
    vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextActiveListPtr[0U] 
        = vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regActiveListPtr
        [0U];
    if ((2U & vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regPhase
         [0U])) {
        if ((1U & vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regPhase
             [0U])) {
            if (vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Release
                [0U]) {
                vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextPhase[0U] = 0U;
            }
        } else if (vlSelfRef.__PVT__fpDivSqrtUnit__DOT__finished
                   [0U]) {
            vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextPhase[0U] = 3U;
        }
    } else if ((1U & vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regPhase
                [0U])) {
        if (vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Req
            [0U]) {
            vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextPhase[0U] = 2U;
        }
    } else if (vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Acquire
               [0U]) {
        vlSelfRef.fpDivSqrtUnit__DOT____Vlvbound_ha7e1d1c3__0 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__acquireActiveListPtr
            [0U];
        vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextPhase[0U] = 1U;
        vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextActiveListPtr[0U] 
            = vlSelfRef.fpDivSqrtUnit__DOT____Vlvbound_ha7e1d1c3__0;
    }
    __Vfunc_SelectiveFlushDetector__631__opPtr = vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regActiveListPtr
        [0U];
    __Vfunc_SelectiveFlushDetector__631__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__631__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__631__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__631__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__631__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__631__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 1U;
                goto __Vlabel151;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__631__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__631__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__631__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__631__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__631__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__631__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__631__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 1U;
                    goto __Vlabel151;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 0U;
                    goto __Vlabel151;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__631__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__631__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__631__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__631__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__631__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__631__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__631__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 1U;
                    goto __Vlabel151;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__631__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__631__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__631__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__631__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 1U;
                    goto __Vlabel151;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 0U;
                    goto __Vlabel151;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 0U;
                goto __Vlabel151;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout = 0U;
        }
        __Vlabel151: ;
    }
    fpDivSqrtUnit__DOT____Vlvbound_h4d794342__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__631__Vfuncout;
    vlSelfRef.__PVT__fpDivSqrtUnit__DOT__flush[0U] 
        = fpDivSqrtUnit__DOT____Vlvbound_h4d794342__0;
    if (vlSelfRef.__PVT__fpDivSqrtUnit__DOT__flush[0U]) {
        vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextPhase[0U] = 0U;
    }
    fpDivSqrtUnit__DOT____Vlvbound_h7d0655ac__0 = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                                                   | vlSelfRef.__PVT__fpDivSqrtUnit__DOT__flush
                                                   [0U]);
    vlSelfRef.__PVT__fpDivSqrtUnit__DOT__rst_divider[0U] 
        = fpDivSqrtUnit__DOT____Vlvbound_h7d0655ac__0;
    fpDivSqrtUnit__DOT____Vlvbound_he40ef903__0 = (0U 
                                                   == 
                                                   vlSelfRef.__PVT__fpDivSqrtUnit__DOT__nextPhase
                                                   [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Free[0U] 
        = fpDivSqrtUnit__DOT____Vlvbound_he40ef903__0;
    fpDivSqrtUnit__DOT____Vlvbound_h7d81fd01__0 = (3U 
                                                   == 
                                                   vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regPhase
                                                   [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Finished[0U] 
        = fpDivSqrtUnit__DOT____Vlvbound_h7d81fd01__0;
    fpDivSqrtUnit__DOT____Vlvbound_h6d7d750a__0 = (2U 
                                                   == 
                                                   vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regPhase
                                                   [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Busy[0U] 
        = fpDivSqrtUnit__DOT____Vlvbound_h6d7d750a__0;
    fpDivSqrtUnit__DOT____Vlvbound_h08df1577__0 = (1U 
                                                   == 
                                                   vlSelfRef.__PVT__fpDivSqrtUnit__DOT__regPhase
                                                   [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Reserved[0U] 
        = fpDivSqrtUnit__DOT____Vlvbound_h08df1577__0;
    vlSelfRef.__PVT__fpDivSqrtUnit__DOT__unnamedblk2__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Release[0U] 
        = (((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Finished
             [0U] & (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                     [0U][3U][2U] >> 0x1eU)) & ((2U 
                                                 == 
                                                 (7U 
                                                  & (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                     [0U]
                                                     [3U][2U] 
                                                     >> 0xeU))) 
                                                | (3U 
                                                   == 
                                                   (7U 
                                                    & (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                       [0U]
                                                       [3U][2U] 
                                                       >> 0xeU))))) 
           & (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
              [0U][3U][2U] >> 0x1dU));
    fpExStage__DOT____Vlvbound_h033cc2ef__0 = (0xfffU 
                                               & ((vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                   [0U][7U] 
                                                   << 8U) 
                                                  | (vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                     [0U][6U] 
                                                     >> 0x18U)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][0U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_h033cc2ef__0) 
                             << 0x1fU));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][3U] 
        = (0x7ffU & ((IData)(fpExStage__DOT____Vlvbound_h033cc2ef__0) 
                     >> 1U));
    fpExStage__DOT____Vlvbound_h00d27605__0 = (1U & 
                                               ((~ 
                                                 vlSelfRef.__PVT__fpExStage__DOT__flush
                                                 [0U]
                                                 [0U]) 
                                                & (vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                   [0U][6U] 
                                                   >> 0x17U)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][0U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_h00d27605__0) 
                             << 0x1eU));
    fpExStage__DOT____Vlvbound_h19911eea__0[0U] = (
                                                   (vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                    [0U][4U] 
                                                    << 8U) 
                                                   | (vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                      [0U][3U] 
                                                      >> 0x18U));
    fpExStage__DOT____Vlvbound_h19911eea__0[1U] = (
                                                   (vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                    [0U][5U] 
                                                    << 8U) 
                                                   | (vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                      [0U][4U] 
                                                      >> 0x18U));
    fpExStage__DOT____Vlvbound_h19911eea__0[2U] = (0x1fffffffU 
                                                   & ((vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                       [0U][6U] 
                                                       << 8U) 
                                                      | (vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                                                         [0U][5U] 
                                                         >> 0x18U)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][0U] 
        = fpExStage__DOT____Vlvbound_h19911eea__0[0U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][1U] 
        = fpExStage__DOT____Vlvbound_h19911eea__0[1U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
        = ((0xe0000000U & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][0U][2U]) | fpExStage__DOT____Vlvbound_h19911eea__0[2U]);
    if (vlSelfRef.__PVT__fpExStage__DOT__isDivSqrt[0U]) {
        vlSelfRef.fpExStage__DOT____Vlvbound_h00bc0b09__0 
            = ((vlSelfRef.__PVT__fpExStage__DOT__pipeReg
                [0U][6U] >> 0x16U) & vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Finished
               [0U]);
        vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
            = ((0xdfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
                [0U][0U][2U]) | ((IData)(vlSelfRef.fpExStage__DOT____Vlvbound_h00bc0b09__0) 
                                 << 0x1dU));
    } else {
        vlSelfRef.fpExStage__DOT____Vlvbound_h00bc0b09__1 
            = vlSelfRef.__PVT__fpExStage__DOT__regValid
            [0U];
        vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][0U][2U] 
            = ((0xdfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
                [0U][0U][2U]) | ((IData)(vlSelfRef.fpExStage__DOT____Vlvbound_h00bc0b09__1) 
                                 << 0x1dU));
    }
    fpExStage__DOT____Vlvbound_hf054b8c4__0 = (0xfffU 
                                               & ((vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [0U][3U] 
                                                   << 1U) 
                                                  | (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                     [0U]
                                                     [0U][2U] 
                                                     >> 0x1fU)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hf054b8c4__0) 
                             << 0x1fU));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][1U][3U] 
        = (0x7ffU & ((IData)(fpExStage__DOT____Vlvbound_hf054b8c4__0) 
                     >> 1U));
    fpExStage__DOT____Vlvbound_hb707da19__0 = (1U & 
                                               ((~ 
                                                 vlSelfRef.__PVT__fpExStage__DOT__flush
                                                 [0U]
                                                 [1U]) 
                                                & (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [0U][2U] 
                                                   >> 0x1eU)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hb707da19__0) 
                             << 0x1eU));
    fpExStage__DOT____Vlvbound_hb70d5785__0 = (1U & 
                                               (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                [0U]
                                                [0U][2U] 
                                                >> 0x1dU));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hb70d5785__0) 
                             << 0x1dU));
    fpExStage__DOT____Vlvbound_hde270e27__0[0U] = vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
        [0U][0U][0U];
    fpExStage__DOT____Vlvbound_hde270e27__0[1U] = vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
        [0U][0U][1U];
    fpExStage__DOT____Vlvbound_hde270e27__0[2U] = (0x1fffffffU 
                                                   & vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [0U][2U]);
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][1U][0U] 
        = fpExStage__DOT____Vlvbound_hde270e27__0[0U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][1U][1U] 
        = fpExStage__DOT____Vlvbound_hde270e27__0[1U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][1U][2U] 
        = ((0xe0000000U & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][1U][2U]) | fpExStage__DOT____Vlvbound_hde270e27__0[2U]);
    fpExStage__DOT____Vlvbound_hf054b8c4__0 = (0xfffU 
                                               & ((vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [1U][3U] 
                                                   << 1U) 
                                                  | (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                     [0U]
                                                     [1U][2U] 
                                                     >> 0x1fU)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][2U][2U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][2U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hf054b8c4__0) 
                             << 0x1fU));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][2U][3U] 
        = (0x7ffU & ((IData)(fpExStage__DOT____Vlvbound_hf054b8c4__0) 
                     >> 1U));
    fpExStage__DOT____Vlvbound_hb707da19__0 = (1U & 
                                               ((~ 
                                                 vlSelfRef.__PVT__fpExStage__DOT__flush
                                                 [0U]
                                                 [2U]) 
                                                & (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [1U][2U] 
                                                   >> 0x1eU)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][2U][2U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][2U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hb707da19__0) 
                             << 0x1eU));
    fpExStage__DOT____Vlvbound_hb70d5785__0 = (1U & 
                                               (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                [0U]
                                                [1U][2U] 
                                                >> 0x1dU));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][2U][2U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][2U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hb70d5785__0) 
                             << 0x1dU));
    fpExStage__DOT____Vlvbound_hde270e27__0[0U] = vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
        [0U][1U][0U];
    fpExStage__DOT____Vlvbound_hde270e27__0[1U] = vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
        [0U][1U][1U];
    fpExStage__DOT____Vlvbound_hde270e27__0[2U] = (0x1fffffffU 
                                                   & vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [1U][2U]);
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][2U][0U] 
        = fpExStage__DOT____Vlvbound_hde270e27__0[0U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][2U][1U] 
        = fpExStage__DOT____Vlvbound_hde270e27__0[1U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][2U][2U] 
        = ((0xe0000000U & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][2U][2U]) | fpExStage__DOT____Vlvbound_hde270e27__0[2U]);
    fpExStage__DOT____Vlvbound_hf054b8c4__0 = (0xfffU 
                                               & ((vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [2U][3U] 
                                                   << 1U) 
                                                  | (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                     [0U]
                                                     [2U][2U] 
                                                     >> 0x1fU)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][3U][2U] 
        = ((0x7fffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][3U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hf054b8c4__0) 
                             << 0x1fU));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][3U][3U] 
        = (0x7ffU & ((IData)(fpExStage__DOT____Vlvbound_hf054b8c4__0) 
                     >> 1U));
    fpExStage__DOT____Vlvbound_hb707da19__0 = (1U & 
                                               ((~ 
                                                 vlSelfRef.__PVT__fpExStage__DOT__flush
                                                 [0U]
                                                 [3U]) 
                                                & (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [2U][2U] 
                                                   >> 0x1eU)));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][3U][2U] 
        = ((0xbfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][3U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hb707da19__0) 
                             << 0x1eU));
    fpExStage__DOT____Vlvbound_hb70d5785__0 = (1U & 
                                               (vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                [0U]
                                                [2U][2U] 
                                                >> 0x1dU));
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][3U][2U] 
        = ((0xdfffffffU & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][3U][2U]) | ((IData)(fpExStage__DOT____Vlvbound_hb70d5785__0) 
                             << 0x1dU));
    fpExStage__DOT____Vlvbound_hde270e27__0[0U] = vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
        [0U][2U][0U];
    fpExStage__DOT____Vlvbound_hde270e27__0[1U] = vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
        [0U][2U][1U];
    fpExStage__DOT____Vlvbound_hde270e27__0[2U] = (0x1fffffffU 
                                                   & vlSelfRef.__PVT__fpExStage__DOT__localPipeReg
                                                   [0U]
                                                   [2U][2U]);
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][3U][0U] 
        = fpExStage__DOT____Vlvbound_hde270e27__0[0U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][3U][1U] 
        = fpExStage__DOT____Vlvbound_hde270e27__0[1U];
    vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg[0U][3U][2U] 
        = ((0xe0000000U & vlSelfRef.__PVT__fpExStage__DOT__nextLocalPipeReg
            [0U][3U][2U]) | fpExStage__DOT____Vlvbound_hde270e27__0[2U]);
    vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt = 1U;
    if ((1U & (~ vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Free
               [0U]))) {
        vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt = 0U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0U] 
        = (1U & (((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                  & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                 & ((~ (IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt)) 
                    | ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                       & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[1U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 1U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 1U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 1U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[2U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 2U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 2U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 2U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[3U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 3U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 3U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 3U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[4U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 4U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 4U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 4U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[5U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 5U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 5U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 5U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[6U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 6U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 6U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 6U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[7U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 7U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 7U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 7U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[8U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 8U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 8U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 8U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[9U] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 9U) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                >> 9U)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                            >> 9U) 
                                           & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0xaU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 0xaU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                  >> 0xaU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                                >> 0xaU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0xbU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 0xbU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                  >> 0xbU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                                >> 0xbU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0xcU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 0xcU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                  >> 0xcU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                                >> 0xcU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0xdU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 0xdU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                  >> 0xdU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                                >> 0xdU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0xeU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 0xeU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                  >> 0xeU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                                >> 0xeU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0xfU] 
        = (1U & ((((IData)(vlSelfRef.__PVT__scheduler__DOT__notIssued) 
                   & (IData)(vlSelfRef.__PVT__scheduler__DOT__isFP)) 
                  >> 0xfU) & ((~ ((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                  >> 0xfU)) | (((IData)(vlSelfRef.__PVT__scheduler__DOT__isFPDivSqrt) 
                                                >> 0xfU) 
                                               & (IData)(vlSelfRef.__PVT__scheduler__DOT__canIssueFPDivSqrt)))));
}
