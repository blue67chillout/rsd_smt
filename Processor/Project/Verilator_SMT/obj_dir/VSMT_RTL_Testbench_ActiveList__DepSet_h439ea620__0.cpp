// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_ActiveList.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___ico_sequent__TOP__SMT_RTL_Testbench__core__activeList__0(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___ico_sequent__TOP__SMT_RTL_Testbench__core__activeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ __Vlvbound_h2b7b2099__0;
    __Vlvbound_h2b7b2099__0 = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__0__Vfuncout;
    __Vfunc_ActiveListPtrToAge__0__Vfuncout = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__0__ptr;
    __Vfunc_ActiveListPtrToAge__0__ptr = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__0__head;
    __Vfunc_ActiveListPtrToAge__0__head = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__0__age;
    __Vfunc_ActiveListPtrToAge__0__age = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__ptr = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__1__head;
    __Vfunc_ActiveListPtrToAge__1__head = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__1__age;
    __Vfunc_ActiveListPtrToAge__1__age = 0;
    IData/*31:0*/ __Vfunc_ToAddrFromPC__3__Vfuncout;
    __Vfunc_ToAddrFromPC__3__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToAddrFromPC__3__pc;
    __Vfunc_ToAddrFromPC__3__pc = 0;
    // Body
    vlSelfRef.__PVT__headPtrList[0U] = vlSelfRef.headPtr;
    vlSelfRef.__PVT__headPtrList[1U] = (0x3fU & ((0x40U 
                                                  > 
                                                  ((IData)(1U) 
                                                   + (IData)(vlSelfRef.headPtr)))
                                                  ? 
                                                 ((IData)(1U) 
                                                  + (IData)(vlSelfRef.headPtr))
                                                  : 
                                                 ((IData)(1U) 
                                                  + (IData)(vlSelfRef.headPtr))));
    vlSelfRef.__PVT__tailPtrList[0U] = (0x3fU & ((0x40U 
                                                  <= 
                                                  ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)))
                                                  ? 
                                                 ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                  - (IData)(1U))
                                                  : 
                                                 ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                  - (IData)(1U))));
    vlSelfRef.__PVT__tailPtrList[1U] = (0x3fU & ((0x40U 
                                                  <= 
                                                  (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                    - (IData)(1U)) 
                                                   - (IData)(1U)))
                                                  ? 
                                                 (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)) 
                                                  - (IData)(1U))
                                                  : 
                                                 (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)) 
                                                  - (IData)(1U))));
    if ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum))) {
        vlSelfRef.__PVT__readPtrList[0U] = vlSelfRef.__PVT__tailPtrList
            [0U];
        vlSelfRef.__PVT__readPtrList[1U] = vlSelfRef.__PVT__tailPtrList
            [1U];
    } else {
        vlSelfRef.__PVT__readPtrList[0U] = vlSelfRef.__PVT__headPtrList
            [0U];
        vlSelfRef.__PVT__readPtrList[1U] = vlSelfRef.__PVT__headPtrList
            [1U];
    }
    vlSelfRef.__PVT__nextRecoveryReg[0U] = vlSelfRef.__PVT__recoveryReg[0U];
    vlSelfRef.__PVT__nextRecoveryReg[1U] = vlSelfRef.__PVT__recoveryReg[1U];
    vlSelfRef.__PVT__nextRecoveryReg[2U] = vlSelfRef.__PVT__recoveryReg[2U];
    __Vfunc_ActiveListPtrToAge__0__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__0__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__recoveryReg[0U] 
                                                   >> 0xdU));
    __Vfunc_ActiveListPtrToAge__0__age = __Vfunc_ActiveListPtrToAge__0__ptr;
    __Vfunc_ActiveListPtrToAge__0__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__0__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__0__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__0__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__0__age)));
    vlSelfRef.__PVT__oldestAge = __Vfunc_ActiveListPtrToAge__0__Vfuncout;
    vlSelfRef.__PVT__exceptionIndex = 0U;
    vlSelfRef.__PVT__exceptionDetected = 0U;
    vlSelfRef.__PVT__refetchType = 0U;
    vlSelfRef.__PVT__startRecoveryAtCommit = 0U;
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [0U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[0U] = __Vlvbound_h2b7b2099__0;
    if ((vlSelfRef.__PVT__we[0U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [0U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [0U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [0U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [0U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [0U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [0U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [0U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [0U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 0U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [0U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [0U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [0U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [0U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [0U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [0U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [0U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [0U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [0U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [0U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [0U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [0U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [0U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [0U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [1U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[1U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[1U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [1U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [1U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [1U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [1U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [1U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [1U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [1U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [1U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 1U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [1U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [1U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [1U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [1U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [1U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [1U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [1U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [1U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [1U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [1U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [1U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [1U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [1U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [1U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [2U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[2U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[2U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [2U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [2U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [2U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [2U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [2U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [2U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [2U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [2U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 2U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [2U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [2U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [2U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [2U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [2U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [2U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [2U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [2U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [2U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [2U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [2U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [2U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [2U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [2U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [3U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[3U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[3U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [3U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [3U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [3U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [3U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [3U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [3U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [3U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [3U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 3U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [3U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [3U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [3U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [3U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [3U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [3U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [3U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [3U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [3U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [3U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [3U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [3U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [3U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [3U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [4U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[4U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[4U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [4U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [4U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [4U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [4U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [4U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [4U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [4U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [4U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 4U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [4U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [4U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [4U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [4U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [4U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [4U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [4U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [4U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [4U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [4U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [4U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [4U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [4U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [4U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [5U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[5U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[5U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [5U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [5U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [5U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [5U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [5U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [5U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [5U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [5U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 5U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [5U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [5U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [5U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [5U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [5U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [5U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [5U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [5U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [5U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [5U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [5U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [5U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [5U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [5U][1U] >> 0x16U)));
        }
    }
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    __Vfunc_ToAddrFromPC__3__pc = (0xfffffU & ((vlSelfRef.__PVT__nextRecoveryReg[2U] 
                                                << 0xdU) 
                                               | (vlSelfRef.__PVT__nextRecoveryReg[1U] 
                                                  >> 0x13U)));
    __Vfunc_ToAddrFromPC__3__Vfuncout = ((0x80000000U 
                                          & (__Vfunc_ToAddrFromPC__3__pc 
                                             << 0xdU)) 
                                         | (0x3ffffU 
                                            & __Vfunc_ToAddrFromPC__3__pc));
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage 
        = __Vfunc_ToAddrFromPC__3__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInRwStage 
        = ((IData)(vlSelfRef.__PVT__exceptionDetected) 
           & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromRwStage 
        = vlSelfRef.__PVT__refetchType;
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__exceptionOpPtr 
        = (0x3fU & (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                    >> 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__ra[0U] 
        = vlSelfRef.__PVT__readPtrList[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__ra[1U] 
        = vlSelfRef.__PVT__readPtrList[1U];
    vlSelfRef.__PVT__ffsRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__ffsRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    vlSelfRef.__PVT__esRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__esRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    vlSelfRef.__PVT__headExecState[0U] = (0xfU & ((
                                                   (vlSelfRef.__PVT__recoveryReg[0U] 
                                                    >> 4U) 
                                                   & (vlSelfRef.__PVT__esRA
                                                      [0U] 
                                                      == 
                                                      (0x3fU 
                                                       & (vlSelfRef.__PVT__recoveryReg[0U] 
                                                          >> 0xdU))))
                                                   ? 
                                                  vlSelfRef.__PVT__recoveryReg[0U]
                                                   : 
                                                  (vlSelfRef.__PVT__esRV
                                                   [0U]
                                                    ? 1U
                                                    : 0U)));
    vlSelfRef.__PVT__headExecState[1U] = (0xfU & ((
                                                   (vlSelfRef.__PVT__recoveryReg[0U] 
                                                    >> 4U) 
                                                   & (vlSelfRef.__PVT__esRA
                                                      [1U] 
                                                      == 
                                                      (0x3fU 
                                                       & (vlSelfRef.__PVT__recoveryReg[0U] 
                                                          >> 0xdU))))
                                                   ? 
                                                  vlSelfRef.__PVT__recoveryReg[0U]
                                                   : 
                                                  (vlSelfRef.__PVT__esRV
                                                   [1U]
                                                    ? 1U
                                                    : 0U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState[0U] 
        = vlSelfRef.__PVT__headExecState[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState[1U] 
        = vlSelfRef.__PVT__headExecState[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__ra[0U] 
        = vlSelfRef.__PVT__ffsRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__ra[1U] 
        = vlSelfRef.__PVT__ffsRA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__ra[0U] 
        = vlSelfRef.__PVT__esRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__ra[1U] 
        = vlSelfRef.__PVT__esRA[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___ico_comb__TOP__SMT_RTL_Testbench__core__activeList__0(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___ico_comb__TOP__SMT_RTL_Testbench__core__activeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__esRefRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__esRefRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__esRefWE[0U] = 0U;
        vlSelfRef.__PVT__esRefWA[0U] = 0U;
        vlSelfRef.__PVT__esRefWV[0U] = 0U;
        vlSelfRef.__PVT__unnamedblk16__DOT__i = 8U;
        vlSelfRef.__PVT__esRefWE[1U] = 0U;
        vlSelfRef.__PVT__esRefWA[1U] = 1U;
        vlSelfRef.__PVT__esRefWV[1U] = 0U;
        vlSelfRef.__PVT__esRefWE[2U] = 0U;
        vlSelfRef.__PVT__esRefWA[2U] = 2U;
        vlSelfRef.__PVT__esRefWV[2U] = 0U;
        vlSelfRef.__PVT__esRefWE[3U] = 0U;
        vlSelfRef.__PVT__esRefWA[3U] = 3U;
        vlSelfRef.__PVT__esRefWV[3U] = 0U;
        vlSelfRef.__PVT__esRefWE[4U] = 0U;
        vlSelfRef.__PVT__esRefWA[4U] = 4U;
        vlSelfRef.__PVT__esRefWV[4U] = 0U;
        vlSelfRef.__PVT__esRefWE[5U] = 0U;
        vlSelfRef.__PVT__esRefWA[5U] = 5U;
        vlSelfRef.__PVT__esRefWV[5U] = 0U;
        vlSelfRef.__PVT__esRefWE[6U] = 0U;
        vlSelfRef.__PVT__esRefWA[6U] = 6U;
        vlSelfRef.__PVT__esRefWV[6U] = 0U;
        vlSelfRef.__PVT__esRefWE[7U] = 0U;
        vlSelfRef.__PVT__esRefWA[7U] = 7U;
        vlSelfRef.__PVT__esRefWV[7U] = 0U;
    } else {
        vlSelfRef.__PVT__esRefWE[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [0U];
        vlSelfRef.__PVT__esRefWA[0U] = vlSelfRef.__PVT__pushedTailPtr
            [0U];
        vlSelfRef.__PVT__esRefWV[0U] = 0U;
        vlSelfRef.__PVT__unnamedblk17__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk18__DOT__i = 6U;
        vlSelfRef.__PVT__esRefWE[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [1U];
        vlSelfRef.__PVT__esRefWA[1U] = vlSelfRef.__PVT__pushedTailPtr
            [1U];
        vlSelfRef.__PVT__esRefWV[1U] = 0U;
        vlSelfRef.__PVT__esRefWE[2U] = vlSelfRef.__PVT__we
            [0U];
        vlSelfRef.__PVT__esRefWA[2U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [0U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[2U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (0U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [0U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[3U] = vlSelfRef.__PVT__we
            [1U];
        vlSelfRef.__PVT__esRefWA[3U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [1U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[3U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (1U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [1U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[4U] = vlSelfRef.__PVT__we
            [2U];
        vlSelfRef.__PVT__esRefWA[4U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [2U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[4U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (2U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [2U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[5U] = vlSelfRef.__PVT__we
            [3U];
        vlSelfRef.__PVT__esRefWA[5U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [3U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[5U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (3U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [3U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[6U] = vlSelfRef.__PVT__we
            [4U];
        vlSelfRef.__PVT__esRefWA[6U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [4U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[6U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (4U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [4U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[7U] = vlSelfRef.__PVT__we
            [5U];
        vlSelfRef.__PVT__esRefWA[7U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [5U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[7U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (5U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [5U][1U] 
                                                    >> 0x16U)));
    }
    vlSelfRef.__PVT__nextInRecovery = vlSelfRef.__PVT__regInRecovery;
    if ((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.__PVT__nextInRecovery = 1U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__toCommitPhase) {
        vlSelfRef.__PVT__nextInRecovery = 0U;
    }
    vlSelfRef.__PVT__execStateIsDifferentFromRef = 0U;
    vlSelfRef.__PVT__unnamedblk19__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__unnamedblk19__DOT__i)) {
            vlSelfRef.__PVT__headExecStateRef[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                = vlSelfRef.__PVT__esRefRV[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)];
            if (((vlSelfRef.__PVT__esRV[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                  & (vlSelfRef.__PVT__unnamedblk19__DOT__i 
                     < (IData)(vlSelfRef.__PVT__activeListPointer__DOT__regCount))) 
                 & (~ (IData)(vlSelfRef.__PVT__nextInRecovery)))) {
                vlSelfRef.__PVT__execStateIsDifferentFromRef 
                    = ((IData)(vlSelfRef.__PVT__execStateIsDifferentFromRef) 
                       | (vlSelfRef.__PVT__headExecState
                          [(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                          != vlSelfRef.__PVT__headExecStateRef
                          [(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)]));
            }
            if ((vlSelfRef.__PVT__esRA[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                 == (0x3fU & (vlSelfRef.__PVT__recoveryReg[0U] 
                              >> 0xdU)))) {
                goto __Vlabel1;
            }
            vlSelfRef.__PVT__unnamedblk19__DOT__i = 
                ((IData)(1U) + vlSelfRef.__PVT__unnamedblk19__DOT__i);
        }
        __Vlabel1: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[0U] 
        = vlSelfRef.__PVT__esRefWV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[1U] 
        = vlSelfRef.__PVT__esRefWV[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[2U] 
        = vlSelfRef.__PVT__esRefWV[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[3U] 
        = vlSelfRef.__PVT__esRefWV[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[4U] 
        = vlSelfRef.__PVT__esRefWV[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[5U] 
        = vlSelfRef.__PVT__esRefWV[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[6U] 
        = vlSelfRef.__PVT__esRefWV[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[7U] 
        = vlSelfRef.__PVT__esRefWV[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__ra[0U] 
        = vlSelfRef.__PVT__esRefRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__ra[1U] 
        = vlSelfRef.__PVT__esRefRA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[0U] 
        = vlSelfRef.__PVT__esRefWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[1U] 
        = vlSelfRef.__PVT__esRefWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[2U] 
        = vlSelfRef.__PVT__esRefWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[3U] 
        = vlSelfRef.__PVT__esRefWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[4U] 
        = vlSelfRef.__PVT__esRefWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[5U] 
        = vlSelfRef.__PVT__esRefWE[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[6U] 
        = vlSelfRef.__PVT__esRefWE[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[7U] 
        = vlSelfRef.__PVT__esRefWE[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[0U] 
        = vlSelfRef.__PVT__esRefWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[1U] 
        = vlSelfRef.__PVT__esRefWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[2U] 
        = vlSelfRef.__PVT__esRefWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[3U] 
        = vlSelfRef.__PVT__esRefWA[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[4U] 
        = vlSelfRef.__PVT__esRefWA[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[5U] 
        = vlSelfRef.__PVT__esRefWA[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[6U] 
        = vlSelfRef.__PVT__esRefWA[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[7U] 
        = vlSelfRef.__PVT__esRefWA[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__0(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vlvbound_hb8910d2c__0;
    __Vlvbound_hb8910d2c__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_heffd43cc__0;
    VL_ZERO_W(72, __Vlvbound_heffd43cc__0);
    CData/*0:0*/ __Vlvbound_hb74f74a4__0;
    __Vlvbound_hb74f74a4__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_hf2c31c44__0;
    VL_ZERO_W(72, __Vlvbound_hf2c31c44__0);
    CData/*0:0*/ __Vlvbound_he294a3d5__0;
    __Vlvbound_he294a3d5__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_hc2060c75__0;
    VL_ZERO_W(72, __Vlvbound_hc2060c75__0);
    CData/*0:0*/ __Vlvbound_hdd464555__0;
    __Vlvbound_hdd464555__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_h192a2af5__0;
    VL_ZERO_W(72, __Vlvbound_h192a2af5__0);
    // Body
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk10__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk11__DOT__i = 6U;
        vlSelfRef.__PVT__unnamedblk15__DOT__i = 1U;
        vlSelfRef.__PVT__unnamedblk14__DOT__i = 2U;
    }
    __Vlvbound_hb8910d2c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite
        [0U];
    vlSelfRef.__PVT__we[0U] = __Vlvbound_hb8910d2c__0;
    __Vlvbound_hb8910d2c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite
        [1U];
    vlSelfRef.__PVT__we[1U] = __Vlvbound_hb8910d2c__0;
    __Vlvbound_hb74f74a4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWrite
        [0U];
    vlSelfRef.__PVT__we[2U] = __Vlvbound_hb74f74a4__0;
    __Vlvbound_he294a3d5__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite
        [0U];
    vlSelfRef.__PVT__we[3U] = __Vlvbound_he294a3d5__0;
    __Vlvbound_he294a3d5__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite
        [1U];
    vlSelfRef.__PVT__we[4U] = __Vlvbound_he294a3d5__0;
    __Vlvbound_hdd464555__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWrite
        [0U];
    vlSelfRef.__PVT__we[5U] = __Vlvbound_hdd464555__0;
    __Vlvbound_heffd43cc__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [0U][0U];
    __Vlvbound_heffd43cc__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [0U][1U];
    __Vlvbound_heffd43cc__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[0U][0U] = __Vlvbound_heffd43cc__0[0U];
    vlSelfRef.__PVT__writeData[0U][1U] = __Vlvbound_heffd43cc__0[1U];
    vlSelfRef.__PVT__writeData[0U][2U] = __Vlvbound_heffd43cc__0[2U];
    __Vlvbound_heffd43cc__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [1U][0U];
    __Vlvbound_heffd43cc__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [1U][1U];
    __Vlvbound_heffd43cc__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [1U][2U];
    vlSelfRef.__PVT__writeData[1U][0U] = __Vlvbound_heffd43cc__0[0U];
    vlSelfRef.__PVT__writeData[1U][1U] = __Vlvbound_heffd43cc__0[1U];
    vlSelfRef.__PVT__writeData[1U][2U] = __Vlvbound_heffd43cc__0[2U];
    __Vlvbound_hf2c31c44__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData
        [0U][0U];
    __Vlvbound_hf2c31c44__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData
        [0U][1U];
    __Vlvbound_hf2c31c44__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[2U][0U] = __Vlvbound_hf2c31c44__0[0U];
    vlSelfRef.__PVT__writeData[2U][1U] = __Vlvbound_hf2c31c44__0[1U];
    vlSelfRef.__PVT__writeData[2U][2U] = __Vlvbound_hf2c31c44__0[2U];
    __Vlvbound_hc2060c75__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [0U][0U];
    __Vlvbound_hc2060c75__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [0U][1U];
    __Vlvbound_hc2060c75__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[3U][0U] = __Vlvbound_hc2060c75__0[0U];
    vlSelfRef.__PVT__writeData[3U][1U] = __Vlvbound_hc2060c75__0[1U];
    vlSelfRef.__PVT__writeData[3U][2U] = __Vlvbound_hc2060c75__0[2U];
    __Vlvbound_hc2060c75__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [1U][0U];
    __Vlvbound_hc2060c75__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [1U][1U];
    __Vlvbound_hc2060c75__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [1U][2U];
    vlSelfRef.__PVT__writeData[4U][0U] = __Vlvbound_hc2060c75__0[0U];
    vlSelfRef.__PVT__writeData[4U][1U] = __Vlvbound_hc2060c75__0[1U];
    vlSelfRef.__PVT__writeData[4U][2U] = __Vlvbound_hc2060c75__0[2U];
    __Vlvbound_h192a2af5__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData
        [0U][0U];
    __Vlvbound_h192a2af5__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData
        [0U][1U];
    __Vlvbound_h192a2af5__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[5U][0U] = __Vlvbound_h192a2af5__0[0U];
    vlSelfRef.__PVT__writeData[5U][1U] = __Vlvbound_h192a2af5__0[1U];
    vlSelfRef.__PVT__writeData[5U][2U] = __Vlvbound_h192a2af5__0[2U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk9__DOT__i = 8U;
        vlSelfRef.__PVT__unnamedblk13__DOT__i = 3U;
        vlSelfRef.__PVT__ffsWV[0U] = 0U;
        vlSelfRef.__PVT__ffsWV[1U] = 0U;
        vlSelfRef.__PVT__ffsWV[2U] = 0U;
        vlSelfRef.__PVT__esWV[0U] = 0U;
        vlSelfRef.__PVT__esWV[1U] = 0U;
        vlSelfRef.__PVT__esWV[2U] = 0U;
        vlSelfRef.__PVT__esWV[3U] = 0U;
        vlSelfRef.__PVT__esWV[4U] = 0U;
        vlSelfRef.__PVT__esWV[5U] = 0U;
        vlSelfRef.__PVT__esWV[6U] = 0U;
        vlSelfRef.__PVT__esWV[7U] = 0U;
    } else {
        vlSelfRef.__PVT__ffsWV[0U] = 0U;
        vlSelfRef.__PVT__ffsWV[1U] = 0U;
        vlSelfRef.__Vlvbound_hd8cbcf9b__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpFFlagsData
            [0U];
        vlSelfRef.__PVT__ffsWV[2U] = vlSelfRef.__Vlvbound_hd8cbcf9b__0;
        vlSelfRef.__PVT__esWV[0U] = 0U;
        vlSelfRef.__PVT__esWV[1U] = 0U;
        vlSelfRef.__PVT__esWV[2U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [0U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[3U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [1U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[4U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [2U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[5U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [3U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[6U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [4U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[7U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [5U][1U] 
                                             >> 0x16U)));
    }
    vlSelfRef.__Vcellinp__fflagsState__wv[0U] = vlSelfRef.__PVT__ffsWV
        [0U];
    vlSelfRef.__Vcellinp__fflagsState__wv[1U] = vlSelfRef.__PVT__ffsWV
        [1U];
    vlSelfRef.__Vcellinp__fflagsState__wv[2U] = vlSelfRef.__PVT__ffsWV
        [2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__fflagsState__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__fflagsState__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wv[2U] 
        = vlSelfRef.__Vcellinp__fflagsState__wv[2U];
    vlSelfRef.__Vcellinp__execState__wv[0U] = vlSelfRef.__PVT__esWV
        [0U];
    vlSelfRef.__Vcellinp__execState__wv[1U] = vlSelfRef.__PVT__esWV
        [1U];
    vlSelfRef.__Vcellinp__execState__wv[2U] = vlSelfRef.__PVT__esWV
        [2U];
    vlSelfRef.__Vcellinp__execState__wv[3U] = vlSelfRef.__PVT__esWV
        [3U];
    vlSelfRef.__Vcellinp__execState__wv[4U] = vlSelfRef.__PVT__esWV
        [4U];
    vlSelfRef.__Vcellinp__execState__wv[5U] = vlSelfRef.__PVT__esWV
        [5U];
    vlSelfRef.__Vcellinp__execState__wv[6U] = vlSelfRef.__PVT__esWV
        [6U];
    vlSelfRef.__Vcellinp__execState__wv[7U] = vlSelfRef.__PVT__esWV
        [7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__execState__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__execState__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[2U] 
        = vlSelfRef.__Vcellinp__execState__wv[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[3U] 
        = vlSelfRef.__Vcellinp__execState__wv[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[4U] 
        = vlSelfRef.__Vcellinp__execState__wv[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[5U] 
        = vlSelfRef.__Vcellinp__execState__wv[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[6U] 
        = vlSelfRef.__Vcellinp__execState__wv[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[7U] 
        = vlSelfRef.__Vcellinp__execState__wv[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__1(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__esRefRV[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__rv
        [0U];
    vlSelfRef.__PVT__esRefRV[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__rv
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__2(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__execState__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__execState__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__rv
        [1U];
    vlSelfRef.__PVT__esRV[0U] = vlSelfRef.__Vcellout__execState__rv
        [0U];
    vlSelfRef.__PVT__esRV[1U] = vlSelfRef.__Vcellout__execState__rv
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__3(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_sequent__TOP__SMT_RTL_Testbench__core__activeList__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__fflagsState__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__fflagsState__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__rv
        [1U];
    vlSelfRef.__PVT__ffsRV[0U] = vlSelfRef.__Vcellout__fflagsState__rv
        [0U];
    vlSelfRef.__PVT__ffsRV[1U] = vlSelfRef.__Vcellout__fflagsState__rv
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fflagsData[0U] 
        = vlSelfRef.__PVT__ffsRV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fflagsData[1U] 
        = vlSelfRef.__PVT__ffsRV[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__0(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*6:0*/ __Vlvbound_h2b7b2099__0;
    __Vlvbound_h2b7b2099__0 = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__0__Vfuncout;
    __Vfunc_ActiveListPtrToAge__0__Vfuncout = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__0__ptr;
    __Vfunc_ActiveListPtrToAge__0__ptr = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__0__head;
    __Vfunc_ActiveListPtrToAge__0__head = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__0__age;
    __Vfunc_ActiveListPtrToAge__0__age = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__ptr = 0;
    CData/*5:0*/ __Vfunc_ActiveListPtrToAge__1__head;
    __Vfunc_ActiveListPtrToAge__1__head = 0;
    CData/*6:0*/ __Vfunc_ActiveListPtrToAge__1__age;
    __Vfunc_ActiveListPtrToAge__1__age = 0;
    IData/*31:0*/ __Vfunc_ToAddrFromPC__3__Vfuncout;
    __Vfunc_ToAddrFromPC__3__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToAddrFromPC__3__pc;
    __Vfunc_ToAddrFromPC__3__pc = 0;
    // Body
    vlSelfRef.__PVT__nextRecoveryReg[0U] = vlSelfRef.__PVT__recoveryReg[0U];
    vlSelfRef.__PVT__nextRecoveryReg[1U] = vlSelfRef.__PVT__recoveryReg[1U];
    vlSelfRef.__PVT__nextRecoveryReg[2U] = vlSelfRef.__PVT__recoveryReg[2U];
    __Vfunc_ActiveListPtrToAge__0__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__0__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__recoveryReg[0U] 
                                                   >> 0xdU));
    __Vfunc_ActiveListPtrToAge__0__age = __Vfunc_ActiveListPtrToAge__0__ptr;
    __Vfunc_ActiveListPtrToAge__0__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__0__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__0__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__0__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__0__age)));
    vlSelfRef.__PVT__oldestAge = __Vfunc_ActiveListPtrToAge__0__Vfuncout;
    vlSelfRef.__PVT__exceptionIndex = 0U;
    vlSelfRef.__PVT__exceptionDetected = 0U;
    vlSelfRef.__PVT__refetchType = 0U;
    vlSelfRef.__PVT__startRecoveryAtCommit = 0U;
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [0U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[0U] = __Vlvbound_h2b7b2099__0;
    if ((vlSelfRef.__PVT__we[0U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [0U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [0U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [0U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [0U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [0U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [0U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [0U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [0U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 0U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [0U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [0U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [0U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [0U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [0U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [0U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [0U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [0U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [0U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [0U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [0U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [0U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [0U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [0U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [1U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[1U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[1U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [1U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [1U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [1U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [1U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [1U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [1U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [1U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [1U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 1U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [1U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [1U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [1U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [1U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [1U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [1U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [1U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [1U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [1U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [1U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [1U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [1U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [1U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [1U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [2U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[2U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[2U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [2U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [2U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [2U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [2U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [2U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [2U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [2U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [2U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 2U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [2U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [2U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [2U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [2U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [2U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [2U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [2U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [2U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [2U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [2U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [2U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [2U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [2U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [2U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [3U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[3U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[3U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [3U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [3U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [3U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [3U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [3U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [3U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [3U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [3U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 3U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [3U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [3U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [3U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [3U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [3U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [3U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [3U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [3U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [3U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [3U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [3U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [3U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [3U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [3U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [4U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[4U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[4U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [4U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [4U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [4U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [4U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [4U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [4U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [4U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [4U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 4U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [4U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [4U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [4U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [4U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [4U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [4U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [4U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [4U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [4U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [4U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [4U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [4U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [4U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [4U][1U] >> 0x16U)));
        }
    }
    __Vfunc_ActiveListPtrToAge__1__head = vlSelfRef.headPtr;
    __Vfunc_ActiveListPtrToAge__1__ptr = (0x3fU & (
                                                   vlSelfRef.__PVT__writeData
                                                   [5U][2U] 
                                                   >> 2U));
    __Vfunc_ActiveListPtrToAge__1__age = __Vfunc_ActiveListPtrToAge__1__ptr;
    __Vfunc_ActiveListPtrToAge__1__Vfuncout = (0x7fU 
                                               & (((IData)(__Vfunc_ActiveListPtrToAge__1__ptr) 
                                                   < (IData)(__Vfunc_ActiveListPtrToAge__1__head))
                                                   ? 
                                                  ((IData)(0x40U) 
                                                   + (IData)(__Vfunc_ActiveListPtrToAge__1__age))
                                                   : (IData)(__Vfunc_ActiveListPtrToAge__1__age)));
    __Vlvbound_h2b7b2099__0 = __Vfunc_ActiveListPtrToAge__1__Vfuncout;
    vlSelfRef.__PVT__writeAge[5U] = __Vlvbound_h2b7b2099__0;
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    if ((vlSelfRef.__PVT__we[5U] & (~ ((0U == (0xfU 
                                               & (vlSelfRef.__PVT__writeData
                                                  [5U][1U] 
                                                  >> 0x16U))) 
                                       | (1U == (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [5U][1U] 
                                                    >> 0x16U))))))) {
        if ((1U & ((~ (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                       >> 4U)) | (vlSelfRef.__PVT__writeAge
                                  [5U] < (IData)(vlSelfRef.__PVT__oldestAge))))) {
            vlSelfRef.__PVT__oldestAge = vlSelfRef.__PVT__writeAge
                [5U];
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfff8000fU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfffffff0U & (0x10U | (0x7ffe0U 
                                            & ((vlSelfRef.__PVT__writeData
                                                [5U][2U] 
                                                << 0xbU) 
                                               | (0x7e0U 
                                                  & (vlSelfRef.__PVT__writeData
                                                     [5U][1U] 
                                                     >> 0x15U)))))));
            vlSelfRef.__PVT__exceptionDetected = ((3U 
                                                   == 
                                                   (0xfU 
                                                    & (vlSelfRef.__PVT__writeData
                                                       [5U][1U] 
                                                       >> 0x16U))) 
                                                  | (2U 
                                                     == 
                                                     (0xfU 
                                                      & (vlSelfRef.__PVT__writeData
                                                         [5U][1U] 
                                                         >> 0x16U))));
            vlSelfRef.__PVT__exceptionIndex = 5U;
            if ((1U & (~ ((3U == (0xfU & (vlSelfRef.__PVT__writeData
                                          [5U][1U] 
                                          >> 0x16U))) 
                          | (2U == (0xfU & (vlSelfRef.__PVT__writeData
                                            [5U][1U] 
                                            >> 0x16U))))))) {
                vlSelfRef.__PVT__startRecoveryAtCommit = 1U;
            }
            vlSelfRef.__PVT__refetchType = ((3U == 
                                             (0xfU 
                                              & (vlSelfRef.__PVT__writeData
                                                 [5U][1U] 
                                                 >> 0x16U)))
                                             ? ((2U 
                                                 & vlSelfRef.__PVT__writeData
                                                 [5U][0U])
                                                 ? 3U
                                                 : 
                                                ((1U 
                                                  & vlSelfRef.__PVT__writeData
                                                  [5U][0U])
                                                  ? 2U
                                                  : 1U))
                                             : 0U);
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0x7ffffU & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | ((IData)((0xfffffffffffffULL & (
                                                   ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [5U][1U])) 
                                                    << 0x1eU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__writeData
                                                                      [5U][0U])) 
                                                      >> 2U)))) 
                    << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[1U] = 
                (((IData)((0xfffffffffffffULL & (((QData)((IData)(
                                                                  vlSelfRef.__PVT__writeData
                                                                  [5U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__writeData
                                                                    [5U][0U])) 
                                                    >> 2U)))) 
                  >> 0xdU) | ((IData)(((0xfffffffffffffULL 
                                        & (((QData)((IData)(
                                                            vlSelfRef.__PVT__writeData
                                                            [5U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSelfRef.__PVT__writeData
                                                              [5U][0U])) 
                                              >> 2U))) 
                                       >> 0x20U)) << 0x13U));
            vlSelfRef.__PVT__nextRecoveryReg[2U] = 
                (0x7fU & ((IData)(((0xfffffffffffffULL 
                                    & (((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [5U][1U])) 
                                        << 0x1eU) | 
                                       ((QData)((IData)(
                                                        vlSelfRef.__PVT__writeData
                                                        [5U][0U])) 
                                        >> 2U))) >> 0x20U)) 
                          >> 0xdU));
            vlSelfRef.__PVT__nextRecoveryReg[0U] = 
                ((0xfffffff0U & vlSelfRef.__PVT__nextRecoveryReg[0U]) 
                 | (0xfU & (vlSelfRef.__PVT__writeData
                            [5U][1U] >> 0x16U)));
        }
    }
    if (((IData)(vlSelfRef.__PVT__exceptionDetected) 
         & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)))) {
        vlSelfRef.__PVT__nextRecoveryReg[0U] = (1U 
                                                | (0xfffffff0U 
                                                   & vlSelfRef.__PVT__nextRecoveryReg[0U]));
    }
    __Vfunc_ToAddrFromPC__3__pc = (0xfffffU & ((vlSelfRef.__PVT__nextRecoveryReg[2U] 
                                                << 0xdU) 
                                               | (vlSelfRef.__PVT__nextRecoveryReg[1U] 
                                                  >> 0x13U)));
    __Vfunc_ToAddrFromPC__3__Vfuncout = ((0x80000000U 
                                          & (__Vfunc_ToAddrFromPC__3__pc 
                                             << 0xdU)) 
                                         | (0x3ffffU 
                                            & __Vfunc_ToAddrFromPC__3__pc));
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage 
        = __Vfunc_ToAddrFromPC__3__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInRwStage 
        = ((IData)(vlSelfRef.__PVT__exceptionDetected) 
           & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromRwStage 
        = vlSelfRef.__PVT__refetchType;
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__exceptionOpPtr 
        = (0x3fU & (vlSelfRef.__PVT__nextRecoveryReg[0U] 
                    >> 0xdU));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__1(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__pushedTailData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
        [0U];
    vlSelfRef.__PVT__pushedTailData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
        [1U];
    vlSelfRef.__PVT__pushTail[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
        [0U];
    vlSelfRef.__PVT__pushTail[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
        [1U];
    vlSelfRef.__PVT__pushedTailPtr[0U] = vlSelfRef.__PVT__activeListPointer__DOT__regTail;
    vlSelfRef.__PVT__pushNum = (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
                                [0U] ? 1U : 0U);
    vlSelfRef.__PVT__pushedTailPtr[1U] = (0x3fU & (
                                                   (0x40U 
                                                    <= 
                                                    ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                     + (IData)(vlSelfRef.__PVT__pushNum)))
                                                    ? 
                                                   ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                    + (IData)(vlSelfRef.__PVT__pushNum))
                                                    : 
                                                   ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                    + (IData)(vlSelfRef.__PVT__pushNum))));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__ffsWE[0U] = 0U;
        vlSelfRef.__PVT__ffsWE[1U] = 0U;
        vlSelfRef.__PVT__ffsWE[2U] = 0U;
        vlSelfRef.__PVT__esWE[0U] = 0U;
        vlSelfRef.__PVT__esWE[1U] = 0U;
        vlSelfRef.__PVT__esWE[2U] = 0U;
        vlSelfRef.__PVT__esWE[3U] = 0U;
        vlSelfRef.__PVT__esWE[4U] = 0U;
        vlSelfRef.__PVT__esWE[5U] = 0U;
        vlSelfRef.__PVT__esWE[6U] = 0U;
        vlSelfRef.__PVT__esWE[7U] = 0U;
        vlSelfRef.__PVT__ffsWA[0U] = 0U;
        vlSelfRef.__PVT__ffsWA[1U] = 1U;
        vlSelfRef.__PVT__ffsWA[2U] = 2U;
        vlSelfRef.__PVT__esWA[0U] = 0U;
        vlSelfRef.__PVT__esWA[1U] = 1U;
        vlSelfRef.__PVT__esWA[2U] = 2U;
        vlSelfRef.__PVT__esWA[3U] = 3U;
        vlSelfRef.__PVT__esWA[4U] = 4U;
        vlSelfRef.__PVT__esWA[5U] = 5U;
        vlSelfRef.__PVT__esWA[6U] = 6U;
        vlSelfRef.__PVT__esWA[7U] = 7U;
    } else {
        vlSelfRef.__Vlvbound_h46781e09__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [0U];
        vlSelfRef.__PVT__ffsWE[0U] = vlSelfRef.__Vlvbound_h46781e09__0;
        vlSelfRef.__Vlvbound_h46781e09__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [1U];
        vlSelfRef.__PVT__ffsWE[1U] = vlSelfRef.__Vlvbound_h46781e09__0;
        vlSelfRef.__Vlvbound_h6be00a99__0 = vlSelfRef.__PVT__we
            [5U];
        vlSelfRef.__PVT__ffsWE[2U] = vlSelfRef.__Vlvbound_h6be00a99__0;
        vlSelfRef.__PVT__esWE[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [0U];
        vlSelfRef.__PVT__esWE[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [1U];
        vlSelfRef.__PVT__esWE[2U] = vlSelfRef.__PVT__we
            [0U];
        vlSelfRef.__PVT__esWE[3U] = vlSelfRef.__PVT__we
            [1U];
        vlSelfRef.__PVT__esWE[4U] = vlSelfRef.__PVT__we
            [2U];
        vlSelfRef.__PVT__esWE[5U] = vlSelfRef.__PVT__we
            [3U];
        vlSelfRef.__PVT__esWE[6U] = vlSelfRef.__PVT__we
            [4U];
        vlSelfRef.__PVT__esWE[7U] = vlSelfRef.__PVT__we
            [5U];
        vlSelfRef.__Vlvbound_h0432b08f__0 = vlSelfRef.__PVT__pushedTailPtr
            [0U];
        vlSelfRef.__PVT__ffsWA[0U] = vlSelfRef.__Vlvbound_h0432b08f__0;
        vlSelfRef.__Vlvbound_h0432b08f__0 = vlSelfRef.__PVT__pushedTailPtr
            [1U];
        vlSelfRef.__PVT__ffsWA[1U] = vlSelfRef.__Vlvbound_h0432b08f__0;
        vlSelfRef.__Vlvbound_h7dcaa4bf__0 = (0x3fU 
                                             & (vlSelfRef.__PVT__writeData
                                                [5U][2U] 
                                                >> 2U));
        vlSelfRef.__PVT__ffsWA[2U] = vlSelfRef.__Vlvbound_h7dcaa4bf__0;
        vlSelfRef.__PVT__esWA[0U] = vlSelfRef.__PVT__pushedTailPtr
            [0U];
        vlSelfRef.__PVT__esWA[1U] = vlSelfRef.__PVT__pushedTailPtr
            [1U];
        vlSelfRef.__PVT__esWA[2U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [0U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[3U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [1U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[4U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [2U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[5U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [3U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[6U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [4U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[7U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [5U][2U] 
                                              >> 2U));
    }
    vlSelfRef.__PVT__pushNum = (3U & ((IData)(vlSelfRef.__PVT__pushNum) 
                                      + (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
                                         [1U] ? 1U : 0U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__detectedFlushRangeTailPtr 
        = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                    + (IData)(vlSelfRef.__PVT__pushNum)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr[0U] 
        = vlSelfRef.__PVT__pushedTailPtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr[1U] 
        = vlSelfRef.__PVT__pushedTailPtr[1U];
    vlSelfRef.__Vcellinp__activeList__wv[0U] = vlSelfRef.__PVT__pushedTailData
        [0U];
    vlSelfRef.__Vcellinp__activeList__wv[1U] = vlSelfRef.__PVT__pushedTailData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[0U] 
        = vlSelfRef.__PVT__ffsWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[1U] 
        = vlSelfRef.__PVT__ffsWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[2U] 
        = vlSelfRef.__PVT__ffsWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[0U] 
        = vlSelfRef.__PVT__esWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[1U] 
        = vlSelfRef.__PVT__esWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[2U] 
        = vlSelfRef.__PVT__esWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[3U] 
        = vlSelfRef.__PVT__esWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[4U] 
        = vlSelfRef.__PVT__esWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[5U] 
        = vlSelfRef.__PVT__esWE[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[6U] 
        = vlSelfRef.__PVT__esWE[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[7U] 
        = vlSelfRef.__PVT__esWE[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__we[0U] 
        = vlSelfRef.__PVT__pushTail[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__we[1U] 
        = vlSelfRef.__PVT__pushTail[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wa[0U] 
        = vlSelfRef.__PVT__pushedTailPtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wa[1U] 
        = vlSelfRef.__PVT__pushedTailPtr[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__activeList__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__activeList__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[0U] 
        = vlSelfRef.__PVT__ffsWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[1U] 
        = vlSelfRef.__PVT__ffsWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[2U] 
        = vlSelfRef.__PVT__ffsWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[0U] 
        = vlSelfRef.__PVT__esWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[1U] 
        = vlSelfRef.__PVT__esWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[2U] 
        = vlSelfRef.__PVT__esWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[3U] 
        = vlSelfRef.__PVT__esWA[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[4U] 
        = vlSelfRef.__PVT__esWA[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[5U] 
        = vlSelfRef.__PVT__esWA[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[6U] 
        = vlSelfRef.__PVT__esWA[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[7U] 
        = vlSelfRef.__PVT__esWA[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__2(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__activeList__rv[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__activeList__rv[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__rv
        [1U];
    vlSelfRef.__PVT__readData[0U] = vlSelfRef.__Vcellout__activeList__rv
        [0U];
    vlSelfRef.__PVT__readData[1U] = vlSelfRef.__Vcellout__activeList__rv
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData[0U] 
        = vlSelfRef.__PVT__readData[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData[1U] 
        = vlSelfRef.__PVT__readData[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__3(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__activeListPointer__DOT__nextHead 
        = vlSelfRef.__PVT__activeListPointer__DOT__regHead;
    vlSelfRef.__PVT__activeListPointer__DOT__nextTail 
        = vlSelfRef.__PVT__activeListPointer__DOT__regTail;
    vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
        = vlSelfRef.__PVT__activeListPointer__DOT__regCount;
    if ((0U < (IData)(vlSelfRef.__PVT__pushNum))) {
        vlSelfRef.__PVT__activeListPointer__DOT__nextTail 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextTail) 
                        + (IData)(vlSelfRef.__PVT__pushNum)));
        vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
            = (0x7fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextCount) 
                        + (IData)(vlSelfRef.__PVT__pushNum)));
    } else if ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum))) {
        vlSelfRef.__PVT__activeListPointer__DOT__nextTail 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextTail) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)));
        vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
            = (0x7fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextCount) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)));
    }
    if ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum))) {
        vlSelfRef.__PVT__activeListPointer__DOT__nextHead 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextHead) 
                        + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum)));
        vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
            = (0x7fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextCount) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum)));
    }
    vlSelfRef.__PVT__headPtrList[0U] = vlSelfRef.headPtr;
    vlSelfRef.__PVT__headPtrList[1U] = (0x3fU & ((0x40U 
                                                  > 
                                                  ((IData)(1U) 
                                                   + (IData)(vlSelfRef.headPtr)))
                                                  ? 
                                                 ((IData)(1U) 
                                                  + (IData)(vlSelfRef.headPtr))
                                                  : 
                                                 ((IData)(1U) 
                                                  + (IData)(vlSelfRef.headPtr))));
    vlSelfRef.__PVT__tailPtrList[0U] = (0x3fU & ((0x40U 
                                                  <= 
                                                  ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)))
                                                  ? 
                                                 ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                  - (IData)(1U))
                                                  : 
                                                 ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                  - (IData)(1U))));
    vlSelfRef.__PVT__tailPtrList[1U] = (0x3fU & ((0x40U 
                                                  <= 
                                                  (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                    - (IData)(1U)) 
                                                   - (IData)(1U)))
                                                  ? 
                                                 (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)) 
                                                  - (IData)(1U))
                                                  : 
                                                 (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)) 
                                                  - (IData)(1U))));
    if ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum))) {
        vlSelfRef.__PVT__readPtrList[0U] = vlSelfRef.__PVT__tailPtrList
            [0U];
        vlSelfRef.__PVT__readPtrList[1U] = vlSelfRef.__PVT__tailPtrList
            [1U];
    } else {
        vlSelfRef.__PVT__readPtrList[0U] = vlSelfRef.__PVT__headPtrList
            [0U];
        vlSelfRef.__PVT__readPtrList[1U] = vlSelfRef.__PVT__headPtrList
            [1U];
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__ra[0U] 
        = vlSelfRef.__PVT__readPtrList[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__ra[1U] 
        = vlSelfRef.__PVT__readPtrList[1U];
    vlSelfRef.__PVT__ffsRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__ffsRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__ra[0U] 
        = vlSelfRef.__PVT__ffsRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__ra[1U] 
        = vlSelfRef.__PVT__ffsRA[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__4(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__esRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__esRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    vlSelfRef.__PVT__headExecState[0U] = (0xfU & ((
                                                   (vlSelfRef.__PVT__recoveryReg[0U] 
                                                    >> 4U) 
                                                   & (vlSelfRef.__PVT__esRA
                                                      [0U] 
                                                      == 
                                                      (0x3fU 
                                                       & (vlSelfRef.__PVT__recoveryReg[0U] 
                                                          >> 0xdU))))
                                                   ? 
                                                  vlSelfRef.__PVT__recoveryReg[0U]
                                                   : 
                                                  (vlSelfRef.__PVT__esRV
                                                   [0U]
                                                    ? 1U
                                                    : 0U)));
    vlSelfRef.__PVT__headExecState[1U] = (0xfU & ((
                                                   (vlSelfRef.__PVT__recoveryReg[0U] 
                                                    >> 4U) 
                                                   & (vlSelfRef.__PVT__esRA
                                                      [1U] 
                                                      == 
                                                      (0x3fU 
                                                       & (vlSelfRef.__PVT__recoveryReg[0U] 
                                                          >> 0xdU))))
                                                   ? 
                                                  vlSelfRef.__PVT__recoveryReg[0U]
                                                   : 
                                                  (vlSelfRef.__PVT__esRV
                                                   [1U]
                                                    ? 1U
                                                    : 0U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState[0U] 
        = vlSelfRef.__PVT__headExecState[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState[1U] 
        = vlSelfRef.__PVT__headExecState[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__ra[0U] 
        = vlSelfRef.__PVT__esRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__ra[1U] 
        = vlSelfRef.__PVT__esRA[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__5(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___act_comb__TOP__SMT_RTL_Testbench__core__activeList__5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__esRefRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__esRefRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__esRefWE[0U] = 0U;
        vlSelfRef.__PVT__esRefWA[0U] = 0U;
        vlSelfRef.__PVT__esRefWV[0U] = 0U;
        vlSelfRef.__PVT__unnamedblk16__DOT__i = 8U;
        vlSelfRef.__PVT__esRefWE[1U] = 0U;
        vlSelfRef.__PVT__esRefWA[1U] = 1U;
        vlSelfRef.__PVT__esRefWV[1U] = 0U;
        vlSelfRef.__PVT__esRefWE[2U] = 0U;
        vlSelfRef.__PVT__esRefWA[2U] = 2U;
        vlSelfRef.__PVT__esRefWV[2U] = 0U;
        vlSelfRef.__PVT__esRefWE[3U] = 0U;
        vlSelfRef.__PVT__esRefWA[3U] = 3U;
        vlSelfRef.__PVT__esRefWV[3U] = 0U;
        vlSelfRef.__PVT__esRefWE[4U] = 0U;
        vlSelfRef.__PVT__esRefWA[4U] = 4U;
        vlSelfRef.__PVT__esRefWV[4U] = 0U;
        vlSelfRef.__PVT__esRefWE[5U] = 0U;
        vlSelfRef.__PVT__esRefWA[5U] = 5U;
        vlSelfRef.__PVT__esRefWV[5U] = 0U;
        vlSelfRef.__PVT__esRefWE[6U] = 0U;
        vlSelfRef.__PVT__esRefWA[6U] = 6U;
        vlSelfRef.__PVT__esRefWV[6U] = 0U;
        vlSelfRef.__PVT__esRefWE[7U] = 0U;
        vlSelfRef.__PVT__esRefWA[7U] = 7U;
        vlSelfRef.__PVT__esRefWV[7U] = 0U;
    } else {
        vlSelfRef.__PVT__esRefWE[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [0U];
        vlSelfRef.__PVT__esRefWA[0U] = vlSelfRef.__PVT__pushedTailPtr
            [0U];
        vlSelfRef.__PVT__esRefWV[0U] = 0U;
        vlSelfRef.__PVT__unnamedblk17__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk18__DOT__i = 6U;
        vlSelfRef.__PVT__esRefWE[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [1U];
        vlSelfRef.__PVT__esRefWA[1U] = vlSelfRef.__PVT__pushedTailPtr
            [1U];
        vlSelfRef.__PVT__esRefWV[1U] = 0U;
        vlSelfRef.__PVT__esRefWE[2U] = vlSelfRef.__PVT__we
            [0U];
        vlSelfRef.__PVT__esRefWA[2U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [0U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[2U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (0U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [0U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[3U] = vlSelfRef.__PVT__we
            [1U];
        vlSelfRef.__PVT__esRefWA[3U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [1U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[3U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (1U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [1U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[4U] = vlSelfRef.__PVT__we
            [2U];
        vlSelfRef.__PVT__esRefWA[4U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [2U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[4U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (2U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [2U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[5U] = vlSelfRef.__PVT__we
            [3U];
        vlSelfRef.__PVT__esRefWA[5U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [3U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[5U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (3U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [3U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[6U] = vlSelfRef.__PVT__we
            [4U];
        vlSelfRef.__PVT__esRefWA[6U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [4U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[6U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (4U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [4U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[7U] = vlSelfRef.__PVT__we
            [5U];
        vlSelfRef.__PVT__esRefWA[7U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [5U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[7U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (5U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [5U][1U] 
                                                    >> 0x16U)));
    }
    vlSelfRef.__PVT__nextInRecovery = vlSelfRef.__PVT__regInRecovery;
    if ((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.__PVT__nextInRecovery = 1U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__toCommitPhase) {
        vlSelfRef.__PVT__nextInRecovery = 0U;
    }
    vlSelfRef.__PVT__execStateIsDifferentFromRef = 0U;
    vlSelfRef.__PVT__unnamedblk19__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__unnamedblk19__DOT__i)) {
            vlSelfRef.__PVT__headExecStateRef[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                = vlSelfRef.__PVT__esRefRV[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)];
            if (((vlSelfRef.__PVT__esRV[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                  & (vlSelfRef.__PVT__unnamedblk19__DOT__i 
                     < (IData)(vlSelfRef.__PVT__activeListPointer__DOT__regCount))) 
                 & (~ (IData)(vlSelfRef.__PVT__nextInRecovery)))) {
                vlSelfRef.__PVT__execStateIsDifferentFromRef 
                    = ((IData)(vlSelfRef.__PVT__execStateIsDifferentFromRef) 
                       | (vlSelfRef.__PVT__headExecState
                          [(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                          != vlSelfRef.__PVT__headExecStateRef
                          [(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)]));
            }
            if ((vlSelfRef.__PVT__esRA[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                 == (0x3fU & (vlSelfRef.__PVT__recoveryReg[0U] 
                              >> 0xdU)))) {
                goto __Vlabel2;
            }
            vlSelfRef.__PVT__unnamedblk19__DOT__i = 
                ((IData)(1U) + vlSelfRef.__PVT__unnamedblk19__DOT__i);
        }
        __Vlabel2: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[0U] 
        = vlSelfRef.__PVT__esRefWV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[1U] 
        = vlSelfRef.__PVT__esRefWV[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[2U] 
        = vlSelfRef.__PVT__esRefWV[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[3U] 
        = vlSelfRef.__PVT__esRefWV[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[4U] 
        = vlSelfRef.__PVT__esRefWV[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[5U] 
        = vlSelfRef.__PVT__esRefWV[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[6U] 
        = vlSelfRef.__PVT__esRefWV[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[7U] 
        = vlSelfRef.__PVT__esRefWV[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__ra[0U] 
        = vlSelfRef.__PVT__esRefRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__ra[1U] 
        = vlSelfRef.__PVT__esRefRA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[0U] 
        = vlSelfRef.__PVT__esRefWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[1U] 
        = vlSelfRef.__PVT__esRefWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[2U] 
        = vlSelfRef.__PVT__esRefWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[3U] 
        = vlSelfRef.__PVT__esRefWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[4U] 
        = vlSelfRef.__PVT__esRefWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[5U] 
        = vlSelfRef.__PVT__esRefWE[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[6U] 
        = vlSelfRef.__PVT__esRefWE[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[7U] 
        = vlSelfRef.__PVT__esRefWE[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[0U] 
        = vlSelfRef.__PVT__esRefWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[1U] 
        = vlSelfRef.__PVT__esRefWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[2U] 
        = vlSelfRef.__PVT__esRefWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[3U] 
        = vlSelfRef.__PVT__esRefWA[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[4U] 
        = vlSelfRef.__PVT__esRefWA[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[5U] 
        = vlSelfRef.__PVT__esRefWA[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[6U] 
        = vlSelfRef.__PVT__esRefWA[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[7U] 
        = vlSelfRef.__PVT__esRefWA[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__0(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*31:0*/ __Vfunc_ToAddrFromPC__2__Vfuncout;
    __Vfunc_ToAddrFromPC__2__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToAddrFromPC__2__pc;
    __Vfunc_ToAddrFromPC__2__pc = 0;
    // Body
    if (VL_UNLIKELY((vlSelfRef.__PVT__execStateIsDifferentFromRef))) {
        VL_WRITEF_NX("Execution State value is different from the reference's value.\n",0);
    }
    if (VL_UNLIKELY(((1U & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                               | (~ ((0U < (IData)(vlSelfRef.__PVT__pushNum)) 
                                     & (0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)))))))))) {
        VL_WRITEF_NX("BiTailMultiWidthQueuePointer is pushed and popped simultaneously.\n",0);
    }
    if (VL_UNLIKELY(((1U & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                               | (0x40U >= (IData)(vlSelfRef.__PVT__activeListPointer__DOT__regCount)))))))) {
        VL_WRITEF_NX("The count of a queue exceeds its size.\n",0);
    }
    vlSelfRef.__PVT__regInRecovery = ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
                                      && (IData)(vlSelfRef.__PVT__nextInRecovery));
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__activeListPointer__DOT__regHead = 0U;
        vlSelfRef.__PVT__activeListPointer__DOT__regTail = 0U;
        vlSelfRef.__PVT__recoveryEntryNum = 0U;
        vlSelfRef.__PVT__activeListPointer__DOT__regCount = 0U;
    } else {
        vlSelfRef.__PVT__activeListPointer__DOT__regHead 
            = vlSelfRef.__PVT__activeListPointer__DOT__nextHead;
        vlSelfRef.__PVT__activeListPointer__DOT__regTail 
            = vlSelfRef.__PVT__activeListPointer__DOT__nextTail;
        if ((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__recoveryEntryNum = vlSelfRef.__PVT__nextRecoveryEntryNum;
        }
        vlSelfRef.__PVT__activeListPointer__DOT__regCount 
            = vlSelfRef.__PVT__activeListPointer__DOT__nextCount;
    }
    if (((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
         | ((IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                     & (~ (IData)(vlSelfRef.__PVT__exceptionDetected)))) 
            & (~ (IData)(vlSelfRef.__PVT__startRecoveryAtCommit))))) {
        vlSelfRef.__PVT__recoveryReg[0U] = 0U;
        vlSelfRef.__PVT__recoveryReg[1U] = 0U;
        vlSelfRef.__PVT__recoveryReg[2U] = 0U;
    } else {
        vlSelfRef.__PVT__recoveryReg[0U] = vlSelfRef.__PVT__nextRecoveryReg[0U];
        vlSelfRef.__PVT__recoveryReg[1U] = vlSelfRef.__PVT__nextRecoveryReg[1U];
        vlSelfRef.__PVT__recoveryReg[2U] = vlSelfRef.__PVT__nextRecoveryReg[2U];
    }
    vlSelfRef.headPtr = vlSelfRef.__PVT__activeListPointer__DOT__regHead;
    __Vfunc_ToAddrFromPC__2__pc = (0xfffffU & ((vlSelfRef.__PVT__recoveryReg[2U] 
                                                << 0xdU) 
                                               | (vlSelfRef.__PVT__recoveryReg[1U] 
                                                  >> 0x13U)));
    __Vfunc_ToAddrFromPC__2__Vfuncout = ((0x80000000U 
                                          & (__Vfunc_ToAddrFromPC__2__pc 
                                             << 0xdU)) 
                                         | (0x3ffffU 
                                            & __Vfunc_ToAddrFromPC__2__pc));
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromCommitStage 
        = __Vfunc_ToAddrFromPC__2__Vfuncout;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__1(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__flushRangeHeadPtr = (0x3fU & (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                   >> 0xaU));
    vlSelfRef.__PVT__flushRangeTailPtr = (0x3fU & (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                   >> 4U));
    if ((((IData)(vlSelfRef.__PVT__flushRangeHeadPtr) 
          == (IData)(vlSelfRef.__PVT__flushRangeTailPtr)) 
         & (0x40U == (IData)(vlSelfRef.__PVT__activeListPointer__DOT__regCount)))) {
        vlSelfRef.__PVT__nextRecoveryEntryNum = 0x40U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns = 1U;
    } else {
        vlSelfRef.__PVT__nextRecoveryEntryNum = (0x7fU 
                                                 & (((IData)(vlSelfRef.__PVT__flushRangeTailPtr) 
                                                     >= (IData)(vlSelfRef.__PVT__flushRangeHeadPtr))
                                                     ? 
                                                    ((IData)(vlSelfRef.__PVT__flushRangeTailPtr) 
                                                     - (IData)(vlSelfRef.__PVT__flushRangeHeadPtr))
                                                     : 
                                                    (((IData)(0x40U) 
                                                      + (IData)(vlSelfRef.__PVT__flushRangeTailPtr)) 
                                                     - (IData)(vlSelfRef.__PVT__flushRangeHeadPtr))));
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns = 0U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__2(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vlvbound_hb8910d2c__0;
    __Vlvbound_hb8910d2c__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_heffd43cc__0;
    VL_ZERO_W(72, __Vlvbound_heffd43cc__0);
    CData/*0:0*/ __Vlvbound_hb74f74a4__0;
    __Vlvbound_hb74f74a4__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_hf2c31c44__0;
    VL_ZERO_W(72, __Vlvbound_hf2c31c44__0);
    CData/*0:0*/ __Vlvbound_he294a3d5__0;
    __Vlvbound_he294a3d5__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_hc2060c75__0;
    VL_ZERO_W(72, __Vlvbound_hc2060c75__0);
    CData/*0:0*/ __Vlvbound_hdd464555__0;
    __Vlvbound_hdd464555__0 = 0;
    VlWide<3>/*71:0*/ __Vlvbound_h192a2af5__0;
    VL_ZERO_W(72, __Vlvbound_h192a2af5__0);
    // Body
    __Vlvbound_hb8910d2c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite
        [0U];
    vlSelfRef.__PVT__we[0U] = __Vlvbound_hb8910d2c__0;
    __Vlvbound_hb8910d2c__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite
        [1U];
    vlSelfRef.__PVT__we[1U] = __Vlvbound_hb8910d2c__0;
    __Vlvbound_hb74f74a4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWrite
        [0U];
    vlSelfRef.__PVT__we[2U] = __Vlvbound_hb74f74a4__0;
    __Vlvbound_he294a3d5__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite
        [0U];
    vlSelfRef.__PVT__we[3U] = __Vlvbound_he294a3d5__0;
    __Vlvbound_he294a3d5__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite
        [1U];
    vlSelfRef.__PVT__we[4U] = __Vlvbound_he294a3d5__0;
    __Vlvbound_hdd464555__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWrite
        [0U];
    vlSelfRef.__PVT__we[5U] = __Vlvbound_hdd464555__0;
    __Vlvbound_heffd43cc__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [0U][0U];
    __Vlvbound_heffd43cc__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [0U][1U];
    __Vlvbound_heffd43cc__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[0U][0U] = __Vlvbound_heffd43cc__0[0U];
    vlSelfRef.__PVT__writeData[0U][1U] = __Vlvbound_heffd43cc__0[1U];
    vlSelfRef.__PVT__writeData[0U][2U] = __Vlvbound_heffd43cc__0[2U];
    __Vlvbound_heffd43cc__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [1U][0U];
    __Vlvbound_heffd43cc__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [1U][1U];
    __Vlvbound_heffd43cc__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData
        [1U][2U];
    vlSelfRef.__PVT__writeData[1U][0U] = __Vlvbound_heffd43cc__0[0U];
    vlSelfRef.__PVT__writeData[1U][1U] = __Vlvbound_heffd43cc__0[1U];
    vlSelfRef.__PVT__writeData[1U][2U] = __Vlvbound_heffd43cc__0[2U];
    __Vlvbound_hf2c31c44__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData
        [0U][0U];
    __Vlvbound_hf2c31c44__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData
        [0U][1U];
    __Vlvbound_hf2c31c44__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[2U][0U] = __Vlvbound_hf2c31c44__0[0U];
    vlSelfRef.__PVT__writeData[2U][1U] = __Vlvbound_hf2c31c44__0[1U];
    vlSelfRef.__PVT__writeData[2U][2U] = __Vlvbound_hf2c31c44__0[2U];
    __Vlvbound_hc2060c75__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [0U][0U];
    __Vlvbound_hc2060c75__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [0U][1U];
    __Vlvbound_hc2060c75__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[3U][0U] = __Vlvbound_hc2060c75__0[0U];
    vlSelfRef.__PVT__writeData[3U][1U] = __Vlvbound_hc2060c75__0[1U];
    vlSelfRef.__PVT__writeData[3U][2U] = __Vlvbound_hc2060c75__0[2U];
    __Vlvbound_hc2060c75__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [1U][0U];
    __Vlvbound_hc2060c75__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [1U][1U];
    __Vlvbound_hc2060c75__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData
        [1U][2U];
    vlSelfRef.__PVT__writeData[4U][0U] = __Vlvbound_hc2060c75__0[0U];
    vlSelfRef.__PVT__writeData[4U][1U] = __Vlvbound_hc2060c75__0[1U];
    vlSelfRef.__PVT__writeData[4U][2U] = __Vlvbound_hc2060c75__0[2U];
    __Vlvbound_h192a2af5__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData
        [0U][0U];
    __Vlvbound_h192a2af5__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData
        [0U][1U];
    __Vlvbound_h192a2af5__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData
        [0U][2U];
    vlSelfRef.__PVT__writeData[5U][0U] = __Vlvbound_h192a2af5__0[0U];
    vlSelfRef.__PVT__writeData[5U][1U] = __Vlvbound_h192a2af5__0[1U];
    vlSelfRef.__PVT__writeData[5U][2U] = __Vlvbound_h192a2af5__0[2U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__3(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk9__DOT__i = 8U;
        vlSelfRef.__PVT__unnamedblk13__DOT__i = 3U;
    }
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk10__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk11__DOT__i = 6U;
        vlSelfRef.__PVT__unnamedblk15__DOT__i = 1U;
        vlSelfRef.__PVT__unnamedblk14__DOT__i = 2U;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___nba_comb__TOP__SMT_RTL_Testbench__core__activeList__0(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___nba_comb__TOP__SMT_RTL_Testbench__core__activeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__ffsWV[0U] = 0U;
        vlSelfRef.__PVT__ffsWV[1U] = 0U;
        vlSelfRef.__PVT__ffsWV[2U] = 0U;
        vlSelfRef.__PVT__esWV[0U] = 0U;
        vlSelfRef.__PVT__esWV[1U] = 0U;
        vlSelfRef.__PVT__esWV[2U] = 0U;
        vlSelfRef.__PVT__esWV[3U] = 0U;
        vlSelfRef.__PVT__esWV[4U] = 0U;
        vlSelfRef.__PVT__esWV[5U] = 0U;
        vlSelfRef.__PVT__esWV[6U] = 0U;
        vlSelfRef.__PVT__esWV[7U] = 0U;
    } else {
        vlSelfRef.__PVT__ffsWV[0U] = 0U;
        vlSelfRef.__PVT__ffsWV[1U] = 0U;
        vlSelfRef.__Vlvbound_hd8cbcf9b__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpFFlagsData
            [0U];
        vlSelfRef.__PVT__ffsWV[2U] = vlSelfRef.__Vlvbound_hd8cbcf9b__0;
        vlSelfRef.__PVT__esWV[0U] = 0U;
        vlSelfRef.__PVT__esWV[1U] = 0U;
        vlSelfRef.__PVT__esWV[2U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [0U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[3U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [1U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[4U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [2U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[5U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [3U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[6U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [4U][1U] 
                                             >> 0x16U)));
        vlSelfRef.__PVT__esWV[7U] = (0U != (0xfU & 
                                            (vlSelfRef.__PVT__writeData
                                             [5U][1U] 
                                             >> 0x16U)));
    }
    vlSelfRef.__Vcellinp__fflagsState__wv[0U] = vlSelfRef.__PVT__ffsWV
        [0U];
    vlSelfRef.__Vcellinp__fflagsState__wv[1U] = vlSelfRef.__PVT__ffsWV
        [1U];
    vlSelfRef.__Vcellinp__fflagsState__wv[2U] = vlSelfRef.__PVT__ffsWV
        [2U];
    vlSelfRef.__Vcellinp__execState__wv[0U] = vlSelfRef.__PVT__esWV
        [0U];
    vlSelfRef.__Vcellinp__execState__wv[1U] = vlSelfRef.__PVT__esWV
        [1U];
    vlSelfRef.__Vcellinp__execState__wv[2U] = vlSelfRef.__PVT__esWV
        [2U];
    vlSelfRef.__Vcellinp__execState__wv[3U] = vlSelfRef.__PVT__esWV
        [3U];
    vlSelfRef.__Vcellinp__execState__wv[4U] = vlSelfRef.__PVT__esWV
        [4U];
    vlSelfRef.__Vcellinp__execState__wv[5U] = vlSelfRef.__PVT__esWV
        [5U];
    vlSelfRef.__Vcellinp__execState__wv[6U] = vlSelfRef.__PVT__esWV
        [6U];
    vlSelfRef.__Vcellinp__execState__wv[7U] = vlSelfRef.__PVT__esWV
        [7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__fflagsState__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__fflagsState__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wv[2U] 
        = vlSelfRef.__Vcellinp__fflagsState__wv[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__execState__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__execState__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[2U] 
        = vlSelfRef.__Vcellinp__execState__wv[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[3U] 
        = vlSelfRef.__Vcellinp__execState__wv[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[4U] 
        = vlSelfRef.__Vcellinp__execState__wv[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[5U] 
        = vlSelfRef.__Vcellinp__execState__wv[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[6U] 
        = vlSelfRef.__Vcellinp__execState__wv[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wv[7U] 
        = vlSelfRef.__Vcellinp__execState__wv[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___nba_comb__TOP__SMT_RTL_Testbench__core__activeList__2(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___nba_comb__TOP__SMT_RTL_Testbench__core__activeList__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__pushedTailData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
        [0U];
    vlSelfRef.__PVT__pushedTailData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
        [1U];
    vlSelfRef.__PVT__pushTail[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
        [0U];
    vlSelfRef.__PVT__pushTail[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
        [1U];
    vlSelfRef.__PVT__pushedTailPtr[0U] = vlSelfRef.__PVT__activeListPointer__DOT__regTail;
    vlSelfRef.__PVT__pushNum = (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
                                [0U] ? 1U : 0U);
    vlSelfRef.__PVT__pushedTailPtr[1U] = (0x3fU & (
                                                   (0x40U 
                                                    <= 
                                                    ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                     + (IData)(vlSelfRef.__PVT__pushNum)))
                                                    ? 
                                                   ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                    + (IData)(vlSelfRef.__PVT__pushNum))
                                                    : 
                                                   ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                    + (IData)(vlSelfRef.__PVT__pushNum))));
    vlSelfRef.__PVT__pushNum = (3U & ((IData)(vlSelfRef.__PVT__pushNum) 
                                      + (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
                                         [1U] ? 1U : 0U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__detectedFlushRangeTailPtr 
        = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                    + (IData)(vlSelfRef.__PVT__pushNum)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr[0U] 
        = vlSelfRef.__PVT__pushedTailPtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr[1U] 
        = vlSelfRef.__PVT__pushedTailPtr[1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__ffsWE[0U] = 0U;
        vlSelfRef.__PVT__ffsWE[1U] = 0U;
        vlSelfRef.__PVT__ffsWE[2U] = 0U;
        vlSelfRef.__PVT__esWE[0U] = 0U;
        vlSelfRef.__PVT__esWE[1U] = 0U;
        vlSelfRef.__PVT__esWE[2U] = 0U;
        vlSelfRef.__PVT__esWE[3U] = 0U;
        vlSelfRef.__PVT__esWE[4U] = 0U;
        vlSelfRef.__PVT__esWE[5U] = 0U;
        vlSelfRef.__PVT__esWE[6U] = 0U;
        vlSelfRef.__PVT__esWE[7U] = 0U;
        vlSelfRef.__PVT__ffsWA[0U] = 0U;
        vlSelfRef.__PVT__ffsWA[1U] = 1U;
        vlSelfRef.__PVT__ffsWA[2U] = 2U;
        vlSelfRef.__PVT__esWA[0U] = 0U;
        vlSelfRef.__PVT__esWA[1U] = 1U;
        vlSelfRef.__PVT__esWA[2U] = 2U;
        vlSelfRef.__PVT__esWA[3U] = 3U;
        vlSelfRef.__PVT__esWA[4U] = 4U;
        vlSelfRef.__PVT__esWA[5U] = 5U;
        vlSelfRef.__PVT__esWA[6U] = 6U;
        vlSelfRef.__PVT__esWA[7U] = 7U;
    } else {
        vlSelfRef.__Vlvbound_h46781e09__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [0U];
        vlSelfRef.__PVT__ffsWE[0U] = vlSelfRef.__Vlvbound_h46781e09__0;
        vlSelfRef.__Vlvbound_h46781e09__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [1U];
        vlSelfRef.__PVT__ffsWE[1U] = vlSelfRef.__Vlvbound_h46781e09__0;
        vlSelfRef.__Vlvbound_h6be00a99__0 = vlSelfRef.__PVT__we
            [5U];
        vlSelfRef.__PVT__ffsWE[2U] = vlSelfRef.__Vlvbound_h6be00a99__0;
        vlSelfRef.__PVT__esWE[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [0U];
        vlSelfRef.__PVT__esWE[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [1U];
        vlSelfRef.__PVT__esWE[2U] = vlSelfRef.__PVT__we
            [0U];
        vlSelfRef.__PVT__esWE[3U] = vlSelfRef.__PVT__we
            [1U];
        vlSelfRef.__PVT__esWE[4U] = vlSelfRef.__PVT__we
            [2U];
        vlSelfRef.__PVT__esWE[5U] = vlSelfRef.__PVT__we
            [3U];
        vlSelfRef.__PVT__esWE[6U] = vlSelfRef.__PVT__we
            [4U];
        vlSelfRef.__PVT__esWE[7U] = vlSelfRef.__PVT__we
            [5U];
        vlSelfRef.__Vlvbound_h0432b08f__0 = vlSelfRef.__PVT__pushedTailPtr
            [0U];
        vlSelfRef.__PVT__ffsWA[0U] = vlSelfRef.__Vlvbound_h0432b08f__0;
        vlSelfRef.__Vlvbound_h0432b08f__0 = vlSelfRef.__PVT__pushedTailPtr
            [1U];
        vlSelfRef.__PVT__ffsWA[1U] = vlSelfRef.__Vlvbound_h0432b08f__0;
        vlSelfRef.__Vlvbound_h7dcaa4bf__0 = (0x3fU 
                                             & (vlSelfRef.__PVT__writeData
                                                [5U][2U] 
                                                >> 2U));
        vlSelfRef.__PVT__ffsWA[2U] = vlSelfRef.__Vlvbound_h7dcaa4bf__0;
        vlSelfRef.__PVT__esWA[0U] = vlSelfRef.__PVT__pushedTailPtr
            [0U];
        vlSelfRef.__PVT__esWA[1U] = vlSelfRef.__PVT__pushedTailPtr
            [1U];
        vlSelfRef.__PVT__esWA[2U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [0U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[3U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [1U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[4U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [2U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[5U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [3U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[6U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [4U][2U] 
                                              >> 2U));
        vlSelfRef.__PVT__esWA[7U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                              [5U][2U] 
                                              >> 2U));
    }
    vlSelfRef.__Vcellinp__activeList__wv[0U] = vlSelfRef.__PVT__pushedTailData
        [0U];
    vlSelfRef.__Vcellinp__activeList__wv[1U] = vlSelfRef.__PVT__pushedTailData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__we[0U] 
        = vlSelfRef.__PVT__pushTail[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__we[1U] 
        = vlSelfRef.__PVT__pushTail[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wa[0U] 
        = vlSelfRef.__PVT__pushedTailPtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wa[1U] 
        = vlSelfRef.__PVT__pushedTailPtr[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[0U] 
        = vlSelfRef.__PVT__ffsWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[1U] 
        = vlSelfRef.__PVT__ffsWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[2U] 
        = vlSelfRef.__PVT__ffsWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[0U] 
        = vlSelfRef.__PVT__esWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[1U] 
        = vlSelfRef.__PVT__esWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[2U] 
        = vlSelfRef.__PVT__esWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[3U] 
        = vlSelfRef.__PVT__esWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[4U] 
        = vlSelfRef.__PVT__esWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[5U] 
        = vlSelfRef.__PVT__esWE[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[6U] 
        = vlSelfRef.__PVT__esWE[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[7U] 
        = vlSelfRef.__PVT__esWE[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__activeList__wv[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wv[1U] 
        = vlSelfRef.__Vcellinp__activeList__wv[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[0U] 
        = vlSelfRef.__PVT__ffsWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[1U] 
        = vlSelfRef.__PVT__ffsWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[2U] 
        = vlSelfRef.__PVT__ffsWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[0U] 
        = vlSelfRef.__PVT__esWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[1U] 
        = vlSelfRef.__PVT__esWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[2U] 
        = vlSelfRef.__PVT__esWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[3U] 
        = vlSelfRef.__PVT__esWA[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[4U] 
        = vlSelfRef.__PVT__esWA[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[5U] 
        = vlSelfRef.__PVT__esWA[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[6U] 
        = vlSelfRef.__PVT__esWA[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[7U] 
        = vlSelfRef.__PVT__esWA[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_ActiveList___nba_comb__TOP__SMT_RTL_Testbench__core__activeList__4(VSMT_RTL_Testbench_ActiveList* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_ActiveList___nba_comb__TOP__SMT_RTL_Testbench__core__activeList__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__activeListPointer__DOT__nextHead 
        = vlSelfRef.__PVT__activeListPointer__DOT__regHead;
    vlSelfRef.__PVT__activeListPointer__DOT__nextTail 
        = vlSelfRef.__PVT__activeListPointer__DOT__regTail;
    vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
        = vlSelfRef.__PVT__activeListPointer__DOT__regCount;
    if ((0U < (IData)(vlSelfRef.__PVT__pushNum))) {
        vlSelfRef.__PVT__activeListPointer__DOT__nextTail 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextTail) 
                        + (IData)(vlSelfRef.__PVT__pushNum)));
        vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
            = (0x7fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextCount) 
                        + (IData)(vlSelfRef.__PVT__pushNum)));
    } else if ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum))) {
        vlSelfRef.__PVT__activeListPointer__DOT__nextTail 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextTail) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)));
        vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
            = (0x7fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextCount) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum)));
    }
    if ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum))) {
        vlSelfRef.__PVT__activeListPointer__DOT__nextHead 
            = (0x3fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextHead) 
                        + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum)));
        vlSelfRef.__PVT__activeListPointer__DOT__nextCount 
            = (0x7fU & ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__nextCount) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum)));
    }
    vlSelfRef.__PVT__headPtrList[0U] = vlSelfRef.headPtr;
    vlSelfRef.__PVT__headPtrList[1U] = (0x3fU & ((0x40U 
                                                  > 
                                                  ((IData)(1U) 
                                                   + (IData)(vlSelfRef.headPtr)))
                                                  ? 
                                                 ((IData)(1U) 
                                                  + (IData)(vlSelfRef.headPtr))
                                                  : 
                                                 ((IData)(1U) 
                                                  + (IData)(vlSelfRef.headPtr))));
    vlSelfRef.__PVT__tailPtrList[0U] = (0x3fU & ((0x40U 
                                                  <= 
                                                  ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)))
                                                  ? 
                                                 ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                  - (IData)(1U))
                                                  : 
                                                 ((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                  - (IData)(1U))));
    vlSelfRef.__PVT__tailPtrList[1U] = (0x3fU & ((0x40U 
                                                  <= 
                                                  (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                    - (IData)(1U)) 
                                                   - (IData)(1U)))
                                                  ? 
                                                 (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)) 
                                                  - (IData)(1U))
                                                  : 
                                                 (((IData)(vlSelfRef.__PVT__activeListPointer__DOT__regTail) 
                                                   - (IData)(1U)) 
                                                  - (IData)(1U))));
    if ((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum))) {
        vlSelfRef.__PVT__readPtrList[0U] = vlSelfRef.__PVT__tailPtrList
            [0U];
        vlSelfRef.__PVT__readPtrList[1U] = vlSelfRef.__PVT__tailPtrList
            [1U];
    } else {
        vlSelfRef.__PVT__readPtrList[0U] = vlSelfRef.__PVT__headPtrList
            [0U];
        vlSelfRef.__PVT__readPtrList[1U] = vlSelfRef.__PVT__headPtrList
            [1U];
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__ra[0U] 
        = vlSelfRef.__PVT__readPtrList[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__ra[1U] 
        = vlSelfRef.__PVT__readPtrList[1U];
    vlSelfRef.__PVT__ffsRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__ffsRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    vlSelfRef.__PVT__esRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__esRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    vlSelfRef.__PVT__headExecState[0U] = (0xfU & ((
                                                   (vlSelfRef.__PVT__recoveryReg[0U] 
                                                    >> 4U) 
                                                   & (vlSelfRef.__PVT__esRA
                                                      [0U] 
                                                      == 
                                                      (0x3fU 
                                                       & (vlSelfRef.__PVT__recoveryReg[0U] 
                                                          >> 0xdU))))
                                                   ? 
                                                  vlSelfRef.__PVT__recoveryReg[0U]
                                                   : 
                                                  (vlSelfRef.__PVT__esRV
                                                   [0U]
                                                    ? 1U
                                                    : 0U)));
    vlSelfRef.__PVT__headExecState[1U] = (0xfU & ((
                                                   (vlSelfRef.__PVT__recoveryReg[0U] 
                                                    >> 4U) 
                                                   & (vlSelfRef.__PVT__esRA
                                                      [1U] 
                                                      == 
                                                      (0x3fU 
                                                       & (vlSelfRef.__PVT__recoveryReg[0U] 
                                                          >> 0xdU))))
                                                   ? 
                                                  vlSelfRef.__PVT__recoveryReg[0U]
                                                   : 
                                                  (vlSelfRef.__PVT__esRV
                                                   [1U]
                                                    ? 1U
                                                    : 0U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState[0U] 
        = vlSelfRef.__PVT__headExecState[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState[1U] 
        = vlSelfRef.__PVT__headExecState[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__ra[0U] 
        = vlSelfRef.__PVT__ffsRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__ra[1U] 
        = vlSelfRef.__PVT__ffsRA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__ra[0U] 
        = vlSelfRef.__PVT__esRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__ra[1U] 
        = vlSelfRef.__PVT__esRA[1U];
    vlSelfRef.__PVT__esRefRA[0U] = vlSelfRef.__PVT__headPtrList
        [0U];
    vlSelfRef.__PVT__esRefRA[1U] = vlSelfRef.__PVT__headPtrList
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__esRefWE[0U] = 0U;
        vlSelfRef.__PVT__esRefWA[0U] = 0U;
        vlSelfRef.__PVT__esRefWV[0U] = 0U;
        vlSelfRef.__PVT__unnamedblk16__DOT__i = 8U;
        vlSelfRef.__PVT__esRefWE[1U] = 0U;
        vlSelfRef.__PVT__esRefWA[1U] = 1U;
        vlSelfRef.__PVT__esRefWV[1U] = 0U;
        vlSelfRef.__PVT__esRefWE[2U] = 0U;
        vlSelfRef.__PVT__esRefWA[2U] = 2U;
        vlSelfRef.__PVT__esRefWV[2U] = 0U;
        vlSelfRef.__PVT__esRefWE[3U] = 0U;
        vlSelfRef.__PVT__esRefWA[3U] = 3U;
        vlSelfRef.__PVT__esRefWV[3U] = 0U;
        vlSelfRef.__PVT__esRefWE[4U] = 0U;
        vlSelfRef.__PVT__esRefWA[4U] = 4U;
        vlSelfRef.__PVT__esRefWV[4U] = 0U;
        vlSelfRef.__PVT__esRefWE[5U] = 0U;
        vlSelfRef.__PVT__esRefWA[5U] = 5U;
        vlSelfRef.__PVT__esRefWV[5U] = 0U;
        vlSelfRef.__PVT__esRefWE[6U] = 0U;
        vlSelfRef.__PVT__esRefWA[6U] = 6U;
        vlSelfRef.__PVT__esRefWV[6U] = 0U;
        vlSelfRef.__PVT__esRefWE[7U] = 0U;
        vlSelfRef.__PVT__esRefWA[7U] = 7U;
        vlSelfRef.__PVT__esRefWV[7U] = 0U;
    } else {
        vlSelfRef.__PVT__esRefWE[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [0U];
        vlSelfRef.__PVT__esRefWA[0U] = vlSelfRef.__PVT__pushedTailPtr
            [0U];
        vlSelfRef.__PVT__esRefWV[0U] = 0U;
        vlSelfRef.__PVT__unnamedblk17__DOT__i = 2U;
        vlSelfRef.__PVT__unnamedblk18__DOT__i = 6U;
        vlSelfRef.__PVT__esRefWE[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail
            [1U];
        vlSelfRef.__PVT__esRefWA[1U] = vlSelfRef.__PVT__pushedTailPtr
            [1U];
        vlSelfRef.__PVT__esRefWV[1U] = 0U;
        vlSelfRef.__PVT__esRefWE[2U] = vlSelfRef.__PVT__we
            [0U];
        vlSelfRef.__PVT__esRefWA[2U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [0U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[2U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (0U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [0U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[3U] = vlSelfRef.__PVT__we
            [1U];
        vlSelfRef.__PVT__esRefWA[3U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [1U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[3U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (1U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [1U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[4U] = vlSelfRef.__PVT__we
            [2U];
        vlSelfRef.__PVT__esRefWA[4U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [2U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[4U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (2U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [2U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[5U] = vlSelfRef.__PVT__we
            [3U];
        vlSelfRef.__PVT__esRefWA[5U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [3U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[5U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (3U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [3U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[6U] = vlSelfRef.__PVT__we
            [4U];
        vlSelfRef.__PVT__esRefWA[6U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [4U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[6U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (4U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [4U][1U] 
                                                    >> 0x16U)));
        vlSelfRef.__PVT__esRefWE[7U] = vlSelfRef.__PVT__we
            [5U];
        vlSelfRef.__PVT__esRefWA[7U] = (0x3fU & (vlSelfRef.__PVT__writeData
                                                 [5U][2U] 
                                                 >> 2U));
        vlSelfRef.__PVT__esRefWV[7U] = ((((IData)(vlSelfRef.__PVT__exceptionDetected) 
                                          & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery))) 
                                         & (5U == (IData)(vlSelfRef.__PVT__exceptionIndex)))
                                         ? 1U : (0xfU 
                                                 & (vlSelfRef.__PVT__writeData
                                                    [5U][1U] 
                                                    >> 0x16U)));
    }
    vlSelfRef.__PVT__nextInRecovery = vlSelfRef.__PVT__regInRecovery;
    if ((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.__PVT__nextInRecovery = 1U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__toCommitPhase) {
        vlSelfRef.__PVT__nextInRecovery = 0U;
    }
    vlSelfRef.__PVT__execStateIsDifferentFromRef = 0U;
    vlSelfRef.__PVT__unnamedblk19__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__unnamedblk19__DOT__i)) {
            vlSelfRef.__PVT__headExecStateRef[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                = vlSelfRef.__PVT__esRefRV[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)];
            if (((vlSelfRef.__PVT__esRV[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                  & (vlSelfRef.__PVT__unnamedblk19__DOT__i 
                     < (IData)(vlSelfRef.__PVT__activeListPointer__DOT__regCount))) 
                 & (~ (IData)(vlSelfRef.__PVT__nextInRecovery)))) {
                vlSelfRef.__PVT__execStateIsDifferentFromRef 
                    = ((IData)(vlSelfRef.__PVT__execStateIsDifferentFromRef) 
                       | (vlSelfRef.__PVT__headExecState
                          [(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                          != vlSelfRef.__PVT__headExecStateRef
                          [(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)]));
            }
            if ((vlSelfRef.__PVT__esRA[(1U & vlSelfRef.__PVT__unnamedblk19__DOT__i)] 
                 == (0x3fU & (vlSelfRef.__PVT__recoveryReg[0U] 
                              >> 0xdU)))) {
                goto __Vlabel3;
            }
            vlSelfRef.__PVT__unnamedblk19__DOT__i = 
                ((IData)(1U) + vlSelfRef.__PVT__unnamedblk19__DOT__i);
        }
        __Vlabel3: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[0U] 
        = vlSelfRef.__PVT__esRefWV[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[1U] 
        = vlSelfRef.__PVT__esRefWV[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[2U] 
        = vlSelfRef.__PVT__esRefWV[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[3U] 
        = vlSelfRef.__PVT__esRefWV[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[4U] 
        = vlSelfRef.__PVT__esRefWV[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[5U] 
        = vlSelfRef.__PVT__esRefWV[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[6U] 
        = vlSelfRef.__PVT__esRefWV[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wv[7U] 
        = vlSelfRef.__PVT__esRefWV[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__ra[0U] 
        = vlSelfRef.__PVT__esRefRA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__ra[1U] 
        = vlSelfRef.__PVT__esRefRA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[0U] 
        = vlSelfRef.__PVT__esRefWE[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[1U] 
        = vlSelfRef.__PVT__esRefWE[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[2U] 
        = vlSelfRef.__PVT__esRefWE[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[3U] 
        = vlSelfRef.__PVT__esRefWE[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[4U] 
        = vlSelfRef.__PVT__esRefWE[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[5U] 
        = vlSelfRef.__PVT__esRefWE[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[6U] 
        = vlSelfRef.__PVT__esRefWE[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__we[7U] 
        = vlSelfRef.__PVT__esRefWE[7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[0U] 
        = vlSelfRef.__PVT__esRefWA[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[1U] 
        = vlSelfRef.__PVT__esRefWA[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[2U] 
        = vlSelfRef.__PVT__esRefWA[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[3U] 
        = vlSelfRef.__PVT__esRefWA[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[4U] 
        = vlSelfRef.__PVT__esRefWA[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[5U] 
        = vlSelfRef.__PVT__esRefWA[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[6U] 
        = vlSelfRef.__PVT__esRefWA[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execStateRef.__PVT__wa[7U] 
        = vlSelfRef.__PVT__esRefWA[7U];
}
