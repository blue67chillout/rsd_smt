// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_StoreQueue.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_StoreQueue___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__0(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__releasedStoreQueuePtr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreQueuePtr;
    vlSelfRef.__PVT__sqReadPtr[1U] = vlSelfRef.__PVT__releasedStoreQueuePtr;
    vlSelfRef.__PVT__headAddrEntry = vlSelfRef.__PVT__storeQueue
        [vlSelfRef.__PVT__releasedStoreQueuePtr];
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreLSQ_BlockAddr 
        = (0xfffffU & (vlSelfRef.__PVT__headAddrEntry 
                       >> 5U));
    vlSelfRef.__PVT__sqReadPtr[0U] = vlSelfRef.__PVT__pickedPtr
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__ra[0U] 
        = vlSelfRef.__PVT__sqReadPtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__ra[1U] 
        = vlSelfRef.__PVT__sqReadPtr[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_StoreQueue___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__1(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___act_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__storeQueueData__rv[0U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__rv
        [0U];
    vlSelfRef.__Vcellout__storeQueueData__rv[1U] = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__rv
        [1U];
    vlSelfRef.__PVT__sqReadData[0U] = vlSelfRef.__Vcellout__storeQueueData__rv
        [0U];
    vlSelfRef.__PVT__sqReadData[1U] = vlSelfRef.__Vcellout__storeQueueData__rv
        [1U];
    vlSelfRef.__PVT__headDataEntry = vlSelfRef.__PVT__sqReadData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreData 
        = (IData)((vlSelfRef.__PVT__headDataEntry >> 5U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreCondEnabled 
        = (1U & (IData)((vlSelfRef.__PVT__headDataEntry 
                         >> 0x25U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreWordWE 
        = (1U & (IData)((vlSelfRef.__PVT__headDataEntry 
                         >> 4U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreByteWE 
        = (0xfU & (IData)(vlSelfRef.__PVT__headDataEntry));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_StoreQueue___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__0(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr[0U] 
        = vlSelfRef.__PVT__storeQueuePointer__DOT__regTail;
    vlSelfRef.__PVT__pushCount = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr[1U] 
        = (0xfU & ((0x10U > ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail) 
                             + (IData)(vlSelfRef.__PVT__pushCount)))
                    ? ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail) 
                       + (IData)(vlSelfRef.__PVT__pushCount))
                    : ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail) 
                       + (IData)(vlSelfRef.__PVT__pushCount))));
    vlSelfRef.__PVT__pushCount = (3U & ((IData)(vlSelfRef.__PVT__pushCount) 
                                        + vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue
                                        [1U]));
    vlSelfRef.__PVT__push = (0U < (IData)(vlSelfRef.__PVT__pushCount));
    vlSelfRef.__PVT__storeQueuePointer__DOT__nextCount 
        = vlSelfRef.__PVT__storeQueuePointer__DOT__regCount;
    vlSelfRef.__PVT__storeQueuePointer__DOT__roundedSetTailPtr 
        = (0xfU & ((2U == (7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U]))
                    ? ((IData)(1U) + (0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                              >> 5U)))
                    : (0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                               >> 5U))));
    if (vlSelfRef.__PVT__push) {
        vlSelfRef.__PVT__storeQueuePointer__DOT__nextCount 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__nextCount) 
                        + (IData)(vlSelfRef.__PVT__pushCount)));
    } else if ((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                             >> 0x15U)))) {
        vlSelfRef.__PVT__storeQueuePointer__DOT__nextCount 
            = (0x1fU & (((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead) 
                         <= (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__roundedSetTailPtr))
                         ? ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__roundedSetTailPtr) 
                            - (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead))
                         : (((IData)(0x10U) + (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__roundedSetTailPtr)) 
                            - (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead))));
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHead) {
        vlSelfRef.__PVT__storeQueuePointer__DOT__nextCount 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__nextCount) 
                        - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHeadEntryNum)));
    }
    vlSelfRef.__PVT__storeQueuePointer__DOT__nextTail 
        = vlSelfRef.__PVT__storeQueuePointer__DOT__regTail;
    if (vlSelfRef.__PVT__push) {
        vlSelfRef.__PVT__storeQueuePointer__DOT__nextTail 
            = (0xfU & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__nextTail) 
                       + (IData)(vlSelfRef.__PVT__pushCount)));
    } else if ((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                             >> 0x15U)))) {
        vlSelfRef.__PVT__storeQueuePointer__DOT__nextTail 
            = vlSelfRef.__PVT__storeQueuePointer__DOT__roundedSetTailPtr;
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_StoreQueue___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__1(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ __Vlvbound_h074f4c6e__0;
    __Vlvbound_h074f4c6e__0 = 0;
    QData/*37:0*/ __Vlvbound_hf3377e8b__0;
    __Vlvbound_hf3377e8b__0 = 0;
    CData/*0:0*/ __Vlvbound_hd2f5e023__0;
    __Vlvbound_hd2f5e023__0 = 0;
    CData/*3:0*/ __Vlvbound_h1ff68868__0;
    __Vlvbound_h1ff68868__0 = 0;
    CData/*0:0*/ __Vlvbound_h5cb28530__0;
    __Vlvbound_h5cb28530__0 = 0;
    CData/*0:0*/ __Vlvbound_h27eb5485__0;
    __Vlvbound_h27eb5485__0 = 0;
    IData/*31:0*/ __Vlvbound_h1e97cff3__0;
    __Vlvbound_h1e97cff3__0 = 0;
    CData/*0:0*/ __Vlvbound_hb5336e34__0;
    __Vlvbound_hb5336e34__0 = 0;
    CData/*0:0*/ __Vfunc_LSQ_ToBlockWordEnable__5__Vfuncout;
    __Vfunc_LSQ_ToBlockWordEnable__5__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_LSQ_ToBlockWordEnable__5__addr;
    __Vfunc_LSQ_ToBlockWordEnable__5__addr = 0;
    CData/*3:0*/ __Vfunc_LSQ_ToWordByteEnable__7__Vfuncout;
    __Vfunc_LSQ_ToWordByteEnable__7__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_LSQ_ToWordByteEnable__7__addr;
    __Vfunc_LSQ_ToWordByteEnable__7__addr = 0;
    CData/*2:0*/ __Vfunc_LSQ_ToWordByteEnable__7__mode;
    __Vfunc_LSQ_ToWordByteEnable__7__mode = 0;
    CData/*3:0*/ __Vfunc_LSQ_ToWordByteEnable__7__byteEnable;
    __Vfunc_LSQ_ToWordByteEnable__7__byteEnable = 0;
    // Body
    __Vlvbound_h074f4c6e__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByLoad
        [0U];
    vlSelfRef.__PVT__executedStoreQueuePtrByLoad[0U] 
        = __Vlvbound_h074f4c6e__0;
    __Vlvbound_hf3377e8b__0 = vlSelfRef.__PVT__sqReadData
        [0U];
    vlSelfRef.__PVT__forwardedDataEntry[0U] = __Vlvbound_hf3377e8b__0;
    __Vfunc_LSQ_ToBlockWordEnable__5__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
        [0U];
    __Vfunc_LSQ_ToBlockWordEnable__5__Vfuncout = (1U 
                                                  & VL_SHIFTL_III(1,1,32, (IData)(1U), 
                                                                  ([&]() {
                    vlSelfRef.__Vfunc_LSQ_SelectBits__6__width = 0U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__6__offset = 2U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__6__data 
                        = __Vfunc_LSQ_ToBlockWordEnable__5__addr;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__6__ret = 0U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i = 0U;
                    while (VL_LTS_III(32, vlSelfRef.__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i, vlSelfRef.__Vfunc_LSQ_SelectBits__6__width)) {
                        vlSelfRef.__Vfunc_LSQ_SelectBits__6__ret 
                            = (((~ ((IData)(1U) << 
                                    (0x1fU & vlSelfRef.__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i))) 
                                & vlSelfRef.__Vfunc_LSQ_SelectBits__6__ret) 
                               | (0xffffffffULL & (
                                                   (1U 
                                                    & (vlSelfRef.__Vfunc_LSQ_SelectBits__6__data 
                                                       >> 
                                                       (0x1fU 
                                                        & (vlSelfRef.__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i 
                                                           + vlSelfRef.__Vfunc_LSQ_SelectBits__6__offset)))) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i))));
                        vlSelfRef.__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i 
                            = ((IData)(1U) + vlSelfRef.__Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i);
                    }
                    vlSelfRef.__Vfunc_LSQ_SelectBits__6__Vfuncout 
                        = vlSelfRef.__Vfunc_LSQ_SelectBits__6__ret;
                }(), vlSelfRef.__Vfunc_LSQ_SelectBits__6__Vfuncout)));
    __Vlvbound_hd2f5e023__0 = __Vfunc_LSQ_ToBlockWordEnable__5__Vfuncout;
    vlSelfRef.__PVT__executedLoadWordRE[0U] = __Vlvbound_hd2f5e023__0;
    __Vfunc_LSQ_ToWordByteEnable__7__mode = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemAccessMode
        [0U];
    __Vfunc_LSQ_ToWordByteEnable__7__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
        [0U];
    __Vfunc_LSQ_ToWordByteEnable__7__byteEnable = (
                                                   (0U 
                                                    == 
                                                    (3U 
                                                     & (IData)(__Vfunc_LSQ_ToWordByteEnable__7__mode)))
                                                    ? 1U
                                                    : 
                                                   ((1U 
                                                     == 
                                                     (3U 
                                                      & (IData)(__Vfunc_LSQ_ToWordByteEnable__7__mode)))
                                                     ? 3U
                                                     : 0xfU));
    __Vfunc_LSQ_ToWordByteEnable__7__Vfuncout = (0xfU 
                                                 & ((IData)(__Vfunc_LSQ_ToWordByteEnable__7__byteEnable) 
                                                    << 
                                                    (3U 
                                                     & __Vfunc_LSQ_ToWordByteEnable__7__addr)));
    __Vlvbound_h1ff68868__0 = __Vfunc_LSQ_ToWordByteEnable__7__Vfuncout;
    vlSelfRef.__PVT__executedLoadByteRE[0U] = __Vlvbound_h1ff68868__0;
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [0U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [0U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [0U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [0U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xfffeU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | (IData)(__Vlvbound_h5cb28530__0));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [1U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [1U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [1U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [1U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xfffdU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 1U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [2U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [2U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [2U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [2U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xfffbU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 2U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [3U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [3U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [3U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [3U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xfff7U & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 3U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [4U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [4U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [4U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [4U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xffefU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 4U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [5U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [5U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [5U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [5U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xffdfU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 5U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [6U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [6U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [6U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [6U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xffbfU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 6U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [7U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [7U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [7U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [7U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xff7fU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 7U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [8U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [8U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [8U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [8U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xfeffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 8U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [9U] >> 0x19U) & 
                                  (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                   [0U])) & ((0xfffffU 
                                              & (vlSelfRef.__PVT__storeQueue
                                                 [9U] 
                                                 >> 5U)) 
                                             == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [9U] >> 4U) & vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [9U] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xfdffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 9U));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [0xaU] >> 0x19U) 
                                  & (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                     [0U])) & ((0xfffffU 
                                                & (vlSelfRef.__PVT__storeQueue
                                                   [0xaU] 
                                                   >> 5U)) 
                                               == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [0xaU] >> 4U) & 
                                   vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [0xaU] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xfbffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 0xaU));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [0xbU] >> 0x19U) 
                                  & (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                     [0U])) & ((0xfffffU 
                                                & (vlSelfRef.__PVT__storeQueue
                                                   [0xbU] 
                                                   >> 5U)) 
                                               == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [0xbU] >> 4U) & 
                                   vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [0xbU] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xf7ffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 0xbU));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [0xcU] >> 0x19U) 
                                  & (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                     [0U])) & ((0xfffffU 
                                                & (vlSelfRef.__PVT__storeQueue
                                                   [0xcU] 
                                                   >> 5U)) 
                                               == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [0xcU] >> 4U) & 
                                   vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [0xcU] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xefffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 0xcU));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [0xdU] >> 0x19U) 
                                  & (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                     [0U])) & ((0xfffffU 
                                                & (vlSelfRef.__PVT__storeQueue
                                                   [0xdU] 
                                                   >> 5U)) 
                                               == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [0xdU] >> 4U) & 
                                   vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [0xdU] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xdfffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 0xdU));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [0xeU] >> 0x19U) 
                                  & (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                     [0U])) & ((0xfffffU 
                                                & (vlSelfRef.__PVT__storeQueue
                                                   [0xeU] 
                                                   >> 5U)) 
                                               == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [0xeU] >> 4U) & 
                                   vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [0xeU] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0xbfffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 0xeU));
    __Vlvbound_h5cb28530__0 = (((((vlSelfRef.__PVT__storeQueue
                                   [0xfU] >> 0x19U) 
                                  & (2U != vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                     [0U])) & ((0xfffffU 
                                                & (vlSelfRef.__PVT__storeQueue
                                                   [0xfU] 
                                                   >> 5U)) 
                                               == ([&]() {
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                [0U];
                            vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout 
                                = (0xfffffU & (vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__addr 
                                               >> 2U));
                        }(), vlSelfRef.__Vfunc_LSQ_ToBlockAddr__8__Vfuncout))) 
                                & ((vlSelfRef.__PVT__storeQueue
                                    [0xfU] >> 4U) & 
                                   vlSelfRef.__PVT__executedLoadWordRE
                                   [0U])) & (0U != 
                                             (vlSelfRef.__PVT__storeQueue
                                              [0xfU] 
                                              & vlSelfRef.__PVT__executedLoadByteRE
                                              [0U])));
    vlSelfRef.__PVT__addrMatch[0U] = ((0x7fffU & vlSelfRef.__PVT__addrMatch
                                       [0U]) | ((IData)(__Vlvbound_h5cb28530__0) 
                                                << 0xfU));
    __Vlvbound_h27eb5485__0 = (1U & (((~ (vlSelfRef.__PVT__storeQueue
                                          [vlSelfRef.__PVT__pickedPtr
                                          [0U]] >> 0x1aU)) 
                                      | ((~ (IData)(
                                                    (vlSelfRef.__PVT__forwardedDataEntry
                                                     [0U] 
                                                     >> 4U))) 
                                         & vlSelfRef.__PVT__executedLoadWordRE
                                         [0U])) | (0U 
                                                   != 
                                                   ((~ (IData)(
                                                               vlSelfRef.__PVT__forwardedDataEntry
                                                               [0U])) 
                                                    & vlSelfRef.__PVT__executedLoadByteRE
                                                    [0U]))));
    vlSelfRef.__PVT__forwardMiss[0U] = __Vlvbound_h27eb5485__0;
    __Vlvbound_h1e97cff3__0 = (IData)((vlSelfRef.__PVT__forwardedDataEntry
                                       [0U] >> 5U));
    vlSelfRef.__PVT__forwardedLoadData[0U] = __Vlvbound_h1e97cff3__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__forwardedLoadData[0U] 
        = vlSelfRef.__PVT__forwardedLoadData[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__forwardMiss[0U] 
        = vlSelfRef.__PVT__forwardMiss[0U];
    vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
        = (0x7fffffffU & ((vlSelfRef.__PVT__addrMatch
                           [0U] << 0x10U) | vlSelfRef.__PVT__addrMatch
                          [0U]));
    vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
        = ((3U == (3U & (vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                         [0U] >> 2U))) ? (0x7ffffU 
                                          & (vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                             >> 0xcU))
            : ((2U == (3U & (vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                             [0U] >> 2U))) ? (0x7ffffU 
                                              & (vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                                 >> 8U))
                : ((1U == (3U & (vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                                 [0U] >> 2U))) ? (0x7ffffU 
                                                  & (vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                                     >> 4U))
                    : (0x7ffffU & vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp))));
    vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
        = ((3U == (3U & vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                   [0U])) ? (0xffffU & (vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                        >> 3U)) : (
                                                   (2U 
                                                    == 
                                                    (3U 
                                                     & vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                                                     [0U]))
                                                    ? 
                                                   (0xffffU 
                                                    & (vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                                       >> 2U))
                                                    : 
                                                   ((1U 
                                                     == 
                                                     (3U 
                                                      & vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                                                      [0U]))
                                                     ? 
                                                    (0xffffU 
                                                     & (vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp 
                                                        >> 1U))
                                                     : 
                                                    (0xffffU 
                                                     & vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp))));
    vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq 
        = (0xffffU & vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp);
    vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 0U;
    vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0U;
    if ((1U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((2U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 1U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((4U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 2U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((8U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 3U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x10U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 4U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x20U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 5U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x40U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 6U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x80U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 7U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x100U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 8U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x200U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 9U;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x400U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xaU;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x800U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xbU;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x1000U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xcU;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x2000U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xdU;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x4000U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xeU;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    if ((0x8000U & (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq))) {
        vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = 0xfU;
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 1U;
    }
    vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr 
        = (0xfU & ((0x10U > ((IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant) 
                             + vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                             [0U])) ? ((IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant) 
                                       + vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                                       [0U]) : ((IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant) 
                                                + vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                                                [0U])));
    if ((vlSelfRef.__PVT__executedStoreQueuePtrByLoad
         [0U] >= (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead))) {
        if (((((IData)(0x10U) + (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead)) 
              - vlSelfRef.__PVT__executedStoreQueuePtrByLoad
              [0U]) > (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant))) {
            vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 0U;
        }
    } else if (((0xfU & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead) 
                         - vlSelfRef.__PVT__executedStoreQueuePtrByLoad
                         [0U])) > (IData)(vlSelfRef.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant))) {
        vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = 0U;
    }
    vlSelfRef.__PVT__pickedPtr[0U] = vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr;
    vlSelfRef.__PVT__picked[0U] = vlSelfRef.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked;
    __Vlvbound_hb5336e34__0 = (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeLoad
                               [0U] & vlSelfRef.__PVT__picked
                               [0U]);
    vlSelfRef.__PVT__storeLoadForwarded[0U] = __Vlvbound_hb5336e34__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__storeLoadForwarded[0U] 
        = vlSelfRef.__PVT__storeLoadForwarded[0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_StoreQueue___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__2(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___act_comb__TOP__SMT_RTL_Testbench__core__storeQueue__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vlvbound_hd8cb9fa9__0;
    __Vlvbound_hd8cb9fa9__0 = 0;
    IData/*19:0*/ __Vlvbound_h8e17ccaa__0;
    __Vlvbound_h8e17ccaa__0 = 0;
    CData/*0:0*/ __Vlvbound_h138be52e__0;
    __Vlvbound_h138be52e__0 = 0;
    CData/*0:0*/ __Vlvbound_h77aa90fb__0;
    __Vlvbound_h77aa90fb__0 = 0;
    CData/*3:0*/ __Vlvbound_hb1177faf__0;
    __Vlvbound_hb1177faf__0 = 0;
    CData/*0:0*/ __Vlvbound_h7b862a5f__0;
    __Vlvbound_h7b862a5f__0 = 0;
    CData/*3:0*/ __Vlvbound_h9d8b1124__0;
    __Vlvbound_h9d8b1124__0 = 0;
    IData/*31:0*/ __Vlvbound_h7a0758e3__0;
    __Vlvbound_h7a0758e3__0 = 0;
    CData/*0:0*/ __Vlvbound_h54f85ec8__0;
    __Vlvbound_h54f85ec8__0 = 0;
    IData/*31:0*/ __Vlvbound_h13192e71__0;
    __Vlvbound_h13192e71__0 = 0;
    CData/*0:0*/ __Vlvbound_hf6ea1d33__0;
    __Vlvbound_hf6ea1d33__0 = 0;
    CData/*0:0*/ __Vlvbound_hf6ec10ee__0;
    __Vlvbound_hf6ec10ee__0 = 0;
    CData/*3:0*/ __Vlvbound_h0c9efe36__0;
    __Vlvbound_h0c9efe36__0 = 0;
    IData/*19:0*/ __Vfunc_LSQ_ToBlockAddr__0__Vfuncout;
    __Vfunc_LSQ_ToBlockAddr__0__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_LSQ_ToBlockAddr__0__addr;
    __Vfunc_LSQ_ToBlockAddr__0__addr = 0;
    CData/*0:0*/ __Vfunc_LSQ_ToBlockWordEnable__1__Vfuncout;
    __Vfunc_LSQ_ToBlockWordEnable__1__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_LSQ_ToBlockWordEnable__1__addr;
    __Vfunc_LSQ_ToBlockWordEnable__1__addr = 0;
    CData/*3:0*/ __Vfunc_LSQ_ToWordByteEnable__3__Vfuncout;
    __Vfunc_LSQ_ToWordByteEnable__3__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_LSQ_ToWordByteEnable__3__addr;
    __Vfunc_LSQ_ToWordByteEnable__3__addr = 0;
    CData/*2:0*/ __Vfunc_LSQ_ToWordByteEnable__3__mode;
    __Vfunc_LSQ_ToWordByteEnable__3__mode = 0;
    CData/*3:0*/ __Vfunc_LSQ_ToWordByteEnable__3__byteEnable;
    __Vfunc_LSQ_ToWordByteEnable__3__byteEnable = 0;
    IData/*31:0*/ __Vtask_GenerateStoreData__4__dataOut;
    __Vtask_GenerateStoreData__4__dataOut = 0;
    IData/*31:0*/ __Vtask_GenerateStoreData__4__dataIn;
    __Vtask_GenerateStoreData__4__dataIn = 0;
    IData/*21:0*/ __Vtask_GenerateStoreData__4__addr;
    __Vtask_GenerateStoreData__4__addr = 0;
    // Body
    __Vlvbound_h77aa90fb__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreRegValid
        [0U];
    vlSelfRef.__PVT__executedStoreRegValid[0U] = __Vlvbound_h77aa90fb__0;
    __Vlvbound_hb1177faf__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByStore
        [0U];
    vlSelfRef.__PVT__executedStoreQueuePtrByStore[0U] 
        = __Vlvbound_hb1177faf__0;
    __Vlvbound_h138be52e__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreCondEnabled
        [0U];
    vlSelfRef.__PVT__executedStoreCondEnabled[0U] = __Vlvbound_h138be52e__0;
    __Vlvbound_hd8cb9fa9__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeStore
        [0U];
    vlSelfRef.__PVT__executeStore[0U] = __Vlvbound_hd8cb9fa9__0;
    __Vfunc_LSQ_ToBlockAddr__0__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
        [0U];
    __Vfunc_LSQ_ToBlockAddr__0__Vfuncout = (0xfffffU 
                                            & (__Vfunc_LSQ_ToBlockAddr__0__addr 
                                               >> 2U));
    __Vlvbound_h8e17ccaa__0 = __Vfunc_LSQ_ToBlockAddr__0__Vfuncout;
    vlSelfRef.__PVT__executedStoreAddr[0U] = __Vlvbound_h8e17ccaa__0;
    __Vfunc_LSQ_ToWordByteEnable__3__mode = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreMemAccessMode
        [0U];
    __Vfunc_LSQ_ToWordByteEnable__3__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
        [0U];
    __Vfunc_LSQ_ToWordByteEnable__3__byteEnable = (
                                                   (0U 
                                                    == 
                                                    (3U 
                                                     & (IData)(__Vfunc_LSQ_ToWordByteEnable__3__mode)))
                                                    ? 1U
                                                    : 
                                                   ((1U 
                                                     == 
                                                     (3U 
                                                      & (IData)(__Vfunc_LSQ_ToWordByteEnable__3__mode)))
                                                     ? 3U
                                                     : 0xfU));
    __Vfunc_LSQ_ToWordByteEnable__3__Vfuncout = (0xfU 
                                                 & ((IData)(__Vfunc_LSQ_ToWordByteEnable__3__byteEnable) 
                                                    << 
                                                    (3U 
                                                     & __Vfunc_LSQ_ToWordByteEnable__3__addr)));
    __Vlvbound_h9d8b1124__0 = __Vfunc_LSQ_ToWordByteEnable__3__Vfuncout;
    vlSelfRef.__PVT__executedStoreByteWE[0U] = __Vlvbound_h9d8b1124__0;
    __Vfunc_LSQ_ToBlockWordEnable__1__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
        [0U];
    __Vfunc_LSQ_ToBlockWordEnable__1__Vfuncout = (1U 
                                                  & VL_SHIFTL_III(1,1,32, (IData)(1U), 
                                                                  ([&]() {
                    vlSelfRef.__Vfunc_LSQ_SelectBits__2__width = 0U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__2__offset = 2U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__2__data 
                        = __Vfunc_LSQ_ToBlockWordEnable__1__addr;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__2__ret = 0U;
                    vlSelfRef.__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i = 0U;
                    while (VL_LTS_III(32, vlSelfRef.__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i, vlSelfRef.__Vfunc_LSQ_SelectBits__2__width)) {
                        vlSelfRef.__Vfunc_LSQ_SelectBits__2__ret 
                            = (((~ ((IData)(1U) << 
                                    (0x1fU & vlSelfRef.__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i))) 
                                & vlSelfRef.__Vfunc_LSQ_SelectBits__2__ret) 
                               | (0xffffffffULL & (
                                                   (1U 
                                                    & (vlSelfRef.__Vfunc_LSQ_SelectBits__2__data 
                                                       >> 
                                                       (0x1fU 
                                                        & (vlSelfRef.__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i 
                                                           + vlSelfRef.__Vfunc_LSQ_SelectBits__2__offset)))) 
                                                   << 
                                                   (0x1fU 
                                                    & vlSelfRef.__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i))));
                        vlSelfRef.__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i 
                            = ((IData)(1U) + vlSelfRef.__Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i);
                    }
                    vlSelfRef.__Vfunc_LSQ_SelectBits__2__Vfuncout 
                        = vlSelfRef.__Vfunc_LSQ_SelectBits__2__ret;
                }(), vlSelfRef.__Vfunc_LSQ_SelectBits__2__Vfuncout)));
    __Vlvbound_h7b862a5f__0 = __Vfunc_LSQ_ToBlockWordEnable__1__Vfuncout;
    vlSelfRef.__PVT__executedStoreWordWE[0U] = __Vlvbound_h7b862a5f__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__wa[0U] 
        = vlSelfRef.__PVT__executedStoreQueuePtrByStore
        [0U];
    __Vlvbound_h54f85ec8__0 = vlSelfRef.__PVT__executeStore
        [0U];
    vlSelfRef.__PVT__sqWE[0U] = __Vlvbound_h54f85ec8__0;
    __Vtask_GenerateStoreData__4__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
        [0U];
    __Vtask_GenerateStoreData__4__dataIn = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreData
        [0U];
    __Vtask_GenerateStoreData__4__dataOut = __Vtask_GenerateStoreData__4__dataIn;
    __Vtask_GenerateStoreData__4__dataOut = VL_SHIFTL_III(32,32,32, __Vtask_GenerateStoreData__4__dataOut, 
                                                          VL_SHIFTL_III(32,32,32, 
                                                                        (3U 
                                                                         & __Vtask_GenerateStoreData__4__addr), 3U));
    __Vlvbound_h7a0758e3__0 = __Vtask_GenerateStoreData__4__dataOut;
    vlSelfRef.__PVT__sqWriteStoreData[0U] = __Vlvbound_h7a0758e3__0;
    __Vlvbound_h13192e71__0 = vlSelfRef.__PVT__sqWriteStoreData
        [0U];
    vlSelfRef.__PVT__sqWriteData[0U] = ((0x200000001fULL 
                                         & vlSelfRef.__PVT__sqWriteData
                                         [0U]) | ((QData)((IData)(__Vlvbound_h13192e71__0)) 
                                                  << 5U));
    __Vlvbound_hf6ea1d33__0 = vlSelfRef.__PVT__executedStoreCondEnabled
        [0U];
    vlSelfRef.__PVT__sqWriteData[0U] = ((0x1fffffffffULL 
                                         & vlSelfRef.__PVT__sqWriteData
                                         [0U]) | ((QData)((IData)(__Vlvbound_hf6ea1d33__0)) 
                                                  << 0x25U));
    __Vlvbound_hf6ec10ee__0 = vlSelfRef.__PVT__executedStoreWordWE
        [0U];
    vlSelfRef.__PVT__sqWriteData[0U] = ((0x3fffffffefULL 
                                         & vlSelfRef.__PVT__sqWriteData
                                         [0U]) | ((QData)((IData)(__Vlvbound_hf6ec10ee__0)) 
                                                  << 4U));
    __Vlvbound_h0c9efe36__0 = vlSelfRef.__PVT__executedStoreByteWE
        [0U];
    vlSelfRef.__PVT__sqWriteData[0U] = ((0x3ffffffff0ULL 
                                         & vlSelfRef.__PVT__sqWriteData
                                         [0U]) | (IData)((IData)(__Vlvbound_h0c9efe36__0)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__we[0U] 
        = vlSelfRef.__PVT__sqWE[0U];
    vlSelfRef.__Vcellinp__storeQueueData__wv[0U] = 
        vlSelfRef.__PVT__sqWriteData[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__wv[0U] 
        = vlSelfRef.__Vcellinp__storeQueueData__wv[0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_StoreQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__0(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __VdlySet__storeQueue__v0;
    __VdlySet__storeQueue__v0 = 0;
    CData/*0:0*/ __VdlyVal__storeQueue__v64;
    __VdlyVal__storeQueue__v64 = 0;
    CData/*3:0*/ __VdlyDim0__storeQueue__v64;
    __VdlyDim0__storeQueue__v64 = 0;
    CData/*0:0*/ __VdlySet__storeQueue__v64;
    __VdlySet__storeQueue__v64 = 0;
    CData/*0:0*/ __VdlyVal__storeQueue__v65;
    __VdlyVal__storeQueue__v65 = 0;
    CData/*3:0*/ __VdlyDim0__storeQueue__v65;
    __VdlyDim0__storeQueue__v65 = 0;
    IData/*19:0*/ __VdlyVal__storeQueue__v66;
    __VdlyVal__storeQueue__v66 = 0;
    CData/*3:0*/ __VdlyDim0__storeQueue__v66;
    __VdlyDim0__storeQueue__v66 = 0;
    CData/*0:0*/ __VdlyVal__storeQueue__v67;
    __VdlyVal__storeQueue__v67 = 0;
    CData/*3:0*/ __VdlyDim0__storeQueue__v67;
    __VdlyDim0__storeQueue__v67 = 0;
    CData/*3:0*/ __VdlyVal__storeQueue__v68;
    __VdlyVal__storeQueue__v68 = 0;
    CData/*3:0*/ __VdlyDim0__storeQueue__v68;
    __VdlyDim0__storeQueue__v68 = 0;
    CData/*3:0*/ __VdlyDim0__storeQueue__v69;
    __VdlyDim0__storeQueue__v69 = 0;
    CData/*0:0*/ __VdlySet__storeQueue__v69;
    __VdlySet__storeQueue__v69 = 0;
    CData/*3:0*/ __VdlyDim0__storeQueue__v70;
    __VdlyDim0__storeQueue__v70 = 0;
    CData/*0:0*/ __VdlySet__storeQueue__v70;
    __VdlySet__storeQueue__v70 = 0;
    // Body
    if (VL_UNLIKELY(((1U & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                               | (0x10U >= (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regCount)))))))) {
        VL_WRITEF_NX("The count of a queue exceeds its size.\n",0);
    }
    if (VL_UNLIKELY(((1U & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                               | (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHead) 
                                     & (0U == (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regCount)))))))))) {
        VL_WRITEF_NX("Pop from a empty store queue.\n",0);
    }
    if (VL_UNLIKELY((((vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeLoad
                       [0U] & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead) 
                               < (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail))) 
                      & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByLoad
                         [0U] < (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead)))))) {
        VL_WRITEF_NX("1:A load's executedStoreQueuePtr is illegal.\n",0);
    }
    if (VL_UNLIKELY((((vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeLoad
                       [0U] & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead) 
                               < (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail))) 
                      & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail) 
                         < vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByLoad
                         [0U]))))) {
        VL_WRITEF_NX("2:A load's executedStoreQueuePtr is illegal.\n",0);
    }
    if (VL_UNLIKELY(((((vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeLoad
                        [0U] & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail) 
                                <= (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead))) 
                       & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regTail) 
                          < vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByLoad
                          [0U])) & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByLoad
                                    [0U] < (IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__regHead)))))) {
        VL_WRITEF_NX("3:A load's executedStoreQueuePtr is illegal.\n",0);
    }
    __VdlySet__storeQueue__v0 = 0U;
    __VdlySet__storeQueue__v64 = 0U;
    __VdlySet__storeQueue__v69 = 0U;
    __VdlySet__storeQueue__v70 = 0U;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk2__DOT__i = 0x10U;
        __VdlySet__storeQueue__v0 = 1U;
        vlSelfRef.__PVT__storeQueuePointer__DOT__regCount = 0U;
        vlSelfRef.__PVT__storeQueuePointer__DOT__regTail = 0U;
        vlSelfRef.__PVT__storeQueuePointer__DOT__regHead = 0U;
    } else {
        if (vlSelfRef.__PVT__executeStore[0U]) {
            __VdlyVal__storeQueue__v64 = vlSelfRef.__PVT__executedStoreRegValid
                [0U];
            __VdlyDim0__storeQueue__v64 = vlSelfRef.__PVT__executedStoreQueuePtrByStore
                [0U];
            __VdlySet__storeQueue__v64 = 1U;
            __VdlyVal__storeQueue__v65 = vlSelfRef.__PVT__executedStoreCondEnabled
                [0U];
            __VdlyDim0__storeQueue__v65 = vlSelfRef.__PVT__executedStoreQueuePtrByStore
                [0U];
            __VdlyVal__storeQueue__v66 = vlSelfRef.__PVT__executedStoreAddr
                [0U];
            __VdlyDim0__storeQueue__v66 = vlSelfRef.__PVT__executedStoreQueuePtrByStore
                [0U];
            __VdlyVal__storeQueue__v67 = vlSelfRef.__PVT__executedStoreWordWE
                [0U];
            __VdlyDim0__storeQueue__v67 = vlSelfRef.__PVT__executedStoreQueuePtrByStore
                [0U];
            __VdlyVal__storeQueue__v68 = vlSelfRef.__PVT__executedStoreByteWE
                [0U];
            __VdlyDim0__storeQueue__v68 = vlSelfRef.__PVT__executedStoreQueuePtrByStore
                [0U];
        }
        if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue
            [0U]) {
            __VdlyDim0__storeQueue__v69 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr
                [0U];
            __VdlySet__storeQueue__v69 = 1U;
        }
        if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue
            [1U]) {
            __VdlyDim0__storeQueue__v70 = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr
                [1U];
            __VdlySet__storeQueue__v70 = 1U;
        }
        vlSelfRef.__PVT__storeQueuePointer__DOT__regCount 
            = vlSelfRef.__PVT__storeQueuePointer__DOT__nextCount;
        vlSelfRef.__PVT__storeQueuePointer__DOT__regTail 
            = vlSelfRef.__PVT__storeQueuePointer__DOT__nextTail;
        vlSelfRef.__PVT__storeQueuePointer__DOT__regHead 
            = vlSelfRef.__PVT__storeQueuePointer__DOT__nextHead;
    }
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk3__DOT__i = 1U;
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
    }
    if (__VdlySet__storeQueue__v0) {
        vlSelfRef.__PVT__storeQueue[0U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [0U]);
        vlSelfRef.__PVT__storeQueue[0U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [0U]);
        vlSelfRef.__PVT__storeQueue[0U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [0U]);
        vlSelfRef.__PVT__storeQueue[0U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [0U]);
        vlSelfRef.__PVT__storeQueue[1U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [1U]);
        vlSelfRef.__PVT__storeQueue[1U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [1U]);
        vlSelfRef.__PVT__storeQueue[1U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [1U]);
        vlSelfRef.__PVT__storeQueue[1U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [1U]);
        vlSelfRef.__PVT__storeQueue[2U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [2U]);
        vlSelfRef.__PVT__storeQueue[2U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [2U]);
        vlSelfRef.__PVT__storeQueue[2U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [2U]);
        vlSelfRef.__PVT__storeQueue[2U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [2U]);
        vlSelfRef.__PVT__storeQueue[3U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [3U]);
        vlSelfRef.__PVT__storeQueue[3U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [3U]);
        vlSelfRef.__PVT__storeQueue[3U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [3U]);
        vlSelfRef.__PVT__storeQueue[3U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [3U]);
        vlSelfRef.__PVT__storeQueue[4U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [4U]);
        vlSelfRef.__PVT__storeQueue[4U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [4U]);
        vlSelfRef.__PVT__storeQueue[4U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [4U]);
        vlSelfRef.__PVT__storeQueue[4U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [4U]);
        vlSelfRef.__PVT__storeQueue[5U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [5U]);
        vlSelfRef.__PVT__storeQueue[5U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [5U]);
        vlSelfRef.__PVT__storeQueue[5U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [5U]);
        vlSelfRef.__PVT__storeQueue[5U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [5U]);
        vlSelfRef.__PVT__storeQueue[6U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [6U]);
        vlSelfRef.__PVT__storeQueue[6U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [6U]);
        vlSelfRef.__PVT__storeQueue[6U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [6U]);
        vlSelfRef.__PVT__storeQueue[6U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [6U]);
        vlSelfRef.__PVT__storeQueue[7U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [7U]);
        vlSelfRef.__PVT__storeQueue[7U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [7U]);
        vlSelfRef.__PVT__storeQueue[7U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [7U]);
        vlSelfRef.__PVT__storeQueue[7U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [7U]);
        vlSelfRef.__PVT__storeQueue[8U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [8U]);
        vlSelfRef.__PVT__storeQueue[8U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [8U]);
        vlSelfRef.__PVT__storeQueue[8U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [8U]);
        vlSelfRef.__PVT__storeQueue[8U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [8U]);
        vlSelfRef.__PVT__storeQueue[9U] = (0x5ffffffU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [9U]);
        vlSelfRef.__PVT__storeQueue[9U] = (0x600001fU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [9U]);
        vlSelfRef.__PVT__storeQueue[9U] = (0x7ffffefU 
                                           & vlSelfRef.__PVT__storeQueue
                                           [9U]);
        vlSelfRef.__PVT__storeQueue[9U] = (0x7fffff0U 
                                           & vlSelfRef.__PVT__storeQueue
                                           [9U]);
        vlSelfRef.__PVT__storeQueue[0xaU] = (0x5ffffffU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xaU]);
        vlSelfRef.__PVT__storeQueue[0xaU] = (0x600001fU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xaU]);
        vlSelfRef.__PVT__storeQueue[0xaU] = (0x7ffffefU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xaU]);
        vlSelfRef.__PVT__storeQueue[0xaU] = (0x7fffff0U 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xaU]);
        vlSelfRef.__PVT__storeQueue[0xbU] = (0x5ffffffU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xbU]);
        vlSelfRef.__PVT__storeQueue[0xbU] = (0x600001fU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xbU]);
        vlSelfRef.__PVT__storeQueue[0xbU] = (0x7ffffefU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xbU]);
        vlSelfRef.__PVT__storeQueue[0xbU] = (0x7fffff0U 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xbU]);
        vlSelfRef.__PVT__storeQueue[0xcU] = (0x5ffffffU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xcU]);
        vlSelfRef.__PVT__storeQueue[0xcU] = (0x600001fU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xcU]);
        vlSelfRef.__PVT__storeQueue[0xcU] = (0x7ffffefU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xcU]);
        vlSelfRef.__PVT__storeQueue[0xcU] = (0x7fffff0U 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xcU]);
        vlSelfRef.__PVT__storeQueue[0xdU] = (0x5ffffffU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xdU]);
        vlSelfRef.__PVT__storeQueue[0xdU] = (0x600001fU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xdU]);
        vlSelfRef.__PVT__storeQueue[0xdU] = (0x7ffffefU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xdU]);
        vlSelfRef.__PVT__storeQueue[0xdU] = (0x7fffff0U 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xdU]);
        vlSelfRef.__PVT__storeQueue[0xeU] = (0x5ffffffU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xeU]);
        vlSelfRef.__PVT__storeQueue[0xeU] = (0x600001fU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xeU]);
        vlSelfRef.__PVT__storeQueue[0xeU] = (0x7ffffefU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xeU]);
        vlSelfRef.__PVT__storeQueue[0xeU] = (0x7fffff0U 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xeU]);
        vlSelfRef.__PVT__storeQueue[0xfU] = (0x5ffffffU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xfU]);
        vlSelfRef.__PVT__storeQueue[0xfU] = (0x600001fU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xfU]);
        vlSelfRef.__PVT__storeQueue[0xfU] = (0x7ffffefU 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xfU]);
        vlSelfRef.__PVT__storeQueue[0xfU] = (0x7fffff0U 
                                             & vlSelfRef.__PVT__storeQueue
                                             [0xfU]);
    }
    if (__VdlySet__storeQueue__v64) {
        vlSelfRef.__PVT__storeQueue[__VdlyDim0__storeQueue__v64] 
            = ((0x3ffffffU & vlSelfRef.__PVT__storeQueue
                [__VdlyDim0__storeQueue__v64]) | ((IData)(__VdlyVal__storeQueue__v64) 
                                                  << 0x1aU));
        vlSelfRef.__PVT__storeQueue[__VdlyDim0__storeQueue__v65] 
            = ((0x5ffffffU & vlSelfRef.__PVT__storeQueue
                [__VdlyDim0__storeQueue__v65]) | ((IData)(__VdlyVal__storeQueue__v65) 
                                                  << 0x19U));
        vlSelfRef.__PVT__storeQueue[__VdlyDim0__storeQueue__v66] 
            = ((0x600001fU & vlSelfRef.__PVT__storeQueue
                [__VdlyDim0__storeQueue__v66]) | (__VdlyVal__storeQueue__v66 
                                                  << 5U));
        vlSelfRef.__PVT__storeQueue[__VdlyDim0__storeQueue__v67] 
            = ((0x7ffffefU & vlSelfRef.__PVT__storeQueue
                [__VdlyDim0__storeQueue__v67]) | ((IData)(__VdlyVal__storeQueue__v67) 
                                                  << 4U));
        vlSelfRef.__PVT__storeQueue[__VdlyDim0__storeQueue__v68] 
            = ((0x7fffff0U & vlSelfRef.__PVT__storeQueue
                [__VdlyDim0__storeQueue__v68]) | (IData)(__VdlyVal__storeQueue__v68));
    }
    if (__VdlySet__storeQueue__v69) {
        vlSelfRef.__PVT__storeQueue[__VdlyDim0__storeQueue__v69] 
            = (0x5ffffffU & vlSelfRef.__PVT__storeQueue
               [__VdlyDim0__storeQueue__v69]);
    }
    if (__VdlySet__storeQueue__v70) {
        vlSelfRef.__PVT__storeQueue[__VdlyDim0__storeQueue__v70] 
            = (0x5ffffffU & vlSelfRef.__PVT__storeQueue
               [__VdlyDim0__storeQueue__v70]);
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_StoreQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__1(VSMT_RTL_Testbench_StoreQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_StoreQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__storeQueue__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__storeQueuePointer__DOT__nextHead 
        = vlSelfRef.__PVT__storeQueuePointer__DOT__regHead;
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHead) {
        vlSelfRef.__PVT__storeQueuePointer__DOT__nextHead 
            = (0xfU & ((IData)(vlSelfRef.__PVT__storeQueuePointer__DOT__nextHead) 
                       + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHeadEntryNum)));
    }
}
