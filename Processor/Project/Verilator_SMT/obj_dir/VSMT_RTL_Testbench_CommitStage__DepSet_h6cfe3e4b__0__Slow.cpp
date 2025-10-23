// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_CommitStage.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_CommitStage___stl_comb__TOP__SMT_RTL_Testbench__core__cmStage__0(VSMT_RTL_Testbench_CommitStage* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_CommitStage___stl_comb__TOP__SMT_RTL_Testbench__core__cmStage__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vtask_DecideCommit__0__toRecoveryPhase;
    __Vtask_DecideCommit__0__toRecoveryPhase = 0;
    CData/*0:0*/ __Vtask_DecideCommit__0__recoveredIndex;
    __Vtask_DecideCommit__0__recoveredIndex = 0;
    CData/*2:0*/ __Vtask_DecideCommit__0__refetchType;
    __Vtask_DecideCommit__0__refetchType = 0;
    CData/*3:0*/ __Vtask_DecideCommit__0__recoveryCause;
    __Vtask_DecideCommit__0__recoveryCause = 0;
    CData/*0:0*/ __Vtask_DecideCommit__0__startCommit;
    __Vtask_DecideCommit__0__startCommit = 0;
    CData/*6:0*/ __Vtask_DecideCommit__0__activeListCount;
    __Vtask_DecideCommit__0__activeListCount = 0;
    CData/*0:0*/ __Vtask_DecideCommit__0__unableToStartRecovery;
    __Vtask_DecideCommit__0__unableToStartRecovery = 0;
    CData/*0:0*/ __Vtask_DecideCommit__0__recoveryTrigger;
    __Vtask_DecideCommit__0__recoveryTrigger = 0;
    CData/*1:0*/ __Vtask_DecideCommit__0__recoveryStart;
    __Vtask_DecideCommit__0__recoveryStart = 0;
    CData/*1:0*/ __Vtask_DecideCommit__0__finishedOpNum;
    __Vtask_DecideCommit__0__finishedOpNum = 0;
    CData/*1:0*/ __Vtask_DecideCommit__0__finishedInsnRange;
    __Vtask_DecideCommit__0__finishedInsnRange = 0;
    IData/*31:0*/ __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j;
    __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j = 0;
    IData/*31:0*/ __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j;
    __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j = 0;
    CData/*1:0*/ __Vtask_GetFinishedOpNum__2__finishedOpNum;
    __Vtask_GetFinishedOpNum__2__finishedOpNum = 0;
    CData/*6:0*/ __Vtask_GetFinishedOpNum__2__activeListCount;
    __Vtask_GetFinishedOpNum__2__activeListCount = 0;
    IData/*31:0*/ __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i;
    __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i = 0;
    CData/*1:0*/ __Vtask_GetFinishedInsnRange__3__finishedInsnRange;
    __Vtask_GetFinishedInsnRange__3__finishedInsnRange = 0;
    CData/*1:0*/ __Vtask_GetFinishedInsnRange__3__finishedOpNum;
    __Vtask_GetFinishedInsnRange__3__finishedOpNum = 0;
    VlUnpacked<CData/*3:0*/, 2> __Vtask_GetFinishedInsnRange__3__execState;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        __Vtask_GetFinishedInsnRange__3__execState[__Vi0] = 0;
    }
    IData/*31:0*/ __Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i;
    __Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i = 0;
    // Body
    vlSelfRef.alReadData[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
        [0U];
    vlSelfRef.alReadData[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
        [1U];
    vlSelfRef.__PVT__execState[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState
        [0U];
    vlSelfRef.__PVT__execState[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__headExecState
        [1U];
    vlSelfRef.__PVT__phase = (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                    >> 0x15U));
    vlSelfRef.__PVT__last[0U] = (1U & (IData)((vlSelfRef.alReadData
                                               [0U] 
                                               >> 0x13U)));
    vlSelfRef.__PVT__isBranch[0U] = (1U & (IData)((
                                                   vlSelfRef.alReadData
                                                   [0U] 
                                                   >> 0x15U)));
    vlSelfRef.__PVT__isStore[0U] = (1U & (IData)((vlSelfRef.alReadData
                                                  [0U] 
                                                  >> 0x16U)));
    vlSelfRef.__PVT__last[1U] = (1U & (IData)((vlSelfRef.alReadData
                                               [1U] 
                                               >> 0x13U)));
    vlSelfRef.__PVT__isBranch[1U] = (1U & (IData)((
                                                   vlSelfRef.alReadData
                                                   [1U] 
                                                   >> 0x15U)));
    vlSelfRef.__PVT__isStore[1U] = (1U & (IData)((vlSelfRef.alReadData
                                                  [1U] 
                                                  >> 0x16U)));
    vlSelfRef.__PVT__unnamedblk1__DOT__i = 2U;
    __Vtask_DecideCommit__0__unableToStartRecovery 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery;
    vlSelfRef.__Vtask_DecideCommit__0__last[0U] = vlSelfRef.__PVT__last
        [0U];
    vlSelfRef.__Vtask_DecideCommit__0__last[1U] = vlSelfRef.__PVT__last
        [1U];
    vlSelfRef.__Vtask_DecideCommit__0__isStore[0U] 
        = vlSelfRef.__PVT__isStore[0U];
    vlSelfRef.__Vtask_DecideCommit__0__isStore[1U] 
        = vlSelfRef.__PVT__isStore[1U];
    vlSelfRef.__Vtask_DecideCommit__0__isBranch[0U] 
        = vlSelfRef.__PVT__isBranch[0U];
    vlSelfRef.__Vtask_DecideCommit__0__isBranch[1U] 
        = vlSelfRef.__PVT__isBranch[1U];
    vlSelfRef.__Vtask_DecideCommit__0__execState[0U] 
        = vlSelfRef.__PVT__execState[0U];
    vlSelfRef.__Vtask_DecideCommit__0__execState[1U] 
        = vlSelfRef.__PVT__execState[1U];
    __Vtask_DecideCommit__0__activeListCount = vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount;
    __Vtask_DecideCommit__0__startCommit = (0U == (IData)(vlSelfRef.__PVT__phase));
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__headOfThisInsn[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__tailOfThisInsn[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__recovery[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__recoveryPoint[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtask_DecideCommit__0__opRefetchType[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelfRef.__Vtask_GetInsnPtr__1__last[0U] = vlSelfRef.__Vtask_DecideCommit__0__last
        [0U];
    vlSelfRef.__Vtask_GetInsnPtr__1__last[1U] = vlSelfRef.__Vtask_DecideCommit__0__last
        [1U];
    vlSelfRef.__Vtask_GetInsnPtr__1__headOfThisInsn[0U] = 0U;
    __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j = 0xffffffffU;
    {
        while (VL_LTES_III(32, 0U, __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j)) {
            if (vlSelfRef.__Vtask_GetInsnPtr__1__last
                [(1U & __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j)]) {
                vlSelfRef.__Vtask_GetInsnPtr__1__headOfThisInsn[0U] 
                    = (1U & ((IData)(1U) + __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j));
                goto __Vlabel1;
            }
            __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j 
                = (__Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j 
                   - (IData)(1U));
        }
        __Vlabel1: ;
    }
    vlSelfRef.__Vtask_GetInsnPtr__1__tailOfThisInsn[0U] = 1U;
    __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j)) {
            if (vlSelfRef.__Vtask_GetInsnPtr__1__last
                [(1U & __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j)]) {
                vlSelfRef.__Vtask_GetInsnPtr__1__tailOfThisInsn[0U] 
                    = (1U & __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j);
                goto __Vlabel2;
            }
            __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j 
                = ((IData)(1U) + __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j);
        }
        __Vlabel2: ;
    }
    vlSelfRef.__Vtask_GetInsnPtr__1__headOfThisInsn[1U] = 0U;
    __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j = 0U;
    {
        while (VL_LTES_III(32, 0U, __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j)) {
            if (vlSelfRef.__Vtask_GetInsnPtr__1__last
                [(1U & __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j)]) {
                vlSelfRef.__Vtask_GetInsnPtr__1__headOfThisInsn[1U] 
                    = (1U & ((IData)(1U) + __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j));
                goto __Vlabel3;
            }
            __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j 
                = (__Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk4__DOT__j 
                   - (IData)(1U));
        }
        __Vlabel3: ;
    }
    vlSelfRef.__Vtask_GetInsnPtr__1__tailOfThisInsn[1U] = 1U;
    __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j = 1U;
    {
        while (VL_GTS_III(32, 2U, __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j)) {
            if (vlSelfRef.__Vtask_GetInsnPtr__1__last
                [(1U & __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j)]) {
                vlSelfRef.__Vtask_GetInsnPtr__1__tailOfThisInsn[1U] 
                    = (1U & __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j);
                goto __Vlabel4;
            }
            __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j 
                = ((IData)(1U) + __Vtask_GetInsnPtr__1__unnamedblk3__DOT__unnamedblk5__DOT__j);
        }
        __Vlabel4: ;
    }
    vlSelfRef.__Vtask_DecideCommit__0__headOfThisInsn[0U] 
        = vlSelfRef.__Vtask_GetInsnPtr__1__headOfThisInsn
        [0U];
    vlSelfRef.__Vtask_DecideCommit__0__headOfThisInsn[1U] 
        = vlSelfRef.__Vtask_GetInsnPtr__1__headOfThisInsn
        [1U];
    vlSelfRef.__Vtask_DecideCommit__0__tailOfThisInsn[0U] 
        = vlSelfRef.__Vtask_GetInsnPtr__1__tailOfThisInsn
        [0U];
    vlSelfRef.__Vtask_DecideCommit__0__tailOfThisInsn[1U] 
        = vlSelfRef.__Vtask_GetInsnPtr__1__tailOfThisInsn
        [1U];
    vlSelfRef.__Vtask_GetFinishedOpNum__2__execState[0U] 
        = vlSelfRef.__Vtask_DecideCommit__0__execState
        [0U];
    vlSelfRef.__Vtask_GetFinishedOpNum__2__execState[1U] 
        = vlSelfRef.__Vtask_DecideCommit__0__execState
        [1U];
    __Vtask_GetFinishedOpNum__2__activeListCount = __Vtask_DecideCommit__0__activeListCount;
    __Vtask_GetFinishedOpNum__2__finishedOpNum = 0U;
    __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i)) {
            if (((__Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i 
                  < (IData)(__Vtask_GetFinishedOpNum__2__activeListCount)) 
                 & (0U != vlSelfRef.__Vtask_GetFinishedOpNum__2__execState
                    [(1U & __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i)]))) {
                __Vtask_GetFinishedOpNum__2__finishedOpNum 
                    = (3U & ((IData)(1U) + __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i));
            } else {
                goto __Vlabel5;
            }
            __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i 
                = ((IData)(1U) + __Vtask_GetFinishedOpNum__2__unnamedblk2__DOT__i);
        }
        __Vlabel5: ;
    }
    __Vtask_DecideCommit__0__finishedOpNum = __Vtask_GetFinishedOpNum__2__finishedOpNum;
    vlSelfRef.__Vtask_GetFinishedInsnRange__3__last[0U] 
        = vlSelfRef.__Vtask_DecideCommit__0__last[0U];
    vlSelfRef.__Vtask_GetFinishedInsnRange__3__last[1U] 
        = vlSelfRef.__Vtask_DecideCommit__0__last[1U];
    __Vtask_GetFinishedInsnRange__3__execState[0U] 
        = vlSelfRef.__Vtask_DecideCommit__0__execState
        [0U];
    __Vtask_GetFinishedInsnRange__3__execState[1U] 
        = vlSelfRef.__Vtask_DecideCommit__0__execState
        [1U];
    __Vtask_GetFinishedInsnRange__3__finishedOpNum 
        = __Vtask_DecideCommit__0__finishedOpNum;
    __Vtask_GetFinishedInsnRange__3__finishedInsnRange = 0U;
    __Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i = 1U;
    {
        while (VL_LTES_III(32, 0U, __Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i)) {
            if (((__Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i 
                  < (IData)(__Vtask_GetFinishedInsnRange__3__finishedOpNum)) 
                 & vlSelfRef.__Vtask_GetFinishedInsnRange__3__last
                 [(1U & __Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i)])) {
                __Vtask_GetFinishedInsnRange__3__finishedInsnRange 
                    = (3U & ((IData)(1U) + __Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i));
                goto __Vlabel6;
            }
            __Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i 
                = (__Vtask_GetFinishedInsnRange__3__unnamedblk1__DOT__i 
                   - (IData)(1U));
        }
        __Vlabel6: ;
    }
    __Vtask_DecideCommit__0__finishedInsnRange = __Vtask_GetFinishedInsnRange__3__finishedInsnRange;
    if ((3U == vlSelfRef.__Vtask_DecideCommit__0__execState
         [0U])) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[0U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[0U] 
            = vlSelfRef.__Vtask_DecideCommit__0__tailOfThisInsn
            [0U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[0U] 
            = (vlSelfRef.__Vtask_DecideCommit__0__isBranch
               [0U] ? 3U : (vlSelfRef.__Vtask_DecideCommit__0__isStore
                            [0U] ? 2U : 1U));
    } else if (((2U == vlSelfRef.__Vtask_DecideCommit__0__execState
                 [0U]) | (0xfU == vlSelfRef.__Vtask_DecideCommit__0__execState
                          [0U]))) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[0U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[0U] 
            = vlSelfRef.__Vtask_DecideCommit__0__headOfThisInsn
            [0U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[0U] = 0U;
    } else if (((((4U == vlSelfRef.__Vtask_DecideCommit__0__execState
                   [0U]) | (5U == vlSelfRef.__Vtask_DecideCommit__0__execState
                            [0U])) | (6U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                      [0U])) | (0xeU 
                                                == 
                                                vlSelfRef.__Vtask_DecideCommit__0__execState
                                                [0U]))) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[0U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[0U] 
            = vlSelfRef.__Vtask_DecideCommit__0__tailOfThisInsn
            [0U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[0U] = 4U;
    } else if (((((((8U == vlSelfRef.__Vtask_DecideCommit__0__execState
                     [0U]) | (9U == vlSelfRef.__Vtask_DecideCommit__0__execState
                              [0U])) | (0xaU == vlSelfRef.__Vtask_DecideCommit__0__execState
                                        [0U])) | (0xbU 
                                                  == 
                                                  vlSelfRef.__Vtask_DecideCommit__0__execState
                                                  [0U])) 
                 | (0xcU == vlSelfRef.__Vtask_DecideCommit__0__execState
                    [0U])) | (0xdU == vlSelfRef.__Vtask_DecideCommit__0__execState
                              [0U]))) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[0U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[0U] 
            = vlSelfRef.__Vtask_DecideCommit__0__headOfThisInsn
            [0U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[0U] = 5U;
    } else {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[0U] = 0U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[0U] = 0U;
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[0U] = 0U;
    }
    if ((3U == vlSelfRef.__Vtask_DecideCommit__0__execState
         [1U])) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[1U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[1U] 
            = vlSelfRef.__Vtask_DecideCommit__0__tailOfThisInsn
            [1U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[1U] 
            = (vlSelfRef.__Vtask_DecideCommit__0__isBranch
               [1U] ? 3U : (vlSelfRef.__Vtask_DecideCommit__0__isStore
                            [1U] ? 2U : 1U));
    } else if (((2U == vlSelfRef.__Vtask_DecideCommit__0__execState
                 [1U]) | (0xfU == vlSelfRef.__Vtask_DecideCommit__0__execState
                          [1U]))) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[1U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[1U] 
            = vlSelfRef.__Vtask_DecideCommit__0__headOfThisInsn
            [1U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[1U] = 0U;
    } else if (((((4U == vlSelfRef.__Vtask_DecideCommit__0__execState
                   [1U]) | (5U == vlSelfRef.__Vtask_DecideCommit__0__execState
                            [1U])) | (6U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                      [1U])) | (0xeU 
                                                == 
                                                vlSelfRef.__Vtask_DecideCommit__0__execState
                                                [1U]))) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[1U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[1U] 
            = vlSelfRef.__Vtask_DecideCommit__0__tailOfThisInsn
            [1U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[1U] = 4U;
    } else if (((((((8U == vlSelfRef.__Vtask_DecideCommit__0__execState
                     [1U]) | (9U == vlSelfRef.__Vtask_DecideCommit__0__execState
                              [1U])) | (0xaU == vlSelfRef.__Vtask_DecideCommit__0__execState
                                        [1U])) | (0xbU 
                                                  == 
                                                  vlSelfRef.__Vtask_DecideCommit__0__execState
                                                  [1U])) 
                 | (0xcU == vlSelfRef.__Vtask_DecideCommit__0__execState
                    [1U])) | (0xdU == vlSelfRef.__Vtask_DecideCommit__0__execState
                              [1U]))) {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[1U] = 1U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[1U] 
            = vlSelfRef.__Vtask_DecideCommit__0__headOfThisInsn
            [1U];
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[1U] = 5U;
    } else {
        vlSelfRef.__Vtask_DecideCommit__0__recovery[1U] = 0U;
        vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint[1U] = 0U;
        vlSelfRef.__Vtask_DecideCommit__0__opRefetchType[1U] = 0U;
    }
    __Vtask_DecideCommit__0__recoveryTrigger = 0U;
    __Vtask_DecideCommit__0__recoveredIndex = 0U;
    __Vtask_DecideCommit__0__recoveryStart = 2U;
    __Vtask_DecideCommit__0__refetchType = 0U;
    __Vtask_DecideCommit__0__recoveryCause = 1U;
    if ((0U < (IData)(__Vtask_DecideCommit__0__finishedInsnRange))) {
        if ((vlSelfRef.__Vtask_DecideCommit__0__recovery
             [0U] & (vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint
                     [0U] < (IData)(__Vtask_DecideCommit__0__recoveryStart)))) {
            __Vtask_DecideCommit__0__recoveryTrigger = 1U;
            __Vtask_DecideCommit__0__recoveredIndex = 0U;
            __Vtask_DecideCommit__0__recoveryStart 
                = vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint
                [0U];
            __Vtask_DecideCommit__0__refetchType = 
                vlSelfRef.__Vtask_DecideCommit__0__opRefetchType
                [0U];
            __Vtask_DecideCommit__0__recoveryCause 
                = vlSelfRef.__Vtask_DecideCommit__0__execState
                [0U];
        }
    }
    if ((1U < (IData)(__Vtask_DecideCommit__0__finishedInsnRange))) {
        if ((vlSelfRef.__Vtask_DecideCommit__0__recovery
             [1U] & (vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint
                     [1U] < (IData)(__Vtask_DecideCommit__0__recoveryStart)))) {
            __Vtask_DecideCommit__0__recoveryTrigger = 1U;
            __Vtask_DecideCommit__0__recoveredIndex = 1U;
            __Vtask_DecideCommit__0__recoveryStart 
                = vlSelfRef.__Vtask_DecideCommit__0__recoveryPoint
                [1U];
            __Vtask_DecideCommit__0__refetchType = 
                vlSelfRef.__Vtask_DecideCommit__0__opRefetchType
                [1U];
            __Vtask_DecideCommit__0__recoveryCause 
                = vlSelfRef.__Vtask_DecideCommit__0__execState
                [1U];
        }
    }
    __Vtask_DecideCommit__0__toRecoveryPhase = (((IData)(__Vtask_DecideCommit__0__startCommit) 
                                                 & (IData)(__Vtask_DecideCommit__0__recoveryTrigger)) 
                                                & (~ (IData)(__Vtask_DecideCommit__0__unableToStartRecovery)));
    vlSelfRef.__Vtask_DecideCommit__0__commit[0U] = 
        ((IData)(__Vtask_DecideCommit__0__startCommit) 
         && ((IData)(__Vtask_DecideCommit__0__recoveryTrigger)
              ? ((0U < (IData)(__Vtask_DecideCommit__0__recoveryStart)) 
                 || ((0U == (IData)(__Vtask_DecideCommit__0__recoveryStart)) 
                     && (1U & (~ (((((((((2U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                          [0U]) | (0xfU 
                                                   == 
                                                   vlSelfRef.__Vtask_DecideCommit__0__execState
                                                   [0U])) 
                                        | (8U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                           [0U])) | 
                                       (9U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                        [0U])) | (0xaU 
                                                  == 
                                                  vlSelfRef.__Vtask_DecideCommit__0__execState
                                                  [0U])) 
                                     | (0xbU == vlSelfRef.__Vtask_DecideCommit__0__execState
                                        [0U])) | (0xcU 
                                                  == 
                                                  vlSelfRef.__Vtask_DecideCommit__0__execState
                                                  [0U])) 
                                   | (0xdU == vlSelfRef.__Vtask_DecideCommit__0__execState
                                      [0U])) | (IData)(__Vtask_DecideCommit__0__unableToStartRecovery))))))
              : (0U < (IData)(__Vtask_DecideCommit__0__finishedInsnRange))));
    vlSelfRef.__Vtask_DecideCommit__0__commit[1U] = 
        ((IData)(__Vtask_DecideCommit__0__startCommit) 
         && ((IData)(__Vtask_DecideCommit__0__recoveryTrigger)
              ? ((1U < (IData)(__Vtask_DecideCommit__0__recoveryStart)) 
                 || ((1U == (IData)(__Vtask_DecideCommit__0__recoveryStart)) 
                     && (1U & (~ (((((((((2U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                          [1U]) | (0xfU 
                                                   == 
                                                   vlSelfRef.__Vtask_DecideCommit__0__execState
                                                   [1U])) 
                                        | (8U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                           [1U])) | 
                                       (9U == vlSelfRef.__Vtask_DecideCommit__0__execState
                                        [1U])) | (0xaU 
                                                  == 
                                                  vlSelfRef.__Vtask_DecideCommit__0__execState
                                                  [1U])) 
                                     | (0xbU == vlSelfRef.__Vtask_DecideCommit__0__execState
                                        [1U])) | (0xcU 
                                                  == 
                                                  vlSelfRef.__Vtask_DecideCommit__0__execState
                                                  [1U])) 
                                   | (0xdU == vlSelfRef.__Vtask_DecideCommit__0__execState
                                      [1U])) | (IData)(__Vtask_DecideCommit__0__unableToStartRecovery))))))
              : (1U < (IData)(__Vtask_DecideCommit__0__finishedInsnRange))));
    vlSelfRef.commit[0U] = vlSelfRef.__Vtask_DecideCommit__0__commit
        [0U];
    vlSelfRef.commit[1U] = vlSelfRef.__Vtask_DecideCommit__0__commit
        [1U];
    vlSelfRef.__PVT__toRecoveryPhase = __Vtask_DecideCommit__0__toRecoveryPhase;
    vlSelfRef.__PVT__recoveryOpIndex = __Vtask_DecideCommit__0__recoveredIndex;
    vlSelfRef.__PVT__refetchType = __Vtask_DecideCommit__0__refetchType;
    vlSelfRef.__PVT__recoveryCause = __Vtask_DecideCommit__0__recoveryCause;
    vlSelfRef.__PVT__commitNum = 0U;
    vlSelfRef.__PVT__commitLoadNum = 0U;
    vlSelfRef.__PVT__commitStoreNum = 0U;
    vlSelfRef.__PVT__unnamedblk2__DOT__i = 2U;
    if (vlSelfRef.commit[0U]) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commit = 1U;
        vlSelfRef.__PVT__commitNum = (3U & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__commitNum)));
        if ((1U & (IData)((vlSelfRef.alReadData[0U] 
                           >> 0x17U)))) {
            vlSelfRef.__PVT__commitLoadNum = (3U & 
                                              ((IData)(1U) 
                                               + (IData)(vlSelfRef.__PVT__commitLoadNum)));
        }
        if ((1U & (IData)((vlSelfRef.alReadData[0U] 
                           >> 0x16U)))) {
            vlSelfRef.__PVT__commitStoreNum = (3U & 
                                               ((IData)(1U) 
                                                + (IData)(vlSelfRef.__PVT__commitStoreNum)));
        }
    } else {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commit = 0U;
    }
    if (vlSelfRef.commit[1U]) {
        vlSelfRef.__PVT__commitNum = (3U & ((IData)(1U) 
                                            + (IData)(vlSelfRef.__PVT__commitNum)));
        if ((1U & (IData)((vlSelfRef.alReadData[1U] 
                           >> 0x17U)))) {
            vlSelfRef.__PVT__commitLoadNum = (3U & 
                                              ((IData)(1U) 
                                               + (IData)(vlSelfRef.__PVT__commitLoadNum)));
        }
        if ((1U & (IData)((vlSelfRef.alReadData[1U] 
                           >> 0x16U)))) {
            vlSelfRef.__PVT__commitStoreNum = (3U & 
                                               ((IData)(1U) 
                                                + (IData)(vlSelfRef.__PVT__commitStoreNum)));
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commitNum 
        = vlSelfRef.__PVT__commitNum;
    vlSelfRef.__PVT__unnamedblk3__DOT__i = 2U;
    if (vlSelfRef.commit[0U]) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueue = 1U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueueEntryNum 
            = vlSelfRef.__PVT__commitLoadNum;
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStore = 1U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStoreNum 
            = vlSelfRef.__PVT__commitStoreNum;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage 
            = vlSelfRef.__PVT__toRecoveryPhase;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromCommitStage 
            = vlSelfRef.__PVT__refetchType;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryOpIndex 
            = vlSelfRef.__PVT__recoveryOpIndex;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage 
            = vlSelfRef.__PVT__recoveryCause;
        vlSelfRef.__PVT__lastCommittedPC = vlSelfRef.__PVT__prevLastCommittedPC;
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg 
            = ((2U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg)) 
               | (1U & (IData)((vlSelfRef.alReadData
                                [0U] >> 0x18U))));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum[0U] 
            = (0x7fU & (IData)((vlSelfRef.alReadData
                                [0U] >> 0xbU)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum[0U] 
            = (0x3fU & (IData)((vlSelfRef.alReadData
                                [0U] >> 0x19U)));
        vlSelfRef.__PVT__lastCommittedPC = (0xfffffU 
                                            & (IData)(
                                                      (vlSelfRef.alReadData
                                                       [0U] 
                                                       >> 0x1fU)));
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__commitNum 
            = vlSelfRef.__PVT__commitNum;
        vlSelfRef.__PVT__fflagsWE = 0U;
        vlSelfRef.__PVT__fflagsData = vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflags;
        vlSelfRef.__PVT__fflagsWE = 1U;
        vlSelfRef.__PVT__fflagsData = ((IData)(vlSelfRef.__PVT__fflagsData) 
                                       | vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fflagsData
                                       [0U]);
    } else {
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueue = 0U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueueEntryNum 
            = vlSelfRef.__PVT__commitLoadNum;
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStore = 0U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStoreNum 
            = vlSelfRef.__PVT__commitStoreNum;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage 
            = vlSelfRef.__PVT__toRecoveryPhase;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromCommitStage 
            = vlSelfRef.__PVT__refetchType;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryOpIndex 
            = vlSelfRef.__PVT__recoveryOpIndex;
        vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage 
            = vlSelfRef.__PVT__recoveryCause;
        vlSelfRef.__PVT__lastCommittedPC = vlSelfRef.__PVT__prevLastCommittedPC;
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg 
            = (2U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum[0U] = 0U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum[0U] = 0U;
        vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__commitNum 
            = vlSelfRef.__PVT__commitNum;
        vlSelfRef.__PVT__fflagsWE = 0U;
        vlSelfRef.__PVT__fflagsData = vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflags;
    }
    if (vlSelfRef.commit[1U]) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg 
            = ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg)) 
               | (2U & ((IData)((vlSelfRef.alReadData
                                 [1U] >> 0x18U)) << 1U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum[1U] 
            = (0x7fU & (IData)((vlSelfRef.alReadData
                                [1U] >> 0xbU)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum[1U] 
            = (0x3fU & (IData)((vlSelfRef.alReadData
                                [1U] >> 0x19U)));
        vlSelfRef.__PVT__lastCommittedPC = (0xfffffU 
                                            & (IData)(
                                                      (vlSelfRef.alReadData
                                                       [1U] 
                                                       >> 0x1fU)));
        vlSelfRef.__PVT__fflagsWE = 1U;
        vlSelfRef.__PVT__fflagsData = ((IData)(vlSelfRef.__PVT__fflagsData) 
                                       | vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fflagsData
                                       [1U]);
    } else {
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg 
            = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg));
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum[1U] = 0U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum[1U] = 0U;
    }
    vlSelfRef.__PVT__unnamedblk5__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsWE 
        = vlSelfRef.__PVT__fflagsWE;
    vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsData 
        = vlSelfRef.__PVT__fflagsData;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[0U] 
        = ((0x1fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [0U]) | (vlSelfRef.commit[0U] << 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[0U] 
        = ((0x2fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [0U]) | (((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__flushNum)) 
                      & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__renameLogicRecoveryRMT)) 
                     << 0x14U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[0U] 
        = ((0x3000ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [0U]) | (0xfff00U & ((IData)((vlSelfRef.alReadData
                                          [0U] >> 0x33U)) 
                                 << 8U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[0U] 
        = ((0x3fff7fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg
                     [0U] << 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[0U] 
        = ((0x3fff80U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
           [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__toRecoveryPhase 
        = vlSelfRef.__PVT__toRecoveryPhase;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[1U] 
        = ((0x1fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [1U]) | (vlSelfRef.commit[1U] << 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[1U] 
        = ((0x2fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [1U]) | (((1U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__flushNum)) 
                      & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__renameLogicRecoveryRMT)) 
                     << 0x14U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[1U] 
        = ((0x3000ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [1U]) | (0xfff00U & ((IData)((vlSelfRef.alReadData
                                          [1U] >> 0x33U)) 
                                 << 8U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[1U] 
        = ((0x3fff7fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [1U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg
                     [1U] << 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg[1U] 
        = ((0x3fff80U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
            [1U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
           [1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__lastCommittedPC 
        = vlSelfRef.__PVT__lastCommittedPC;
    vlSelfRef.__PVT__unnamedblk6__DOT__i = 2U;
}
