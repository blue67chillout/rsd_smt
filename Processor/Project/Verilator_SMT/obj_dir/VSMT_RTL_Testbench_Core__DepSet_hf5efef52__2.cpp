// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__11(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__11\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vfunc_IsPhyAddrIO__583__Vfuncout;
    __Vfunc_IsPhyAddrIO__583__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_IsPhyAddrIO__583__phyAddr;
    __Vfunc_IsPhyAddrIO__583__phyAddr = 0;
    IData/*19:0*/ __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__blockAddr;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__blockAddr = 0;
    CData/*0:0*/ __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__wordWE;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__wordWE = 0;
    IData/*31:0*/ __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__unnamedblk2__DOT__i;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__unnamedblk2__DOT__i = 0;
    CData/*0:0*/ __Vfunc_IsPhyAddrUncachable__585__Vfuncout;
    __Vfunc_IsPhyAddrUncachable__585__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_IsPhyAddrUncachable__585__phyAddr;
    __Vfunc_IsPhyAddrUncachable__585__phyAddr = 0;
    IData/*19:0*/ __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__blockAddr;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__blockAddr = 0;
    CData/*0:0*/ __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__wordWE;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__wordWE = 0;
    IData/*31:0*/ __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__unnamedblk2__DOT__i;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__unnamedblk2__DOT__i = 0;
    CData/*31:0*/ __Vtemp_3;
    CData/*31:0*/ __Vtemp_5;
    // Body
    vlSelfRef.__PVT__storeCommitter__DOT__retiredStoreQueuePtr 
        = (0xfU & (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regHead) 
                    + ((1U & (IData)((vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                      >> 0x3cU))) ? 1U
                        : 0U)) + ((1U & (IData)((vlSelfRef.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                                 >> 0x3cU)))
                                   ? 1U : 0U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreQueuePtr 
        = vlSelfRef.__PVT__storeCommitter__DOT__retiredStoreQueuePtr;
    vlSelfRef.__PVT__storeCommitter__DOT__nextUnfinishedStoreNum 
        = vlSelfRef.__PVT__storeCommitter__DOT__unfinishedStoreNum;
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStore) {
        vlSelfRef.__PVT__storeCommitter__DOT__nextUnfinishedStoreNum 
            = (0x1fU & ((IData)(vlSelfRef.__PVT__storeCommitter__DOT__nextUnfinishedStoreNum) 
                        + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStoreNum)));
    }
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__wordWE 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreWordWE;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__blockAddr 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreLSQ_BlockAddr;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__unnamedblk2__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__ret = VL_RAND_RESET_I(22);
    {
        vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__ret 
            = (__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__blockAddr 
               << 2U);
        if (__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__wordWE) {
            vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__Vfuncout 
                = vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__ret;
            goto __Vlabel62;
        }
        __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__unnamedblk2__DOT__i = 1U;
        vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__Vfuncout 
            = vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__ret;
        __Vlabel62: ;
    }
    __Vfunc_IsPhyAddrIO__583__phyAddr = vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__Vfuncout;
    __Vfunc_IsPhyAddrIO__583__Vfuncout = (1U & (__Vfunc_IsPhyAddrIO__583__phyAddr 
                                                >> 0x14U));
    vlSelfRef.__PVT__storeCommitter__DOT__isIO = __Vfunc_IsPhyAddrIO__583__Vfuncout;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__wordWE 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreWordWE;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__blockAddr 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreLSQ_BlockAddr;
    __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__unnamedblk2__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__ret = VL_RAND_RESET_I(22);
    {
        vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__ret 
            = (__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__blockAddr 
               << 2U);
        if (__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__wordWE) {
            vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__Vfuncout 
                = vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__ret;
            goto __Vlabel63;
        }
        __Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__unnamedblk2__DOT__i = 1U;
        vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__Vfuncout 
            = vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__ret;
        __Vlabel63: ;
    }
    __Vfunc_IsPhyAddrUncachable__585__phyAddr = vlSelfRef.__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__Vfuncout;
    __Vfunc_IsPhyAddrUncachable__585__Vfuncout = (1U 
                                                  & (__Vfunc_IsPhyAddrUncachable__585__phyAddr 
                                                     >> 0x15U));
    vlSelfRef.__PVT__storeCommitter__DOT__isUncachable 
        = __Vfunc_IsPhyAddrUncachable__585__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__busyInRecovery 
        = vlSelfRef.__PVT__storeCommitter__DOT__phase;
    if ((0U == (IData)(vlSelfRef.__PVT__storeCommitter__DOT__unfinishedStoreNum))) {
        vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg = 0ULL;
        vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
            = (0xfffffffffffffffULL & vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg);
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteReq = 0U;
    } else {
        vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
            = ((0x1ffffffffffffffcULL & vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg) 
               | (IData)((IData)((((IData)(vlSelfRef.__PVT__storeCommitter__DOT__isIO) 
                                   << 1U) | (IData)(vlSelfRef.__PVT__storeCommitter__DOT__isUncachable)))));
        if ((1U & ((~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreCondEnabled)) 
                   | (IData)(vlSelfRef.__PVT__storeCommitter__DOT__isIO)))) {
            vlSelfRef.__PVT__storeCommitter__DOT__dcWriteReq = 0U;
            vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                = (0x1000000000000000ULL | vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg);
        } else {
            vlSelfRef.__PVT__storeCommitter__DOT__dcWriteReq 
                = (1U & (~ (IData)(vlSelfRef.__PVT__storeCommitter__DOT__stallStoreTagStage)));
            vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                = ((0xfffffffffffffffULL & vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg) 
                   | ((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheGrt
                                      [1U])) << 0x3cU));
        }
        vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
            = ((0x17ffffffffffffffULL & vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg) 
               | ((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreCondEnabled)) 
                  << 0x3bU));
        vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
            = ((0x1800000000000003ULL & vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg) 
               | (((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreData)) 
                   << 0x1bU) | ((QData)((IData)(((vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreLSQ_BlockAddr 
                                                  << 5U) 
                                                 | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreWordWE) 
                                                     << 4U) 
                                                    | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreByteWE))))) 
                                << 2U)));
        if (((~ (IData)(vlSelfRef.__PVT__storeCommitter__DOT__stallStoreTagStage)) 
             & (vlSelfRef.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                >> 0x3cU))) {
            vlSelfRef.__PVT__storeCommitter__DOT__nextUnfinishedStoreNum 
                = (0x1fU & ((IData)(vlSelfRef.__PVT__storeCommitter__DOT__nextUnfinishedStoreNum) 
                            - (IData)(1U)));
        }
    }
    if (vlSelfRef.__PVT__storeCommitter__DOT__stallStoreTagStage) {
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteReq 
            = ((1U & (~ (IData)(vlSelfRef.__PVT__storeCommitter__DOT__headStoreHasAllocatedMSHRPipeReg))) 
               && (1U & (IData)((vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                 >> 0x3bU))));
        vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__587__blockAddr 
            = (0xfffffU & (IData)((vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                   >> 7U)));
        vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__587__Vfuncout 
            = (vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__587__blockAddr 
               << 2U);
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteAddr 
            = vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__587__Vfuncout;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__data 
            = (IData)((vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg 
                       >> 0x1bU));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__line 
            = (((QData)((IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__data)) 
                << 0x20U) | (QData)((IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__data)));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__Vfuncout 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__line;
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteData 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__Vfuncout;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__blockAddr 
            = (0xfffffU & (IData)((vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                   >> 7U)));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__byteWE 
            = (0xfU & (IData)((vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg 
                               >> 2U)));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__wordWE 
            = (1U & (IData)((vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg 
                             >> 6U)));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__we 
            = ((IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__wordWE)
                ? (IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__byteWE)
                : 0U);
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__ret 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__we;
        __Vtemp_3 = (0xffU & VL_SHIFTL_III(8,8,32, (IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__ret), 
                                           VL_MULS_III(32, (IData)(4U), 
                                                       ([&]() {
                            vlSelfRef.__Vfunc_LSQ_SelectBits__590__width = 1U;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__590__offset = 0U;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__590__data 
                                = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__blockAddr;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__590__ret = 0U;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i = 0U;
                            while (VL_LTS_III(32, vlSelfRef.__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i, vlSelfRef.__Vfunc_LSQ_SelectBits__590__width)) {
                                vlSelfRef.__Vfunc_LSQ_SelectBits__590__ret 
                                    = (((~ ((IData)(1U) 
                                            << (0x1fU 
                                                & vlSelfRef.__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i))) 
                                        & vlSelfRef.__Vfunc_LSQ_SelectBits__590__ret) 
                                       | (0xffffffffULL 
                                          & ((1U & 
                                              (vlSelfRef.__Vfunc_LSQ_SelectBits__590__data 
                                               >> (0x1fU 
                                                   & (vlSelfRef.__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i 
                                                      + vlSelfRef.__Vfunc_LSQ_SelectBits__590__offset)))) 
                                             << (0x1fU 
                                                 & vlSelfRef.__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i))));
                                vlSelfRef.__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i 
                                    = ((IData)(1U) 
                                       + vlSelfRef.__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i);
                            }
                            vlSelfRef.__Vfunc_LSQ_SelectBits__590__Vfuncout 
                                = vlSelfRef.__Vfunc_LSQ_SelectBits__590__ret;
                        }(), vlSelfRef.__Vfunc_LSQ_SelectBits__590__Vfuncout))));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__ret 
            = __Vtemp_3;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__Vfuncout 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__ret;
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteByteWE 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__Vfuncout;
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteUncachable 
            = (1U & (IData)(vlSelfRef.__PVT__storeCommitter__DOT__tagStagePipeReg));
    } else {
        vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__591__blockAddr 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreLSQ_BlockAddr;
        vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__591__Vfuncout 
            = (vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__591__blockAddr 
               << 2U);
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteAddr 
            = vlSelfRef.__Vfunc_LSQ_ToFullAddrFromBlockAddr__591__Vfuncout;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__data 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreData;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__line 
            = (((QData)((IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__data)) 
                << 0x20U) | (QData)((IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__data)));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__Vfuncout 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__line;
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteData 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__Vfuncout;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__blockAddr 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreLSQ_BlockAddr;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__byteWE 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreByteWE;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__wordWE 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreWordWE;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__we 
            = ((IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__wordWE)
                ? (IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__byteWE)
                : 0U);
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__ret 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__we;
        __Vtemp_5 = (0xffU & VL_SHIFTL_III(8,8,32, (IData)(vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__ret), 
                                           VL_MULS_III(32, (IData)(4U), 
                                                       ([&]() {
                            vlSelfRef.__Vfunc_LSQ_SelectBits__594__width = 1U;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__594__offset = 0U;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__594__data 
                                = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__blockAddr;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__594__ret = 0U;
                            vlSelfRef.__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i = 0U;
                            while (VL_LTS_III(32, vlSelfRef.__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i, vlSelfRef.__Vfunc_LSQ_SelectBits__594__width)) {
                                vlSelfRef.__Vfunc_LSQ_SelectBits__594__ret 
                                    = (((~ ((IData)(1U) 
                                            << (0x1fU 
                                                & vlSelfRef.__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i))) 
                                        & vlSelfRef.__Vfunc_LSQ_SelectBits__594__ret) 
                                       | (0xffffffffULL 
                                          & ((1U & 
                                              (vlSelfRef.__Vfunc_LSQ_SelectBits__594__data 
                                               >> (0x1fU 
                                                   & (vlSelfRef.__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i 
                                                      + vlSelfRef.__Vfunc_LSQ_SelectBits__594__offset)))) 
                                             << (0x1fU 
                                                 & vlSelfRef.__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i))));
                                vlSelfRef.__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i 
                                    = ((IData)(1U) 
                                       + vlSelfRef.__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i);
                            }
                            vlSelfRef.__Vfunc_LSQ_SelectBits__594__Vfuncout 
                                = vlSelfRef.__Vfunc_LSQ_SelectBits__594__ret;
                        }(), vlSelfRef.__Vfunc_LSQ_SelectBits__594__Vfuncout))));
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__ret 
            = __Vtemp_5;
        vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__Vfuncout 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__ret;
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteByteWE 
            = vlSelfRef.__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__Vfuncout;
        vlSelfRef.__PVT__storeCommitter__DOT__dcWriteUncachable 
            = vlSelfRef.__PVT__storeCommitter__DOT__isUncachable;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteReq 
        = vlSelfRef.__PVT__storeCommitter__DOT__dcWriteReq;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteData 
        = vlSelfRef.__PVT__storeCommitter__DOT__dcWriteData;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteByteWE 
        = vlSelfRef.__PVT__storeCommitter__DOT__dcWriteByteWE;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteAddr 
        = vlSelfRef.__PVT__storeCommitter__DOT__dcWriteAddr;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteUncachable 
        = vlSelfRef.__PVT__storeCommitter__DOT__dcWriteUncachable;
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatable 
        = (((0xdU >= (IData)(vlSelfRef.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regCount)) 
            & (0xdU >= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount))) 
           & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__busyInRecovery)));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__12(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__12\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*7:0*/ __Vfunc_ToIndexPartFromFullAddr__597__Vfuncout;
    __Vfunc_ToIndexPartFromFullAddr__597__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_ToIndexPartFromFullAddr__597__addr;
    __Vfunc_ToIndexPartFromFullAddr__597__addr = 0;
    SData/*10:0*/ __Vfunc_ToTagPartFromFullAddr__598__Vfuncout;
    __Vfunc_ToTagPartFromFullAddr__598__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_ToTagPartFromFullAddr__598__addr;
    __Vfunc_ToTagPartFromFullAddr__598__addr = 0;
    CData/*7:0*/ __Vfunc_ToIndexPartFromFullAddr__599__Vfuncout;
    __Vfunc_ToIndexPartFromFullAddr__599__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_ToIndexPartFromFullAddr__599__addr;
    __Vfunc_ToIndexPartFromFullAddr__599__addr = 0;
    SData/*10:0*/ __Vfunc_ToTagPartFromFullAddr__600__Vfuncout;
    __Vfunc_ToTagPartFromFullAddr__600__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_ToTagPartFromFullAddr__600__addr;
    __Vfunc_ToTagPartFromFullAddr__600__addr = 0;
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][2U] 
        = (0xfbffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][2U]);
    __Vfunc_ToIndexPartFromFullAddr__597__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadAddr
        [0U];
    __Vfunc_ToIndexPartFromFullAddr__597__Vfuncout 
        = (0xffU & (__Vfunc_ToIndexPartFromFullAddr__597__addr 
                    >> 3U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][2U] 
        = ((0x7ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
            [0U][2U]) | ((IData)(__Vfunc_ToIndexPartFromFullAddr__597__Vfuncout) 
                         << 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][3U] 
        = (7U & ((IData)(__Vfunc_ToIndexPartFromFullAddr__597__Vfuncout) 
                 >> 5U));
    __Vfunc_ToTagPartFromFullAddr__598__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadAddr
        [0U];
    __Vfunc_ToTagPartFromFullAddr__598__Vfuncout = 
        (0x7ffU & (__Vfunc_ToTagPartFromFullAddr__598__addr 
                   >> 0xbU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][2U] 
        = ((0xfc007fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
            [0U][2U]) | ((IData)(__Vfunc_ToTagPartFromFullAddr__598__Vfuncout) 
                         << 0xfU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][2U] 
        = (0x4000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][0U] 
        = (0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][1U] = 0U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][2U] 
        = (0xfffff000U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][0U] 
        = (0xfffff00fU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][2U] 
        = (0xffffefffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][2U] 
        = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][0U] 
        = (0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][0U] 
        = (0xfffffffbU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[0U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [0U][0U]);
    vlSelfRef.__PVT__dCache__DOT__storedLineData = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteData;
    vlSelfRef.__PVT__dCache__DOT__storedLineByteWE 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteByteWE;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][2U] 
        = (0xfbffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][2U]);
    __Vfunc_ToIndexPartFromFullAddr__599__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteAddr;
    __Vfunc_ToIndexPartFromFullAddr__599__Vfuncout 
        = (0xffU & (__Vfunc_ToIndexPartFromFullAddr__599__addr 
                    >> 3U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][2U] 
        = ((0x7ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
            [1U][2U]) | ((IData)(__Vfunc_ToIndexPartFromFullAddr__599__Vfuncout) 
                         << 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][3U] 
        = (7U & ((IData)(__Vfunc_ToIndexPartFromFullAddr__599__Vfuncout) 
                 >> 5U));
    __Vfunc_ToTagPartFromFullAddr__600__addr = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteAddr;
    __Vfunc_ToTagPartFromFullAddr__600__Vfuncout = 
        (0x7ffU & (__Vfunc_ToTagPartFromFullAddr__600__addr 
                   >> 0xbU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][2U] 
        = ((0xfc007fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
            [1U][2U]) | ((IData)(__Vfunc_ToTagPartFromFullAddr__600__Vfuncout) 
                         << 0xfU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][2U] 
        = (0x4000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][0U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
            [1U][0U]) | ((IData)(vlSelfRef.__PVT__dCache__DOT__storedLineData) 
                         << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][1U] 
        = (((IData)(vlSelfRef.__PVT__dCache__DOT__storedLineData) 
            >> 0x14U) | ((IData)((vlSelfRef.__PVT__dCache__DOT__storedLineData 
                                  >> 0x20U)) << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][2U] 
        = ((0xfffff000U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
            [1U][2U]) | ((IData)((vlSelfRef.__PVT__dCache__DOT__storedLineData 
                                  >> 0x20U)) >> 0x14U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][0U] 
        = ((0xfffff00fU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
            [1U][0U]) | ((IData)(vlSelfRef.__PVT__dCache__DOT__storedLineByteWE) 
                         << 4U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][2U] 
        = (0xffffefffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][2U] 
        = (0x2000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][0U] 
        = (8U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][0U] 
        = (0xfffffffbU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn[1U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
           [1U][0U]);
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__13(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__13\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__608__detectRange;
    __Vfunc_SelectiveFlushDetector__608__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__608__headPtr;
    __Vfunc_SelectiveFlushDetector__608__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__608__tailPtr;
    __Vfunc_SelectiveFlushDetector__608__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__608__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__608__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__608__opPtr;
    __Vfunc_SelectiveFlushDetector__608__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__609__detectRange;
    __Vfunc_SelectiveFlushDetector__609__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__609__headPtr;
    __Vfunc_SelectiveFlushDetector__609__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__609__tailPtr;
    __Vfunc_SelectiveFlushDetector__609__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__609__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__609__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__609__opPtr;
    __Vfunc_SelectiveFlushDetector__609__opPtr = 0;
    CData/*7:0*/ __Vfunc_ToIndexPartFromFullAddr__610__Vfuncout;
    __Vfunc_ToIndexPartFromFullAddr__610__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_ToIndexPartFromFullAddr__610__addr;
    __Vfunc_ToIndexPartFromFullAddr__610__addr = 0;
    SData/*10:0*/ __Vfunc_ToTagPartFromFullAddr__611__Vfuncout;
    __Vfunc_ToTagPartFromFullAddr__611__Vfuncout = 0;
    IData/*21:0*/ __Vfunc_ToTagPartFromFullAddr__611__addr;
    __Vfunc_ToTagPartFromFullAddr__611__addr = 0;
    CData/*31:0*/ __Vtemp_1;
    CData/*31:0*/ __Vtemp_2;
    CData/*31:0*/ __Vtemp_3;
    CData/*31:0*/ __Vtemp_4;
    // Body
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isAllocatedByStore
        [0U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isAllocatedByStore
        [1U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__unnamedblk4__DOT__i = 2U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [0U][0U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [0U][1U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [0U][2U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [0U][3U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [0U][4U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [0U][5U];
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCanBeInvalidDirect
        [0U]) {
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0x40000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
    }
    __Vfunc_SelectiveFlushDetector__608__opPtr = vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
        [0U];
    __Vfunc_SelectiveFlushDetector__608__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__608__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__608__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__608__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__608__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__608__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                goto __Vlabel64;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__608__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                    goto __Vlabel64;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
                    goto __Vlabel64;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__608__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                    goto __Vlabel64;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                    goto __Vlabel64;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
                    goto __Vlabel64;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
                goto __Vlabel64;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
        }
        __Vlabel64: ;
    }
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation[0U] 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout;
    __Vfunc_SelectiveFlushDetector__609__opPtr = (0x3fU 
                                                  & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [0U][0U]);
    __Vfunc_SelectiveFlushDetector__609__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__609__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__609__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__609__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__609__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__609__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                goto __Vlabel65;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__609__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                    goto __Vlabel65;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
                    goto __Vlabel65;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__609__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                    goto __Vlabel65;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                    goto __Vlabel65;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
                    goto __Vlabel65;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
                goto __Vlabel65;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
        }
        __Vlabel65: ;
    }
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Entry[0U] 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout;
    if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Entry
         [0U] & (~ (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [0U][0U] >> 0x10U)))) {
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0x20000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0x40000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
    }
    __Vfunc_ToIndexPartFromFullAddr__610__addr = (0x3fffffU 
                                                  & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [0U][3U] 
                                                      << 0xaU) 
                                                     | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                        [0U][2U] 
                                                        >> 0x16U)));
    __Vfunc_ToIndexPartFromFullAddr__610__Vfuncout 
        = (0xffU & (__Vfunc_ToIndexPartFromFullAddr__610__addr 
                    >> 3U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
        = ((0x7ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [0U][2U]) | ((IData)(__Vfunc_ToIndexPartFromFullAddr__610__Vfuncout) 
                         << 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][3U] 
        = (7U & ((IData)(__Vfunc_ToIndexPartFromFullAddr__610__Vfuncout) 
                 >> 5U));
    __Vfunc_ToTagPartFromFullAddr__611__addr = (0x3fffffU 
                                                & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [0U][3U] 
                                                    << 0xaU) 
                                                   | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [0U][2U] 
                                                      >> 0x16U)));
    __Vfunc_ToTagPartFromFullAddr__611__Vfuncout = 
        (0x7ffU & (__Vfunc_ToTagPartFromFullAddr__611__addr 
                   >> 0xbU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
        = ((0xfc007fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [0U][2U]) | ((IData)(__Vfunc_ToTagPartFromFullAddr__611__Vfuncout) 
                         << 0xfU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [0U][0U]) | ((IData)((((QData)((IData)(
                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [0U][2U])) 
                                   << 0x2dU) | (((QData)((IData)(
                                                                 vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                 [0U][1U])) 
                                                 << 0xdU) 
                                                | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [0U][0U])) 
                                                   >> 0x13U)))) 
                         << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][1U] 
        = (((IData)((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                      [0U][2U])) << 0x2dU) 
                     | (((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [0U][1U])) 
                         << 0xdU) | ((QData)((IData)(
                                                     vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                     [0U][0U])) 
                                     >> 0x13U)))) >> 0x14U) 
           | ((IData)(((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [0U][2U])) 
                         << 0x2dU) | (((QData)((IData)(
                                                       vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                       [0U][1U])) 
                                       << 0xdU) | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [0U][0U])) 
                                                   >> 0x13U))) 
                       >> 0x20U)) << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
        = ((0xfffff000U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [0U][2U]) | ((IData)(((((QData)((IData)(
                                                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [0U][2U])) 
                                    << 0x2dU) | (((QData)((IData)(
                                                                  vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                  [0U][1U])) 
                                                  << 0xdU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                    [0U][0U])) 
                                                    >> 0x13U))) 
                                  >> 0x20U)) >> 0x14U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
        = (0xff0U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
        = (0x4000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[0U] = 0U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
        = (0xfbffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
        = (0xffffefffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
        = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
        = (0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
        = ((0xfffffffbU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [0U][0U]) | (4U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                               [0U][0U] >> 0xcU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 0U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][0U] 
        = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
            [0U][0U]) | ((IData)((((QData)((IData)(
                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [0U][2U])) 
                                   << 0x2dU) | (((QData)((IData)(
                                                                 vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                 [0U][1U])) 
                                                 << 0xdU) 
                                                | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [0U][0U])) 
                                                   >> 0x13U)))) 
                         << 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][1U] 
        = (((IData)((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                      [0U][2U])) << 0x2dU) 
                     | (((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [0U][1U])) 
                         << 0xdU) | ((QData)((IData)(
                                                     vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                     [0U][0U])) 
                                     >> 0x13U)))) >> 0x1fU) 
           | ((IData)(((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [0U][2U])) 
                         << 0x2dU) | (((QData)((IData)(
                                                       vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                       [0U][1U])) 
                                       << 0xdU) | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [0U][0U])) 
                                                   >> 0x13U))) 
                       >> 0x20U)) << 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][2U] 
        = ((0x7ffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
            [0U][2U]) | (0x7fffffU & ((IData)(((((QData)((IData)(
                                                                 vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                 [0U][2U])) 
                                                 << 0x2dU) 
                                                | (((QData)((IData)(
                                                                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                    [0U][1U])) 
                                                    << 0xdU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                      [0U][0U])) 
                                                      >> 0x13U))) 
                                               >> 0x20U)) 
                                      >> 0x1fU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][2U] 
        = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
            [0U][2U]) | (0x7ffffeU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                      [0U][4U] >> 4U)));
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete[0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine[0U] = 0ULL;
    if ((4U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
         [0U][5U])) {
        if ((2U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [0U][5U])) {
            if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                 [0U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                         [0U]))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [0U] << 0x16U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [0U] >> 0xaU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                                     [0U] << 0x10U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                     [0U] << 0xfU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                      [0U] ? 0xdU : 7U) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                              [0U] ? 0xdU
                                               : 7U) 
                                             >> 2U)));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                       [0U]);
            } else if ((1U == (IData)(vlSelfRef.__PVT__dCache__DOT__controller__DOT__regPhase))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = (0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] = 0U;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0x40000000U | (0x3fffffffU & 
                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                      [0U][4U]));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                       [0U]);
            }
        } else if ((1U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [0U][5U])) {
            if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                 [0U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                         [0U]))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [0U] << 0x16U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [0U] >> 0xaU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                                     [0U] << 0x10U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                     [0U] << 0xfU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                      [0U] ? 0xdU : 7U) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                              [0U] ? 0xdU
                                               : 7U) 
                                             >> 2U)));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                       [0U]);
            } else if ((1U == (IData)(vlSelfRef.__PVT__dCache__DOT__controller__DOT__regPhase))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = (0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                    = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] = 0U;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0x40000000U | (0x3fffffffU & 
                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                      [0U][4U]));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                       [0U]);
            }
        } else if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [0U][4U] >> 0x1fU)) {
            if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [0U][4U])) {
                if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                     [0U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                             [0U]))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][5U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                         [0U] << 0x16U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                        = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                         [0U] >> 0xaU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                        = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][3U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                                         [0U] << 0x10U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                         [0U] << 0xfU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                          [0U] ? 0xdU
                                           : 7U) << 0x1eU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                                  [0U]
                                                   ? 0xdU
                                                   : 7U) 
                                                 >> 2U)));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                           [0U]);
                } else if ((1U == (IData)(vlSelfRef.__PVT__dCache__DOT__controller__DOT__regPhase))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][5U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = (0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                        = (0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][3U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                        = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][3U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] = 0U;
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = (0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0x40000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][5U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                           [0U]);
                }
            } else if ((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                               [0U][0U] >> 0x12U) | 
                              (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                               [0U][0U] >> 0x10U)))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (7U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][5U]);
            }
        } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [0U][4U])) {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[0U] = 1U;
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
                = (0x4000000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                   [0U][2U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
                = (0x1000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                   [0U][2U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
                = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                   [0U][2U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
                = ((0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                    [0U][0U]) | (8U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [0U][0U] >> 0xdU)));
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
                = ((0xfffffffbU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                    [0U][0U]) | (4U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [0U][0U] >> 0xcU)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                  [0U] ? 0x12U : ((0x20000U 
                                                   & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [0U][0U])
                                                   ? 0x12U
                                                   : 0x11U)) 
                                 << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                          [0U] ? 0x12U
                                           : ((0x20000U 
                                               & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [0U][0U])
                                               ? 0x12U
                                               : 0x11U)) 
                                         >> 2U)));
        } else {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 0U;
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                          [0U][3U] 
                                          >> 0xcU) 
                                         & (~ (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                               [1U][0U] 
                                               & ((1U 
                                                   & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [0U][2U] 
                                                      >> 0x13U)) 
                                                  == 
                                                  (1U 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                      [1U][0U] 
                                                      >> 1U)))))))
                                   ? 0x10U : 0x12U) 
                                 << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][5U]) | (0xfU & (((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [0U][3U] 
                                                  >> 0xcU) 
                                                 & (~ 
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                     [1U][0U] 
                                                     & ((1U 
                                                         & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                            [0U][2U] 
                                                            >> 0x13U)) 
                                                        == 
                                                        (1U 
                                                         & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                            [1U][0U] 
                                                            >> 1U)))))))
                                           ? 0x10U : 0x12U) 
                                         >> 2U)));
        }
    } else if ((2U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [0U][5U])) {
        if ((1U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [0U][5U])) {
            if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [0U][4U] >> 0x1fU)) {
                if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                     [0U][4U])) {
                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 1U;
                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][0U] 
                        = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                           [0U][0U]);
                    vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__addr 
                        = (0x3fffffU & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [0U][3U] << 0xaU) 
                                        | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                           [0U][2U] 
                                           >> 0x16U)));
                    vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__Vfuncout 
                        = (0x3ffff8U & vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__addr);
                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][2U] 
                        = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                            [0U][2U]) | (0x7fffffU 
                                         & (vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__Vfuncout 
                                            << 1U)));
                    if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                         [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                 [0U] >> 3U))) {
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                            = ((0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                [0U][2U]) | (0x80000U 
                                             & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                                [0U] 
                                                << 0x13U)));
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                            = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                               [0U][4U]);
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                            = (4U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                     [0U][5U]));
                    } else {
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                            = (0xc0000000U | (0x3fffffffU 
                                              & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                              [0U][4U]));
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                            = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                     [0U][5U]));
                    }
                } else if (((vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                             [4U][2U] >> 4U) & ((3U 
                                                 & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [0U][2U] 
                                                    >> 0x14U)) 
                                                == 
                                                (3U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [4U][0U] 
                                                    >> 2U))))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][0U]) | ((IData)((((QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                   [4U][2U])) 
                                                   << 0x3cU) 
                                                  | (((QData)((IData)(
                                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                      [4U][1U])) 
                                                      << 0x1cU) 
                                                     | ((QData)((IData)(
                                                                        vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                        [4U][0U])) 
                                                        >> 4U)))) 
                                         << 0x13U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] 
                        = (((IData)((((QData)((IData)(
                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                      [4U][2U])) 
                                      << 0x3cU) | (
                                                   ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [4U][1U])) 
                                                    << 0x1cU) 
                                                   | ((QData)((IData)(
                                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                      [4U][0U])) 
                                                      >> 4U)))) 
                            >> 0xdU) | ((IData)(((((QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                   [4U][2U])) 
                                                   << 0x3cU) 
                                                  | (((QData)((IData)(
                                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                      [4U][1U])) 
                                                      << 0x1cU) 
                                                     | ((QData)((IData)(
                                                                        vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                        [4U][0U])) 
                                                        >> 4U))) 
                                                 >> 0x20U)) 
                                        << 0x13U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][2U]) | ((IData)(((
                                                   ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [4U][2U])) 
                                                    << 0x3cU) 
                                                   | (((QData)((IData)(
                                                                       vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                       [4U][1U])) 
                                                       << 0x1cU) 
                                                      | ((QData)((IData)(
                                                                         vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                         [4U][0U])) 
                                                         >> 4U))) 
                                                  >> 0x20U)) 
                                         >> 0xdU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                        = (0x1000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][3U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][4U]) | (((0x10000U 
                                           & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                           [0U][0U])
                                           ? 0xcU : 
                                          ((0x8000U 
                                            & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                            [0U][0U])
                                            ? 0x12U
                                            : 0x11U)) 
                                         << 0x1eU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][5U]) | (0xfU & (((0x10000U 
                                                   & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [0U][0U])
                                                   ? 0xcU
                                                   : 
                                                  ((0x8000U 
                                                    & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [0U][0U])
                                                    ? 0x12U
                                                    : 0x11U)) 
                                                 >> 2U)));
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0x80000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [0U][5U]));
                }
            } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                        [0U][4U])) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 1U;
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][0U] 
                    = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                       [0U][0U]);
                vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__addr 
                    = (0x3fffffU & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                     [0U][3U] << 0xaU) 
                                    | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [0U][2U] >> 0x16U)));
                vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__Vfuncout 
                    = (0x3ffff8U & vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__addr);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][2U] 
                    = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                        [0U][2U]) | (0x7fffffU & (vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__Vfuncout 
                                                  << 1U)));
                if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                     [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                             [0U] >> 3U))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = ((0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][2U]) | (0x300000U 
                                         & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                            [0U] << 0x13U)));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0x80000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [0U][5U]));
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0x40000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [0U][5U]));
                }
            } else {
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty 
                    = vlSelfRef.__PVT__dCache__DOT__storedLineByteWE;
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                    = vlSelfRef.__PVT__dCache__DOT__storedLineData;
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                    = (((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                        [0U][2U])) 
                        << 0x2dU) | (((QData)((IData)(
                                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [0U][1U])) 
                                      << 0xdU) | ((QData)((IData)(
                                                                  vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                  [0U][0U])) 
                                                  >> 0x13U)));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffffffcULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | (IData)((IData)(((2U & (((1U 
                                                   & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                   ? (IData)(
                                                             (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                              >> 1U))
                                                   : (IData)(
                                                             (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                              >> 1U))) 
                                                 << 1U)) 
                                          | (1U & (
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine)
                                                    : (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine)))))));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffffff3ULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 3U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 3U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((1U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 2U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 2U))))))) 
                          << 2U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffffffcfULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 5U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 5U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((1U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 4U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 4U))))))) 
                          << 4U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffffff3fULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 7U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 7U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((1U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 6U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 6U))))))) 
                          << 6U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffffcffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 9U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 9U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 8U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 8U))))))) 
                          << 8U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffff3ffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0xbU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0xbU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0xaU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0xaU))))))) 
                          << 0xaU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffffcfffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0xdU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0xdU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0xcU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0xcU))))))) 
                          << 0xcU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffff3fffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0xfU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0xfU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0xeU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0xeU))))))) 
                          << 0xeU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffcffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x11U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x11U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x10U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x10U))))))) 
                          << 0x10U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffff3ffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x13U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x13U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x12U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x12U))))))) 
                          << 0x12U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffcfffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x15U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x15U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x14U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x14U))))))) 
                          << 0x14U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffff3fffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x17U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x17U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x16U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x16U))))))) 
                          << 0x16U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffcffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x19U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x19U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x18U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x18U))))))) 
                          << 0x18U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffff3ffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x1bU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x1bU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x1aU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x1aU))))))) 
                          << 0x1aU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffcfffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x1dU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x1dU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x1cU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x1cU))))))) 
                          << 0x1cU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffff3fffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x1fU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x1fU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x1eU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x1eU))))))) 
                          << 0x1eU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffcffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x21U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x21U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x20U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x20U))))))) 
                          << 0x20U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffff3ffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x23U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x23U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x22U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x22U))))))) 
                          << 0x22U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffcfffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x25U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x25U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x24U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x24U))))))) 
                          << 0x24U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffff3fffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x27U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x27U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x26U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x26U))))))) 
                          << 0x26U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffcffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x29U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x29U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x28U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x28U))))))) 
                          << 0x28U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffff3ffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x2bU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x2bU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x2aU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x2aU))))))) 
                          << 0x2aU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffcfffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x2dU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x2dU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x2cU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x2cU))))))) 
                          << 0x2cU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffff3fffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x2fU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x2fU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x2eU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x2eU))))))) 
                          << 0x2eU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffcffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x31U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x31U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x30U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x30U))))))) 
                          << 0x30U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfff3ffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x33U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x33U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x32U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x32U))))))) 
                          << 0x32U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffcfffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x35U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x35U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x34U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x34U))))))) 
                          << 0x34U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xff3fffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x37U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x37U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x36U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x36U))))))) 
                          << 0x36U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfcffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x39U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x39U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x38U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x38U))))))) 
                          << 0x38U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xf3ffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x3bU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x3bU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x3aU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x3aU))))))) 
                          << 0x3aU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xcfffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x3dU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x3dU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x3cU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x3cU))))))) 
                          << 0x3cU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0x3fffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x3fU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x3fU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x3eU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x3eU))))))) 
                          << 0x3eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine[0U] 
                    = vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][0U]) | ((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                             [0U]) 
                                     << 0x13U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] 
                    = (((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                [0U]) >> 0xdU) | ((IData)(
                                                          (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                                           [0U] 
                                                           >> 0x20U)) 
                                                  << 0x13U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][2U]) | ((IData)((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                              [0U] 
                                              >> 0x20U)) 
                                     >> 0xdU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][4U]) | (((0x8000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [0U][0U]) ? 0xfU
                                       : 0x11U) << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][5U]) | (0xfU & (((0x8000U 
                                               & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [0U][0U])
                                               ? 0xfU
                                               : 0x11U) 
                                             >> 2U)));
            }
        } else if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [0U][4U] >> 0x1fU)) {
            if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [0U][4U])) {
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 0U;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][4U]) | (((1U & (((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [0U][4U] 
                                               >> 0x1cU) 
                                              & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                 [0U][4U] 
                                                 >> 0x1dU)) 
                                             & (~ (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                   [1U][0U] 
                                                   & ((1U 
                                                       & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                          [0U][2U] 
                                                          >> 0x13U)) 
                                                      == 
                                                      (1U 
                                                       & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                          [1U][0U] 
                                                          >> 1U)))))))
                                       ? 0xbU : 0xdU) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][5U]) | (0xfU & (((1U & 
                                               (((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [0U][4U] 
                                                  >> 0x1cU) 
                                                 & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [0U][4U] 
                                                    >> 0x1dU)) 
                                                & (~ 
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [1U][0U] 
                                                    & ((1U 
                                                        & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                           [0U][2U] 
                                                           >> 0x13U)) 
                                                       == 
                                                       (1U 
                                                        & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                           [1U][0U] 
                                                           >> 1U)))))))
                                               ? 0xbU
                                               : 0xdU) 
                                             >> 2U)));
            } else {
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 1U;
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][0U] 
                    = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                       [0U][0U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][2U] 
                    = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                        [0U][2U]) | (0x7ffffeU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [0U][4U] 
                                                  >> 4U)));
                if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                     [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                             [0U] >> 3U))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                        = ((0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][2U]) | (0x80000U & 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                          [0U] << 0x13U)));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0xc0000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (2U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [0U][5U]));
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0x80000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (2U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [0U][5U]));
                }
            }
        } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [0U][4U])) {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                = ((0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][0U]) | (0x4000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                            [0U][0U] 
                                            << 0xeU)));
            if (((0xa3U >= ((IData)(0x6dU) + (1U & 
                                              (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                               [0U][0U] 
                                               >> 0xeU)))) 
                 && (1U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                           [0U][(((IData)(0x6dU) + 
                                  (1U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                         [0U][0U] >> 0xeU))) 
                                 >> 5U)] >> (0x1fU 
                                             & ((IData)(0x6dU) 
                                                + (1U 
                                                   & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                      [0U][0U] 
                                                      >> 0xeU)))))))) {
                vlSelfRef.__Vfunc_BuildFullAddr__615__tag 
                    = ((0xa3U >= ((IData)(0x6fU) + 
                                  (0x1fU & ((IData)(0xbU) 
                                            * (1U & 
                                               (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                [0U][0U] 
                                                >> 0xeU))))))
                        ? (0x7ffU & (((0U == (0x1fU 
                                              & ((IData)(0x6fU) 
                                                 + 
                                                 (0x1fU 
                                                  & ((IData)(0xbU) 
                                                     * 
                                                     (1U 
                                                      & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                         [0U][0U] 
                                                         >> 0xeU)))))))
                                       ? 0U : (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [0U][
                                               (((IData)(0x79U) 
                                                 + 
                                                 (0x1fU 
                                                  & ((IData)(0xbU) 
                                                     * 
                                                     (1U 
                                                      & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                         [0U][0U] 
                                                         >> 0xeU))))) 
                                                >> 5U)] 
                                               << ((IData)(0x20U) 
                                                   - 
                                                   (0x1fU 
                                                    & ((IData)(0x6fU) 
                                                       + 
                                                       (0x1fU 
                                                        & ((IData)(0xbU) 
                                                           * 
                                                           (1U 
                                                            & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                               [0U][0U] 
                                                               >> 0xeU))))))))) 
                                     | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                        [0U][(((IData)(0x6fU) 
                                               + (0x1fU 
                                                  & ((IData)(0xbU) 
                                                     * 
                                                     (1U 
                                                      & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                         [0U][0U] 
                                                         >> 0xeU))))) 
                                              >> 5U)] 
                                        >> (0x1fU & 
                                            ((IData)(0x6fU) 
                                             + (0x1fU 
                                                & ((IData)(0xbU) 
                                                   * 
                                                   (1U 
                                                    & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                       [0U][0U] 
                                                       >> 0xeU)))))))))
                        : 0U);
                vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__addr 
                    = (0x3fffffU & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                     [0U][3U] << 0xaU) 
                                    | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [0U][2U] >> 0x16U)));
                vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__Vfuncout 
                    = (0xffU & (vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__addr 
                                >> 3U));
                vlSelfRef.__Vfunc_BuildFullAddr__615__index 
                    = vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__Vfuncout;
                vlSelfRef.__Vfunc_BuildFullAddr__615__Vfuncout 
                    = (((IData)(vlSelfRef.__Vfunc_BuildFullAddr__615__tag) 
                        << 0xbU) | ((IData)(vlSelfRef.__Vfunc_BuildFullAddr__615__index) 
                                    << 3U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = ((0xf800001fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][4U]) | (vlSelfRef.__Vfunc_BuildFullAddr__615__Vfuncout 
                                     << 5U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0x10000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
            } else {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
            }
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = (0x8000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [0U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][0U]) | ((IData)((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [0U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [0U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [0U][0U])) 
                                                >> 2U)))) 
                                 << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] 
                = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                              [0U][2U])) 
                              << 0x3eU) | (((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [0U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [0U][0U])) 
                                              >> 2U)))) 
                    >> 0xdU) | ((IData)(((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [0U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [0U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [0U][0U])) 
                                                >> 2U))) 
                                         >> 0x20U)) 
                                << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][2U]) | ((IData)(((((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [0U][2U])) 
                                            << 0x3eU) 
                                           | (((QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                               [0U][1U])) 
                                               << 0x1eU) 
                                              | ((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                 [0U][0U])) 
                                                 >> 2U))) 
                                          >> 0x20U)) 
                                 >> 0xdU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (0x20000000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                [0U][0U] 
                                                << 0x1cU)));
            __Vtemp_1 = ((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [0U][4U] >> 0x1cU) 
                                & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                   [0U][4U] >> 0x1dU)))
                          ? 0xaU : 0xdU);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U] 
                                          >> 0x1cU) 
                                         & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                            [0U][4U] 
                                            >> 0x1dU)))
                                   ? 0xaU : 0xdU) << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][5U]) | (0xfU & (__Vtemp_1 
                                         >> 2U)));
        } else {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                = ((0x7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][3U]) | (0xffff8000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                                [0U][2U] 
                                                << 8U)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0xffffffe0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                          [0U][2U] 
                                          >> 0x18U)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
                = ((0xffff9fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][3U]) | (0x6000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                            [0U][2U] 
                                            << 8U)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = (0x40000000U | (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                  [0U][4U]));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = (2U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                         [0U][5U]));
        }
    } else if ((1U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [0U][5U])) {
        if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [0U][4U] >> 0x1fU)) {
            if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [0U][4U])) {
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[0U] = 1U;
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
                    = (0xfbffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [0U][2U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
                    = (0xffffefffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [0U][2U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
                    = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [0U][2U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
                    = (0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [0U][0U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
                    = (2U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                      [0U] ? 8U : (
                                                   (0x20000U 
                                                    & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [0U][0U])
                                                    ? 0x12U
                                                    : 7U)) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                              [0U] ? 8U
                                               : ((0x20000U 
                                                   & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [0U][0U])
                                                   ? 0x12U
                                                   : 7U)) 
                                             >> 2U)));
            } else {
                if ((0xffU == (0xffU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                        [0U][0U] >> 6U)))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete[0U] = 1U;
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                        = ((0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [0U][0U]) | (0x3fc0U & 
                                         (((IData)(1U) 
                                           + ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [0U][0U] 
                                               << 0x1aU) 
                                              | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                 [0U][0U] 
                                                 >> 6U))) 
                                          << 6U)));
                }
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                    = (0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] = 0U;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = (0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][2U]);
                if (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete
                    [0U]) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][5U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (7U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][5U]);
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                        = (0x40000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [0U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                        = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [0U][5U]);
                }
            }
        } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [0U][4U])) {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 0U;
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (((1U & (((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                           [0U][4U] 
                                           >> 0x1cU) 
                                          & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                             [0U][4U] 
                                             >> 0x1dU)) 
                                         & (~ (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                               [1U][0U] 
                                               & ((1U 
                                                   & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [0U][2U] 
                                                      >> 0x13U)) 
                                                  == 
                                                  (1U 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                      [1U][0U] 
                                                      >> 1U)))))))
                                   ? 5U : 6U) << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][5U]) | (0xfU & (((1U & (((
                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [0U][4U] 
                                                   >> 0x1cU) 
                                                  & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                     [0U][4U] 
                                                     >> 0x1dU)) 
                                                 & (~ 
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                     [1U][0U] 
                                                     & ((1U 
                                                         & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                            [0U][2U] 
                                                            >> 0x13U)) 
                                                        == 
                                                        (1U 
                                                         & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                            [1U][0U] 
                                                            >> 1U)))))))
                                           ? 5U : 6U) 
                                         >> 2U)));
        } else {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0U] = 1U;
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][0U] 
                = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                   [0U][0U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[0U][2U] 
                = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                    [0U][2U]) | (0x7ffffeU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                              [0U][4U] 
                                              >> 4U)));
            if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                 [0U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                         [0U] >> 3U))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                    = ((0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [0U][2U]) | (0x80000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                                 [0U] 
                                                 << 0x13U)));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0x40000000U | (0x3fffffffU & 
                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                      [0U][4U]));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (1U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                             [0U][5U]));
            } else {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                    = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [0U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                    = (1U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                             [0U][5U]));
            }
        }
    } else if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [0U][4U] >> 0x1fU)) {
        if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [0U][4U])) {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = (0x8000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [0U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
                = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][0U]) | ((IData)((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [0U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [0U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [0U][0U])) 
                                                >> 2U)))) 
                                 << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] 
                = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                              [0U][2U])) 
                              << 0x3eU) | (((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [0U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [0U][0U])) 
                                              >> 2U)))) 
                    >> 0xdU) | ((IData)(((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [0U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [0U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [0U][0U])) 
                                                >> 2U))) 
                                         >> 0x20U)) 
                                << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
                = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][2U]) | ((IData)(((((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [0U][2U])) 
                                            << 0x3eU) 
                                           | (((QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                               [0U][1U])) 
                                               << 0x1eU) 
                                              | ((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                 [0U][0U])) 
                                                 >> 2U))) 
                                          >> 0x20U)) 
                                 >> 0xdU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (0x20000000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                [0U][0U] 
                                                << 0x1cU)));
            __Vtemp_2 = ((0x20000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                          [0U][4U]) ? 4U : 6U);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (((0x20000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                   [0U][4U]) ? 4U : 6U) 
                                 << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][5U]) | (0xfU & (__Vtemp_2 
                                         >> 2U)));
        } else if ((0U != (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                 [0U][2U] >> 5U)))) {
            vlSelfRef.__Vfunc_BuildFullAddr__617__tag 
                = (0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                             [0U][2U] >> 7U));
            vlSelfRef.__Vfunc_BuildFullAddr__617__index 
                = (0xffU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                            [0U][0U] >> 6U));
            vlSelfRef.__Vfunc_BuildFullAddr__617__Vfuncout 
                = (((IData)(vlSelfRef.__Vfunc_BuildFullAddr__617__tag) 
                    << 0xbU) | ((IData)(vlSelfRef.__Vfunc_BuildFullAddr__617__index) 
                                << 3U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = ((0xf800001fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [0U][4U]) | (vlSelfRef.__Vfunc_BuildFullAddr__617__Vfuncout 
                                 << 5U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = (0x10000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [0U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = (0xc0000000U | (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                  [0U][4U]));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [0U][5U]);
        } else {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [0U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
                = (0x80000000U | (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                  [0U][4U]));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
                = (1U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                         [0U][5U]));
        }
    } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [0U][4U])) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[0U] = 1U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
            = ((0x7ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                [0U][2U]) | (0xf8000000U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                            [0U][0U] 
                                            << 0x15U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][3U] 
            = (7U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                     [0U][0U] >> 0xbU));
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
            = (0xffffbfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [0U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
            = (0x4000000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [0U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
            = (0x1000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [0U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][2U] 
            = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [0U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
            = (0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [0U][0U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[0U][0U] 
            = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                              [0U] ? 2U : 1U) << 0x1eU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
            = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                      [0U] ? 2U : 1U) 
                                     >> 2U)));
    } else if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                [0U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                        [0U]))) {
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
            = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][5U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
            = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                             [0U] << 0x16U));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
            = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                             [0U] >> 0xaU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
            = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][3U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
            = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
            = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                             [0U] << 0x10U));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                             [0U] << 0xfU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                              [0U] ? 0xdU : 7U) << 0x1eU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
            = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                      [0U] ? 0xdU : 7U) 
                                     >> 2U)));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
               [0U]);
    } else if ((1U == (IData)(vlSelfRef.__PVT__dCache__DOT__controller__DOT__regPhase))) {
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
            = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][5U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
            = (0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
            = (0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][3U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][3U] 
            = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][3U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
            = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
            = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = (0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][1U] = 0U;
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][2U] 
            = (0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][4U] 
            = (0x40000000U | (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                              [0U][4U]));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][5U] 
            = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [0U][5U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[0U][0U] 
            = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
               [0U]);
    }
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [1U][0U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][1U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [1U][1U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [1U][2U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [1U][3U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [1U][4U];
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
        = vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
        [1U][5U];
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCanBeInvalidDirect
        [1U]) {
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = (0x40000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][0U]);
    }
    __Vfunc_SelectiveFlushDetector__608__opPtr = vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
        [1U];
    __Vfunc_SelectiveFlushDetector__608__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__608__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__608__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__608__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__608__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__608__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                goto __Vlabel66;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__608__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                    goto __Vlabel66;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
                    goto __Vlabel66;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__608__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                    goto __Vlabel66;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__608__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__608__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__608__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 1U;
                    goto __Vlabel66;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
                    goto __Vlabel66;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
                goto __Vlabel66;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout = 0U;
        }
        __Vlabel66: ;
    }
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation[1U] 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__608__Vfuncout;
    __Vfunc_SelectiveFlushDetector__609__opPtr = (0x3fU 
                                                  & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [1U][0U]);
    __Vfunc_SelectiveFlushDetector__609__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__609__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__609__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__609__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__609__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__609__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                goto __Vlabel67;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__609__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                    goto __Vlabel67;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
                    goto __Vlabel67;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__609__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                    goto __Vlabel67;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__609__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__609__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__609__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 1U;
                    goto __Vlabel67;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
                    goto __Vlabel67;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
                goto __Vlabel67;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout = 0U;
        }
        __Vlabel67: ;
    }
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Entry[1U] 
        = vlSelfRef.__Vfunc_SelectiveFlushDetector__609__Vfuncout;
    if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Entry
         [1U] & (~ (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [1U][0U] >> 0x10U)))) {
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = (0x20000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = (0x40000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][0U]);
    }
    __Vfunc_ToIndexPartFromFullAddr__610__addr = (0x3fffffU 
                                                  & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [1U][3U] 
                                                      << 0xaU) 
                                                     | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                        [1U][2U] 
                                                        >> 0x16U)));
    __Vfunc_ToIndexPartFromFullAddr__610__Vfuncout 
        = (0xffU & (__Vfunc_ToIndexPartFromFullAddr__610__addr 
                    >> 3U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
        = ((0x7ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [1U][2U]) | ((IData)(__Vfunc_ToIndexPartFromFullAddr__610__Vfuncout) 
                         << 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][3U] 
        = (7U & ((IData)(__Vfunc_ToIndexPartFromFullAddr__610__Vfuncout) 
                 >> 5U));
    __Vfunc_ToTagPartFromFullAddr__611__addr = (0x3fffffU 
                                                & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [1U][3U] 
                                                    << 0xaU) 
                                                   | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [1U][2U] 
                                                      >> 0x16U)));
    __Vfunc_ToTagPartFromFullAddr__611__Vfuncout = 
        (0x7ffU & (__Vfunc_ToTagPartFromFullAddr__611__addr 
                   >> 0xbU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
        = ((0xfc007fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [1U][2U]) | ((IData)(__Vfunc_ToTagPartFromFullAddr__611__Vfuncout) 
                         << 0xfU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [1U][0U]) | ((IData)((((QData)((IData)(
                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [1U][2U])) 
                                   << 0x2dU) | (((QData)((IData)(
                                                                 vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                 [1U][1U])) 
                                                 << 0xdU) 
                                                | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [1U][0U])) 
                                                   >> 0x13U)))) 
                         << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][1U] 
        = (((IData)((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                      [1U][2U])) << 0x2dU) 
                     | (((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [1U][1U])) 
                         << 0xdU) | ((QData)((IData)(
                                                     vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                     [1U][0U])) 
                                     >> 0x13U)))) >> 0x14U) 
           | ((IData)(((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [1U][2U])) 
                         << 0x2dU) | (((QData)((IData)(
                                                       vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                       [1U][1U])) 
                                       << 0xdU) | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [1U][0U])) 
                                                   >> 0x13U))) 
                       >> 0x20U)) << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
        = ((0xfffff000U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [1U][2U]) | ((IData)(((((QData)((IData)(
                                                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [1U][2U])) 
                                    << 0x2dU) | (((QData)((IData)(
                                                                  vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                  [1U][1U])) 
                                                  << 0xdU) 
                                                 | ((QData)((IData)(
                                                                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                    [1U][0U])) 
                                                    >> 0x13U))) 
                                  >> 0x20U)) >> 0x14U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
        = (0xff0U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
        = (0x4000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[1U] = 0U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
        = (0xfbffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
        = (0xffffefffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
        = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
        = (0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
        = ((0xfffffffbU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
            [1U][0U]) | (4U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                               [1U][0U] >> 0xcU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
        = (0xfffffffdU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 0U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][0U] 
        = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
            [1U][0U]) | ((IData)((((QData)((IData)(
                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [1U][2U])) 
                                   << 0x2dU) | (((QData)((IData)(
                                                                 vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                 [1U][1U])) 
                                                 << 0xdU) 
                                                | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [1U][0U])) 
                                                   >> 0x13U)))) 
                         << 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][1U] 
        = (((IData)((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                      [1U][2U])) << 0x2dU) 
                     | (((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [1U][1U])) 
                         << 0xdU) | ((QData)((IData)(
                                                     vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                     [1U][0U])) 
                                     >> 0x13U)))) >> 0x1fU) 
           | ((IData)(((((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [1U][2U])) 
                         << 0x2dU) | (((QData)((IData)(
                                                       vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                       [1U][1U])) 
                                       << 0xdU) | ((QData)((IData)(
                                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                   [1U][0U])) 
                                                   >> 0x13U))) 
                       >> 0x20U)) << 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][2U] 
        = ((0x7ffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
            [1U][2U]) | (0x7fffffU & ((IData)(((((QData)((IData)(
                                                                 vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                 [1U][2U])) 
                                                 << 0x2dU) 
                                                | (((QData)((IData)(
                                                                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                    [1U][1U])) 
                                                    << 0xdU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                      [1U][0U])) 
                                                      >> 0x13U))) 
                                               >> 0x20U)) 
                                      >> 0x1fU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][0U] 
        = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][2U] 
        = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
            [1U][2U]) | (0x7ffffeU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                      [1U][4U] >> 4U)));
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete[1U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine[1U] = 0ULL;
    if ((4U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
         [1U][5U])) {
        if ((2U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [1U][5U])) {
            if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                 [1U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                         [1U]))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [1U] << 0x16U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                    = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [1U] >> 0xaU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                    = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                                     [1U] << 0x10U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                     [1U] << 0xfU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                      [1U] ? 0xdU : 7U) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                              [1U] ? 0xdU
                                               : 7U) 
                                             >> 2U)));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                       [1U]);
            }
        } else if ((1U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [1U][5U])) {
            if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                 [1U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                         [1U]))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [1U] << 0x16U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                    = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                     [1U] >> 0xaU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                    = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][3U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                                     [1U] << 0x10U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                     [1U] << 0xfU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                      [1U] ? 0xdU : 7U) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                              [1U] ? 0xdU
                                               : 7U) 
                                             >> 2U)));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                       [1U]);
            }
        } else if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [1U][4U] >> 0x1fU)) {
            if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [1U][4U])) {
                if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                     [1U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                             [1U]))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][5U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                        = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                         [1U] << 0x16U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                        = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                         [1U] >> 0xaU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                        = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][3U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                        = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                        = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][2U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                                         [1U] << 0x10U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                         [1U] << 0xfU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                          [1U] ? 0xdU
                                           : 7U) << 0x1eU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                                  [1U]
                                                   ? 0xdU
                                                   : 7U) 
                                                 >> 2U)));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
                           [1U]);
                }
            } else if ((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                               [1U][0U] >> 0x12U) | 
                              (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                               [1U][0U] >> 0x10U)))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][5U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = (7U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][5U]);
            }
        } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [1U][4U])) {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[1U] = 1U;
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
                = (0x4000000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                   [1U][2U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
                = (0x1000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                   [1U][2U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
                = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                   [1U][2U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
                = ((0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                    [1U][0U]) | (8U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [1U][0U] >> 0xdU)));
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
                = ((0xfffffffbU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                    [1U][0U]) | (4U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [1U][0U] >> 0xcU)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                  [1U] ? 0x12U : ((0x20000U 
                                                   & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [1U][0U])
                                                   ? 0x12U
                                                   : 0x11U)) 
                                 << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                          [1U] ? 0x12U
                                           : ((0x20000U 
                                               & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [1U][0U])
                                               ? 0x12U
                                               : 0x11U)) 
                                         >> 2U)));
        } else {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 0U;
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                          [1U][3U] 
                                          >> 0xcU) 
                                         & (~ (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                               [1U][0U] 
                                               & ((1U 
                                                   & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [1U][2U] 
                                                      >> 0x13U)) 
                                                  == 
                                                  (1U 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                      [1U][0U] 
                                                      >> 1U)))))))
                                   ? 0x10U : 0x12U) 
                                 << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][5U]) | (0xfU & (((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [1U][3U] 
                                                  >> 0xcU) 
                                                 & (~ 
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                     [1U][0U] 
                                                     & ((1U 
                                                         & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                            [1U][2U] 
                                                            >> 0x13U)) 
                                                        == 
                                                        (1U 
                                                         & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                            [1U][0U] 
                                                            >> 1U)))))))
                                           ? 0x10U : 0x12U) 
                                         >> 2U)));
        }
    } else if ((2U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [1U][5U])) {
        if ((1U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [1U][5U])) {
            if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [1U][4U] >> 0x1fU)) {
                if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                     [1U][4U])) {
                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 1U;
                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][0U] 
                        = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                           [1U][0U]);
                    vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__addr 
                        = (0x3fffffU & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                         [1U][3U] << 0xaU) 
                                        | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                           [1U][2U] 
                                           >> 0x16U)));
                    vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__Vfuncout 
                        = (0x3ffff8U & vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__addr);
                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][2U] 
                        = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                            [1U][2U]) | (0x7fffffU 
                                         & (vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__612__Vfuncout 
                                            << 1U)));
                    if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                         [1U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                 [1U] >> 3U))) {
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                            = ((0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                [1U][2U]) | (0x80000U 
                                             & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                                [1U] 
                                                << 0x13U)));
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                            = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                               [1U][4U]);
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                            = (4U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                     [1U][5U]));
                    } else {
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                            = (0xc0000000U | (0x3fffffffU 
                                              & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                              [1U][4U]));
                        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                            = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                     [1U][5U]));
                    }
                } else if (((vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                             [4U][2U] >> 4U) & ((3U 
                                                 & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [1U][2U] 
                                                    >> 0x14U)) 
                                                == 
                                                (3U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [4U][0U] 
                                                    >> 2U))))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][0U]) | ((IData)((((QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                   [4U][2U])) 
                                                   << 0x3cU) 
                                                  | (((QData)((IData)(
                                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                      [4U][1U])) 
                                                      << 0x1cU) 
                                                     | ((QData)((IData)(
                                                                        vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                        [4U][0U])) 
                                                        >> 4U)))) 
                                         << 0x13U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][1U] 
                        = (((IData)((((QData)((IData)(
                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                      [4U][2U])) 
                                      << 0x3cU) | (
                                                   ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [4U][1U])) 
                                                    << 0x1cU) 
                                                   | ((QData)((IData)(
                                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                      [4U][0U])) 
                                                      >> 4U)))) 
                            >> 0xdU) | ((IData)(((((QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                   [4U][2U])) 
                                                   << 0x3cU) 
                                                  | (((QData)((IData)(
                                                                      vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                      [4U][1U])) 
                                                      << 0x1cU) 
                                                     | ((QData)((IData)(
                                                                        vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                        [4U][0U])) 
                                                        >> 4U))) 
                                                 >> 0x20U)) 
                                        << 0x13U));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                        = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][2U]) | ((IData)(((
                                                   ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [4U][2U])) 
                                                    << 0x3cU) 
                                                   | (((QData)((IData)(
                                                                       vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                       [4U][1U])) 
                                                       << 0x1cU) 
                                                      | ((QData)((IData)(
                                                                         vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                         [4U][0U])) 
                                                         >> 4U))) 
                                                  >> 0x20U)) 
                                         >> 0xdU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                        = (0x1000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][3U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][4U]) | (((0x10000U 
                                           & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                           [1U][0U])
                                           ? 0xcU : 
                                          ((0x8000U 
                                            & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                            [1U][0U])
                                            ? 0x12U
                                            : 0x11U)) 
                                         << 0x1eU));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][5U]) | (0xfU & (((0x10000U 
                                                   & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [1U][0U])
                                                   ? 0xcU
                                                   : 
                                                  ((0x8000U 
                                                    & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [1U][0U])
                                                    ? 0x12U
                                                    : 0x11U)) 
                                                 >> 2U)));
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0x80000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [1U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [1U][5U]));
                }
            } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                        [1U][4U])) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 1U;
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][0U] 
                    = (0xfffffffeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                       [1U][0U]);
                vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__addr 
                    = (0x3fffffU & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                     [1U][3U] << 0xaU) 
                                    | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [1U][2U] >> 0x16U)));
                vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__Vfuncout 
                    = (0x3ffff8U & vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__addr);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][2U] 
                    = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                        [1U][2U]) | (0x7fffffU & (vlSelfRef.__Vfunc_ToLineAddrFromFullAddr__613__Vfuncout 
                                                  << 1U)));
                if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                     [1U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                             [1U] >> 3U))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                        = ((0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][2U]) | (0x300000U 
                                         & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                            [1U] << 0x13U)));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0x80000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [1U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [1U][5U]));
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0x40000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [1U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (3U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [1U][5U]));
                }
            } else {
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty 
                    = vlSelfRef.__PVT__dCache__DOT__storedLineByteWE;
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                    = vlSelfRef.__PVT__dCache__DOT__storedLineData;
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                    = (((QData)((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                        [1U][2U])) 
                        << 0x2dU) | (((QData)((IData)(
                                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [1U][1U])) 
                                      << 0xdU) | ((QData)((IData)(
                                                                  vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                                  [1U][0U])) 
                                                  >> 0x13U)));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffffffcULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | (IData)((IData)(((2U & (((1U 
                                                   & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                   ? (IData)(
                                                             (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                              >> 1U))
                                                   : (IData)(
                                                             (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                              >> 1U))) 
                                                 << 1U)) 
                                          | (1U & (
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine)
                                                    : (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine)))))));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffffff3ULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 3U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 3U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((1U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 2U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 2U))))))) 
                          << 2U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffffffcfULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 5U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 5U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((1U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 4U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 4U))))))) 
                          << 4U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffffff3fULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 7U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 7U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((1U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 6U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 6U))))))) 
                          << 6U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffffcffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 9U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 9U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 8U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 8U))))))) 
                          << 8U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffff3ffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0xbU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0xbU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0xaU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0xaU))))))) 
                          << 0xaU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffffcfffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0xdU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0xdU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0xcU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0xcU))))))) 
                          << 0xcU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffff3fffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (2U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0xfU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0xfU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((2U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0xeU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0xeU))))))) 
                          << 0xeU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffffcffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x11U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x11U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x10U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x10U))))))) 
                          << 0x10U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffff3ffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x13U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x13U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x12U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x12U))))))) 
                          << 0x12U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffffcfffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x15U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x15U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x14U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x14U))))))) 
                          << 0x14U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffff3fffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x17U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x17U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((4U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x16U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x16U))))))) 
                          << 0x16U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffffcffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x19U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x19U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x18U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x18U))))))) 
                          << 0x18U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffff3ffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x1bU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x1bU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x1aU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x1aU))))))) 
                          << 0x1aU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffffcfffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x1dU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x1dU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x1cU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x1cU))))))) 
                          << 0x1cU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffff3fffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x1fU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x1fU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((8U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x1eU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x1eU))))))) 
                          << 0x1eU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffffcffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x21U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x21U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x20U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x20U))))))) 
                          << 0x20U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffff3ffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x23U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x23U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x22U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x22U))))))) 
                          << 0x22U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffffcfffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x25U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x25U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x24U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x24U))))))) 
                          << 0x24U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffff3fffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x10U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x27U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x27U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x10U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x26U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x26U))))))) 
                          << 0x26U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffffcffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x29U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x29U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x28U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x28U))))))) 
                          << 0x28U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffff3ffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x2bU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x2bU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x2aU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x2aU))))))) 
                          << 0x2aU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffffcfffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x2dU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x2dU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x2cU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x2cU))))))) 
                          << 0x2cU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffff3fffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x20U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x2fU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x2fU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x20U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x2eU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x2eU))))))) 
                          << 0x2eU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfffcffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x31U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x31U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x30U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x30U))))))) 
                          << 0x30U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfff3ffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x33U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x33U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x32U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x32U))))))) 
                          << 0x32U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xffcfffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x35U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x35U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x34U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x34U))))))) 
                          << 0x34U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xff3fffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x40U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x37U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x37U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x40U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x36U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x36U))))))) 
                          << 0x36U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xfcffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x39U))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x39U))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x38U))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x38U))))))) 
                          << 0x38U));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xf3ffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x3bU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x3bU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x3aU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x3aU))))))) 
                          << 0x3aU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0xcfffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x3dU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x3dU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x3cU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x3cU))))))) 
                          << 0x3cU));
                vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine 
                    = ((0x3fffffffffffffffULL & vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine) 
                       | ((QData)((IData)(((2U & ((
                                                   (0x80U 
                                                    & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                    ? (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                               >> 0x3fU))
                                                    : (IData)(
                                                              (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                               >> 0x3fU))) 
                                                  << 1U)) 
                                           | (1U & 
                                              ((0x80U 
                                                & (IData)(vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedDirty))
                                                ? (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__storedLine 
                                                           >> 0x3eU))
                                                : (IData)(
                                                          (vlSelfRef.__Vtask_MergeStoreDataToLine__614__fetchedLine 
                                                           >> 0x3eU))))))) 
                          << 0x3eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine[1U] 
                    = vlSelfRef.__Vtask_MergeStoreDataToLine__614__dstLine;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][0U]) | ((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                             [1U]) 
                                     << 0x13U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][1U] 
                    = (((IData)(vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                [1U]) >> 0xdU) | ((IData)(
                                                          (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                                           [1U] 
                                                           >> 0x20U)) 
                                                  << 0x13U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][2U]) | ((IData)((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mergedLine
                                              [1U] 
                                              >> 0x20U)) 
                                     >> 0xdU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][4U]) | (((0x8000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [1U][0U]) ? 0xfU
                                       : 0x11U) << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][5U]) | (0xfU & (((0x8000U 
                                               & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [1U][0U])
                                               ? 0xfU
                                               : 0x11U) 
                                             >> 2U)));
            }
        } else if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [1U][4U] >> 0x1fU)) {
            if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [1U][4U])) {
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 0U;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][4U]) | (((1U & (((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [1U][4U] 
                                               >> 0x1cU) 
                                              & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                 [1U][4U] 
                                                 >> 0x1dU)) 
                                             & (~ (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                   [1U][0U] 
                                                   & ((1U 
                                                       & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                          [1U][2U] 
                                                          >> 0x13U)) 
                                                      == 
                                                      (1U 
                                                       & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                          [1U][0U] 
                                                          >> 1U)))))))
                                       ? 0xbU : 0xdU) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][5U]) | (0xfU & (((1U & 
                                               (((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [1U][4U] 
                                                  >> 0x1cU) 
                                                 & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [1U][4U] 
                                                    >> 0x1dU)) 
                                                & (~ 
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [1U][0U] 
                                                    & ((1U 
                                                        & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                           [1U][2U] 
                                                           >> 0x13U)) 
                                                       == 
                                                       (1U 
                                                        & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                           [1U][0U] 
                                                           >> 1U)))))))
                                               ? 0xbU
                                               : 0xdU) 
                                             >> 2U)));
            } else {
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 1U;
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][0U] 
                    = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                       [1U][0U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][2U] 
                    = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                        [1U][2U]) | (0x7ffffeU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                  [1U][4U] 
                                                  >> 4U)));
                if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                     [1U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                             [1U] >> 3U))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                        = ((0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][2U]) | (0x80000U & 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                          [1U] << 0x13U)));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0xc0000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [1U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (2U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [1U][5U]));
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0x80000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [1U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (2U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [1U][5U]));
                }
            }
        } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [1U][4U])) {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                = ((0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][0U]) | (0x4000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                            [1U][0U] 
                                            << 0xeU)));
            if (((0xa3U >= ((IData)(0x6dU) + (1U & 
                                              (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                               [1U][0U] 
                                               >> 0xeU)))) 
                 && (1U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                           [1U][(((IData)(0x6dU) + 
                                  (1U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                         [1U][0U] >> 0xeU))) 
                                 >> 5U)] >> (0x1fU 
                                             & ((IData)(0x6dU) 
                                                + (1U 
                                                   & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                      [1U][0U] 
                                                      >> 0xeU)))))))) {
                vlSelfRef.__Vfunc_BuildFullAddr__615__tag 
                    = ((0xa3U >= ((IData)(0x6fU) + 
                                  (0x1fU & ((IData)(0xbU) 
                                            * (1U & 
                                               (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                [1U][0U] 
                                                >> 0xeU))))))
                        ? (0x7ffU & (((0U == (0x1fU 
                                              & ((IData)(0x6fU) 
                                                 + 
                                                 (0x1fU 
                                                  & ((IData)(0xbU) 
                                                     * 
                                                     (1U 
                                                      & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                         [1U][0U] 
                                                         >> 0xeU)))))))
                                       ? 0U : (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [1U][
                                               (((IData)(0x79U) 
                                                 + 
                                                 (0x1fU 
                                                  & ((IData)(0xbU) 
                                                     * 
                                                     (1U 
                                                      & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                         [1U][0U] 
                                                         >> 0xeU))))) 
                                                >> 5U)] 
                                               << ((IData)(0x20U) 
                                                   - 
                                                   (0x1fU 
                                                    & ((IData)(0x6fU) 
                                                       + 
                                                       (0x1fU 
                                                        & ((IData)(0xbU) 
                                                           * 
                                                           (1U 
                                                            & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                               [1U][0U] 
                                                               >> 0xeU))))))))) 
                                     | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                        [1U][(((IData)(0x6fU) 
                                               + (0x1fU 
                                                  & ((IData)(0xbU) 
                                                     * 
                                                     (1U 
                                                      & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                         [1U][0U] 
                                                         >> 0xeU))))) 
                                              >> 5U)] 
                                        >> (0x1fU & 
                                            ((IData)(0x6fU) 
                                             + (0x1fU 
                                                & ((IData)(0xbU) 
                                                   * 
                                                   (1U 
                                                    & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                       [1U][0U] 
                                                       >> 0xeU)))))))))
                        : 0U);
                vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__addr 
                    = (0x3fffffU & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                     [1U][3U] << 0xaU) 
                                    | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                       [1U][2U] >> 0x16U)));
                vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__Vfuncout 
                    = (0xffU & (vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__addr 
                                >> 3U));
                vlSelfRef.__Vfunc_BuildFullAddr__615__index 
                    = vlSelfRef.__Vfunc_ToIndexPartFromFullAddr__616__Vfuncout;
                vlSelfRef.__Vfunc_BuildFullAddr__615__Vfuncout 
                    = (((IData)(vlSelfRef.__Vfunc_BuildFullAddr__615__tag) 
                        << 0xbU) | ((IData)(vlSelfRef.__Vfunc_BuildFullAddr__615__index) 
                                    << 3U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = ((0xf800001fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][4U]) | (vlSelfRef.__Vfunc_BuildFullAddr__615__Vfuncout 
                                     << 5U));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0x10000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
            } else {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
            }
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = (0x8000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [1U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][0U]) | ((IData)((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [1U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [1U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [1U][0U])) 
                                                >> 2U)))) 
                                 << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][1U] 
                = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                              [1U][2U])) 
                              << 0x3eU) | (((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [1U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [1U][0U])) 
                                              >> 2U)))) 
                    >> 0xdU) | ((IData)(((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [1U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [1U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [1U][0U])) 
                                                >> 2U))) 
                                         >> 0x20U)) 
                                << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][2U]) | ((IData)(((((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [1U][2U])) 
                                            << 0x3eU) 
                                           | (((QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                               [1U][1U])) 
                                               << 0x1eU) 
                                              | ((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                 [1U][0U])) 
                                                 >> 2U))) 
                                          >> 0x20U)) 
                                 >> 0xdU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (0x20000000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                [1U][0U] 
                                                << 0x1cU)));
            __Vtemp_3 = ((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                 [1U][4U] >> 0x1cU) 
                                & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                   [1U][4U] >> 0x1dU)))
                          ? 0xaU : 0xdU);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (((1U & ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [1U][4U] 
                                          >> 0x1cU) 
                                         & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                            [1U][4U] 
                                            >> 0x1dU)))
                                   ? 0xaU : 0xdU) << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][5U]) | (0xfU & (__Vtemp_3 
                                         >> 2U)));
        } else {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                = ((0x7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][3U]) | (0xffff8000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                                [1U][2U] 
                                                << 8U)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0xffffffe0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                          [1U][2U] 
                                          >> 0x18U)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
                = ((0xffff9fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][3U]) | (0x6000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                            [1U][2U] 
                                            << 8U)));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = (0x40000000U | (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                  [1U][4U]));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = (2U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                         [1U][5U]));
        }
    } else if ((1U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [1U][5U])) {
        if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [1U][4U] >> 0x1fU)) {
            if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                 [1U][4U])) {
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[1U] = 1U;
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
                    = (0xfbffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [1U][2U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
                    = (0xffffefffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [1U][2U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
                    = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [1U][2U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
                    = (0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [1U][0U]);
                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
                    = (2U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                      [1U] ? 8U : (
                                                   (0x20000U 
                                                    & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                    [1U][0U])
                                                    ? 0x12U
                                                    : 7U)) 
                                     << 0x1eU));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                              [1U] ? 8U
                                               : ((0x20000U 
                                                   & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [1U][0U])
                                                   ? 0x12U
                                                   : 7U)) 
                                             >> 2U)));
            } else {
                if ((0xffU == (0xffU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                        [1U][0U] >> 6U)))) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][0U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete[1U] = 1U;
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                        = ((0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                            [1U][0U]) | (0x3fc0U & 
                                         (((IData)(1U) 
                                           + ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                               [1U][0U] 
                                               << 0x1aU) 
                                              | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                 [1U][0U] 
                                                 >> 6U))) 
                                          << 6U)));
                }
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][2U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                    = (0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][0U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][1U] = 0U;
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = (0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][2U]);
                if (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete
                    [1U]) {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][4U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][5U]);
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (7U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][5U]);
                } else {
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                        = (0x40000000U | (0x3fffffffU 
                                          & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                          [1U][4U]));
                    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                        = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                           [1U][5U]);
                }
            }
        } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                    [1U][4U])) {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 0U;
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (((1U & (((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                           [1U][4U] 
                                           >> 0x1cU) 
                                          & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                             [1U][4U] 
                                             >> 0x1dU)) 
                                         & (~ (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                               [1U][0U] 
                                               & ((1U 
                                                   & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                      [1U][2U] 
                                                      >> 0x13U)) 
                                                  == 
                                                  (1U 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                      [1U][0U] 
                                                      >> 1U)))))))
                                   ? 5U : 6U) << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][5U]) | (0xfU & (((1U & (((
                                                   vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                   [1U][4U] 
                                                   >> 0x1cU) 
                                                  & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                     [1U][4U] 
                                                     >> 0x1dU)) 
                                                 & (~ 
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                     [1U][0U] 
                                                     & ((1U 
                                                         & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                                            [1U][2U] 
                                                            >> 0x13U)) 
                                                        == 
                                                        (1U 
                                                         & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                            [1U][0U] 
                                                            >> 1U)))))))
                                           ? 5U : 6U) 
                                         >> 2U)));
        } else {
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1U] = 1U;
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][0U] 
                = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                   [1U][0U]);
            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn[1U][2U] 
                = ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                    [1U][2U]) | (0x7ffffeU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                              [1U][4U] 
                                              >> 4U)));
            if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt
                 [1U] & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                         [1U] >> 3U))) {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                    = ((0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                        [1U][2U]) | (0x80000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                                 [1U] 
                                                 << 0x13U)));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0x40000000U | (0x3fffffffU & 
                                      vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                      [1U][4U]));
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = (1U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                             [1U][5U]));
            } else {
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                    = (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                       [1U][4U]);
                vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                    = (1U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                             [1U][5U]));
            }
        }
    } else if ((vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [1U][4U] >> 0x1fU)) {
        if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
             [1U][4U])) {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = (0x8000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [1U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
                = ((0x7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][0U]) | ((IData)((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [1U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [1U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [1U][0U])) 
                                                >> 2U)))) 
                                 << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][1U] 
                = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                              [1U][2U])) 
                              << 0x3eU) | (((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [1U][1U])) 
                                            << 0x1eU) 
                                           | ((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [1U][0U])) 
                                              >> 2U)))) 
                    >> 0xdU) | ((IData)(((((QData)((IData)(
                                                           vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                           [1U][2U])) 
                                           << 0x3eU) 
                                          | (((QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                              [1U][1U])) 
                                              << 0x1eU) 
                                             | ((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                [1U][0U])) 
                                                >> 2U))) 
                                         >> 0x20U)) 
                                << 0x13U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
                = ((0xfff80000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][2U]) | ((IData)(((((QData)((IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                            [1U][2U])) 
                                            << 0x3eU) 
                                           | (((QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                               [1U][1U])) 
                                               << 0x1eU) 
                                              | ((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                 [1U][0U])) 
                                                 >> 2U))) 
                                          >> 0x20U)) 
                                 >> 0xdU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (0x20000000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                [1U][0U] 
                                                << 0x1cU)));
            __Vtemp_4 = ((0x20000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                          [1U][4U]) ? 4U : 6U);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (((0x20000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                   [1U][4U]) ? 4U : 6U) 
                                 << 0x1eU));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][5U]) | (0xfU & (__Vtemp_4 
                                         >> 2U)));
        } else if ((0U != (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                 [1U][2U] >> 5U)))) {
            vlSelfRef.__Vfunc_BuildFullAddr__617__tag 
                = (0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                             [1U][2U] >> 7U));
            vlSelfRef.__Vfunc_BuildFullAddr__617__index 
                = (0xffU & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                            [1U][0U] >> 6U));
            vlSelfRef.__Vfunc_BuildFullAddr__617__Vfuncout 
                = (((IData)(vlSelfRef.__Vfunc_BuildFullAddr__617__tag) 
                    << 0xbU) | ((IData)(vlSelfRef.__Vfunc_BuildFullAddr__617__index) 
                                << 3U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = ((0xf800001fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                    [1U][4U]) | (vlSelfRef.__Vfunc_BuildFullAddr__617__Vfuncout 
                                 << 5U));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = (0x10000000U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [1U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = (0xc0000000U | (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                  [1U][4U]));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [1U][5U]);
        } else {
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                   [1U][4U]);
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
                = (0x80000000U | (0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                  [1U][4U]));
            vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
                = (1U | (8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                         [1U][5U]));
        }
    } else if ((0x40000000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                [1U][4U])) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[1U] = 1U;
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
            = ((0x7ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                [1U][2U]) | (0xf8000000U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                                            [1U][0U] 
                                            << 0x15U)));
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][3U] 
            = (7U & (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__mshr
                     [1U][0U] >> 0xbU));
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
            = (0xffffbfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [1U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
            = (0x4000000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [1U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
            = (0x1000U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [1U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][2U] 
            = (0xffffdfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [1U][2U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
            = (0xfffffff7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [1U][0U]);
        vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn[1U][0U] 
            = (1U | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
               [1U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
            = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                              [1U] ? 2U : 1U) << 0x1eU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
            = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt
                                      [1U] ? 2U : 1U) 
                                     >> 2U)));
    } else if ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR
                [1U] & (~ vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation
                        [1U]))) {
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
            = (8U | vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][5U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
            = ((0x3fffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][2U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                             [1U] << 0x16U));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
            = ((0xfffff000U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][3U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                             [1U] >> 0xaU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][3U] 
            = (0xffffefffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][3U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
            = (0xefffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
            = (0xdfffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
            = (0xf7ffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][4U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
            = (0xffcfffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][2U] 
            = (0xfff7ffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][2U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = (0xfffbffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = (0xfffdffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = ((0xfffeffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][0U]) | (vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore
                             [1U] << 0x10U));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = ((0xffff7fffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][0U]) | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                             [1U] << 0xfU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = (0xffffbfffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = (0xffffc03fU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
               [1U][0U]);
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][4U] 
            = ((0x3fffffffU & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][4U]) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                              [1U] ? 0xdU : 7U) << 0x1eU));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][5U] 
            = ((8U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][5U]) | (0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable
                                      [1U] ? 0xdU : 7U) 
                                     >> 2U)));
        vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR[1U][0U] 
            = ((0xffffffc0U & vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                [1U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr
               [1U]);
    }
    vlSelfRef.__PVT__dCache__DOT__missHandler__DOT__unnamedblk5__DOT__i = 2U;
    vlSelfRef.__PVT__dCache__DOT__memMux__DOT__portIn 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memInSel;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memAddr 
        = (0x3fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                        [vlSelfRef.__PVT__dCache__DOT__memMux__DOT__portIn][2U] 
                        >> 1U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memData 
        = (((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                            [vlSelfRef.__PVT__dCache__DOT__memMux__DOT__portIn][2U])) 
            << 0x3fU) | (((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                          [vlSelfRef.__PVT__dCache__DOT__memMux__DOT__portIn][1U])) 
                          << 0x1fU) | ((QData)((IData)(
                                                       vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                                       [vlSelfRef.__PVT__dCache__DOT__memMux__DOT__portIn][0U])) 
                                       >> 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memWE 
        = (1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
           [vlSelfRef.__PVT__dCache__DOT__memMux__DOT__portIn][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[0U] 
        = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memData);
    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[1U] 
        = (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memData 
                   >> 0x20U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
        = (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memValid) 
            << 0x17U) | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memWE) 
                          << 0x16U) | vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memAddr));
    vlSelfRef.__PVT__memoryAccessController__DOT__icAck = 0U;
    vlSelfRef.__PVT__memoryAccessController__DOT__dcAck = 0U;
    if (((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__processLatencyCount)) 
         & (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReq 
            >> 0x16U))) {
        vlSelfRef.__PVT__memoryAccessController__DOT__icAck = 1U;
    } else if ((IData)(((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__processLatencyCount)) 
                        & (0x800000U == (0xc00000U 
                                         & vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U]))))) {
        vlSelfRef.__PVT__memoryAccessController__DOT__dcAck = 1U;
    } else if ((IData)(((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__processLatencyCount)) 
                        & (0xc00000U == (0xc00000U 
                                         & vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U]))))) {
        vlSelfRef.__PVT__memoryAccessController__DOT__dcAck = 1U;
    }
    if (vlSelfRef.__PVT__memoryAccessController__DOT__icAck) {
        vlSelfRef.__PVT__memAccessAddr = (0x3fffffU 
                                          & vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReq);
        vlSelfRef.__PVT__memAccessWriteData = 0ULL;
        vlSelfRef.__PVT__memAccessRE = 1U;
        vlSelfRef.__PVT__memAccessWE = 0U;
    } else if (vlSelfRef.__PVT__memoryAccessController__DOT__dcAck) {
        vlSelfRef.__PVT__memAccessAddr = (0x3fffffU 
                                          & vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U]);
        vlSelfRef.__PVT__memAccessWriteData = (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[1U])) 
                                                << 0x20U) 
                                               | (QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[0U])));
        vlSelfRef.__PVT__memAccessRE = (1U & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
                                                 >> 0x16U)));
        vlSelfRef.__PVT__memAccessWE = (1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
                                              >> 0x16U));
    } else {
        vlSelfRef.__PVT__memAccessAddr = 0U;
        vlSelfRef.__PVT__memAccessWriteData = 0ULL;
        vlSelfRef.__PVT__memAccessRE = 0U;
        vlSelfRef.__PVT__memAccessWE = 0U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReqAck 
        = ((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReqAck)) 
           | (((IData)(vlSelfRef.__PVT__memoryAccessController__DOT__icAck) 
               << 3U) | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextMemReadSerial) 
                         << 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck 
        = (((IData)(vlSelfRef.__PVT__memoryAccessController__DOT__dcAck) 
            << 3U) | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextMemReadSerial) 
                       << 1U) | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextMemWriteSerial)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut[0U] 
        = ((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
            [0U]) | (8U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut[0U] 
        = ((9U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
            [0U]) | (6U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut[0U] 
        = ((0xeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
            [0U]) | (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut[1U] 
        = ((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
            [1U]) | (8U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut[1U] 
        = ((9U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
            [1U]) | (6U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut[1U] 
        = ((0xeU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
            [1U]) | (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck)));
    vlSelfRef.__PVT__memoryAccessController__DOT__nextReqSerial 
        = vlSelfRef.__PVT__memoryAccessController__DOT__reqSerial;
    if (((IData)(vlSelfRef.__PVT__memoryAccessController__DOT__icAck) 
         | ((IData)(vlSelfRef.__PVT__memoryAccessController__DOT__dcAck) 
            & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
                  >> 0x16U))))) {
        vlSelfRef.__PVT__memoryAccessController__DOT__nextReqSerial 
            = (3U & ((IData)(1U) + (IData)(vlSelfRef.__PVT__memoryAccessController__DOT__reqSerial)));
    }
    vlSelfRef.__PVT__iCache__DOT__nextSerial = vlSelfRef.__PVT__iCache__DOT__regSerial;
    if ((1U & (~ ((IData)(vlSelfRef.__PVT__iCache__DOT__regPhase) 
                  >> 2U)))) {
        if ((1U & (~ ((IData)(vlSelfRef.__PVT__iCache__DOT__regPhase) 
                      >> 1U)))) {
            if ((1U & (IData)(vlSelfRef.__PVT__iCache__DOT__regPhase))) {
                if ((8U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReqAck))) {
                    vlSelfRef.__PVT__iCache__DOT__nextSerial 
                        = (3U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReqAck) 
                                 >> 1U));
                }
            }
        }
    }
}
