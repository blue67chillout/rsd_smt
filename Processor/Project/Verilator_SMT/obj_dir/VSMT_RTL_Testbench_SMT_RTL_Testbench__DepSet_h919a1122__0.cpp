// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_SMT_RTL_Testbench.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT VlCoroutine VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_initial__TOP__SMT_RTL_Testbench__Vtiming__0(VSMT_RTL_Testbench_SMT_RTL_Testbench* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+      VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_initial__TOP__SMT_RTL_Testbench__Vtiming__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rst = 1U;
    vlSelfRef.__PVT__rstStart = 0U;
    co_await vlSymsp->TOP.__VdlySched.delay(0x186a0ULL, 
                                            nullptr, 
                                            "SMT_RTL_Testbench.sv", 
                                            22);
    vlSelfRef.__PVT__rst = 0U;
    co_await vlSymsp->TOP.__VdlySched.delay(0x2710ULL, 
                                            nullptr, 
                                            "SMT_RTL_Testbench.sv", 
                                            23);
    vlSelfRef.__PVT__rstStart = 1U;
    co_await vlSymsp->TOP.__VdlySched.delay(0x4e20ULL, 
                                            nullptr, 
                                            "SMT_RTL_Testbench.sv", 
                                            24);
    vlSelfRef.__PVT__rstStart = 0U;
}

VL_INLINE_OPT VlCoroutine VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_initial__TOP__SMT_RTL_Testbench__Vtiming__1(VSMT_RTL_Testbench_SMT_RTL_Testbench* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+      VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_initial__TOP__SMT_RTL_Testbench__Vtiming__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<6>/*191:0*/ __Vtemp_1;
    // Body
    __Vtemp_1[0U] = 0x2e766364U;
    __Vtemp_1[1U] = 0x656e6368U;
    __Vtemp_1[2U] = 0x65737462U;
    __Vtemp_1[3U] = 0x544c5f54U;
    __Vtemp_1[4U] = 0x4d545f52U;
    __Vtemp_1[5U] = 0x53U;
    vlSymsp->_vm_contextp__->dumpfile(VL_CVT_PACK_STR_NW(6, __Vtemp_1));
    vlSymsp->_traceDumpOpen();
    co_await vlSymsp->TOP.__VdlySched.delay(0x989680ULL, 
                                            nullptr, 
                                            "SMT_RTL_Testbench.sv", 
                                            102);
    vlSelfRef.__PVT__unnamedblk1__DOT__thread0_x3 = (IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.debugValue
                                                            [3U]);
    vlSelfRef.__PVT__unnamedblk1__DOT__thread1_x3 = (IData)(
                                                            vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile__phyReg.debugValue
                                                            [0x43U]);
    VL_WRITEF_NX("Thread 0: x3 = %0# (expected 6)\nThread 1: x3 = %0# (expected 6, same program)\n",0,
                 32,vlSelfRef.__PVT__unnamedblk1__DOT__thread0_x3,
                 32,vlSelfRef.__PVT__unnamedblk1__DOT__thread1_x3);
    if (((6U == vlSelfRef.__PVT__unnamedblk1__DOT__thread0_x3) 
         | (6U == vlSelfRef.__PVT__unnamedblk1__DOT__thread1_x3))) {
        VL_WRITEF_NX("PASS: At least one thread executed correctly\n",0);
    } else {
        VL_WRITEF_NX("FAIL: Neither thread produced expected result\n",0);
    }
    VL_FINISH_MT("SMT_RTL_Testbench.sv", 120, "");
}

VL_INLINE_OPT VlCoroutine VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_initial__TOP__SMT_RTL_Testbench__Vtiming__2(VSMT_RTL_Testbench_SMT_RTL_Testbench* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+      VSMT_RTL_Testbench_SMT_RTL_Testbench___eval_initial__TOP__SMT_RTL_Testbench__Vtiming__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    while (1U) {
        co_await vlSymsp->TOP.__VdlySched.delay(0x1388ULL, 
                                                nullptr, 
                                                "SMT_RTL_Testbench.sv", 
                                                17);
        vlSelfRef.__PVT__clk = (1U & (~ (IData)(vlSelfRef.__PVT__clk)));
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_SMT_RTL_Testbench___nba_sequent__TOP__SMT_RTL_Testbench__0(VSMT_RTL_Testbench_SMT_RTL_Testbench* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+      VSMT_RTL_Testbench_SMT_RTL_Testbench___nba_sequent__TOP__SMT_RTL_Testbench__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if (VL_UNLIKELY(((0U != vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__lastCommittedPC)))) {
        VL_WRITEF_NX("Time: %t, Committed PC: %x\n",0,
                     64,VL_TIME_UNITED_Q(1000),-9,32,
                     vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__lastCommittedPC);
    }
    if (VL_UNLIKELY((vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__serialWE))) {
        VL_WRITEF_NX("Serial Output: %c\n",0,8,(0xffU 
                                                & vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioWriteDataIn));
    }
}
