// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Memory__Iz1.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_Memory__Iz1___stl_sequent__TOP__SMT_RTL_Testbench__memory__0(VSMT_RTL_Testbench_Memory__Iz1* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Memory__Iz1___stl_sequent__TOP__SMT_RTL_Testbench__memory__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__memReqQueue__DOT__randNext = vlSelfRef.__PVT__memReqQueue__DOT__randReg;
    vlSelfRef.__PVT__memReqQueue__DOT__count = vlSelfRef.__PVT__memReqQueue__DOT__countReg;
    if ((0U == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regCount))) {
        vlSelfRef.__PVT__memReqQueue__DOT__pop = 0U;
    } else if (((IData)(vlSelfRef.__PVT__memReqQueue__DOT__count) 
                == VL_MODDIV_III(32, vlSelfRef.__PVT__memReqQueue__DOT__randReg, (IData)(0xaU)))) {
        vlSelfRef.__PVT__memReqQueue__DOT__pop = 1U;
        vlSelfRef.__PVT__memReqQueue__DOT__count = 0U;
        vlSelfRef.__PVT__memReqQueue__DOT__randNext 
            = (vlSelfRef.__PVT__memReqQueue__DOT__randNext 
               ^ VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__memReqQueue__DOT__randNext, 0xdU));
        vlSelfRef.__PVT__memReqQueue__DOT__randNext 
            = (vlSelfRef.__PVT__memReqQueue__DOT__randNext 
               ^ VL_SHIFTR_III(32,32,32, vlSelfRef.__PVT__memReqQueue__DOT__randNext, 0x11U));
        vlSelfRef.__PVT__memReqQueue__DOT__randNext 
            = (vlSelfRef.__PVT__memReqQueue__DOT__randNext 
               ^ VL_SHIFTL_III(32,32,32, vlSelfRef.__PVT__memReqQueue__DOT__randNext, 5U));
    } else {
        vlSelfRef.__PVT__memReqQueue__DOT__count = 
            (0x1fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__memReqQueue__DOT__count)));
        vlSelfRef.__PVT__memReqQueue__DOT__pop = 0U;
    }
    vlSelfRef.__PVT__hasRequest = vlSelfRef.__PVT__memReqQueue__DOT__pop;
    vlSelfRef.__PVT__requestData[0U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][0U];
    vlSelfRef.__PVT__requestData[1U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][1U];
    vlSelfRef.__PVT__requestData[2U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][2U];
    vlSelfRef.__PVT__requestData[3U] = vlSelfRef.__PVT__memReqQueue__DOT__memoryRequestQueue
        [vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage][3U];
    vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage 
        = vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage;
    if (vlSelfRef.__PVT__memReqQueue__DOT__pop) {
        vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage 
            = ((0x7fU == (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage))
                ? 0U : (0x7fU & ((IData)(1U) + (IData)(vlSelfRef.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage))));
    }
}

VL_ATTR_COLD void VSMT_RTL_Testbench_Memory__Iz1___ctor_var_reset(VSMT_RTL_Testbench_Memory__Iz1* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Memory__Iz1___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memAccessAddr = VL_RAND_RESET_I(32);
    vlSelf->__PVT__memAccessWriteData = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__memAccessRE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memAccessWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memAccessBusy = VL_RAND_RESET_I(1);
    vlSelf->__PVT__nextMemReadSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__nextMemWriteSerial = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memReadDataReady = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memReadData = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__memReadSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__memAccessResponse = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        VL_RAND_RESET_W(69, vlSelf->__PVT__memPipeReg[__Vi0]);
    }
    VL_RAND_RESET_W(69, vlSelf->__PVT__nextMemPipeReg);
    vlSelf->__PVT__nextNextMemReadSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__nextNextMemWriteSerial = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memReadAccessAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__prevMemReadAccessAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memWriteAccessAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__prevMemWriteAccessAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__processLatencyCount = VL_RAND_RESET_I(2);
    vlSelf->__PVT__nextProcessLatencyCount = VL_RAND_RESET_I(2);
    vlSelf->__PVT__pushRequestQueue = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(102, vlSelf->__PVT__pushedData);
    vlSelf->__PVT__hasRequest = VL_RAND_RESET_I(1);
    vlSelf->__PVT__hasRequestReg = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(102, vlSelf->__PVT__requestData);
    VL_RAND_RESET_W(102, vlSelf->__PVT__requestDataReg);
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__unnamedblk2__DOT__i = 0;
    VL_RAND_RESET_W(69, vlSelf->__Vlvbound_hd17915af__0);
    vlSelf->__PVT__memReqQueue__DOT__pop = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memReqQueue__DOT__count = VL_RAND_RESET_I(5);
    vlSelf->__PVT__memReqQueue__DOT__countReg = VL_RAND_RESET_I(5);
    vlSelf->__PVT__memReqQueue__DOT__randReg = VL_RAND_RESET_I(32);
    vlSelf->__PVT__memReqQueue__DOT__randNext = VL_RAND_RESET_I(32);
    vlSelf->__PVT__memReqQueue__DOT__RANDOM_VALUE = VL_RAND_RESET_I(32);
    for (int __Vi0 = 0; __Vi0 < 128; ++__Vi0) {
        VL_RAND_RESET_W(102, vlSelf->__PVT__memReqQueue__DOT__memoryRequestQueue[__Vi0]);
    }
    vlSelf->__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage = VL_RAND_RESET_I(7);
    vlSelf->__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage = VL_RAND_RESET_I(7);
    vlSelf->__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage = VL_RAND_RESET_I(7);
    vlSelf->__PVT__memReqQueue__DOT__pointer__DOT__nextTailStorage = VL_RAND_RESET_I(7);
    vlSelf->__PVT__memReqQueue__DOT__pointer__DOT__regCount = VL_RAND_RESET_I(8);
    vlSelf->__PVT__memReqQueue__DOT__pointer__DOT__nextCount = VL_RAND_RESET_I(8);
}
