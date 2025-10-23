// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "VSMT_RTL_Testbench__Syms.h"


void VSMT_RTL_Testbench___024root__trace_chg_0_sub_5(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root__trace_chg_0_sub_5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode + 15786);
    VlWide<4>/*127:0*/ __Vtemp_1;
    VlWide<4>/*127:0*/ __Vtemp_2;
    VlWide<4>/*127:0*/ __Vtemp_3;
    VlWide<4>/*127:0*/ __Vtemp_4;
    VlWide<4>/*127:0*/ __Vtemp_5;
    // Body
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x41U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x88U])))) {
        bufp->chgBit(oldp+0,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
                                    [1U] >> 0xaU))));
        bufp->chgSData(oldp+1,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
                                [1U])),10);
        bufp->chgBit(oldp+2,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn
                                    [0U] >> 0x13U))));
        bufp->chgIData(oldp+3,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn
                                [0U])),19);
        bufp->chgBit(oldp+4,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn
                                    [1U] >> 0x13U))));
        bufp->chgIData(oldp+5,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcIn
                                [1U])),19);
        bufp->chgBit(oldp+6,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__icNextReadAddrIn 
                                    >> 0x15U))));
        bufp->chgBit(oldp+7,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__icNextReadAddrIn 
                                    >> 0x14U))));
        bufp->chgIData(oldp+8,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__icNextReadAddrIn)),20);
        bufp->chgSData(oldp+9,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                          [0U] >> 0x15U))),10);
        bufp->chgBit(oldp+10,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                     [0U] >> 0x14U))));
        bufp->chgBit(oldp+11,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                     [0U] >> 0x13U))));
        bufp->chgIData(oldp+12,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                 [0U])),19);
        bufp->chgSData(oldp+13,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                           [1U] >> 0x15U))),10);
        bufp->chgBit(oldp+14,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                     [1U] >> 0x14U))));
        bufp->chgBit(oldp+15,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                     [1U] >> 0x13U))));
        bufp->chgIData(oldp+16,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__nextStage
                                 [1U])),19);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x42U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x89U])))) {
        bufp->chgBit(oldp+17,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__stall));
        bufp->chgBit(oldp+18,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__clear));
        bufp->chgBit(oldp+19,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__flush[0]));
        bufp->chgBit(oldp+20,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__flush[1]));
        bufp->chgBit(oldp+21,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__valid[0]));
        bufp->chgBit(oldp+22,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__valid[1]));
        bufp->chgSData(oldp+23,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0xeU))),10);
        bufp->chgCData(oldp+24,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [0U][4U] >> 0xcU))),2);
        bufp->chgBit(oldp+25,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][4U] >> 0xbU))));
        bufp->chgSData(oldp+26,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 1U))),10);
        bufp->chgCData(oldp+27,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                        [0U][4U] << 1U) 
                                       | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+28,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+29,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+30,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                         [0U][3U] >> 0x17U))),4);
        bufp->chgBit(oldp+31,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+32,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                                 [0U][3U] 
                                                 << 8U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                                   [0U][2U] 
                                                   >> 0x18U)))),30);
        bufp->chgIData(oldp+33,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [0U][2U] 
                                             >> 6U))),18);
        bufp->chgBit(oldp+34,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+35,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [0U][3U] 
                                             >> 7U))),19);
        bufp->chgBit(oldp+36,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][3U] >> 6U))));
        bufp->chgSData(oldp+37,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                            [0U][3U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                              [0U][2U] 
                                              >> 0x1cU)))),10);
        bufp->chgCData(oldp+38,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+39,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [0U][2U] 
                                             >> 6U))),20);
        bufp->chgCData(oldp+40,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+41,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                 [0U][2U])),3);
        bufp->chgCData(oldp+42,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                 [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+43,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                         [0U][1U] >> 0x16U))),4);
        bufp->chgCData(oldp+44,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                         [0U][1U] >> 0x12U))),4);
        bufp->chgBit(oldp+45,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+46,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0xbU))),6);
        bufp->chgBit(oldp+47,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+48,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 4U))),6);
        bufp->chgBit(oldp+49,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][1U] >> 3U))));
        bufp->chgCData(oldp+50,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [0U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [0U][0U] 
                                           >> 0x1dU)))),6);
        bufp->chgBit(oldp+51,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+52,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+53,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [0U][0U] 
                                          >> 0x15U))),6);
        bufp->chgBit(oldp+54,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+55,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [0U][0U] 
                                             >> 1U))),19);
        bufp->chgBit(oldp+56,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                               [0U][0U])));
        bufp->chgSData(oldp+57,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0xeU))),10);
        bufp->chgCData(oldp+58,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [1U][4U] >> 0xcU))),2);
        bufp->chgBit(oldp+59,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][4U] >> 0xbU))));
        bufp->chgSData(oldp+60,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 1U))),10);
        bufp->chgCData(oldp+61,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                        [1U][4U] << 1U) 
                                       | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [1U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+62,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+63,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+64,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                         [1U][3U] >> 0x17U))),4);
        bufp->chgBit(oldp+65,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+66,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                                 [1U][3U] 
                                                 << 8U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                                   [1U][2U] 
                                                   >> 0x18U)))),30);
        bufp->chgIData(oldp+67,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [1U][2U] 
                                             >> 6U))),18);
        bufp->chgBit(oldp+68,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+69,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [1U][3U] 
                                             >> 7U))),19);
        bufp->chgBit(oldp+70,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][3U] >> 6U))));
        bufp->chgSData(oldp+71,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                            [1U][3U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                              [1U][2U] 
                                              >> 0x1cU)))),10);
        bufp->chgCData(oldp+72,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+73,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [1U][2U] 
                                             >> 6U))),20);
        bufp->chgCData(oldp+74,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                       [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+75,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                 [1U][2U])),3);
        bufp->chgCData(oldp+76,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                 [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+77,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                         [1U][1U] >> 0x16U))),4);
        bufp->chgCData(oldp+78,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                         [1U][1U] >> 0x12U))),4);
        bufp->chgBit(oldp+79,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+80,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [1U][1U] 
                                          >> 0xbU))),6);
        bufp->chgBit(oldp+81,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+82,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [1U][1U] 
                                          >> 4U))),6);
        bufp->chgBit(oldp+83,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][1U] >> 3U))));
        bufp->chgCData(oldp+84,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [1U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                           [1U][0U] 
                                           >> 0x1dU)))),6);
        bufp->chgBit(oldp+85,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+86,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+87,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                          [1U][0U] 
                                          >> 0x15U))),6);
        bufp->chgBit(oldp+88,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                     [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+89,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                                             [1U][0U] 
                                             >> 1U))),19);
        bufp->chgBit(oldp+90,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__nextStage
                               [1U][0U])));
        bufp->chgSData(oldp+91,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [0U][4U] 
                                           >> 1U))),10);
        bufp->chgCData(oldp+92,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                        [0U][4U] << 1U) 
                                       | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+93,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                       [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+94,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                       [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+95,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                         [0U][3U] >> 0x17U))),4);
        bufp->chgBit(oldp+96,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                     [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+97,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                                 [0U][3U] 
                                                 << 8U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                                   [0U][2U] 
                                                   >> 0x18U)))),30);
        bufp->chgIData(oldp+98,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                             [0U][2U] 
                                             >> 6U))),18);
        bufp->chgBit(oldp+99,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                     [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+100,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [0U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+101,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [0U][3U] >> 6U))));
        bufp->chgSData(oldp+102,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                             [0U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                               [0U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+103,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                        [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+104,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [0U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+105,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                        [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+106,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                  [0U][2U])),3);
        bufp->chgCData(oldp+107,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+108,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+109,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+110,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+111,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+112,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+113,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+114,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+115,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+116,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+117,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+118,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+119,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+120,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+121,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                [0U][0U])));
        bufp->chgSData(oldp+122,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                            [1U][4U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+123,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                         [1U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [1U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgCData(oldp+124,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                        [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+125,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                        [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+126,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                          [1U][3U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+127,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+128,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                                  [1U][3U] 
                                                  << 8U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                                    [1U][2U] 
                                                    >> 0x18U)))),30);
        bufp->chgIData(oldp+129,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [1U][2U] 
                                              >> 6U))),18);
        bufp->chgBit(oldp+130,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+131,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [1U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+132,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][3U] >> 6U))));
        bufp->chgSData(oldp+133,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                             [1U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                               [1U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+134,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                        [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+135,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [1U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+136,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                        [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+137,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                  [1U][2U])),3);
        bufp->chgCData(oldp+138,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                  [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+139,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                          [1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+140,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                          [1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+142,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+144,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][1U] >> 3U))));
        bufp->chgCData(oldp+146,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                            [1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+147,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+148,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+149,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                           [1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+150,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                      [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+151,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                              [1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+152,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issuedData
                                [1U][0U])));
        bufp->chgCData(oldp+153,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issueQueuePtr[0]),4);
        bufp->chgCData(oldp+154,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__issueQueuePtr[1]),4);
        bufp->chgIData(oldp+155,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+156,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgCData(oldp+157,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__intIssuePtr[0]),4);
        bufp->chgCData(oldp+158,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__intIssuePtr[1]),4);
        bufp->chgSData(oldp+159,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 0xeU))),10);
        bufp->chgCData(oldp+160,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [0U][4U] >> 0xcU))),2);
        bufp->chgBit(oldp+161,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][4U] >> 0xbU))));
        bufp->chgSData(oldp+162,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+163,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                         [0U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgCData(oldp+164,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+165,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+166,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                          [0U][3U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+167,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+168,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                                  [0U][3U] 
                                                  << 8U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                                    [0U][2U] 
                                                    >> 0x18U)))),30);
        bufp->chgIData(oldp+169,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [0U][2U] 
                                              >> 6U))),18);
        bufp->chgBit(oldp+170,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+171,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [0U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+172,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][3U] >> 6U))));
        bufp->chgSData(oldp+173,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                             [0U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                               [0U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+174,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+175,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [0U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+176,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+177,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                  [0U][2U])),3);
        bufp->chgCData(oldp+178,((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+179,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+180,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+181,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+182,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+183,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+184,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+185,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+186,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+187,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+188,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+189,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+190,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+191,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+192,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+193,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 0xeU))),10);
        bufp->chgCData(oldp+194,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [1U][4U] >> 0xcU))),2);
        bufp->chgBit(oldp+195,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][4U] >> 0xbU))));
        bufp->chgSData(oldp+196,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+197,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                         [1U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgCData(oldp+198,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+199,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+200,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                          [1U][3U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+201,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+202,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                                  [1U][3U] 
                                                  << 8U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                                    [1U][2U] 
                                                    >> 0x18U)))),30);
        bufp->chgIData(oldp+203,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [1U][2U] 
                                              >> 6U))),18);
        bufp->chgBit(oldp+204,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+205,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [1U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+206,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][3U] >> 6U))));
        bufp->chgSData(oldp+207,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                             [1U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                               [1U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+208,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+209,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [1U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+210,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                        [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+211,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                  [1U][2U])),3);
        bufp->chgCData(oldp+212,((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                  [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+213,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                          [1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+214,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                          [1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+215,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+216,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+217,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+218,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+219,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][1U] >> 3U))));
        bufp->chgCData(oldp+220,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                            [1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+222,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+223,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                           [1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+224,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+225,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                              [1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+226,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__intIsStageIF.__PVT__nextStage
                                [1U][0U])));
        bufp->chgBit(oldp+227,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssue[0]));
        bufp->chgBit(oldp+228,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssue[1]));
        bufp->chgCData(oldp+229,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuePtr[0]),4);
        bufp->chgCData(oldp+230,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuePtr[1]),4);
        bufp->chgBit(oldp+231,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                      [0U] >> 0xdU))));
        bufp->chgBit(oldp+232,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                      [0U] >> 0xcU))));
        bufp->chgSData(oldp+233,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                            [0U] >> 2U))),10);
        bufp->chgCData(oldp+234,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                  [0U])),2);
        bufp->chgBit(oldp+235,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                      [1U] >> 0xdU))));
        bufp->chgBit(oldp+236,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                      [1U] >> 0xcU))));
        bufp->chgSData(oldp+237,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                            [1U] >> 2U))),10);
        bufp->chgCData(oldp+238,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
                                  [1U])),2);
        bufp->chgCData(oldp+239,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__ra[0]),4);
        bufp->chgCData(oldp+240,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__ra[1]),4);
        bufp->chgCData(oldp+241,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr[0]),4);
        bufp->chgCData(oldp+242,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr[1]),4);
        bufp->chgCData(oldp+243,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr
                                 [0U]),4);
        bufp->chgCData(oldp+244,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr
                                 [1U]),4);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x43U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8aU])))) {
        bufp->chgBit(oldp+245,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__stall));
        bufp->chgBit(oldp+246,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__clear));
        bufp->chgBit(oldp+247,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__flush[0]));
        bufp->chgBit(oldp+248,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__flush[1]));
        bufp->chgBit(oldp+249,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__valid[0]));
        bufp->chgBit(oldp+250,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__valid[1]));
        bufp->chgSData(oldp+251,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+252,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][4U] >> 3U))),2);
        bufp->chgBit(oldp+253,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][4U] >> 2U))));
        bufp->chgSData(oldp+254,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                             [0U][4U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                               [0U][3U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+255,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][3U] >> 0x16U))),2);
        bufp->chgCData(oldp+256,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][3U] >> 0x13U))),3);
        bufp->chgCData(oldp+257,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][3U] >> 0x10U))),3);
        bufp->chgCData(oldp+258,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][3U] >> 0xeU))),2);
        bufp->chgCData(oldp+259,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][3U] >> 0xcU))),2);
        bufp->chgSData(oldp+260,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                  [0U][3U])),12);
        bufp->chgBit(oldp+261,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                [0U][2U] >> 0x1fU)));
        bufp->chgBit(oldp+262,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][2U] >> 0x1eU))));
        bufp->chgBit(oldp+263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][2U] >> 0x1dU))));
        bufp->chgCData(oldp+264,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][2U] >> 0x1bU))),2);
        bufp->chgCData(oldp+265,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 0x16U))),5);
        bufp->chgBit(oldp+266,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][2U] >> 0x15U))));
        bufp->chgCData(oldp+267,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][2U] >> 0x13U))),2);
        bufp->chgCData(oldp+268,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [0U][2U] >> 0x10U))),3);
        bufp->chgBit(oldp+269,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][2U] >> 0xfU))));
        bufp->chgCData(oldp+270,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [0U][2U] 
                                          >> 0xbU))),4);
        bufp->chgCData(oldp+271,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [0U][2U] 
                                          >> 7U))),4);
        bufp->chgBit(oldp+272,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][2U] >> 6U))));
        bufp->chgBit(oldp+273,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][2U] >> 5U))));
        bufp->chgCData(oldp+274,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                            [0U][2U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                              [0U][1U] 
                                              >> 0x1fU)))),6);
        bufp->chgCData(oldp+275,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x1bU))),4);
        bufp->chgCData(oldp+276,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+277,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][1U] >> 0x16U))));
        bufp->chgCData(oldp+278,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+279,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][1U] >> 0xfU))));
        bufp->chgCData(oldp+280,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 9U))),6);
        bufp->chgBit(oldp+281,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][1U] >> 8U))));
        bufp->chgCData(oldp+282,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+283,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][1U] >> 1U))));
        bufp->chgBit(oldp+284,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                [0U][1U])));
        bufp->chgCData(oldp+285,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                  [0U][0U] >> 0x1aU)),6);
        bufp->chgBit(oldp+286,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][0U] >> 0x19U))));
        bufp->chgIData(oldp+287,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                              [0U][0U] 
                                              >> 6U))),19);
        bufp->chgBit(oldp+288,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [0U][0U] >> 5U))));
        bufp->chgCData(oldp+289,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [0U][0U] 
                                          >> 1U))),4);
        bufp->chgBit(oldp+290,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+291,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+292,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][4U] >> 3U))),2);
        bufp->chgBit(oldp+293,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][4U] >> 2U))));
        bufp->chgSData(oldp+294,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                             [1U][4U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                               [1U][3U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+295,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][3U] >> 0x16U))),2);
        bufp->chgCData(oldp+296,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][3U] >> 0x13U))),3);
        bufp->chgCData(oldp+297,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][3U] >> 0x10U))),3);
        bufp->chgCData(oldp+298,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][3U] >> 0xeU))),2);
        bufp->chgCData(oldp+299,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][3U] >> 0xcU))),2);
        bufp->chgSData(oldp+300,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                  [1U][3U])),12);
        bufp->chgBit(oldp+301,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                [1U][2U] >> 0x1fU)));
        bufp->chgBit(oldp+302,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][2U] >> 0x1eU))));
        bufp->chgBit(oldp+303,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][2U] >> 0x1dU))));
        bufp->chgCData(oldp+304,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][2U] >> 0x1bU))),2);
        bufp->chgCData(oldp+305,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [1U][2U] 
                                           >> 0x16U))),5);
        bufp->chgBit(oldp+306,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][2U] >> 0x15U))));
        bufp->chgCData(oldp+307,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][2U] >> 0x13U))),2);
        bufp->chgCData(oldp+308,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                        [1U][2U] >> 0x10U))),3);
        bufp->chgBit(oldp+309,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][2U] >> 0xfU))));
        bufp->chgCData(oldp+310,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [1U][2U] 
                                          >> 0xbU))),4);
        bufp->chgCData(oldp+311,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [1U][2U] 
                                          >> 7U))),4);
        bufp->chgBit(oldp+312,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][2U] >> 6U))));
        bufp->chgBit(oldp+313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][2U] >> 5U))));
        bufp->chgCData(oldp+314,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                            [1U][2U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                              [1U][1U] 
                                              >> 0x1fU)))),6);
        bufp->chgCData(oldp+315,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [1U][1U] 
                                          >> 0x1bU))),4);
        bufp->chgCData(oldp+316,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [1U][1U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+317,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][1U] >> 0x16U))));
        bufp->chgCData(oldp+318,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+319,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][1U] >> 0xfU))));
        bufp->chgCData(oldp+320,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 9U))),6);
        bufp->chgBit(oldp+321,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][1U] >> 8U))));
        bufp->chgCData(oldp+322,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+323,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][1U] >> 1U))));
        bufp->chgBit(oldp+324,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                [1U][1U])));
        bufp->chgCData(oldp+325,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                  [1U][0U] >> 0x1aU)),6);
        bufp->chgBit(oldp+326,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][0U] >> 0x19U))));
        bufp->chgIData(oldp+327,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                              [1U][0U] 
                                              >> 6U))),19);
        bufp->chgBit(oldp+328,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                      [1U][0U] >> 5U))));
        bufp->chgCData(oldp+329,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                          [1U][0U] 
                                          >> 1U))),4);
        bufp->chgBit(oldp+330,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__nextStage
                                [1U][0U])));
        bufp->chgSData(oldp+331,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                            [0U][3U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+332,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+333,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+334,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+335,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+336,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+337,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                             [0U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                               [0U][2U] 
                                               >> 0x1bU)))),12);
        bufp->chgBit(oldp+338,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+339,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+340,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+341,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+342,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [0U][2U] 
                                           >> 0x11U))),5);
        bufp->chgBit(oldp+343,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+344,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+345,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+346,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+347,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [0U][2U] 
                                          >> 6U))),4);
        bufp->chgCData(oldp+348,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [0U][2U] 
                                          >> 2U))),4);
        bufp->chgBit(oldp+349,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][2U] >> 1U))));
        bufp->chgBit(oldp+350,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                [0U][2U])));
        bufp->chgCData(oldp+351,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+352,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+353,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+354,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+355,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+357,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+358,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+359,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+360,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+361,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+362,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+363,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+364,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+365,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                [0U][0U])));
        bufp->chgSData(oldp+366,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                            [1U][3U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+367,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+368,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+369,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+370,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][3U] >> 9U))),2);
        bufp->chgCData(oldp+371,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][3U] >> 7U))),2);
        bufp->chgSData(oldp+372,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                             [1U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                               [1U][2U] 
                                               >> 0x1bU)))),12);
        bufp->chgBit(oldp+373,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+374,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+375,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][2U] >> 0x18U))));
        bufp->chgCData(oldp+376,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+377,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [1U][2U] 
                                           >> 0x11U))),5);
        bufp->chgBit(oldp+378,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][2U] >> 0x10U))));
        bufp->chgCData(oldp+379,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+380,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                        [1U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+381,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][2U] >> 0xaU))));
        bufp->chgCData(oldp+382,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [1U][2U] 
                                          >> 6U))),4);
        bufp->chgCData(oldp+383,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [1U][2U] 
                                          >> 2U))),4);
        bufp->chgBit(oldp+384,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][2U] >> 1U))));
        bufp->chgBit(oldp+385,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                [1U][2U])));
        bufp->chgCData(oldp+386,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                  [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+387,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+388,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                          [1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+389,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+390,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+391,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+392,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+393,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][1U] >> 3U))));
        bufp->chgCData(oldp+394,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                            [1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                              [1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+395,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+396,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+397,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                           [1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+398,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                      [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+399,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                              [1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+400,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issuedData
                                [1U][0U])));
        bufp->chgCData(oldp+401,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issueQueuePtr[0]),4);
        bufp->chgCData(oldp+402,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__issueQueuePtr[1]),4);
        bufp->chgIData(oldp+403,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+404,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgCData(oldp+405,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__memIssuePtr[0]),4);
        bufp->chgCData(oldp+406,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__memIssuePtr[1]),4);
        bufp->chgSData(oldp+407,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+408,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][4U] >> 3U))),2);
        bufp->chgBit(oldp+409,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][4U] >> 2U))));
        bufp->chgSData(oldp+410,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                             [0U][4U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+411,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][3U] >> 0x16U))),2);
        bufp->chgCData(oldp+412,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][3U] >> 0x13U))),3);
        bufp->chgCData(oldp+413,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][3U] >> 0x10U))),3);
        bufp->chgCData(oldp+414,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][3U] >> 0xeU))),2);
        bufp->chgCData(oldp+415,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][3U] >> 0xcU))),2);
        bufp->chgSData(oldp+416,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                  [0U][3U])),12);
        bufp->chgBit(oldp+417,((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                [0U][2U] >> 0x1fU)));
        bufp->chgBit(oldp+418,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x1eU))));
        bufp->chgBit(oldp+419,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x1dU))));
        bufp->chgCData(oldp+420,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 0x1bU))),2);
        bufp->chgCData(oldp+421,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [0U][2U] 
                                           >> 0x16U))),5);
        bufp->chgBit(oldp+422,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x15U))));
        bufp->chgCData(oldp+423,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 0x13U))),2);
        bufp->chgCData(oldp+424,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 0x10U))),3);
        bufp->chgBit(oldp+425,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0xfU))));
        bufp->chgCData(oldp+426,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [0U][2U] 
                                          >> 0xbU))),4);
        bufp->chgCData(oldp+427,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [0U][2U] 
                                          >> 7U))),4);
        bufp->chgBit(oldp+428,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 6U))));
        bufp->chgBit(oldp+429,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 5U))));
        bufp->chgCData(oldp+430,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 0x1fU)))),6);
        bufp->chgCData(oldp+431,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x1bU))),4);
        bufp->chgCData(oldp+432,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+433,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x16U))));
        bufp->chgCData(oldp+434,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+435,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0xfU))));
        bufp->chgCData(oldp+436,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 9U))),6);
        bufp->chgBit(oldp+437,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 8U))));
        bufp->chgCData(oldp+438,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+439,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 1U))));
        bufp->chgBit(oldp+440,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                [0U][1U])));
        bufp->chgCData(oldp+441,((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                  [0U][0U] >> 0x1aU)),6);
        bufp->chgBit(oldp+442,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x19U))));
        bufp->chgIData(oldp+443,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                              [0U][0U] 
                                              >> 6U))),19);
        bufp->chgBit(oldp+444,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 5U))));
        bufp->chgCData(oldp+445,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [0U][0U] 
                                          >> 1U))),4);
        bufp->chgBit(oldp+446,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+447,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+448,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][4U] >> 3U))),2);
        bufp->chgBit(oldp+449,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][4U] >> 2U))));
        bufp->chgSData(oldp+450,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                             [1U][4U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+451,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][3U] >> 0x16U))),2);
        bufp->chgCData(oldp+452,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][3U] >> 0x13U))),3);
        bufp->chgCData(oldp+453,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][3U] >> 0x10U))),3);
        bufp->chgCData(oldp+454,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][3U] >> 0xeU))),2);
        bufp->chgCData(oldp+455,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][3U] >> 0xcU))),2);
        bufp->chgSData(oldp+456,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                  [1U][3U])),12);
        bufp->chgBit(oldp+457,((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                [1U][2U] >> 0x1fU)));
        bufp->chgBit(oldp+458,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x1eU))));
        bufp->chgBit(oldp+459,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x1dU))));
        bufp->chgCData(oldp+460,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][2U] >> 0x1bU))),2);
        bufp->chgCData(oldp+461,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [1U][2U] 
                                           >> 0x16U))),5);
        bufp->chgBit(oldp+462,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x15U))));
        bufp->chgCData(oldp+463,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][2U] >> 0x13U))),2);
        bufp->chgCData(oldp+464,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                        [1U][2U] >> 0x10U))),3);
        bufp->chgBit(oldp+465,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][2U] >> 0xfU))));
        bufp->chgCData(oldp+466,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [1U][2U] 
                                          >> 0xbU))),4);
        bufp->chgCData(oldp+467,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [1U][2U] 
                                          >> 7U))),4);
        bufp->chgBit(oldp+468,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][2U] >> 6U))));
        bufp->chgBit(oldp+469,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][2U] >> 5U))));
        bufp->chgCData(oldp+470,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                            [1U][2U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                              [1U][1U] 
                                              >> 0x1fU)))),6);
        bufp->chgCData(oldp+471,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [1U][1U] 
                                          >> 0x1bU))),4);
        bufp->chgCData(oldp+472,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [1U][1U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+473,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x16U))));
        bufp->chgCData(oldp+474,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+475,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][1U] >> 0xfU))));
        bufp->chgCData(oldp+476,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 9U))),6);
        bufp->chgBit(oldp+477,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][1U] >> 8U))));
        bufp->chgCData(oldp+478,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+479,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][1U] >> 1U))));
        bufp->chgBit(oldp+480,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                [1U][1U])));
        bufp->chgCData(oldp+481,((vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                  [1U][0U] >> 0x1aU)),6);
        bufp->chgBit(oldp+482,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x19U))));
        bufp->chgIData(oldp+483,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                              [1U][0U] 
                                              >> 6U))),19);
        bufp->chgBit(oldp+484,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                      [1U][0U] >> 5U))));
        bufp->chgCData(oldp+485,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                          [1U][0U] 
                                          >> 1U))),4);
        bufp->chgBit(oldp+486,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__memIsStageIF.__PVT__nextStage
                                [1U][0U])));
        bufp->chgBit(oldp+487,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssue[0]));
        bufp->chgBit(oldp+488,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssue[1]));
        bufp->chgCData(oldp+489,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuePtr[0]),4);
        bufp->chgCData(oldp+490,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuePtr[1]),4);
        bufp->chgBit(oldp+491,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                      [0U] >> 0xdU))));
        bufp->chgBit(oldp+492,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                      [0U] >> 0xcU))));
        bufp->chgSData(oldp+493,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                            [0U] >> 2U))),10);
        bufp->chgCData(oldp+494,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                  [0U])),2);
        bufp->chgBit(oldp+495,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                      [1U] >> 0xdU))));
        bufp->chgBit(oldp+496,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                      [1U] >> 0xcU))));
        bufp->chgSData(oldp+497,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                            [1U] >> 2U))),10);
        bufp->chgCData(oldp+498,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
                                  [1U])),2);
        bufp->chgCData(oldp+499,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__ra[0]),4);
        bufp->chgCData(oldp+500,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__ra[1]),4);
        bufp->chgCData(oldp+501,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr[0]),4);
        bufp->chgCData(oldp+502,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr[1]),4);
        bufp->chgCData(oldp+503,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr
                                 [0U]),4);
        bufp->chgCData(oldp+504,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr
                                 [1U]),4);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x44U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8bU])))) {
        bufp->chgBit(oldp+505,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__stall));
        bufp->chgBit(oldp+506,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__clear));
        bufp->chgBit(oldp+507,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__flush[0]));
        bufp->chgBit(oldp+508,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__valid[0]));
        bufp->chgSData(oldp+509,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                  [0U][2U] >> 0x16U)),10);
        bufp->chgCData(oldp+510,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                        [0U][2U] >> 0x14U))),2);
        bufp->chgBit(oldp+511,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][2U] >> 0x13U))));
        bufp->chgBit(oldp+512,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][2U] >> 0x12U))));
        bufp->chgSData(oldp+513,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                            [0U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+514,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                        [0U][2U] >> 6U))),2);
        bufp->chgBit(oldp+515,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][2U] >> 5U))));
        bufp->chgCData(oldp+516,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                        [0U][2U] >> 3U))),2);
        bufp->chgCData(oldp+517,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                  [0U][2U])),3);
        bufp->chgCData(oldp+518,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+519,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+520,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+521,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+522,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+523,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+524,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+525,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+526,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+527,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+528,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+529,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+530,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+531,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+532,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+533,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                            [0U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+534,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                        [0U][2U] >> 6U))),2);
        bufp->chgBit(oldp+535,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                      [0U][2U] >> 5U))));
        bufp->chgCData(oldp+536,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                        [0U][2U] >> 3U))),2);
        bufp->chgCData(oldp+537,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                  [0U][2U])),3);
        bufp->chgCData(oldp+538,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+539,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+540,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+541,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+542,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+543,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+544,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+546,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+547,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+548,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+549,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+550,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+551,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+552,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issuedData
                                [0U][0U])));
        bufp->chgCData(oldp+553,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__issueQueuePtr[0]),4);
        bufp->chgIData(oldp+554,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+555,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgCData(oldp+556,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__complexIssuePtr[0]),4);
        bufp->chgSData(oldp+557,((vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                  [0U][2U] >> 0x16U)),10);
        bufp->chgCData(oldp+558,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 0x14U))),2);
        bufp->chgBit(oldp+559,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x13U))));
        bufp->chgBit(oldp+560,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x12U))));
        bufp->chgSData(oldp+561,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+562,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 6U))),2);
        bufp->chgBit(oldp+563,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 5U))));
        bufp->chgCData(oldp+564,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 3U))),2);
        bufp->chgCData(oldp+565,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                  [0U][2U])),3);
        bufp->chgCData(oldp+566,((vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+567,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+568,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+569,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+570,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+572,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+574,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+575,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+576,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+577,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+578,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+579,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+580,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__complexIsStageIF.__PVT__nextStage
                                [0U][0U])));
        bufp->chgBit(oldp+581,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divAcquire[0]));
        bufp->chgCData(oldp+582,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__acquireActiveListPtr[0]),6);
        bufp->chgBit(oldp+583,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssue[0]));
        bufp->chgCData(oldp+584,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuePtr[0]),4);
        bufp->chgBit(oldp+585,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
                                      [0U] >> 0xdU))));
        bufp->chgBit(oldp+586,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
                                      [0U] >> 0xcU))));
        bufp->chgSData(oldp+587,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
                                            [0U] >> 2U))),10);
        bufp->chgCData(oldp+588,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
                                  [0U])),2);
        bufp->chgCData(oldp+589,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__ra[0]),4);
        bufp->chgCData(oldp+590,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr[0]),4);
        bufp->chgCData(oldp+591,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr
                                 [0U]),4);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x45U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8cU])))) {
        bufp->chgBit(oldp+592,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__stall));
        bufp->chgBit(oldp+593,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__clear));
        bufp->chgBit(oldp+594,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__flush[0]));
        bufp->chgBit(oldp+595,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__valid[0]));
        bufp->chgSData(oldp+596,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+597,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                         [0U][3U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 0x1fU)))),2);
        bufp->chgBit(oldp+598,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][2U] >> 0x1eU))));
        bufp->chgBit(oldp+599,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][2U] >> 0x1dU))));
        bufp->chgSData(oldp+600,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                            [0U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+601,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                        [0U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+602,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                        [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+603,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+604,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                        [0U][2U] >> 6U))),3);
        bufp->chgCData(oldp+605,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                        [0U][2U] >> 4U))),2);
        bufp->chgCData(oldp+606,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                        [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+607,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                  [0U][2U])),2);
        bufp->chgCData(oldp+608,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+609,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+610,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+611,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+612,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+613,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+614,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+615,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+616,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+617,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+618,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+619,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+620,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+621,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+622,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+623,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                            [0U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+624,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                        [0U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+625,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                        [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+626,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                           [0U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+627,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                        [0U][2U] >> 6U))),3);
        bufp->chgCData(oldp+628,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                        [0U][2U] >> 4U))),2);
        bufp->chgCData(oldp+629,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                        [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+630,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                  [0U][2U])),2);
        bufp->chgCData(oldp+631,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+632,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+633,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+634,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+635,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+636,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+637,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+638,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+639,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+640,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+641,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+642,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+643,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+644,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+645,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issuedData
                                [0U][0U])));
        bufp->chgCData(oldp+646,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__issueQueuePtr[0]),4);
        bufp->chgIData(oldp+647,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+648,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgCData(oldp+649,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__fpIssuePtr[0]),4);
        bufp->chgSData(oldp+650,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+651,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                         [0U][3U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                           [0U][2U] 
                                           >> 0x1fU)))),2);
        bufp->chgBit(oldp+652,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x1eU))));
        bufp->chgBit(oldp+653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x1dU))));
        bufp->chgSData(oldp+654,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+655,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+656,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+657,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                           [0U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+658,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 6U))),3);
        bufp->chgCData(oldp+659,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 4U))),2);
        bufp->chgCData(oldp+660,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                        [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+661,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                  [0U][2U])),2);
        bufp->chgCData(oldp+662,((vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+663,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+664,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+665,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+666,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+667,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+668,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+669,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+670,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+671,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+672,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+673,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+674,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+675,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+676,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__fpIsStageIF.__PVT__nextStage
                                [0U][0U])));
        bufp->chgBit(oldp+677,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Acquire[0]));
        bufp->chgCData(oldp+678,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__acquireActiveListPtr[0]),6);
        bufp->chgBit(oldp+679,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssue[0]));
        bufp->chgCData(oldp+680,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuePtr[0]),4);
        bufp->chgBit(oldp+681,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
                                      [0U] >> 0xdU))));
        bufp->chgBit(oldp+682,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
                                      [0U] >> 0xcU))));
        bufp->chgSData(oldp+683,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
                                            [0U] >> 2U))),10);
        bufp->chgCData(oldp+684,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
                                  [0U])),2);
        bufp->chgCData(oldp+685,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__ra[0]),4);
        bufp->chgCData(oldp+686,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr[0]),4);
        bufp->chgCData(oldp+687,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__genblk1__DOT__body__DOT__rbReadAddr
                                 [0U]),4);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x46U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8dU])))) {
        bufp->chgBit(oldp+688,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__valid[0]));
        bufp->chgBit(oldp+689,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__valid[1]));
        bufp->chgBit(oldp+690,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__hit));
        bufp->chgCData(oldp+691,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__hitArray),2);
        bufp->chgBit(oldp+692,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__hitWay));
        bufp->chgBit(oldp+693,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__we[0]));
        bufp->chgBit(oldp+694,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__we[1]));
        bufp->chgCData(oldp+695,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readIndex),8);
        bufp->chgCData(oldp+696,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextReadIndex),8);
        bufp->chgSData(oldp+697,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readTag),11);
        bufp->chgCData(oldp+698,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__updatedNRUState),2);
        bufp->chgCData(oldp+699,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__wayToEvictOneHot),2);
        bufp->chgBit(oldp+700,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__wayToEvict));
        bufp->chgBit(oldp+701,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__wordPtr[0]));
        bufp->chgBit(oldp+702,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__wordPtr[1]));
        bufp->chgBit(oldp+703,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__we
                               [0U]));
        bufp->chgBit(oldp+704,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__hit));
        bufp->chgBit(oldp+705,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__valid));
        bufp->chgBit(oldp+706,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__we
                               [1U]));
        bufp->chgBit(oldp+707,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__hit));
        bufp->chgBit(oldp+708,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__valid));
        bufp->chgBit(oldp+709,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT____Vcellinp__iCacheHitLogic__hitIn));
        bufp->chgBit(oldp+710,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__wordPtr
                               [0U]));
        bufp->chgBit(oldp+711,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT____Vcellout__iCacheHitLogic__hitOut[0]));
        bufp->chgBit(oldp+712,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT____Vcellout__iCacheHitLogic__hitOut[1]));
        bufp->chgCData(oldp+713,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__iCacheHitLogic__DOT__wordPtr[0]),2);
        bufp->chgCData(oldp+714,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__iCacheHitLogic__DOT__wordPtr[1]),2);
        bufp->chgCData(oldp+715,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nruStateArray__DOT__writeNRUStateIndex),8);
        bufp->chgCData(oldp+716,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nruStateArray__DOT__writeNRUStateData),2);
        bufp->chgIData(oldp+717,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+718,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__unnamedblk2__DOT__i),32);
        bufp->chgBit(oldp+719,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__stall));
        bufp->chgBit(oldp+720,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__beginStall));
        bufp->chgBit(oldp+721,(vlSymsp->TOP__SMT_RTL_Testbench__core__perfCounterIF.__PVT__icMiss));
        bufp->chgBit(oldp+722,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadHit[0]));
        bufp->chgBit(oldp+723,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadHit[1]));
        bufp->chgIData(oldp+724,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadDataOut[0]),32);
        bufp->chgIData(oldp+725,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__icReadDataOut[1]),32);
        bufp->chgBit(oldp+726,(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStageSendBubbleLower));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x47U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8eU])))) {
        bufp->chgSData(oldp+727,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                  [0U][0U][2U] >> 0x16U)),10);
        bufp->chgCData(oldp+728,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 0x14U))),2);
        bufp->chgBit(oldp+729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][2U] 
                                      >> 0x13U))));
        bufp->chgBit(oldp+730,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][2U] 
                                      >> 0x12U))));
        bufp->chgSData(oldp+731,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                            [0U][0U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+732,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 6U))),2);
        bufp->chgBit(oldp+733,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][2U] 
                                      >> 5U))));
        bufp->chgCData(oldp+734,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 3U))),2);
        bufp->chgCData(oldp+735,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                  [0U][0U][2U])),3);
        bufp->chgCData(oldp+736,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                  [0U][0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+737,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                          [0U][0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+738,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                          [0U][0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+739,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+740,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                           [0U][0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+741,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+742,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                           [0U][0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+743,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+744,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                            [0U][0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                              [0U][0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+745,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+747,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                           [0U][0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][0U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+749,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                              [0U][0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+750,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                [0U][0U][0U])));
        bufp->chgSData(oldp+751,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                  [0U][1U][2U] >> 0x16U)),10);
        bufp->chgCData(oldp+752,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 0x14U))),2);
        bufp->chgBit(oldp+753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][2U] 
                                      >> 0x13U))));
        bufp->chgBit(oldp+754,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][2U] 
                                      >> 0x12U))));
        bufp->chgSData(oldp+755,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                            [0U][1U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+756,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 6U))),2);
        bufp->chgBit(oldp+757,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][2U] 
                                      >> 5U))));
        bufp->chgCData(oldp+758,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 3U))),2);
        bufp->chgCData(oldp+759,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                  [0U][1U][2U])),3);
        bufp->chgCData(oldp+760,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                  [0U][1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+761,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                          [0U][1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+762,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                          [0U][1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+763,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+764,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                           [0U][1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+765,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+766,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                           [0U][1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+767,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+768,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                            [0U][1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                              [0U][1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+769,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+770,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+771,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                           [0U][1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+772,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                      [0U][1U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+773,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                              [0U][1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+774,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextLocalPipeReg
                                [0U][1U][0U])));
        bufp->chgCData(oldp+775,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__nextPhase
                                 [0U]),2);
        bufp->chgBit(oldp+776,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__flush[0]));
        bufp->chgBit(oldp+777,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__rst_divider[0]));
        bufp->chgCData(oldp+778,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__nextActiveListPtr[0]),6);
        bufp->chgBit(oldp+779,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__rst_divider
                               [0U]));
        bufp->chgIData(oldp+780,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__unnamedblk2__DOT__i),32);
        bufp->chgBit(oldp+781,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__canIssueDiv));
        bufp->chgBit(oldp+782,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReserved[0]));
        bufp->chgBit(oldp+783,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divFinished[0]));
        bufp->chgBit(oldp+784,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divBusy[0]));
        bufp->chgBit(oldp+785,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divFree[0]));
        bufp->chgBit(oldp+786,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divRelease[0]));
        bufp->chgBit(oldp+787,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[0]));
        bufp->chgBit(oldp+788,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[1]));
        bufp->chgBit(oldp+789,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[2]));
        bufp->chgBit(oldp+790,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[3]));
        bufp->chgBit(oldp+791,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[4]));
        bufp->chgBit(oldp+792,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[5]));
        bufp->chgBit(oldp+793,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[6]));
        bufp->chgBit(oldp+794,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[7]));
        bufp->chgBit(oldp+795,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[8]));
        bufp->chgBit(oldp+796,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[9]));
        bufp->chgBit(oldp+797,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[10]));
        bufp->chgBit(oldp+798,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[11]));
        bufp->chgBit(oldp+799,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[12]));
        bufp->chgBit(oldp+800,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[13]));
        bufp->chgBit(oldp+801,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[14]));
        bufp->chgBit(oldp+802,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__complexIssueReq[15]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x48U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8fU])))) {
        bufp->chgCData(oldp+803,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__nextPhase
                                 [0U]),2);
        bufp->chgBit(oldp+804,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__flush[0]));
        bufp->chgBit(oldp+805,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__rst_divider[0]));
        bufp->chgCData(oldp+806,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__nextActiveListPtr[0]),6);
        bufp->chgBit(oldp+807,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__rst_divider
                               [0U]));
        bufp->chgIData(oldp+808,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__unnamedblk2__DOT__i),32);
        bufp->chgSData(oldp+809,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][0U][3U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+810,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                         [0U][0U][3U] 
                                         << 1U) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                                   [0U]
                                                   [0U][2U] 
                                                   >> 0x1fU)))),2);
        bufp->chgBit(oldp+811,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][2U] 
                                      >> 0x1eU))));
        bufp->chgBit(oldp+812,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][2U] 
                                      >> 0x1dU))));
        bufp->chgSData(oldp+813,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][0U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+814,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 0x11U))),2);
        bufp->chgCData(oldp+815,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 0xeU))),3);
        bufp->chgCData(oldp+816,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][0U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+817,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 6U))),3);
        bufp->chgCData(oldp+818,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 4U))),2);
        bufp->chgCData(oldp+819,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][0U][2U] 
                                        >> 2U))),2);
        bufp->chgCData(oldp+820,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][0U][2U])),2);
        bufp->chgCData(oldp+821,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+822,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+823,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+824,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+825,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+826,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+827,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+828,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+829,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+830,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+831,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+832,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+833,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][0U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+834,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+835,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                [0U][0U][0U])));
        bufp->chgSData(oldp+836,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][1U][3U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+837,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                         [0U][1U][3U] 
                                         << 1U) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                                   [0U]
                                                   [1U][2U] 
                                                   >> 0x1fU)))),2);
        bufp->chgBit(oldp+838,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][2U] 
                                      >> 0x1eU))));
        bufp->chgBit(oldp+839,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][2U] 
                                      >> 0x1dU))));
        bufp->chgSData(oldp+840,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][1U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+841,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 0x11U))),2);
        bufp->chgCData(oldp+842,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 0xeU))),3);
        bufp->chgCData(oldp+843,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][1U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+844,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 6U))),3);
        bufp->chgCData(oldp+845,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 4U))),2);
        bufp->chgCData(oldp+846,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][1U][2U] 
                                        >> 2U))),2);
        bufp->chgCData(oldp+847,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][1U][2U])),2);
        bufp->chgCData(oldp+848,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+849,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+850,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+851,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+852,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+853,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+854,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+855,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+856,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+857,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+858,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+859,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+860,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][1U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+861,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+862,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                [0U][1U][0U])));
        bufp->chgSData(oldp+863,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][2U][3U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+864,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                         [0U][2U][3U] 
                                         << 1U) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                                   [0U]
                                                   [2U][2U] 
                                                   >> 0x1fU)))),2);
        bufp->chgBit(oldp+865,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][2U] 
                                      >> 0x1eU))));
        bufp->chgBit(oldp+866,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][2U] 
                                      >> 0x1dU))));
        bufp->chgSData(oldp+867,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][2U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+868,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][2U][2U] 
                                        >> 0x11U))),2);
        bufp->chgCData(oldp+869,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][2U][2U] 
                                        >> 0xeU))),3);
        bufp->chgCData(oldp+870,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][2U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+871,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][2U][2U] 
                                        >> 6U))),3);
        bufp->chgCData(oldp+872,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][2U][2U] 
                                        >> 4U))),2);
        bufp->chgCData(oldp+873,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][2U][2U] 
                                        >> 2U))),2);
        bufp->chgCData(oldp+874,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][2U][2U])),2);
        bufp->chgCData(oldp+875,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][2U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+876,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][2U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+877,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][2U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+878,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+879,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][2U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+880,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+881,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][2U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+882,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+883,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][2U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][2U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+884,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+885,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+886,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][2U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+887,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][2U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+888,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][2U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+889,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                [0U][2U][0U])));
        bufp->chgSData(oldp+890,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][3U][3U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+891,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                         [0U][3U][3U] 
                                         << 1U) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                                   [0U]
                                                   [3U][2U] 
                                                   >> 0x1fU)))),2);
        bufp->chgBit(oldp+892,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][2U] 
                                      >> 0x1eU))));
        bufp->chgBit(oldp+893,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][2U] 
                                      >> 0x1dU))));
        bufp->chgSData(oldp+894,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][3U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+895,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][3U][2U] 
                                        >> 0x11U))),2);
        bufp->chgCData(oldp+896,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][3U][2U] 
                                        >> 0xeU))),3);
        bufp->chgCData(oldp+897,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][3U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+898,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][3U][2U] 
                                        >> 6U))),3);
        bufp->chgCData(oldp+899,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][3U][2U] 
                                        >> 4U))),2);
        bufp->chgCData(oldp+900,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                        [0U][3U][2U] 
                                        >> 2U))),2);
        bufp->chgCData(oldp+901,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][3U][2U])),2);
        bufp->chgCData(oldp+902,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                  [0U][3U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+903,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][3U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+904,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                          [0U][3U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+905,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+906,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][3U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+907,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+908,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][3U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+909,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+910,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                            [0U][3U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][3U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+911,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+912,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+913,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                           [0U][3U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+914,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                      [0U][3U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+915,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                              [0U][3U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+916,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextLocalPipeReg
                                [0U][3U][0U])));
        bufp->chgBit(oldp+917,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__canIssueFPDivSqrt));
        bufp->chgBit(oldp+918,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Reserved[0]));
        bufp->chgBit(oldp+919,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Finished[0]));
        bufp->chgBit(oldp+920,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Busy[0]));
        bufp->chgBit(oldp+921,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Free[0]));
        bufp->chgBit(oldp+922,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Release[0]));
        bufp->chgBit(oldp+923,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[0]));
        bufp->chgBit(oldp+924,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[1]));
        bufp->chgBit(oldp+925,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[2]));
        bufp->chgBit(oldp+926,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[3]));
        bufp->chgBit(oldp+927,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[4]));
        bufp->chgBit(oldp+928,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[5]));
        bufp->chgBit(oldp+929,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[6]));
        bufp->chgBit(oldp+930,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[7]));
        bufp->chgBit(oldp+931,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[8]));
        bufp->chgBit(oldp+932,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[9]));
        bufp->chgBit(oldp+933,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[10]));
        bufp->chgBit(oldp+934,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[11]));
        bufp->chgBit(oldp+935,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[12]));
        bufp->chgBit(oldp+936,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[13]));
        bufp->chgBit(oldp+937,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[14]));
        bufp->chgBit(oldp+938,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__fpIssueReq[15]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x49U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x90U])))) {
        bufp->chgCData(oldp+939,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextPhase),3);
        bufp->chgBit(oldp+940,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextFlushReqAck));
        bufp->chgBit(oldp+941,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nruStateWE));
        bufp->chgBit(oldp+942,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextMissValid));
        bufp->chgCData(oldp+943,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextMissIndex),8);
        bufp->chgSData(oldp+944,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextMissTag),11);
        bufp->chgBit(oldp+945,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nruStateArray__DOT__we));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x4aU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x91U])))) {
        bufp->chgSData(oldp+946,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                  [0U][2U] >> 0x16U)),10);
        bufp->chgBit(oldp+947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                      [0U][2U] >> 0x15U))));
        bufp->chgIData(oldp+948,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                   [0U][2U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                     [0U][1U] >> 0x15U))),32);
        bufp->chgBit(oldp+949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                      [0U][1U] >> 0x14U))));
        bufp->chgIData(oldp+950,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                              [0U][1U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+951,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                [0U][1U])));
        bufp->chgIData(oldp+952,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                  [0U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+953,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                      [0U][0U] >> 0xcU))));
        bufp->chgSData(oldp+954,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                            [0U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+955,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                  [0U][0U])),2);
        bufp->chgSData(oldp+956,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                  [1U][2U] >> 0x16U)),10);
        bufp->chgBit(oldp+957,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                      [1U][2U] >> 0x15U))));
        bufp->chgIData(oldp+958,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                   [1U][2U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                     [1U][1U] >> 0x15U))),32);
        bufp->chgBit(oldp+959,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                      [1U][1U] >> 0x14U))));
        bufp->chgIData(oldp+960,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                              [1U][1U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+961,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                [1U][1U])));
        bufp->chgIData(oldp+962,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                  [1U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+963,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                      [1U][0U] >> 0xcU))));
        bufp->chgSData(oldp+964,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                            [1U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+965,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__nextStage
                                  [1U][0U])),2);
        bufp->chgBit(oldp+966,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                      [0U] >> 0xcU))));
        bufp->chgSData(oldp+967,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                            [0U] >> 2U))),10);
        bufp->chgBit(oldp+968,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+969,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                [0U])));
        bufp->chgBit(oldp+970,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                      [1U] >> 0xcU))));
        bufp->chgSData(oldp+971,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                            [1U] >> 2U))),10);
        bufp->chgBit(oldp+972,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                      [1U] >> 1U))));
        bufp->chgBit(oldp+973,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
                                [1U])));
        bufp->chgSData(oldp+974,((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                  [0U][2U] >> 0x16U)),10);
        bufp->chgBit(oldp+975,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x15U))));
        bufp->chgIData(oldp+976,(((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                   [0U][2U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                     [0U][1U] >> 0x15U))),32);
        bufp->chgBit(oldp+977,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x14U))));
        bufp->chgIData(oldp+978,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+979,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                [0U][1U])));
        bufp->chgIData(oldp+980,((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                  [0U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+981,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xcU))));
        bufp->chgSData(oldp+982,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                            [0U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+983,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                  [0U][0U])),2);
        bufp->chgSData(oldp+984,((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                  [1U][2U] >> 0x16U)),10);
        bufp->chgBit(oldp+985,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x15U))));
        bufp->chgIData(oldp+986,(((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                   [1U][2U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                     [1U][1U] >> 0x15U))),32);
        bufp->chgBit(oldp+987,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x14U))));
        bufp->chgIData(oldp+988,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                              [1U][1U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+989,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                [1U][1U])));
        bufp->chgIData(oldp+990,((vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                  [1U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+991,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xcU))));
        bufp->chgSData(oldp+992,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                            [1U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+993,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__nextStage
                                  [1U][0U])),2);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x4bU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x92U])))) {
        bufp->chgIData(oldp+994,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__next[6U]),32);
        bufp->chgIData(oldp+995,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__next[5U]),32);
        bufp->chgIData(oldp+996,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__next[4U]),32);
        bufp->chgIData(oldp+997,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__next[3U]),32);
        bufp->chgIData(oldp+998,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__next[2U]),32);
        bufp->chgIData(oldp+999,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__next[1U]),32);
        bufp->chgIData(oldp+1000,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__next[0U]),32);
        bufp->chgIData(oldp+1001,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[6U]),32);
        bufp->chgIData(oldp+1002,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[5U]),32);
        bufp->chgIData(oldp+1003,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[4U]),32);
        bufp->chgIData(oldp+1004,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[3U]),32);
        bufp->chgIData(oldp+1005,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[2U]),32);
        bufp->chgIData(oldp+1006,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[1U]),32);
        bufp->chgIData(oldp+1007,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[0U]),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x4cU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8eU])))) {
        bufp->chgSData(oldp+1008,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__compRequest),16);
        bufp->chgSData(oldp+1009,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__compGrant),16);
        bufp->chgBit(oldp+1010,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__compSelected[0]));
        bufp->chgCData(oldp+1011,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__compSelectedPtr[0]),4);
        bufp->chgSData(oldp+1012,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__compPicker__DOT__reqTmp),16);
        bufp->chgIData(oldp+1013,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__p),32);
        bufp->chgIData(oldp+1014,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x4dU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x8fU])))) {
        bufp->chgSData(oldp+1015,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__fpRequest),16);
        bufp->chgSData(oldp+1016,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__fpGrant),16);
        bufp->chgBit(oldp+1017,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__fpSelected[0]));
        bufp->chgCData(oldp+1018,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__fpSelectedPtr[0]),4);
        bufp->chgSData(oldp+1019,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__fpPicker__DOT__reqTmp),16);
        bufp->chgIData(oldp+1020,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__p),32);
        bufp->chgIData(oldp+1021,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x4fU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x93U])))) {
        bufp->chgBit(oldp+1022,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelected[0]));
        bufp->chgBit(oldp+1023,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelected[1]));
        bufp->chgBit(oldp+1024,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelected[2]));
        bufp->chgBit(oldp+1025,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelected[3]));
        bufp->chgBit(oldp+1026,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelected[4]));
        bufp->chgBit(oldp+1027,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelected[5]));
        bufp->chgCData(oldp+1028,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedPtr[0]),4);
        bufp->chgCData(oldp+1029,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedPtr[1]),4);
        bufp->chgCData(oldp+1030,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedPtr[2]),4);
        bufp->chgCData(oldp+1031,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedPtr[3]),4);
        bufp->chgCData(oldp+1032,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedPtr[4]),4);
        bufp->chgCData(oldp+1033,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedPtr[5]),4);
        bufp->chgSData(oldp+1034,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedVector[0]),16);
        bufp->chgSData(oldp+1035,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedVector[1]),16);
        bufp->chgSData(oldp+1036,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedVector[2]),16);
        bufp->chgSData(oldp+1037,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedVector[3]),16);
        bufp->chgSData(oldp+1038,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedVector[4]),16);
        bufp->chgSData(oldp+1039,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__portSelectedVector[5]),16);
        bufp->chgBit(oldp+1040,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelected[0]));
        bufp->chgBit(oldp+1041,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelected[1]));
        bufp->chgBit(oldp+1042,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelected[2]));
        bufp->chgBit(oldp+1043,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelected[3]));
        bufp->chgBit(oldp+1044,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelected[4]));
        bufp->chgBit(oldp+1045,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelected[5]));
        bufp->chgCData(oldp+1046,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelectedPtr[0]),4);
        bufp->chgCData(oldp+1047,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelectedPtr[1]),4);
        bufp->chgCData(oldp+1048,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelectedPtr[2]),4);
        bufp->chgCData(oldp+1049,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelectedPtr[3]),4);
        bufp->chgCData(oldp+1050,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelectedPtr[4]),4);
        bufp->chgCData(oldp+1051,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__selectLogic__DOT__recoverySelectedPtr[5]),4);
        bufp->chgCData(oldp+1052,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__selectedPtr[0]),4);
        bufp->chgCData(oldp+1053,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__selectedPtr[1]),4);
        bufp->chgCData(oldp+1054,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__selectedPtr[2]),4);
        bufp->chgCData(oldp+1055,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__selectedPtr[3]),4);
        bufp->chgCData(oldp+1056,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__selectedPtr[4]),4);
        bufp->chgCData(oldp+1057,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__selectedPtr[5]),4);
        bufp->chgCData(oldp+1058,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr[0]),4);
        bufp->chgCData(oldp+1059,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr[1]),4);
        bufp->chgCData(oldp+1060,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr[2]),4);
        bufp->chgCData(oldp+1061,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr[3]),4);
        bufp->chgCData(oldp+1062,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr[4]),4);
        bufp->chgCData(oldp+1063,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selectedPtr[5]),4);
        bufp->chgBit(oldp+1064,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected[0]));
        bufp->chgBit(oldp+1065,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected[1]));
        bufp->chgBit(oldp+1066,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected[2]));
        bufp->chgBit(oldp+1067,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected[3]));
        bufp->chgBit(oldp+1068,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected[4]));
        bufp->chgBit(oldp+1069,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selected[5]));
        bufp->chgCData(oldp+1070,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr[0]),4);
        bufp->chgCData(oldp+1071,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr[1]),4);
        bufp->chgCData(oldp+1072,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr[2]),4);
        bufp->chgCData(oldp+1073,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr[3]),4);
        bufp->chgCData(oldp+1074,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr[4]),4);
        bufp->chgCData(oldp+1075,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedPtr[5]),4);
        bufp->chgSData(oldp+1076,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector[0]),16);
        bufp->chgSData(oldp+1077,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector[1]),16);
        bufp->chgSData(oldp+1078,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector[2]),16);
        bufp->chgSData(oldp+1079,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector[3]),16);
        bufp->chgSData(oldp+1080,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector[4]),16);
        bufp->chgSData(oldp+1081,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__selectedVector[5]),16);
        bufp->chgBit(oldp+1082,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selected[0]));
        bufp->chgBit(oldp+1083,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selected[1]));
        bufp->chgBit(oldp+1084,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selected[2]));
        bufp->chgBit(oldp+1085,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selected[3]));
        bufp->chgBit(oldp+1086,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selected[4]));
        bufp->chgBit(oldp+1087,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selected[5]));
        bufp->chgCData(oldp+1088,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr[0]),4);
        bufp->chgCData(oldp+1089,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr[1]),4);
        bufp->chgCData(oldp+1090,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr[2]),4);
        bufp->chgCData(oldp+1091,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr[3]),4);
        bufp->chgCData(oldp+1092,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr[4]),4);
        bufp->chgCData(oldp+1093,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr[5]),4);
        bufp->chgCData(oldp+1094,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[0]),6);
        bufp->chgCData(oldp+1095,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[1]),6);
        bufp->chgCData(oldp+1096,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[2]),6);
        bufp->chgCData(oldp+1097,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[3]),6);
        bufp->chgCData(oldp+1098,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[4]),6);
        bufp->chgCData(oldp+1099,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[5]),6);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x50U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x95U])))) {
        bufp->chgBit(oldp+1100,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__stall));
        bufp->chgBit(oldp+1101,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__clear));
        bufp->chgBit(oldp+1102,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__flush[0]));
        bufp->chgBit(oldp+1103,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__flush[1]));
        bufp->chgBit(oldp+1104,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__flush[2]));
        bufp->chgBit(oldp+1105,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__flush[3]));
        bufp->chgBit(oldp+1106,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__flush[4]));
        bufp->chgBit(oldp+1107,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__flush[5]));
        bufp->chgBit(oldp+1108,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__valid[0]));
        bufp->chgBit(oldp+1109,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__valid[1]));
        bufp->chgBit(oldp+1110,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__valid[2]));
        bufp->chgBit(oldp+1111,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__valid[3]));
        bufp->chgBit(oldp+1112,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__valid[4]));
        bufp->chgBit(oldp+1113,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__valid[5]));
        bufp->chgBit(oldp+1114,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__update[0]));
        bufp->chgBit(oldp+1115,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__update[1]));
        bufp->chgBit(oldp+1116,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__update[2]));
        bufp->chgBit(oldp+1117,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__update[3]));
        bufp->chgBit(oldp+1118,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__update[4]));
        bufp->chgBit(oldp+1119,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__update[5]));
        bufp->chgBit(oldp+1120,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                       [0U] >> 4U))));
        bufp->chgCData(oldp+1121,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                   [0U])),4);
        bufp->chgBit(oldp+1122,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                       [1U] >> 4U))));
        bufp->chgCData(oldp+1123,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                   [1U])),4);
        bufp->chgBit(oldp+1124,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                       [2U] >> 4U))));
        bufp->chgCData(oldp+1125,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                   [2U])),4);
        bufp->chgBit(oldp+1126,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                       [3U] >> 4U))));
        bufp->chgCData(oldp+1127,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                   [3U])),4);
        bufp->chgBit(oldp+1128,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                       [4U] >> 4U))));
        bufp->chgCData(oldp+1129,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                   [4U])),4);
        bufp->chgBit(oldp+1130,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                       [5U] >> 4U))));
        bufp->chgCData(oldp+1131,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__nextStage
                                   [5U])),4);
        bufp->chgCData(oldp+1132,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__issueQueuePtr[0]),4);
        bufp->chgCData(oldp+1133,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__issueQueuePtr[1]),4);
        bufp->chgCData(oldp+1134,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__issueQueuePtr[2]),4);
        bufp->chgCData(oldp+1135,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__issueQueuePtr[3]),4);
        bufp->chgCData(oldp+1136,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__issueQueuePtr[4]),4);
        bufp->chgCData(oldp+1137,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__issueQueuePtr[5]),4);
        bufp->chgSData(oldp+1138,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scStage__DOT__flushIQ_Entry),16);
        bufp->chgSData(oldp+1139,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__selectedVector),16);
        bufp->chgIData(oldp+1140,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__unnamedblk10__DOT__i),32);
        bufp->chgBit(oldp+1141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                       [0U] >> 0x1aU))));
        bufp->chgCData(oldp+1142,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                           [0U] >> 0x16U))),4);
        bufp->chgSData(oldp+1143,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                              [0U] 
                                              >> 6U))),16);
        bufp->chgCData(oldp+1144,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                   [0U])),6);
        bufp->chgBit(oldp+1145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                       [1U] >> 0x1aU))));
        bufp->chgCData(oldp+1146,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                           [1U] >> 0x16U))),4);
        bufp->chgSData(oldp+1147,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                              [1U] 
                                              >> 6U))),16);
        bufp->chgCData(oldp+1148,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg
                                   [1U])),6);
        bufp->chgBit(oldp+1149,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                       [0U] >> 0x1aU))));
        bufp->chgCData(oldp+1150,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                           [0U] >> 0x16U))),4);
        bufp->chgSData(oldp+1151,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                              [0U] 
                                              >> 6U))),16);
        bufp->chgCData(oldp+1152,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                   [0U])),6);
        bufp->chgBit(oldp+1153,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                       [1U] >> 0x1aU))));
        bufp->chgCData(oldp+1154,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                           [1U] >> 0x16U))),4);
        bufp->chgSData(oldp+1155,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                              [1U] 
                                              >> 6U))),16);
        bufp->chgCData(oldp+1156,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg
                                   [1U])),6);
        bufp->chgBit(oldp+1157,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
                                       [0U] >> 0x1aU))));
        bufp->chgCData(oldp+1158,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
                                           [0U] >> 0x16U))),4);
        bufp->chgSData(oldp+1159,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
                                              [0U] 
                                              >> 6U))),16);
        bufp->chgCData(oldp+1160,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg
                                   [0U])),6);
        bufp->chgBit(oldp+1161,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushComplex[0]));
        bufp->chgCData(oldp+1162,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__complexSelectedPtr[0]),4);
        bufp->chgBit(oldp+1163,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
                                       [0U] >> 0x1aU))));
        bufp->chgCData(oldp+1164,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
                                           [0U] >> 0x16U))),4);
        bufp->chgSData(oldp+1165,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
                                              [0U] 
                                              >> 6U))),16);
        bufp->chgCData(oldp+1166,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg
                                   [0U])),6);
        bufp->chgBit(oldp+1167,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushFP[0]));
        bufp->chgCData(oldp+1168,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__fpSelectedPtr[0]),4);
        bufp->chgBit(oldp+1169,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushInt[0]));
        bufp->chgBit(oldp+1170,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushInt[1]));
        bufp->chgBit(oldp+1171,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushMem[0]));
        bufp->chgCData(oldp+1172,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__intSelectedPtr[0]),4);
        bufp->chgCData(oldp+1173,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__intSelectedPtr[1]),4);
        bufp->chgCData(oldp+1174,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__memSelectedPtr[0]),4);
        bufp->chgCData(oldp+1175,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__memSelectedPtr[1]),4);
        bufp->chgSData(oldp+1176,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry),16);
        bufp->chgIData(oldp+1177,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk31__DOT__i),32);
        bufp->chgIData(oldp+1178,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk32__DOT__i),32);
        bufp->chgIData(oldp+1179,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk33__DOT__i),32);
        bufp->chgIData(oldp+1180,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk34__DOT__i),32);
        bufp->chgIData(oldp+1181,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk35__DOT__i),32);
        bufp->chgIData(oldp+1182,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk36__DOT__i),32);
        bufp->chgIData(oldp+1183,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk37__DOT__i),32);
        bufp->chgIData(oldp+1184,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk38__DOT__i),32);
        bufp->chgIData(oldp+1185,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk39__DOT__i),32);
        bufp->chgBit(oldp+1186,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[0]));
        bufp->chgBit(oldp+1187,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[1]));
        bufp->chgBit(oldp+1188,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[2]));
        bufp->chgBit(oldp+1189,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[3]));
        bufp->chgBit(oldp+1190,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[4]));
        bufp->chgBit(oldp+1191,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[5]));
        bufp->chgBit(oldp+1192,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[6]));
        bufp->chgBit(oldp+1193,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releaseEntry[7]));
        bufp->chgCData(oldp+1194,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readPtr[0]),4);
        bufp->chgCData(oldp+1195,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readPtr[1]),4);
        bufp->chgCData(oldp+1196,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readPtr[2]),4);
        bufp->chgCData(oldp+1197,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readPtr[3]),4);
        bufp->chgCData(oldp+1198,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readPtr[4]),4);
        bufp->chgBit(oldp+1199,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+1200,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+1201,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                 [0U])));
        bufp->chgBit(oldp+1202,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                       [1U] >> 7U))));
        bufp->chgCData(oldp+1203,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                            [1U] >> 1U))),6);
        bufp->chgBit(oldp+1204,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                 [1U])));
        bufp->chgBit(oldp+1205,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                       [2U] >> 7U))));
        bufp->chgCData(oldp+1206,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                            [2U] >> 1U))),6);
        bufp->chgBit(oldp+1207,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                 [2U])));
        bufp->chgBit(oldp+1208,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                       [3U] >> 7U))));
        bufp->chgCData(oldp+1209,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                            [3U] >> 1U))),6);
        bufp->chgBit(oldp+1210,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                 [3U])));
        bufp->chgBit(oldp+1211,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                       [4U] >> 7U))));
        bufp->chgCData(oldp+1212,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                            [4U] >> 1U))),6);
        bufp->chgBit(oldp+1213,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__readData
                                 [4U])));
        bufp->chgBit(oldp+1214,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegValid[0]));
        bufp->chgBit(oldp+1215,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegValid[1]));
        bufp->chgBit(oldp+1216,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegValid[2]));
        bufp->chgBit(oldp+1217,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegValid[3]));
        bufp->chgBit(oldp+1218,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegValid[4]));
        bufp->chgBit(oldp+1219,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1220,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+1221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1222,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                   [1U])),6);
        bufp->chgBit(oldp+1223,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                       [2U] >> 6U))));
        bufp->chgCData(oldp+1224,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                   [2U])),6);
        bufp->chgBit(oldp+1225,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                       [3U] >> 6U))));
        bufp->chgCData(oldp+1226,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                   [3U])),6);
        bufp->chgBit(oldp+1227,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                       [4U] >> 6U))));
        bufp->chgCData(oldp+1228,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstRegNum
                                   [4U])),6);
        bufp->chgBit(oldp+1229,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegReady
                                [0U][0U]));
        bufp->chgBit(oldp+1230,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegReady
                                [0U][1U]));
        bufp->chgBit(oldp+1231,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegReady
                                [0U][2U]));
        bufp->chgBit(oldp+1232,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegReady
                                [1U][0U]));
        bufp->chgBit(oldp+1233,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegReady
                                [1U][1U]));
        bufp->chgBit(oldp+1234,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegReady
                                [1U][2U]));
        bufp->chgSData(oldp+1235,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstVector[0]),16);
        bufp->chgSData(oldp+1236,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstVector[1]),16);
        bufp->chgSData(oldp+1237,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstVector[2]),16);
        bufp->chgSData(oldp+1238,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstVector[3]),16);
        bufp->chgSData(oldp+1239,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstVector[4]),16);
        bufp->chgSData(oldp+1240,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__wakeupDstVector[5]),16);
        bufp->chgBit(oldp+1241,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__intNextStage
                                       [0U] >> 4U))));
        bufp->chgCData(oldp+1242,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__intNextStage
                                   [0U])),4);
        bufp->chgBit(oldp+1243,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__intNextStage
                                       [1U] >> 4U))));
        bufp->chgCData(oldp+1244,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__intNextStage
                                   [1U])),4);
        bufp->chgBit(oldp+1245,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__complexNextStage
                                       [0U] >> 4U))));
        bufp->chgCData(oldp+1246,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__complexNextStage
                                   [0U])),4);
        bufp->chgBit(oldp+1247,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__memNextStage
                                       [0U] >> 4U))));
        bufp->chgCData(oldp+1248,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__memNextStage
                                   [0U])),4);
        bufp->chgBit(oldp+1249,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__memNextStage
                                       [1U] >> 4U))));
        bufp->chgCData(oldp+1250,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__memNextStage
                                   [1U])),4);
        bufp->chgBit(oldp+1251,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__fpNextStage
                                       [0U] >> 4U))));
        bufp->chgCData(oldp+1252,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__scStageIF.__PVT__fpNextStage
                                   [0U])),4);
        bufp->chgBit(oldp+1253,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[0]));
        bufp->chgBit(oldp+1254,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[1]));
        bufp->chgBit(oldp+1255,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[2]));
        bufp->chgBit(oldp+1256,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[3]));
        bufp->chgBit(oldp+1257,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[4]));
        bufp->chgBit(oldp+1258,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__selected[5]));
        bufp->chgBit(oldp+1259,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__stall));
        bufp->chgBit(oldp+1260,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[0]));
        bufp->chgBit(oldp+1261,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[1]));
        bufp->chgBit(oldp+1262,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[2]));
        bufp->chgBit(oldp+1263,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[3]));
        bufp->chgBit(oldp+1264,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeup[4]));
        bufp->chgCData(oldp+1265,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[0]),4);
        bufp->chgCData(oldp+1266,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[1]),4);
        bufp->chgCData(oldp+1267,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[2]),4);
        bufp->chgCData(oldp+1268,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[3]),4);
        bufp->chgCData(oldp+1269,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[4]),4);
        bufp->chgCData(oldp+1270,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupPtr[5]),4);
        bufp->chgSData(oldp+1271,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[0]),16);
        bufp->chgSData(oldp+1272,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[1]),16);
        bufp->chgSData(oldp+1273,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[2]),16);
        bufp->chgSData(oldp+1274,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[3]),16);
        bufp->chgSData(oldp+1275,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[4]),16);
        bufp->chgSData(oldp+1276,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupVector[5]),16);
        bufp->chgBit(oldp+1277,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+1278,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+1279,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                 [0U])));
        bufp->chgBit(oldp+1280,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                       [1U] >> 7U))));
        bufp->chgCData(oldp+1281,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                            [1U] >> 1U))),6);
        bufp->chgBit(oldp+1282,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                 [1U])));
        bufp->chgBit(oldp+1283,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                       [2U] >> 7U))));
        bufp->chgCData(oldp+1284,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                            [2U] >> 1U))),6);
        bufp->chgBit(oldp+1285,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                 [2U])));
        bufp->chgBit(oldp+1286,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                       [3U] >> 7U))));
        bufp->chgCData(oldp+1287,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                            [3U] >> 1U))),6);
        bufp->chgBit(oldp+1288,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                 [3U])));
        bufp->chgBit(oldp+1289,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                       [4U] >> 7U))));
        bufp->chgCData(oldp+1290,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                            [4U] >> 1U))),6);
        bufp->chgBit(oldp+1291,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__wakeupDstTag
                                 [4U])));
        bufp->chgBit(oldp+1292,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[0]));
        bufp->chgBit(oldp+1293,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[1]));
        bufp->chgBit(oldp+1294,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[2]));
        bufp->chgBit(oldp+1295,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[3]));
        bufp->chgBit(oldp+1296,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[4]));
        bufp->chgBit(oldp+1297,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry[5]));
        bufp->chgBit(oldp+1298,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[0]));
        bufp->chgBit(oldp+1299,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[1]));
        bufp->chgBit(oldp+1300,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[2]));
        bufp->chgBit(oldp+1301,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[3]));
        bufp->chgBit(oldp+1302,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[4]));
        bufp->chgBit(oldp+1303,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[5]));
        bufp->chgBit(oldp+1304,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[6]));
        bufp->chgBit(oldp+1305,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[7]));
        bufp->chgCData(oldp+1306,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushCount),4);
        bufp->chgBit(oldp+1307,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[0]));
        bufp->chgBit(oldp+1308,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[1]));
        bufp->chgBit(oldp+1309,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[2]));
        bufp->chgBit(oldp+1310,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[3]));
        bufp->chgBit(oldp+1311,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[4]));
        bufp->chgBit(oldp+1312,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[5]));
        bufp->chgBit(oldp+1313,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[6]));
        bufp->chgBit(oldp+1314,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__we[7]));
        bufp->chgCData(oldp+1315,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[0]),4);
        bufp->chgCData(oldp+1316,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[1]),4);
        bufp->chgCData(oldp+1317,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[2]),4);
        bufp->chgCData(oldp+1318,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[3]),4);
        bufp->chgCData(oldp+1319,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[4]),4);
        bufp->chgCData(oldp+1320,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[5]),4);
        bufp->chgCData(oldp+1321,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[6]),4);
        bufp->chgCData(oldp+1322,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__wa[7]),4);
        bufp->chgBit(oldp+1323,((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushCount))));
        bufp->chgCData(oldp+1324,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__nextTail),4);
        bufp->chgBit(oldp+1325,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[0]));
        bufp->chgBit(oldp+1326,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[1]));
        bufp->chgBit(oldp+1327,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[2]));
        bufp->chgBit(oldp+1328,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[3]));
        bufp->chgBit(oldp+1329,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeup[4]));
        bufp->chgBit(oldp+1330,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[0]));
        bufp->chgBit(oldp+1331,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[1]));
        bufp->chgBit(oldp+1332,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[2]));
        bufp->chgBit(oldp+1333,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[3]));
        bufp->chgBit(oldp+1334,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstValid[4]));
        bufp->chgCData(oldp+1335,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[0]),7);
        bufp->chgCData(oldp+1336,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[1]),7);
        bufp->chgCData(oldp+1337,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[2]),7);
        bufp->chgCData(oldp+1338,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[3]),7);
        bufp->chgCData(oldp+1339,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__wakeupDstRegNum[4]),7);
        bufp->chgBit(oldp+1340,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
                                [0U][0U]));
        bufp->chgBit(oldp+1341,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
                                [0U][1U]));
        bufp->chgBit(oldp+1342,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
                                [0U][2U]));
        bufp->chgBit(oldp+1343,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
                                [1U][0U]));
        bufp->chgBit(oldp+1344,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
                                [1U][1U]));
        bufp->chgBit(oldp+1345,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__dispatchedSrcReady
                                [1U][2U]));
        bufp->chgBit(oldp+1346,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWE[0]));
        bufp->chgBit(oldp+1347,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWE[1]));
        bufp->chgBit(oldp+1348,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWE[2]));
        bufp->chgBit(oldp+1349,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWE[3]));
        bufp->chgBit(oldp+1350,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWE[4]));
        bufp->chgBit(oldp+1351,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWE[5]));
        bufp->chgBit(oldp+1352,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWE[6]));
        bufp->chgCData(oldp+1353,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWA[0]),7);
        bufp->chgCData(oldp+1354,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWA[1]),7);
        bufp->chgCData(oldp+1355,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWA[2]),7);
        bufp->chgCData(oldp+1356,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWA[3]),7);
        bufp->chgCData(oldp+1357,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWA[4]),7);
        bufp->chgCData(oldp+1358,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWA[5]),7);
        bufp->chgCData(oldp+1359,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl.__PVT__readyWA[6]),7);
        bufp->chgCData(oldp+1360,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[0]),4);
        bufp->chgCData(oldp+1361,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[1]),4);
        bufp->chgCData(oldp+1362,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[2]),4);
        bufp->chgCData(oldp+1363,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[3]),4);
        bufp->chgCData(oldp+1364,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__ra[4]),4);
        bufp->chgCData(oldp+1365,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv[0]),8);
        bufp->chgCData(oldp+1366,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv[1]),8);
        bufp->chgCData(oldp+1367,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv[2]),8);
        bufp->chgCData(oldp+1368,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv[3]),8);
        bufp->chgCData(oldp+1369,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM.__PVT__rv[4]),8);
        bufp->chgBit(oldp+1370,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[0]));
        bufp->chgBit(oldp+1371,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[1]));
        bufp->chgBit(oldp+1372,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[2]));
        bufp->chgBit(oldp+1373,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[3]));
        bufp->chgBit(oldp+1374,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[4]));
        bufp->chgBit(oldp+1375,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[5]));
        bufp->chgBit(oldp+1376,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[6]));
        bufp->chgBit(oldp+1377,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__we[7]));
        bufp->chgCData(oldp+1378,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[0]),4);
        bufp->chgCData(oldp+1379,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[1]),4);
        bufp->chgCData(oldp+1380,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[2]),4);
        bufp->chgCData(oldp+1381,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[3]),4);
        bufp->chgCData(oldp+1382,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[4]),4);
        bufp->chgCData(oldp+1383,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[5]),4);
        bufp->chgCData(oldp+1384,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[6]),4);
        bufp->chgCData(oldp+1385,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__wa[7]),4);
        bufp->chgBit(oldp+1386,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[0]));
        bufp->chgBit(oldp+1387,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[1]));
        bufp->chgBit(oldp+1388,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[2]));
        bufp->chgBit(oldp+1389,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[3]));
        bufp->chgBit(oldp+1390,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[4]));
        bufp->chgBit(oldp+1391,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[5]));
        bufp->chgBit(oldp+1392,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__we[6]));
        bufp->chgCData(oldp+1393,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[0]),7);
        bufp->chgCData(oldp+1394,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[1]),7);
        bufp->chgCData(oldp+1395,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[2]),7);
        bufp->chgCData(oldp+1396,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[3]),7);
        bufp->chgCData(oldp+1397,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[4]),7);
        bufp->chgCData(oldp+1398,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[5]),7);
        bufp->chgCData(oldp+1399,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable.__PVT__wa[6]),7);
        bufp->chgCData(oldp+1400,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra[0]),4);
        bufp->chgCData(oldp+1401,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra[1]),4);
        bufp->chgCData(oldp+1402,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra[2]),4);
        bufp->chgCData(oldp+1403,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra[3]),4);
        bufp->chgCData(oldp+1404,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra[4]),4);
        bufp->chgCData(oldp+1405,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__rv[0]),8);
        bufp->chgCData(oldp+1406,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__rv[1]),8);
        bufp->chgCData(oldp+1407,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__rv[2]),8);
        bufp->chgCData(oldp+1408,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__rv[3]),8);
        bufp->chgCData(oldp+1409,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__rv[4]),8);
        bufp->chgCData(oldp+1410,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [0U][0U]),8);
        bufp->chgCData(oldp+1411,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [0U][1U]),8);
        bufp->chgCData(oldp+1412,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [1U][0U]),8);
        bufp->chgCData(oldp+1413,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [1U][1U]),8);
        bufp->chgCData(oldp+1414,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [2U][0U]),8);
        bufp->chgCData(oldp+1415,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [2U][1U]),8);
        bufp->chgCData(oldp+1416,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [3U][0U]),8);
        bufp->chgCData(oldp+1417,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [3U][1U]),8);
        bufp->chgCData(oldp+1418,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [4U][0U]),8);
        bufp->chgCData(oldp+1419,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                  [4U][1U]),8);
        bufp->chgBit(oldp+1420,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[0]));
        bufp->chgBit(oldp+1421,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[1]));
        bufp->chgBit(oldp+1422,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[2]));
        bufp->chgBit(oldp+1423,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[3]));
        bufp->chgBit(oldp+1424,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[4]));
        bufp->chgCData(oldp+1425,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra
                                  [0U]),4);
        bufp->chgCData(oldp+1426,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra
                                  [1U]),4);
        bufp->chgCData(oldp+1427,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra
                                  [2U]),4);
        bufp->chgCData(oldp+1428,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra
                                  [3U]),4);
        bufp->chgCData(oldp+1429,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__ra
                                  [4U]),4);
        bufp->chgCData(oldp+1430,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[0]),4);
        bufp->chgCData(oldp+1431,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[1]),4);
        bufp->chgCData(oldp+1432,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[2]),4);
        bufp->chgCData(oldp+1433,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[3]),4);
        bufp->chgCData(oldp+1434,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[4]),4);
        bufp->chgBit(oldp+1435,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [0U][0U]));
        bufp->chgBit(oldp+1436,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [0U][1U]));
        bufp->chgBit(oldp+1437,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [0U][2U]));
        bufp->chgBit(oldp+1438,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [0U][3U]));
        bufp->chgBit(oldp+1439,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [0U][4U]));
        bufp->chgBit(oldp+1440,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [1U][0U]));
        bufp->chgBit(oldp+1441,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [1U][1U]));
        bufp->chgBit(oldp+1442,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [1U][2U]));
        bufp->chgBit(oldp+1443,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [1U][3U]));
        bufp->chgBit(oldp+1444,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                                [1U][4U]));
        bufp->chgCData(oldp+1445,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                  [0U]),4);
        bufp->chgCData(oldp+1446,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                  [1U]),4);
        bufp->chgCData(oldp+1447,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                  [2U]),4);
        bufp->chgCData(oldp+1448,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                  [3U]),4);
        bufp->chgCData(oldp+1449,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                  [4U]),4);
        bufp->chgBit(oldp+1450,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we[0]));
        bufp->chgBit(oldp+1451,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we[1]));
        bufp->chgBit(oldp+1452,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we[2]));
        bufp->chgBit(oldp+1453,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we[3]));
        bufp->chgBit(oldp+1454,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we[4]));
        bufp->chgBit(oldp+1455,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we[5]));
        bufp->chgBit(oldp+1456,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we[6]));
        bufp->chgCData(oldp+1457,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa[0]),7);
        bufp->chgCData(oldp+1458,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa[1]),7);
        bufp->chgCData(oldp+1459,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa[2]),7);
        bufp->chgCData(oldp+1460,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa[3]),7);
        bufp->chgCData(oldp+1461,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa[4]),7);
        bufp->chgCData(oldp+1462,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa[5]),7);
        bufp->chgCData(oldp+1463,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa[6]),7);
        bufp->chgBit(oldp+1464,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we
                                [0U]));
        bufp->chgCData(oldp+1465,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa
                                  [0U]),7);
        bufp->chgBit(oldp+1466,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we
                                [1U]));
        bufp->chgCData(oldp+1467,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa
                                  [1U]),7);
        bufp->chgBit(oldp+1468,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we
                                [2U]));
        bufp->chgCData(oldp+1469,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa
                                  [2U]),7);
        bufp->chgBit(oldp+1470,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we
                                [3U]));
        bufp->chgCData(oldp+1471,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa
                                  [3U]),7);
        bufp->chgBit(oldp+1472,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we
                                [4U]));
        bufp->chgCData(oldp+1473,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa
                                  [4U]),7);
        bufp->chgBit(oldp+1474,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we
                                [5U]));
        bufp->chgCData(oldp+1475,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa
                                  [5U]),7);
        bufp->chgBit(oldp+1476,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__we
                                [6U]));
        bufp->chgCData(oldp+1477,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__wa
                                  [6U]),7);
        bufp->chgCData(oldp+1478,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[0]),3);
        bufp->chgCData(oldp+1479,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[1]),3);
        bufp->chgCData(oldp+1480,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[2]),3);
        bufp->chgCData(oldp+1481,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[3]),3);
        bufp->chgCData(oldp+1482,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[4]),3);
        bufp->chgCData(oldp+1483,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[5]),3);
        bufp->chgCData(oldp+1484,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[6]),3);
        bufp->chgCData(oldp+1485,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[0]),7);
        bufp->chgCData(oldp+1486,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[1]),7);
        bufp->chgCData(oldp+1487,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[2]),7);
        bufp->chgCData(oldp+1488,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[3]),7);
        bufp->chgCData(oldp+1489,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[4]),7);
        bufp->chgCData(oldp+1490,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[5]),7);
        bufp->chgCData(oldp+1491,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[6]),7);
        bufp->chgCData(oldp+1492,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                  [0U]),3);
        bufp->chgCData(oldp+1493,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                  [1U]),3);
        bufp->chgCData(oldp+1494,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                  [2U]),3);
        bufp->chgCData(oldp+1495,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                  [3U]),3);
        bufp->chgCData(oldp+1496,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                  [4U]),3);
        bufp->chgCData(oldp+1497,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                  [5U]),3);
        bufp->chgCData(oldp+1498,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                  [6U]),3);
        bufp->chgCData(oldp+1499,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [1U]),7);
        bufp->chgCData(oldp+1500,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [2U]),7);
        bufp->chgCData(oldp+1501,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [3U]),7);
        bufp->chgCData(oldp+1502,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [4U]),7);
        bufp->chgCData(oldp+1503,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [5U]),7);
        bufp->chgCData(oldp+1504,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [6U]),7);
        bufp->chgCData(oldp+1505,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [0U]),7);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x51U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x96U])))) {
        bufp->chgCData(oldp+1506,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__poppedData[0]),4);
        bufp->chgCData(oldp+1507,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__poppedData[1]),4);
        bufp->chgCData(oldp+1508,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__popCount),2);
        bufp->chgCData(oldp+1509,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__rv[0]),4);
        bufp->chgCData(oldp+1510,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__rv[1]),4);
        bufp->chgBit(oldp+1511,((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__popCount))));
        bufp->chgCData(oldp+1512,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__nextHead),4);
        bufp->chgCData(oldp+1513,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__nextCount),5);
        bufp->chgCData(oldp+1514,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__rv[0]),4);
        bufp->chgCData(oldp+1515,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__rv[1]),4);
        bufp->chgCData(oldp+1516,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),4);
        bufp->chgCData(oldp+1517,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),4);
        bufp->chgCData(oldp+1518,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[2]),4);
        bufp->chgCData(oldp+1519,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[3]),4);
        bufp->chgCData(oldp+1520,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[4]),4);
        bufp->chgCData(oldp+1521,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[5]),4);
        bufp->chgCData(oldp+1522,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[6]),4);
        bufp->chgCData(oldp+1523,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[7]),4);
        bufp->chgCData(oldp+1524,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),4);
        bufp->chgCData(oldp+1525,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),4);
        bufp->chgCData(oldp+1526,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[2]),4);
        bufp->chgCData(oldp+1527,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[3]),4);
        bufp->chgCData(oldp+1528,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[4]),4);
        bufp->chgCData(oldp+1529,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[5]),4);
        bufp->chgCData(oldp+1530,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[6]),4);
        bufp->chgCData(oldp+1531,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[7]),4);
        bufp->chgCData(oldp+1532,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[0]),4);
        bufp->chgCData(oldp+1533,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[1]),4);
        bufp->chgCData(oldp+1534,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[2]),4);
        bufp->chgCData(oldp+1535,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[3]),4);
        bufp->chgCData(oldp+1536,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[4]),4);
        bufp->chgCData(oldp+1537,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[5]),4);
        bufp->chgCData(oldp+1538,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[6]),4);
        bufp->chgCData(oldp+1539,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__rvBank[7]),4);
        bufp->chgCData(oldp+1540,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]),4);
        bufp->chgCData(oldp+1541,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]),4);
        bufp->chgCData(oldp+1542,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[2]),4);
        bufp->chgCData(oldp+1543,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[3]),4);
        bufp->chgCData(oldp+1544,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[4]),4);
        bufp->chgCData(oldp+1545,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[5]),4);
        bufp->chgCData(oldp+1546,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[6]),4);
        bufp->chgCData(oldp+1547,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[7]),4);
        bufp->chgBit(oldp+1548,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+1549,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+1550,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[2]));
        bufp->chgBit(oldp+1551,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[3]));
        bufp->chgBit(oldp+1552,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[4]));
        bufp->chgBit(oldp+1553,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[5]));
        bufp->chgBit(oldp+1554,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[6]));
        bufp->chgBit(oldp+1555,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[7]));
        bufp->chgBit(oldp+1556,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [0U]));
        bufp->chgBit(oldp+1557,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [0U] >> 3U))));
        bufp->chgCData(oldp+1558,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [0U]),4);
        bufp->chgBit(oldp+1559,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [0U] >> 3U))));
        bufp->chgBit(oldp+1560,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [1U]));
        bufp->chgBit(oldp+1561,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [1U] >> 3U))));
        bufp->chgCData(oldp+1562,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [1U]),4);
        bufp->chgBit(oldp+1563,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [1U] >> 3U))));
        bufp->chgBit(oldp+1564,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [2U]));
        bufp->chgBit(oldp+1565,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [2U] >> 3U))));
        bufp->chgCData(oldp+1566,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [2U]),4);
        bufp->chgBit(oldp+1567,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [2U] >> 3U))));
        bufp->chgBit(oldp+1568,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [3U]));
        bufp->chgBit(oldp+1569,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [3U] >> 3U))));
        bufp->chgCData(oldp+1570,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [3U]),4);
        bufp->chgBit(oldp+1571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [3U] >> 3U))));
        bufp->chgBit(oldp+1572,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [4U]));
        bufp->chgBit(oldp+1573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [4U] >> 3U))));
        bufp->chgCData(oldp+1574,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [4U]),4);
        bufp->chgBit(oldp+1575,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [4U] >> 3U))));
        bufp->chgBit(oldp+1576,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [5U]));
        bufp->chgBit(oldp+1577,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [5U] >> 3U))));
        bufp->chgCData(oldp+1578,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [5U]),4);
        bufp->chgBit(oldp+1579,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [5U] >> 3U))));
        bufp->chgBit(oldp+1580,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [6U]));
        bufp->chgBit(oldp+1581,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [6U] >> 3U))));
        bufp->chgCData(oldp+1582,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [6U]),4);
        bufp->chgBit(oldp+1583,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [6U] >> 3U))));
        bufp->chgBit(oldp+1584,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [7U]));
        bufp->chgBit(oldp+1585,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                       [7U] >> 3U))));
        bufp->chgCData(oldp+1586,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [7U]),4);
        bufp->chgBit(oldp+1587,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                       [7U] >> 3U))));
        bufp->chgIData(oldp+1588,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b),32);
        bufp->chgIData(oldp+1589,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+1590,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b),32);
        bufp->chgIData(oldp+1591,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+1592,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+1593,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x52U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x97U])))) {
        bufp->chgBit(oldp+1594,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[0]));
        bufp->chgBit(oldp+1595,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[1]));
        bufp->chgBit(oldp+1596,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[2]));
        bufp->chgBit(oldp+1597,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[3]));
        bufp->chgBit(oldp+1598,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[4]));
        bufp->chgBit(oldp+1599,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[5]));
        bufp->chgBit(oldp+1600,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[6]));
        bufp->chgBit(oldp+1601,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[7]));
        bufp->chgBit(oldp+1602,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[8]));
        bufp->chgBit(oldp+1603,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[9]));
        bufp->chgBit(oldp+1604,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[10]));
        bufp->chgBit(oldp+1605,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[11]));
        bufp->chgBit(oldp+1606,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[12]));
        bufp->chgBit(oldp+1607,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[13]));
        bufp->chgBit(oldp+1608,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[14]));
        bufp->chgBit(oldp+1609,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__opMatrixReady[15]));
        bufp->chgSData(oldp+1610,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[0U])),16);
        bufp->chgSData(oldp+1611,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[0U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1612,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[1U])),16);
        bufp->chgSData(oldp+1613,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[1U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1614,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[2U])),16);
        bufp->chgSData(oldp+1615,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[2U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1616,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[3U])),16);
        bufp->chgSData(oldp+1617,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[3U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1618,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[4U])),16);
        bufp->chgSData(oldp+1619,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[4U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1620,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[5U])),16);
        bufp->chgSData(oldp+1621,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[5U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1622,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[6U])),16);
        bufp->chgSData(oldp+1623,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[6U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1624,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[7U])),16);
        bufp->chgSData(oldp+1625,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__nextMatrix[7U] 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1626,((0xffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__dispatchVector)),16);
        bufp->chgSData(oldp+1627,((vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__dispatchVector 
                                   >> 0x10U)),16);
        bufp->chgSData(oldp+1628,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__producerMatrix__DOT__wakeupVector),16);
    }
    if (VL_UNLIKELY((vlSelfRef.__Vm_traceActivity[0x53U]))) {
        bufp->chgIData(oldp+1629,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextMemReadSerial),32);
        bufp->chgIData(oldp+1630,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextMemWriteSerial),32);
        __Vtemp_1[0U] = (IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                  [4U][2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                [4U][1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [4U][0U])) 
                                                  >> 4U))));
        __Vtemp_1[1U] = (IData)(((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                   [4U][2U])) 
                                   << 0x3cU) | (((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                 [4U][1U])) 
                                                 << 0x1cU) 
                                                | ((QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                   [4U][0U])) 
                                                   >> 4U))) 
                                 >> 0x20U));
        __Vtemp_1[2U] = 0U;
        __Vtemp_1[3U] = 0U;
        bufp->chgWData(oldp+1631,(__Vtemp_1),128);
        bufp->chgBit(oldp+1635,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [4U][2U] >> 4U))));
        bufp->chgIData(oldp+1636,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                         [4U][0U] >> 2U))),32);
        bufp->chgQData(oldp+1637,((QData)((IData)((
                                                   (2U 
                                                    & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                       [1U][0U] 
                                                       << 1U)) 
                                                   | (1U 
                                                      & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                         [1U][0U] 
                                                         >> 1U)))))),64);
        bufp->chgBit(oldp+1639,((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__processLatencyCount))));
        bufp->chgIData(oldp+1640,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0U]),32);
        bufp->chgBit(oldp+1641,(vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__serialWE));
        bufp->chgCData(oldp+1642,((0xffU & vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioWriteDataIn)),8);
        bufp->chgCData(oldp+1643,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextMemReadSerial),2);
        bufp->chgBit(oldp+1644,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextMemWriteSerial));
        bufp->chgQData(oldp+1645,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [4U][2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [4U][1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [4U][0U])) 
                                                    >> 4U)))),64);
        bufp->chgCData(oldp+1647,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                         [4U][0U] >> 2U))),2);
        bufp->chgBit(oldp+1648,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                 [1U][0U])));
        bufp->chgBit(oldp+1649,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+1650,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5bU] 
                                       >> 6U))));
        bufp->chgSData(oldp+1651,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5bU] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                                >> 0x1cU)))),10);
        bufp->chgBit(oldp+1652,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5bU] 
                                       >> 0x11U))));
        bufp->chgSData(oldp+1653,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5bU] 
                                             >> 7U))),10);
        bufp->chgBit(oldp+1654,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                       >> 0xeU))));
        bufp->chgSData(oldp+1655,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                             >> 4U))),10);
        bufp->chgBit(oldp+1656,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                       >> 3U))));
        bufp->chgBit(oldp+1657,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                       >> 2U))));
        bufp->chgBit(oldp+1658,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                       >> 0x1bU))));
        bufp->chgSData(oldp+1659,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                             >> 0x11U))),10);
        bufp->chgBit(oldp+1660,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1661,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                       >> 0xfU))));
        bufp->chgBit(oldp+1662,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x59U] 
                                       >> 0xfU))));
        bufp->chgSData(oldp+1663,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x59U] 
                                             >> 5U))),10);
        bufp->chgCData(oldp+1664,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x59U] 
                                           >> 1U))),4);
        bufp->chgCData(oldp+1665,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x59U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x58U] 
                                          >> 0x1eU)))),3);
        bufp->chgBit(oldp+1666,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                       >> 1U))));
        bufp->chgSData(oldp+1667,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x5aU] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x59U] 
                                                >> 0x17U)))),10);
        bufp->chgCData(oldp+1668,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x59U] 
                                           >> 0x13U))),4);
        bufp->chgCData(oldp+1669,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x59U] 
                                         >> 0x10U))),3);
        bufp->chgBit(oldp+1670,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x56U] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+1671,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x56U] 
                                       >> 0xbU))));
        bufp->chgBit(oldp+1672,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x56U] 
                                       >> 0xaU))));
        bufp->chgSData(oldp+1673,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x56U])),10);
        bufp->chgCData(oldp+1674,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x55U] 
                                   >> 0x1eU)),2);
        bufp->chgIData(oldp+1675,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x55U] 
                                    << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x54U] 
                                              >> 0x1eU))),32);
        bufp->chgIData(oldp+1676,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x54U] 
                                    << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                              >> 0x1eU))),32);
        bufp->chgBit(oldp+1677,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1678,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1679,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x58U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1680,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x58U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1681,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x58U] 
                                       >> 0x1bU))));
        bufp->chgSData(oldp+1682,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x58U] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1683,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x58U] 
                                         >> 0xfU))),2);
        bufp->chgIData(oldp+1684,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x58U] 
                                    << 0x11U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x57U] 
                                                 >> 0xfU))),32);
        bufp->chgIData(oldp+1685,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x57U] 
                                    << 0x11U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x56U] 
                                                 >> 0xfU))),32);
        bufp->chgBit(oldp+1686,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x56U] 
                                       >> 0xeU))));
        bufp->chgBit(oldp+1687,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x56U] 
                                       >> 0xdU))));
        bufp->chgBit(oldp+1688,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                       >> 0xeU))));
        bufp->chgSData(oldp+1689,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1690,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                         >> 2U))),2);
        bufp->chgBit(oldp+1691,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                       >> 0x1bU))));
        bufp->chgSData(oldp+1692,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1693,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                         >> 0xfU))),2);
        bufp->chgBit(oldp+1694,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                       >> 0xbU))));
        bufp->chgSData(oldp+1695,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                             >> 1U))),10);
        bufp->chgCData(oldp+1696,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                          >> 0x1fU)))),2);
        bufp->chgBit(oldp+1697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+1698,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+1699,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+1700,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 0x17U))));
        bufp->chgCData(oldp+1701,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                            >> 0x11U))),6);
        bufp->chgBit(oldp+1702,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1703,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1704,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                            >> 0xaU))),5);
        bufp->chgBit(oldp+1705,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 9U))));
        bufp->chgCData(oldp+1706,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                            >> 3U))),6);
        bufp->chgBit(oldp+1707,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 2U))));
        bufp->chgBit(oldp+1708,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                       >> 1U))));
        bufp->chgCData(oldp+1709,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4fU] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                               >> 0x1cU)))),5);
        bufp->chgBit(oldp+1710,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1711,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1712,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+1713,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1714,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                            >> 0xeU))),5);
        bufp->chgBit(oldp+1715,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                       >> 0xdU))));
        bufp->chgCData(oldp+1716,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1717,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU] 
                                       >> 6U))));
        bufp->chgCData(oldp+1718,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4eU])),6);
        bufp->chgCData(oldp+1719,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                   >> 0x1aU)),6);
        bufp->chgCData(oldp+1720,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                           >> 0x16U))),4);
        bufp->chgBit(oldp+1721,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                       >> 1U))));
        bufp->chgSData(oldp+1722,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x53U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                                >> 0x17U)))),10);
        bufp->chgCData(oldp+1723,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                         >> 0x15U))),2);
        bufp->chgBit(oldp+1724,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+1725,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1726,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                            >> 0xeU))),5);
        bufp->chgBit(oldp+1727,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                       >> 0xdU))));
        bufp->chgCData(oldp+1728,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                       >> 6U))));
        bufp->chgBit(oldp+1730,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U] 
                                       >> 5U))));
        bufp->chgCData(oldp+1731,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x52U])),5);
        bufp->chgBit(oldp+1732,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                 >> 0x1fU)));
        bufp->chgCData(oldp+1733,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                            >> 0x19U))),6);
        bufp->chgBit(oldp+1734,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1735,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                       >> 0x17U))));
        bufp->chgCData(oldp+1736,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                            >> 0x12U))),5);
        bufp->chgBit(oldp+1737,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+1738,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1739,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                       >> 0xaU))));
        bufp->chgBit(oldp+1740,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                       >> 9U))));
        bufp->chgCData(oldp+1741,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                            >> 4U))),5);
        bufp->chgBit(oldp+1742,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1743,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x51U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1744,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                       >> 0x1cU))));
        bufp->chgCData(oldp+1745,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                            >> 0x16U))),6);
        bufp->chgCData(oldp+1746,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                            >> 0x10U))),6);
        bufp->chgCData(oldp+1747,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x50U] 
                                           >> 0xcU))),4);
        bufp->chgBit(oldp+1748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                       >> 7U))));
        bufp->chgBit(oldp+1749,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                       >> 6U))));
        bufp->chgSData(oldp+1750,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1751,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                         >> 0x1aU))),2);
        bufp->chgBit(oldp+1752,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                       >> 0x14U))));
        bufp->chgSData(oldp+1754,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                             >> 0xaU))),10);
        bufp->chgCData(oldp+1755,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4dU] 
                                         >> 8U))),2);
        bufp->chgBit(oldp+1756,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                       >> 0xbU))));
        bufp->chgBit(oldp+1757,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                       >> 0xaU))));
        bufp->chgSData(oldp+1758,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU])),10);
        bufp->chgCData(oldp+1759,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4bU] 
                                   >> 0x1eU)),2);
        bufp->chgBit(oldp+1760,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                       >> 0x19U))));
        bufp->chgBit(oldp+1761,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                       >> 0x18U))));
        bufp->chgSData(oldp+1762,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                             >> 0xeU))),10);
        bufp->chgCData(oldp+1763,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4cU] 
                                         >> 0xcU))),2);
        bufp->chgBit(oldp+1764,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x48U] 
                                       >> 7U))));
        bufp->chgBit(oldp+1765,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x48U] 
                                       >> 6U))));
        bufp->chgSData(oldp+1766,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x48U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x47U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1767,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x47U] 
                                         >> 0x1aU))),2);
        bufp->chgIData(oldp+1768,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x47U] 
                                    << 6U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x46U] 
                                              >> 0x1aU))),32);
        bufp->chgIData(oldp+1769,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x46U] 
                                    << 6U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x45U] 
                                              >> 0x1aU))),32);
        bufp->chgIData(oldp+1770,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x45U] 
                                    << 6U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                              >> 0x1aU))),32);
        bufp->chgCData(oldp+1771,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1772,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                         >> 0x13U))),3);
        bufp->chgBit(oldp+1773,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                       >> 0x12U))));
        bufp->chgBit(oldp+1774,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4bU] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1775,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4bU] 
                                       >> 0x1cU))));
        bufp->chgSData(oldp+1776,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4bU] 
                                             >> 0x12U))),10);
        bufp->chgCData(oldp+1777,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4bU] 
                                         >> 0x10U))),2);
        bufp->chgIData(oldp+1778,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4bU] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4aU] 
                                                 >> 0x10U))),32);
        bufp->chgIData(oldp+1779,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x4aU] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x49U] 
                                                 >> 0x10U))),32);
        bufp->chgIData(oldp+1780,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x49U] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x48U] 
                                                 >> 0x10U))),32);
        bufp->chgCData(oldp+1781,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x48U] 
                                           >> 0xcU))),4);
        bufp->chgCData(oldp+1782,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x48U] 
                                         >> 9U))),3);
        bufp->chgBit(oldp+1783,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x48U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                       >> 3U))));
        bufp->chgBit(oldp+1785,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                       >> 2U))));
        bufp->chgSData(oldp+1786,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                                >> 0x18U)))),10);
        bufp->chgCData(oldp+1787,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                         >> 0x16U))),2);
        bufp->chgBit(oldp+1788,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1789,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                       >> 0x10U))));
        bufp->chgSData(oldp+1790,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                             >> 6U))),10);
        bufp->chgCData(oldp+1791,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x44U] 
                                         >> 4U))),2);
        bufp->chgBit(oldp+1792,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1793,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                       >> 0x14U))));
        bufp->chgSData(oldp+1794,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                             >> 0xaU))),10);
        bufp->chgCData(oldp+1795,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                         >> 8U))),2);
        bufp->chgBit(oldp+1796,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                       >> 7U))));
        bufp->chgBit(oldp+1797,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                       >> 6U))));
        bufp->chgSData(oldp+1798,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x43U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x42U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1799,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x42U] 
                                         >> 0x1aU))),2);
        bufp->chgCData(oldp+1800,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x42U] 
                                         >> 0x17U))),3);
        bufp->chgCData(oldp+1801,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x42U] 
                                         >> 0x14U))),3);
        bufp->chgSData(oldp+1802,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x41U] 
                                             >> 0x12U))),10);
        bufp->chgCData(oldp+1803,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x41U] 
                                         >> 0x10U))),2);
        bufp->chgSData(oldp+1804,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x42U] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x41U] 
                                                >> 0x1eU)))),10);
        bufp->chgCData(oldp+1805,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x41U] 
                                         >> 0x1cU))),2);
        bufp->chgSData(oldp+1806,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x42U] 
                                             >> 0xaU))),10);
        bufp->chgCData(oldp+1807,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x42U] 
                                         >> 8U))),2);
        bufp->chgIData(oldp+1808,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x41U] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x40U] 
                                                 >> 0x10U))),32);
        bufp->chgIData(oldp+1809,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x40U] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3fU] 
                                                 >> 0x10U))),32);
        bufp->chgIData(oldp+1810,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3fU] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3eU] 
                                                 >> 0x10U))),32);
        bufp->chgBit(oldp+1811,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3eU] 
                                       >> 0xfU))));
        bufp->chgBit(oldp+1812,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3eU] 
                                       >> 0xeU))));
        bufp->chgSData(oldp+1813,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3eU] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1814,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3eU] 
                                         >> 2U))),2);
        bufp->chgBit(oldp+1815,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                       >> 0x13U))));
        bufp->chgBit(oldp+1816,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                       >> 0x12U))));
        bufp->chgSData(oldp+1817,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+1818,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                         >> 6U))),2);
        bufp->chgBit(oldp+1819,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3eU] 
                                       >> 1U))));
        bufp->chgBit(oldp+1820,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3eU])));
        bufp->chgSData(oldp+1821,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                   >> 0x16U)),10);
        bufp->chgCData(oldp+1822,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                         >> 0x14U))),2);
        bufp->chgBit(oldp+1823,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1824,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                       >> 0x16U))));
        bufp->chgSData(oldp+1825,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                             >> 0xcU))),10);
        bufp->chgCData(oldp+1826,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                         >> 0xaU))),2);
        bufp->chgBit(oldp+1827,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                       >> 5U))));
        bufp->chgBit(oldp+1828,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                       >> 4U))));
        bufp->chgSData(oldp+1829,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3dU] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+1830,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                         >> 0x18U))),2);
        bufp->chgBit(oldp+1831,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1832,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                       >> 0x14U))));
        bufp->chgSData(oldp+1833,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                             >> 0xaU))),10);
        bufp->chgCData(oldp+1834,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                         >> 8U))),2);
        bufp->chgIData(oldp+1835,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                    << 0x18U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x37U] 
                                                 >> 8U))),32);
        bufp->chgIData(oldp+1836,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x37U] 
                                    << 0x18U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x36U] 
                                                 >> 8U))),32);
        bufp->chgIData(oldp+1837,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x36U] 
                                    << 0x18U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x35U] 
                                                 >> 8U))),32);
        bufp->chgCData(oldp+1838,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x35U] 
                                         >> 5U))),3);
        bufp->chgCData(oldp+1839,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x35U] 
                                         >> 3U))),2);
        bufp->chgBit(oldp+1840,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x35U] 
                                       >> 2U))));
        bufp->chgBit(oldp+1841,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                       >> 9U))));
        bufp->chgBit(oldp+1842,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                       >> 8U))));
        bufp->chgSData(oldp+1843,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3cU] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3bU] 
                                                >> 0x1eU)))),10);
        bufp->chgCData(oldp+1844,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3bU] 
                                         >> 0x1cU))),2);
        bufp->chgIData(oldp+1845,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3bU] 
                                    << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3aU] 
                                              >> 0x1cU))),32);
        bufp->chgIData(oldp+1846,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x3aU] 
                                    << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x39U] 
                                              >> 0x1cU))),32);
        bufp->chgIData(oldp+1847,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x39U] 
                                    << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                              >> 0x1cU))),32);
        bufp->chgCData(oldp+1848,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                         >> 0x19U))),3);
        bufp->chgCData(oldp+1849,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                         >> 0x17U))),2);
        bufp->chgBit(oldp+1850,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x38U] 
                                       >> 0x16U))));
        bufp->chgBit(oldp+1851,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2cU] 
                                       >> 0xfU))));
        bufp->chgBit(oldp+1852,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2cU] 
                                       >> 0xeU))));
        bufp->chgSData(oldp+1853,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2cU] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1854,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2cU] 
                                         >> 2U))),2);
        bufp->chgBit(oldp+1855,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2cU] 
                                       >> 1U))));
        bufp->chgIData(oldp+1856,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2cU] 
                                    << 0x1fU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2bU] 
                                                 >> 1U))),32);
        bufp->chgBit(oldp+1857,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2bU])));
        bufp->chgBit(oldp+1858,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2aU] 
                                 >> 0x1fU)));
        bufp->chgIData(oldp+1859,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2aU] 
                                    << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x29U] 
                                              >> 0x1fU))),32);
        bufp->chgBit(oldp+1860,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x29U] 
                                       >> 0x1eU))));
        bufp->chgIData(oldp+1861,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x29U] 
                                    << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x28U] 
                                              >> 0x1eU))),32);
        bufp->chgIData(oldp+1862,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x28U] 
                                    << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x27U] 
                                              >> 0x1eU))),32);
        __Vtemp_2[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x24U] 
                          << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x23U] 
                                    >> 0x1eU));
        __Vtemp_2[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x25U] 
                          << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x24U] 
                                    >> 0x1eU));
        __Vtemp_2[2U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x26U] 
                          << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x25U] 
                                    >> 0x1eU));
        __Vtemp_2[3U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x27U] 
                          << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x26U] 
                                    >> 0x1eU));
        bufp->chgWData(oldp+1863,(__Vtemp_2),128);
        bufp->chgBit(oldp+1867,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x35U] 
                                       >> 1U))));
        bufp->chgBit(oldp+1868,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x35U])));
        bufp->chgSData(oldp+1869,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x34U] 
                                   >> 0x16U)),10);
        bufp->chgCData(oldp+1870,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x34U] 
                                         >> 0x14U))),2);
        bufp->chgBit(oldp+1871,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x34U] 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1872,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x34U] 
                                    << 0xdU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x33U] 
                                                >> 0x13U))),32);
        bufp->chgBit(oldp+1873,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x33U] 
                                       >> 0x12U))));
        bufp->chgBit(oldp+1874,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x33U] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+1875,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x33U] 
                                    << 0xfU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x32U] 
                                                >> 0x11U))),32);
        bufp->chgBit(oldp+1876,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x32U] 
                                       >> 0x10U))));
        bufp->chgIData(oldp+1877,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x32U] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x31U] 
                                                 >> 0x10U))),32);
        bufp->chgIData(oldp+1878,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x31U] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x30U] 
                                                 >> 0x10U))),32);
        __Vtemp_3[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2dU] 
                          << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2cU] 
                                       >> 0x10U));
        __Vtemp_3[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2eU] 
                          << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2dU] 
                                       >> 0x10U));
        __Vtemp_3[2U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2fU] 
                          << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2eU] 
                                       >> 0x10U));
        __Vtemp_3[3U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x30U] 
                          << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x2fU] 
                                       >> 0x10U));
        bufp->chgWData(oldp+1879,(__Vtemp_3),128);
        bufp->chgBit(oldp+1883,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1eU] 
                                       >> 0xeU))));
        bufp->chgBit(oldp+1884,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1eU] 
                                       >> 0xdU))));
        bufp->chgSData(oldp+1885,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1eU] 
                                             >> 3U))),10);
        bufp->chgCData(oldp+1886,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1eU] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1887,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1eU])));
        bufp->chgIData(oldp+1888,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1dU]),32);
        __Vtemp_4[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x19U];
        __Vtemp_4[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1aU];
        __Vtemp_4[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1bU];
        __Vtemp_4[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1cU];
        bufp->chgWData(oldp+1889,(__Vtemp_4),128);
        bufp->chgBit(oldp+1893,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x23U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1894,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x23U] 
                                       >> 0x1cU))));
        bufp->chgSData(oldp+1895,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x23U] 
                                             >> 0x12U))),10);
        bufp->chgCData(oldp+1896,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x23U] 
                                         >> 0x10U))),2);
        bufp->chgBit(oldp+1897,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x23U] 
                                       >> 0xfU))));
        bufp->chgIData(oldp+1898,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x23U] 
                                    << 0x11U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x22U] 
                                                 >> 0xfU))),32);
        __Vtemp_5[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1fU] 
                          << 0x11U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1eU] 
                                       >> 0xfU));
        __Vtemp_5[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x20U] 
                          << 0x11U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x1fU] 
                                       >> 0xfU));
        __Vtemp_5[2U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x21U] 
                          << 0x11U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x20U] 
                                       >> 0xfU));
        __Vtemp_5[3U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x22U] 
                          << 0x11U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x21U] 
                                       >> 0xfU));
        bufp->chgWData(oldp+1899,(__Vtemp_5),128);
        bufp->chgBit(oldp+1903,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1904,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                       >> 0x10U))));
        bufp->chgSData(oldp+1905,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                             >> 6U))),10);
        bufp->chgCData(oldp+1906,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                         >> 4U))),2);
        bufp->chgBit(oldp+1907,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1908,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                       >> 0x1eU))));
        bufp->chgSData(oldp+1909,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                             >> 0x14U))),10);
        bufp->chgCData(oldp+1910,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                         >> 0x12U))),2);
        bufp->chgBit(oldp+1911,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                       >> 3U))));
        bufp->chgBit(oldp+1912,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                       >> 2U))));
        bufp->chgSData(oldp+1913,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x18U] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                                >> 0x18U)))),10);
        bufp->chgCData(oldp+1914,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                         >> 0x16U))),2);
        bufp->chgBit(oldp+1915,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1916,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                       >> 0x14U))));
        bufp->chgSData(oldp+1917,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                             >> 0xaU))),10);
        bufp->chgCData(oldp+1918,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                         >> 8U))),2);
        bufp->chgCData(oldp+1919,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                            >> 3U))),5);
        bufp->chgCData(oldp+1920,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x17U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x16U] 
                                               >> 0x1eU)))),5);
        bufp->chgSData(oldp+1921,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x15U] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1922,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x15U] 
                                         >> 2U))),2);
        bufp->chgSData(oldp+1923,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x15U] 
                                             >> 0x10U))),10);
        bufp->chgCData(oldp+1924,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x15U] 
                                         >> 0xeU))),2);
        bufp->chgSData(oldp+1925,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x16U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x15U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1926,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x15U] 
                                         >> 0x1aU))),2);
        bufp->chgSData(oldp+1927,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x16U] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+1928,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x16U] 
                                         >> 6U))),2);
        bufp->chgSData(oldp+1929,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x16U] 
                                             >> 0x14U))),10);
        bufp->chgCData(oldp+1930,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x16U] 
                                         >> 0x12U))),2);
        bufp->chgIData(oldp+1931,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x15U] 
                                    << 0x1eU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x14U] 
                                                 >> 2U))),32);
        bufp->chgIData(oldp+1932,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x14U] 
                                    << 0x1eU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x13U] 
                                                 >> 2U))),32);
        bufp->chgIData(oldp+1933,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x13U] 
                                    << 0x1eU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x12U] 
                                                 >> 2U))),32);
        bufp->chgIData(oldp+1934,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x12U] 
                                    << 0x1eU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x11U] 
                                                 >> 2U))),32);
        bufp->chgBit(oldp+1935,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x11U] 
                                       >> 1U))));
        bufp->chgBit(oldp+1936,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x11U])));
        bufp->chgSData(oldp+1937,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                   >> 0x16U)),10);
        bufp->chgCData(oldp+1938,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                         >> 0x14U))),2);
        bufp->chgBit(oldp+1939,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1940,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 0x1cU))));
        bufp->chgSData(oldp+1941,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                             >> 0x12U))),10);
        bufp->chgCData(oldp+1942,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                         >> 0x10U))),2);
        bufp->chgBit(oldp+1943,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 0xfU))));
        bufp->chgBit(oldp+1944,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 0xeU))));
        bufp->chgCData(oldp+1945,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                            >> 8U))),6);
        bufp->chgBit(oldp+1946,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                       >> 0x13U))));
        bufp->chgBit(oldp+1947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                       >> 0x12U))));
        bufp->chgSData(oldp+1948,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+1949,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                         >> 6U))),2);
        bufp->chgBit(oldp+1950,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                       >> 5U))));
        bufp->chgBit(oldp+1951,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                       >> 4U))));
        bufp->chgCData(oldp+1952,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0x10U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+1953,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1954,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x19U))));
        bufp->chgBit(oldp+1955,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+1956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+1957,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1958,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1959,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+1960,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1961,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU])));
        bufp->chgBit(oldp+1962,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 1U))));
        bufp->chgBit(oldp+1963,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 2U))));
        bufp->chgBit(oldp+1964,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 3U))));
        bufp->chgBit(oldp+1965,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 4U))));
        bufp->chgBit(oldp+1966,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 5U))));
        bufp->chgBit(oldp+1967,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 6U))));
        bufp->chgBit(oldp+1968,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xfU] 
                                       >> 7U))));
        bufp->chgBit(oldp+1969,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                       >> 0x14U))));
        bufp->chgSData(oldp+1970,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                             >> 0xaU))),10);
        bufp->chgCData(oldp+1971,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                         >> 8U))),2);
        bufp->chgBit(oldp+1972,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                       >> 1U))));
        bufp->chgSData(oldp+1973,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                                >> 0x17U)))),10);
        bufp->chgCData(oldp+1974,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                         >> 0x15U))),2);
        bufp->chgBit(oldp+1975,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                       >> 0xeU))));
        bufp->chgSData(oldp+1976,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1977,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                         >> 2U))),2);
        bufp->chgBit(oldp+1978,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                       >> 0x1bU))));
        bufp->chgSData(oldp+1979,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1980,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                         >> 0xfU))),2);
        bufp->chgBit(oldp+1981,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xaU] 
                                       >> 8U))));
        bufp->chgSData(oldp+1982,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xaU] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                                >> 0x1eU)))),10);
        bufp->chgCData(oldp+1983,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[9U] 
                                         >> 0x1cU))),2);
        bufp->chgBit(oldp+1984,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xaU] 
                                       >> 0x15U))));
        bufp->chgSData(oldp+1985,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xaU] 
                                             >> 0xbU))),10);
        bufp->chgCData(oldp+1986,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xaU] 
                                         >> 9U))),2);
        bufp->chgBit(oldp+1987,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                       >> 2U))));
        bufp->chgSData(oldp+1988,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xaU] 
                                                >> 0x18U)))),10);
        bufp->chgCData(oldp+1989,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xaU] 
                                         >> 0x16U))),2);
        bufp->chgBit(oldp+1990,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                       >> 0xfU))));
        bufp->chgSData(oldp+1991,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                             >> 5U))),10);
        bufp->chgCData(oldp+1992,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                         >> 3U))),2);
        bufp->chgBit(oldp+1993,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                       >> 0x1cU))));
        bufp->chgSData(oldp+1994,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                             >> 0x12U))),10);
        bufp->chgCData(oldp+1995,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                         >> 0x10U))),2);
        bufp->chgBit(oldp+1996,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xcU] 
                                       >> 9U))));
        bufp->chgSData(oldp+1997,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xcU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                                >> 0x1fU)))),10);
        bufp->chgCData(oldp+1998,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xbU] 
                                         >> 0x1dU))),2);
        bufp->chgBit(oldp+1999,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xcU] 
                                       >> 0x16U))));
        bufp->chgSData(oldp+2000,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xcU] 
                                             >> 0xcU))),10);
        bufp->chgCData(oldp+2001,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xcU] 
                                         >> 0xaU))),2);
        bufp->chgBit(oldp+2002,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                       >> 3U))));
        bufp->chgSData(oldp+2003,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xcU] 
                                                >> 0x19U)))),10);
        bufp->chgCData(oldp+2004,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xcU] 
                                         >> 0x17U))),2);
        bufp->chgBit(oldp+2005,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                       >> 0x10U))));
        bufp->chgSData(oldp+2006,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                             >> 6U))),10);
        bufp->chgCData(oldp+2007,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                         >> 4U))),2);
        bufp->chgBit(oldp+2008,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                       >> 0x1dU))));
        bufp->chgSData(oldp+2009,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2010,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                         >> 0x11U))),2);
        bufp->chgBit(oldp+2011,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0xaU))));
        bufp->chgSData(oldp+2012,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU])),10);
        bufp->chgCData(oldp+2013,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xdU] 
                                   >> 0x1eU)),2);
        bufp->chgBit(oldp+2014,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                       >> 0x17U))));
        bufp->chgSData(oldp+2015,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                             >> 0xdU))),10);
        bufp->chgCData(oldp+2016,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[0xeU] 
                                         >> 0xbU))),2);
        bufp->chgBit(oldp+2017,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                       >> 7U))));
        bufp->chgCData(oldp+2018,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                            >> 1U))),6);
        bufp->chgCData(oldp+2019,((0x7fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[8U] 
                                             << 6U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                               >> 0x1aU)))),7);
        bufp->chgBit(oldp+2020,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x19U))));
        bufp->chgBit(oldp+2021,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+2022,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+2023,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x16U))));
        bufp->chgBit(oldp+2024,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+2025,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+2026,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x13U))));
        bufp->chgBit(oldp+2027,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x12U))));
        bufp->chgBit(oldp+2028,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+2029,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+2030,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0xfU))));
        bufp->chgBit(oldp+2031,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0xeU))));
        bufp->chgBit(oldp+2032,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0xdU))));
        bufp->chgBit(oldp+2033,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+2034,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0xbU))));
        bufp->chgBit(oldp+2035,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 0xaU))));
        bufp->chgBit(oldp+2036,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 9U))));
        bufp->chgBit(oldp+2037,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 8U))));
        bufp->chgBit(oldp+2038,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 7U))));
        bufp->chgCData(oldp+2039,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                            >> 2U))),5);
        bufp->chgBit(oldp+2040,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U] 
                                       >> 1U))));
        bufp->chgBit(oldp+2041,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[7U])));
        bufp->chgIData(oldp+2042,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[6U]),32);
        bufp->chgIData(oldp+2043,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[5U]),32);
        bufp->chgIData(oldp+2044,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[4U]),32);
        bufp->chgIData(oldp+2045,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[3U]),32);
        bufp->chgIData(oldp+2046,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[2U]),32);
        bufp->chgIData(oldp+2047,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__debug__DOT__debugRegister[1U]),32);
        bufp->chgIData(oldp+2048,(vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioWriteDataIn),32);
        bufp->chgBit(oldp+2049,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intEX
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+2050,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intEX
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+2051,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intEX
                                 [0U])));
        bufp->chgBit(oldp+2052,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intEX
                                       [1U] >> 7U))));
        bufp->chgCData(oldp+2053,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intEX
                                            [1U] >> 1U))),6);
        bufp->chgBit(oldp+2054,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intEX
                                 [1U])));
        bufp->chgBit(oldp+2055,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intWB
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+2056,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intWB
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+2057,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intWB
                                 [0U])));
        bufp->chgBit(oldp+2058,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intWB
                                       [1U] >> 7U))));
        bufp->chgCData(oldp+2059,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intWB
                                            [1U] >> 1U))),6);
        bufp->chgBit(oldp+2060,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intWB
                                 [1U])));
        bufp->chgBit(oldp+2061,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memEX
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+2062,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memEX
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+2063,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memEX
                                 [0U])));
        bufp->chgBit(oldp+2064,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memMT
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+2065,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memMT
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+2066,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memMT
                                 [0U])));
        bufp->chgBit(oldp+2067,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memMA
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+2068,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memMA
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+2069,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memMA
                                 [0U])));
        bufp->chgBit(oldp+2070,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memWB
                                       [0U] >> 7U))));
        bufp->chgCData(oldp+2071,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memWB
                                            [0U] >> 1U))),6);
        bufp->chgBit(oldp+2072,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memWB
                                 [0U])));
        bufp->chgBit(oldp+2073,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntRR__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2074,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntRR__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2075,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntRR__DOT__body))));
        bufp->chgBit(oldp+2076,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntEX__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2077,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntEX__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2078,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntEX__DOT__body))));
        bufp->chgBit(oldp+2079,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntRR__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2080,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntRR__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2081,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntRR__DOT__body))));
        bufp->chgBit(oldp+2082,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntEX__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2083,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntEX__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2084,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntEX__DOT__body))));
        bufp->chgBit(oldp+2085,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemRR__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2086,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemRR__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2087,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemRR__DOT__body))));
        bufp->chgBit(oldp+2088,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemEX__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2089,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemEX__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2090,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemEX__DOT__body))));
        bufp->chgBit(oldp+2091,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMT__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2092,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMT__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2093,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMT__DOT__body))));
        bufp->chgBit(oldp+2094,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMA__DOT__body) 
                                       >> 7U))));
        bufp->chgCData(oldp+2095,((0x3fU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMA__DOT__body) 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2096,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMA__DOT__body))));
        bufp->chgBit(oldp+2097,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intEX
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2098,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intEX
                                          [0U])),32);
        bufp->chgBit(oldp+2099,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intEX
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2100,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intEX
                                          [1U])),32);
        bufp->chgBit(oldp+2101,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intWB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2102,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intWB
                                          [0U])),32);
        bufp->chgBit(oldp+2103,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intWB
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2104,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intWB
                                          [1U])),32);
        bufp->chgBit(oldp+2105,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memMA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2106,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memMA
                                          [0U])),32);
        bufp->chgBit(oldp+2107,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memWB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2108,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memWB
                                          [0U])),32);
        bufp->chgBit(oldp+2109,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__0__KET____DOT__stgIntEX__DOT__body 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2110,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__0__KET____DOT__stgIntEX__DOT__body)),32);
        bufp->chgBit(oldp+2111,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__0__KET____DOT__stgIntWB__DOT__body 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2112,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__0__KET____DOT__stgIntWB__DOT__body)),32);
        bufp->chgBit(oldp+2113,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__1__KET____DOT__stgIntEX__DOT__body 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2114,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__1__KET____DOT__stgIntEX__DOT__body)),32);
        bufp->chgBit(oldp+2115,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__1__KET____DOT__stgIntWB__DOT__body 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2116,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgInt__BRA__1__KET____DOT__stgIntWB__DOT__body)),32);
        bufp->chgBit(oldp+2117,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgMem__BRA__0__KET____DOT__stgMemMA__DOT__body 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2118,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgMem__BRA__0__KET____DOT__stgMemMA__DOT__body)),32);
        bufp->chgBit(oldp+2119,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgMem__BRA__0__KET____DOT__stgMemWB__DOT__body 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2120,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__stgMem__BRA__0__KET____DOT__stgMemWB__DOT__body)),32);
        bufp->chgCData(oldp+2121,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__cacheFlushManager__DOT__regPhase),2);
        bufp->chgBit(oldp+2122,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__cacheFlushManager__DOT__regIcFlushComplete));
        bufp->chgBit(oldp+2123,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__cacheFlushManager__DOT__regDcFlushComplete));
        bufp->chgBit(oldp+2124,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__cacheFlushManager__DOT__cacheFlushComplete));
        bufp->chgIData(oldp+2125,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2126,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__j),32);
        bufp->chgIData(oldp+2127,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__unnamedblk6__DOT__i),32);
        bufp->chgBit(oldp+2128,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__pipeReg
                                       [0U] >> 4U))));
        bufp->chgCData(oldp+2129,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__pipeReg
                                   [0U])),4);
        bufp->chgIData(oldp+2130,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexIsStage__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+2131,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2132,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRwStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2133,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0xaU] 
                                   >> 8U)),24);
        bufp->chgBit(oldp+2134,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0xaU] 
                                       >> 7U))));
        bufp->chgCData(oldp+2135,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0xaU] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+2136,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0xaU] 
                                       >> 3U))));
        bufp->chgCData(oldp+2137,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0xaU])),3);
        bufp->chgIData(oldp+2138,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[9U] 
                                   >> 0xcU)),20);
        bufp->chgBit(oldp+2139,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[9U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+2140,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[9U] 
                                         >> 8U))),3);
        bufp->chgBit(oldp+2141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[9U] 
                                       >> 7U))));
        bufp->chgCData(oldp+2142,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[9U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+2143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[9U] 
                                       >> 3U))));
        bufp->chgCData(oldp+2144,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[9U])),3);
        bufp->chgIData(oldp+2145,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[8U] 
                                   >> 0xcU)),20);
        bufp->chgBit(oldp+2146,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[8U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+2147,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[8U] 
                                         >> 8U))),3);
        bufp->chgBit(oldp+2148,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[8U] 
                                       >> 7U))));
        bufp->chgCData(oldp+2149,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[8U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+2150,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[8U] 
                                       >> 3U))));
        bufp->chgCData(oldp+2151,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[8U])),3);
        bufp->chgBit(oldp+2152,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[7U] 
                                 >> 0x1fU)));
        bufp->chgIData(oldp+2153,((0x3ffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[7U] 
                                                 >> 5U))),26);
        bufp->chgCData(oldp+2154,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[7U])),5);
        bufp->chgIData(oldp+2155,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[6U] 
                                   >> 2U)),30);
        bufp->chgCData(oldp+2156,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[6U])),2);
        bufp->chgIData(oldp+2157,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[5U]),32);
        bufp->chgIData(oldp+2158,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[4U]),32);
        bufp->chgIData(oldp+2159,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[3U]),32);
        bufp->chgIData(oldp+2160,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[2U]),32);
        bufp->chgIData(oldp+2161,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[1U]),32);
        bufp->chgIData(oldp+2162,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0U] 
                                   >> 8U)),24);
        bufp->chgCData(oldp+2163,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0U] 
                                         >> 5U))),3);
        bufp->chgBit(oldp+2164,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0U] 
                                       >> 4U))));
        bufp->chgBit(oldp+2165,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0U] 
                                       >> 3U))));
        bufp->chgBit(oldp+2166,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0U] 
                                       >> 2U))));
        bufp->chgBit(oldp+2167,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0U] 
                                       >> 1U))));
        bufp->chgBit(oldp+2168,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrReg[0U])));
        bufp->chgCData(oldp+2169,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__regCommitNum),2);
        bufp->chgCData(oldp+2170,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__externalInterruptCodeReg),5);
        bufp->chgBit(oldp+2171,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missAddr
                                       [0U] >> 0x15U))));
        bufp->chgBit(oldp+2172,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missAddr
                                       [0U] >> 0x14U))));
        bufp->chgIData(oldp+2173,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missAddr
                                   [0U])),20);
        bufp->chgBit(oldp+2174,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missAddr
                                       [1U] >> 0x15U))));
        bufp->chgBit(oldp+2175,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missAddr
                                       [1U] >> 0x14U))));
        bufp->chgIData(oldp+2176,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missAddr
                                   [1U])),20);
        bufp->chgBit(oldp+2177,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missIsUncachable[0]));
        bufp->chgBit(oldp+2178,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missIsUncachable[1]));
        bufp->chgCData(oldp+2179,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missActiveListPtr[0]),6);
        bufp->chgCData(oldp+2180,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missActiveListPtr[1]),6);
        bufp->chgBit(oldp+2181,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__dcReadReqReg[0]));
        bufp->chgBit(oldp+2182,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuCacheGrtReg[0]));
        bufp->chgBit(oldp+2183,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuCacheGrtReg[1]));
        bufp->chgBit(oldp+2184,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__dcWriteReqReg));
        bufp->chgCData(oldp+2185,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [0U][0U][0U]),8);
        bufp->chgCData(oldp+2186,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [0U][0U][1U]),8);
        bufp->chgCData(oldp+2187,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [0U][1U][0U]),8);
        bufp->chgCData(oldp+2188,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [0U][1U][1U]),8);
        bufp->chgCData(oldp+2189,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [1U][0U][0U]),8);
        bufp->chgCData(oldp+2190,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [1U][0U][1U]),8);
        bufp->chgCData(oldp+2191,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [1U][1U][0U]),8);
        bufp->chgCData(oldp+2192,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [1U][1U][1U]),8);
        bufp->chgCData(oldp+2193,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [2U][0U][0U]),8);
        bufp->chgCData(oldp+2194,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [2U][0U][1U]),8);
        bufp->chgCData(oldp+2195,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [2U][1U][0U]),8);
        bufp->chgCData(oldp+2196,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [2U][1U][1U]),8);
        bufp->chgCData(oldp+2197,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [3U][0U][0U]),8);
        bufp->chgCData(oldp+2198,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [3U][0U][1U]),8);
        bufp->chgCData(oldp+2199,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [3U][1U][0U]),8);
        bufp->chgCData(oldp+2200,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [3U][1U][1U]),8);
        bufp->chgCData(oldp+2201,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [4U][0U][0U]),8);
        bufp->chgCData(oldp+2202,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [4U][0U][1U]),8);
        bufp->chgCData(oldp+2203,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [4U][1U][0U]),8);
        bufp->chgCData(oldp+2204,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [4U][1U][1U]),8);
        bufp->chgCData(oldp+2205,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [5U][0U][0U]),8);
        bufp->chgCData(oldp+2206,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [5U][0U][1U]),8);
        bufp->chgCData(oldp+2207,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [5U][1U][0U]),8);
        bufp->chgCData(oldp+2208,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [5U][1U][1U]),8);
        bufp->chgCData(oldp+2209,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [6U][0U][0U]),8);
        bufp->chgCData(oldp+2210,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [6U][0U][1U]),8);
        bufp->chgCData(oldp+2211,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [6U][1U][0U]),8);
        bufp->chgCData(oldp+2212,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [6U][1U][1U]),8);
        bufp->chgCData(oldp+2213,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [7U][0U][0U]),8);
        bufp->chgCData(oldp+2214,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [7U][0U][1U]),8);
        bufp->chgCData(oldp+2215,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [7U][1U][0U]),8);
        bufp->chgCData(oldp+2216,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOut
                                  [7U][1U][1U]),8);
        bufp->chgBit(oldp+2217,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDirtyOut
                                [0U][0U]));
        bufp->chgBit(oldp+2218,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDirtyOut
                                [0U][1U]));
        bufp->chgBit(oldp+2219,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDirtyOut
                                [1U][0U]));
        bufp->chgBit(oldp+2220,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDirtyOut
                                [1U][1U]));
        bufp->chgBit(oldp+2221,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayReadWayReg[0]));
        bufp->chgBit(oldp+2222,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayReadWayReg[1]));
        bufp->chgBit(oldp+2223,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayReadWay[0]));
        bufp->chgBit(oldp+2224,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayReadWay[1]));
        bufp->chgBit(oldp+2225,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDoesReadEvictedWayReg[0]));
        bufp->chgBit(oldp+2226,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDoesReadEvictedWayReg[1]));
        bufp->chgQData(oldp+2227,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOutTmp
                                  [0U][0U]),64);
        bufp->chgQData(oldp+2229,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOutTmp
                                  [0U][1U]),64);
        bufp->chgQData(oldp+2231,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOutTmp
                                  [1U][0U]),64);
        bufp->chgQData(oldp+2233,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayOutTmp
                                  [1U][1U]),64);
        bufp->chgBit(oldp+2235,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                       [0U][0U] >> 0xbU))));
        bufp->chgSData(oldp+2236,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                   [0U][0U])),11);
        bufp->chgBit(oldp+2237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                       [0U][1U] >> 0xbU))));
        bufp->chgSData(oldp+2238,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                   [0U][1U])),11);
        bufp->chgBit(oldp+2239,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                       [1U][0U] >> 0xbU))));
        bufp->chgSData(oldp+2240,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                   [1U][0U])),11);
        bufp->chgBit(oldp+2241,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                       [1U][1U] >> 0xbU))));
        bufp->chgSData(oldp+2242,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOut
                                   [1U][1U])),11);
        bufp->chgSData(oldp+2243,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
                                  [0U][0U]),12);
        bufp->chgSData(oldp+2244,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
                                  [0U][1U]),12);
        bufp->chgSData(oldp+2245,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
                                  [1U][0U]),12);
        bufp->chgSData(oldp+2246,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
                                  [1U][1U]),12);
        bufp->chgBit(oldp+2247,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayOut
                                [0U][0U]));
        bufp->chgBit(oldp+2248,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayOut
                                [0U][1U]));
        bufp->chgBit(oldp+2249,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayOutFlat[0]));
        bufp->chgBit(oldp+2250,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayOutFlat[1]));
        bufp->chgBit(oldp+2251,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayResult[0]));
        bufp->chgBit(oldp+2252,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayResult[1]));
        bufp->chgCData(oldp+2253,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__rstIndex),8);
        bufp->chgBit(oldp+2254,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__dirtyArray__rv[0]));
        bufp->chgBit(oldp+2255,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__dirtyArray__rv[1]));
        bufp->chgCData(oldp+2256,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2257,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2258,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2259,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2260,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2261,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2262,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2263,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2264,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2265,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2266,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2267,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2268,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2269,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2270,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2271,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__rv[1]),8);
        bufp->chgSData(oldp+2272,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__tagArray__rv[0]),12);
        bufp->chgSData(oldp+2273,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__tagArray__rv[1]),12);
        bufp->chgBit(oldp+2274,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__dirtyArray__rv[0]));
        bufp->chgBit(oldp+2275,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__dirtyArray__rv[1]));
        bufp->chgCData(oldp+2276,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2277,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2278,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2279,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2280,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2281,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2282,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2283,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2284,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2285,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2286,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2287,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2288,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2289,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__rv[1]),8);
        bufp->chgCData(oldp+2290,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__rv[0]),8);
        bufp->chgCData(oldp+2291,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__rv[1]),8);
        bufp->chgSData(oldp+2292,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__tagArray__rv[0]),12);
        bufp->chgSData(oldp+2293,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__tagArray__rv[1]),12);
        bufp->chgBit(oldp+2294,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk2__BRA__0__KET____DOT__replArray__rv[0]));
        bufp->chgBit(oldp+2295,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellout__genblk2__BRA__0__KET____DOT__replArray__rv[1]));
        bufp->chgIData(oldp+2296,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__unnamedblk1__DOT__p),32);
        bufp->chgCData(oldp+2297,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portInRegTagStg[0]),2);
        bufp->chgCData(oldp+2298,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portInRegTagStg[1]),2);
        bufp->chgBit(oldp+2299,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portInRegGrantTagStg[0]));
        bufp->chgBit(oldp+2300,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portInRegGrantTagStg[1]));
        bufp->chgBit(oldp+2301,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegTagStg[0]));
        bufp->chgBit(oldp+2302,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegTagStg[1]));
        bufp->chgBit(oldp+2303,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegTagStg[2]));
        bufp->chgBit(oldp+2304,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegTagStg[3]));
        bufp->chgBit(oldp+2305,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegDataStg[0]));
        bufp->chgBit(oldp+2306,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegDataStg[1]));
        bufp->chgBit(oldp+2307,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegDataStg[2]));
        bufp->chgBit(oldp+2308,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portOutRegDataStg[3]));
        bufp->chgCData(oldp+2309,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                             [0U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                               [0U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+2310,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [0U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+2311,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                             [0U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+2312,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [0U][2U] >> 0xeU))));
        bufp->chgBit(oldp+2313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [0U][2U] >> 0xdU))));
        bufp->chgBit(oldp+2314,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [0U][2U] >> 0xcU))));
        bufp->chgQData(oldp+2315,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                                    [0U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                                                  [0U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                                                    [0U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+2317,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                            [0U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+2318,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+2319,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2320,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2321,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                 [0U][0U])));
        bufp->chgCData(oldp+2322,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                             [1U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                               [1U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+2323,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [1U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+2324,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                             [1U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+2325,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [1U][2U] >> 0xeU))));
        bufp->chgBit(oldp+2326,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [1U][2U] >> 0xdU))));
        bufp->chgBit(oldp+2327,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [1U][2U] >> 0xcU))));
        bufp->chgQData(oldp+2328,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                                    [1U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                                                  [1U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                                                    [1U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+2330,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                            [1U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+2331,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [1U][0U] >> 3U))));
        bufp->chgBit(oldp+2332,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+2333,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+2334,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxInReg
                                 [1U][0U])));
        bufp->chgIData(oldp+2335,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+2336,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk2__DOT__i),32);
        bufp->chgCData(oldp+2337,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__regPhase),2);
        bufp->chgBit(oldp+2338,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__dcFlushComplete));
        bufp->chgIData(oldp+2339,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+2340,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2341,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__cycles[0]),32);
        bufp->chgIData(oldp+2342,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__cycles[1]),32);
        bufp->chgIData(oldp+2343,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2344,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+2345,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2346,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2347,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk4__DOT__i),32);
        bufp->chgSData(oldp+2348,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [0U][6U] 
                                             >> 0xdU))),10);
        bufp->chgCData(oldp+2349,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][6U] >> 0xbU))),2);
        bufp->chgBit(oldp+2350,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][6U] >> 0xaU))));
        bufp->chgCData(oldp+2351,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][6U] >> 7U))),3);
        bufp->chgCData(oldp+2352,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][6U] >> 5U))),2);
        bufp->chgCData(oldp+2353,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][6U] >> 2U))),3);
        bufp->chgBit(oldp+2354,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][6U] >> 1U))));
        bufp->chgCData(oldp+2355,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [0U][6U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [0U][5U] 
                                               >> 0x1cU)))),5);
        bufp->chgBit(oldp+2356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 0x1bU))));
        bufp->chgCData(oldp+2357,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][5U] 
                                            >> 0x16U))),5);
        bufp->chgBit(oldp+2358,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 0x15U))));
        bufp->chgCData(oldp+2359,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][5U] 
                                            >> 0x10U))),5);
        bufp->chgCData(oldp+2360,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [0U][5U] 
                                           >> 0xcU))),4);
        bufp->chgBit(oldp+2361,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 0xbU))));
        bufp->chgIData(oldp+2362,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                   [0U][5U] 
                                                   << 0x13U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                     [0U][4U] 
                                                     >> 0xdU)))),30);
        bufp->chgBit(oldp+2363,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 0xfU))));
        bufp->chgBit(oldp+2364,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 0xeU))));
        bufp->chgBit(oldp+2365,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 0xdU))));
        bufp->chgCData(oldp+2366,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][5U] >> 0xbU))),2);
        bufp->chgCData(oldp+2367,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][5U] 
                                            >> 6U))),5);
        bufp->chgBit(oldp+2368,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 5U))));
        bufp->chgCData(oldp+2369,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][5U] >> 3U))),2);
        bufp->chgSData(oldp+2370,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [0U][5U] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [0U][4U] 
                                                >> 0x19U)))),10);
        bufp->chgSData(oldp+2371,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [0U][4U] 
                                             >> 0xdU))),12);
        bufp->chgSData(oldp+2372,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [0U][5U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+2373,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [0U][5U] 
                                                << 0x13U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                  [0U][4U] 
                                                  >> 0xdU)))),20);
        bufp->chgCData(oldp+2374,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][5U] >> 0xdU))),2);
        bufp->chgSData(oldp+2375,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [0U][5U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                 [0U][4U] 
                                                 >> 0x1bU)))),16);
        bufp->chgSData(oldp+2376,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [0U][4U] 
                                              >> 0xdU))),14);
        bufp->chgSData(oldp+2377,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [0U][5U] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                 [0U][4U] 
                                                 >> 0x1fU)))),15);
        bufp->chgIData(oldp+2378,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [0U][4U] 
                                               >> 0xdU))),18);
        bufp->chgCData(oldp+2379,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][5U] >> 0xdU))),3);
        bufp->chgBit(oldp+2380,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][5U] >> 0xcU))));
        bufp->chgIData(oldp+2381,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [0U][5U] 
                                                << 7U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                  [0U][4U] 
                                                  >> 0x19U)))),19);
        bufp->chgCData(oldp+2382,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][5U] 
                                            >> 0xaU))),5);
        bufp->chgCData(oldp+2383,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][5U] 
                                            >> 5U))),5);
        bufp->chgCData(oldp+2384,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][5U] >> 2U))),3);
        bufp->chgIData(oldp+2385,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                 [0U][5U] 
                                                 << 0x13U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                   [0U][4U] 
                                                   >> 0xdU)))),21);
        bufp->chgCData(oldp+2386,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][4U] >> 0xbU))),2);
        bufp->chgCData(oldp+2387,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][4U] >> 9U))),2);
        bufp->chgCData(oldp+2388,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][4U] >> 7U))),2);
        bufp->chgBit(oldp+2389,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][4U] >> 6U))));
        bufp->chgBit(oldp+2390,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][4U] >> 5U))));
        bufp->chgBit(oldp+2391,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][4U] >> 4U))));
        bufp->chgBit(oldp+2392,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][4U] >> 3U))));
        bufp->chgBit(oldp+2393,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][4U] >> 2U))));
        bufp->chgBit(oldp+2394,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][4U] >> 1U))));
        bufp->chgCData(oldp+2395,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                          [0U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgBit(oldp+2396,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][3U] >> 0x1eU))));
        bufp->chgBit(oldp+2397,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][3U] >> 0x1dU))));
        bufp->chgIData(oldp+2398,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [0U][3U] 
                                               >> 0xaU))),19);
        bufp->chgBit(oldp+2399,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][3U] >> 9U))));
        bufp->chgIData(oldp+2400,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [0U][3U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                  [0U][2U] 
                                                  >> 0x16U)))),19);
        bufp->chgBit(oldp+2401,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][2U] >> 0x15U))));
        bufp->chgSData(oldp+2402,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [0U][2U] 
                                             >> 0xbU))),10);
        bufp->chgCData(oldp+2403,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [0U][2U] >> 9U))),2);
        bufp->chgBit(oldp+2404,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][2U] >> 8U))));
        bufp->chgCData(oldp+2405,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][2U] 
                                            >> 2U))),6);
        bufp->chgBit(oldp+2406,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][2U] >> 1U))));
        bufp->chgCData(oldp+2407,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [0U][2U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [0U][1U] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+2408,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][1U] >> 0x1aU))));
        bufp->chgCData(oldp+2409,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][1U] 
                                            >> 0x14U))),6);
        bufp->chgBit(oldp+2410,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][1U] >> 0x13U))));
        bufp->chgCData(oldp+2411,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][1U] 
                                            >> 0xdU))),6);
        bufp->chgBit(oldp+2412,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [0U][1U] >> 0xcU))));
        bufp->chgCData(oldp+2413,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][1U] 
                                            >> 6U))),6);
        bufp->chgCData(oldp+2414,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [0U][1U] 
                                           >> 2U))),4);
        bufp->chgCData(oldp+2415,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][1U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [0U][0U] 
                                              >> 0x1eU)))),4);
        bufp->chgCData(oldp+2416,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [0U][0U] 
                                           >> 0x1aU))),4);
        bufp->chgCData(oldp+2417,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [0U][0U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2418,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [0U][0U] 
                                            >> 0x10U))),6);
        bufp->chgCData(oldp+2419,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [0U][0U] 
                                           >> 0xcU))),4);
        bufp->chgCData(oldp+2420,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [0U][0U] 
                                           >> 8U))),4);
        bufp->chgCData(oldp+2421,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [0U][0U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2422,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                   [0U][0U])),4);
        bufp->chgSData(oldp+2423,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [1U][6U] 
                                             >> 0xdU))),10);
        bufp->chgCData(oldp+2424,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][6U] >> 0xbU))),2);
        bufp->chgBit(oldp+2425,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][6U] >> 0xaU))));
        bufp->chgCData(oldp+2426,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][6U] >> 7U))),3);
        bufp->chgCData(oldp+2427,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][6U] >> 5U))),2);
        bufp->chgCData(oldp+2428,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][6U] >> 2U))),3);
        bufp->chgBit(oldp+2429,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][6U] >> 1U))));
        bufp->chgCData(oldp+2430,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [1U][6U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [1U][5U] 
                                               >> 0x1cU)))),5);
        bufp->chgBit(oldp+2431,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 0x1bU))));
        bufp->chgCData(oldp+2432,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][5U] 
                                            >> 0x16U))),5);
        bufp->chgBit(oldp+2433,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 0x15U))));
        bufp->chgCData(oldp+2434,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][5U] 
                                            >> 0x10U))),5);
        bufp->chgCData(oldp+2435,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [1U][5U] 
                                           >> 0xcU))),4);
        bufp->chgBit(oldp+2436,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 0xbU))));
        bufp->chgIData(oldp+2437,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                   [1U][5U] 
                                                   << 0x13U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                     [1U][4U] 
                                                     >> 0xdU)))),30);
        bufp->chgBit(oldp+2438,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 0xfU))));
        bufp->chgBit(oldp+2439,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 0xeU))));
        bufp->chgBit(oldp+2440,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 0xdU))));
        bufp->chgCData(oldp+2441,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][5U] >> 0xbU))),2);
        bufp->chgCData(oldp+2442,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][5U] 
                                            >> 6U))),5);
        bufp->chgBit(oldp+2443,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 5U))));
        bufp->chgCData(oldp+2444,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][5U] >> 3U))),2);
        bufp->chgSData(oldp+2445,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [1U][5U] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [1U][4U] 
                                                >> 0x19U)))),10);
        bufp->chgSData(oldp+2446,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [1U][4U] 
                                             >> 0xdU))),12);
        bufp->chgSData(oldp+2447,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [1U][5U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+2448,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [1U][5U] 
                                                << 0x13U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                  [1U][4U] 
                                                  >> 0xdU)))),20);
        bufp->chgCData(oldp+2449,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][5U] >> 0xdU))),2);
        bufp->chgSData(oldp+2450,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [1U][5U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                 [1U][4U] 
                                                 >> 0x1bU)))),16);
        bufp->chgSData(oldp+2451,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [1U][4U] 
                                              >> 0xdU))),14);
        bufp->chgSData(oldp+2452,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [1U][5U] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                 [1U][4U] 
                                                 >> 0x1fU)))),15);
        bufp->chgIData(oldp+2453,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [1U][4U] 
                                               >> 0xdU))),18);
        bufp->chgCData(oldp+2454,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][5U] >> 0xdU))),3);
        bufp->chgBit(oldp+2455,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][5U] >> 0xcU))));
        bufp->chgIData(oldp+2456,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [1U][5U] 
                                                << 7U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                  [1U][4U] 
                                                  >> 0x19U)))),19);
        bufp->chgCData(oldp+2457,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][5U] 
                                            >> 0xaU))),5);
        bufp->chgCData(oldp+2458,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][5U] 
                                            >> 5U))),5);
        bufp->chgCData(oldp+2459,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][5U] >> 2U))),3);
        bufp->chgIData(oldp+2460,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                 [1U][5U] 
                                                 << 0x13U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                   [1U][4U] 
                                                   >> 0xdU)))),21);
        bufp->chgCData(oldp+2461,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][4U] >> 0xbU))),2);
        bufp->chgCData(oldp+2462,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][4U] >> 9U))),2);
        bufp->chgCData(oldp+2463,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][4U] >> 7U))),2);
        bufp->chgBit(oldp+2464,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][4U] >> 6U))));
        bufp->chgBit(oldp+2465,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][4U] >> 5U))));
        bufp->chgBit(oldp+2466,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][4U] >> 4U))));
        bufp->chgBit(oldp+2467,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][4U] >> 3U))));
        bufp->chgBit(oldp+2468,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][4U] >> 2U))));
        bufp->chgBit(oldp+2469,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][4U] >> 1U))));
        bufp->chgCData(oldp+2470,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                          [1U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                          [1U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgBit(oldp+2471,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][3U] >> 0x1eU))));
        bufp->chgBit(oldp+2472,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][3U] >> 0x1dU))));
        bufp->chgIData(oldp+2473,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [1U][3U] 
                                               >> 0xaU))),19);
        bufp->chgBit(oldp+2474,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][3U] >> 9U))));
        bufp->chgIData(oldp+2475,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                [1U][3U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                                  [1U][2U] 
                                                  >> 0x16U)))),19);
        bufp->chgBit(oldp+2476,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][2U] >> 0x15U))));
        bufp->chgSData(oldp+2477,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [1U][2U] 
                                             >> 0xbU))),10);
        bufp->chgCData(oldp+2478,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                         [1U][2U] >> 9U))),2);
        bufp->chgBit(oldp+2479,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][2U] >> 8U))));
        bufp->chgCData(oldp+2480,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][2U] 
                                            >> 2U))),6);
        bufp->chgBit(oldp+2481,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][2U] >> 1U))));
        bufp->chgCData(oldp+2482,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                             [1U][2U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                               [1U][1U] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+2483,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][1U] >> 0x1aU))));
        bufp->chgCData(oldp+2484,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][1U] 
                                            >> 0x14U))),6);
        bufp->chgBit(oldp+2485,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][1U] >> 0x13U))));
        bufp->chgCData(oldp+2486,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][1U] 
                                            >> 0xdU))),6);
        bufp->chgBit(oldp+2487,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                       [1U][1U] >> 0xcU))));
        bufp->chgCData(oldp+2488,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][1U] 
                                            >> 6U))),6);
        bufp->chgCData(oldp+2489,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [1U][1U] 
                                           >> 2U))),4);
        bufp->chgCData(oldp+2490,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][1U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                              [1U][0U] 
                                              >> 0x1eU)))),4);
        bufp->chgCData(oldp+2491,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [1U][0U] 
                                           >> 0x1aU))),4);
        bufp->chgCData(oldp+2492,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [1U][0U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2493,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                            [1U][0U] 
                                            >> 0x10U))),6);
        bufp->chgCData(oldp+2494,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [1U][0U] 
                                           >> 0xcU))),4);
        bufp->chgCData(oldp+2495,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [1U][0U] 
                                           >> 8U))),4);
        bufp->chgCData(oldp+2496,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                           [1U][0U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2497,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__pipeReg
                                   [1U][0U])),4);
        bufp->chgCData(oldp+2498,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][2U] >> 9U))),3);
        bufp->chgCData(oldp+2499,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][2U] >> 7U))),2);
        bufp->chgCData(oldp+2500,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][2U] >> 4U))),3);
        bufp->chgBit(oldp+2501,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][2U] >> 3U))));
        bufp->chgCData(oldp+2502,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                             [0U][2U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                               [0U][1U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+2503,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 0x1dU))));
        bufp->chgCData(oldp+2504,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [0U][1U] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+2505,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 0x17U))));
        bufp->chgCData(oldp+2506,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [0U][1U] 
                                            >> 0x12U))),5);
        bufp->chgCData(oldp+2507,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                           [0U][1U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+2508,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 0xdU))));
        bufp->chgIData(oldp+2509,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                   [0U][1U] 
                                                   << 0x11U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                     [0U][0U] 
                                                     >> 0xfU)))),30);
        bufp->chgBit(oldp+2510,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 0x11U))));
        bufp->chgBit(oldp+2511,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 0x10U))));
        bufp->chgBit(oldp+2512,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 0xfU))));
        bufp->chgCData(oldp+2513,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][1U] >> 0xdU))),2);
        bufp->chgCData(oldp+2514,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [0U][1U] 
                                            >> 8U))),5);
        bufp->chgBit(oldp+2515,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 7U))));
        bufp->chgCData(oldp+2516,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][1U] >> 5U))),2);
        bufp->chgSData(oldp+2517,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [0U][1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [0U][0U] 
                                                >> 0x1bU)))),10);
        bufp->chgSData(oldp+2518,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                             [0U][0U] 
                                             >> 0xfU))),12);
        bufp->chgSData(oldp+2519,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [0U][1U] 
                                              >> 3U))),15);
        bufp->chgIData(oldp+2520,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [0U][1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                  [0U][0U] 
                                                  >> 0xfU)))),20);
        bufp->chgCData(oldp+2521,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][1U] >> 0xfU))),2);
        bufp->chgSData(oldp+2522,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                               [0U][1U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                 [0U][0U] 
                                                 >> 0x1dU)))),16);
        bufp->chgSData(oldp+2523,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [0U][0U] 
                                              >> 0xfU))),14);
        bufp->chgSData(oldp+2524,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [0U][1U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+2525,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [0U][1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                  [0U][0U] 
                                                  >> 0xfU)))),18);
        bufp->chgCData(oldp+2526,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][1U] >> 0xfU))),3);
        bufp->chgBit(oldp+2527,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][1U] >> 0xeU))));
        bufp->chgIData(oldp+2528,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [0U][1U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                  [0U][0U] 
                                                  >> 0x1bU)))),19);
        bufp->chgCData(oldp+2529,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [0U][1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+2530,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [0U][1U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+2531,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][1U] >> 4U))),3);
        bufp->chgIData(oldp+2532,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                 [0U][1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                   [0U][0U] 
                                                   >> 0xfU)))),21);
        bufp->chgCData(oldp+2533,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][0U] >> 0xdU))),2);
        bufp->chgCData(oldp+2534,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][0U] >> 0xbU))),2);
        bufp->chgCData(oldp+2535,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][0U] >> 9U))),2);
        bufp->chgBit(oldp+2536,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][0U] >> 8U))));
        bufp->chgBit(oldp+2537,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][0U] >> 7U))));
        bufp->chgBit(oldp+2538,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][0U] >> 6U))));
        bufp->chgBit(oldp+2539,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][0U] >> 5U))));
        bufp->chgBit(oldp+2540,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][0U] >> 4U))));
        bufp->chgBit(oldp+2541,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [0U][0U] >> 3U))));
        bufp->chgCData(oldp+2542,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [0U][0U] >> 1U))),2);
        bufp->chgBit(oldp+2543,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                 [0U][0U])));
        bufp->chgCData(oldp+2544,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][2U] >> 9U))),3);
        bufp->chgCData(oldp+2545,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][2U] >> 7U))),2);
        bufp->chgCData(oldp+2546,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][2U] >> 4U))),3);
        bufp->chgBit(oldp+2547,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][2U] >> 3U))));
        bufp->chgCData(oldp+2548,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                             [1U][2U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                               [1U][1U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+2549,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 0x1dU))));
        bufp->chgCData(oldp+2550,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [1U][1U] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+2551,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 0x17U))));
        bufp->chgCData(oldp+2552,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [1U][1U] 
                                            >> 0x12U))),5);
        bufp->chgCData(oldp+2553,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                           [1U][1U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+2554,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 0xdU))));
        bufp->chgIData(oldp+2555,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                   [1U][1U] 
                                                   << 0x11U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                     [1U][0U] 
                                                     >> 0xfU)))),30);
        bufp->chgBit(oldp+2556,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 0x11U))));
        bufp->chgBit(oldp+2557,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 0x10U))));
        bufp->chgBit(oldp+2558,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 0xfU))));
        bufp->chgCData(oldp+2559,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][1U] >> 0xdU))),2);
        bufp->chgCData(oldp+2560,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [1U][1U] 
                                            >> 8U))),5);
        bufp->chgBit(oldp+2561,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 7U))));
        bufp->chgCData(oldp+2562,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][1U] >> 5U))),2);
        bufp->chgSData(oldp+2563,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [1U][1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [1U][0U] 
                                                >> 0x1bU)))),10);
        bufp->chgSData(oldp+2564,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                             [1U][0U] 
                                             >> 0xfU))),12);
        bufp->chgSData(oldp+2565,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [1U][1U] 
                                              >> 3U))),15);
        bufp->chgIData(oldp+2566,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [1U][1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                  [1U][0U] 
                                                  >> 0xfU)))),20);
        bufp->chgCData(oldp+2567,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][1U] >> 0xfU))),2);
        bufp->chgSData(oldp+2568,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                               [1U][1U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                 [1U][0U] 
                                                 >> 0x1dU)))),16);
        bufp->chgSData(oldp+2569,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [1U][0U] 
                                              >> 0xfU))),14);
        bufp->chgSData(oldp+2570,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                              [1U][1U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+2571,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [1U][1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                  [1U][0U] 
                                                  >> 0xfU)))),18);
        bufp->chgCData(oldp+2572,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][1U] >> 0xfU))),3);
        bufp->chgBit(oldp+2573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][1U] >> 0xeU))));
        bufp->chgIData(oldp+2574,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                [1U][1U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                  [1U][0U] 
                                                  >> 0x1bU)))),19);
        bufp->chgCData(oldp+2575,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [1U][1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+2576,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                            [1U][1U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+2577,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][1U] >> 4U))),3);
        bufp->chgIData(oldp+2578,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                 [1U][1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                                   [1U][0U] 
                                                   >> 0xfU)))),21);
        bufp->chgCData(oldp+2579,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][0U] >> 0xdU))),2);
        bufp->chgCData(oldp+2580,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][0U] >> 0xbU))),2);
        bufp->chgCData(oldp+2581,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][0U] >> 9U))),2);
        bufp->chgBit(oldp+2582,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][0U] >> 8U))));
        bufp->chgBit(oldp+2583,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][0U] >> 7U))));
        bufp->chgBit(oldp+2584,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][0U] >> 6U))));
        bufp->chgBit(oldp+2585,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][0U] >> 5U))));
        bufp->chgBit(oldp+2586,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][0U] >> 4U))));
        bufp->chgBit(oldp+2587,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                       [1U][0U] >> 3U))));
        bufp->chgCData(oldp+2588,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                         [1U][0U] >> 1U))),2);
        bufp->chgBit(oldp+2589,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opInfo
                                 [1U][0U])));
        bufp->chgSData(oldp+2590,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                             [0U][4U] 
                                             >> 1U))),10);
        bufp->chgCData(oldp+2591,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                          [0U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+2592,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+2593,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+2594,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                           [0U][3U] 
                                           >> 0x17U))),4);
        bufp->chgBit(oldp+2595,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+2596,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                                   [0U][3U] 
                                                   << 8U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                                     [0U][2U] 
                                                     >> 0x18U)))),30);
        bufp->chgIData(oldp+2597,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [0U][2U] 
                                               >> 6U))),18);
        bufp->chgBit(oldp+2598,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+2599,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [0U][3U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+2600,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][3U] >> 6U))));
        bufp->chgSData(oldp+2601,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                              [0U][3U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                                [0U][2U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+2602,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+2603,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [0U][2U] 
                                               >> 6U))),20);
        bufp->chgCData(oldp+2604,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+2605,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                   [0U][2U])),3);
        bufp->chgCData(oldp+2606,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2607,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2608,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2609,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2610,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2611,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2612,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2613,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2614,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2615,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2616,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2617,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2618,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2619,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2620,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                 [0U][0U])));
        bufp->chgSData(oldp+2621,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                             [1U][4U] 
                                             >> 1U))),10);
        bufp->chgCData(oldp+2622,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                          [1U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                          [1U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+2623,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+2624,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+2625,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                           [1U][3U] 
                                           >> 0x17U))),4);
        bufp->chgBit(oldp+2626,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+2627,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                                   [1U][3U] 
                                                   << 8U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                                     [1U][2U] 
                                                     >> 0x18U)))),30);
        bufp->chgIData(oldp+2628,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [1U][2U] 
                                               >> 6U))),18);
        bufp->chgBit(oldp+2629,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+2630,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [1U][3U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+2631,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][3U] >> 6U))));
        bufp->chgSData(oldp+2632,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                              [1U][3U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                                [1U][2U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+2633,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+2634,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [1U][2U] 
                                               >> 6U))),20);
        bufp->chgCData(oldp+2635,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                         [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+2636,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                   [1U][2U])),3);
        bufp->chgCData(oldp+2637,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2638,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2639,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2640,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2641,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2642,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2643,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2644,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+2645,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2646,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2647,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2648,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                            [1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2649,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2650,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2651,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intEntry
                                 [1U][0U])));
        bufp->chgSData(oldp+2652,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                             [0U][2U] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+2653,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                         [0U][2U] >> 6U))),2);
        bufp->chgBit(oldp+2654,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [0U][2U] >> 5U))));
        bufp->chgCData(oldp+2655,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                         [0U][2U] >> 3U))),2);
        bufp->chgCData(oldp+2656,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                   [0U][2U])),3);
        bufp->chgCData(oldp+2657,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2658,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2659,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2660,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2661,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2662,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2663,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2664,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2665,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2666,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2667,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2668,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2669,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2670,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2671,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                 [0U][0U])));
        bufp->chgSData(oldp+2672,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                             [1U][2U] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+2673,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                         [1U][2U] >> 6U))),2);
        bufp->chgBit(oldp+2674,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [1U][2U] >> 5U))));
        bufp->chgCData(oldp+2675,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                         [1U][2U] >> 3U))),2);
        bufp->chgCData(oldp+2676,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                   [1U][2U])),3);
        bufp->chgCData(oldp+2677,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2678,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2679,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2680,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2681,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2682,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2683,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2684,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+2685,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2686,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2687,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2688,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                            [1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2689,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2690,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2691,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__complexEntry
                                 [1U][0U])));
        bufp->chgSData(oldp+2692,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2693,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2694,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2695,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2696,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2697,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2698,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2699,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2700,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2701,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2702,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2703,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2704,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2705,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2706,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2707,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2708,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2709,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2710,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+2711,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                 [0U][2U])));
        bufp->chgCData(oldp+2712,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2713,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2714,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2715,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2716,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2717,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2718,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2719,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2720,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2721,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2722,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2723,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2724,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2725,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2726,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                 [0U][0U])));
        bufp->chgSData(oldp+2727,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                             [1U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2728,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2729,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2730,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2731,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2732,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2733,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                              [1U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                                [1U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2734,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2735,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2736,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2737,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2738,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [1U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2739,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2740,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2741,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2742,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2743,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [1U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2744,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [1U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2745,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][2U] >> 1U))));
        bufp->chgBit(oldp+2746,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                 [1U][2U])));
        bufp->chgCData(oldp+2747,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2748,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2749,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2750,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2751,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2752,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2753,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2754,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+2755,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
    }
}
