// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "VSMT_RTL_Testbench__Syms.h"


void VSMT_RTL_Testbench___024root__trace_chg_0_sub_3(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root__trace_chg_0_sub_3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode + 10400);
    VlWide<4>/*127:0*/ __Vtemp_1;
    VlWide<4>/*127:0*/ __Vtemp_2;
    VlWide<4>/*127:0*/ __Vtemp_3;
    VlWide<4>/*127:0*/ __Vtemp_4;
    // Body
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x20U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x69U])))) {
        bufp->chgBit(oldp+0,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x1eU)))));
        bufp->chgCData(oldp+1,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                 [1U] 
                                                 >> 0x19U)))),5);
        bufp->chgBit(oldp+2,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x18U)))));
        bufp->chgBit(oldp+3,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x17U)))));
        bufp->chgBit(oldp+4,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x16U)))));
        bufp->chgBit(oldp+5,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x15U)))));
        bufp->chgBit(oldp+6,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x14U)))));
        bufp->chgBit(oldp+7,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x13U)))));
        bufp->chgBit(oldp+8,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x12U)))));
        bufp->chgBit(oldp+9,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                            [1U] >> 0x11U)))));
        bufp->chgCData(oldp+10,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                  [1U] 
                                                  >> 0xbU)))),6);
        bufp->chgBit(oldp+11,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                             [1U] >> 0xaU)))));
        bufp->chgCData(oldp+12,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                  [1U] 
                                                  >> 4U)))),6);
        bufp->chgCData(oldp+13,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                [1U]))),4);
        bufp->chgSData(oldp+14,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [0U][6U] 
                                           >> 0xdU))),10);
        bufp->chgCData(oldp+15,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][6U] >> 0xbU))),2);
        bufp->chgBit(oldp+16,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][6U] >> 0xaU))));
        bufp->chgCData(oldp+17,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][6U] >> 7U))),3);
        bufp->chgCData(oldp+18,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][6U] >> 5U))),2);
        bufp->chgCData(oldp+19,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][6U] >> 2U))),3);
        bufp->chgBit(oldp+20,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][6U] >> 1U))));
        bufp->chgCData(oldp+21,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [0U][6U] 
                                           << 4U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [0U][5U] 
                                           >> 0x1cU)))),5);
        bufp->chgBit(oldp+22,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 0x1bU))));
        bufp->chgCData(oldp+23,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][5U] 
                                          >> 0x16U))),5);
        bufp->chgBit(oldp+24,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 0x15U))));
        bufp->chgCData(oldp+25,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][5U] 
                                          >> 0x10U))),5);
        bufp->chgCData(oldp+26,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [0U][5U] >> 0xcU))),4);
        bufp->chgBit(oldp+27,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 0xbU))));
        bufp->chgIData(oldp+28,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                 [0U][5U] 
                                                 << 0x13U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                   [0U][4U] 
                                                   >> 0xdU)))),30);
        bufp->chgBit(oldp+29,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 0xfU))));
        bufp->chgBit(oldp+30,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 0xeU))));
        bufp->chgBit(oldp+31,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 0xdU))));
        bufp->chgCData(oldp+32,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][5U] >> 0xbU))),2);
        bufp->chgCData(oldp+33,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][5U] 
                                          >> 6U))),5);
        bufp->chgBit(oldp+34,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 5U))));
        bufp->chgCData(oldp+35,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][5U] >> 3U))),2);
        bufp->chgSData(oldp+36,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                            [0U][5U] 
                                            << 7U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [0U][4U] 
                                              >> 0x19U)))),10);
        bufp->chgSData(oldp+37,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0xdU))),12);
        bufp->chgSData(oldp+38,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 1U))),15);
        bufp->chgIData(oldp+39,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [0U][5U] 
                                              << 0x13U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                [0U][4U] 
                                                >> 0xdU)))),20);
        bufp->chgCData(oldp+40,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][5U] >> 0xdU))),2);
        bufp->chgSData(oldp+41,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                             [0U][5U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                               [0U][4U] 
                                               >> 0x1bU)))),16);
        bufp->chgSData(oldp+42,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 0xdU))),14);
        bufp->chgSData(oldp+43,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                             [0U][5U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                               [0U][4U] 
                                               >> 0x1fU)))),15);
        bufp->chgIData(oldp+44,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                             [0U][4U] 
                                             >> 0xdU))),18);
        bufp->chgCData(oldp+45,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][5U] >> 0xdU))),3);
        bufp->chgBit(oldp+46,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][5U] >> 0xcU))));
        bufp->chgIData(oldp+47,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [0U][5U] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                [0U][4U] 
                                                >> 0x19U)))),19);
        bufp->chgCData(oldp+48,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][5U] 
                                          >> 0xaU))),5);
        bufp->chgCData(oldp+49,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][5U] 
                                          >> 5U))),5);
        bufp->chgCData(oldp+50,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][5U] >> 2U))),3);
        bufp->chgIData(oldp+51,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                               [0U][5U] 
                                               << 0x13U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                 [0U][4U] 
                                                 >> 0xdU)))),21);
        bufp->chgCData(oldp+52,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][4U] >> 0xbU))),2);
        bufp->chgCData(oldp+53,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][4U] >> 9U))),2);
        bufp->chgCData(oldp+54,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][4U] >> 7U))),2);
        bufp->chgBit(oldp+55,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][4U] >> 6U))));
        bufp->chgBit(oldp+56,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][4U] >> 5U))));
        bufp->chgBit(oldp+57,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][4U] >> 4U))));
        bufp->chgBit(oldp+58,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][4U] >> 3U))));
        bufp->chgBit(oldp+59,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][4U] >> 2U))));
        bufp->chgBit(oldp+60,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][4U] >> 1U))));
        bufp->chgCData(oldp+61,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [0U][4U] << 1U) 
                                       | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgBit(oldp+62,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][3U] >> 0x1eU))));
        bufp->chgBit(oldp+63,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][3U] >> 0x1dU))));
        bufp->chgIData(oldp+64,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                             [0U][3U] 
                                             >> 0xaU))),19);
        bufp->chgBit(oldp+65,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][3U] >> 9U))));
        bufp->chgIData(oldp+66,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [0U][3U] 
                                              << 0xaU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                [0U][2U] 
                                                >> 0x16U)))),19);
        bufp->chgBit(oldp+67,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][2U] >> 0x15U))));
        bufp->chgSData(oldp+68,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 0xbU))),10);
        bufp->chgCData(oldp+69,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [0U][2U] >> 9U))),2);
        bufp->chgBit(oldp+70,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][2U] >> 8U))));
        bufp->chgCData(oldp+71,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][2U] 
                                          >> 2U))),6);
        bufp->chgBit(oldp+72,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][2U] >> 1U))));
        bufp->chgCData(oldp+73,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [0U][2U] 
                                           << 5U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0x1bU)))),6);
        bufp->chgBit(oldp+74,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][1U] >> 0x1aU))));
        bufp->chgCData(oldp+75,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x14U))),6);
        bufp->chgBit(oldp+76,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][1U] >> 0x13U))));
        bufp->chgCData(oldp+77,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0xdU))),6);
        bufp->chgBit(oldp+78,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [0U][1U] >> 0xcU))));
        bufp->chgCData(oldp+79,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 6U))),6);
        bufp->chgCData(oldp+80,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [0U][1U] >> 2U))),4);
        bufp->chgCData(oldp+81,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][1U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][0U] 
                                          >> 0x1eU)))),4);
        bufp->chgCData(oldp+82,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [0U][0U] >> 0x1aU))),4);
        bufp->chgCData(oldp+83,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [0U][0U] >> 0x16U))),4);
        bufp->chgCData(oldp+84,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [0U][0U] 
                                          >> 0x10U))),6);
        bufp->chgCData(oldp+85,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [0U][0U] >> 0xcU))),4);
        bufp->chgCData(oldp+86,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [0U][0U] >> 8U))),4);
        bufp->chgCData(oldp+87,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [0U][0U] >> 4U))),4);
        bufp->chgCData(oldp+88,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                 [0U][0U])),4);
        bufp->chgSData(oldp+89,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][6U] 
                                           >> 0xdU))),10);
        bufp->chgCData(oldp+90,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [1U][6U] >> 0xbU))),2);
        bufp->chgBit(oldp+91,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [1U][6U] >> 0xaU))));
        bufp->chgCData(oldp+92,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [1U][6U] >> 7U))),3);
        bufp->chgCData(oldp+93,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [1U][6U] >> 5U))),2);
        bufp->chgCData(oldp+94,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                       [1U][6U] >> 2U))),3);
        bufp->chgBit(oldp+95,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [1U][6U] >> 1U))));
        bufp->chgCData(oldp+96,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][6U] 
                                           << 4U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][5U] 
                                           >> 0x1cU)))),5);
        bufp->chgBit(oldp+97,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [1U][5U] >> 0x1bU))));
        bufp->chgCData(oldp+98,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][5U] 
                                          >> 0x16U))),5);
        bufp->chgBit(oldp+99,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                     [1U][5U] >> 0x15U))));
        bufp->chgCData(oldp+100,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][5U] 
                                           >> 0x10U))),5);
        bufp->chgCData(oldp+101,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][5U] 
                                          >> 0xcU))),4);
        bufp->chgBit(oldp+102,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][5U] >> 0xbU))));
        bufp->chgIData(oldp+103,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                  [1U][5U] 
                                                  << 0x13U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                    [1U][4U] 
                                                    >> 0xdU)))),30);
        bufp->chgBit(oldp+104,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][5U] >> 0xfU))));
        bufp->chgBit(oldp+105,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][5U] >> 0xeU))));
        bufp->chgBit(oldp+106,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][5U] >> 0xdU))));
        bufp->chgCData(oldp+107,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][5U] >> 0xbU))),2);
        bufp->chgCData(oldp+108,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][5U] 
                                           >> 6U))),5);
        bufp->chgBit(oldp+109,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][5U] >> 5U))));
        bufp->chgCData(oldp+110,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][5U] >> 3U))),2);
        bufp->chgSData(oldp+111,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                             [1U][5U] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                               [1U][4U] 
                                               >> 0x19U)))),10);
        bufp->chgSData(oldp+112,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 0xdU))),12);
        bufp->chgSData(oldp+113,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                             [1U][5U] 
                                             >> 1U))),15);
        bufp->chgIData(oldp+114,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                               [1U][5U] 
                                               << 0x13U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                 [1U][4U] 
                                                 >> 0xdU)))),20);
        bufp->chgCData(oldp+115,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][5U] >> 0xdU))),2);
        bufp->chgSData(oldp+116,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [1U][5U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                [1U][4U] 
                                                >> 0x1bU)))),16);
        bufp->chgSData(oldp+117,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                             [1U][4U] 
                                             >> 0xdU))),14);
        bufp->chgSData(oldp+118,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [1U][5U] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                [1U][4U] 
                                                >> 0x1fU)))),15);
        bufp->chgIData(oldp+119,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [1U][4U] 
                                              >> 0xdU))),18);
        bufp->chgCData(oldp+120,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][5U] >> 0xdU))),3);
        bufp->chgBit(oldp+121,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][5U] >> 0xcU))));
        bufp->chgIData(oldp+122,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                               [1U][5U] 
                                               << 7U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                 [1U][4U] 
                                                 >> 0x19U)))),19);
        bufp->chgCData(oldp+123,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][5U] 
                                           >> 0xaU))),5);
        bufp->chgCData(oldp+124,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][5U] 
                                           >> 5U))),5);
        bufp->chgCData(oldp+125,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][5U] >> 2U))),3);
        bufp->chgIData(oldp+126,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                [1U][5U] 
                                                << 0x13U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                  [1U][4U] 
                                                  >> 0xdU)))),21);
        bufp->chgCData(oldp+127,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][4U] >> 0xbU))),2);
        bufp->chgCData(oldp+128,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][4U] >> 9U))),2);
        bufp->chgCData(oldp+129,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][4U] >> 7U))),2);
        bufp->chgBit(oldp+130,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][4U] >> 6U))));
        bufp->chgBit(oldp+131,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][4U] >> 5U))));
        bufp->chgBit(oldp+132,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][4U] >> 4U))));
        bufp->chgBit(oldp+133,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][4U] >> 3U))));
        bufp->chgBit(oldp+134,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][4U] >> 2U))));
        bufp->chgBit(oldp+135,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][4U] >> 1U))));
        bufp->chgCData(oldp+136,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                         [1U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgBit(oldp+137,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][3U] >> 0x1eU))));
        bufp->chgBit(oldp+138,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][3U] >> 0x1dU))));
        bufp->chgIData(oldp+139,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [1U][3U] 
                                              >> 0xaU))),19);
        bufp->chgBit(oldp+140,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][3U] >> 9U))));
        bufp->chgIData(oldp+141,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                               [1U][3U] 
                                               << 0xaU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                                 [1U][2U] 
                                                 >> 0x16U)))),19);
        bufp->chgBit(oldp+142,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][2U] >> 0x15U))));
        bufp->chgSData(oldp+143,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                            [1U][2U] 
                                            >> 0xbU))),10);
        bufp->chgCData(oldp+144,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                        [1U][2U] >> 9U))),2);
        bufp->chgBit(oldp+145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][2U] >> 8U))));
        bufp->chgCData(oldp+146,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][2U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+147,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][2U] >> 1U))));
        bufp->chgCData(oldp+148,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                            [1U][2U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                              [1U][1U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+149,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][1U] >> 0x1aU))));
        bufp->chgCData(oldp+150,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+151,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][1U] >> 0x13U))));
        bufp->chgCData(oldp+152,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 0xdU))),6);
        bufp->chgBit(oldp+153,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                      [1U][1U] >> 0xcU))));
        bufp->chgCData(oldp+154,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 6U))),6);
        bufp->chgCData(oldp+155,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][1U] 
                                          >> 2U))),4);
        bufp->chgCData(oldp+156,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][1U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][0U] 
                                           >> 0x1eU)))),4);
        bufp->chgCData(oldp+157,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][0U] 
                                          >> 0x1aU))),4);
        bufp->chgCData(oldp+158,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][0U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+159,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                           [1U][0U] 
                                           >> 0x10U))),6);
        bufp->chgCData(oldp+160,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][0U] 
                                          >> 0xcU))),4);
        bufp->chgCData(oldp+161,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][0U] 
                                          >> 8U))),4);
        bufp->chgCData(oldp+162,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                          [1U][0U] 
                                          >> 4U))),4);
        bufp->chgCData(oldp+163,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__nextStage
                                  [1U][0U])),4);
        bufp->chgBit(oldp+164,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isLoad[0]));
        bufp->chgBit(oldp+165,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isLoad[1]));
        bufp->chgBit(oldp+166,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isStore[0]));
        bufp->chgBit(oldp+167,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isStore[1]));
        bufp->chgBit(oldp+168,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isBranch[0]));
        bufp->chgBit(oldp+169,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isBranch[1]));
        bufp->chgCData(oldp+170,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__pushCount),2);
        bufp->chgBit(oldp+171,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__push));
        bufp->chgCData(oldp+172,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__nextTail),4);
        bufp->chgCData(oldp+173,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__roundedSetTailPtr),4);
        bufp->chgCData(oldp+174,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__nextCount),5);
        bufp->chgBit(oldp+175,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatePhyReg[0]));
        bufp->chgBit(oldp+176,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatePhyReg[1]));
        bufp->chgBit(oldp+177,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatePhyScalarReg[0]));
        bufp->chgBit(oldp+178,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatePhyScalarReg[1]));
        bufp->chgBit(oldp+179,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatePhyScalarFPReg[0]));
        bufp->chgBit(oldp+180,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatePhyScalarFPReg[1]));
        bufp->chgIData(oldp+181,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+182,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__unnamedblk7__DOT__i),32);
        bufp->chgCData(oldp+183,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailPtr[0]),6);
        bufp->chgCData(oldp+184,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailPtr[1]),6);
        bufp->chgCData(oldp+185,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushNum),2);
        bufp->chgBit(oldp+186,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushTail[0]));
        bufp->chgBit(oldp+187,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushTail[1]));
        bufp->chgSData(oldp+188,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                    [0U] 
                                                    >> 0x35U)))),10);
        bufp->chgCData(oldp+189,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                [0U] 
                                                >> 0x33U)))),2);
        bufp->chgBit(oldp+190,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x32U)))));
        bufp->chgIData(oldp+191,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                      [0U] 
                                                      >> 0x1fU)))),19);
        bufp->chgBit(oldp+192,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x1eU)))));
        bufp->chgCData(oldp+193,((0x1fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                   [0U] 
                                                   >> 0x19U)))),5);
        bufp->chgBit(oldp+194,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x18U)))));
        bufp->chgBit(oldp+195,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x17U)))));
        bufp->chgBit(oldp+196,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x16U)))));
        bufp->chgBit(oldp+197,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x15U)))));
        bufp->chgBit(oldp+198,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+199,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x13U)))));
        bufp->chgBit(oldp+200,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x12U)))));
        bufp->chgBit(oldp+201,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x11U)))));
        bufp->chgCData(oldp+202,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                   [0U] 
                                                   >> 0xbU)))),6);
        bufp->chgBit(oldp+203,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [0U] 
                                              >> 0xaU)))));
        bufp->chgCData(oldp+204,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                   [0U] 
                                                   >> 4U)))),6);
        bufp->chgCData(oldp+205,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                 [0U]))),4);
        bufp->chgSData(oldp+206,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                    [1U] 
                                                    >> 0x35U)))),10);
        bufp->chgCData(oldp+207,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                [1U] 
                                                >> 0x33U)))),2);
        bufp->chgBit(oldp+208,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x32U)))));
        bufp->chgIData(oldp+209,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                      [1U] 
                                                      >> 0x1fU)))),19);
        bufp->chgBit(oldp+210,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x1eU)))));
        bufp->chgCData(oldp+211,((0x1fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                   [1U] 
                                                   >> 0x19U)))),5);
        bufp->chgBit(oldp+212,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x18U)))));
        bufp->chgBit(oldp+213,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x17U)))));
        bufp->chgBit(oldp+214,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x16U)))));
        bufp->chgBit(oldp+215,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x15U)))));
        bufp->chgBit(oldp+216,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+217,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x13U)))));
        bufp->chgBit(oldp+218,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x12U)))));
        bufp->chgBit(oldp+219,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x11U)))));
        bufp->chgCData(oldp+220,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                   [1U] 
                                                   >> 0xbU)))),6);
        bufp->chgBit(oldp+221,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                              [1U] 
                                              >> 0xaU)))));
        bufp->chgCData(oldp+222,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                   [1U] 
                                                   >> 4U)))),6);
        bufp->chgCData(oldp+223,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushedTailData
                                                 [1U]))),4);
        bufp->chgBit(oldp+224,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[0]));
        bufp->chgBit(oldp+225,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[1]));
        bufp->chgBit(oldp+226,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[2]));
        bufp->chgBit(oldp+227,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[3]));
        bufp->chgBit(oldp+228,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[4]));
        bufp->chgBit(oldp+229,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[5]));
        bufp->chgBit(oldp+230,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[6]));
        bufp->chgBit(oldp+231,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWE[7]));
        bufp->chgCData(oldp+232,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[0]),6);
        bufp->chgCData(oldp+233,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[1]),6);
        bufp->chgCData(oldp+234,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[2]),6);
        bufp->chgCData(oldp+235,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[3]),6);
        bufp->chgCData(oldp+236,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[4]),6);
        bufp->chgCData(oldp+237,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[5]),6);
        bufp->chgCData(oldp+238,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[6]),6);
        bufp->chgCData(oldp+239,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__esWA[7]),6);
        bufp->chgBit(oldp+240,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__ffsWE[0]));
        bufp->chgBit(oldp+241,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__ffsWE[1]));
        bufp->chgBit(oldp+242,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__ffsWE[2]));
        bufp->chgCData(oldp+243,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__ffsWA[0]),6);
        bufp->chgCData(oldp+244,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__ffsWA[1]),6);
        bufp->chgCData(oldp+245,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__ffsWA[2]),6);
        bufp->chgBit(oldp+246,((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__pushNum))));
        bufp->chgBit(oldp+247,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [0U] >> 5U))));
        bufp->chgCData(oldp+248,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [0U])),5);
        bufp->chgBit(oldp+249,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [1U] >> 5U))));
        bufp->chgCData(oldp+250,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [1U])),5);
        bufp->chgBit(oldp+251,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [2U] >> 5U))));
        bufp->chgCData(oldp+252,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [2U])),5);
        bufp->chgBit(oldp+253,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [3U] >> 5U))));
        bufp->chgCData(oldp+254,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [3U])),5);
        bufp->chgBit(oldp+255,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [4U] >> 5U))));
        bufp->chgCData(oldp+256,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [4U])),5);
        bufp->chgBit(oldp+257,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [5U] >> 5U))));
        bufp->chgCData(oldp+258,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [5U])),5);
        bufp->chgBit(oldp+259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [6U] >> 5U))));
        bufp->chgCData(oldp+260,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [6U])),5);
        bufp->chgBit(oldp+261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                      [7U] >> 5U))));
        bufp->chgCData(oldp+262,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRA
                                  [7U])),5);
        bufp->chgCData(oldp+263,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [0U] >> 4U))),6);
        bufp->chgCData(oldp+264,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [0U])),4);
        bufp->chgCData(oldp+265,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [1U] >> 4U))),6);
        bufp->chgCData(oldp+266,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [1U])),4);
        bufp->chgCData(oldp+267,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [2U] >> 4U))),6);
        bufp->chgCData(oldp+268,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [2U])),4);
        bufp->chgCData(oldp+269,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [3U] >> 4U))),6);
        bufp->chgCData(oldp+270,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [3U])),4);
        bufp->chgCData(oldp+271,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [4U] >> 4U))),6);
        bufp->chgCData(oldp+272,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [4U])),4);
        bufp->chgCData(oldp+273,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [5U] >> 4U))),6);
        bufp->chgCData(oldp+274,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [5U])),4);
        bufp->chgCData(oldp+275,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [6U] >> 4U))),6);
        bufp->chgCData(oldp+276,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [6U])),4);
        bufp->chgCData(oldp+277,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                           [7U] >> 4U))),6);
        bufp->chgCData(oldp+278,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtRV
                                  [7U])),4);
        bufp->chgSData(oldp+279,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            >> 0xdU))),10);
        bufp->chgCData(oldp+280,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][6U] >> 0xbU))),2);
        bufp->chgBit(oldp+281,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][6U] >> 0xaU))));
        bufp->chgCData(oldp+282,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][6U] >> 7U))),3);
        bufp->chgCData(oldp+283,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][6U] >> 5U))),2);
        bufp->chgCData(oldp+284,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][6U] >> 2U))),3);
        bufp->chgBit(oldp+285,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][6U] >> 1U))));
        bufp->chgCData(oldp+286,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [0U][5U] 
                                              >> 0x1cU)))),5);
        bufp->chgBit(oldp+287,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 0x1bU))));
        bufp->chgCData(oldp+288,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 0x16U))),5);
        bufp->chgBit(oldp+289,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 0x15U))));
        bufp->chgCData(oldp+290,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 0x10U))),5);
        bufp->chgCData(oldp+291,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [0U][5U] 
                                          >> 0xcU))),4);
        bufp->chgBit(oldp+292,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 0xbU))));
        bufp->chgIData(oldp+293,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                  [0U][5U] 
                                                  << 0x13U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                    [0U][4U] 
                                                    >> 0xdU)))),30);
        bufp->chgBit(oldp+294,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 0xfU))));
        bufp->chgBit(oldp+295,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 0xeU))));
        bufp->chgBit(oldp+296,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 0xdU))));
        bufp->chgCData(oldp+297,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][5U] >> 0xbU))),2);
        bufp->chgCData(oldp+298,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 6U))),5);
        bufp->chgBit(oldp+299,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 5U))));
        bufp->chgCData(oldp+300,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][5U] >> 3U))),2);
        bufp->chgSData(oldp+301,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                             [0U][5U] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [0U][4U] 
                                               >> 0x19U)))),10);
        bufp->chgSData(oldp+302,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 0xdU))),12);
        bufp->chgSData(oldp+303,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                             [0U][5U] 
                                             >> 1U))),15);
        bufp->chgIData(oldp+304,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [0U][5U] 
                                               << 0x13U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                 [0U][4U] 
                                                 >> 0xdU)))),20);
        bufp->chgCData(oldp+305,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][5U] >> 0xdU))),2);
        bufp->chgSData(oldp+306,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [0U][5U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                [0U][4U] 
                                                >> 0x1bU)))),16);
        bufp->chgSData(oldp+307,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                             [0U][4U] 
                                             >> 0xdU))),14);
        bufp->chgSData(oldp+308,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [0U][5U] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                [0U][4U] 
                                                >> 0x1fU)))),15);
        bufp->chgIData(oldp+309,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [0U][4U] 
                                              >> 0xdU))),18);
        bufp->chgCData(oldp+310,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][5U] >> 0xdU))),3);
        bufp->chgBit(oldp+311,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][5U] >> 0xcU))));
        bufp->chgIData(oldp+312,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [0U][5U] 
                                               << 7U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                 [0U][4U] 
                                                 >> 0x19U)))),19);
        bufp->chgCData(oldp+313,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 0xaU))),5);
        bufp->chgCData(oldp+314,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 5U))),5);
        bufp->chgCData(oldp+315,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][5U] >> 2U))),3);
        bufp->chgIData(oldp+316,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                [0U][5U] 
                                                << 0x13U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                  [0U][4U] 
                                                  >> 0xdU)))),21);
        bufp->chgCData(oldp+317,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][4U] >> 0xbU))),2);
        bufp->chgCData(oldp+318,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][4U] >> 9U))),2);
        bufp->chgCData(oldp+319,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][4U] >> 7U))),2);
        bufp->chgBit(oldp+320,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][4U] >> 6U))));
        bufp->chgBit(oldp+321,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][4U] >> 5U))));
        bufp->chgBit(oldp+322,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][4U] >> 4U))));
        bufp->chgBit(oldp+323,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][4U] >> 3U))));
        bufp->chgBit(oldp+324,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][4U] >> 2U))));
        bufp->chgBit(oldp+325,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][4U] >> 1U))));
        bufp->chgCData(oldp+326,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                         [0U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgBit(oldp+327,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x1eU))));
        bufp->chgBit(oldp+328,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x1dU))));
        bufp->chgIData(oldp+329,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [0U][3U] 
                                              >> 0xaU))),19);
        bufp->chgBit(oldp+330,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][3U] >> 9U))));
        bufp->chgIData(oldp+331,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               << 0xaU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                 [0U][2U] 
                                                 >> 0x16U)))),19);
        bufp->chgBit(oldp+332,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x15U))));
        bufp->chgSData(oldp+333,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            >> 0xbU))),10);
        bufp->chgCData(oldp+334,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [0U][2U] >> 9U))),2);
        bufp->chgBit(oldp+335,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][2U] >> 8U))));
        bufp->chgCData(oldp+336,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][2U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+337,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][2U] >> 1U))));
        bufp->chgCData(oldp+338,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+339,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x1aU))));
        bufp->chgCData(oldp+340,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+341,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x13U))));
        bufp->chgCData(oldp+342,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0xdU))),6);
        bufp->chgBit(oldp+343,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [0U][1U] >> 0xcU))));
        bufp->chgCData(oldp+344,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 6U))),6);
        bufp->chgCData(oldp+345,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [0U][1U] 
                                          >> 2U))),4);
        bufp->chgCData(oldp+346,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][0U] 
                                           >> 0x1eU)))),4);
        bufp->chgCData(oldp+347,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [0U][0U] 
                                          >> 0x1aU))),4);
        bufp->chgCData(oldp+348,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [0U][0U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+349,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [0U][0U] 
                                           >> 0x10U))),6);
        bufp->chgCData(oldp+350,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [0U][0U] 
                                          >> 0xcU))),4);
        bufp->chgCData(oldp+351,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [0U][0U] 
                                          >> 8U))),4);
        bufp->chgCData(oldp+352,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [0U][0U] 
                                          >> 4U))),4);
        bufp->chgCData(oldp+353,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                  [0U][0U])),4);
        bufp->chgSData(oldp+354,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [1U][6U] 
                                            >> 0xdU))),10);
        bufp->chgCData(oldp+355,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][6U] >> 0xbU))),2);
        bufp->chgBit(oldp+356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][6U] >> 0xaU))));
        bufp->chgCData(oldp+357,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][6U] >> 7U))),3);
        bufp->chgCData(oldp+358,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][6U] >> 5U))),2);
        bufp->chgCData(oldp+359,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][6U] >> 2U))),3);
        bufp->chgBit(oldp+360,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][6U] >> 1U))));
        bufp->chgCData(oldp+361,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [1U][6U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [1U][5U] 
                                              >> 0x1cU)))),5);
        bufp->chgBit(oldp+362,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 0x1bU))));
        bufp->chgCData(oldp+363,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][5U] 
                                           >> 0x16U))),5);
        bufp->chgBit(oldp+364,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 0x15U))));
        bufp->chgCData(oldp+365,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][5U] 
                                           >> 0x10U))),5);
        bufp->chgCData(oldp+366,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [1U][5U] 
                                          >> 0xcU))),4);
        bufp->chgBit(oldp+367,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 0xbU))));
        bufp->chgIData(oldp+368,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                  [1U][5U] 
                                                  << 0x13U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                    [1U][4U] 
                                                    >> 0xdU)))),30);
        bufp->chgBit(oldp+369,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 0xfU))));
        bufp->chgBit(oldp+370,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 0xeU))));
        bufp->chgBit(oldp+371,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 0xdU))));
        bufp->chgCData(oldp+372,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][5U] >> 0xbU))),2);
        bufp->chgCData(oldp+373,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][5U] 
                                           >> 6U))),5);
        bufp->chgBit(oldp+374,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 5U))));
        bufp->chgCData(oldp+375,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][5U] >> 3U))),2);
        bufp->chgSData(oldp+376,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                             [1U][5U] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [1U][4U] 
                                               >> 0x19U)))),10);
        bufp->chgSData(oldp+377,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 0xdU))),12);
        bufp->chgSData(oldp+378,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                             [1U][5U] 
                                             >> 1U))),15);
        bufp->chgIData(oldp+379,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [1U][5U] 
                                               << 0x13U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                 [1U][4U] 
                                                 >> 0xdU)))),20);
        bufp->chgCData(oldp+380,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][5U] >> 0xdU))),2);
        bufp->chgSData(oldp+381,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [1U][5U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                [1U][4U] 
                                                >> 0x1bU)))),16);
        bufp->chgSData(oldp+382,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                             [1U][4U] 
                                             >> 0xdU))),14);
        bufp->chgSData(oldp+383,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [1U][5U] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                [1U][4U] 
                                                >> 0x1fU)))),15);
        bufp->chgIData(oldp+384,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [1U][4U] 
                                              >> 0xdU))),18);
        bufp->chgCData(oldp+385,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][5U] >> 0xdU))),3);
        bufp->chgBit(oldp+386,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][5U] >> 0xcU))));
        bufp->chgIData(oldp+387,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [1U][5U] 
                                               << 7U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                 [1U][4U] 
                                                 >> 0x19U)))),19);
        bufp->chgCData(oldp+388,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][5U] 
                                           >> 0xaU))),5);
        bufp->chgCData(oldp+389,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][5U] 
                                           >> 5U))),5);
        bufp->chgCData(oldp+390,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][5U] >> 2U))),3);
        bufp->chgIData(oldp+391,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                [1U][5U] 
                                                << 0x13U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                  [1U][4U] 
                                                  >> 0xdU)))),21);
        bufp->chgCData(oldp+392,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][4U] >> 0xbU))),2);
        bufp->chgCData(oldp+393,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][4U] >> 9U))),2);
        bufp->chgCData(oldp+394,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][4U] >> 7U))),2);
        bufp->chgBit(oldp+395,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][4U] >> 6U))));
        bufp->chgBit(oldp+396,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][4U] >> 5U))));
        bufp->chgBit(oldp+397,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][4U] >> 4U))));
        bufp->chgBit(oldp+398,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][4U] >> 3U))));
        bufp->chgBit(oldp+399,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][4U] >> 2U))));
        bufp->chgBit(oldp+400,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][4U] >> 1U))));
        bufp->chgCData(oldp+401,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                         [1U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgBit(oldp+402,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x1eU))));
        bufp->chgBit(oldp+403,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x1dU))));
        bufp->chgIData(oldp+404,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [1U][3U] 
                                              >> 0xaU))),19);
        bufp->chgBit(oldp+405,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][3U] >> 9U))));
        bufp->chgIData(oldp+406,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               << 0xaU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                                 [1U][2U] 
                                                 >> 0x16U)))),19);
        bufp->chgBit(oldp+407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x15U))));
        bufp->chgSData(oldp+408,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [1U][2U] 
                                            >> 0xbU))),10);
        bufp->chgCData(oldp+409,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                        [1U][2U] >> 9U))),2);
        bufp->chgBit(oldp+410,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][2U] >> 8U))));
        bufp->chgCData(oldp+411,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][2U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+412,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][2U] >> 1U))));
        bufp->chgCData(oldp+413,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                            [1U][2U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                              [1U][1U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+414,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x1aU))));
        bufp->chgCData(oldp+415,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+416,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x13U))));
        bufp->chgCData(oldp+417,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 0xdU))),6);
        bufp->chgBit(oldp+418,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                      [1U][1U] >> 0xcU))));
        bufp->chgCData(oldp+419,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 6U))),6);
        bufp->chgCData(oldp+420,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [1U][1U] 
                                          >> 2U))),4);
        bufp->chgCData(oldp+421,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][0U] 
                                           >> 0x1eU)))),4);
        bufp->chgCData(oldp+422,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [1U][0U] 
                                          >> 0x1aU))),4);
        bufp->chgCData(oldp+423,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [1U][0U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+424,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                           [1U][0U] 
                                           >> 0x10U))),6);
        bufp->chgCData(oldp+425,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [1U][0U] 
                                          >> 0xcU))),4);
        bufp->chgCData(oldp+426,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [1U][0U] 
                                          >> 8U))),4);
        bufp->chgCData(oldp+427,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                          [1U][0U] 
                                          >> 4U))),4);
        bufp->chgCData(oldp+428,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__nextStage
                                  [1U][0U])),4);
        bufp->chgBit(oldp+429,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateLoadQueue[0]));
        bufp->chgBit(oldp+430,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateLoadQueue[1]));
        bufp->chgBit(oldp+431,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue[0]));
        bufp->chgBit(oldp+432,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocateStoreQueue[1]));
        bufp->chgCData(oldp+433,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr[0]),4);
        bufp->chgCData(oldp+434,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedLoadQueuePtr[1]),4);
        bufp->chgCData(oldp+435,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr[0]),4);
        bufp->chgCData(oldp+436,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatedStoreQueuePtr[1]),4);
        bufp->chgBit(oldp+437,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
                                      [0U] >> 5U))));
        bufp->chgCData(oldp+438,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
                                  [0U])),5);
        bufp->chgBit(oldp+439,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
                                      [1U] >> 5U))));
        bufp->chgCData(oldp+440,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegA
                                  [1U])),5);
        bufp->chgBit(oldp+441,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
                                      [0U] >> 5U))));
        bufp->chgCData(oldp+442,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
                                  [0U])),5);
        bufp->chgBit(oldp+443,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
                                      [1U] >> 5U))));
        bufp->chgCData(oldp+444,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegB
                                  [1U])),5);
        bufp->chgBit(oldp+445,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                      [0U] >> 5U))));
        bufp->chgCData(oldp+446,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                  [0U])),5);
        bufp->chgBit(oldp+447,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                      [1U] >> 5U))));
        bufp->chgCData(oldp+448,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logDstReg
                                  [1U])),5);
        bufp->chgCData(oldp+449,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__updateRMT),2);
        bufp->chgCData(oldp+450,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteRegFromPipeReg),2);
        bufp->chgBit(oldp+451,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail[0]));
        bufp->chgBit(oldp+452,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushTail[1]));
        bufp->chgSData(oldp+453,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                    [0U] 
                                                    >> 0x35U)))),10);
        bufp->chgCData(oldp+454,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                [0U] 
                                                >> 0x33U)))),2);
        bufp->chgBit(oldp+455,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x32U)))));
        bufp->chgIData(oldp+456,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                      [0U] 
                                                      >> 0x1fU)))),19);
        bufp->chgBit(oldp+457,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x1eU)))));
        bufp->chgCData(oldp+458,((0x1fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                   [0U] 
                                                   >> 0x19U)))),5);
        bufp->chgBit(oldp+459,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x18U)))));
        bufp->chgBit(oldp+460,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x17U)))));
        bufp->chgBit(oldp+461,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x16U)))));
        bufp->chgBit(oldp+462,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x15U)))));
        bufp->chgBit(oldp+463,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+464,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x13U)))));
        bufp->chgBit(oldp+465,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x12U)))));
        bufp->chgBit(oldp+466,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0x11U)))));
        bufp->chgCData(oldp+467,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                   [0U] 
                                                   >> 0xbU)))),6);
        bufp->chgBit(oldp+468,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [0U] 
                                              >> 0xaU)))));
        bufp->chgCData(oldp+469,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                   [0U] 
                                                   >> 4U)))),6);
        bufp->chgCData(oldp+470,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                 [0U]))),4);
        bufp->chgSData(oldp+471,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                    [1U] 
                                                    >> 0x35U)))),10);
        bufp->chgCData(oldp+472,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                [1U] 
                                                >> 0x33U)))),2);
        bufp->chgBit(oldp+473,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x32U)))));
        bufp->chgIData(oldp+474,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                      [1U] 
                                                      >> 0x1fU)))),19);
        bufp->chgBit(oldp+475,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x1eU)))));
        bufp->chgCData(oldp+476,((0x1fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                   [1U] 
                                                   >> 0x19U)))),5);
        bufp->chgBit(oldp+477,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x18U)))));
        bufp->chgBit(oldp+478,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x17U)))));
        bufp->chgBit(oldp+479,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x16U)))));
        bufp->chgBit(oldp+480,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x15U)))));
        bufp->chgBit(oldp+481,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+482,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x13U)))));
        bufp->chgBit(oldp+483,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x12U)))));
        bufp->chgBit(oldp+484,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0x11U)))));
        bufp->chgCData(oldp+485,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                   [1U] 
                                                   >> 0xbU)))),6);
        bufp->chgBit(oldp+486,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                              [1U] 
                                              >> 0xaU)))));
        bufp->chgCData(oldp+487,((0x3fU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                   [1U] 
                                                   >> 4U)))),6);
        bufp->chgCData(oldp+488,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailData
                                                 [1U]))),4);
        bufp->chgCData(oldp+489,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr[0]),6);
        bufp->chgCData(oldp+490,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__pushedTailPtr[1]),6);
        bufp->chgCData(oldp+491,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__detectedFlushRangeTailPtr),6);
        bufp->chgBit(oldp+492,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocate[0]));
        bufp->chgBit(oldp+493,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocate[1]));
        bufp->chgBit(oldp+494,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__pop[0]));
        bufp->chgBit(oldp+495,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__pop[1]));
        bufp->chgBit(oldp+496,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__pop[0]));
        bufp->chgBit(oldp+497,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__pop[1]));
        bufp->chgBit(oldp+498,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__pop[0]));
        bufp->chgBit(oldp+499,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__pop[1]));
        bufp->chgBit(oldp+500,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pop[0]));
        bufp->chgBit(oldp+501,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pop[1]));
        bufp->chgBit(oldp+502,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__we[0]));
        bufp->chgBit(oldp+503,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__we[1]));
        bufp->chgCData(oldp+504,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wa[0]),6);
        bufp->chgCData(oldp+505,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wa[1]),6);
        bufp->chgQData(oldp+506,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wv[0]),63);
        bufp->chgQData(oldp+508,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__wv[1]),63);
        bufp->chgBit(oldp+510,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[0]));
        bufp->chgBit(oldp+511,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[1]));
        bufp->chgBit(oldp+512,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[2]));
        bufp->chgBit(oldp+513,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[3]));
        bufp->chgBit(oldp+514,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[4]));
        bufp->chgBit(oldp+515,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[5]));
        bufp->chgBit(oldp+516,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[6]));
        bufp->chgBit(oldp+517,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__we[7]));
        bufp->chgCData(oldp+518,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[0]),6);
        bufp->chgCData(oldp+519,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[1]),6);
        bufp->chgCData(oldp+520,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[2]),6);
        bufp->chgCData(oldp+521,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[3]),6);
        bufp->chgCData(oldp+522,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[4]),6);
        bufp->chgCData(oldp+523,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[5]),6);
        bufp->chgCData(oldp+524,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[6]),6);
        bufp->chgCData(oldp+525,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState.__PVT__wa[7]),6);
        bufp->chgBit(oldp+526,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[0]));
        bufp->chgBit(oldp+527,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[1]));
        bufp->chgBit(oldp+528,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__we[2]));
        bufp->chgCData(oldp+529,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[0]),6);
        bufp->chgCData(oldp+530,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[1]),6);
        bufp->chgCData(oldp+531,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState.__PVT__wa[2]),6);
        bufp->chgCData(oldp+532,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[0]),6);
        bufp->chgCData(oldp+533,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[1]),6);
        bufp->chgCData(oldp+534,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[2]),6);
        bufp->chgCData(oldp+535,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[3]),6);
        bufp->chgCData(oldp+536,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[4]),6);
        bufp->chgCData(oldp+537,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[5]),6);
        bufp->chgCData(oldp+538,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[6]),6);
        bufp->chgCData(oldp+539,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__ra[7]),6);
        bufp->chgSData(oldp+540,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[0]),10);
        bufp->chgSData(oldp+541,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[1]),10);
        bufp->chgSData(oldp+542,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[2]),10);
        bufp->chgSData(oldp+543,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[3]),10);
        bufp->chgSData(oldp+544,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[4]),10);
        bufp->chgSData(oldp+545,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[5]),10);
        bufp->chgSData(oldp+546,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[6]),10);
        bufp->chgSData(oldp+547,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__rv[7]),10);
        bufp->chgBit(oldp+548,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[0]));
        bufp->chgBit(oldp+549,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[1]));
        bufp->chgBit(oldp+550,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[2]));
        bufp->chgBit(oldp+551,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[3]));
        bufp->chgBit(oldp+552,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[4]));
        bufp->chgBit(oldp+553,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[5]));
        bufp->chgBit(oldp+554,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[6]));
        bufp->chgBit(oldp+555,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we[7]));
        bufp->chgCData(oldp+556,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[0]),6);
        bufp->chgCData(oldp+557,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[1]),6);
        bufp->chgCData(oldp+558,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[2]),6);
        bufp->chgCData(oldp+559,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[3]),6);
        bufp->chgCData(oldp+560,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[4]),6);
        bufp->chgCData(oldp+561,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[5]),6);
        bufp->chgCData(oldp+562,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[6]),6);
        bufp->chgCData(oldp+563,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa[7]),6);
        bufp->chgBit(oldp+564,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [0U]));
        bufp->chgCData(oldp+565,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [0U]),6);
        bufp->chgBit(oldp+566,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [1U]));
        bufp->chgCData(oldp+567,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [1U]),6);
        bufp->chgBit(oldp+568,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [2U]));
        bufp->chgCData(oldp+569,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [2U]),6);
        bufp->chgBit(oldp+570,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [3U]));
        bufp->chgCData(oldp+571,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [3U]),6);
        bufp->chgBit(oldp+572,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [4U]));
        bufp->chgCData(oldp+573,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [4U]),6);
        bufp->chgBit(oldp+574,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [5U]));
        bufp->chgCData(oldp+575,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [5U]),6);
        bufp->chgBit(oldp+576,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [6U]));
        bufp->chgCData(oldp+577,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [6U]),6);
        bufp->chgBit(oldp+578,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__we
                               [7U]));
        bufp->chgCData(oldp+579,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body.__PVT__wa
                                 [7U]),6);
        bufp->chgBit(oldp+580,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__we[0]));
        bufp->chgBit(oldp+581,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__we[1]));
        bufp->chgBit(oldp+582,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__we[2]));
        bufp->chgCData(oldp+583,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__wa[0]),6);
        bufp->chgCData(oldp+584,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__wa[1]),6);
        bufp->chgCData(oldp+585,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__wa[2]),6);
        bufp->chgBit(oldp+586,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__we
                               [0U]));
        bufp->chgCData(oldp+587,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__wa
                                 [0U]),6);
        bufp->chgBit(oldp+588,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__we
                               [1U]));
        bufp->chgCData(oldp+589,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__wa
                                 [1U]),6);
        bufp->chgBit(oldp+590,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__we
                               [2U]));
        bufp->chgCData(oldp+591,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__wa
                                 [2U]),6);
        bufp->chgCData(oldp+592,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[0]),2);
        bufp->chgCData(oldp+593,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[1]),2);
        bufp->chgCData(oldp+594,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[2]),2);
        bufp->chgCData(oldp+595,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[0]),6);
        bufp->chgCData(oldp+596,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[1]),6);
        bufp->chgCData(oldp+597,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[2]),6);
        bufp->chgCData(oldp+598,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [0U][0U]),2);
        bufp->chgCData(oldp+599,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [0U][1U]),2);
        bufp->chgCData(oldp+600,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [0U][2U]),2);
        bufp->chgCData(oldp+601,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [1U][0U]),2);
        bufp->chgCData(oldp+602,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [1U][1U]),2);
        bufp->chgCData(oldp+603,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [1U][2U]),2);
        bufp->chgCData(oldp+604,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [2U][0U]),2);
        bufp->chgCData(oldp+605,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [2U][1U]),2);
        bufp->chgCData(oldp+606,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                 [2U][2U]),2);
        bufp->chgCData(oldp+607,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                 [0U]),2);
        bufp->chgCData(oldp+608,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                 [1U]),2);
        bufp->chgCData(oldp+609,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                 [2U]),2);
        bufp->chgCData(oldp+610,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                 [1U]),6);
        bufp->chgCData(oldp+611,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                 [2U]),6);
        bufp->chgCData(oldp+612,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                 [0U]),6);
        bufp->chgCData(oldp+613,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[0]),6);
        bufp->chgCData(oldp+614,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[1]),6);
        bufp->chgCData(oldp+615,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[2]),6);
        bufp->chgCData(oldp+616,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[3]),6);
        bufp->chgCData(oldp+617,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[4]),6);
        bufp->chgCData(oldp+618,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[5]),6);
        bufp->chgCData(oldp+619,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[6]),6);
        bufp->chgCData(oldp+620,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra[7]),6);
        bufp->chgSData(oldp+621,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[0]),10);
        bufp->chgSData(oldp+622,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[1]),10);
        bufp->chgSData(oldp+623,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[2]),10);
        bufp->chgSData(oldp+624,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[3]),10);
        bufp->chgSData(oldp+625,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[4]),10);
        bufp->chgSData(oldp+626,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[5]),10);
        bufp->chgSData(oldp+627,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[6]),10);
        bufp->chgSData(oldp+628,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__rv[7]),10);
        bufp->chgSData(oldp+629,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [0U][0U]),10);
        bufp->chgSData(oldp+630,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [0U][1U]),10);
        bufp->chgSData(oldp+631,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [1U][0U]),10);
        bufp->chgSData(oldp+632,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [1U][1U]),10);
        bufp->chgSData(oldp+633,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [2U][0U]),10);
        bufp->chgSData(oldp+634,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [2U][1U]),10);
        bufp->chgSData(oldp+635,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [3U][0U]),10);
        bufp->chgSData(oldp+636,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [3U][1U]),10);
        bufp->chgSData(oldp+637,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [4U][0U]),10);
        bufp->chgSData(oldp+638,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [4U][1U]),10);
        bufp->chgSData(oldp+639,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [5U][0U]),10);
        bufp->chgSData(oldp+640,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [5U][1U]),10);
        bufp->chgSData(oldp+641,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [6U][0U]),10);
        bufp->chgSData(oldp+642,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [6U][1U]),10);
        bufp->chgSData(oldp+643,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [7U][0U]),10);
        bufp->chgSData(oldp+644,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__rvBank
                                 [7U][1U]),10);
        bufp->chgBit(oldp+645,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[0]));
        bufp->chgBit(oldp+646,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[1]));
        bufp->chgBit(oldp+647,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[2]));
        bufp->chgBit(oldp+648,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[3]));
        bufp->chgBit(oldp+649,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[4]));
        bufp->chgBit(oldp+650,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[5]));
        bufp->chgBit(oldp+651,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[6]));
        bufp->chgBit(oldp+652,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvo[7]));
        bufp->chgCData(oldp+653,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [0U]),6);
        bufp->chgCData(oldp+654,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [1U]),6);
        bufp->chgCData(oldp+655,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [2U]),6);
        bufp->chgCData(oldp+656,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [3U]),6);
        bufp->chgCData(oldp+657,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [4U]),6);
        bufp->chgCData(oldp+658,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [5U]),6);
        bufp->chgCData(oldp+659,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [6U]),6);
        bufp->chgCData(oldp+660,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                 [7U]),6);
        bufp->chgCData(oldp+661,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[0]),6);
        bufp->chgCData(oldp+662,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[1]),6);
        bufp->chgCData(oldp+663,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[2]),6);
        bufp->chgCData(oldp+664,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[3]),6);
        bufp->chgCData(oldp+665,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[4]),6);
        bufp->chgCData(oldp+666,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[5]),6);
        bufp->chgCData(oldp+667,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[6]),6);
        bufp->chgCData(oldp+668,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr[7]),6);
        bufp->chgBit(oldp+669,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][0U]));
        bufp->chgBit(oldp+670,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][1U]));
        bufp->chgBit(oldp+671,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][2U]));
        bufp->chgBit(oldp+672,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][3U]));
        bufp->chgBit(oldp+673,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][4U]));
        bufp->chgBit(oldp+674,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][5U]));
        bufp->chgBit(oldp+675,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][6U]));
        bufp->chgBit(oldp+676,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [0U][7U]));
        bufp->chgBit(oldp+677,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][0U]));
        bufp->chgBit(oldp+678,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][1U]));
        bufp->chgBit(oldp+679,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][2U]));
        bufp->chgBit(oldp+680,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][3U]));
        bufp->chgBit(oldp+681,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][4U]));
        bufp->chgBit(oldp+682,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][5U]));
        bufp->chgBit(oldp+683,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][6U]));
        bufp->chgBit(oldp+684,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadValue
                               [1U][7U]));
        bufp->chgCData(oldp+685,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [0U]),6);
        bufp->chgCData(oldp+686,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [1U]),6);
        bufp->chgCData(oldp+687,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [2U]),6);
        bufp->chgCData(oldp+688,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [3U]),6);
        bufp->chgCData(oldp+689,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [4U]),6);
        bufp->chgCData(oldp+690,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [5U]),6);
        bufp->chgCData(oldp+691,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [6U]),6);
        bufp->chgCData(oldp+692,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                 [7U]),6);
        bufp->chgBit(oldp+693,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[0]));
        bufp->chgBit(oldp+694,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[1]));
        bufp->chgBit(oldp+695,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[2]));
        bufp->chgBit(oldp+696,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[3]));
        bufp->chgBit(oldp+697,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[4]));
        bufp->chgBit(oldp+698,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[5]));
        bufp->chgBit(oldp+699,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[6]));
        bufp->chgBit(oldp+700,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we[7]));
        bufp->chgCData(oldp+701,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[0]),6);
        bufp->chgCData(oldp+702,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[1]),6);
        bufp->chgCData(oldp+703,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[2]),6);
        bufp->chgCData(oldp+704,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[3]),6);
        bufp->chgCData(oldp+705,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[4]),6);
        bufp->chgCData(oldp+706,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[5]),6);
        bufp->chgCData(oldp+707,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[6]),6);
        bufp->chgCData(oldp+708,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa[7]),6);
        bufp->chgCData(oldp+709,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[0]),3);
        bufp->chgCData(oldp+710,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[1]),3);
        bufp->chgCData(oldp+711,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[2]),3);
        bufp->chgCData(oldp+712,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[3]),3);
        bufp->chgCData(oldp+713,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[4]),3);
        bufp->chgCData(oldp+714,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[5]),3);
        bufp->chgCData(oldp+715,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[6]),3);
        bufp->chgCData(oldp+716,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue[7]),3);
        bufp->chgCData(oldp+717,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[0]),6);
        bufp->chgCData(oldp+718,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[1]),6);
        bufp->chgCData(oldp+719,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[2]),6);
        bufp->chgCData(oldp+720,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[3]),6);
        bufp->chgCData(oldp+721,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[4]),6);
        bufp->chgCData(oldp+722,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[5]),6);
        bufp->chgCData(oldp+723,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[6]),6);
        bufp->chgCData(oldp+724,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr[7]),6);
        bufp->chgBit(oldp+725,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [0U]));
        bufp->chgCData(oldp+726,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [0U]),6);
        bufp->chgCData(oldp+727,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [0U]),3);
        bufp->chgBit(oldp+728,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [1U]));
        bufp->chgCData(oldp+729,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [1U]),6);
        bufp->chgCData(oldp+730,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [1U]),3);
        bufp->chgBit(oldp+731,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [2U]));
        bufp->chgCData(oldp+732,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [2U]),6);
        bufp->chgCData(oldp+733,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [2U]),3);
        bufp->chgBit(oldp+734,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [3U]));
        bufp->chgCData(oldp+735,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [3U]),6);
        bufp->chgCData(oldp+736,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [3U]),3);
        bufp->chgBit(oldp+737,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [4U]));
        bufp->chgCData(oldp+738,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [4U]),6);
        bufp->chgCData(oldp+739,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [4U]),3);
        bufp->chgBit(oldp+740,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [5U]));
        bufp->chgCData(oldp+741,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [5U]),6);
        bufp->chgCData(oldp+742,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [5U]),3);
        bufp->chgBit(oldp+743,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [6U]));
        bufp->chgCData(oldp+744,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [6U]),6);
        bufp->chgCData(oldp+745,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [6U]),3);
        bufp->chgBit(oldp+746,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__we
                               [7U]));
        bufp->chgCData(oldp+747,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wa
                                 [7U]),6);
        bufp->chgCData(oldp+748,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__rwbWriteValue
                                 [7U]),3);
        bufp->chgCData(oldp+749,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [1U]),6);
        bufp->chgCData(oldp+750,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [2U]),6);
        bufp->chgCData(oldp+751,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [3U]),6);
        bufp->chgCData(oldp+752,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [4U]),6);
        bufp->chgCData(oldp+753,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [5U]),6);
        bufp->chgCData(oldp+754,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [6U]),6);
        bufp->chgCData(oldp+755,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [7U]),6);
        bufp->chgCData(oldp+756,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                 [0U]),6);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x21U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x6aU])))) {
        bufp->chgCData(oldp+757,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[0]),7);
        bufp->chgCData(oldp+758,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[1]),7);
        bufp->chgCData(oldp+759,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[2]),7);
        bufp->chgCData(oldp+760,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[3]),7);
        bufp->chgCData(oldp+761,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[4]),7);
        bufp->chgCData(oldp+762,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[5]),7);
        bufp->chgCData(oldp+763,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[6]),7);
        bufp->chgCData(oldp+764,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[7]),7);
        bufp->chgCData(oldp+765,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[8]),7);
        bufp->chgCData(oldp+766,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[9]),7);
        bufp->chgCData(oldp+767,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcRegNum[10]),7);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x22U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x6bU])))) {
        bufp->chgCData(oldp+768,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcFPRegNum[0]),7);
        bufp->chgCData(oldp+769,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcFPRegNum[1]),7);
        bufp->chgCData(oldp+770,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcFPRegNum[2]),7);
        bufp->chgCData(oldp+771,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcFPRegNum[3]),7);
        bufp->chgCData(oldp+772,(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__srcFPRegNum[4]),7);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x23U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x6cU])))) {
        bufp->chgBit(oldp+773,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                      [0U] >> 7U))));
        bufp->chgCData(oldp+774,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                           [0U] >> 1U))),6);
        bufp->chgBit(oldp+775,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                [0U])));
        bufp->chgBit(oldp+776,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                      [1U] >> 7U))));
        bufp->chgCData(oldp+777,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                           [1U] >> 1U))),6);
        bufp->chgBit(oldp+778,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                [1U])));
        bufp->chgBit(oldp+779,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memRR
                                      [0U] >> 7U))));
        bufp->chgCData(oldp+780,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memRR
                                           [0U] >> 1U))),6);
        bufp->chgBit(oldp+781,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memRR
                                [0U])));
        bufp->chgBit(oldp+782,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+783,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+785,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+786,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+787,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+788,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+789,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+790,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+791,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+792,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+793,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+794,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+795,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+796,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+797,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+798,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+799,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                [0U])));
        bufp->chgBit(oldp+800,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 0x14U))));
        bufp->chgCData(oldp+801,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                        [1U] >> 0x12U))),2);
        bufp->chgBit(oldp+802,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 0x11U))));
        bufp->chgBit(oldp+803,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 0x10U))));
        bufp->chgBit(oldp+804,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 0xfU))));
        bufp->chgBit(oldp+805,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 0xeU))));
        bufp->chgBit(oldp+806,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 0xdU))));
        bufp->chgCData(oldp+807,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                        [1U] >> 0xbU))),2);
        bufp->chgBit(oldp+808,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 0xaU))));
        bufp->chgBit(oldp+809,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 9U))));
        bufp->chgBit(oldp+810,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 8U))));
        bufp->chgBit(oldp+811,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 7U))));
        bufp->chgBit(oldp+812,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+813,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                        [1U] >> 4U))),2);
        bufp->chgBit(oldp+814,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 3U))));
        bufp->chgBit(oldp+815,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 2U))));
        bufp->chgBit(oldp+816,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                      [1U] >> 1U))));
        bufp->chgBit(oldp+817,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intBypassCtrl
                                [1U])));
        bufp->chgBit(oldp+818,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+819,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+820,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+821,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+822,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+823,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+824,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+825,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+826,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+827,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+828,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+829,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+830,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+831,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+832,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+833,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+834,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+835,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__complexBypassCtrl
                                [0U])));
        bufp->chgBit(oldp+836,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+837,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+838,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+839,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+840,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+841,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+842,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+843,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+844,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+845,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+846,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+847,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+848,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+849,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+850,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+851,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+852,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+853,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                [0U])));
        bufp->chgBit(oldp+854,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 0x14U))));
        bufp->chgCData(oldp+855,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                        [1U] >> 0x12U))),2);
        bufp->chgBit(oldp+856,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 0x11U))));
        bufp->chgBit(oldp+857,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 0x10U))));
        bufp->chgBit(oldp+858,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 0xfU))));
        bufp->chgBit(oldp+859,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 0xeU))));
        bufp->chgBit(oldp+860,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 0xdU))));
        bufp->chgCData(oldp+861,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                        [1U] >> 0xbU))),2);
        bufp->chgBit(oldp+862,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 0xaU))));
        bufp->chgBit(oldp+863,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 9U))));
        bufp->chgBit(oldp+864,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 8U))));
        bufp->chgBit(oldp+865,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 7U))));
        bufp->chgBit(oldp+866,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+867,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                        [1U] >> 4U))),2);
        bufp->chgBit(oldp+868,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 3U))));
        bufp->chgBit(oldp+869,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 2U))));
        bufp->chgBit(oldp+870,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                      [1U] >> 1U))));
        bufp->chgBit(oldp+871,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memBypassCtrl
                                [1U])));
        bufp->chgBit(oldp+872,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+873,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+874,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+875,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+876,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+877,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+878,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+879,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+880,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+881,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+882,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+883,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+884,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+885,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+886,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+887,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+888,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+889,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__fpBypassCtrl
                                [0U])));
        bufp->chgBit(oldp+890,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                      [0U] >> 7U))));
        bufp->chgCData(oldp+891,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                           [0U] >> 1U))),6);
        bufp->chgBit(oldp+892,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                [0U])));
        bufp->chgBit(oldp+893,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                      [1U] >> 7U))));
        bufp->chgCData(oldp+894,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                           [1U] >> 1U))),6);
        bufp->chgBit(oldp+895,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__intRR
                                [1U])));
        bufp->chgBit(oldp+896,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memRR
                                      [0U] >> 7U))));
        bufp->chgCData(oldp+897,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memRR
                                           [0U] >> 1U))),6);
        bufp->chgBit(oldp+898,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__memRR
                                [0U])));
        bufp->chgIData(oldp+899,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+900,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+901,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+902,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+903,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassController__DOT__unnamedblk7__DOT__i),32);
        bufp->chgBit(oldp+904,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+905,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+906,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+907,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+908,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+909,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+910,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+911,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+912,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+913,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+914,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+915,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+916,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+917,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+918,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+919,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+920,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+921,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                [0U])));
        bufp->chgBit(oldp+922,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 0x14U))));
        bufp->chgCData(oldp+923,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                        [1U] >> 0x12U))),2);
        bufp->chgBit(oldp+924,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 0x11U))));
        bufp->chgBit(oldp+925,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 0x10U))));
        bufp->chgBit(oldp+926,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 0xfU))));
        bufp->chgBit(oldp+927,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 0xeU))));
        bufp->chgBit(oldp+928,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 0xdU))));
        bufp->chgCData(oldp+929,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                        [1U] >> 0xbU))),2);
        bufp->chgBit(oldp+930,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 0xaU))));
        bufp->chgBit(oldp+931,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 9U))));
        bufp->chgBit(oldp+932,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 8U))));
        bufp->chgBit(oldp+933,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 7U))));
        bufp->chgBit(oldp+934,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+935,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                        [1U] >> 4U))),2);
        bufp->chgBit(oldp+936,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 3U))));
        bufp->chgBit(oldp+937,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 2U))));
        bufp->chgBit(oldp+938,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                      [1U] >> 1U))));
        bufp->chgBit(oldp+939,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
                                [1U])));
        bufp->chgBit(oldp+940,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+941,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+942,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+943,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+944,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+945,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+946,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+947,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+948,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+950,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+951,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+952,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+953,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+954,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+955,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+957,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
                                [0U])));
        bufp->chgBit(oldp+958,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+959,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+960,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+961,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+962,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+963,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+964,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+965,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+966,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+967,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+968,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+969,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+970,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+971,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+972,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+973,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+974,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+975,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                [0U])));
        bufp->chgBit(oldp+976,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 0x14U))));
        bufp->chgCData(oldp+977,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                        [1U] >> 0x12U))),2);
        bufp->chgBit(oldp+978,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 0x11U))));
        bufp->chgBit(oldp+979,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 0x10U))));
        bufp->chgBit(oldp+980,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 0xfU))));
        bufp->chgBit(oldp+981,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 0xeU))));
        bufp->chgBit(oldp+982,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 0xdU))));
        bufp->chgCData(oldp+983,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                        [1U] >> 0xbU))),2);
        bufp->chgBit(oldp+984,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 0xaU))));
        bufp->chgBit(oldp+985,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 9U))));
        bufp->chgBit(oldp+986,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 8U))));
        bufp->chgBit(oldp+987,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 7U))));
        bufp->chgBit(oldp+988,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+989,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                        [1U] >> 4U))),2);
        bufp->chgBit(oldp+990,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 3U))));
        bufp->chgBit(oldp+991,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 2U))));
        bufp->chgBit(oldp+992,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                      [1U] >> 1U))));
        bufp->chgBit(oldp+993,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlOut
                                [1U])));
        bufp->chgBit(oldp+994,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+995,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+996,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+997,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+998,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+999,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+1000,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 0xdU))));
        bufp->chgCData(oldp+1001,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                         [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+1002,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 0xaU))));
        bufp->chgBit(oldp+1003,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 9U))));
        bufp->chgBit(oldp+1004,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 8U))));
        bufp->chgBit(oldp+1005,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+1006,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1007,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                         [0U] >> 4U))),2);
        bufp->chgBit(oldp+1008,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 3U))));
        bufp->chgBit(oldp+1009,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 2U))));
        bufp->chgBit(oldp+1010,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                       [0U] >> 1U))));
        bufp->chgBit(oldp+1011,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlOut
                                 [0U])));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x24U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x6dU])))) {
        bufp->chgCData(oldp+1012,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatedPhyScalarFPRegNum[0]),7);
        bufp->chgCData(oldp+1013,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatedPhyScalarFPRegNum[1]),7);
        bufp->chgCData(oldp+1014,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__poppedData[0]),7);
        bufp->chgCData(oldp+1015,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__poppedData[1]),7);
        bufp->chgCData(oldp+1016,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__popCount),2);
        bufp->chgBit(oldp+1017,((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__popCount))));
        bufp->chgCData(oldp+1018,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__queuePointer__DOT__nextHead),5);
        bufp->chgCData(oldp+1019,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__queuePointer__DOT__nextCount),6);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x25U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x6eU])))) {
        bufp->chgCData(oldp+1020,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__poppedData[0]),7);
        bufp->chgCData(oldp+1021,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__poppedData[1]),7);
        bufp->chgCData(oldp+1022,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__popCount),2);
        bufp->chgBit(oldp+1023,((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__popCount))));
        bufp->chgCData(oldp+1024,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__queuePointer__DOT__nextHead),5);
        bufp->chgCData(oldp+1025,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__queuePointer__DOT__nextCount),6);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x26U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x6fU])))) {
        bufp->chgCData(oldp+1026,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__poppedData[0]),7);
        bufp->chgCData(oldp+1027,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__poppedData[1]),7);
        bufp->chgCData(oldp+1028,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__popCount),2);
        bufp->chgBit(oldp+1029,((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__popCount))));
        bufp->chgCData(oldp+1030,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__queuePointer__DOT__nextHead),5);
        bufp->chgCData(oldp+1031,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__queuePointer__DOT__nextCount),6);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x27U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x70U])))) {
        bufp->chgSData(oldp+1032,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                     [0U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1033,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                 [0U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1034,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1035,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                       [0U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1036,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1037,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                    [0U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1038,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1039,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1040,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1041,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1042,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1043,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1044,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1045,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1046,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                    [0U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1047,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [0U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1048,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                    [0U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1049,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                  [0U]))),4);
        bufp->chgSData(oldp+1050,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                     [1U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1051,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                 [1U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1052,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1053,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                       [1U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1054,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1055,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                    [1U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1056,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1057,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1058,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1059,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1060,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1061,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1062,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1063,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1064,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                    [1U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1065,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                               [1U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1066,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                    [1U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1067,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__readData
                                                  [1U]))),4);
        bufp->chgSData(oldp+1068,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                     [0U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1069,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                 [0U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1070,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1071,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                       [0U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1072,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1073,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                    [0U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1074,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1075,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1076,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1077,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1078,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1079,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1080,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1081,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1082,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                    [0U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1083,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [0U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1084,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                    [0U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1085,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                  [0U]))),4);
        bufp->chgSData(oldp+1086,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                     [1U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1087,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                 [1U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1088,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1089,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                       [1U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1090,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1091,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                    [1U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1092,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1093,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1094,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1095,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1096,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1097,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1098,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1099,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1100,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                    [1U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1101,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                               [1U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1102,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                    [1U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1103,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__readData
                                                  [1U]))),4);
        bufp->chgQData(oldp+1104,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__rv[0]),63);
        bufp->chgQData(oldp+1106,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__rv[1]),63);
        bufp->chgCData(oldp+1108,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),6);
        bufp->chgCData(oldp+1109,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),6);
        bufp->chgCData(oldp+1110,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),6);
        bufp->chgCData(oldp+1111,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),6);
        bufp->chgQData(oldp+1112,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]),63);
        bufp->chgQData(oldp+1114,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]),63);
        bufp->chgBit(oldp+1116,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+1117,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+1118,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [0U]));
        bufp->chgCData(oldp+1119,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                            [0U] >> 1U))),5);
        bufp->chgQData(oldp+1120,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [0U]),63);
        bufp->chgCData(oldp+1122,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                            [0U] >> 1U))),5);
        bufp->chgBit(oldp+1123,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [1U]));
        bufp->chgCData(oldp+1124,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                            [1U] >> 1U))),5);
        bufp->chgQData(oldp+1125,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [1U]),63);
        bufp->chgCData(oldp+1127,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                            [1U] >> 1U))),5);
        bufp->chgIData(oldp+1128,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b),32);
        bufp->chgIData(oldp+1129,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+1130,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b),32);
        bufp->chgIData(oldp+1131,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+1132,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+1133,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__activeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x28U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x71U])))) {
        bufp->chgIData(oldp+1134,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__lastCommittedPC),32);
        bufp->chgBit(oldp+1135,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__lastCommittedPC 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1136,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__lastCommittedPC)),19);
        bufp->chgBit(oldp+1137,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueue));
        bufp->chgCData(oldp+1138,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseLoadQueueEntryNum),2);
        bufp->chgCData(oldp+1139,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextHead),4);
        bufp->chgCData(oldp+1140,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextTail),4);
        bufp->chgCData(oldp+1141,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__roundedSetTailPtr),4);
        bufp->chgCData(oldp+1142,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount),5);
        bufp->chgBit(oldp+1143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+1144,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1145,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                                   [0U])),6);
        bufp->chgBit(oldp+1146,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                                       [1U] >> 7U))));
        bufp->chgBit(oldp+1147,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1148,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__nextReleasedReg
                                   [1U])),6);
        bufp->chgSData(oldp+1149,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                     [0U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1150,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                 [0U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1151,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1152,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                       [0U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1153,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1154,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                    [0U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1155,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1156,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1157,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1158,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1159,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1160,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1161,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1162,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1163,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                    [0U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1164,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [0U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1165,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                    [0U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1166,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                  [0U]))),4);
        bufp->chgSData(oldp+1167,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                     [1U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1168,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                 [1U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1169,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1170,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                       [1U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1171,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1172,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                    [1U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1173,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1174,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1175,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1176,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1177,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1178,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1179,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1180,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1181,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                    [1U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1182,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                               [1U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1183,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                    [1U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1184,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__alReadData
                                                  [1U]))),4);
        bufp->chgCData(oldp+1185,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__releaseNum),2);
        bufp->chgCData(oldp+1186,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__flushNum),2);
        bufp->chgBit(oldp+1187,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__toRecoveryPhase));
        bufp->chgBit(oldp+1188,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__last[0]));
        bufp->chgBit(oldp+1189,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__last[1]));
        bufp->chgBit(oldp+1190,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__isBranch[0]));
        bufp->chgBit(oldp+1191,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__isBranch[1]));
        bufp->chgBit(oldp+1192,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__isStore[0]));
        bufp->chgBit(oldp+1193,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__isStore[1]));
        bufp->chgCData(oldp+1194,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__execState
                                  [0U]),4);
        bufp->chgCData(oldp+1195,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__execState
                                  [1U]),4);
        bufp->chgBit(oldp+1196,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__recoveryOpIndex));
        bufp->chgCData(oldp+1197,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__refetchType),3);
        bufp->chgCData(oldp+1198,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__recoveryCause),4);
        bufp->chgCData(oldp+1199,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__commitNum),2);
        bufp->chgCData(oldp+1200,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__commitLoadNum),2);
        bufp->chgCData(oldp+1201,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__commitStoreNum),2);
        bufp->chgBit(oldp+1202,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__fflagsWE));
        bufp->chgBit(oldp+1203,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__fflagsData) 
                                       >> 4U))));
        bufp->chgBit(oldp+1204,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__fflagsData) 
                                       >> 3U))));
        bufp->chgBit(oldp+1205,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__fflagsData) 
                                       >> 2U))));
        bufp->chgBit(oldp+1206,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__fflagsData) 
                                       >> 1U))));
        bufp->chgBit(oldp+1207,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__fflagsData))));
        bufp->chgCData(oldp+1208,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__phase),2);
        bufp->chgBit(oldp+1209,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__lastCommittedPC 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1210,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__lastCommittedPC)),19);
        bufp->chgIData(oldp+1211,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+1212,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+1213,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+1214,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+1215,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+1216,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__unnamedblk6__DOT__i),32);
        bufp->chgBit(oldp+1217,((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum))));
        bufp->chgBit(oldp+1218,((0U < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum))));
        bufp->chgCData(oldp+1219,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popTailNum),2);
        bufp->chgCData(oldp+1220,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__popHeadNum),2);
        bufp->chgCData(oldp+1221,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__nextHead),6);
        bufp->chgCData(oldp+1222,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__nextTail),6);
        bufp->chgCData(oldp+1223,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__nextCount),7);
        bufp->chgBit(oldp+1224,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__we[0]));
        bufp->chgBit(oldp+1225,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__we[1]));
        bufp->chgBit(oldp+1226,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__writeLogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+1227,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__writeLogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+1228,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__writeLogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+1229,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__writeLogRegNum
                                   [1U])),5);
        bufp->chgCData(oldp+1230,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__writePhyRegNum[0]),6);
        bufp->chgCData(oldp+1231,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__writePhyRegNum[1]),6);
        bufp->chgBit(oldp+1232,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStore));
        bufp->chgCData(oldp+1233,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__commitStoreNum),2);
        bufp->chgCData(oldp+1234,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg),2);
        bufp->chgBit(oldp+1235,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1236,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+1237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1238,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_PhyRegNum
                                   [1U])),6);
        bufp->chgBit(oldp+1239,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+1240,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+1241,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+1242,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_WriteReg_LogRegNum
                                   [1U])),5);
        bufp->chgBit(oldp+1243,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commit));
        bufp->chgCData(oldp+1244,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__commitNum),2);
        bufp->chgCData(oldp+1245,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__flushNum),2);
        bufp->chgCData(oldp+1246,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__commitNum),2);
        bufp->chgBit(oldp+1247,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsWE));
        bufp->chgBit(oldp+1248,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsData) 
                                       >> 4U))));
        bufp->chgBit(oldp+1249,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsData) 
                                       >> 3U))));
        bufp->chgBit(oldp+1250,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsData) 
                                       >> 2U))));
        bufp->chgBit(oldp+1251,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsData) 
                                       >> 1U))));
        bufp->chgBit(oldp+1252,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflagsData))));
        bufp->chgBit(oldp+1253,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [0U] >> 0x15U))));
        bufp->chgBit(oldp+1254,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [0U] >> 0x14U))));
        bufp->chgSData(oldp+1255,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                             [0U] >> 0xaU))),10);
        bufp->chgCData(oldp+1256,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                         [0U] >> 8U))),2);
        bufp->chgBit(oldp+1257,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+1258,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1259,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                   [0U])),6);
        bufp->chgBit(oldp+1260,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [1U] >> 0x15U))));
        bufp->chgBit(oldp+1261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [1U] >> 0x14U))));
        bufp->chgSData(oldp+1262,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                             [1U] >> 0xaU))),10);
        bufp->chgCData(oldp+1263,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                         [1U] >> 8U))),2);
        bufp->chgBit(oldp+1264,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [1U] >> 7U))));
        bufp->chgBit(oldp+1265,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1266,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
                                   [1U])),6);
        bufp->chgBit(oldp+1267,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__toRecoveryPhase));
        bufp->chgCData(oldp+1268,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromCommitStage),3);
        bufp->chgBit(oldp+1269,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryOpIndex));
        bufp->chgBit(oldp+1270,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage));
        bufp->chgCData(oldp+1271,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage),4);
        bufp->chgBit(oldp+1272,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__we[0]));
        bufp->chgBit(oldp+1273,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__we[1]));
        bufp->chgCData(oldp+1274,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wa[0]),6);
        bufp->chgCData(oldp+1275,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wa[1]),6);
        bufp->chgCData(oldp+1276,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wv[0]),6);
        bufp->chgCData(oldp+1277,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT.__PVT__wv[1]),6);
        bufp->chgBit(oldp+1278,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__we[0]));
        bufp->chgBit(oldp+1279,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__we[1]));
        bufp->chgCData(oldp+1280,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wa[0]),6);
        bufp->chgCData(oldp+1281,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wa[1]),6);
        bufp->chgCData(oldp+1282,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wv[0]),6);
        bufp->chgCData(oldp+1283,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wv[1]),6);
        bufp->chgBit(oldp+1284,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__we
                                [0U]));
        bufp->chgCData(oldp+1285,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wa
                                  [0U]),6);
        bufp->chgCData(oldp+1286,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wv
                                  [0U]),6);
        bufp->chgBit(oldp+1287,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__we
                                [1U]));
        bufp->chgCData(oldp+1288,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wa
                                  [1U]),6);
        bufp->chgCData(oldp+1289,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__wv
                                  [1U]),6);
        bufp->chgBit(oldp+1290,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[0]));
        bufp->chgBit(oldp+1291,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[1]));
        bufp->chgCData(oldp+1292,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[0]),6);
        bufp->chgCData(oldp+1293,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[1]),6);
        bufp->chgBit(oldp+1294,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [0U][0U]));
        bufp->chgBit(oldp+1295,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [0U][1U]));
        bufp->chgBit(oldp+1296,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [1U][0U]));
        bufp->chgBit(oldp+1297,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [1U][1U]));
        bufp->chgBit(oldp+1298,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                [0U]));
        bufp->chgBit(oldp+1299,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                [1U]));
        bufp->chgCData(oldp+1300,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [1U]),6);
        bufp->chgCData(oldp+1301,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [0U]),6);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x29U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x72U])))) {
        bufp->chgCData(oldp+1302,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextUnfinishedStoreNum),5);
        bufp->chgBit(oldp+1303,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                                               >> 0x3cU)))));
        bufp->chgBit(oldp+1304,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                                               >> 0x3bU)))));
        bufp->chgIData(oldp+1305,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                                           >> 0x1bU))),32);
        bufp->chgIData(oldp+1306,((0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                                                       >> 7U)))),20);
        bufp->chgBit(oldp+1307,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                                               >> 6U)))));
        bufp->chgCData(oldp+1308,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                                                   >> 2U)))),4);
        bufp->chgBit(oldp+1309,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg 
                                               >> 1U)))));
        bufp->chgBit(oldp+1310,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextTagStagePipeReg))));
        bufp->chgBit(oldp+1311,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dcWriteReq));
        bufp->chgBit(oldp+1312,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dcWriteAddr 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dcWriteAddr 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1314,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dcWriteAddr)),20);
        bufp->chgQData(oldp+1315,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dcWriteData),64);
        bufp->chgBit(oldp+1317,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dcWriteUncachable));
        bufp->chgBit(oldp+1318,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__isIO));
        bufp->chgCData(oldp+1319,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dcWriteByteWE),8);
        bufp->chgBit(oldp+1320,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__isUncachable));
        bufp->chgCData(oldp+1321,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__retiredStoreQueuePtr),4);
        bufp->chgBit(oldp+1322,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatable));
        bufp->chgCData(oldp+1323,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreQueuePtr),4);
        bufp->chgBit(oldp+1324,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__busyInRecovery));
        bufp->chgBit(oldp+1325,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteReq));
        bufp->chgBit(oldp+1326,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteAddr 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1327,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteAddr 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1328,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteAddr)),20);
        bufp->chgQData(oldp+1329,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteData),64);
        bufp->chgCData(oldp+1331,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteByteWE),8);
        bufp->chgBit(oldp+1332,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteUncachable));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x2aU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x73U])))) {
        bufp->chgBit(oldp+1333,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatedPhyRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1334,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatedPhyRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+1335,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatedPhyRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1336,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__allocatedPhyRegNum
                                   [1U])),6);
        bufp->chgSData(oldp+1337,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                     [0U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1338,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                 [0U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1339,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1340,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                       [0U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1341,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1342,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                    [0U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1343,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1344,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1345,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1346,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1347,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1348,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1349,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1350,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1351,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                    [0U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1352,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [0U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1353,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                    [0U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1354,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                  [0U]))),4);
        bufp->chgSData(oldp+1355,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                     [1U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+1356,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                 [1U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+1357,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+1358,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                       [1U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+1359,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+1360,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                    [1U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+1361,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+1362,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+1363,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+1364,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+1365,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+1366,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+1367,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+1368,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+1369,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                    [1U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+1370,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                               [1U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+1371,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                    [1U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+1372,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__alReadData
                                                  [1U]))),4);
        bufp->chgBit(oldp+1373,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__inRecoveryRMT));
        bufp->chgCData(oldp+1374,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg),2);
        bufp->chgBit(oldp+1375,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_PhyRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1376,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_PhyRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+1377,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_PhyRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1378,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_PhyRegNum
                                   [1U])),6);
        bufp->chgBit(oldp+1379,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_LogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+1380,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_LogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+1381,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_LogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+1382,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtWriteReg_LogRegNum
                                   [1U])),5);
        bufp->chgBit(oldp+1383,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteReg[0]));
        bufp->chgBit(oldp+1384,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteReg[1]));
        bufp->chgBit(oldp+1385,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteLogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+1386,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteLogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+1387,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteLogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+1388,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteLogRegNum
                                   [1U])),5);
        bufp->chgCData(oldp+1389,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteIssueQueuePtr[0]),4);
        bufp->chgCData(oldp+1390,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__watWriteIssueQueuePtr[1]),4);
        bufp->chgIData(oldp+1391,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__unnamedblk10__DOT__i),32);
        bufp->chgIData(oldp+1392,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__unnamedblk11__DOT__i),32);
        bufp->chgIData(oldp+1393,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__unnamedblk12__DOT__i),32);
        bufp->chgIData(oldp+1394,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__unnamedblk13__DOT__i),32);
        bufp->chgBit(oldp+1395,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegA
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1396,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegA
                                   [0U])),6);
        bufp->chgBit(oldp+1397,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegA
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1398,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegA
                                   [1U])),6);
        bufp->chgBit(oldp+1399,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegB
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1400,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegB
                                   [0U])),6);
        bufp->chgBit(oldp+1401,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegB
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1402,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegB
                                   [1U])),6);
        bufp->chgBit(oldp+1403,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegC
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1404,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegC
                                   [0U])),6);
        bufp->chgBit(oldp+1405,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegC
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1406,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phySrcRegC
                                   [1U])),6);
        bufp->chgBit(oldp+1407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phyPrevDstReg
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1408,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phyPrevDstReg
                                   [0U])),6);
        bufp->chgBit(oldp+1409,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phyPrevDstReg
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1410,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__phyPrevDstReg
                                   [1U])),6);
        bufp->chgCData(oldp+1411,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__srcIssueQueuePtrRegA[0]),4);
        bufp->chgCData(oldp+1412,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__srcIssueQueuePtrRegA[1]),4);
        bufp->chgCData(oldp+1413,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__srcIssueQueuePtrRegB[0]),4);
        bufp->chgCData(oldp+1414,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__srcIssueQueuePtrRegB[1]),4);
        bufp->chgCData(oldp+1415,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__srcIssueQueuePtrRegC[0]),4);
        bufp->chgCData(oldp+1416,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__srcIssueQueuePtrRegC[1]),4);
        bufp->chgBit(oldp+1417,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWE[0]));
        bufp->chgBit(oldp+1418,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWE[1]));
        bufp->chgBit(oldp+1419,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWA
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+1420,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWA
                                   [0U])),5);
        bufp->chgBit(oldp+1421,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWA
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+1422,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWA
                                   [1U])),5);
        bufp->chgCData(oldp+1423,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWV
                                            [0U] >> 4U))),6);
        bufp->chgCData(oldp+1424,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWV
                                   [0U])),4);
        bufp->chgCData(oldp+1425,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWV
                                            [1U] >> 4U))),6);
        bufp->chgCData(oldp+1426,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rmtWV
                                   [1U])),4);
        bufp->chgBit(oldp+1427,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1428,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA
                                   [0U])),6);
        bufp->chgBit(oldp+1429,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1430,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegA
                                   [1U])),6);
        bufp->chgBit(oldp+1431,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1432,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                                   [0U])),6);
        bufp->chgBit(oldp+1433,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1434,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegB
                                   [1U])),6);
        bufp->chgBit(oldp+1435,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1436,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC
                                   [0U])),6);
        bufp->chgBit(oldp+1437,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1438,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phySrcRegC
                                   [1U])),6);
        bufp->chgBit(oldp+1439,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1440,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg
                                   [0U])),6);
        bufp->chgBit(oldp+1441,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1442,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyDstReg
                                   [1U])),6);
        bufp->chgBit(oldp+1443,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1444,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg
                                   [0U])),6);
        bufp->chgBit(oldp+1445,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1446,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyPrevDstReg
                                   [1U])),6);
        bufp->chgBit(oldp+1447,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__allocatable));
        bufp->chgCData(oldp+1448,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg),2);
        bufp->chgBit(oldp+1449,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1450,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+1451,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1452,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_PhyRegNum
                                   [1U])),6);
        bufp->chgBit(oldp+1453,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+1454,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+1455,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+1456,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__rmtWriteReg_LogRegNum
                                   [1U])),5);
        bufp->chgCData(oldp+1457,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegA[0]),4);
        bufp->chgCData(oldp+1458,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegA[1]),4);
        bufp->chgCData(oldp+1459,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB[0]),4);
        bufp->chgCData(oldp+1460,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegB[1]),4);
        bufp->chgCData(oldp+1461,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegC[0]),4);
        bufp->chgCData(oldp+1462,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__srcIssueQueuePtrRegC[1]),4);
        bufp->chgCData(oldp+1463,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__prevDependIssueQueuePtr[0]),4);
        bufp->chgCData(oldp+1464,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__prevDependIssueQueuePtr[1]),4);
        bufp->chgBit(oldp+1465,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteReg[0]));
        bufp->chgBit(oldp+1466,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteReg[1]));
        bufp->chgBit(oldp+1467,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteLogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+1468,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteLogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+1469,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteLogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+1470,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteLogRegNum
                                   [1U])),5);
        bufp->chgCData(oldp+1471,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr[0]),4);
        bufp->chgCData(oldp+1472,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtr[1]),4);
        bufp->chgBit(oldp+1473,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__renameLogicRecoveryRMT));
        bufp->chgBit(oldp+1474,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__we[0]));
        bufp->chgBit(oldp+1475,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__we[1]));
        bufp->chgCData(oldp+1476,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wa[0]),6);
        bufp->chgCData(oldp+1477,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wa[1]),6);
        bufp->chgSData(oldp+1478,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wv[0]),10);
        bufp->chgSData(oldp+1479,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT.__PVT__wv[1]),10);
        bufp->chgBit(oldp+1480,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__we[0]));
        bufp->chgBit(oldp+1481,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__we[1]));
        bufp->chgCData(oldp+1482,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wa[0]),6);
        bufp->chgCData(oldp+1483,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wa[1]),6);
        bufp->chgSData(oldp+1484,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wv[0]),10);
        bufp->chgSData(oldp+1485,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wv[1]),10);
        bufp->chgBit(oldp+1486,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__we
                                [0U]));
        bufp->chgCData(oldp+1487,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wa
                                  [0U]),6);
        bufp->chgSData(oldp+1488,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wv
                                  [0U]),10);
        bufp->chgBit(oldp+1489,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__we
                                [1U]));
        bufp->chgCData(oldp+1490,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wa
                                  [1U]),6);
        bufp->chgSData(oldp+1491,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__wv
                                  [1U]),10);
        bufp->chgBit(oldp+1492,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[0]));
        bufp->chgBit(oldp+1493,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue[1]));
        bufp->chgCData(oldp+1494,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[0]),6);
        bufp->chgCData(oldp+1495,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr[1]),6);
        bufp->chgBit(oldp+1496,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [0U][0U]));
        bufp->chgBit(oldp+1497,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [0U][1U]));
        bufp->chgBit(oldp+1498,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [1U][0U]));
        bufp->chgBit(oldp+1499,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadValue
                                [1U][1U]));
        bufp->chgBit(oldp+1500,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                [0U]));
        bufp->chgBit(oldp+1501,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rwbWriteValue
                                [1U]));
        bufp->chgCData(oldp+1502,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [1U]),6);
        bufp->chgCData(oldp+1503,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__wbReadAddr
                                  [0U]),6);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x2cU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x74U])))) {
        bufp->chgQData(oldp+1504,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__storedLineData),64);
        bufp->chgCData(oldp+1506,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__storedLineByteWE),8);
        bufp->chgCData(oldp+1507,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                             [0U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                               [0U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1508,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [0U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1509,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                             [0U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1510,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [0U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1511,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [0U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1512,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [0U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1513,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                                    [0U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                                                  [0U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                                                    [0U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1515,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                            [0U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1516,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+1517,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+1518,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+1519,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                 [0U][0U])));
        bufp->chgCData(oldp+1520,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                             [1U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                               [1U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1521,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [1U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1522,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                             [1U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1523,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [1U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1524,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [1U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1525,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [1U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1526,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                                    [1U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                                                  [1U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                                                    [1U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1528,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                            [1U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1529,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [1U][0U] >> 3U))));
        bufp->chgBit(oldp+1530,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+1531,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+1532,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxIn
                                 [1U][0U])));
        __Vtemp_1[0U] = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__storedLineData);
        __Vtemp_1[1U] = (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__storedLineData 
                                 >> 0x20U));
        __Vtemp_1[2U] = 0U;
        __Vtemp_1[3U] = 0U;
        bufp->chgWData(oldp+1533,(__Vtemp_1),128);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x2eU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x75U])))) {
        bufp->chgIData(oldp+1537,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessAddr),32);
        __Vtemp_2[0U] = (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWriteData);
        __Vtemp_2[1U] = (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWriteData 
                                 >> 0x20U));
        __Vtemp_2[2U] = 0U;
        __Vtemp_2[3U] = 0U;
        bufp->chgWData(oldp+1538,(__Vtemp_2),128);
        bufp->chgBit(oldp+1542,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessRE));
        bufp->chgBit(oldp+1543,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWE));
        bufp->chgBit(oldp+1544,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessAddr 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessAddr 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1546,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessAddr)),20);
        bufp->chgQData(oldp+1547,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memAccessWriteData),64);
        bufp->chgBit(oldp+1549,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__memMux__DOT__portIn));
        bufp->chgBit(oldp+1550,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][5U] >> 3U))));
        bufp->chgCData(oldp+1551,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                             [0U][5U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                               [0U][4U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+1552,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1553,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][4U] >> 0x1cU))));
        bufp->chgBit(oldp+1554,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+1555,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+1556,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][4U] >> 0x19U))));
        bufp->chgIData(oldp+1557,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                               [0U][4U] 
                                               >> 5U))),20);
        bufp->chgSData(oldp+1558,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                             [0U][3U] 
                                             >> 0xfU))),11);
        bufp->chgSData(oldp+1559,((0x7ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                              [0U][4U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                [0U][3U] 
                                                >> 0x1aU)))),11);
        bufp->chgCData(oldp+1560,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                         [0U][3U] >> 0xdU))),2);
        bufp->chgBit(oldp+1561,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][3U] >> 0xcU))));
        bufp->chgBit(oldp+1562,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][3U] >> 0xbU))));
        bufp->chgBit(oldp+1563,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][3U] >> 0xaU))));
        bufp->chgIData(oldp+1564,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                [0U][3U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                  [0U][2U] 
                                                  >> 0x16U)))),20);
        bufp->chgCData(oldp+1565,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                         [0U][2U] >> 0x14U))),2);
        bufp->chgBit(oldp+1566,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][2U] >> 0x13U))));
        bufp->chgQData(oldp+1567,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                    [0U][2U])) 
                                    << 0x2dU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                                  [0U][1U])) 
                                                  << 0xdU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                                    [0U][0U])) 
                                                    >> 0x13U)))),64);
        bufp->chgBit(oldp+1569,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][0U] >> 0x12U))));
        bufp->chgBit(oldp+1570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][0U] >> 0x11U))));
        bufp->chgBit(oldp+1571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+1572,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+1573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [0U][0U] >> 0xeU))));
        bufp->chgCData(oldp+1574,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                            [0U][0U] 
                                            >> 6U))),8);
        bufp->chgCData(oldp+1575,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                   [0U][0U])),6);
        bufp->chgBit(oldp+1576,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][5U] >> 3U))));
        bufp->chgCData(oldp+1577,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                             [1U][5U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                               [1U][4U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+1578,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1579,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][4U] >> 0x1cU))));
        bufp->chgBit(oldp+1580,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+1581,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+1582,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][4U] >> 0x19U))));
        bufp->chgIData(oldp+1583,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                               [1U][4U] 
                                               >> 5U))),20);
        bufp->chgSData(oldp+1584,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                             [1U][3U] 
                                             >> 0xfU))),11);
        bufp->chgSData(oldp+1585,((0x7ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                              [1U][4U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                [1U][3U] 
                                                >> 0x1aU)))),11);
        bufp->chgCData(oldp+1586,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                         [1U][3U] >> 0xdU))),2);
        bufp->chgBit(oldp+1587,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][3U] >> 0xcU))));
        bufp->chgBit(oldp+1588,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][3U] >> 0xbU))));
        bufp->chgBit(oldp+1589,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][3U] >> 0xaU))));
        bufp->chgIData(oldp+1590,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                [1U][3U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                  [1U][2U] 
                                                  >> 0x16U)))),20);
        bufp->chgCData(oldp+1591,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                         [1U][2U] >> 0x14U))),2);
        bufp->chgBit(oldp+1592,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][2U] >> 0x13U))));
        bufp->chgQData(oldp+1593,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                    [1U][2U])) 
                                    << 0x2dU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                                  [1U][1U])) 
                                                  << 0xdU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                                                    [1U][0U])) 
                                                    >> 0x13U)))),64);
        bufp->chgBit(oldp+1595,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][0U] >> 0x12U))));
        bufp->chgBit(oldp+1596,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][0U] >> 0x11U))));
        bufp->chgBit(oldp+1597,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][0U] >> 0x10U))));
        bufp->chgBit(oldp+1598,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][0U] >> 0xfU))));
        bufp->chgBit(oldp+1599,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                       [1U][0U] >> 0xeU))));
        bufp->chgCData(oldp+1600,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                            [1U][0U] 
                                            >> 6U))),8);
        bufp->chgCData(oldp+1601,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__nextMSHR
                                   [1U][0U])),6);
        bufp->chgBit(oldp+1602,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Entry[0]));
        bufp->chgBit(oldp+1603,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Entry[1]));
        bufp->chgBit(oldp+1604,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation[0]));
        bufp->chgBit(oldp+1605,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation[1]));
        bufp->chgBit(oldp+1606,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore[0]));
        bufp->chgBit(oldp+1607,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore[1]));
        bufp->chgQData(oldp+1608,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__mergedLine[0]),64);
        bufp->chgQData(oldp+1610,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__mergedLine[1]),64);
        bufp->chgBit(oldp+1612,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete[0]));
        bufp->chgBit(oldp+1613,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete[1]));
        bufp->chgIData(oldp+1614,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+1615,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__unnamedblk5__DOT__i),32);
        bufp->chgCData(oldp+1616,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextSerial),2);
        bufp->chgBit(oldp+1617,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__icAck));
        bufp->chgBit(oldp+1618,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__dcAck));
        bufp->chgCData(oldp+1619,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__nextReqSerial),2);
        bufp->chgCData(oldp+1620,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextNextMemReadSerial),2);
        bufp->chgBit(oldp+1621,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__nextNextMemWriteSerial));
        bufp->chgBit(oldp+1622,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReadAccessAck));
        bufp->chgBit(oldp+1623,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memWriteAccessAck));
        bufp->chgBit(oldp+1624,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushRequestQueue));
        bufp->chgBit(oldp+1625,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[3U] 
                                       >> 5U))));
        bufp->chgBit(oldp+1626,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[3U] 
                                       >> 4U))));
        bufp->chgIData(oldp+1627,(((vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[3U] 
                                    << 0x1cU) | (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[2U] 
                                                 >> 4U))),32);
        bufp->chgQData(oldp+1628,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[0U])) 
                                                    >> 4U)))),64);
        bufp->chgCData(oldp+1630,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[0U] 
                                         >> 2U))),2);
        bufp->chgBit(oldp+1631,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[0U] 
                                       >> 1U))));
        bufp->chgBit(oldp+1632,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__pushedData[0U])));
        bufp->chgCData(oldp+1633,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__nextTailStorage),7);
        bufp->chgCData(oldp+1634,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__nextCount),8);
        bufp->chgBit(oldp+1635,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReqAck) 
                                       >> 3U))));
        bufp->chgCData(oldp+1636,((3U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReqAck) 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1637,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessReqAck))));
        bufp->chgBit(oldp+1638,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1639,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
                                       >> 0x16U))));
        bufp->chgBit(oldp+1640,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1641,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U] 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1642,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[2U])),20);
        bufp->chgQData(oldp+1643,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReq[0U])))),64);
        bufp->chgBit(oldp+1645,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck) 
                                       >> 3U))));
        bufp->chgCData(oldp+1646,((3U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck) 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1647,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcMemAccessReqAck))));
        bufp->chgBit(oldp+1648,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[0]));
        bufp->chgBit(oldp+1649,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheReq[1]));
        bufp->chgCData(oldp+1650,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                             [0U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                               [0U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1651,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [0U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1652,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                             [0U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [0U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1654,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [0U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1655,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [0U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1656,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                                    [0U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                                                  [0U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                                                    [0U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1658,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                            [0U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1659,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+1660,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+1661,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+1662,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                 [0U][0U])));
        bufp->chgCData(oldp+1663,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                             [1U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                               [1U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1664,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [1U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1665,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                             [1U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1666,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [1U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1667,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [1U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1668,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [1U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1669,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                                    [1U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                                                  [1U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                                                    [1U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1671,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                            [1U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1672,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [1U][0U] >> 3U))));
        bufp->chgBit(oldp+1673,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+1674,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+1675,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxIn
                                 [1U][0U])));
        bufp->chgBit(oldp+1676,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[0]));
        bufp->chgBit(oldp+1677,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq[1]));
        bufp->chgBit(oldp+1678,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                       [0U][2U] >> 0x16U))));
        bufp->chgBit(oldp+1679,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                       [0U][2U] >> 0x15U))));
        bufp->chgIData(oldp+1680,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                               [0U][2U] 
                                               >> 1U))),20);
        bufp->chgQData(oldp+1681,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                                    [0U][2U])) 
                                    << 0x3fU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                                                  [0U][1U])) 
                                                  << 0x1fU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                                                    [0U][0U])) 
                                                    >> 1U)))),64);
        bufp->chgBit(oldp+1683,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                 [0U][0U])));
        bufp->chgBit(oldp+1684,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                       [1U][2U] >> 0x16U))));
        bufp->chgBit(oldp+1685,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                       [1U][2U] >> 0x15U))));
        bufp->chgIData(oldp+1686,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                               [1U][2U] 
                                               >> 1U))),20);
        bufp->chgQData(oldp+1687,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                                    [1U][2U])) 
                                    << 0x3fU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                                                  [1U][1U])) 
                                                  << 0x1fU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                                                    [1U][0U])) 
                                                    >> 1U)))),64);
        bufp->chgBit(oldp+1689,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxIn
                                 [1U][0U])));
        bufp->chgBit(oldp+1690,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                       [0U] >> 3U))));
        bufp->chgCData(oldp+1691,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                         [0U] >> 1U))),2);
        bufp->chgBit(oldp+1692,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                 [0U])));
        bufp->chgBit(oldp+1693,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                       [1U] >> 3U))));
        bufp->chgCData(oldp+1694,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                         [1U] >> 1U))),2);
        bufp->chgBit(oldp+1695,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemMuxOut
                                 [1U])));
        bufp->chgBit(oldp+1696,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memAddr 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memAddr 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1698,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memAddr)),20);
        bufp->chgQData(oldp+1699,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memData),64);
        bufp->chgBit(oldp+1701,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memWE));
        bufp->chgBit(oldp+1702,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete
                                [0U]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x2fU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x76U])))) {
        bufp->chgBit(oldp+1703,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__cacheFlushManager__DOT__icFlushReq));
        bufp->chgCData(oldp+1704,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__nextPhase),2);
        bufp->chgBit(oldp+1705,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__dcFlushReqAck));
        bufp->chgBit(oldp+1706,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__mshrBusy));
        bufp->chgBit(oldp+1707,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextFlushStart));
        bufp->chgBit(oldp+1708,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__nextFlush));
        bufp->chgBit(oldp+1709,(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icFlushReq));
        bufp->chgBit(oldp+1710,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dcFlushReqAck));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x30U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x77U])))) {
        bufp->chgBit(oldp+1711,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__hit[0]));
        bufp->chgBit(oldp+1712,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__hit[1]));
        bufp->chgBit(oldp+1713,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missReq[0]));
        bufp->chgBit(oldp+1714,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__missReq[1]));
        bufp->chgBit(oldp+1715,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuLoadHasAllocatedMSHR[0]));
        bufp->chgBit(oldp+1716,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuLoadMSHRID[0]));
        bufp->chgBit(oldp+1717,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuStoreHasAllocatedMSHR[0]));
        bufp->chgBit(oldp+1718,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuStoreMSHRID[0]));
        bufp->chgBit(oldp+1719,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuMSHRAddrHit[0]));
        bufp->chgBit(oldp+1720,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuMSHRAddrHitMSHRID[0]));
        bufp->chgBit(oldp+1721,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuMSHRReadHit[0]));
        bufp->chgQData(oldp+1722,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__lsuMSHRReadData[0]),64);
        bufp->chgBit(oldp+1724,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__mshrConflict[0]));
        bufp->chgBit(oldp+1725,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__mshrConflict[1]));
        bufp->chgBit(oldp+1726,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR[0]));
        bufp->chgBit(oldp+1727,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR[1]));
        bufp->chgBit(oldp+1728,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_Addr
                                       [0U] >> 0x15U))));
        bufp->chgBit(oldp+1729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_Addr
                                       [0U] >> 0x14U))));
        bufp->chgIData(oldp+1730,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_Addr
                                   [0U])),20);
        bufp->chgBit(oldp+1731,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_Addr
                                       [1U] >> 0x15U))));
        bufp->chgBit(oldp+1732,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_Addr
                                       [1U] >> 0x14U))));
        bufp->chgIData(oldp+1733,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_Addr
                                   [1U])),20);
        bufp->chgCData(oldp+1734,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_ActiveListPtr[0]),6);
        bufp->chgCData(oldp+1735,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portInitMSHR_ActiveListPtr[1]),6);
        bufp->chgBit(oldp+1736,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portIsAllocatedByStore[0]));
        bufp->chgBit(oldp+1737,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portIsAllocatedByStore[1]));
        bufp->chgBit(oldp+1738,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portIsUncachable[0]));
        bufp->chgBit(oldp+1739,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__portIsUncachable[1]));
        bufp->chgBit(oldp+1740,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayWE
                                [0U][0U]));
        bufp->chgBit(oldp+1741,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayWE
                                [0U][1U]));
        bufp->chgBit(oldp+1742,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayWE
                                [1U][0U]));
        bufp->chgBit(oldp+1743,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayWE
                                [1U][1U]));
        bufp->chgBit(oldp+1744,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [0U][0U][0U]));
        bufp->chgBit(oldp+1745,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [0U][0U][1U]));
        bufp->chgBit(oldp+1746,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [0U][1U][0U]));
        bufp->chgBit(oldp+1747,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [0U][1U][1U]));
        bufp->chgBit(oldp+1748,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [1U][0U][0U]));
        bufp->chgBit(oldp+1749,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [1U][0U][1U]));
        bufp->chgBit(oldp+1750,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [1U][1U][0U]));
        bufp->chgBit(oldp+1751,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [1U][1U][1U]));
        bufp->chgBit(oldp+1752,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [2U][0U][0U]));
        bufp->chgBit(oldp+1753,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [2U][0U][1U]));
        bufp->chgBit(oldp+1754,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [2U][1U][0U]));
        bufp->chgBit(oldp+1755,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [2U][1U][1U]));
        bufp->chgBit(oldp+1756,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [3U][0U][0U]));
        bufp->chgBit(oldp+1757,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [3U][0U][1U]));
        bufp->chgBit(oldp+1758,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [3U][1U][0U]));
        bufp->chgBit(oldp+1759,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [3U][1U][1U]));
        bufp->chgBit(oldp+1760,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [4U][0U][0U]));
        bufp->chgBit(oldp+1761,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [4U][0U][1U]));
        bufp->chgBit(oldp+1762,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [4U][1U][0U]));
        bufp->chgBit(oldp+1763,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [4U][1U][1U]));
        bufp->chgBit(oldp+1764,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [5U][0U][0U]));
        bufp->chgBit(oldp+1765,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [5U][0U][1U]));
        bufp->chgBit(oldp+1766,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [5U][1U][0U]));
        bufp->chgBit(oldp+1767,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [5U][1U][1U]));
        bufp->chgBit(oldp+1768,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [6U][0U][0U]));
        bufp->chgBit(oldp+1769,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [6U][0U][1U]));
        bufp->chgBit(oldp+1770,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [6U][1U][0U]));
        bufp->chgBit(oldp+1771,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [6U][1U][1U]));
        bufp->chgBit(oldp+1772,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [7U][0U][0U]));
        bufp->chgBit(oldp+1773,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [7U][0U][1U]));
        bufp->chgBit(oldp+1774,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [7U][1U][0U]));
        bufp->chgBit(oldp+1775,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE
                                [7U][1U][1U]));
        bufp->chgCData(oldp+1776,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIndex[0]),8);
        bufp->chgCData(oldp+1777,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIndex[1]),8);
        bufp->chgCData(oldp+1778,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [0U][0U]),8);
        bufp->chgCData(oldp+1779,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [0U][1U]),8);
        bufp->chgCData(oldp+1780,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [1U][0U]),8);
        bufp->chgCData(oldp+1781,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [1U][1U]),8);
        bufp->chgCData(oldp+1782,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [2U][0U]),8);
        bufp->chgCData(oldp+1783,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [2U][1U]),8);
        bufp->chgCData(oldp+1784,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [3U][0U]),8);
        bufp->chgCData(oldp+1785,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [3U][1U]),8);
        bufp->chgCData(oldp+1786,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [4U][0U]),8);
        bufp->chgCData(oldp+1787,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [4U][1U]),8);
        bufp->chgCData(oldp+1788,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [5U][0U]),8);
        bufp->chgCData(oldp+1789,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [5U][1U]),8);
        bufp->chgCData(oldp+1790,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [6U][0U]),8);
        bufp->chgCData(oldp+1791,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [6U][1U]),8);
        bufp->chgCData(oldp+1792,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [7U][0U]),8);
        bufp->chgCData(oldp+1793,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayIn
                                  [7U][1U]),8);
        bufp->chgBit(oldp+1794,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDirtyIn[0]));
        bufp->chgBit(oldp+1795,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayDirtyIn[1]));
        bufp->chgCData(oldp+1796,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE_Tmp[0]),8);
        bufp->chgCData(oldp+1797,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayByteWE_Tmp[1]),8);
        bufp->chgQData(oldp+1798,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayInTmp[0]),64);
        bufp->chgQData(oldp+1800,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__dataArrayInTmp[1]),64);
        bufp->chgBit(oldp+1802,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayWE
                                [0U][0U]));
        bufp->chgBit(oldp+1803,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayWE
                                [0U][1U]));
        bufp->chgBit(oldp+1804,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayWE
                                [1U][0U]));
        bufp->chgBit(oldp+1805,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayWE
                                [1U][1U]));
        bufp->chgCData(oldp+1806,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayIndex[0]),8);
        bufp->chgCData(oldp+1807,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayIndex[1]),8);
        bufp->chgBit(oldp+1808,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayIn
                                       [0U] >> 0xbU))));
        bufp->chgSData(oldp+1809,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayIn
                                   [0U])),11);
        bufp->chgBit(oldp+1810,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayIn
                                       [1U] >> 0xbU))));
        bufp->chgSData(oldp+1811,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__tagArrayIn
                                   [1U])),11);
        bufp->chgBit(oldp+1812,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayWE
                                [0U][0U]));
        bufp->chgBit(oldp+1813,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayWE
                                [0U][1U]));
        bufp->chgBit(oldp+1814,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayWE_Flat[0]));
        bufp->chgBit(oldp+1815,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayWE_Flat[1]));
        bufp->chgCData(oldp+1816,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayIndex[0]),8);
        bufp->chgCData(oldp+1817,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayIndex[1]),8);
        bufp->chgBit(oldp+1818,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayIn
                                [0U][0U]));
        bufp->chgBit(oldp+1819,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayIn
                                [0U][1U]));
        bufp->chgBit(oldp+1820,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayInFlat[0]));
        bufp->chgBit(oldp+1821,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__array__DOT__replArrayInFlat[1]));
        bufp->chgBit(oldp+1822,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__dirtyArray__we[0]));
        bufp->chgBit(oldp+1823,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__dirtyArray__we[1]));
        bufp->chgBit(oldp+1824,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__dirtyArray__wv[0]));
        bufp->chgBit(oldp+1825,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__dirtyArray__wv[1]));
        bufp->chgBit(oldp+1826,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1827,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1828,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1829,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1830,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1831,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1832,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1833,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1834,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1835,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1836,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1837,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1838,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1839,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1840,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1841,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1842,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1843,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1844,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1845,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1846,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1847,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1848,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1849,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1850,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1851,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1852,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1853,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1854,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1855,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1856,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1857,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1858,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__tagArray__we[0]));
        bufp->chgBit(oldp+1859,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__tagArray__we[1]));
        bufp->chgSData(oldp+1860,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__tagArray__wv[0]),12);
        bufp->chgSData(oldp+1861,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__tagArray__wv[1]),12);
        bufp->chgBit(oldp+1862,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__dirtyArray__we[0]));
        bufp->chgBit(oldp+1863,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__dirtyArray__we[1]));
        bufp->chgBit(oldp+1864,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__dirtyArray__wv[0]));
        bufp->chgBit(oldp+1865,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__dirtyArray__wv[1]));
        bufp->chgBit(oldp+1866,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1867,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1868,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1869,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1870,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1871,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1872,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1873,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1874,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1875,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1876,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1877,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1878,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1879,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1880,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1881,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1882,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1883,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1884,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1885,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1886,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1887,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1888,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1889,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1890,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1891,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1892,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1893,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1894,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__we[0]));
        bufp->chgBit(oldp+1895,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__we[1]));
        bufp->chgCData(oldp+1896,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__wv[0]),8);
        bufp->chgCData(oldp+1897,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__wv[1]),8);
        bufp->chgBit(oldp+1898,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__tagArray__we[0]));
        bufp->chgBit(oldp+1899,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__tagArray__we[1]));
        bufp->chgSData(oldp+1900,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__tagArray__wv[0]),12);
        bufp->chgSData(oldp+1901,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__tagArray__wv[1]),12);
        bufp->chgBit(oldp+1902,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk2__BRA__0__KET____DOT__replArray__we[0]));
        bufp->chgBit(oldp+1903,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk2__BRA__0__KET____DOT__replArray__we[1]));
        bufp->chgBit(oldp+1904,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk2__BRA__0__KET____DOT__replArray__wv[0]));
        bufp->chgBit(oldp+1905,(vlSymsp->TOP__SMT_RTL_Testbench__core.dCache__DOT__array__DOT____Vcellinp__genblk2__BRA__0__KET____DOT__replArray__wv[1]));
        bufp->chgCData(oldp+1906,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portIn),2);
        bufp->chgCData(oldp+1907,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [0U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                               [0U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1908,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [0U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1909,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [0U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1910,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [0U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1911,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [0U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1912,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [0U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1913,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                    [0U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                  [0U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                    [0U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1915,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                            [0U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1916,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+1917,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+1918,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+1919,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                 [0U][0U])));
        bufp->chgCData(oldp+1920,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [1U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                               [1U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1921,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [1U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1922,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [1U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1923,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [1U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1924,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [1U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1925,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [1U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1926,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                    [1U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                  [1U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                    [1U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1928,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                            [1U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1929,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [1U][0U] >> 3U))));
        bufp->chgBit(oldp+1930,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+1931,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+1932,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                 [1U][0U])));
        bufp->chgCData(oldp+1933,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [2U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                               [2U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1934,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [2U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1935,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [2U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1936,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [2U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1937,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [2U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1938,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [2U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1939,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                    [2U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                  [2U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                    [2U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1941,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                            [2U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1942,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [2U][0U] >> 3U))));
        bufp->chgBit(oldp+1943,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [2U][0U] >> 2U))));
        bufp->chgBit(oldp+1944,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [2U][0U] >> 1U))));
        bufp->chgBit(oldp+1945,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                 [2U][0U])));
        bufp->chgCData(oldp+1946,((0xffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [3U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                               [3U][2U] 
                                               >> 0x1bU)))),8);
        bufp->chgBit(oldp+1947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [3U][2U] >> 0x1aU))));
        bufp->chgSData(oldp+1948,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                             [3U][2U] 
                                             >> 0xfU))),11);
        bufp->chgBit(oldp+1949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [3U][2U] >> 0xeU))));
        bufp->chgBit(oldp+1950,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [3U][2U] >> 0xdU))));
        bufp->chgBit(oldp+1951,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [3U][2U] >> 0xcU))));
        bufp->chgQData(oldp+1952,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                    [3U][2U])) 
                                    << 0x34U) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                  [3U][1U])) 
                                                  << 0x14U) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                                                    [3U][0U])) 
                                                    >> 0xcU)))),64);
        bufp->chgCData(oldp+1954,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                            [3U][0U] 
                                            >> 4U))),8);
        bufp->chgBit(oldp+1955,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [3U][0U] >> 3U))));
        bufp->chgBit(oldp+1956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [3U][0U] >> 2U))));
        bufp->chgBit(oldp+1957,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                       [3U][0U] >> 1U))));
        bufp->chgBit(oldp+1958,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxIn
                                 [3U][0U])));
        bufp->chgSData(oldp+1959,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [0U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+1960,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [0U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+1961,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                         [0U][2U] >> 5U))),2);
        bufp->chgBit(oldp+1962,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [0U][2U] >> 4U))));
        bufp->chgBit(oldp+1963,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [0U][2U] >> 3U))));
        bufp->chgBit(oldp+1964,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [0U][2U] >> 2U))));
        bufp->chgBit(oldp+1965,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+1966,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                 [0U][2U])));
        bufp->chgQData(oldp+1967,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                                [0U][0U])))),64);
        bufp->chgSData(oldp+1969,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [1U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+1970,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [1U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+1971,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                         [1U][2U] >> 5U))),2);
        bufp->chgBit(oldp+1972,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [1U][2U] >> 4U))));
        bufp->chgBit(oldp+1973,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [1U][2U] >> 3U))));
        bufp->chgBit(oldp+1974,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [1U][2U] >> 2U))));
        bufp->chgBit(oldp+1975,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [1U][2U] >> 1U))));
        bufp->chgBit(oldp+1976,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                 [1U][2U])));
        bufp->chgQData(oldp+1977,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                                [1U][0U])))),64);
        bufp->chgSData(oldp+1979,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [2U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+1980,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [2U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+1981,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                         [2U][2U] >> 5U))),2);
        bufp->chgBit(oldp+1982,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [2U][2U] >> 4U))));
        bufp->chgBit(oldp+1983,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [2U][2U] >> 3U))));
        bufp->chgBit(oldp+1984,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [2U][2U] >> 2U))));
        bufp->chgBit(oldp+1985,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [2U][2U] >> 1U))));
        bufp->chgBit(oldp+1986,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                 [2U][2U])));
        bufp->chgQData(oldp+1987,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                    [2U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                                [2U][0U])))),64);
        bufp->chgSData(oldp+1989,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [3U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+1990,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                             [3U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+1991,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                         [3U][2U] >> 5U))),2);
        bufp->chgBit(oldp+1992,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [3U][2U] >> 4U))));
        bufp->chgBit(oldp+1993,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [3U][2U] >> 3U))));
        bufp->chgBit(oldp+1994,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [3U][2U] >> 2U))));
        bufp->chgBit(oldp+1995,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                       [3U][2U] >> 1U))));
        bufp->chgBit(oldp+1996,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                 [3U][2U])));
        bufp->chgQData(oldp+1997,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                    [3U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxTagOut
                                                                [3U][0U])))),64);
        bufp->chgQData(oldp+1999,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                    [0U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                  [0U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                    [0U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2001,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2002,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                 [0U][0U])));
        bufp->chgQData(oldp+2003,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                    [1U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                  [1U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                    [1U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2005,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+2006,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                 [1U][0U])));
        bufp->chgQData(oldp+2007,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                    [2U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                  [2U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                    [2U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2009,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                       [2U][0U] >> 1U))));
        bufp->chgBit(oldp+2010,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                 [2U][0U])));
        bufp->chgQData(oldp+2011,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                    [3U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                  [3U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                                                    [3U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2013,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                       [3U][0U] >> 1U))));
        bufp->chgBit(oldp+2014,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__muxDataOut
                                 [3U][0U])));
        bufp->chgBit(oldp+2015,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagHit
                                [0U][0U]));
        bufp->chgBit(oldp+2016,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagHit
                                [0U][1U]));
        bufp->chgBit(oldp+2017,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagHit
                                [1U][0U]));
        bufp->chgBit(oldp+2018,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagHit
                                [1U][1U]));
        bufp->chgBit(oldp+2019,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrConflict[0]));
        bufp->chgBit(oldp+2020,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrConflict[1]));
        bufp->chgBit(oldp+2021,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrAddrHit[0]));
        bufp->chgBit(oldp+2022,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrAddrHit[1]));
        bufp->chgBit(oldp+2023,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrAddrHitMSHRID[0]));
        bufp->chgBit(oldp+2024,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrAddrHitMSHRID[1]));
        bufp->chgBit(oldp+2025,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrReadHit[0]));
        bufp->chgBit(oldp+2026,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrReadHit[1]));
        bufp->chgQData(oldp+2027,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrReadData[0]),64);
        bufp->chgQData(oldp+2029,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__mshrReadData[1]),64);
        bufp->chgQData(oldp+2031,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portMSHRData[0]),64);
        bufp->chgQData(oldp+2033,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__portMSHRData[1]),64);
        bufp->chgBit(oldp+2035,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__repIsHit[0]));
        bufp->chgBit(oldp+2036,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__repIsHit[1]));
        bufp->chgBit(oldp+2037,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__repHitWay[0]));
        bufp->chgBit(oldp+2038,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__repHitWay[1]));
        bufp->chgSData(oldp+2039,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayDataOutTmp
                                  [0U][0U]),11);
        bufp->chgSData(oldp+2040,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayDataOutTmp
                                  [0U][1U]),11);
        bufp->chgSData(oldp+2041,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayDataOutTmp
                                  [1U][0U]),11);
        bufp->chgSData(oldp+2042,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayDataOutTmp
                                  [1U][1U]),11);
        bufp->chgBit(oldp+2043,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayValidOutTmp
                                [0U][0U]));
        bufp->chgBit(oldp+2044,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayValidOutTmp
                                [0U][1U]));
        bufp->chgBit(oldp+2045,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayValidOutTmp
                                [1U][0U]));
        bufp->chgBit(oldp+2046,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__tagArrayValidOutTmp
                                [1U][1U]));
        bufp->chgBit(oldp+2047,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__dataArrayDirtyOutTmp[0]));
        bufp->chgBit(oldp+2048,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__dataArrayDirtyOutTmp[1]));
        bufp->chgQData(oldp+2049,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__dataArrayDataOutTmp[0]),64);
        bufp->chgQData(oldp+2051,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__dataArrayDataOutTmp[1]),64);
        bufp->chgBit(oldp+2053,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__replArrayDataOutTmp[0]));
        bufp->chgBit(oldp+2054,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__replArrayDataOutTmp[1]));
        bufp->chgBit(oldp+2055,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__isReplSameIndex[0]));
        bufp->chgBit(oldp+2056,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__isReplSameIndex[1]));
        bufp->chgIData(oldp+2057,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk10__DOT__r),32);
        bufp->chgIData(oldp+2058,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk10__DOT__unnamedblk11__DOT__w),32);
        bufp->chgIData(oldp+2059,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk12__DOT__p),32);
        bufp->chgIData(oldp+2060,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk13__DOT__p),32);
        bufp->chgIData(oldp+2061,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk13__DOT__unnamedblk14__DOT__i),32);
        bufp->chgIData(oldp+2062,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk15__DOT__r),32);
        bufp->chgIData(oldp+2063,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk3__DOT__r),32);
        bufp->chgIData(oldp+2064,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk4__DOT__r),32);
        bufp->chgIData(oldp+2065,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk5__DOT__r),32);
        bufp->chgIData(oldp+2066,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk6__DOT__p),32);
        bufp->chgIData(oldp+2067,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk7__DOT__p),32);
        bufp->chgIData(oldp+2068,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk7__DOT__unnamedblk8__DOT__m),32);
        bufp->chgIData(oldp+2069,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayMux__DOT__unnamedblk7__DOT__unnamedblk9__DOT__way),32);
        bufp->chgIData(oldp+2070,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk12__DOT__i),32);
        bufp->chgIData(oldp+2071,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk13__DOT__i),32);
        bufp->chgIData(oldp+2072,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk14__DOT__i),32);
        bufp->chgIData(oldp+2073,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk15__DOT__i),32);
        bufp->chgIData(oldp+2074,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk16__DOT__i),32);
        bufp->chgIData(oldp+2075,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk16__DOT__unnamedblk17__DOT__m),32);
        bufp->chgIData(oldp+2076,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk18__DOT__i),32);
        bufp->chgIData(oldp+2077,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk19__DOT__i),32);
        bufp->chgIData(oldp+2078,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk19__DOT__unnamedblk20__DOT__m),32);
        bufp->chgIData(oldp+2079,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk21__DOT__i),32);
        bufp->chgIData(oldp+2080,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk22__DOT__i),32);
        bufp->chgIData(oldp+2081,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk23__DOT__i),32);
        bufp->chgIData(oldp+2082,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk24__DOT__i),32);
        bufp->chgIData(oldp+2083,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__unnamedblk25__DOT__i),32);
        bufp->chgIData(oldp+2084,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__loadLSQ_BlockData[0]),32);
        bufp->chgIData(oldp+2085,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__shiftedLoadData[0]),32);
        bufp->chgIData(oldp+2086,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__extendedLoadData[0]),32);
        bufp->chgIData(oldp+2087,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2088,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__unnamedblk3__DOT__i),32);
        bufp->chgBit(oldp+2089,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__storeHasAllocatedMSHR
                                [0U]));
        bufp->chgBit(oldp+2090,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__storeMSHRID
                                [0U]));
        bufp->chgBit(oldp+2091,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__stallStoreTagStage));
        bufp->chgBit(oldp+2092,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg 
                                               >> 0x3cU)))));
        bufp->chgBit(oldp+2093,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg 
                                               >> 0x3bU)))));
        bufp->chgIData(oldp+2094,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg 
                                           >> 0x1bU))),32);
        bufp->chgIData(oldp+2095,((0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg 
                                                       >> 7U)))),20);
        bufp->chgBit(oldp+2096,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg 
                                               >> 6U)))));
        bufp->chgCData(oldp+2097,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg 
                                                   >> 2U)))),4);
        bufp->chgBit(oldp+2098,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg 
                                               >> 1U)))));
        bufp->chgBit(oldp+2099,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__nextDataStagePipeReg))));
        bufp->chgIData(oldp+2100,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadData[0]),32);
        bufp->chgWData(oldp+2101,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadVectorData[0]),128);
        bufp->chgBit(oldp+2105,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadBusy[0]));
        bufp->chgBit(oldp+2106,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadHit[0]));
        bufp->chgQData(oldp+2107,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadData[0]),64);
        bufp->chgBit(oldp+2109,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__loadHasAllocatedMSHR[0]));
        bufp->chgBit(oldp+2110,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__loadMSHRID[0]));
        bufp->chgBit(oldp+2111,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__storeHasAllocatedMSHR[0]));
        bufp->chgBit(oldp+2112,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__storeMSHRID[0]));
        bufp->chgBit(oldp+2113,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteBusy));
        bufp->chgBit(oldp+2114,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcWriteHit));
        bufp->chgBit(oldp+2115,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrAddrHit[0]));
        bufp->chgBit(oldp+2116,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrAddrHitMSHRID[0]));
        bufp->chgBit(oldp+2117,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrReadHit[0]));
        bufp->chgQData(oldp+2118,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrReadData[0]),64);
        bufp->chgBit(oldp+2120,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayWE[0]));
        bufp->chgBit(oldp+2121,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayWE[1]));
        bufp->chgBit(oldp+2122,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayWriteWay[0]));
        bufp->chgBit(oldp+2123,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayWriteWay[1]));
        bufp->chgCData(oldp+2124,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayIndexIn[0]),8);
        bufp->chgCData(oldp+2125,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayIndexIn[1]),8);
        bufp->chgSData(oldp+2126,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayDataIn[0]),11);
        bufp->chgSData(oldp+2127,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayDataIn[1]),11);
        bufp->chgBit(oldp+2128,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayValidIn[0]));
        bufp->chgBit(oldp+2129,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayValidIn[1]));
        bufp->chgBit(oldp+2130,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayWE[0]));
        bufp->chgBit(oldp+2131,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayWE[1]));
        bufp->chgCData(oldp+2132,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayIndexIn[0]),8);
        bufp->chgCData(oldp+2133,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayIndexIn[1]),8);
        bufp->chgQData(oldp+2134,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayDataIn[0]),64);
        bufp->chgQData(oldp+2136,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayDataIn[1]),64);
        bufp->chgCData(oldp+2138,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayByteWE_In[0]),8);
        bufp->chgCData(oldp+2139,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayByteWE_In[1]),8);
        bufp->chgBit(oldp+2140,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayWriteWay[0]));
        bufp->chgBit(oldp+2141,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayWriteWay[1]));
        bufp->chgBit(oldp+2142,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayReadWay[0]));
        bufp->chgBit(oldp+2143,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayReadWay[1]));
        bufp->chgBit(oldp+2144,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayDoesReadEvictedWay[0]));
        bufp->chgBit(oldp+2145,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayDoesReadEvictedWay[1]));
        bufp->chgBit(oldp+2146,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayDirtyIn[0]));
        bufp->chgBit(oldp+2147,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dataArrayDirtyIn[1]));
        bufp->chgBit(oldp+2148,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__replArrayWE[0]));
        bufp->chgBit(oldp+2149,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__replArrayWE[1]));
        bufp->chgCData(oldp+2150,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__replArrayIndexIn[0]),8);
        bufp->chgCData(oldp+2151,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__replArrayIndexIn[1]),8);
        bufp->chgBit(oldp+2152,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__replArrayDataIn[0]));
        bufp->chgBit(oldp+2153,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__replArrayDataIn[1]));
        bufp->chgSData(oldp+2154,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                             [0U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+2155,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                             [0U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+2156,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                         [0U][2U] >> 5U))),2);
        bufp->chgBit(oldp+2157,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [0U][2U] >> 4U))));
        bufp->chgBit(oldp+2158,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [0U][2U] >> 3U))));
        bufp->chgBit(oldp+2159,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [0U][2U] >> 2U))));
        bufp->chgBit(oldp+2160,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+2161,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                 [0U][2U])));
        bufp->chgQData(oldp+2162,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                                                [0U][0U])))),64);
        bufp->chgSData(oldp+2164,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                             [1U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+2165,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                             [1U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+2166,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                         [1U][2U] >> 5U))),2);
        bufp->chgBit(oldp+2167,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [1U][2U] >> 4U))));
        bufp->chgBit(oldp+2168,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [1U][2U] >> 3U))));
        bufp->chgBit(oldp+2169,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [1U][2U] >> 2U))));
        bufp->chgBit(oldp+2170,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                       [1U][2U] >> 1U))));
        bufp->chgBit(oldp+2171,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                 [1U][2U])));
        bufp->chgQData(oldp+2172,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxTagOut
                                                                [1U][0U])))),64);
        bufp->chgQData(oldp+2174,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                                    [0U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                                                  [0U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                                                    [0U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2176,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2177,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                 [0U][0U])));
        bufp->chgQData(oldp+2178,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                                    [1U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                                                  [1U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                                                    [1U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2180,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+2181,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuMuxDataOut
                                 [1U][0U])));
        bufp->chgSData(oldp+2182,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                             [0U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+2183,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                             [0U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+2184,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                         [0U][2U] >> 5U))),2);
        bufp->chgBit(oldp+2185,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [0U][2U] >> 4U))));
        bufp->chgBit(oldp+2186,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [0U][2U] >> 3U))));
        bufp->chgBit(oldp+2187,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [0U][2U] >> 2U))));
        bufp->chgBit(oldp+2188,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+2189,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                 [0U][2U])));
        bufp->chgQData(oldp+2190,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                                                [0U][0U])))),64);
        bufp->chgSData(oldp+2192,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                             [1U][2U] 
                                             >> 7U))),11);
        bufp->chgSData(oldp+2193,((0x7ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                             [1U][2U] 
                                             >> 0x12U))),11);
        bufp->chgCData(oldp+2194,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                         [1U][2U] >> 5U))),2);
        bufp->chgBit(oldp+2195,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [1U][2U] >> 4U))));
        bufp->chgBit(oldp+2196,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [1U][2U] >> 3U))));
        bufp->chgBit(oldp+2197,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [1U][2U] >> 2U))));
        bufp->chgBit(oldp+2198,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                       [1U][2U] >> 1U))));
        bufp->chgBit(oldp+2199,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                 [1U][2U])));
        bufp->chgQData(oldp+2200,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxTagOut
                                                                [1U][0U])))),64);
        bufp->chgQData(oldp+2202,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                    [0U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                  [0U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                    [0U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2204,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2205,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                 [0U][0U])));
        bufp->chgQData(oldp+2206,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                    [1U][2U])) 
                                    << 0x3eU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                  [1U][1U])) 
                                                  << 0x1eU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                                                    [1U][0U])) 
                                                    >> 2U)))),64);
        bufp->chgBit(oldp+2208,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+2209,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheMuxDataOut
                                 [1U][0U])));
        bufp->chgBit(oldp+2210,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR[0]));
        bufp->chgBit(oldp+2211,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR[1]));
        bufp->chgBit(oldp+2212,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                       [0U] >> 0x15U))));
        bufp->chgBit(oldp+2213,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                       [0U] >> 0x14U))));
        bufp->chgIData(oldp+2214,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                   [0U])),20);
        bufp->chgBit(oldp+2215,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                       [1U] >> 0x15U))));
        bufp->chgBit(oldp+2216,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                       [1U] >> 0x14U))));
        bufp->chgIData(oldp+2217,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_Addr
                                   [1U])),20);
        bufp->chgCData(oldp+2218,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr[0]),6);
        bufp->chgCData(oldp+2219,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__initMSHR_ActiveListPtr[1]),6);
        bufp->chgBit(oldp+2220,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isAllocatedByStore[0]));
        bufp->chgBit(oldp+2221,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isAllocatedByStore[1]));
        bufp->chgBit(oldp+2222,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable[0]));
        bufp->chgBit(oldp+2223,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__isUncachable[1]));
        bufp->chgBit(oldp+2224,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheGrtReg[0]));
        bufp->chgBit(oldp+2225,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheGrtReg[1]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x31U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x78U])))) {
        bufp->chgBit(oldp+2226,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isStore[0]));
        bufp->chgBit(oldp+2227,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isStore[1]));
        bufp->chgBit(oldp+2228,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isLoad[0]));
        bufp->chgBit(oldp+2229,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isLoad[1]));
        bufp->chgBit(oldp+2230,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isCSR[0]));
        bufp->chgBit(oldp+2231,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isCSR[1]));
        bufp->chgBit(oldp+2232,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isDiv[0]));
        bufp->chgBit(oldp+2233,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isDiv[1]));
        bufp->chgBit(oldp+2234,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isMul[0]));
        bufp->chgBit(oldp+2235,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__isMul[1]));
        bufp->chgBit(oldp+2236,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__valid[0]));
        bufp->chgBit(oldp+2237,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__valid[1]));
        bufp->chgBit(oldp+2238,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__update[0]));
        bufp->chgBit(oldp+2239,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__update[1]));
        bufp->chgBit(oldp+2240,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__regValid[0]));
        bufp->chgBit(oldp+2241,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__regValid[1]));
        bufp->chgBit(oldp+2242,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__stall));
        bufp->chgBit(oldp+2243,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__clear));
        bufp->chgBit(oldp+2244,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__flush[0]));
        bufp->chgBit(oldp+2245,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__flush[1]));
        bufp->chgSData(oldp+2246,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                             [0U][4U] 
                                             >> 3U))),10);
        bufp->chgCData(oldp+2247,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                         [0U][4U] >> 1U))),2);
        bufp->chgBit(oldp+2248,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                 [0U][4U])));
        bufp->chgCData(oldp+2249,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                   [0U][3U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2250,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2251,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x12U))),4);
        bufp->chgIData(oldp+2252,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                    [0U][3U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                      [0U][2U] >> 0x12U))),32);
        bufp->chgIData(oldp+2253,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                    [0U][2U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                      [0U][1U] >> 0x12U))),32);
        bufp->chgBit(oldp+2254,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [0U][1U] >> 0x11U))));
        bufp->chgBit(oldp+2255,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [0U][1U] >> 0x10U))));
        bufp->chgCData(oldp+2256,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                            [0U][1U] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+2257,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 6U))),4);
        bufp->chgBit(oldp+2258,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [0U][1U] >> 5U))));
        bufp->chgBit(oldp+2259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [0U][1U] >> 4U))));
        bufp->chgBit(oldp+2260,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [0U][1U] >> 3U))));
        bufp->chgIData(oldp+2261,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                    [0U][1U] << 0x1dU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                      [0U][0U] >> 3U))),32);
        bufp->chgBit(oldp+2262,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2264,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+2265,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                             [1U][4U] 
                                             >> 3U))),10);
        bufp->chgCData(oldp+2266,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                         [1U][4U] >> 1U))),2);
        bufp->chgBit(oldp+2267,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                 [1U][4U])));
        bufp->chgCData(oldp+2268,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                   [1U][3U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2269,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2270,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x12U))),4);
        bufp->chgIData(oldp+2271,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                    [1U][3U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                      [1U][2U] >> 0x12U))),32);
        bufp->chgIData(oldp+2272,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                    [1U][2U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                      [1U][1U] >> 0x12U))),32);
        bufp->chgBit(oldp+2273,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [1U][1U] >> 0x11U))));
        bufp->chgBit(oldp+2274,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [1U][1U] >> 0x10U))));
        bufp->chgCData(oldp+2275,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                            [1U][1U] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+2276,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 6U))),4);
        bufp->chgBit(oldp+2277,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [1U][1U] >> 5U))));
        bufp->chgBit(oldp+2278,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [1U][1U] >> 4U))));
        bufp->chgBit(oldp+2279,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [1U][1U] >> 3U))));
        bufp->chgIData(oldp+2280,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                    [1U][1U] << 0x1dU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                      [1U][0U] >> 3U))),32);
        bufp->chgBit(oldp+2281,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+2282,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+2283,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__nextStage
                                 [1U][0U])));
        bufp->chgBit(oldp+2284,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__dataOut
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2285,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__dataOut
                                          [0U])),32);
        bufp->chgBit(oldp+2286,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__dataOut
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2287,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__dataOut
                                          [1U])),32);
        bufp->chgBit(oldp+2288,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__ldDataOut
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2289,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__ldDataOut
                                          [0U])),32);
        bufp->chgBit(oldp+2290,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__stDataOut
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2291,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__stDataOut
                                          [0U])),32);
        bufp->chgIData(oldp+2292,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk10__DOT__i),32);
        bufp->chgIData(oldp+2293,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2294,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+2295,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+2296,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+2297,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk7__DOT__i),32);
        bufp->chgIData(oldp+2298,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk8__DOT__i),32);
        bufp->chgIData(oldp+2299,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk9__DOT__i),32);
        bufp->chgSData(oldp+2300,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                             [0U][4U] 
                                             >> 3U))),10);
        bufp->chgCData(oldp+2301,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                         [0U][4U] >> 1U))),2);
        bufp->chgBit(oldp+2302,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                 [0U][4U])));
        bufp->chgCData(oldp+2303,((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                   [0U][3U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2304,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2305,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x12U))),4);
        bufp->chgIData(oldp+2306,(((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                    [0U][3U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x12U))),32);
        bufp->chgIData(oldp+2307,(((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                    [0U][2U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x12U))),32);
        bufp->chgBit(oldp+2308,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x11U))));
        bufp->chgBit(oldp+2309,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x10U))));
        bufp->chgCData(oldp+2310,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+2311,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 6U))),4);
        bufp->chgBit(oldp+2312,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [0U][1U] >> 5U))));
        bufp->chgBit(oldp+2313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [0U][1U] >> 4U))));
        bufp->chgBit(oldp+2314,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [0U][1U] >> 3U))));
        bufp->chgIData(oldp+2315,(((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                    [0U][1U] << 0x1dU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                      [0U][0U] >> 3U))),32);
        bufp->chgBit(oldp+2316,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2317,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2318,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+2319,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                             [1U][4U] 
                                             >> 3U))),10);
        bufp->chgCData(oldp+2320,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                         [1U][4U] >> 1U))),2);
        bufp->chgBit(oldp+2321,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                 [1U][4U])));
        bufp->chgCData(oldp+2322,((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                   [1U][3U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2323,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2324,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0x12U))),4);
        bufp->chgIData(oldp+2325,(((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                    [1U][3U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x12U))),32);
        bufp->chgIData(oldp+2326,(((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                    [1U][2U] << 0xeU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x12U))),32);
        bufp->chgBit(oldp+2327,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x11U))));
        bufp->chgBit(oldp+2328,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x10U))));
        bufp->chgCData(oldp+2329,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                            [1U][1U] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+2330,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 6U))),4);
        bufp->chgBit(oldp+2331,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [1U][1U] >> 5U))));
        bufp->chgBit(oldp+2332,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [1U][1U] >> 4U))));
        bufp->chgBit(oldp+2333,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [1U][1U] >> 3U))));
        bufp->chgIData(oldp+2334,(((vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                    [1U][1U] << 0x1dU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                      [1U][0U] >> 3U))),32);
        bufp->chgBit(oldp+2335,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+2336,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+2337,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__maStageIF.__PVT__nextStage
                                 [1U][0U])));
        bufp->chgBit(oldp+2338,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memDstRegDataOut
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2339,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memDstRegDataOut
                                          [0U])),32);
        bufp->chgBit(oldp+2340,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memDstRegDataOut
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2341,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memDstRegDataOut
                                          [1U])),32);
        bufp->chgBit(oldp+2342,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioReadAddrIn 
                                       >> 0x15U))));
        bufp->chgBit(oldp+2343,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioReadAddrIn 
                                       >> 0x14U))));
        bufp->chgIData(oldp+2344,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioReadAddrIn)),20);
        bufp->chgBit(oldp+2345,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                       [0U][5U] >> 0xeU))));
        bufp->chgBit(oldp+2346,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                       [0U][5U] >> 0xdU))));
        bufp->chgSData(oldp+2347,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                             [0U][5U] 
                                             >> 3U))),10);
        bufp->chgCData(oldp+2348,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                         [0U][5U] >> 1U))),2);
        bufp->chgBit(oldp+2349,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                 [0U][5U])));
        bufp->chgIData(oldp+2350,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                  [0U][4U]),32);
        __Vtemp_3[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [0U][0U];
        __Vtemp_3[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [0U][1U];
        __Vtemp_3[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [0U][2U];
        __Vtemp_3[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [0U][3U];
        bufp->chgWData(oldp+2351,(__Vtemp_3),128);
        bufp->chgBit(oldp+2355,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                       [1U][5U] >> 0xeU))));
        bufp->chgBit(oldp+2356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                       [1U][5U] >> 0xdU))));
        bufp->chgSData(oldp+2357,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                             [1U][5U] 
                                             >> 3U))),10);
        bufp->chgCData(oldp+2358,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                         [1U][5U] >> 1U))),2);
        bufp->chgBit(oldp+2359,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                 [1U][5U])));
        bufp->chgIData(oldp+2360,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
                                  [1U][4U]),32);
        __Vtemp_4[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [1U][0U];
        __Vtemp_4[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [1U][1U];
        __Vtemp_4[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [1U][2U];
        __Vtemp_4[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
            [1U][3U];
        bufp->chgWData(oldp+2361,(__Vtemp_4),128);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x32U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x79U])))) {
        bufp->chgCData(oldp+2365,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedLoadQueuePtrByLoad[0]),4);
        bufp->chgIData(oldp+2366,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedLoadAddr[0]),20);
        bufp->chgBit(oldp+2367,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedLoadWordRE[0]));
        bufp->chgBit(oldp+2368,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedLoadRegValid[0]));
        bufp->chgBit(oldp+2369,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__isLoad[0]));
        bufp->chgBit(oldp+2370,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__isCSR[0]));
        bufp->chgBit(oldp+2371,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__isENV[0]));
        bufp->chgBit(oldp+2372,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldUpdate[0]));
        bufp->chgBit(oldp+2373,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRegValid[0]));
        bufp->chgBit(oldp+2374,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldFlush[0]));
        bufp->chgBit(oldp+2375,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__isDiv[0]));
        bufp->chgBit(oldp+2376,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__isMul[0]));
        bufp->chgBit(oldp+2377,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__isFenceI[0]));
        bufp->chgBit(oldp+2378,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__storeForwardMiss[0]));
        bufp->chgSData(oldp+2379,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                              [0U][5U] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                                [0U][4U] 
                                                >> 0x1eU)))),10);
        bufp->chgCData(oldp+2380,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                         [0U][4U] >> 0x1cU))),2);
        bufp->chgBit(oldp+2381,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+2382,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+2383,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x19U))));
        bufp->chgBit(oldp+2384,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x18U))));
        bufp->chgBit(oldp+2385,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x17U))));
        bufp->chgBit(oldp+2386,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x16U))));
        bufp->chgBit(oldp+2387,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x15U))));
        bufp->chgBit(oldp+2388,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][4U] >> 0x14U))));
        bufp->chgCData(oldp+2389,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                            [0U][4U] 
                                            >> 0xeU))),6);
        bufp->chgCData(oldp+2390,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                            [0U][4U] 
                                            >> 8U))),6);
        bufp->chgCData(oldp+2391,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                           [0U][4U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2392,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                   [0U][4U])),4);
        bufp->chgIData(oldp+2393,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                  [0U][3U]),32);
        bufp->chgBit(oldp+2394,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                 [0U][2U] >> 0x1fU)));
        bufp->chgCData(oldp+2395,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                           [0U][2U] 
                                           >> 0x1bU))),4);
        bufp->chgIData(oldp+2396,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                    [0U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                      [0U][1U] >> 0x1bU))),32);
        bufp->chgCData(oldp+2397,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                         [0U][1U] >> 0x19U))),2);
        bufp->chgBit(oldp+2398,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][1U] >> 0x18U))));
        bufp->chgBit(oldp+2399,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][1U] >> 0x17U))));
        bufp->chgIData(oldp+2400,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                               [0U][1U] 
                                               >> 3U))),20);
        bufp->chgIData(oldp+2401,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                    [0U][1U] << 0x1dU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                      [0U][0U] >> 3U))),32);
        bufp->chgBit(oldp+2402,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2403,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2404,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldNextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+2405,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2406,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2407,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2408,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2409,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2410,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2411,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2412,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2413,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2414,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2415,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2416,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2417,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2418,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2419,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2420,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2421,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2422,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2423,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+2424,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                 [0U][2U])));
        bufp->chgCData(oldp+2425,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2426,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2427,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2428,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2429,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2430,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2431,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2432,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2433,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2434,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2435,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2436,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2437,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2438,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2439,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldRecordData
                                 [0U][0U])));
        bufp->chgBit(oldp+2440,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldMSHR_Allocated[0]));
        bufp->chgBit(oldp+2441,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldMSHR_Hit[0]));
        bufp->chgIData(oldp+2442,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldMSHR_EntryID[0]),32);
        bufp->chgIData(oldp+2443,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__unnamedblk5__DOT__i),32);
        bufp->chgBit(oldp+2444,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__forwardedDataEntry
                                               [0U] 
                                               >> 0x25U)))));
        bufp->chgIData(oldp+2445,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__forwardedDataEntry
                                           [0U] >> 5U))),32);
        bufp->chgBit(oldp+2446,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__forwardedDataEntry
                                               [0U] 
                                               >> 4U)))));
        bufp->chgCData(oldp+2447,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__forwardedDataEntry
                                                  [0U]))),4);
        bufp->chgBit(oldp+2448,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedLoadWordRE[0]));
        bufp->chgCData(oldp+2449,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedLoadByteRE[0]),4);
        bufp->chgSData(oldp+2450,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__addrMatch[0]),16);
        bufp->chgCData(oldp+2451,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__pickedPtr[0]),4);
        bufp->chgBit(oldp+2452,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__picked[0]));
        bufp->chgCData(oldp+2453,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreQueuePtrByLoad[0]),4);
        bufp->chgBit(oldp+2454,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeLoadForwarded[0]));
        bufp->chgIData(oldp+2455,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__forwardedLoadData[0]),32);
        bufp->chgBit(oldp+2456,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__forwardMiss[0]));
        bufp->chgCData(oldp+2457,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreQueuePtrByLoad
                                  [0U]),4);
        bufp->chgSData(oldp+2458,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__addrMatch
                                  [0U]),16);
        bufp->chgCData(oldp+2459,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr),4);
        bufp->chgBit(oldp+2460,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__Vcellout__genblk1__BRA__0__KET____DOT__picker__picked));
        bufp->chgIData(oldp+2461,(((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__addrMatch
                                    [0U] << 0x10U) 
                                   | vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__addrMatch
                                   [0U])),32);
        bufp->chgSData(oldp+2462,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq),16);
        bufp->chgCData(oldp+2463,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant),4);
        bufp->chgIData(oldp+2464,((0x7fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__addrMatch
                                                   [0U] 
                                                   << 0x10U) 
                                                  | vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__addrMatch
                                                  [0U]))),31);
        bufp->chgIData(oldp+2465,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp),32);
        bufp->chgBit(oldp+2466,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeLoad[0]));
        bufp->chgBit(oldp+2467,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadRegValid[0]));
        bufp->chgBit(oldp+2468,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                       [0U] >> 0x15U))));
        bufp->chgBit(oldp+2469,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                       [0U] >> 0x14U))));
        bufp->chgIData(oldp+2470,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadAddr
                                   [0U])),20);
        bufp->chgCData(oldp+2471,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemMapType
                                  [0U]),2);
        bufp->chgBit(oldp+2472,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadPC
                                       [0U] >> 0x13U))));
        bufp->chgIData(oldp+2473,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadPC
                                   [0U])),19);
        bufp->chgBit(oldp+2474,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemAccessMode
                                       [0U] >> 2U))));
        bufp->chgCData(oldp+2475,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadMemAccessMode
                                   [0U])),2);
        bufp->chgCData(oldp+2476,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByLoad[0]),4);
        bufp->chgCData(oldp+2477,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadQueuePtrByLoad[0]),4);
        bufp->chgBit(oldp+2478,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__storeLoadForwarded[0]));
        bufp->chgIData(oldp+2479,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__forwardedLoadData[0]),32);
        bufp->chgBit(oldp+2480,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__forwardMiss[0]));
        bufp->chgBit(oldp+2481,(vlSymsp->TOP__SMT_RTL_Testbench__core__perfCounterIF.__PVT__loadMiss[0]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x33U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x7aU])))) {
        bufp->chgBit(oldp+2482,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2483,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                          [0U])),32);
        bufp->chgBit(oldp+2484,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2485,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                          [1U])),32);
        bufp->chgBit(oldp+2486,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memDst
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2487,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memDst
                                          [0U])),32);
        bufp->chgBit(oldp+2488,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2489,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                          [0U])),32);
        bufp->chgBit(oldp+2490,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2491,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__intDst
                                          [1U])),32);
        bufp->chgBit(oldp+2492,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memDst
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2493,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__memDst
                                          [0U])),32);
        bufp->chgIData(oldp+2494,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+2495,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2496,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2497,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+2498,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__bypassNetwork__DOT__unnamedblk5__DOT__i),32);
        bufp->chgBit(oldp+2499,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__req[0]));
        bufp->chgBit(oldp+2500,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__req[1]));
        bufp->chgBit(oldp+2501,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__req[2]));
        bufp->chgBit(oldp+2502,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__req[3]));
        bufp->chgBit(oldp+2503,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__grant[0]));
        bufp->chgBit(oldp+2504,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__grant[1]));
        bufp->chgBit(oldp+2505,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__grant[2]));
        bufp->chgBit(oldp+2506,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__grant[3]));
        bufp->chgCData(oldp+2507,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayInSel[0]),2);
        bufp->chgCData(oldp+2508,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayInSel[1]),2);
        bufp->chgBit(oldp+2509,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayOutSel[0]));
        bufp->chgBit(oldp+2510,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayOutSel[1]));
        bufp->chgBit(oldp+2511,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayOutSel[2]));
        bufp->chgBit(oldp+2512,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayOutSel[3]));
        bufp->chgBit(oldp+2513,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayInGrant[0]));
        bufp->chgBit(oldp+2514,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayInGrant[1]));
        bufp->chgIData(oldp+2515,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk1__DOT__r),32);
        bufp->chgIData(oldp+2516,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk2__DOT__r),32);
        bufp->chgIData(oldp+2517,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk3__DOT__r),32);
        bufp->chgIData(oldp+2518,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk4__DOT__p),32);
        bufp->chgIData(oldp+2519,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk4__DOT__unnamedblk5__DOT__r),32);
        bufp->chgIData(oldp+2520,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk6__DOT__r),32);
        bufp->chgIData(oldp+2521,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk7__DOT__r),32);
        bufp->chgBit(oldp+2522,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dCache__DOT__controller__DOT__loadStoreBusy));
        bufp->chgBit(oldp+2523,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__stall));
        bufp->chgBit(oldp+2524,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__clear));
        bufp->chgBit(oldp+2525,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__flush[0]));
        bufp->chgBit(oldp+2526,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__flush[1]));
        bufp->chgSData(oldp+2527,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2528,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2529,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2530,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2531,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2532,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2533,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2534,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2535,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2536,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2537,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2538,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2539,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2540,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2541,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2542,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2543,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2544,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+2546,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                 [0U][2U])));
        bufp->chgCData(oldp+2547,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2548,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2549,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2550,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2551,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2552,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2553,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2554,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2555,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2556,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2557,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2558,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2559,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2560,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2561,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                 [0U][0U])));
        bufp->chgSData(oldp+2562,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                             [1U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2563,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2564,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2565,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2566,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2567,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2568,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                              [1U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                                [1U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2569,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2572,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2573,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [1U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2574,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2575,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2576,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                         [1U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2577,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2578,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [1U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2579,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [1U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2580,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][2U] >> 1U))));
        bufp->chgBit(oldp+2581,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                 [1U][2U])));
        bufp->chgCData(oldp+2582,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2583,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2584,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2585,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2586,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2587,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2588,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2589,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+2590,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2591,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2592,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2593,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                            [1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2594,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2595,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2596,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__iqData
                                 [1U][0U])));
        bufp->chgCData(oldp+2597,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x24U)))),3);
        bufp->chgCData(oldp+2598,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x21U)))),3);
        bufp->chgCData(oldp+2599,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x1fU)))),2);
        bufp->chgCData(oldp+2600,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x1dU)))),2);
        bufp->chgSData(oldp+2601,((0xfffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                     [0U] 
                                                     >> 0x11U)))),12);
        bufp->chgBit(oldp+2602,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [0U] 
                                               >> 0x10U)))));
        bufp->chgBit(oldp+2603,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [0U] 
                                               >> 0xfU)))));
        bufp->chgBit(oldp+2604,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [0U] 
                                               >> 0xeU)))));
        bufp->chgCData(oldp+2605,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0xcU)))),2);
        bufp->chgCData(oldp+2606,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                    [0U] 
                                                    >> 7U)))),5);
        bufp->chgBit(oldp+2607,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [0U] 
                                               >> 6U)))));
        bufp->chgCData(oldp+2608,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 4U)))),2);
        bufp->chgCData(oldp+2609,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 1U)))),3);
        bufp->chgBit(oldp+2610,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                              [0U]))));
        bufp->chgCData(oldp+2611,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x24U)))),3);
        bufp->chgCData(oldp+2612,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x21U)))),3);
        bufp->chgCData(oldp+2613,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x1fU)))),2);
        bufp->chgCData(oldp+2614,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x1dU)))),2);
        bufp->chgSData(oldp+2615,((0xfffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                     [1U] 
                                                     >> 0x11U)))),12);
        bufp->chgBit(oldp+2616,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [1U] 
                                               >> 0x10U)))));
        bufp->chgBit(oldp+2617,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [1U] 
                                               >> 0xfU)))));
        bufp->chgBit(oldp+2618,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [1U] 
                                               >> 0xeU)))));
        bufp->chgCData(oldp+2619,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0xcU)))),2);
        bufp->chgCData(oldp+2620,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                    [1U] 
                                                    >> 7U)))),5);
        bufp->chgBit(oldp+2621,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                               [1U] 
                                               >> 6U)))));
        bufp->chgCData(oldp+2622,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 4U)))),2);
        bufp->chgCData(oldp+2623,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 1U)))),3);
        bufp->chgBit(oldp+2624,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                              [1U]))));
        bufp->chgBit(oldp+2625,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2626,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpA
                                          [0U])),32);
        bufp->chgBit(oldp+2627,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpA
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2628,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpA
                                          [1U])),32);
        bufp->chgBit(oldp+2629,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2630,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpB
                                          [0U])),32);
        bufp->chgBit(oldp+2631,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpB
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2632,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__fuOpB
                                          [1U])),32);
        bufp->chgBit(oldp+2633,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__regValid[0]));
        bufp->chgBit(oldp+2634,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__regValid[1]));
        bufp->chgIData(oldp+2635,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__addrOut[0]),32);
        bufp->chgIData(oldp+2636,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__addrOut[1]),32);
        bufp->chgCData(oldp+2637,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memMapType
                                  [0U]),2);
        bufp->chgCData(oldp+2638,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memMapType
                                  [1U]),2);
        bufp->chgBit(oldp+2639,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__phyAddrOut
                                       [0U] >> 0x15U))));
        bufp->chgBit(oldp+2640,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__phyAddrOut
                                       [0U] >> 0x14U))));
        bufp->chgIData(oldp+2641,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__phyAddrOut
                                   [0U])),20);
        bufp->chgBit(oldp+2642,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__phyAddrOut
                                       [1U] >> 0x15U))));
        bufp->chgBit(oldp+2643,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__phyAddrOut
                                       [1U] >> 0x14U))));
        bufp->chgIData(oldp+2644,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__phyAddrOut
                                   [1U])),20);
        bufp->chgBit(oldp+2645,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__isUncachable[0]));
        bufp->chgBit(oldp+2646,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__isUncachable[1]));
        bufp->chgBit(oldp+2647,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__cacheFlushReq));
        bufp->chgBit(oldp+2648,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__isCSR));
        bufp->chgIData(oldp+2649,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2650,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgBit(oldp+2651,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2652,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutA
                                          [0U])),32);
        bufp->chgBit(oldp+2653,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutA
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2654,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutA
                                          [1U])),32);
        bufp->chgBit(oldp+2655,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2656,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutB
                                          [0U])),32);
        bufp->chgBit(oldp+2657,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutB
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2658,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intSrcRegDataOutB
                                          [1U])),32);
        bufp->chgBit(oldp+2659,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexSrcRegDataOutA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2660,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexSrcRegDataOutA
                                          [0U])),32);
        bufp->chgBit(oldp+2661,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexSrcRegDataOutB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2662,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexSrcRegDataOutB
                                          [0U])),32);
        bufp->chgBit(oldp+2663,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 0x14U))));
        bufp->chgCData(oldp+2664,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                         [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+2665,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 0x11U))));
        bufp->chgBit(oldp+2666,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 0x10U))));
        bufp->chgBit(oldp+2667,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 0xfU))));
        bufp->chgBit(oldp+2668,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 0xeU))));
        bufp->chgBit(oldp+2669,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 0xdU))));
        bufp->chgCData(oldp+2670,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                         [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+2671,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 0xaU))));
        bufp->chgBit(oldp+2672,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 9U))));
        bufp->chgBit(oldp+2673,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 8U))));
        bufp->chgBit(oldp+2674,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+2675,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2676,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                         [0U] >> 4U))),2);
        bufp->chgBit(oldp+2677,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 3U))));
        bufp->chgBit(oldp+2678,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 2U))));
        bufp->chgBit(oldp+2679,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [0U] >> 1U))));
        bufp->chgBit(oldp+2680,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                 [0U])));
        bufp->chgBit(oldp+2681,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 0x14U))));
        bufp->chgCData(oldp+2682,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                         [1U] >> 0x12U))),2);
        bufp->chgBit(oldp+2683,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 0x11U))));
        bufp->chgBit(oldp+2684,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 0x10U))));
        bufp->chgBit(oldp+2685,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 0xfU))));
        bufp->chgBit(oldp+2686,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 0xeU))));
        bufp->chgBit(oldp+2687,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 0xdU))));
        bufp->chgCData(oldp+2688,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                         [1U] >> 0xbU))),2);
        bufp->chgBit(oldp+2689,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 0xaU))));
        bufp->chgBit(oldp+2690,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 9U))));
        bufp->chgBit(oldp+2691,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 8U))));
        bufp->chgBit(oldp+2692,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 7U))));
        bufp->chgBit(oldp+2693,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+2694,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                         [1U] >> 4U))),2);
        bufp->chgBit(oldp+2695,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 3U))));
        bufp->chgBit(oldp+2696,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 2U))));
        bufp->chgBit(oldp+2697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                       [1U] >> 1U))));
        bufp->chgBit(oldp+2698,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memCtrlIn
                                 [1U])));
        bufp->chgBit(oldp+2699,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2700,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutA
                                          [0U])),32);
        bufp->chgBit(oldp+2701,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutA
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2702,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutA
                                          [1U])),32);
        bufp->chgBit(oldp+2703,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2704,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutB
                                          [0U])),32);
        bufp->chgBit(oldp+2705,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutB
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2706,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memSrcRegDataOutB
                                          [1U])),32);
        bufp->chgBit(oldp+2707,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpSrcRegDataOutA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2708,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpSrcRegDataOutA
                                          [0U])),32);
        bufp->chgBit(oldp+2709,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpSrcRegDataOutB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2710,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpSrcRegDataOutB
                                          [0U])),32);
        bufp->chgBit(oldp+2711,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpSrcRegDataOutC
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2712,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpSrcRegDataOutC
                                          [0U])),32);
        bufp->chgBit(oldp+2713,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadReq[0]));
        bufp->chgBit(oldp+2714,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadAddr
                                       [0U] >> 0x15U))));
        bufp->chgBit(oldp+2715,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadAddr
                                       [0U] >> 0x14U))));
        bufp->chgIData(oldp+2716,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadAddr
                                   [0U])),20);
        bufp->chgBit(oldp+2717,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadUncachable[0]));
        bufp->chgCData(oldp+2718,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__dcReadActiveListPtr[0]),6);
        bufp->chgBit(oldp+2719,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheGrt
                                [1U]));
        bufp->chgBit(oldp+2720,(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheFlushManagerIF.__PVT__cacheFlushReq));
        bufp->chgBit(oldp+2721,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWE));
        bufp->chgSData(oldp+2722,((0xfffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                     [0U] 
                                                     >> 0x11U)))),12);
        bufp->chgCData(oldp+2723,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 4U)))),2);
        bufp->chgIData(oldp+2724,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWriteIn),32);
        bufp->chgBit(oldp+2725,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheReq[0]));
        bufp->chgBit(oldp+2726,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheReq[1]));
        bufp->chgBit(oldp+2727,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheGrt[0]));
        bufp->chgBit(oldp+2728,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__lsuCacheGrt[1]));
        bufp->chgBit(oldp+2729,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt[0]));
        bufp->chgBit(oldp+2730,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCacheGrt[1]));
        bufp->chgBit(oldp+2731,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayInGrant[0]));
        bufp->chgBit(oldp+2732,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayInGrant[1]));
        bufp->chgCData(oldp+2733,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayInSel[0]),2);
        bufp->chgCData(oldp+2734,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayInSel[1]),2);
        bufp->chgBit(oldp+2735,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayOutSel[0]));
        bufp->chgBit(oldp+2736,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayOutSel[1]));
        bufp->chgBit(oldp+2737,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayOutSel[2]));
        bufp->chgBit(oldp+2738,(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__cacheArrayOutSel[3]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x34U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x7bU])))) {
        bufp->chgBit(oldp+2739,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__isStore[0]));
        bufp->chgBit(oldp+2740,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stUpdate[0]));
        bufp->chgBit(oldp+2741,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRegValid[0]));
        bufp->chgBit(oldp+2742,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stFlush[0]));
        bufp->chgSData(oldp+2743,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                              [0U][5U] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                                [0U][4U] 
                                                >> 0x1eU)))),10);
        bufp->chgCData(oldp+2744,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                         [0U][4U] >> 0x1cU))),2);
        bufp->chgBit(oldp+2745,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+2746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+2747,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x19U))));
        bufp->chgBit(oldp+2748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x18U))));
        bufp->chgBit(oldp+2749,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x17U))));
        bufp->chgBit(oldp+2750,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x16U))));
        bufp->chgBit(oldp+2751,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x15U))));
        bufp->chgBit(oldp+2752,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][4U] >> 0x14U))));
        bufp->chgCData(oldp+2753,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                            [0U][4U] 
                                            >> 0xeU))),6);
        bufp->chgCData(oldp+2754,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                            [0U][4U] 
                                            >> 8U))),6);
        bufp->chgCData(oldp+2755,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                           [0U][4U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2756,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                   [0U][4U])),4);
        bufp->chgIData(oldp+2757,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                  [0U][3U]),32);
        bufp->chgBit(oldp+2758,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                 [0U][2U] >> 0x1fU)));
        bufp->chgCData(oldp+2759,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                           [0U][2U] 
                                           >> 0x1bU))),4);
        bufp->chgIData(oldp+2760,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                    [0U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                      [0U][1U] >> 0x1bU))),32);
        bufp->chgCData(oldp+2761,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                         [0U][1U] >> 0x19U))),2);
        bufp->chgBit(oldp+2762,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][1U] >> 0x18U))));
        bufp->chgBit(oldp+2763,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][1U] >> 0x17U))));
        bufp->chgIData(oldp+2764,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                               [0U][1U] 
                                               >> 3U))),20);
        bufp->chgIData(oldp+2765,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                    [0U][1U] << 0x1dU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                      [0U][0U] >> 3U))),32);
        bufp->chgBit(oldp+2766,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2767,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2768,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stNextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+2769,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2770,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2771,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2772,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2773,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2774,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2775,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2776,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2777,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2778,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2779,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2780,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2781,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2782,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2783,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2785,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2786,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2787,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+2788,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                 [0U][2U])));
        bufp->chgCData(oldp+2789,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2790,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2791,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2792,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2793,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2794,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2795,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2796,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2797,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2798,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2799,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2800,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2801,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2802,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2803,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stRecordData
                                 [0U][0U])));
        bufp->chgBit(oldp+2804,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__memAccessOrderViolation[0]));
        bufp->chgBit(oldp+2805,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__flush[0]));
        bufp->chgBit(oldp+2806,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__flush[1]));
        bufp->chgSData(oldp+2807,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                              [0U][5U] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                                [0U][4U] 
                                                >> 0x1eU)))),10);
        bufp->chgCData(oldp+2808,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                         [0U][4U] >> 0x1cU))),2);
        bufp->chgBit(oldp+2809,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+2810,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+2811,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x19U))));
        bufp->chgBit(oldp+2812,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x18U))));
        bufp->chgBit(oldp+2813,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x17U))));
        bufp->chgBit(oldp+2814,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x16U))));
        bufp->chgBit(oldp+2815,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x15U))));
        bufp->chgBit(oldp+2816,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][4U] >> 0x14U))));
        bufp->chgCData(oldp+2817,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 0xeU))),6);
        bufp->chgCData(oldp+2818,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 8U))),6);
        bufp->chgCData(oldp+2819,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2820,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                   [0U][4U])),4);
        bufp->chgIData(oldp+2821,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                  [0U][3U]),32);
        bufp->chgBit(oldp+2822,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                 [0U][2U] >> 0x1fU)));
        bufp->chgCData(oldp+2823,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 0x1bU))),4);
        bufp->chgIData(oldp+2824,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                    [0U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                      [0U][1U] >> 0x1bU))),32);
        bufp->chgCData(oldp+2825,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                         [0U][1U] >> 0x19U))),2);
        bufp->chgBit(oldp+2826,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][1U] >> 0x18U))));
        bufp->chgBit(oldp+2827,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][1U] >> 0x17U))));
        bufp->chgIData(oldp+2828,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                               [0U][1U] 
                                               >> 3U))),20);
        bufp->chgIData(oldp+2829,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                    [0U][1U] << 0x1dU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                      [0U][0U] >> 3U))),32);
        bufp->chgBit(oldp+2830,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2831,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2832,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+2833,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                              [1U][5U] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                                [1U][4U] 
                                                >> 0x1eU)))),10);
        bufp->chgCData(oldp+2834,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                         [1U][4U] >> 0x1cU))),2);
        bufp->chgBit(oldp+2835,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+2836,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+2837,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x19U))));
        bufp->chgBit(oldp+2838,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x18U))));
        bufp->chgBit(oldp+2839,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x17U))));
        bufp->chgBit(oldp+2840,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x16U))));
        bufp->chgBit(oldp+2841,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x15U))));
        bufp->chgBit(oldp+2842,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                       [1U][4U] >> 0x14U))));
        bufp->chgCData(oldp+2843,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 0xeU))),6);
        bufp->chgCData(oldp+2844,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 8U))),6);
        bufp->chgCData(oldp+2845,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2846,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                   [1U][4U])),4);
        bufp->chgIData(oldp+2847,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                  [1U][3U]),32);
        bufp->chgBit(oldp+2848,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                 [1U][2U] >> 0x1fU)));
        bufp->chgCData(oldp+2849,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                           [1U][2U] 
                                           >> 0x1bU))),4);
        bufp->chgIData(oldp+2850,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                    [1U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                      [1U][1U] >> 0x1bU))),32);
        bufp->chgCData(oldp+2851,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                         [1U][1U] >> 0x19U))),2);
    }
}
