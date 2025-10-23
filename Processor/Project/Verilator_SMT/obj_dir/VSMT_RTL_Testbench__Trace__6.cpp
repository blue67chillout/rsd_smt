// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "VSMT_RTL_Testbench__Syms.h"


void VSMT_RTL_Testbench___024root__trace_chg_0_sub_6(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root__trace_chg_0_sub_6\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode + 18542);
    VlWide<3>/*95:0*/ __Vtemp_2;
    VlWide<3>/*95:0*/ __Vtemp_3;
    VlWide<3>/*95:0*/ __Vtemp_7;
    VlWide<3>/*95:0*/ __Vtemp_11;
    VlWide<3>/*95:0*/ __Vtemp_14;
    VlWide<3>/*95:0*/ __Vtemp_17;
    VlWide<3>/*95:0*/ __Vtemp_18;
    VlWide<3>/*95:0*/ __Vtemp_22;
    VlWide<3>/*95:0*/ __Vtemp_23;
    VlWide<3>/*95:0*/ __Vtemp_27;
    VlWide<3>/*95:0*/ __Vtemp_28;
    VlWide<3>/*95:0*/ __Vtemp_32;
    VlWide<3>/*95:0*/ __Vtemp_33;
    VlWide<3>/*95:0*/ __Vtemp_35;
    VlWide<3>/*95:0*/ __Vtemp_36;
    VlWide<3>/*95:0*/ __Vtemp_37;
    VlWide<3>/*95:0*/ __Vtemp_38;
    // Body
    if (VL_UNLIKELY((vlSelfRef.__Vm_traceActivity[0x53U]))) {
        bufp->chgBit(oldp+0,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                    [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+1,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                    [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                         [1U][0U] >> 0x15U))),6);
        bufp->chgBit(oldp+3,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                    [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+4,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                            [1U][0U] 
                                            >> 1U))),19);
        bufp->chgBit(oldp+5,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                              [1U][0U])));
        bufp->chgSData(oldp+6,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][2U] 
                                          >> 0x13U))),10);
        bufp->chgCData(oldp+7,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                      [0U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+8,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                      [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+9,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [0U][2U] >> 9U))),5);
        bufp->chgCData(oldp+10,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 6U))),3);
        bufp->chgCData(oldp+11,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 4U))),2);
        bufp->chgCData(oldp+12,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+13,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [0U][2U])),2);
        bufp->chgCData(oldp+14,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+15,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [0U][1U] >> 0x16U))),4);
        bufp->chgCData(oldp+16,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [0U][1U] >> 0x12U))),4);
        bufp->chgBit(oldp+17,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+18,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][1U] 
                                          >> 0xbU))),6);
        bufp->chgBit(oldp+19,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+20,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][1U] 
                                          >> 4U))),6);
        bufp->chgBit(oldp+21,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][1U] >> 3U))));
        bufp->chgCData(oldp+22,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [0U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [0U][0U] 
                                           >> 0x1dU)))),6);
        bufp->chgBit(oldp+23,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+24,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+25,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][0U] 
                                          >> 0x15U))),6);
        bufp->chgBit(oldp+26,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+27,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                             [0U][0U] 
                                             >> 1U))),19);
        bufp->chgBit(oldp+28,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                               [0U][0U])));
        bufp->chgSData(oldp+29,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [1U][2U] 
                                           >> 0x13U))),10);
        bufp->chgCData(oldp+30,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+31,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+32,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][2U] 
                                          >> 9U))),5);
        bufp->chgCData(oldp+33,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 6U))),3);
        bufp->chgCData(oldp+34,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 4U))),2);
        bufp->chgCData(oldp+35,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 2U))),2);
        bufp->chgCData(oldp+36,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [1U][2U])),2);
        bufp->chgCData(oldp+37,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+38,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [1U][1U] >> 0x16U))),4);
        bufp->chgCData(oldp+39,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [1U][1U] >> 0x12U))),4);
        bufp->chgBit(oldp+40,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+41,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][1U] 
                                          >> 0xbU))),6);
        bufp->chgBit(oldp+42,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+43,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][1U] 
                                          >> 4U))),6);
        bufp->chgBit(oldp+44,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][1U] >> 3U))));
        bufp->chgCData(oldp+45,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [1U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [1U][0U] 
                                           >> 0x1dU)))),6);
        bufp->chgBit(oldp+46,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+47,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+48,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][0U] 
                                          >> 0x15U))),6);
        bufp->chgBit(oldp+49,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+50,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                             [1U][0U] 
                                             >> 1U))),19);
        bufp->chgBit(oldp+51,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                               [1U][0U])));
        bufp->chgCData(oldp+52,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [0U] 
                                               >> 0x2fU)))),2);
        bufp->chgCData(oldp+53,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [0U] 
                                               >> 0x2cU)))),3);
        bufp->chgBit(oldp+54,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x2bU)))));
        bufp->chgBit(oldp+55,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x2aU)))));
        bufp->chgBit(oldp+56,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x29U)))));
        bufp->chgBit(oldp+57,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x28U)))));
        bufp->chgCData(oldp+58,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0x22U)))),6);
        bufp->chgBit(oldp+59,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x21U)))));
        bufp->chgCData(oldp+60,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0x1bU)))),6);
        bufp->chgBit(oldp+61,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x1aU)))));
        bufp->chgCData(oldp+62,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0x14U)))),6);
        bufp->chgBit(oldp+63,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x13U)))));
        bufp->chgBit(oldp+64,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x12U)))));
        bufp->chgCData(oldp+65,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0xcU)))),6);
        bufp->chgCData(oldp+66,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [0U] 
                                                 >> 8U)))),4);
        bufp->chgCData(oldp+67,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [0U] 
                                                 >> 4U)))),4);
        bufp->chgCData(oldp+68,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                [0U]))),4);
        bufp->chgCData(oldp+69,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [1U] 
                                               >> 0x2fU)))),2);
        bufp->chgCData(oldp+70,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [1U] 
                                               >> 0x2cU)))),3);
        bufp->chgBit(oldp+71,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x2bU)))));
        bufp->chgBit(oldp+72,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x2aU)))));
        bufp->chgBit(oldp+73,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x29U)))));
        bufp->chgBit(oldp+74,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x28U)))));
        bufp->chgCData(oldp+75,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0x22U)))),6);
        bufp->chgBit(oldp+76,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x21U)))));
        bufp->chgCData(oldp+77,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0x1bU)))),6);
        bufp->chgBit(oldp+78,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x1aU)))));
        bufp->chgCData(oldp+79,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0x14U)))),6);
        bufp->chgBit(oldp+80,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x13U)))));
        bufp->chgBit(oldp+81,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x12U)))));
        bufp->chgCData(oldp+82,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0xcU)))),6);
        bufp->chgCData(oldp+83,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [1U] 
                                                 >> 8U)))),4);
        bufp->chgCData(oldp+84,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [1U] 
                                                 >> 4U)))),4);
        bufp->chgCData(oldp+85,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                [1U]))),4);
        bufp->chgBit(oldp+86,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [0U] >> 0x14U))));
        bufp->chgCData(oldp+87,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [0U] >> 0xeU))),6);
        bufp->chgBit(oldp+88,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [0U] >> 0xdU))));
        bufp->chgCData(oldp+89,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [0U] >> 7U))),6);
        bufp->chgBit(oldp+90,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [0U] >> 6U))));
        bufp->chgCData(oldp+91,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                 [0U])),6);
        bufp->chgBit(oldp+92,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [1U] >> 0x14U))));
        bufp->chgCData(oldp+93,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [1U] >> 0xeU))),6);
        bufp->chgBit(oldp+94,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [1U] >> 0xdU))));
        bufp->chgCData(oldp+95,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [1U] >> 7U))),6);
        bufp->chgBit(oldp+96,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [1U] >> 6U))));
        bufp->chgCData(oldp+97,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                 [1U])),6);
        bufp->chgBit(oldp+98,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                     [0U] >> 7U))));
        bufp->chgBit(oldp+99,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                     [0U] >> 6U))));
        bufp->chgCData(oldp+100,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                  [0U])),6);
        bufp->chgBit(oldp+101,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                      [1U] >> 7U))));
        bufp->chgBit(oldp+102,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+103,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                  [1U])),6);
        bufp->chgCData(oldp+104,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                [0U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+105,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                [0U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+106,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                  [0U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+107,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                              [0U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+108,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                         [0U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+109,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                     [0U]))),18);
        bufp->chgCData(oldp+110,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                [1U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+111,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                [1U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+112,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                  [1U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+113,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                              [1U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+114,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                         [1U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+115,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                     [1U]))),18);
        bufp->chgCData(oldp+116,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                [0U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+117,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                [0U] 
                                                >> 0x35U)))),2);
        bufp->chgBit(oldp+118,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                              [0U] 
                                              >> 0x34U)))));
        bufp->chgIData(oldp+119,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                      [0U] 
                                                      >> 0x21U)))),19);
        bufp->chgBit(oldp+120,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgSData(oldp+121,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                    [0U] 
                                                    >> 0x16U)))),10);
        bufp->chgCData(oldp+122,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                [0U] 
                                                >> 0x14U)))),2);
        bufp->chgIData(oldp+123,((0xfffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                     [0U]))),20);
        bufp->chgCData(oldp+124,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                [1U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+125,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                [1U] 
                                                >> 0x35U)))),2);
        bufp->chgBit(oldp+126,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                              [1U] 
                                              >> 0x34U)))));
        bufp->chgIData(oldp+127,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                      [1U] 
                                                      >> 0x21U)))),19);
        bufp->chgBit(oldp+128,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgSData(oldp+129,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                    [1U] 
                                                    >> 0x16U)))),10);
        bufp->chgCData(oldp+130,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                [1U] 
                                                >> 0x14U)))),2);
        bufp->chgIData(oldp+131,((0xfffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                     [1U]))),20);
        bufp->chgBit(oldp+132,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                      [0U] >> 2U))));
        bufp->chgCData(oldp+133,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                  [0U])),2);
        bufp->chgBit(oldp+134,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                      [1U] >> 2U))));
        bufp->chgCData(oldp+135,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                  [1U])),2);
        bufp->chgBit(oldp+136,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                      [0U] >> 2U))));
        bufp->chgCData(oldp+137,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                  [0U])),2);
        bufp->chgBit(oldp+138,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                      [1U] >> 2U))));
        bufp->chgCData(oldp+139,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                  [1U])),2);
        bufp->chgIData(oldp+140,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__unnamedblk1__DOT__i),32);
        bufp->chgCData(oldp+141,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__regPhase
                                 [0U]),2);
        bufp->chgBit(oldp+142,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__finished[0]));
        bufp->chgCData(oldp+143,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__regActiveListPtr[0]),6);
        bufp->chgBit(oldp+144,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regPhase))));
        bufp->chgIData(oldp+145,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regResult),32);
        bufp->chgCData(oldp+146,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regPhase),2);
        bufp->chgCData(oldp+147,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regCounter),5);
        bufp->chgSData(oldp+148,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[5U])),10);
        bufp->chgSData(oldp+149,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                  >> 0x16U)),10);
        bufp->chgIData(oldp+150,((0xffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                                << 2U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                  >> 0x1eU)))),24);
        bufp->chgIData(oldp+151,((0xffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                               >> 6U))),24);
        bufp->chgSData(oldp+152,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                               >> 0x1cU)))),10);
        bufp->chgBit(oldp+153,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x1bU))));
        bufp->chgBit(oldp+154,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x1aU))));
        bufp->chgBit(oldp+155,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x19U))));
        bufp->chgBit(oldp+156,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x18U))));
        bufp->chgBit(oldp+157,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x17U))));
        bufp->chgBit(oldp+158,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x16U))));
        bufp->chgBit(oldp+159,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x15U))));
        bufp->chgIData(oldp+160,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                   << 0xbU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[1U] 
                                               >> 0x15U))),32);
        bufp->chgIData(oldp+161,((0x7ffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[1U] 
                                                 << 6U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[0U] 
                                                   >> 0x1aU)))),27);
        bufp->chgIData(oldp+162,((0x3ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[0U])),26);
        bufp->chgBit(oldp+163,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__dividend_normalize));
        bufp->chgSData(oldp+164,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__virtual_expo),10);
        bufp->chgBit(oldp+165,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__subnormal));
        bufp->chgBit(oldp+166,((1U & ((0x8000000U & 
                                       vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                       ? (VL_GTES_III(32, 0xffffffe8U, 
                                                      VL_EXTENDS_II(32,10, (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__virtual_expo))) 
                                          | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                             >> 0x17U))
                                       : (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                          >> 0x17U)))));
        bufp->chgIData(oldp+167,((0x7ffffffU & ((0x8000000U 
                                                 & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                                 ? 
                                                ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__dividend_normalize)
                                                  ? 
                                                 (0x1fffffeU 
                                                  & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                                      << 3U) 
                                                     | (6U 
                                                        & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                           >> 0x1dU))))
                                                  : 
                                                 (0xffffffU 
                                                  & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                                      << 2U) 
                                                     | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                        >> 0x1eU))))
                                                 : 
                                                ((1U 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[5U])
                                                  ? 
                                                 ((0x1fffffeU 
                                                   & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                                       << 3U) 
                                                      | (6U 
                                                         & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                            >> 0x1dU)))) 
                                                  - (IData)(0x1e40000U))
                                                  : 
                                                 ((0x3fffffcU 
                                                   & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                                       << 4U) 
                                                      | (0xcU 
                                                         & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                            >> 0x1cU)))) 
                                                  - (IData)(0x2400000U)))))),27);
        bufp->chgIData(oldp+168,(((0x8000000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                   ? 0U : ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[5U])
                                            ? 0x1600000U
                                            : 0x1800000U))),26);
        bufp->chgCData(oldp+169,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__q),3);
        bufp->chgCData(oldp+170,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__div),4);
        bufp->chgIData(oldp+171,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rem),27);
        bufp->chgIData(oldp+172,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__quo),26);
        bufp->chgQData(oldp+173,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round),48);
        bufp->chgBit(oldp+175,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away));
        bufp->chgBit(oldp+176,((IData)(((0xfffffe000000ULL 
                                         == (0xfffffe000000ULL 
                                             & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round)) 
                                        & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away)))));
        bufp->chgIData(oldp+177,((0x7fffffU & ((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round 
                                                        >> 0x19U)) 
                                               + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away)))),23);
        bufp->chgCData(oldp+178,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__result_expo),8);
        bufp->chgBit(oldp+179,((1U & ((0x8000000U & 
                                       vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                       ? (VL_LTES_III(32, 0xffU, 
                                                      VL_EXTENDS_II(32,10, 
                                                                    (0x3ffU 
                                                                     & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                                         << 4U) 
                                                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                                           >> 0x1cU))))) 
                                          | ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                              >> 0x15U) 
                                             | (0xffU 
                                                == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__result_expo))))
                                       : (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                          >> 0x15U)))));
        bufp->chgIData(oldp+180,((0x7f800000U | (0x80000000U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                    << 5U)))),32);
        bufp->chgIData(oldp+181,((((0x8000000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                    ? (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                       >> 0x1aU) : 
                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                    >> 0x19U)) << 0x1fU)),32);
        bufp->chgIData(oldp+182,(((0x400000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                   ? ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                       << 0xbU) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[1U] 
                                                   >> 0x15U))
                                   : ((0x800000U & 
                                       vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                       ? (((0x8000000U 
                                            & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                            ? (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                               >> 0x1aU)
                                            : (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                               >> 0x19U)) 
                                          << 0x1fU)
                                       : ((1U & ((0x8000000U 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                                  ? 
                                                 (VL_LTES_III(32, 0xffU, 
                                                              VL_EXTENDS_II(32,10, 
                                                                            (0x3ffU 
                                                                             & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                                                << 4U) 
                                                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                                                >> 0x1cU))))) 
                                                  | ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                      >> 0x15U) 
                                                     | (0xffU 
                                                        == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__result_expo))))
                                                  : 
                                                 (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                  >> 0x15U)))
                                           ? (0x7f800000U 
                                              | (0x80000000U 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                    << 5U)))
                                           : ((0x80000000U 
                                               & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                  << 5U)) 
                                              | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__result_expo) 
                                                  << 0x17U) 
                                                 | (0x7fffffU 
                                                    & ((IData)(
                                                               (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round 
                                                                >> 0x19U)) 
                                                       + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away))))))))),32);
        bufp->chgIData(oldp+183,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+184,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaDataOut[0]),32);
        bufp->chgIData(oldp+185,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherDataOut[0]),32);
        bufp->chgBit(oldp+186,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                      [0U] >> 4U))));
        bufp->chgBit(oldp+187,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+188,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+189,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+190,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                [0U])));
        bufp->chgIData(oldp+191,(((0x40U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])
                                   ? vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[1U]
                                   : vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__final_result)),32);
        bufp->chgSData(oldp+192,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                            >> 7U))),10);
        bufp->chgBit(oldp+193,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                      >> 6U))));
        bufp->chgBit(oldp+194,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                      >> 5U))));
        bufp->chgBit(oldp+195,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                      >> 4U))));
        bufp->chgBit(oldp+196,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                      >> 3U))));
        bufp->chgBit(oldp+197,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                      >> 2U))));
        bufp->chgBit(oldp+198,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                      >> 1U))));
        bufp->chgBit(oldp+199,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U])));
        bufp->chgIData(oldp+200,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[1U]),32);
        bufp->chgIData(oldp+201,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[0U]),32);
        __Vtemp_2[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[0U];
        __Vtemp_2[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[1U];
        __Vtemp_2[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U];
        VL_NEGATE_W(3, __Vtemp_3, __Vtemp_2);
        if ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U])) {
            __Vtemp_7[0U] = __Vtemp_3[0U];
            __Vtemp_7[1U] = __Vtemp_3[1U];
            __Vtemp_7[2U] = (0xfffU & __Vtemp_3[2U]);
        } else {
            __Vtemp_7[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[0U];
            __Vtemp_7[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[1U];
            __Vtemp_7[2U] = (0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U]);
        }
        bufp->chgWData(oldp+202,(__Vtemp_7),76);
        bufp->chgSData(oldp+205,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                            >> 7U))),10);
        bufp->chgBit(oldp+206,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U] 
                                      >> 0xcU))));
        bufp->chgBit(oldp+207,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                      >> 6U))));
        bufp->chgBit(oldp+208,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                      >> 5U))));
        bufp->chgBit(oldp+209,((0U == ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[0U] 
                                        | vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[1U]) 
                                       | vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U]))));
        bufp->chgBit(oldp+210,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                      >> 4U))));
        bufp->chgBit(oldp+211,((1U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                       >> 3U) ^ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U] 
                                                 >> 0xcU)))));
        bufp->chgBit(oldp+212,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                      >> 2U))));
        bufp->chgBit(oldp+213,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                      >> 1U))));
        bufp->chgBit(oldp+214,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U])));
        bufp->chgIData(oldp+215,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[1U]),32);
        bufp->chgIData(oldp+216,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[0U]),32);
        __Vtemp_11[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[3U] 
                           << 0xdU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                       >> 0x13U));
        __Vtemp_11[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[4U] 
                           << 0xdU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[3U] 
                                       >> 0x13U));
        __Vtemp_11[2U] = (0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[4U] 
                                    >> 0x13U));
        bufp->chgWData(oldp+217,(__Vtemp_11),76);
        bufp->chgCData(oldp+220,((0xffU & (VL_GTES_III(32, 0U, 
                                                       VL_EXTENDS_II(32,10, (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__virtual_expo)))
                                            ? ((IData)(0x1aU) 
                                               - (0xffU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                                     >> 9U)))
                                            : ((IData)(0x33U) 
                                               - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__leading_zeros))))),8);
        bufp->chgSData(oldp+221,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__virtual_expo),10);
        bufp->chgBit(oldp+222,(VL_GTES_III(32, 0U, 
                                           VL_EXTENDS_II(32,10, (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__virtual_expo)))));
        bufp->chgBit(oldp+223,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 8U))));
        bufp->chgBit(oldp+224,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 7U))));
        bufp->chgBit(oldp+225,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 6U))));
        bufp->chgBit(oldp+226,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 5U))));
        bufp->chgBit(oldp+227,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 4U))));
        bufp->chgBit(oldp+228,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 3U))));
        bufp->chgBit(oldp+229,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 2U))));
        bufp->chgBit(oldp+230,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                      >> 1U))));
        bufp->chgBit(oldp+231,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U])));
        bufp->chgIData(oldp+232,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[1U]),32);
        bufp->chgIData(oldp+233,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[0U]),32);
        bufp->chgWData(oldp+234,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_lhs),77);
        bufp->chgWData(oldp+237,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_rhs),77);
        bufp->chgWData(oldp+240,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_addend),77);
        bufp->chgWData(oldp+243,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result),77);
        bufp->chgBit(oldp+246,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__is_subtract));
        bufp->chgBit(oldp+247,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                      >> 3U))));
        bufp->chgSData(oldp+248,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                            >> 9U))),10);
        bufp->chgCData(oldp+249,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__leading_zeros),8);
        __Vtemp_14[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 0x1cU));
        __Vtemp_14[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                                     >> 0x1cU));
        __Vtemp_14[2U] = (0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[5U] 
                                     << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                                               >> 0x1cU)));
        bufp->chgWData(oldp+250,(__Vtemp_14),76);
        bufp->chgCData(oldp+253,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                           >> 0x14U))),8);
        bufp->chgSData(oldp+254,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                            >> 0xaU))),10);
        bufp->chgBit(oldp+255,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 9U))));
        bufp->chgBit(oldp+256,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 8U))));
        bufp->chgBit(oldp+257,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 7U))));
        bufp->chgBit(oldp+258,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 6U))));
        bufp->chgBit(oldp+259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 5U))));
        bufp->chgBit(oldp+260,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 4U))));
        bufp->chgBit(oldp+261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 3U))));
        bufp->chgBit(oldp+262,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 2U))));
        bufp->chgBit(oldp+263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 1U))));
        bufp->chgBit(oldp+264,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])));
        bufp->chgIData(oldp+265,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[1U]),32);
        bufp->chgIData(oldp+266,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[0U]),32);
        bufp->chgIData(oldp+267,((0xffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U])),24);
        __Vtemp_17[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 0x1cU));
        __Vtemp_17[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                                     >> 0x1cU));
        __Vtemp_17[2U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[5U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                                     >> 0x1cU));
        VL_SHIFTL_WWI(76,76,32, __Vtemp_18, __Vtemp_17, 
                      ((IData)(0x4cU) - (0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                  >> 0x14U))));
        bufp->chgBit(oldp+268,((0U != ((__Vtemp_18[0U] 
                                        | __Vtemp_18[1U]) 
                                       | (0xfffU & 
                                          __Vtemp_18[2U])))));
        __Vtemp_22[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 0x1cU));
        __Vtemp_22[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                                     >> 0x1cU));
        __Vtemp_22[2U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[5U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                                     >> 0x1cU));
        VL_SHIFTL_WWI(76,76,32, __Vtemp_23, __Vtemp_22, 
                      ((IData)(0x4cU) - (0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                  >> 0x14U))));
        bufp->chgBit(oldp+269,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                      & ((vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                          >> 1U) | 
                                         (0U != ((__Vtemp_23[0U] 
                                                  | __Vtemp_23[1U]) 
                                                 | (0xfffU 
                                                    & __Vtemp_23[2U]))))))));
        bufp->chgBit(oldp+270,((0xffffffU <= (0xffffffU 
                                              & vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U]))));
        __Vtemp_27[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 0x1cU));
        __Vtemp_27[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                                     >> 0x1cU));
        __Vtemp_27[2U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[5U] 
                           << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                                     >> 0x1cU));
        VL_SHIFTL_WWI(76,76,32, __Vtemp_28, __Vtemp_27, 
                      ((IData)(0x4cU) - (0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                  >> 0x14U))));
        bufp->chgIData(oldp+271,((0x7fffffU & (((vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                                 << 0x1fU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                                   >> 1U)) 
                                               + (1U 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                                     & ((vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                                         >> 1U) 
                                                        | (0U 
                                                           != 
                                                           ((__Vtemp_28[0U] 
                                                             | __Vtemp_28[1U]) 
                                                            | (0xfffU 
                                                               & __Vtemp_28[2U]))))))))),23);
        bufp->chgCData(oldp+272,((0xffU & (((0x200U 
                                             & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])
                                             ? 0U : 
                                            ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                              << 0x16U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                >> 0xaU))) 
                                           + (0xffffffU 
                                              <= (0xffffffU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U]))))),8);
        bufp->chgBit(oldp+273,((1U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                       >> 7U) | VL_LTES_III(32, 0xffU, 
                                                            VL_EXTENDS_II(32,10, 
                                                                          (0x3ffU 
                                                                           & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                                              >> 0xaU))))))));
        bufp->chgIData(oldp+274,((0x7f800000U | (((0x80U 
                                                   & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])
                                                   ? 
                                                  (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                   >> 2U)
                                                   : 
                                                  (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                   >> 3U)) 
                                                 << 0x1fU))),32);
        bufp->chgIData(oldp+275,((0x80000000U & (((~ 
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U]) 
                                                  << 0x1fU) 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                    << 0x1eU)))),32);
        bufp->chgIData(oldp+276,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__final_result),32);
        bufp->chgIData(oldp+277,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                          [0U] >> 5U))),32);
        bufp->chgBit(oldp+278,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 4U)))));
        bufp->chgBit(oldp+279,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 3U)))));
        bufp->chgBit(oldp+280,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 2U)))));
        bufp->chgBit(oldp+281,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 1U)))));
        bufp->chgBit(oldp+282,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U]))));
        bufp->chgIData(oldp+283,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                          [0U] >> 5U))),32);
        bufp->chgBit(oldp+284,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 4U)))));
        bufp->chgBit(oldp+285,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 3U)))));
        bufp->chgBit(oldp+286,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 2U)))));
        bufp->chgBit(oldp+287,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [0U] 
                                              >> 1U)))));
        bufp->chgBit(oldp+288,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U]))));
        bufp->chgIData(oldp+289,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                          [1U] >> 5U))),32);
        bufp->chgBit(oldp+290,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [1U] 
                                              >> 4U)))));
        bufp->chgBit(oldp+291,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [1U] 
                                              >> 3U)))));
        bufp->chgBit(oldp+292,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [1U] 
                                              >> 2U)))));
        bufp->chgBit(oldp+293,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [1U] 
                                              >> 1U)))));
        bufp->chgBit(oldp+294,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [1U]))));
        bufp->chgIData(oldp+295,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                          [2U] >> 5U))),32);
        bufp->chgBit(oldp+296,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [2U] 
                                              >> 4U)))));
        bufp->chgBit(oldp+297,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [2U] 
                                              >> 3U)))));
        bufp->chgBit(oldp+298,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [2U] 
                                              >> 2U)))));
        bufp->chgBit(oldp+299,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [2U] 
                                              >> 1U)))));
        bufp->chgBit(oldp+300,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [2U]))));
        bufp->chgIData(oldp+301,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                          [3U] >> 5U))),32);
        bufp->chgBit(oldp+302,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [3U] 
                                              >> 4U)))));
        bufp->chgBit(oldp+303,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [3U] 
                                              >> 3U)))));
        bufp->chgBit(oldp+304,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [3U] 
                                              >> 2U)))));
        bufp->chgBit(oldp+305,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                              [3U] 
                                              >> 1U)))));
        bufp->chgBit(oldp+306,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [3U]))));
        bufp->chgIData(oldp+307,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+308,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+309,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__j),32);
        bufp->chgIData(oldp+310,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk6__DOT__i),32);
        bufp->chgBit(oldp+311,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__pipeReg
                                      [0U] >> 4U))));
        bufp->chgCData(oldp+312,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__pipeReg
                                  [0U])),4);
        bufp->chgIData(oldp+313,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+314,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+315,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRwStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgCData(oldp+316,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regPhase),3);
        bufp->chgBit(oldp+317,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regFlushStart));
        bufp->chgBit(oldp+318,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regFlush));
        bufp->chgBit(oldp+319,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regFlushReqAck));
        bufp->chgBit(oldp+320,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__flushComplete));
        bufp->chgIData(oldp+321,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                         [0U])),32);
        bufp->chgIData(oldp+322,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                          [0U] >> 0x20U))),32);
        bufp->chgIData(oldp+323,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                         [1U])),32);
        bufp->chgIData(oldp+324,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                          [1U] >> 0x20U))),32);
        bufp->chgCData(oldp+325,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readNRUState),2);
        bufp->chgCData(oldp+326,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__rstIndex),8);
        bufp->chgBit(oldp+327,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regMissValid));
        bufp->chgQData(oldp+328,((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                                   << 0x20U) | (QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))),64);
        bufp->chgQData(oldp+330,((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[2U])) 
                                   << 0x34U) | (((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[1U])) 
                                                 << 0x14U) 
                                                | ((QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])) 
                                                   >> 0xcU)))),64);
        bufp->chgCData(oldp+332,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__rstIndex),8);
        bufp->chgBit(oldp+333,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U] 
                                      >> 0xbU))));
        bufp->chgSData(oldp+334,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])),11);
        bufp->chgWData(oldp+335,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv),76);
        bufp->chgQData(oldp+338,((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[2U])) 
                                   << 0x34U) | (((QData)((IData)(
                                                                 vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[1U])) 
                                                 << 0x14U) 
                                                | ((QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])) 
                                                   >> 0xcU)))),64);
        bufp->chgCData(oldp+340,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__rstIndex),8);
        bufp->chgBit(oldp+341,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U] 
                                      >> 0xbU))));
        bufp->chgSData(oldp+342,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])),11);
        bufp->chgWData(oldp+343,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv),76);
        bufp->chgBit(oldp+346,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__empty));
        bufp->chgCData(oldp+347,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                        >> 9U))),3);
        bufp->chgCData(oldp+348,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                        >> 7U))),2);
        bufp->chgCData(oldp+349,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                        >> 4U))),3);
        bufp->chgBit(oldp+350,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 3U))));
        bufp->chgCData(oldp+351,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                              >> 0x1eU)))),5);
        bufp->chgBit(oldp+352,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 0x1dU))));
        bufp->chgCData(oldp+353,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                           >> 0x18U))),5);
        bufp->chgBit(oldp+354,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 0x17U))));
        bufp->chgCData(oldp+355,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                           >> 0x12U))),5);
        bufp->chgCData(oldp+356,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                          >> 0xeU))),4);
        bufp->chgBit(oldp+357,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 0xdU))));
        bufp->chgIData(oldp+358,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                                  << 0x11U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                    >> 0xfU)))),30);
        bufp->chgBit(oldp+359,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 0x11U))));
        bufp->chgBit(oldp+360,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 0x10U))));
        bufp->chgBit(oldp+361,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 0xfU))));
        bufp->chgCData(oldp+362,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                        >> 0xdU))),2);
        bufp->chgCData(oldp+363,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                           >> 8U))),5);
        bufp->chgBit(oldp+364,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 7U))));
        bufp->chgCData(oldp+365,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                        >> 5U))),2);
        bufp->chgSData(oldp+366,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                               >> 0x1bU)))),10);
        bufp->chgSData(oldp+367,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                            >> 0xfU))),12);
        bufp->chgSData(oldp+368,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                             >> 3U))),15);
        bufp->chgIData(oldp+369,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                               << 0x11U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                 >> 0xfU)))),20);
        bufp->chgCData(oldp+370,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                        >> 0xfU))),2);
        bufp->chgSData(oldp+371,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                              << 3U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                >> 0x1dU)))),16);
        bufp->chgSData(oldp+372,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                             >> 0xfU))),14);
        bufp->chgSData(oldp+373,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                             >> 1U))),15);
        bufp->chgIData(oldp+374,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                               << 0x11U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                 >> 0xfU)))),18);
        bufp->chgCData(oldp+375,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                        >> 0xfU))),3);
        bufp->chgBit(oldp+376,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                      >> 0xeU))));
        bufp->chgIData(oldp+377,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                 >> 0x1bU)))),19);
        bufp->chgCData(oldp+378,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                           >> 0xcU))),5);
        bufp->chgCData(oldp+379,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                           >> 7U))),5);
        bufp->chgCData(oldp+380,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                        >> 4U))),3);
        bufp->chgIData(oldp+381,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                  >> 0xfU)))),21);
        bufp->chgCData(oldp+382,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                        >> 0xdU))),2);
        bufp->chgCData(oldp+383,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                        >> 0xbU))),2);
        bufp->chgCData(oldp+384,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                        >> 9U))),2);
        bufp->chgBit(oldp+385,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                      >> 8U))));
        bufp->chgBit(oldp+386,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                      >> 7U))));
        bufp->chgBit(oldp+387,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                      >> 6U))));
        bufp->chgBit(oldp+388,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                      >> 5U))));
        bufp->chgBit(oldp+389,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                      >> 4U))));
        bufp->chgBit(oldp+390,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                      >> 3U))));
        bufp->chgCData(oldp+391,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                        >> 1U))),2);
        bufp->chgBit(oldp+392,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U])));
        bufp->chgCData(oldp+393,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                        >> 0x15U))),3);
        bufp->chgCData(oldp+394,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                        >> 0x13U))),2);
        bufp->chgCData(oldp+395,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                        >> 0x10U))),3);
        bufp->chgBit(oldp+396,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 0xfU))));
        bufp->chgCData(oldp+397,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                           >> 0xaU))),5);
        bufp->chgBit(oldp+398,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 9U))));
        bufp->chgCData(oldp+399,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                           >> 4U))),5);
        bufp->chgBit(oldp+400,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 3U))));
        bufp->chgCData(oldp+401,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                              >> 0x1eU)))),5);
        bufp->chgCData(oldp+402,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                          >> 0x1aU))),4);
        bufp->chgBit(oldp+403,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                      >> 0x19U))));
        bufp->chgIData(oldp+404,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                                  << 5U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                    >> 0x1bU)))),30);
        bufp->chgBit(oldp+405,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                      >> 0x1dU))));
        bufp->chgBit(oldp+406,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+408,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                        >> 0x19U))),2);
        bufp->chgCData(oldp+409,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                           >> 0x14U))),5);
        bufp->chgBit(oldp+410,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                      >> 0x13U))));
        bufp->chgCData(oldp+411,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                        >> 0x11U))),2);
        bufp->chgSData(oldp+412,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                            >> 7U))),10);
        bufp->chgSData(oldp+413,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                               >> 0x1bU)))),12);
        bufp->chgSData(oldp+414,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                             >> 0xfU))),15);
        bufp->chgIData(oldp+415,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                 >> 0x1bU)))),20);
        bufp->chgCData(oldp+416,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                        >> 0x1bU))),2);
        bufp->chgSData(oldp+417,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                             >> 9U))),16);
        bufp->chgSData(oldp+418,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                >> 0x1bU)))),14);
        bufp->chgSData(oldp+419,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                             >> 0xdU))),15);
        bufp->chgIData(oldp+420,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                 >> 0x1bU)))),18);
        bufp->chgCData(oldp+421,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                        >> 0x1bU))),3);
        bufp->chgBit(oldp+422,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                      >> 0x1aU))));
        bufp->chgIData(oldp+423,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                              >> 7U))),19);
        bufp->chgCData(oldp+424,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                           >> 0x18U))),5);
        bufp->chgCData(oldp+425,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                           >> 0x13U))),5);
        bufp->chgCData(oldp+426,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                        >> 0x10U))),3);
        bufp->chgIData(oldp+427,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                  >> 0x1bU)))),21);
        bufp->chgCData(oldp+428,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                        >> 0x19U))),2);
        bufp->chgCData(oldp+429,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                        >> 0x17U))),2);
        bufp->chgCData(oldp+430,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                        >> 0x15U))),2);
        bufp->chgBit(oldp+431,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 0x14U))));
        bufp->chgBit(oldp+432,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 0x13U))));
        bufp->chgBit(oldp+433,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 0x12U))));
        bufp->chgBit(oldp+434,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 0x11U))));
        bufp->chgBit(oldp+435,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 0x10U))));
        bufp->chgBit(oldp+436,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 0xfU))));
        bufp->chgCData(oldp+437,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                        >> 0xdU))),2);
        bufp->chgBit(oldp+438,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                      >> 0xcU))));
        bufp->chgCData(oldp+439,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                        >> 1U))),3);
        bufp->chgCData(oldp+440,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                         << 1U) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                                   >> 0x1fU)))),2);
        bufp->chgCData(oldp+441,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                        >> 0x1cU))),3);
        bufp->chgBit(oldp+442,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+443,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                           >> 0x16U))),5);
        bufp->chgBit(oldp+444,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 0x15U))));
        bufp->chgCData(oldp+445,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                           >> 0x10U))),5);
        bufp->chgBit(oldp+446,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 0xfU))));
        bufp->chgCData(oldp+447,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                           >> 0xaU))),5);
        bufp->chgCData(oldp+448,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                          >> 6U))),4);
        bufp->chgBit(oldp+449,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 5U))));
        bufp->chgIData(oldp+450,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                                  << 0x19U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                                    >> 7U)))),30);
        bufp->chgBit(oldp+451,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 9U))));
        bufp->chgBit(oldp+452,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 8U))));
        bufp->chgBit(oldp+453,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 7U))));
        bufp->chgCData(oldp+454,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                        >> 5U))),2);
        bufp->chgCData(oldp+455,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U])),5);
        bufp->chgBit(oldp+456,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                >> 0x1fU)));
        bufp->chgCData(oldp+457,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                        >> 0x1dU))),2);
        bufp->chgSData(oldp+458,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                            >> 0x13U))),10);
        bufp->chgSData(oldp+459,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                            >> 7U))),12);
        bufp->chgSData(oldp+460,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                                >> 0x1bU)))),15);
        bufp->chgIData(oldp+461,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                              >> 7U))),20);
        bufp->chgCData(oldp+462,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                        >> 7U))),2);
        bufp->chgSData(oldp+463,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                              << 0xbU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                                >> 0x15U)))),16);
        bufp->chgSData(oldp+464,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                             >> 7U))),14);
        bufp->chgSData(oldp+465,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                                >> 0x19U)))),15);
        bufp->chgIData(oldp+466,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                              >> 7U))),18);
        bufp->chgCData(oldp+467,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                        >> 7U))),3);
        bufp->chgBit(oldp+468,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                      >> 6U))));
        bufp->chgIData(oldp+469,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                               << 0xdU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                                 >> 0x13U)))),19);
        bufp->chgCData(oldp+470,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                           >> 4U))),5);
        bufp->chgCData(oldp+471,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                              >> 0x1fU)))),5);
        bufp->chgCData(oldp+472,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                        >> 0x1cU))),3);
        bufp->chgIData(oldp+473,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                               >> 7U))),21);
        bufp->chgCData(oldp+474,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                        >> 5U))),2);
        bufp->chgCData(oldp+475,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                        >> 3U))),2);
        bufp->chgCData(oldp+476,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                        >> 1U))),2);
        bufp->chgBit(oldp+477,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U])));
        bufp->chgBit(oldp+478,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                >> 0x1fU)));
        bufp->chgBit(oldp+479,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 0x1eU))));
        bufp->chgBit(oldp+480,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 0x1dU))));
        bufp->chgBit(oldp+481,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+482,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+483,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                        >> 0x19U))),2);
        bufp->chgBit(oldp+484,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                      >> 0x18U))));
        bufp->chgCData(oldp+485,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                        >> 0xdU))),3);
        bufp->chgCData(oldp+486,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                        >> 0xbU))),2);
        bufp->chgCData(oldp+487,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                        >> 8U))),3);
        bufp->chgBit(oldp+488,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 7U))));
        bufp->chgCData(oldp+489,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                           >> 2U))),5);
        bufp->chgBit(oldp+490,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 1U))));
        bufp->chgCData(oldp+491,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                              >> 0x1cU)))),5);
        bufp->chgBit(oldp+492,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+493,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                           >> 0x16U))),5);
        bufp->chgCData(oldp+494,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+495,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                      >> 0x11U))));
        bufp->chgIData(oldp+496,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                                  << 0xdU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                    >> 0x13U)))),30);
        bufp->chgBit(oldp+497,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                      >> 0x15U))));
        bufp->chgBit(oldp+498,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                      >> 0x14U))));
        bufp->chgBit(oldp+499,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                      >> 0x13U))));
        bufp->chgCData(oldp+500,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                        >> 0x11U))),2);
        bufp->chgCData(oldp+501,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                           >> 0xcU))),5);
        bufp->chgBit(oldp+502,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                      >> 0xbU))));
        bufp->chgCData(oldp+503,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                        >> 9U))),2);
        bufp->chgSData(oldp+504,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                               >> 0x1fU)))),10);
        bufp->chgSData(oldp+505,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                            >> 0x13U))),12);
        bufp->chgSData(oldp+506,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                             >> 7U))),15);
        bufp->chgIData(oldp+507,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                               << 0xdU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                 >> 0x13U)))),20);
        bufp->chgCData(oldp+508,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                        >> 0x13U))),2);
        bufp->chgSData(oldp+509,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                             >> 1U))),16);
        bufp->chgSData(oldp+510,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                >> 0x13U)))),14);
        bufp->chgSData(oldp+511,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                             >> 5U))),15);
        bufp->chgIData(oldp+512,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                               << 0xdU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                 >> 0x13U)))),18);
        bufp->chgCData(oldp+513,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                        >> 0x13U))),3);
        bufp->chgBit(oldp+514,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                      >> 0x12U))));
        bufp->chgIData(oldp+515,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                 >> 0x1fU)))),19);
        bufp->chgCData(oldp+516,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                           >> 0x10U))),5);
        bufp->chgCData(oldp+517,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                           >> 0xbU))),5);
        bufp->chgCData(oldp+518,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                        >> 8U))),3);
        bufp->chgIData(oldp+519,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                                << 0xdU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                  >> 0x13U)))),21);
        bufp->chgCData(oldp+520,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                        >> 0x11U))),2);
        bufp->chgCData(oldp+521,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                        >> 0xfU))),2);
        bufp->chgCData(oldp+522,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                        >> 0xdU))),2);
        bufp->chgBit(oldp+523,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                      >> 0xcU))));
        bufp->chgBit(oldp+524,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                      >> 0xbU))));
        bufp->chgBit(oldp+525,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                      >> 0xaU))));
        bufp->chgBit(oldp+526,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                      >> 9U))));
        bufp->chgBit(oldp+527,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                      >> 8U))));
        bufp->chgBit(oldp+528,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                      >> 7U))));
        bufp->chgCData(oldp+529,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                        >> 5U))),2);
        bufp->chgBit(oldp+530,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                      >> 4U))));
        bufp->chgCData(oldp+531,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                        >> 0x19U))),3);
        bufp->chgCData(oldp+532,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                        >> 0x17U))),2);
        bufp->chgCData(oldp+533,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                        >> 0x14U))),3);
        bufp->chgBit(oldp+534,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                      >> 0x13U))));
        bufp->chgCData(oldp+535,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                           >> 0xeU))),5);
        bufp->chgBit(oldp+536,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                      >> 0xdU))));
        bufp->chgCData(oldp+537,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                           >> 8U))),5);
        bufp->chgBit(oldp+538,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                      >> 7U))));
        bufp->chgCData(oldp+539,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                           >> 2U))),5);
        bufp->chgCData(oldp+540,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                           >> 0x1eU)))),4);
        bufp->chgBit(oldp+541,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                      >> 0x1dU))));
        bufp->chgIData(oldp+542,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                  << 1U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                    >> 0x1fU)))),30);
        bufp->chgBit(oldp+543,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                      >> 1U))));
        bufp->chgBit(oldp+544,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU])));
        bufp->chgBit(oldp+545,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                >> 0x1fU)));
        bufp->chgCData(oldp+546,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                        >> 0x1dU))),2);
        bufp->chgCData(oldp+547,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                           >> 0x18U))),5);
        bufp->chgBit(oldp+548,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                      >> 0x17U))));
        bufp->chgCData(oldp+549,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                        >> 0x15U))),2);
        bufp->chgSData(oldp+550,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                            >> 0xbU))),10);
        bufp->chgSData(oldp+551,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                               >> 0x1fU)))),12);
        bufp->chgSData(oldp+552,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                >> 0x13U)))),15);
        bufp->chgIData(oldp+553,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                 >> 0x1fU)))),20);
        bufp->chgCData(oldp+554,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                         << 1U) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                   >> 0x1fU)))),2);
        bufp->chgSData(oldp+555,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                             >> 0xdU))),16);
        bufp->chgSData(oldp+556,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                >> 0x1fU)))),14);
        bufp->chgSData(oldp+557,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                  >> 0x11U)),15);
        bufp->chgIData(oldp+558,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                 >> 0x1fU)))),18);
        bufp->chgCData(oldp+559,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                         << 1U) | (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                   >> 0x1fU)))),3);
        bufp->chgBit(oldp+560,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                      >> 0x1eU))));
        bufp->chgIData(oldp+561,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                              >> 0xbU))),19);
        bufp->chgCData(oldp+562,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                              >> 0x1cU)))),5);
        bufp->chgCData(oldp+563,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                           >> 0x17U))),5);
        bufp->chgCData(oldp+564,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                        >> 0x14U))),3);
        bufp->chgIData(oldp+565,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                << 1U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                  >> 0x1fU)))),21);
        bufp->chgCData(oldp+566,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                        >> 0x1dU))),2);
        bufp->chgCData(oldp+567,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                        >> 0x1bU))),2);
        bufp->chgCData(oldp+568,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                        >> 0x19U))),2);
        bufp->chgBit(oldp+569,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 0x18U))));
        bufp->chgBit(oldp+570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 0x17U))));
        bufp->chgBit(oldp+571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 0x16U))));
        bufp->chgBit(oldp+572,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 0x15U))));
        bufp->chgBit(oldp+573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 0x14U))));
        bufp->chgBit(oldp+574,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 0x13U))));
        bufp->chgCData(oldp+575,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                        >> 0x11U))),2);
        bufp->chgBit(oldp+576,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                      >> 0x10U))));
        bufp->chgCData(oldp+577,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xeU] 
                                        >> 5U))),3);
        bufp->chgCData(oldp+578,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xeU] 
                                        >> 3U))),2);
        bufp->chgCData(oldp+579,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xeU])),3);
        bufp->chgBit(oldp+580,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                >> 0x1fU)));
        bufp->chgCData(oldp+581,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                           >> 0x1aU))),5);
        bufp->chgBit(oldp+582,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 0x19U))));
        bufp->chgCData(oldp+583,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                           >> 0x14U))),5);
        bufp->chgBit(oldp+584,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 0x13U))));
        bufp->chgCData(oldp+585,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                           >> 0xeU))),5);
        bufp->chgCData(oldp+586,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                          >> 0xaU))),4);
        bufp->chgBit(oldp+587,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 9U))));
        bufp->chgIData(oldp+588,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                                  << 0x15U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                                    >> 0xbU)))),30);
        bufp->chgBit(oldp+589,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 0xdU))));
        bufp->chgBit(oldp+590,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 0xcU))));
        bufp->chgBit(oldp+591,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 0xbU))));
        bufp->chgCData(oldp+592,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                        >> 9U))),2);
        bufp->chgCData(oldp+593,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                           >> 4U))),5);
        bufp->chgBit(oldp+594,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 3U))));
        bufp->chgCData(oldp+595,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                        >> 1U))),2);
        bufp->chgSData(oldp+596,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                             << 9U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                               >> 0x17U)))),10);
        bufp->chgSData(oldp+597,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                            >> 0xbU))),12);
        bufp->chgSData(oldp+598,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                                >> 0x1fU)))),15);
        bufp->chgIData(oldp+599,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                              >> 0xbU))),20);
        bufp->chgCData(oldp+600,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                        >> 0xbU))),2);
        bufp->chgSData(oldp+601,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                                >> 0x19U)))),16);
        bufp->chgSData(oldp+602,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                             >> 0xbU))),14);
        bufp->chgSData(oldp+603,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                              << 3U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                                >> 0x1dU)))),15);
        bufp->chgIData(oldp+604,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                              >> 0xbU))),18);
        bufp->chgCData(oldp+605,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                        >> 0xbU))),3);
        bufp->chgBit(oldp+606,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                      >> 0xaU))));
        bufp->chgIData(oldp+607,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                               << 9U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                                 >> 0x17U)))),19);
        bufp->chgCData(oldp+608,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                           >> 8U))),5);
        bufp->chgCData(oldp+609,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                           >> 3U))),5);
        bufp->chgCData(oldp+610,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU])),3);
        bufp->chgIData(oldp+611,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                  >> 0xbU)),21);
        bufp->chgCData(oldp+612,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                        >> 9U))),2);
        bufp->chgCData(oldp+613,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                        >> 7U))),2);
        bufp->chgCData(oldp+614,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                        >> 5U))),2);
        bufp->chgBit(oldp+615,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                      >> 4U))));
        bufp->chgBit(oldp+616,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                      >> 3U))));
        bufp->chgBit(oldp+617,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                      >> 2U))));
        bufp->chgBit(oldp+618,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                      >> 1U))));
        bufp->chgBit(oldp+619,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU])));
        bufp->chgBit(oldp+620,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                >> 0x1fU)));
        bufp->chgCData(oldp+621,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                        >> 0x1dU))),2);
        bufp->chgBit(oldp+622,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+623,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 4U))));
        bufp->chgBit(oldp+624,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 3U))));
        bufp->chgBit(oldp+625,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 2U))));
        bufp->chgBit(oldp+626,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 1U))));
        bufp->chgBit(oldp+627,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo))));
        bufp->chgBit(oldp+628,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 9U))));
        bufp->chgBit(oldp+629,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 8U))));
        bufp->chgBit(oldp+630,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 7U))));
        bufp->chgBit(oldp+631,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 6U))));
        bufp->chgBit(oldp+632,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                      >> 5U))));
        bufp->chgBit(oldp+633,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__initiate));
        bufp->chgCData(oldp+634,((0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0x19U)))),7);
        bufp->chgCData(oldp+635,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0x14U)))),5);
        bufp->chgCData(oldp+636,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0xfU)))),5);
        bufp->chgCData(oldp+637,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                >> 0xcU)))),3);
        bufp->chgCData(oldp+638,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 7U)))),5);
        bufp->chgCData(oldp+639,((0x7fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn))),7);
        bufp->chgCData(oldp+640,((0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0x39U)))),7);
        bufp->chgCData(oldp+641,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0x34U)))),5);
        bufp->chgCData(oldp+642,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0x2fU)))),5);
        bufp->chgCData(oldp+643,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                >> 0x2cU)))),3);
        bufp->chgCData(oldp+644,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0x27U)))),5);
        bufp->chgCData(oldp+645,((0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                   >> 0x20U)))),7);
        bufp->chgBit(oldp+646,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidIn[0]));
        bufp->chgBit(oldp+647,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidIn[1]));
        bufp->chgBit(oldp+648,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U])));
        bufp->chgIData(oldp+649,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U] 
                                  >> 0xdU)),19);
        bufp->chgBit(oldp+650,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U] 
                                      >> 0xcU))));
        bufp->chgSData(oldp+651,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+652,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U])),2);
        bufp->chgBit(oldp+653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[2U] 
                                      >> 1U))));
        bufp->chgIData(oldp+654,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[2U] 
                                               << 0x12U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                                 >> 0xeU)))),19);
        bufp->chgBit(oldp+655,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                      >> 0xdU))));
        bufp->chgSData(oldp+656,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                            >> 3U))),10);
        bufp->chgCData(oldp+657,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                        >> 1U))),2);
        bufp->chgBit(oldp+658,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                      [0U] >> 0x13U))));
        bufp->chgIData(oldp+659,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                  [0U])),19);
        bufp->chgBit(oldp+660,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                      [1U] >> 0x13U))));
        bufp->chgIData(oldp+661,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                  [1U])),19);
        bufp->chgBit(oldp+662,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidOut[0]));
        bufp->chgBit(oldp+663,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidOut[1]));
        bufp->chgBit(oldp+664,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushed[0]));
        bufp->chgBit(oldp+665,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushed[1]));
        bufp->chgBit(oldp+666,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushTriggering[0]));
        bufp->chgBit(oldp+667,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushTriggering[1]));
        bufp->chgBit(oldp+668,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__flushTriggered));
        bufp->chgBit(oldp+669,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+670,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                      [0U] 
                                                      >> 0xdU)))),19);
        bufp->chgBit(oldp+671,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                              [0U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+672,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                    [0U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+673,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                               [0U]))),2);
        bufp->chgBit(oldp+674,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+675,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                      [1U] 
                                                      >> 0xdU)))),19);
        bufp->chgBit(oldp+676,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                              [1U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+677,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                    [1U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+678,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                               [1U]))),2);
        bufp->chgBit(oldp+679,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__recoveredPC 
                                      >> 0x13U))));
        bufp->chgIData(oldp+680,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__recoveredPC)),19);
        bufp->chgCData(oldp+681,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__remainingValidMOps),6);
        bufp->chgCData(oldp+682,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__curValidMOps),6);
        bufp->chgCData(oldp+683,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pickedValidMOps),6);
        bufp->chgCData(oldp+684,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__serializedMOps),6);
        bufp->chgCData(oldp+685,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPickedIndex[0]),3);
        bufp->chgCData(oldp+686,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPickedIndex[1]),3);
        bufp->chgBit(oldp+687,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPicked[0]));
        bufp->chgBit(oldp+688,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPicked[1]));
        bufp->chgBit(oldp+689,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                      [0U] >> 0x13U))));
        bufp->chgIData(oldp+690,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                  [0U])),19);
        bufp->chgBit(oldp+691,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                      [1U] >> 0x13U))));
        bufp->chgIData(oldp+692,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                  [1U])),19);
        bufp->chgBit(oldp+693,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                      [2U] >> 0x13U))));
        bufp->chgIData(oldp+694,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                  [2U])),19);
        bufp->chgBit(oldp+695,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                      [3U] >> 0x13U))));
        bufp->chgIData(oldp+696,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                  [3U])),19);
        bufp->chgBit(oldp+697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS 
                                      >> 0x13U))));
        bufp->chgIData(oldp+698,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS)),19);
        bufp->chgBit(oldp+699,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS));
        bufp->chgBit(oldp+700,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__popRAS));
        bufp->chgCData(oldp+701,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr),2);
        bufp->chgCData(oldp+702,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS_Ptr),2);
        bufp->chgBit(oldp+703,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                      [0U] >> 0x13U))));
        bufp->chgIData(oldp+704,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                  [0U])),19);
        bufp->chgBit(oldp+705,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                      [1U] >> 0x13U))));
        bufp->chgIData(oldp+706,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                  [1U])),19);
        bufp->chgBit(oldp+707,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                      [0U] >> 0x13U))));
        bufp->chgIData(oldp+708,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                  [0U])),19);
        bufp->chgBit(oldp+709,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                      [1U] >> 0x13U))));
        bufp->chgIData(oldp+710,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                  [1U])),19);
        bufp->chgIData(oldp+711,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                  [0U] >> 0xcU)),20);
        bufp->chgCData(oldp+712,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                           [0U] >> 7U))),5);
        bufp->chgCData(oldp+713,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                  [0U])),7);
        bufp->chgIData(oldp+714,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                  [1U] >> 0xcU)),20);
        bufp->chgCData(oldp+715,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                           [1U] >> 7U))),5);
        bufp->chgCData(oldp+716,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                  [1U])),7);
        bufp->chgBit(oldp+717,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck));
        bufp->chgBit(oldp+718,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrIncorrect));
        bufp->chgBit(oldp+719,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane));
        bufp->chgCData(oldp+720,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                                 [0U]),2);
        bufp->chgCData(oldp+721,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                                 [1U]),2);
        bufp->chgBit(oldp+722,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch[0]));
        bufp->chgBit(oldp+723,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch[1]));
        bufp->chgIData(oldp+724,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+725,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+726,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+727,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+728,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+729,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+730,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i),32);
        bufp->chgIData(oldp+731,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk8__DOT__i),32);
        bufp->chgBit(oldp+732,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__clear));
        bufp->chgBit(oldp+733,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__sent));
        bufp->chgCData(oldp+734,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__cur),6);
        bufp->chgIData(oldp+735,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+736,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__mn),32);
        bufp->chgIData(oldp+737,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+738,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk9__DOT__i),32);
        bufp->chgBit(oldp+739,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__empty));
        bufp->chgBit(oldp+740,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regStall));
        bufp->chgSData(oldp+741,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                            [0U] >> 0x15U))),10);
        bufp->chgBit(oldp+742,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                      [0U] >> 0x14U))));
        bufp->chgBit(oldp+743,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                      [0U] >> 0x13U))));
        bufp->chgIData(oldp+744,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                  [0U])),19);
        bufp->chgSData(oldp+745,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                            [1U] >> 0x15U))),10);
        bufp->chgBit(oldp+746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                      [1U] >> 0x14U))));
        bufp->chgBit(oldp+747,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                      [1U] >> 0x13U))));
        bufp->chgIData(oldp+748,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                  [1U])),19);
        bufp->chgBit(oldp+749,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+750,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                      [0U] 
                                                      >> 0xdU)))),19);
        bufp->chgBit(oldp+751,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                              [0U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+752,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                    [0U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+753,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                               [0U]))),2);
        bufp->chgBit(oldp+754,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+755,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                      [1U] 
                                                      >> 0xdU)))),19);
        bufp->chgBit(oldp+756,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                              [1U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+757,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                    [1U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+758,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                               [1U]))),2);
        bufp->chgIData(oldp+759,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__fetchAddrOut),32);
        bufp->chgIData(oldp+760,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+761,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+762,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+763,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgBit(oldp+764,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                      [0U] >> 4U))));
        bufp->chgCData(oldp+765,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                  [0U])),4);
        bufp->chgBit(oldp+766,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                      [1U] >> 4U))));
        bufp->chgCData(oldp+767,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                  [1U])),4);
        bufp->chgIData(oldp+768,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+769,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+770,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRwStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgQData(oldp+771,((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[3U])) 
                                   << 0x20U) | (QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[2U])))),64);
        bufp->chgIData(oldp+773,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[3U]),32);
        bufp->chgIData(oldp+774,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[2U]),32);
        bufp->chgQData(oldp+775,((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[1U])) 
                                   << 0x20U) | (QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[0U])))),64);
        bufp->chgIData(oldp+777,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[1U]),32);
        bufp->chgIData(oldp+778,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[0U]),32);
        bufp->chgQData(oldp+779,((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[3U])) 
                                   << 0x20U) | (QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[2U])))),64);
        bufp->chgIData(oldp+781,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[3U]),32);
        bufp->chgIData(oldp+782,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[2U]),32);
        bufp->chgQData(oldp+783,((((QData)((IData)(
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[1U])) 
                                   << 0x20U) | (QData)((IData)(
                                                               vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[0U])))),64);
        bufp->chgIData(oldp+785,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[1U]),32);
        bufp->chgIData(oldp+786,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[0U]),32);
        bufp->chgIData(oldp+787,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__phyRawWriteAddr),20);
        bufp->chgCData(oldp+788,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead),4);
        bufp->chgCData(oldp+789,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail),4);
        bufp->chgCData(oldp+790,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regCount),5);
        bufp->chgBit(oldp+791,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+792,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+793,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [0U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+794,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+795,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+796,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0U]))),19);
        bufp->chgBit(oldp+797,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [1U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+798,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [1U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+799,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [1U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+800,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [1U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+801,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [1U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+802,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [1U]))),19);
        bufp->chgBit(oldp+803,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [2U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+804,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [2U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+805,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [2U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+806,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [2U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+807,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [2U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+808,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [2U]))),19);
        bufp->chgBit(oldp+809,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [3U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+810,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [3U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+811,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [3U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+812,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [3U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+813,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [3U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+814,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [3U]))),19);
        bufp->chgBit(oldp+815,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [4U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+816,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [4U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+817,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [4U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+818,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [4U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+819,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [4U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+820,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [4U]))),19);
        bufp->chgBit(oldp+821,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [5U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+822,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [5U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+823,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [5U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+824,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [5U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+825,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [5U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+826,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [5U]))),19);
        bufp->chgBit(oldp+827,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [6U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+828,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [6U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+829,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [6U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+830,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [6U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+831,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [6U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+832,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [6U]))),19);
        bufp->chgBit(oldp+833,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [7U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+834,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [7U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+835,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [7U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+836,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [7U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+837,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [7U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+838,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [7U]))),19);
        bufp->chgBit(oldp+839,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [8U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+840,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [8U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+841,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [8U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+842,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [8U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+843,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [8U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+844,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [8U]))),19);
        bufp->chgBit(oldp+845,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [9U] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+846,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [9U] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+847,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [9U] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+848,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [9U] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+849,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [9U] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+850,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [9U]))),19);
        bufp->chgBit(oldp+851,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xaU] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+852,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xaU] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+853,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [0xaU] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+854,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xaU] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+855,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xaU] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+856,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xaU]))),19);
        bufp->chgBit(oldp+857,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xbU] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+858,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xbU] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+859,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [0xbU] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+860,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xbU] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+861,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xbU] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+862,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xbU]))),19);
        bufp->chgBit(oldp+863,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xcU] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+864,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xcU] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+865,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [0xcU] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+866,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xcU] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+867,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xcU] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+868,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xcU]))),19);
        bufp->chgBit(oldp+869,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xdU] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+870,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xdU] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+871,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [0xdU] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+872,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xdU] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+873,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xdU] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+874,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xdU]))),19);
        bufp->chgBit(oldp+875,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xeU] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+876,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xeU] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+877,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [0xeU] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+878,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xeU] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+879,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xeU] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+880,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xeU]))),19);
        bufp->chgBit(oldp+881,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xfU] 
                                              >> 0x2aU)))));
        bufp->chgBit(oldp+882,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xfU] 
                                              >> 0x29U)))));
        bufp->chgIData(oldp+883,((0xfffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                      [0xfU] 
                                                      >> 0x15U)))),20);
        bufp->chgBit(oldp+884,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xfU] 
                                              >> 0x14U)))));
        bufp->chgBit(oldp+885,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                              [0xfU] 
                                              >> 0x13U)))));
        bufp->chgIData(oldp+886,((0x7ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xfU]))),19);
        bufp->chgBit(oldp+887,((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                             >> 0x15U)))));
        bufp->chgCData(oldp+888,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                          >> 9U))),4);
        bufp->chgIData(oldp+889,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+890,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+891,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk4__DOT__i),32);
        bufp->chgBit(oldp+892,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__storeLoadForwardedReg[0]));
        bufp->chgIData(oldp+893,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__forwardedLoadDataReg[0]),32);
        bufp->chgBit(oldp+894,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__mshrReadHitReg[0]));
        bufp->chgQData(oldp+895,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__mshrReadDataReg[0]),64);
        bufp->chgIData(oldp+897,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__loadAddrReg[0]),32);
        bufp->chgBit(oldp+898,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__loadMemAccessSizeReg
                                      [0U] >> 2U))));
        bufp->chgCData(oldp+899,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__loadMemAccessSizeReg
                                  [0U])),2);
        bufp->chgIData(oldp+900,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+901,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+902,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgBit(oldp+903,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                      [0U] >> 4U))));
        bufp->chgCData(oldp+904,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                  [0U])),4);
        bufp->chgBit(oldp+905,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                      [1U] >> 4U))));
        bufp->chgCData(oldp+906,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                  [1U])),4);
        bufp->chgIData(oldp+907,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+908,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+909,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRwStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgCData(oldp+910,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__reqSerial),2);
        bufp->chgCData(oldp+911,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__resultSerial),2);
        bufp->chgCData(oldp+912,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__nextResultSerial),2);
        bufp->chgSData(oldp+913,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                             [0U][7U] 
                                             << 6U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                               [0U][6U] 
                                               >> 0x1aU)))),10);
        bufp->chgCData(oldp+914,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+915,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][6U] >> 0x17U))));
        bufp->chgSData(oldp+916,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                            [0U][6U] 
                                            >> 0xdU))),10);
        bufp->chgCData(oldp+917,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][6U] >> 0xbU))),2);
        bufp->chgCData(oldp+918,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][6U] >> 8U))),3);
        bufp->chgCData(oldp+919,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][6U] >> 5U))),3);
        bufp->chgCData(oldp+920,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][6U] >> 3U))),2);
        bufp->chgCData(oldp+921,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][6U] >> 1U))),2);
        bufp->chgSData(oldp+922,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                             [0U][6U] 
                                             << 0xbU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                               [0U][5U] 
                                               >> 0x15U)))),12);
        bufp->chgBit(oldp+923,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][5U] >> 0x14U))));
        bufp->chgBit(oldp+924,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][5U] >> 0x13U))));
        bufp->chgBit(oldp+925,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][5U] >> 0x12U))));
        bufp->chgCData(oldp+926,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][5U] >> 0x10U))),2);
        bufp->chgCData(oldp+927,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][5U] 
                                           >> 0xbU))),5);
        bufp->chgBit(oldp+928,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][5U] >> 0xaU))));
        bufp->chgCData(oldp+929,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][5U] >> 8U))),2);
        bufp->chgCData(oldp+930,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][5U] >> 5U))),3);
        bufp->chgBit(oldp+931,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][5U] >> 4U))));
        bufp->chgCData(oldp+932,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                  [0U][5U])),4);
        bufp->chgCData(oldp+933,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                  [0U][4U] >> 0x1cU)),4);
        bufp->chgBit(oldp+934,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+935,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][4U] >> 0x1aU))));
        bufp->chgCData(oldp+936,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][4U] 
                                           >> 0x14U))),6);
        bufp->chgCData(oldp+937,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                          [0U][4U] 
                                          >> 0x10U))),4);
        bufp->chgCData(oldp+938,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                          [0U][4U] 
                                          >> 0xcU))),4);
        bufp->chgBit(oldp+939,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][4U] >> 0xbU))));
        bufp->chgCData(oldp+940,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][4U] 
                                           >> 5U))),6);
        bufp->chgBit(oldp+941,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][4U] >> 4U))));
        bufp->chgCData(oldp+942,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                            [0U][4U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                              [0U][3U] 
                                              >> 0x1eU)))),6);
        bufp->chgBit(oldp+943,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][3U] >> 0x1dU))));
        bufp->chgCData(oldp+944,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][3U] 
                                           >> 0x17U))),6);
        bufp->chgBit(oldp+945,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][3U] >> 0x16U))));
        bufp->chgBit(oldp+946,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][3U] >> 0x15U))));
        bufp->chgCData(oldp+947,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][3U] 
                                           >> 0xfU))),6);
        bufp->chgBit(oldp+948,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][3U] >> 0xeU))));
        bufp->chgIData(oldp+949,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                               [0U][3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                                 [0U][2U] 
                                                 >> 0x1bU)))),19);
        bufp->chgBit(oldp+950,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+951,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+952,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][2U] >> 0x18U))));
        bufp->chgIData(oldp+953,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                   [0U][2U] << 8U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][1U] >> 0x18U))),32);
        bufp->chgIData(oldp+954,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                   [0U][1U] << 8U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][0U] >> 0x18U))),32);
        bufp->chgCData(oldp+955,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                        [0U][0U] >> 0x16U))),2);
        bufp->chgBit(oldp+956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][0U] >> 0x15U))));
        bufp->chgBit(oldp+957,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+958,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                  [0U][0U])),20);
        bufp->chgSData(oldp+959,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                             [0U][7U] 
                                             << 6U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                               [0U][6U] 
                                               >> 0x1aU)))),10);
        bufp->chgCData(oldp+960,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+961,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][6U] >> 0x17U))));
        bufp->chgSData(oldp+962,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                            [0U][6U] 
                                            >> 0xdU))),10);
        bufp->chgCData(oldp+963,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][6U] >> 0xbU))),2);
        bufp->chgCData(oldp+964,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][6U] >> 8U))),3);
        bufp->chgCData(oldp+965,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][6U] >> 5U))),3);
        bufp->chgCData(oldp+966,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][6U] >> 3U))),2);
        bufp->chgCData(oldp+967,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][6U] >> 1U))),2);
        bufp->chgSData(oldp+968,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                             [0U][6U] 
                                             << 0xbU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                               [0U][5U] 
                                               >> 0x15U)))),12);
        bufp->chgBit(oldp+969,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][5U] >> 0x14U))));
        bufp->chgBit(oldp+970,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][5U] >> 0x13U))));
        bufp->chgBit(oldp+971,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][5U] >> 0x12U))));
        bufp->chgCData(oldp+972,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][5U] >> 0x10U))),2);
        bufp->chgCData(oldp+973,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][5U] 
                                           >> 0xbU))),5);
        bufp->chgBit(oldp+974,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][5U] >> 0xaU))));
        bufp->chgCData(oldp+975,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][5U] >> 8U))),2);
        bufp->chgCData(oldp+976,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                        [0U][5U] >> 5U))),3);
        bufp->chgBit(oldp+977,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][5U] >> 4U))));
        bufp->chgCData(oldp+978,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                  [0U][5U])),4);
        bufp->chgCData(oldp+979,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                  [0U][4U] >> 0x1cU)),4);
        bufp->chgBit(oldp+980,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+981,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][4U] >> 0x1aU))));
        bufp->chgCData(oldp+982,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][4U] 
                                           >> 0x14U))),6);
        bufp->chgCData(oldp+983,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                          [0U][4U] 
                                          >> 0x10U))),4);
        bufp->chgCData(oldp+984,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                          [0U][4U] 
                                          >> 0xcU))),4);
        bufp->chgBit(oldp+985,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][4U] >> 0xbU))));
        bufp->chgCData(oldp+986,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][4U] 
                                           >> 5U))),6);
        bufp->chgBit(oldp+987,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][4U] >> 4U))));
        bufp->chgCData(oldp+988,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                            [0U][4U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                              [0U][3U] 
                                              >> 0x1eU)))),6);
        bufp->chgBit(oldp+989,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][3U] >> 0x1dU))));
        bufp->chgCData(oldp+990,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][3U] 
                                           >> 0x17U))),6);
        bufp->chgBit(oldp+991,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][3U] >> 0x16U))));
        bufp->chgBit(oldp+992,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][3U] >> 0x15U))));
        bufp->chgCData(oldp+993,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][3U] 
                                           >> 0xfU))),6);
        bufp->chgBit(oldp+994,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][3U] >> 0xeU))));
        bufp->chgIData(oldp+995,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                               [0U][3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                                 [0U][2U] 
                                                 >> 0x1bU)))),19);
        bufp->chgBit(oldp+996,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+997,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+998,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][2U] >> 0x18U))));
        bufp->chgIData(oldp+999,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                   [0U][2U] << 8U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][1U] >> 0x18U))),32);
        bufp->chgIData(oldp+1000,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                    [0U][1U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                      [0U][0U] >> 0x18U))),32);
        bufp->chgCData(oldp+1001,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                         [0U][0U] >> 0x16U))),2);
        bufp->chgBit(oldp+1002,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1003,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1004,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                   [0U][0U])),20);
        bufp->chgSData(oldp+1005,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1006,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+1007,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+1008,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+1009,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+1010,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+1011,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+1012,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1013,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1014,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+1015,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+1016,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+1017,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+1018,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+1019,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+1020,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+1021,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+1022,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+1023,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+1024,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                 [0U][2U])));
        bufp->chgCData(oldp+1025,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1026,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1027,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1028,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+1029,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1030,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+1031,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1032,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+1033,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1034,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+1035,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+1036,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1037,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1038,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1039,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                 [0U][0U])));
        bufp->chgSData(oldp+1040,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1041,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+1042,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+1043,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+1044,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+1045,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+1046,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+1047,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1048,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1049,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+1050,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+1051,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+1052,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+1053,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+1054,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+1055,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+1056,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+1057,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+1058,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+1059,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                 [0U][2U])));
        bufp->chgCData(oldp+1060,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1061,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1062,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1063,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+1064,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1065,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+1066,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1067,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+1068,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1069,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+1070,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+1071,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1072,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1073,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1074,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                 [0U][0U])));
        bufp->chgIData(oldp+1075,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgCData(oldp+1076,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__regPhase
                                  [0U]),2);
        bufp->chgCData(oldp+1077,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__regActiveListPtr[0]),6);
        bufp->chgBit(oldp+1078,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regIsSigned));
        bufp->chgIData(oldp+1079,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDividend),32);
        bufp->chgIData(oldp+1080,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivisor),32);
        bufp->chgCData(oldp+1081,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivCode),2);
        bufp->chgQData(oldp+1082,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ),33);
        bufp->chgQData(oldp+1084,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD),33);
        bufp->chgQData(oldp+1086,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ),33);
        bufp->chgQData(oldp+1088,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR),33);
        bufp->chgBit(oldp+1090,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regSigned));
        bufp->chgCData(oldp+1091,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter),6);
        bufp->chgCData(oldp+1092,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase),2);
        VL_EXTENDS_WQ(66,33, __Vtemp_32, vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcA_Reg);
        __Vtemp_33[0U] = __Vtemp_32[0U];
        __Vtemp_33[1U] = __Vtemp_32[1U];
        __Vtemp_33[2U] = (3U & __Vtemp_32[2U]);
        VL_EXTENDS_WQ(66,33, __Vtemp_35, vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcB_Reg);
        __Vtemp_36[0U] = __Vtemp_35[0U];
        __Vtemp_36[1U] = __Vtemp_35[1U];
        __Vtemp_36[2U] = (3U & __Vtemp_35[2U]);
        VL_MULS_WWW(66, __Vtemp_37, __Vtemp_33, __Vtemp_36);
        __Vtemp_38[0U] = __Vtemp_37[0U];
        __Vtemp_38[1U] = __Vtemp_37[1U];
        __Vtemp_38[2U] = (3U & __Vtemp_37[2U]);
        bufp->chgWData(oldp+1093,(__Vtemp_38),66);
        bufp->chgQData(oldp+1096,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcA_Reg),33);
        bufp->chgQData(oldp+1098,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcB_Reg),33);
        bufp->chgIData(oldp+1100,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+1101,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+1102,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+1103,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+1104,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__unnamedblk1__DOT__i),32);
        bufp->chgBit(oldp+1105,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__threadCounter));
        bufp->chgSData(oldp+1106,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__sidFF__DOT__body),10);
        bufp->chgBit(oldp+1107,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__regStall));
        bufp->chgBit(oldp+1108,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                       [0U] >> 0x13U))));
        bufp->chgIData(oldp+1109,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                   [0U])),19);
        bufp->chgBit(oldp+1110,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                       [1U] >> 0x13U))));
        bufp->chgIData(oldp+1111,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                   [1U])),19);
        bufp->chgIData(oldp+1112,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__pc[0]),32);
        bufp->chgIData(oldp+1113,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__pc[1]),32);
        bufp->chgBit(oldp+1114,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC[0]));
        bufp->chgBit(oldp+1115,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC[1]));
        bufp->chgCData(oldp+1116,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1117,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1118,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1119,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1120,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+1121,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+1122,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+1123,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0x17U))));
        bufp->chgCData(oldp+1124,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            >> 0x12U))),5);
        bufp->chgCData(oldp+1125,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1126,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0xdU))));
        bufp->chgIData(oldp+1127,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                                   << 0x11U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                     >> 0xfU)))),30);
        bufp->chgBit(oldp+1128,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1129,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1130,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1131,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1132,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            >> 8U))),5);
        bufp->chgBit(oldp+1133,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1134,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                         >> 5U))),2);
        bufp->chgSData(oldp+1135,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                >> 0x1bU)))),10);
        bufp->chgSData(oldp+1136,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                             >> 0xfU))),12);
        bufp->chgSData(oldp+1137,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                              >> 3U))),15);
        bufp->chgIData(oldp+1138,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                  >> 0xfU)))),20);
        bufp->chgCData(oldp+1139,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                         >> 0xfU))),2);
        bufp->chgSData(oldp+1140,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                 >> 0x1dU)))),16);
        bufp->chgSData(oldp+1141,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                              >> 0xfU))),14);
        bufp->chgSData(oldp+1142,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+1143,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                  >> 0xfU)))),18);
        bufp->chgCData(oldp+1144,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                         >> 0xfU))),3);
        bufp->chgBit(oldp+1145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0xeU))));
        bufp->chgIData(oldp+1146,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                  >> 0x1bU)))),19);
        bufp->chgCData(oldp+1147,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1148,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1149,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                         >> 4U))),3);
        bufp->chgIData(oldp+1150,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                   >> 0xfU)))),21);
        bufp->chgCData(oldp+1151,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1152,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1153,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                         >> 9U))),2);
        bufp->chgBit(oldp+1154,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1155,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 7U))));
        bufp->chgBit(oldp+1156,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 6U))));
        bufp->chgBit(oldp+1157,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 5U))));
        bufp->chgBit(oldp+1158,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 4U))));
        bufp->chgBit(oldp+1159,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1160,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1161,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U])));
        bufp->chgCData(oldp+1162,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                         >> 0x15U))),3);
        bufp->chgCData(oldp+1163,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                         >> 0x13U))),2);
        bufp->chgCData(oldp+1164,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                         >> 0x10U))),3);
        bufp->chgBit(oldp+1165,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1166,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                            >> 0xaU))),5);
        bufp->chgBit(oldp+1167,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 9U))));
        bufp->chgCData(oldp+1168,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                            >> 4U))),5);
        bufp->chgBit(oldp+1169,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1170,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                               >> 0x1eU)))),5);
        bufp->chgCData(oldp+1171,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                           >> 0x1aU))),4);
        bufp->chgBit(oldp+1172,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x19U))));
        bufp->chgIData(oldp+1173,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                                   << 5U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                     >> 0x1bU)))),30);
        bufp->chgBit(oldp+1174,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1175,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1176,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1177,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+1178,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                            >> 0x14U))),5);
        bufp->chgBit(oldp+1179,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1180,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                         >> 0x11U))),2);
        bufp->chgSData(oldp+1181,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                             >> 7U))),10);
        bufp->chgSData(oldp+1182,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                >> 0x1bU)))),12);
        bufp->chgSData(oldp+1183,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                              >> 0xfU))),15);
        bufp->chgIData(oldp+1184,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                  >> 0x1bU)))),20);
        bufp->chgCData(oldp+1185,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                         >> 0x1bU))),2);
        bufp->chgSData(oldp+1186,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                              >> 9U))),16);
        bufp->chgSData(oldp+1187,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                 >> 0x1bU)))),14);
        bufp->chgSData(oldp+1188,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                              >> 0xdU))),15);
        bufp->chgIData(oldp+1189,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                  >> 0x1bU)))),18);
        bufp->chgCData(oldp+1190,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                         >> 0x1bU))),3);
        bufp->chgBit(oldp+1191,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x1aU))));
        bufp->chgIData(oldp+1192,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                               >> 7U))),19);
        bufp->chgCData(oldp+1193,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                            >> 0x18U))),5);
        bufp->chgCData(oldp+1194,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                            >> 0x13U))),5);
        bufp->chgCData(oldp+1195,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                         >> 0x10U))),3);
        bufp->chgIData(oldp+1196,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                   >> 0x1bU)))),21);
        bufp->chgCData(oldp+1197,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+1198,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+1199,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                         >> 0x15U))),2);
        bufp->chgBit(oldp+1200,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+1201,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x13U))));
        bufp->chgBit(oldp+1202,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x12U))));
        bufp->chgBit(oldp+1203,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1204,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1205,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1206,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                         >> 0xdU))),2);
        bufp->chgBit(oldp+1207,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0xcU))));
        bufp->chgCData(oldp+1208,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                         >> 1U))),3);
        bufp->chgCData(oldp+1209,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+1210,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                         >> 0x1cU))),3);
        bufp->chgBit(oldp+1211,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1212,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                            >> 0x16U))),5);
        bufp->chgBit(oldp+1213,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 0x15U))));
        bufp->chgCData(oldp+1214,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                            >> 0x10U))),5);
        bufp->chgBit(oldp+1215,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1216,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                            >> 0xaU))),5);
        bufp->chgCData(oldp+1217,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                           >> 6U))),4);
        bufp->chgBit(oldp+1218,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 5U))));
        bufp->chgIData(oldp+1219,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                                   << 0x19U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                     >> 7U)))),30);
        bufp->chgBit(oldp+1220,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 9U))));
        bufp->chgBit(oldp+1221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1222,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1223,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1224,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U])),5);
        bufp->chgBit(oldp+1225,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                 >> 0x1fU)));
        bufp->chgCData(oldp+1226,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                         >> 0x1dU))),2);
        bufp->chgSData(oldp+1227,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                             >> 0x13U))),10);
        bufp->chgSData(oldp+1228,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                             >> 7U))),12);
        bufp->chgSData(oldp+1229,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                 >> 0x1bU)))),15);
        bufp->chgIData(oldp+1230,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                               >> 7U))),20);
        bufp->chgCData(oldp+1231,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                         >> 7U))),2);
        bufp->chgSData(oldp+1232,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                               << 0xbU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                 >> 0x15U)))),16);
        bufp->chgSData(oldp+1233,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                              >> 7U))),14);
        bufp->chgSData(oldp+1234,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                               << 7U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                 >> 0x19U)))),15);
        bufp->chgIData(oldp+1235,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                               >> 7U))),18);
        bufp->chgCData(oldp+1236,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                         >> 7U))),3);
        bufp->chgBit(oldp+1237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 6U))));
        bufp->chgIData(oldp+1238,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                                << 0xdU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                  >> 0x13U)))),19);
        bufp->chgCData(oldp+1239,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                            >> 4U))),5);
        bufp->chgCData(oldp+1240,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                               >> 0x1fU)))),5);
        bufp->chgCData(oldp+1241,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                         >> 0x1cU))),3);
        bufp->chgIData(oldp+1242,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                >> 7U))),21);
        bufp->chgCData(oldp+1243,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1244,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                         >> 3U))),2);
        bufp->chgCData(oldp+1245,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1246,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U])));
        bufp->chgBit(oldp+1247,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1248,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+1249,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1250,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1251,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1252,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                         >> 0x19U))),2);
        bufp->chgBit(oldp+1253,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x18U))));
        bufp->chgCData(oldp+1254,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                         >> 0xdU))),3);
        bufp->chgCData(oldp+1255,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1256,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                         >> 8U))),3);
        bufp->chgBit(oldp+1257,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1258,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                            >> 2U))),5);
        bufp->chgBit(oldp+1259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 1U))));
        bufp->chgCData(oldp+1260,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                               >> 0x1cU)))),5);
        bufp->chgBit(oldp+1261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1262,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            >> 0x16U))),5);
        bufp->chgCData(oldp+1263,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1264,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+1265,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                                   << 0xdU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                     >> 0x13U)))),30);
        bufp->chgBit(oldp+1266,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1267,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+1268,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1269,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                         >> 0x11U))),2);
        bufp->chgCData(oldp+1270,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            >> 0xcU))),5);
        bufp->chgBit(oldp+1271,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1272,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                         >> 9U))),2);
        bufp->chgSData(oldp+1273,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                >> 0x1fU)))),10);
        bufp->chgSData(oldp+1274,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                             >> 0x13U))),12);
        bufp->chgSData(oldp+1275,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                              >> 7U))),15);
        bufp->chgIData(oldp+1276,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                                << 0xdU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                  >> 0x13U)))),20);
        bufp->chgCData(oldp+1277,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                         >> 0x13U))),2);
        bufp->chgSData(oldp+1278,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                              >> 1U))),16);
        bufp->chgSData(oldp+1279,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                               << 0xdU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                 >> 0x13U)))),14);
        bufp->chgSData(oldp+1280,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                              >> 5U))),15);
        bufp->chgIData(oldp+1281,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                                << 0xdU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                  >> 0x13U)))),18);
        bufp->chgCData(oldp+1282,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                         >> 0x13U))),3);
        bufp->chgBit(oldp+1283,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x12U))));
        bufp->chgIData(oldp+1284,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                                << 1U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                  >> 0x1fU)))),19);
        bufp->chgCData(oldp+1285,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            >> 0x10U))),5);
        bufp->chgCData(oldp+1286,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            >> 0xbU))),5);
        bufp->chgCData(oldp+1287,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                         >> 8U))),3);
        bufp->chgIData(oldp+1288,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                                 << 0xdU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                   >> 0x13U)))),21);
        bufp->chgCData(oldp+1289,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                         >> 0x11U))),2);
        bufp->chgCData(oldp+1290,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1291,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                         >> 0xdU))),2);
        bufp->chgBit(oldp+1292,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+1293,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 0xbU))));
        bufp->chgBit(oldp+1294,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 0xaU))));
        bufp->chgBit(oldp+1295,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 9U))));
        bufp->chgBit(oldp+1296,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1297,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1298,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                         >> 5U))),2);
        bufp->chgBit(oldp+1299,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 4U))));
        bufp->chgCData(oldp+1300,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                         >> 0x19U))),3);
        bufp->chgCData(oldp+1301,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+1302,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                         >> 0x14U))),3);
        bufp->chgBit(oldp+1303,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1304,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                            >> 0xeU))),5);
        bufp->chgBit(oldp+1305,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 0xdU))));
        bufp->chgCData(oldp+1306,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                            >> 8U))),5);
        bufp->chgBit(oldp+1307,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 7U))));
        bufp->chgCData(oldp+1308,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                            >> 2U))),5);
        bufp->chgCData(oldp+1309,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                              >> 0x1eU)))),4);
        bufp->chgBit(oldp+1310,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                       >> 0x1dU))));
        bufp->chgIData(oldp+1311,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                   << 1U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                     >> 0x1fU)))),30);
        bufp->chgBit(oldp+1312,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 1U))));
        bufp->chgBit(oldp+1313,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU])));
        bufp->chgBit(oldp+1314,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                 >> 0x1fU)));
        bufp->chgCData(oldp+1315,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                         >> 0x1dU))),2);
        bufp->chgCData(oldp+1316,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+1317,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                       >> 0x17U))));
        bufp->chgCData(oldp+1318,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                         >> 0x15U))),2);
        bufp->chgSData(oldp+1319,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                             >> 0xbU))),10);
        bufp->chgSData(oldp+1320,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                >> 0x1fU)))),12);
        bufp->chgSData(oldp+1321,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                               << 0xdU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                 >> 0x13U)))),15);
        bufp->chgIData(oldp+1322,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                << 1U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                  >> 0x1fU)))),20);
        bufp->chgCData(oldp+1323,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                          >> 0x1fU)))),2);
        bufp->chgSData(oldp+1324,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                              >> 0xdU))),16);
        bufp->chgSData(oldp+1325,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                 >> 0x1fU)))),14);
        bufp->chgSData(oldp+1326,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                   >> 0x11U)),15);
        bufp->chgIData(oldp+1327,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                << 1U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                  >> 0x1fU)))),18);
        bufp->chgCData(oldp+1328,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                          >> 0x1fU)))),3);
        bufp->chgBit(oldp+1329,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                       >> 0x1eU))));
        bufp->chgIData(oldp+1330,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                               >> 0xbU))),19);
        bufp->chgCData(oldp+1331,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                               >> 0x1cU)))),5);
        bufp->chgCData(oldp+1332,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                            >> 0x17U))),5);
        bufp->chgCData(oldp+1333,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                         >> 0x14U))),3);
        bufp->chgIData(oldp+1334,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                 << 1U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                   >> 0x1fU)))),21);
        bufp->chgCData(oldp+1335,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                         >> 0x1dU))),2);
        bufp->chgCData(oldp+1336,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                         >> 0x1bU))),2);
        bufp->chgCData(oldp+1337,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                         >> 0x19U))),2);
        bufp->chgBit(oldp+1338,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1339,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1340,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x16U))));
        bufp->chgBit(oldp+1341,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x15U))));
        bufp->chgBit(oldp+1342,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+1343,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1344,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                         >> 0x11U))),2);
        bufp->chgBit(oldp+1345,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x10U))));
        bufp->chgCData(oldp+1346,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xeU] 
                                         >> 5U))),3);
        bufp->chgCData(oldp+1347,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xeU] 
                                         >> 3U))),2);
        bufp->chgCData(oldp+1348,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xeU])),3);
        bufp->chgBit(oldp+1349,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                 >> 0x1fU)));
        bufp->chgCData(oldp+1350,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                            >> 0x1aU))),5);
        bufp->chgBit(oldp+1351,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0x19U))));
        bufp->chgCData(oldp+1352,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                            >> 0x14U))),5);
        bufp->chgBit(oldp+1353,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1354,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                            >> 0xeU))),5);
        bufp->chgCData(oldp+1355,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                           >> 0xaU))),4);
        bufp->chgBit(oldp+1356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 9U))));
        bufp->chgIData(oldp+1357,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                                   << 0x15U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                     >> 0xbU)))),30);
        bufp->chgBit(oldp+1358,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0xdU))));
        bufp->chgBit(oldp+1359,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+1360,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1361,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                         >> 9U))),2);
        bufp->chgCData(oldp+1362,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                            >> 4U))),5);
        bufp->chgBit(oldp+1363,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 3U))));
        bufp->chgCData(oldp+1364,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                         >> 1U))),2);
        bufp->chgSData(oldp+1365,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                >> 0x17U)))),10);
        bufp->chgSData(oldp+1366,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                             >> 0xbU))),12);
        bufp->chgSData(oldp+1367,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                 >> 0x1fU)))),15);
        bufp->chgIData(oldp+1368,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                               >> 0xbU))),20);
        bufp->chgCData(oldp+1369,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                         >> 0xbU))),2);
        bufp->chgSData(oldp+1370,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                               << 7U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                 >> 0x19U)))),16);
        bufp->chgSData(oldp+1371,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                              >> 0xbU))),14);
        bufp->chgSData(oldp+1372,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                 >> 0x1dU)))),15);
        bufp->chgIData(oldp+1373,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                               >> 0xbU))),18);
        bufp->chgCData(oldp+1374,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                         >> 0xbU))),3);
        bufp->chgBit(oldp+1375,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0xaU))));
        bufp->chgIData(oldp+1376,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                                << 9U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                  >> 0x17U)))),19);
        bufp->chgCData(oldp+1377,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                            >> 8U))),5);
        bufp->chgCData(oldp+1378,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                            >> 3U))),5);
        bufp->chgCData(oldp+1379,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU])),3);
        bufp->chgIData(oldp+1380,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                   >> 0xbU)),21);
        bufp->chgCData(oldp+1381,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                         >> 9U))),2);
        bufp->chgCData(oldp+1382,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1383,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                         >> 5U))),2);
        bufp->chgBit(oldp+1384,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                       >> 4U))));
        bufp->chgBit(oldp+1385,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                       >> 3U))));
        bufp->chgBit(oldp+1386,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                       >> 2U))));
        bufp->chgBit(oldp+1387,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                       >> 1U))));
        bufp->chgBit(oldp+1388,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU])));
        bufp->chgBit(oldp+1389,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                 >> 0x1fU)));
        bufp->chgCData(oldp+1390,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                         >> 0x1dU))),2);
        bufp->chgBit(oldp+1391,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1392,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 4U))));
        bufp->chgBit(oldp+1393,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 3U))));
        bufp->chgBit(oldp+1394,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 2U))));
        bufp->chgBit(oldp+1395,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 1U))));
        bufp->chgBit(oldp+1396,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo))));
        bufp->chgBit(oldp+1397,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 9U))));
        bufp->chgBit(oldp+1398,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 8U))));
        bufp->chgBit(oldp+1399,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 7U))));
        bufp->chgBit(oldp+1400,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 6U))));
        bufp->chgBit(oldp+1401,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                       >> 5U))));
        bufp->chgBit(oldp+1402,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__empty));
        bufp->chgBit(oldp+1403,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC
                                [0U]));
        bufp->chgCData(oldp+1404,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1405,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1406,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1408,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+1409,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+1410,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+1411,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0x17U))));
        bufp->chgCData(oldp+1412,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            >> 0x12U))),5);
        bufp->chgCData(oldp+1413,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1414,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0xdU))));
        bufp->chgIData(oldp+1415,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                                   << 0x11U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                     >> 0xfU)))),30);
        bufp->chgBit(oldp+1416,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1417,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1418,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1419,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1420,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            >> 8U))),5);
        bufp->chgBit(oldp+1421,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1422,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                         >> 5U))),2);
        bufp->chgSData(oldp+1423,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                >> 0x1bU)))),10);
        bufp->chgSData(oldp+1424,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                             >> 0xfU))),12);
        bufp->chgSData(oldp+1425,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                              >> 3U))),15);
        bufp->chgIData(oldp+1426,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                  >> 0xfU)))),20);
        bufp->chgCData(oldp+1427,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                         >> 0xfU))),2);
        bufp->chgSData(oldp+1428,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                 >> 0x1dU)))),16);
        bufp->chgSData(oldp+1429,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                              >> 0xfU))),14);
        bufp->chgSData(oldp+1430,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+1431,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                  >> 0xfU)))),18);
        bufp->chgCData(oldp+1432,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                         >> 0xfU))),3);
        bufp->chgBit(oldp+1433,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0xeU))));
        bufp->chgIData(oldp+1434,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                  >> 0x1bU)))),19);
        bufp->chgCData(oldp+1435,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1436,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1437,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                         >> 4U))),3);
        bufp->chgIData(oldp+1438,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                   >> 0xfU)))),21);
        bufp->chgCData(oldp+1439,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1440,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1441,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                         >> 9U))),2);
        bufp->chgBit(oldp+1442,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1443,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 7U))));
        bufp->chgBit(oldp+1444,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 6U))));
        bufp->chgBit(oldp+1445,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 5U))));
        bufp->chgBit(oldp+1446,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 4U))));
        bufp->chgBit(oldp+1447,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1448,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1449,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U])));
        bufp->chgCData(oldp+1450,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                         >> 0x15U))),3);
        bufp->chgCData(oldp+1451,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                         >> 0x13U))),2);
        bufp->chgCData(oldp+1452,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                         >> 0x10U))),3);
        bufp->chgBit(oldp+1453,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1454,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                            >> 0xaU))),5);
        bufp->chgBit(oldp+1455,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 9U))));
        bufp->chgCData(oldp+1456,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                            >> 4U))),5);
        bufp->chgBit(oldp+1457,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1458,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                               >> 0x1eU)))),5);
        bufp->chgCData(oldp+1459,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                           >> 0x1aU))),4);
        bufp->chgBit(oldp+1460,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x19U))));
        bufp->chgIData(oldp+1461,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                                   << 5U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                     >> 0x1bU)))),30);
        bufp->chgBit(oldp+1462,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1463,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1464,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1465,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+1466,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                            >> 0x14U))),5);
        bufp->chgBit(oldp+1467,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1468,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                         >> 0x11U))),2);
        bufp->chgSData(oldp+1469,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                             >> 7U))),10);
        bufp->chgSData(oldp+1470,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                >> 0x1bU)))),12);
        bufp->chgSData(oldp+1471,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                              >> 0xfU))),15);
        bufp->chgIData(oldp+1472,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                  >> 0x1bU)))),20);
        bufp->chgCData(oldp+1473,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                         >> 0x1bU))),2);
        bufp->chgSData(oldp+1474,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                              >> 9U))),16);
        bufp->chgSData(oldp+1475,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                 >> 0x1bU)))),14);
        bufp->chgSData(oldp+1476,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                              >> 0xdU))),15);
        bufp->chgIData(oldp+1477,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                  >> 0x1bU)))),18);
        bufp->chgCData(oldp+1478,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                         >> 0x1bU))),3);
        bufp->chgBit(oldp+1479,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1aU))));
        bufp->chgIData(oldp+1480,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                               >> 7U))),19);
        bufp->chgCData(oldp+1481,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                            >> 0x18U))),5);
        bufp->chgCData(oldp+1482,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                            >> 0x13U))),5);
        bufp->chgCData(oldp+1483,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                         >> 0x10U))),3);
        bufp->chgIData(oldp+1484,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                   >> 0x1bU)))),21);
        bufp->chgCData(oldp+1485,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+1486,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+1487,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                         >> 0x15U))),2);
        bufp->chgBit(oldp+1488,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+1489,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x13U))));
        bufp->chgBit(oldp+1490,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x12U))));
        bufp->chgBit(oldp+1491,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1492,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1493,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1494,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                         >> 0xdU))),2);
        bufp->chgBit(oldp+1495,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0xcU))));
        bufp->chgCData(oldp+1496,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[7U] 
                                         >> 1U))),3);
        bufp->chgCData(oldp+1497,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[7U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+1498,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                         >> 0x1cU))),3);
        bufp->chgBit(oldp+1499,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1500,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                            >> 0x16U))),5);
        bufp->chgBit(oldp+1501,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 0x15U))));
        bufp->chgCData(oldp+1502,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                            >> 0x10U))),5);
        bufp->chgBit(oldp+1503,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1504,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                            >> 0xaU))),5);
        bufp->chgCData(oldp+1505,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                           >> 6U))),4);
        bufp->chgBit(oldp+1506,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 5U))));
        bufp->chgIData(oldp+1507,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                                   << 0x19U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                     >> 7U)))),30);
        bufp->chgBit(oldp+1508,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 9U))));
        bufp->chgBit(oldp+1509,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1510,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1511,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1512,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U])),5);
        bufp->chgBit(oldp+1513,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                 >> 0x1fU)));
        bufp->chgCData(oldp+1514,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                         >> 0x1dU))),2);
        bufp->chgSData(oldp+1515,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                             >> 0x13U))),10);
        bufp->chgSData(oldp+1516,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                             >> 7U))),12);
        bufp->chgSData(oldp+1517,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                 >> 0x1bU)))),15);
        bufp->chgIData(oldp+1518,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                               >> 7U))),20);
        bufp->chgCData(oldp+1519,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                         >> 7U))),2);
        bufp->chgSData(oldp+1520,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                               << 0xbU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                 >> 0x15U)))),16);
        bufp->chgSData(oldp+1521,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                              >> 7U))),14);
        bufp->chgSData(oldp+1522,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                               << 7U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                 >> 0x19U)))),15);
        bufp->chgIData(oldp+1523,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                               >> 7U))),18);
        bufp->chgCData(oldp+1524,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                         >> 7U))),3);
        bufp->chgBit(oldp+1525,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 6U))));
        bufp->chgIData(oldp+1526,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                                << 0xdU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                  >> 0x13U)))),19);
        bufp->chgCData(oldp+1527,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                            >> 4U))),5);
        bufp->chgCData(oldp+1528,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                               >> 0x1fU)))),5);
        bufp->chgCData(oldp+1529,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                         >> 0x1cU))),3);
        bufp->chgIData(oldp+1530,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                >> 7U))),21);
        bufp->chgCData(oldp+1531,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1532,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                         >> 3U))),2);
        bufp->chgCData(oldp+1533,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1534,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U])));
        bufp->chgBit(oldp+1535,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1536,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+1537,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1538,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1539,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1540,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                         >> 0x19U))),2);
        bufp->chgBit(oldp+1541,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1542,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                       >> 4U))));
        bufp->chgBit(oldp+1543,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                       >> 3U))));
        bufp->chgBit(oldp+1544,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                       >> 2U))));
        bufp->chgBit(oldp+1545,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                       >> 1U))));
        bufp->chgBit(oldp+1546,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo))));
        bufp->chgCData(oldp+1547,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                   >> 0x19U)),7);
        bufp->chgCData(oldp+1548,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                            >> 0x14U))),5);
        bufp->chgCData(oldp+1549,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                            >> 0xfU))),5);
        bufp->chgCData(oldp+1550,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+1551,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1552,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf)),7);
        bufp->chgCData(oldp+1553,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__rv32mFunct7),7);
        bufp->chgCData(oldp+1554,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__zbaFunct7),7);
        bufp->chgCData(oldp+1555,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__zicondFunct7),7);
        bufp->chgBit(oldp+1556,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined));
        bufp->chgBit(oldp+1557,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC
                                [1U]));
        bufp->chgCData(oldp+1558,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1559,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1560,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1561,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1562,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+1563,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+1564,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+1565,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0x17U))));
        bufp->chgCData(oldp+1566,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            >> 0x12U))),5);
        bufp->chgCData(oldp+1567,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1568,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0xdU))));
        bufp->chgIData(oldp+1569,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                                   << 0x11U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                     >> 0xfU)))),30);
        bufp->chgBit(oldp+1570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1572,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1573,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1574,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            >> 8U))),5);
        bufp->chgBit(oldp+1575,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1576,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                         >> 5U))),2);
        bufp->chgSData(oldp+1577,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                >> 0x1bU)))),10);
        bufp->chgSData(oldp+1578,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                             >> 0xfU))),12);
        bufp->chgSData(oldp+1579,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                              >> 3U))),15);
        bufp->chgIData(oldp+1580,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                  >> 0xfU)))),20);
        bufp->chgCData(oldp+1581,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                         >> 0xfU))),2);
        bufp->chgSData(oldp+1582,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                 >> 0x1dU)))),16);
        bufp->chgSData(oldp+1583,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                              >> 0xfU))),14);
        bufp->chgSData(oldp+1584,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+1585,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                  >> 0xfU)))),18);
        bufp->chgCData(oldp+1586,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                         >> 0xfU))),3);
        bufp->chgBit(oldp+1587,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0xeU))));
        bufp->chgIData(oldp+1588,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                  >> 0x1bU)))),19);
        bufp->chgCData(oldp+1589,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1590,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1591,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                         >> 4U))),3);
        bufp->chgIData(oldp+1592,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                   >> 0xfU)))),21);
        bufp->chgCData(oldp+1593,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1594,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1595,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                         >> 9U))),2);
        bufp->chgBit(oldp+1596,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1597,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 7U))));
        bufp->chgBit(oldp+1598,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 6U))));
        bufp->chgBit(oldp+1599,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 5U))));
        bufp->chgBit(oldp+1600,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 4U))));
        bufp->chgBit(oldp+1601,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1602,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1603,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U])));
        bufp->chgCData(oldp+1604,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                         >> 0x15U))),3);
        bufp->chgCData(oldp+1605,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                         >> 0x13U))),2);
        bufp->chgCData(oldp+1606,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                         >> 0x10U))),3);
        bufp->chgBit(oldp+1607,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1608,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                            >> 0xaU))),5);
        bufp->chgBit(oldp+1609,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 9U))));
        bufp->chgCData(oldp+1610,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                            >> 4U))),5);
        bufp->chgBit(oldp+1611,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1612,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                               >> 0x1eU)))),5);
        bufp->chgCData(oldp+1613,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                           >> 0x1aU))),4);
        bufp->chgBit(oldp+1614,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x19U))));
        bufp->chgIData(oldp+1615,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                                   << 5U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                     >> 0x1bU)))),30);
        bufp->chgBit(oldp+1616,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1617,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1618,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1619,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+1620,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                            >> 0x14U))),5);
        bufp->chgBit(oldp+1621,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1622,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                         >> 0x11U))),2);
        bufp->chgSData(oldp+1623,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                             >> 7U))),10);
        bufp->chgSData(oldp+1624,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                >> 0x1bU)))),12);
        bufp->chgSData(oldp+1625,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                              >> 0xfU))),15);
        bufp->chgIData(oldp+1626,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                  >> 0x1bU)))),20);
        bufp->chgCData(oldp+1627,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                         >> 0x1bU))),2);
        bufp->chgSData(oldp+1628,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                              >> 9U))),16);
        bufp->chgSData(oldp+1629,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                 >> 0x1bU)))),14);
        bufp->chgSData(oldp+1630,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                              >> 0xdU))),15);
        bufp->chgIData(oldp+1631,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                  >> 0x1bU)))),18);
        bufp->chgCData(oldp+1632,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                         >> 0x1bU))),3);
        bufp->chgBit(oldp+1633,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1aU))));
        bufp->chgIData(oldp+1634,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                               >> 7U))),19);
        bufp->chgCData(oldp+1635,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                            >> 0x18U))),5);
        bufp->chgCData(oldp+1636,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                            >> 0x13U))),5);
        bufp->chgCData(oldp+1637,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                         >> 0x10U))),3);
        bufp->chgIData(oldp+1638,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                   >> 0x1bU)))),21);
        bufp->chgCData(oldp+1639,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+1640,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+1641,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                         >> 0x15U))),2);
        bufp->chgBit(oldp+1642,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+1643,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x13U))));
        bufp->chgBit(oldp+1644,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x12U))));
        bufp->chgBit(oldp+1645,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1646,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1647,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1648,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                         >> 0xdU))),2);
        bufp->chgBit(oldp+1649,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0xcU))));
        bufp->chgCData(oldp+1650,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                                         >> 1U))),3);
        bufp->chgCData(oldp+1651,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+1652,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                         >> 0x1cU))),3);
        bufp->chgBit(oldp+1653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1654,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                            >> 0x16U))),5);
        bufp->chgBit(oldp+1655,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 0x15U))));
        bufp->chgCData(oldp+1656,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                            >> 0x10U))),5);
        bufp->chgBit(oldp+1657,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1658,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                            >> 0xaU))),5);
        bufp->chgCData(oldp+1659,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                           >> 6U))),4);
        bufp->chgBit(oldp+1660,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 5U))));
        bufp->chgIData(oldp+1661,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                                   << 0x19U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                     >> 7U)))),30);
        bufp->chgBit(oldp+1662,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 9U))));
        bufp->chgBit(oldp+1663,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1664,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1665,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1666,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U])),5);
        bufp->chgBit(oldp+1667,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                 >> 0x1fU)));
        bufp->chgCData(oldp+1668,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                         >> 0x1dU))),2);
        bufp->chgSData(oldp+1669,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                             >> 0x13U))),10);
        bufp->chgSData(oldp+1670,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                             >> 7U))),12);
        bufp->chgSData(oldp+1671,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                 >> 0x1bU)))),15);
        bufp->chgIData(oldp+1672,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                               >> 7U))),20);
        bufp->chgCData(oldp+1673,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                         >> 7U))),2);
        bufp->chgSData(oldp+1674,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                               << 0xbU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                 >> 0x15U)))),16);
        bufp->chgSData(oldp+1675,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                              >> 7U))),14);
        bufp->chgSData(oldp+1676,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                               << 7U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                 >> 0x19U)))),15);
        bufp->chgIData(oldp+1677,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                               >> 7U))),18);
        bufp->chgCData(oldp+1678,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                         >> 7U))),3);
        bufp->chgBit(oldp+1679,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 6U))));
        bufp->chgIData(oldp+1680,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                                << 0xdU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                  >> 0x13U)))),19);
        bufp->chgCData(oldp+1681,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                            >> 4U))),5);
        bufp->chgCData(oldp+1682,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                               >> 0x1fU)))),5);
        bufp->chgCData(oldp+1683,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                         >> 0x1cU))),3);
        bufp->chgIData(oldp+1684,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                >> 7U))),21);
        bufp->chgCData(oldp+1685,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1686,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                         >> 3U))),2);
        bufp->chgCData(oldp+1687,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1688,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U])));
        bufp->chgBit(oldp+1689,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1690,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+1691,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1692,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1693,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1694,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                         >> 0x19U))),2);
        bufp->chgBit(oldp+1695,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1696,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                       >> 4U))));
        bufp->chgBit(oldp+1697,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                       >> 3U))));
        bufp->chgBit(oldp+1698,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                       >> 2U))));
        bufp->chgBit(oldp+1699,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                       >> 1U))));
        bufp->chgBit(oldp+1700,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo))));
        bufp->chgCData(oldp+1701,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                   >> 0x19U)),7);
        bufp->chgCData(oldp+1702,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                            >> 0x14U))),5);
        bufp->chgCData(oldp+1703,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                            >> 0xfU))),5);
        bufp->chgCData(oldp+1704,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+1705,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1706,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)),7);
        bufp->chgCData(oldp+1707,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__rv32mFunct7),7);
        bufp->chgCData(oldp+1708,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zbaFunct7),7);
        bufp->chgCData(oldp+1709,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zicondFunct7),7);
        bufp->chgBit(oldp+1710,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined));
        bufp->chgIData(oldp+1711,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+1712,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+1713,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[6U]),32);
        bufp->chgIData(oldp+1714,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[5U]),32);
        bufp->chgIData(oldp+1715,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[4U]),32);
        bufp->chgIData(oldp+1716,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[3U]),32);
        bufp->chgIData(oldp+1717,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[2U]),32);
        bufp->chgIData(oldp+1718,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[1U]),32);
        bufp->chgIData(oldp+1719,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[0U]),32);
        bufp->chgCData(oldp+1720,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                         >> 0x15U))),2);
        bufp->chgBit(oldp+1721,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1722,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                    << 0xcU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[2U] 
                                                >> 0x14U))),32);
        bufp->chgIData(oldp+1723,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[2U] 
                                    << 0xcU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[1U] 
                                                >> 0x14U))),32);
        bufp->chgCData(oldp+1724,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[1U] 
                                           >> 0x10U))),4);
        bufp->chgIData(oldp+1725,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[1U] 
                                    << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                 >> 0x10U))),32);
        bufp->chgCData(oldp+1726,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+1727,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1728,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1729,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U])),3);
        bufp->chgCData(oldp+1730,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__phase),2);
        bufp->chgCData(oldp+1731,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__recoveryCount),7);
        bufp->chgBit(oldp+1732,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+1733,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1734,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                   [0U])),6);
        bufp->chgBit(oldp+1735,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                       [1U] >> 7U))));
        bufp->chgBit(oldp+1736,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1737,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                   [1U])),6);
        bufp->chgIData(oldp+1738,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+1739,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+1740,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+1741,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk6__DOT__i),32);
        bufp->chgCData(oldp+1742,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x16U] 
                                         >> 6U))),2);
        bufp->chgSData(oldp+1743,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1744,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1745,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1746,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1747,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                           >> 7U))),4);
        bufp->chgBit(oldp+1748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                       >> 6U))));
        bufp->chgIData(oldp+1749,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                                   << 0x18U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                                     >> 8U)))),30);
        bufp->chgIData(oldp+1750,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                                  >> 0x16U)))),18);
        bufp->chgBit(oldp+1751,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                       >> 0xaU))));
        bufp->chgIData(oldp+1752,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                                << 9U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                                  >> 0x17U)))),19);
        bufp->chgBit(oldp+1753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                       >> 0x16U))));
        bufp->chgSData(oldp+1754,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                             >> 0xcU))),10);
        bufp->chgCData(oldp+1755,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                         >> 0xaU))),2);
        bufp->chgIData(oldp+1756,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                                  >> 0x16U)))),20);
        bufp->chgCData(oldp+1757,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                         >> 0x13U))),3);
        bufp->chgCData(oldp+1758,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                         >> 0x10U))),3);
        bufp->chgCData(oldp+1759,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+1760,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+1761,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+1762,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                       >> 1U))));
        bufp->chgCData(oldp+1763,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+1764,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                       >> 0x1aU))));
        bufp->chgCData(oldp+1765,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                            >> 0x14U))),6);
        bufp->chgBit(oldp+1766,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1767,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                            >> 0xdU))),6);
        bufp->chgBit(oldp+1768,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+1769,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1770,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1771,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                       >> 4U))));
        bufp->chgIData(oldp+1772,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                                << 0xfU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                                  >> 0x11U)))),19);
        bufp->chgBit(oldp+1773,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                       >> 0x10U))));
        bufp->chgSData(oldp+1774,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x16U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1775,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                         >> 0x1aU))),2);
        bufp->chgCData(oldp+1776,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                         >> 0x18U))),2);
        bufp->chgCData(oldp+1777,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                         >> 0x16U))),2);
        bufp->chgCData(oldp+1778,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1779,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+1780,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                                   << 0xdU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                                     >> 0x13U)))),30);
        bufp->chgIData(oldp+1781,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                               >> 1U))),18);
        bufp->chgBit(oldp+1782,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                       >> 0x15U))));
        bufp->chgIData(oldp+1783,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                               >> 2U))),19);
        bufp->chgBit(oldp+1784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                       >> 1U))));
        bufp->chgSData(oldp+1785,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                                >> 0x17U)))),10);
        bufp->chgCData(oldp+1786,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                         >> 0x15U))),2);
        bufp->chgIData(oldp+1787,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                               >> 1U))),20);
        bufp->chgCData(oldp+1788,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                          >> 0x1eU)))),3);
        bufp->chgCData(oldp+1789,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                         >> 0x1bU))),3);
        bufp->chgCData(oldp+1790,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                            >> 0x15U))),6);
        bufp->chgCData(oldp+1791,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                           >> 0x11U))),4);
        bufp->chgCData(oldp+1792,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                           >> 0xdU))),4);
        bufp->chgBit(oldp+1793,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                       >> 0xcU))));
        bufp->chgCData(oldp+1794,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                            >> 6U))),6);
        bufp->chgBit(oldp+1795,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                       >> 5U))));
        bufp->chgCData(oldp+1796,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                               >> 0x1fU)))),6);
        bufp->chgBit(oldp+1797,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+1798,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                            >> 0x18U))),6);
        bufp->chgBit(oldp+1799,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1800,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                       >> 0x16U))));
        bufp->chgCData(oldp+1801,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                            >> 0x10U))),6);
        bufp->chgBit(oldp+1802,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                       >> 0xfU))));
        bufp->chgIData(oldp+1803,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                                << 4U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                                  >> 0x1cU)))),19);
        bufp->chgBit(oldp+1804,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+1805,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                       >> 0xfU))));
        bufp->chgSData(oldp+1806,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                             >> 5U))),10);
        bufp->chgCData(oldp+1807,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                         >> 3U))),2);
        bufp->chgBit(oldp+1808,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                       >> 2U))));
        bufp->chgCData(oldp+1809,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU])),2);
        bufp->chgCData(oldp+1810,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                   >> 0x1dU)),3);
        bufp->chgCData(oldp+1811,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                            >> 0x17U))),6);
        bufp->chgCData(oldp+1812,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                           >> 0x13U))),4);
        bufp->chgCData(oldp+1813,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                           >> 0xfU))),4);
        bufp->chgBit(oldp+1814,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                       >> 0xeU))));
        bufp->chgCData(oldp+1815,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                            >> 8U))),6);
        bufp->chgBit(oldp+1816,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                       >> 7U))));
        bufp->chgCData(oldp+1817,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                            >> 1U))),6);
        bufp->chgBit(oldp+1818,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU])));
        bufp->chgCData(oldp+1819,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                   >> 0x1aU)),6);
        bufp->chgBit(oldp+1820,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                       >> 0x19U))));
        bufp->chgBit(oldp+1821,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                       >> 0x18U))));
        bufp->chgCData(oldp+1822,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                            >> 0x12U))),6);
        bufp->chgBit(oldp+1823,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+1824,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                                << 2U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                                  >> 0x1eU)))),19);
        bufp->chgBit(oldp+1825,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+1826,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                         >> 0x1bU))),2);
        bufp->chgSData(oldp+1827,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                             >> 0x14U))),10);
        bufp->chgCData(oldp+1828,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                         >> 0x12U))),2);
        bufp->chgCData(oldp+1829,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                         >> 0xfU))),3);
        bufp->chgCData(oldp+1830,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+1831,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                         >> 0xaU))),2);
        bufp->chgCData(oldp+1832,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                         >> 8U))),2);
        bufp->chgSData(oldp+1833,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                                >> 0x1cU)))),12);
        bufp->chgBit(oldp+1834,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+1835,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+1836,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0x19U))));
        bufp->chgCData(oldp+1837,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+1838,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                            >> 0x12U))),5);
        bufp->chgBit(oldp+1839,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+1840,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1841,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                         >> 0xcU))),3);
        bufp->chgBit(oldp+1842,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1843,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                           >> 7U))),4);
        bufp->chgCData(oldp+1844,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                           >> 3U))),4);
        bufp->chgBit(oldp+1845,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 2U))));
        bufp->chgBit(oldp+1846,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 1U))));
        bufp->chgCData(oldp+1847,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                               >> 0x1bU)))),6);
        bufp->chgCData(oldp+1848,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                           >> 0x17U))),4);
        bufp->chgCData(oldp+1849,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                           >> 0x13U))),4);
        bufp->chgBit(oldp+1850,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                       >> 0x12U))));
        bufp->chgCData(oldp+1851,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                            >> 0xcU))),6);
        bufp->chgBit(oldp+1852,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1853,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1854,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                       >> 4U))));
        bufp->chgCData(oldp+1855,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+1856,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1857,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                       >> 0x1cU))));
        bufp->chgCData(oldp+1858,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                            >> 0x16U))),6);
        bufp->chgBit(oldp+1859,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                       >> 0x15U))));
        bufp->chgIData(oldp+1860,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                               >> 2U))),19);
        bufp->chgBit(oldp+1861,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                       >> 1U))));
        bufp->chgSData(oldp+1862,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1863,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1864,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+1865,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1866,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1867,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                         >> 5U))),2);
        bufp->chgSData(oldp+1868,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                                >> 0x19U)))),12);
        bufp->chgBit(oldp+1869,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1870,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1871,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 0x16U))));
        bufp->chgCData(oldp+1872,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                         >> 0x14U))),2);
        bufp->chgCData(oldp+1873,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                            >> 0xfU))),5);
        bufp->chgBit(oldp+1874,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 0xeU))));
        bufp->chgCData(oldp+1875,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                         >> 0xcU))),2);
        bufp->chgCData(oldp+1876,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                         >> 9U))),3);
        bufp->chgBit(oldp+1877,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 8U))));
        bufp->chgCData(oldp+1878,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+1879,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U])),4);
        bufp->chgBit(oldp+1880,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1881,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+1882,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                            >> 0x18U))),6);
        bufp->chgCData(oldp+1883,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                           >> 0x14U))),4);
        bufp->chgCData(oldp+1884,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                           >> 0x10U))),4);
        bufp->chgBit(oldp+1885,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1886,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                            >> 9U))),6);
        bufp->chgBit(oldp+1887,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                       >> 8U))));
        bufp->chgCData(oldp+1888,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                            >> 2U))),6);
        bufp->chgBit(oldp+1889,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                       >> 1U))));
        bufp->chgCData(oldp+1890,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+1891,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+1892,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                       >> 0x19U))));
        bufp->chgCData(oldp+1893,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                            >> 0x13U))),6);
        bufp->chgBit(oldp+1894,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                       >> 0x12U))));
        bufp->chgIData(oldp+1895,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                                << 1U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                                  >> 0x1fU)))),19);
        bufp->chgBit(oldp+1896,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+1897,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U])));
        bufp->chgSData(oldp+1898,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                   >> 0x16U)),10);
        bufp->chgCData(oldp+1899,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                         >> 0x14U))),2);
        bufp->chgCData(oldp+1900,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                         >> 0x11U))),3);
        bufp->chgCData(oldp+1901,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1902,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1903,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1904,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1905,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                         >> 3U))),2);
        bufp->chgCData(oldp+1906,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                               >> 0x1dU)))),6);
        bufp->chgCData(oldp+1907,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                           >> 0x19U))),4);
        bufp->chgCData(oldp+1908,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                           >> 0x15U))),4);
        bufp->chgBit(oldp+1909,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                       >> 0x14U))));
        bufp->chgCData(oldp+1910,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                            >> 0xeU))),6);
        bufp->chgBit(oldp+1911,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                       >> 0xdU))));
        bufp->chgCData(oldp+1912,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1913,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                       >> 6U))));
        bufp->chgCData(oldp+1914,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U])),6);
        bufp->chgBit(oldp+1915,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1916,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+1917,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                            >> 0x18U))),6);
        bufp->chgBit(oldp+1918,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                       >> 0x17U))));
        bufp->chgIData(oldp+1919,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                               >> 4U))),19);
        bufp->chgBit(oldp+1920,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1921,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U])),3);
        bufp->chgCData(oldp+1922,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regHeadStorage),5);
        bufp->chgCData(oldp+1923,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regTailStorage),5);
        bufp->chgBit(oldp+1924,((0x14U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regCount))));
        bufp->chgBit(oldp+1925,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regCount))));
        bufp->chgCData(oldp+1926,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regCount),6);
        bufp->chgBit(oldp+1927,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__validInstCount))));
        bufp->chgCData(oldp+1928,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__validInstCount),6);
        bufp->chgCData(oldp+1929,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__intervalIn),3);
        bufp->chgCData(oldp+1930,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__intervalCount),3);
        bufp->chgCData(oldp+1931,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__canBeFlushedEntryCount),6);
        bufp->chgCData(oldp+1932,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushRangeHeadPtr),6);
        bufp->chgCData(oldp+1933,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushRangeTailPtr),6);
        bufp->chgBit(oldp+1934,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushAllInsns));
        bufp->chgBit(oldp+1935,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayReg));
        bufp->chgBit(oldp+1936,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrID[0]));
        bufp->chgBit(oldp+1937,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrID[1]));
        bufp->chgBit(oldp+1938,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrValid[0]));
        bufp->chgBit(oldp+1939,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrValid[1]));
        bufp->chgCData(oldp+1940,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrPhase
                                  [0U]),5);
        bufp->chgCData(oldp+1941,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrPhase
                                  [1U]),5);
        bufp->chgBit(oldp+1942,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__regFlush));
        bufp->chgBit(oldp+1943,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__empty));
        bufp->chgCData(oldp+1944,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1945,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1946,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1948,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                               >> 0x1eU)))),5);
        bufp->chgBit(oldp+1949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+1950,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            >> 0x18U))),5);
        bufp->chgBit(oldp+1951,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0x17U))));
        bufp->chgCData(oldp+1952,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            >> 0x12U))),5);
        bufp->chgCData(oldp+1953,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1954,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0xdU))));
        bufp->chgIData(oldp+1955,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                                   << 0x11U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                     >> 0xfU)))),30);
        bufp->chgBit(oldp+1956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+1957,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+1958,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1959,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1960,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            >> 8U))),5);
        bufp->chgBit(oldp+1961,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1962,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                         >> 5U))),2);
        bufp->chgSData(oldp+1963,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                >> 0x1bU)))),10);
        bufp->chgSData(oldp+1964,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                             >> 0xfU))),12);
        bufp->chgSData(oldp+1965,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                              >> 3U))),15);
        bufp->chgIData(oldp+1966,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                  >> 0xfU)))),20);
        bufp->chgCData(oldp+1967,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                         >> 0xfU))),2);
        bufp->chgSData(oldp+1968,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                 >> 0x1dU)))),16);
        bufp->chgSData(oldp+1969,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                              >> 0xfU))),14);
        bufp->chgSData(oldp+1970,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                              >> 1U))),15);
        bufp->chgIData(oldp+1971,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                                << 0x11U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                  >> 0xfU)))),18);
        bufp->chgCData(oldp+1972,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                         >> 0xfU))),3);
        bufp->chgBit(oldp+1973,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0xeU))));
        bufp->chgIData(oldp+1974,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                  >> 0x1bU)))),19);
        bufp->chgCData(oldp+1975,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1976,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1977,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                         >> 4U))),3);
        bufp->chgIData(oldp+1978,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                   >> 0xfU)))),21);
        bufp->chgCData(oldp+1979,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1980,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1981,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                         >> 9U))),2);
        bufp->chgBit(oldp+1982,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 8U))));
        bufp->chgBit(oldp+1983,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 7U))));
        bufp->chgBit(oldp+1984,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 6U))));
        bufp->chgBit(oldp+1985,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 5U))));
        bufp->chgBit(oldp+1986,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 4U))));
        bufp->chgBit(oldp+1987,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1988,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                         >> 1U))),2);
        bufp->chgBit(oldp+1989,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U])));
        bufp->chgCData(oldp+1990,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                         >> 0x15U))),3);
        bufp->chgCData(oldp+1991,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                         >> 0x13U))),2);
        bufp->chgCData(oldp+1992,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                         >> 0x10U))),3);
        bufp->chgBit(oldp+1993,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1994,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                            >> 0xaU))),5);
        bufp->chgBit(oldp+1995,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                       >> 9U))));
        bufp->chgCData(oldp+1996,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                            >> 4U))),5);
        bufp->chgBit(oldp+1997,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1998,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                               >> 0x1eU)))),5);
        bufp->chgCData(oldp+1999,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                           >> 0x1aU))),4);
        bufp->chgBit(oldp+2000,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x19U))));
        bufp->chgIData(oldp+2001,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                                   << 5U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                     >> 0x1bU)))),30);
        bufp->chgBit(oldp+2002,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+2003,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+2004,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+2005,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+2006,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                            >> 0x14U))),5);
        bufp->chgBit(oldp+2007,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+2008,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                         >> 0x11U))),2);
        bufp->chgSData(oldp+2009,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                             >> 7U))),10);
        bufp->chgSData(oldp+2010,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                >> 0x1bU)))),12);
        bufp->chgSData(oldp+2011,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                              >> 0xfU))),15);
        bufp->chgIData(oldp+2012,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                  >> 0x1bU)))),20);
        bufp->chgCData(oldp+2013,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                         >> 0x1bU))),2);
        bufp->chgSData(oldp+2014,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                              >> 9U))),16);
        bufp->chgSData(oldp+2015,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                 >> 0x1bU)))),14);
        bufp->chgSData(oldp+2016,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                              >> 0xdU))),15);
        bufp->chgIData(oldp+2017,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                  >> 0x1bU)))),18);
        bufp->chgCData(oldp+2018,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                         >> 0x1bU))),3);
        bufp->chgBit(oldp+2019,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x1aU))));
        bufp->chgIData(oldp+2020,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                               >> 7U))),19);
        bufp->chgCData(oldp+2021,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                            >> 0x18U))),5);
        bufp->chgCData(oldp+2022,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                            >> 0x13U))),5);
        bufp->chgCData(oldp+2023,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                         >> 0x10U))),3);
        bufp->chgIData(oldp+2024,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                   >> 0x1bU)))),21);
        bufp->chgCData(oldp+2025,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                         >> 0x19U))),2);
        bufp->chgCData(oldp+2026,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+2027,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                         >> 0x15U))),2);
        bufp->chgBit(oldp+2028,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x14U))));
        bufp->chgBit(oldp+2029,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x13U))));
        bufp->chgBit(oldp+2030,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x12U))));
        bufp->chgBit(oldp+2031,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x11U))));
        bufp->chgBit(oldp+2032,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x10U))));
        bufp->chgBit(oldp+2033,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+2034,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                         >> 0xdU))),2);
        bufp->chgBit(oldp+2035,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+2036,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount))));
        bufp->chgBit(oldp+2037,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount))));
        bufp->chgBit(oldp+2038,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isEnv[0]));
        bufp->chgBit(oldp+2039,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isEnv[1]));
        bufp->chgCData(oldp+2040,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__serializer__DOT__regPhase),2);
        bufp->chgIData(oldp+2041,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgSData(oldp+2042,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued),16);
        bufp->chgSData(oldp+2043,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushIQ_Entry),16);
        bufp->chgIData(oldp+2044,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+2045,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2046,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__unnamedblk3__DOT__i),32);
        bufp->chgBit(oldp+2047,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__phase));
        bufp->chgCData(oldp+2048,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__unfinishedStoreNum),5);
        bufp->chgCData(oldp+2049,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__portMSHRPhase
                                  [0U]),5);
        bufp->chgCData(oldp+2050,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__portMSHRPhase
                                  [1U]),5);
        bufp->chgBit(oldp+2051,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                               >> 0x3cU)))));
        bufp->chgBit(oldp+2052,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                               >> 0x3bU)))));
        bufp->chgIData(oldp+2053,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                           >> 0x1bU))),32);
        bufp->chgIData(oldp+2054,((0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                                       >> 7U)))),20);
        bufp->chgBit(oldp+2055,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                               >> 6U)))));
        bufp->chgCData(oldp+2056,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                                   >> 2U)))),4);
        bufp->chgBit(oldp+2057,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                               >> 1U)))));
        bufp->chgBit(oldp+2058,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg))));
        bufp->chgBit(oldp+2059,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                               >> 0x3cU)))));
        bufp->chgBit(oldp+2060,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                               >> 0x3bU)))));
        bufp->chgIData(oldp+2061,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                           >> 0x1bU))),32);
        bufp->chgIData(oldp+2062,((0xfffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                                       >> 7U)))),20);
        bufp->chgBit(oldp+2063,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                               >> 6U)))));
        bufp->chgCData(oldp+2064,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                                   >> 2U)))),4);
        bufp->chgBit(oldp+2065,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                               >> 1U)))));
        bufp->chgBit(oldp+2066,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg))));
        bufp->chgBit(oldp+2067,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__headStoreHasAllocatedMSHRPipeReg));
        bufp->chgBit(oldp+2068,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__storeMSHRID));
        bufp->chgBit(oldp+2069,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__finishWriteBack));
        bufp->chgBit(oldp+2070,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__releaseStoreQueueHead));
        bufp->chgCData(oldp+2071,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__releaseStoreQueueHeadEntryNum),2);
        bufp->chgCData(oldp+2072,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountComplex),4);
        bufp->chgCData(oldp+2073,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountFP),4);
        bufp->chgBit(oldp+2074,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountInt));
        bufp->chgCData(oldp+2075,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountMem),3);
        bufp->chgCData(oldp+2076,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr),6);
        bufp->chgCData(oldp+2077,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr),6);
        bufp->chgBit(oldp+2078,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushAllInsns));
        bufp->chgIData(oldp+2079,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk11__DOT__i),32);
        bufp->chgIData(oldp+2080,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk11__DOT__unnamedblk12__DOT__j),32);
        bufp->chgIData(oldp+2081,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk13__DOT__i),32);
        bufp->chgIData(oldp+2082,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk13__DOT__unnamedblk14__DOT__j),32);
        bufp->chgIData(oldp+2083,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk15__DOT__i),32);
        bufp->chgIData(oldp+2084,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j),32);
        bufp->chgIData(oldp+2085,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk17__DOT__i),32);
        bufp->chgIData(oldp+2086,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk17__DOT__unnamedblk18__DOT__j),32);
        bufp->chgIData(oldp+2087,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk19__DOT__i),32);
        bufp->chgIData(oldp+2088,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk19__DOT__unnamedblk20__DOT__j),32);
        bufp->chgIData(oldp+2089,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk21__DOT__i),32);
        bufp->chgIData(oldp+2090,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk21__DOT__unnamedblk22__DOT__j),32);
        bufp->chgIData(oldp+2091,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk23__DOT__i),32);
        bufp->chgIData(oldp+2092,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk23__DOT__unnamedblk24__DOT__j),32);
        bufp->chgIData(oldp+2093,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk25__DOT__i),32);
        bufp->chgIData(oldp+2094,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk25__DOT__unnamedblk26__DOT__j),32);
        bufp->chgIData(oldp+2095,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk27__DOT__i),32);
        bufp->chgIData(oldp+2096,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk27__DOT__unnamedblk28__DOT__j),32);
        bufp->chgIData(oldp+2097,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk29__DOT__i),32);
        bufp->chgIData(oldp+2098,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk29__DOT__unnamedblk30__DOT__j),32);
        bufp->chgIData(oldp+2099,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk9__DOT__i),32);
        bufp->chgIData(oldp+2100,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk9__DOT__unnamedblk10__DOT__j),32);
        bufp->chgBit(oldp+2101,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [0U][2U] >> 4U))));
        bufp->chgQData(oldp+2102,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [0U][2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [0U][1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [0U][0U])) 
                                                    >> 4U)))),64);
        bufp->chgCData(oldp+2104,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                         [0U][0U] >> 2U))),2);
        bufp->chgBit(oldp+2105,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2106,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                 [0U][0U])));
        bufp->chgBit(oldp+2107,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [1U][2U] >> 4U))));
        bufp->chgQData(oldp+2108,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [1U][2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [1U][1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [1U][0U])) 
                                                    >> 4U)))),64);
        bufp->chgCData(oldp+2110,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                         [1U][0U] >> 2U))),2);
        bufp->chgBit(oldp+2111,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+2112,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                 [1U][0U])));
        bufp->chgBit(oldp+2113,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [2U][2U] >> 4U))));
        bufp->chgQData(oldp+2114,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [2U][2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [2U][1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [2U][0U])) 
                                                    >> 4U)))),64);
        bufp->chgCData(oldp+2116,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                         [2U][0U] >> 2U))),2);
        bufp->chgBit(oldp+2117,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [2U][0U] >> 1U))));
        bufp->chgBit(oldp+2118,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                 [2U][0U])));
        bufp->chgBit(oldp+2119,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [3U][2U] >> 4U))));
        bufp->chgQData(oldp+2120,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                    [3U][2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [3U][1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                    [3U][0U])) 
                                                    >> 4U)))),64);
        bufp->chgCData(oldp+2122,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                         [3U][0U] >> 2U))),2);
        bufp->chgBit(oldp+2123,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [3U][0U] >> 1U))));
        bufp->chgBit(oldp+2124,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                 [3U][0U])));
        bufp->chgBit(oldp+2125,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [4U][2U] >> 4U))));
        bufp->chgQData(oldp+2126,((((QData)((IData)(
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
        bufp->chgCData(oldp+2128,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                         [4U][0U] >> 2U))),2);
        bufp->chgBit(oldp+2129,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [4U][0U] >> 1U))));
        bufp->chgBit(oldp+2130,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                 [4U][0U])));
        bufp->chgBit(oldp+2131,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__prevMemReadAccessAck));
        bufp->chgBit(oldp+2132,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__prevMemWriteAccessAck));
        bufp->chgCData(oldp+2133,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__processLatencyCount),2);
        bufp->chgBit(oldp+2134,(((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__hasRequest) 
                                 & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                    >> 4U))));
        bufp->chgIData(oldp+2135,(((vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                    << 0x1cU) | (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                                                 >> 4U))),32);
        bufp->chgQData(oldp+2136,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U])) 
                                                    >> 4U)))),64);
        bufp->chgBit(oldp+2138,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__hasRequest));
        bufp->chgBit(oldp+2139,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__hasRequestReg));
        bufp->chgBit(oldp+2140,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                       >> 5U))));
        bufp->chgBit(oldp+2141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                       >> 4U))));
        bufp->chgCData(oldp+2142,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U] 
                                         >> 2U))),2);
        bufp->chgBit(oldp+2143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U] 
                                       >> 1U))));
        bufp->chgBit(oldp+2144,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U])));
        bufp->chgBit(oldp+2145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[3U] 
                                       >> 5U))));
        bufp->chgBit(oldp+2146,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[3U] 
                                       >> 4U))));
        bufp->chgIData(oldp+2147,(((vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[3U] 
                                    << 0x1cU) | (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[2U] 
                                                 >> 4U))),32);
        bufp->chgQData(oldp+2148,((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[2U])) 
                                    << 0x3cU) | (((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[1U])) 
                                                  << 0x1cU) 
                                                 | ((QData)((IData)(
                                                                    vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U])) 
                                                    >> 4U)))),64);
        bufp->chgCData(oldp+2150,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U] 
                                         >> 2U))),2);
        bufp->chgBit(oldp+2151,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U] 
                                       >> 1U))));
        bufp->chgBit(oldp+2152,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U])));
        bufp->chgBit(oldp+2153,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pop));
        bufp->chgBit(oldp+2154,((0x80U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regCount))));
        bufp->chgBit(oldp+2155,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regCount))));
        bufp->chgCData(oldp+2156,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage),7);
        bufp->chgCData(oldp+2157,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage),7);
        bufp->chgCData(oldp+2158,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__count),5);
        bufp->chgCData(oldp+2159,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__countReg),5);
        bufp->chgIData(oldp+2160,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__randReg),32);
        bufp->chgIData(oldp+2161,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__randNext),32);
        bufp->chgCData(oldp+2162,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage),7);
        bufp->chgCData(oldp+2163,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regCount),8);
        bufp->chgIData(oldp+2164,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+2165,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__unnamedblk2__DOT__i),32);
        bufp->chgBit(oldp+2166,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__prevLastCommittedPC 
                                       >> 0x13U))));
        bufp->chgIData(oldp+2167,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__prevLastCommittedPC)),19);
        bufp->chgIData(oldp+2168,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__cycles),32);
        bufp->chgBit(oldp+2169,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__regRstIndex) 
                                       >> 6U))));
        bufp->chgCData(oldp+2170,((0x3fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__regRstIndex))),6);
        bufp->chgBit(oldp+2171,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__fpRstIndex) 
                                       >> 6U))));
        bufp->chgCData(oldp+2172,((0x3fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__fpRstIndex))),6);
        bufp->chgCData(oldp+2173,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regHead),4);
        bufp->chgCData(oldp+2174,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regTail),4);
        bufp->chgCData(oldp+2175,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount),5);
        bufp->chgBit(oldp+2176,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0U] >> 0x1aU))));
        bufp->chgBit(oldp+2177,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0U] >> 0x19U))));
        bufp->chgIData(oldp+2178,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [0U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2179,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0U] >> 4U))));
        bufp->chgCData(oldp+2180,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [0U])),4);
        bufp->chgBit(oldp+2181,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [1U] >> 0x1aU))));
        bufp->chgBit(oldp+2182,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [1U] >> 0x19U))));
        bufp->chgIData(oldp+2183,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [1U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2184,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [1U] >> 4U))));
        bufp->chgCData(oldp+2185,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [1U])),4);
        bufp->chgBit(oldp+2186,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [2U] >> 0x1aU))));
        bufp->chgBit(oldp+2187,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [2U] >> 0x19U))));
        bufp->chgIData(oldp+2188,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [2U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2189,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [2U] >> 4U))));
        bufp->chgCData(oldp+2190,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [2U])),4);
        bufp->chgBit(oldp+2191,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [3U] >> 0x1aU))));
        bufp->chgBit(oldp+2192,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [3U] >> 0x19U))));
        bufp->chgIData(oldp+2193,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [3U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2194,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [3U] >> 4U))));
        bufp->chgCData(oldp+2195,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [3U])),4);
        bufp->chgBit(oldp+2196,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [4U] >> 0x1aU))));
        bufp->chgBit(oldp+2197,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [4U] >> 0x19U))));
        bufp->chgIData(oldp+2198,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [4U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2199,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [4U] >> 4U))));
        bufp->chgCData(oldp+2200,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [4U])),4);
        bufp->chgBit(oldp+2201,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [5U] >> 0x1aU))));
        bufp->chgBit(oldp+2202,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [5U] >> 0x19U))));
        bufp->chgIData(oldp+2203,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [5U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2204,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [5U] >> 4U))));
        bufp->chgCData(oldp+2205,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [5U])),4);
        bufp->chgBit(oldp+2206,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [6U] >> 0x1aU))));
        bufp->chgBit(oldp+2207,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [6U] >> 0x19U))));
        bufp->chgIData(oldp+2208,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [6U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2209,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [6U] >> 4U))));
        bufp->chgCData(oldp+2210,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [6U])),4);
        bufp->chgBit(oldp+2211,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [7U] >> 0x1aU))));
        bufp->chgBit(oldp+2212,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [7U] >> 0x19U))));
        bufp->chgIData(oldp+2213,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [7U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2214,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [7U] >> 4U))));
        bufp->chgCData(oldp+2215,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [7U])),4);
        bufp->chgBit(oldp+2216,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [8U] >> 0x1aU))));
        bufp->chgBit(oldp+2217,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [8U] >> 0x19U))));
        bufp->chgIData(oldp+2218,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [8U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2219,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [8U] >> 4U))));
        bufp->chgCData(oldp+2220,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [8U])),4);
        bufp->chgBit(oldp+2221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [9U] >> 0x1aU))));
        bufp->chgBit(oldp+2222,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [9U] >> 0x19U))));
        bufp->chgIData(oldp+2223,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [9U] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2224,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [9U] >> 4U))));
        bufp->chgCData(oldp+2225,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [9U])),4);
        bufp->chgBit(oldp+2226,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xaU] >> 0x1aU))));
        bufp->chgBit(oldp+2227,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xaU] >> 0x19U))));
        bufp->chgIData(oldp+2228,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [0xaU] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2229,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xaU] >> 4U))));
        bufp->chgCData(oldp+2230,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [0xaU])),4);
        bufp->chgBit(oldp+2231,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xbU] >> 0x1aU))));
        bufp->chgBit(oldp+2232,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xbU] >> 0x19U))));
        bufp->chgIData(oldp+2233,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [0xbU] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2234,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xbU] >> 4U))));
        bufp->chgCData(oldp+2235,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [0xbU])),4);
        bufp->chgBit(oldp+2236,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xcU] >> 0x1aU))));
        bufp->chgBit(oldp+2237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xcU] >> 0x19U))));
        bufp->chgIData(oldp+2238,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [0xcU] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2239,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xcU] >> 4U))));
        bufp->chgCData(oldp+2240,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [0xcU])),4);
        bufp->chgBit(oldp+2241,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xdU] >> 0x1aU))));
        bufp->chgBit(oldp+2242,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xdU] >> 0x19U))));
        bufp->chgIData(oldp+2243,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [0xdU] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2244,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xdU] >> 4U))));
        bufp->chgCData(oldp+2245,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [0xdU])),4);
        bufp->chgBit(oldp+2246,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xeU] >> 0x1aU))));
        bufp->chgBit(oldp+2247,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xeU] >> 0x19U))));
        bufp->chgIData(oldp+2248,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [0xeU] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2249,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xeU] >> 4U))));
        bufp->chgCData(oldp+2250,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [0xeU])),4);
        bufp->chgBit(oldp+2251,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xfU] >> 0x1aU))));
        bufp->chgBit(oldp+2252,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xfU] >> 0x19U))));
        bufp->chgIData(oldp+2253,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                               [0xfU] 
                                               >> 5U))),20);
        bufp->chgBit(oldp+2254,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                       [0xfU] >> 4U))));
        bufp->chgCData(oldp+2255,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                   [0xfU])),4);
        bufp->chgBit(oldp+2256,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHead));
        bufp->chgCData(oldp+2257,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHeadEntryNum),2);
        bufp->chgCData(oldp+2258,((0xfU & ((2U == (7U 
                                                   & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U]))
                                            ? ((IData)(1U) 
                                               + (0xfU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                     >> 5U)))
                                            : (0xfU 
                                               & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                  >> 5U))))),4);
        bufp->chgCData(oldp+2259,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__nextHead),4);
        bufp->chgIData(oldp+2260,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2261,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2262,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__unnamedblk4__DOT__i),32);
        bufp->chgBit(oldp+2263,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarReg[0]));
        bufp->chgBit(oldp+2264,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarReg[1]));
        bufp->chgCData(oldp+2265,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarRegNum[0]),7);
        bufp->chgCData(oldp+2266,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarRegNum[1]),7);
        bufp->chgCData(oldp+2267,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__scalarFreeListCount),6);
        bufp->chgBit(oldp+2268,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarFPReg[0]));
        bufp->chgBit(oldp+2269,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarFPReg[1]));
        bufp->chgCData(oldp+2270,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarFPRegNum[0]),7);
        bufp->chgCData(oldp+2271,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarFPRegNum[1]),7);
        bufp->chgCData(oldp+2272,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__queuePointer__DOT__regCount),6);
        bufp->chgBit(oldp+2273,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtRecoveryIndex) 
                                       >> 5U))));
        bufp->chgCData(oldp+2274,((0x1fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtRecoveryIndex))),5);
        bufp->chgCData(oldp+2275,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtRecoveryCount),7);
        bufp->chgCData(oldp+2276,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regTail),6);
        bufp->chgCData(oldp+2277,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount),7);
        bufp->chgBit(oldp+2278,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[2U] 
                                       >> 6U))));
        bufp->chgIData(oldp+2279,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[2U] 
                                                << 0xdU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                  >> 0x13U)))),19);
        bufp->chgIData(oldp+2280,(((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                    << 0xdU) | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                >> 0x13U))),32);
        bufp->chgCData(oldp+2281,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                            >> 0xdU))),6);
        bufp->chgCData(oldp+2282,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                           >> 5U))),4);
        bufp->chgBit(oldp+2283,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                       >> 4U))));
        bufp->chgCData(oldp+2284,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U])),4);
        bufp->chgCData(oldp+2285,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryEntryNum),7);
        bufp->chgCData(oldp+2286,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__nextRecoveryEntryNum),7);
        bufp->chgCData(oldp+2287,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__flushRangeHeadPtr),6);
        bufp->chgCData(oldp+2288,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__flushRangeTailPtr),6);
        bufp->chgBit(oldp+2289,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__regInRecovery));
        bufp->chgCData(oldp+2290,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regHead),6);
        bufp->chgBit(oldp+2291,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+2292,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+2293,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+2294,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                   [1U])),5);
        bufp->chgCData(oldp+2295,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWritePhyRegNum[0]),6);
        bufp->chgCData(oldp+2296,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWritePhyRegNum[1]),6);
        bufp->chgIData(oldp+2297,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__unnamedblk5__DOT__i),32);
        bufp->chgBit(oldp+2298,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+2299,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                   [0U])),5);
        bufp->chgBit(oldp+2300,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+2301,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                   [1U])),5);
        bufp->chgCData(oldp+2302,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWritePhyRegNum[0]),6);
        bufp->chgCData(oldp+2303,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWritePhyRegNum[1]),6);
        bufp->chgCData(oldp+2304,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__readPhyRegNum[0]),6);
        bufp->chgCData(oldp+2305,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__readPhyRegNum[1]),6);
        bufp->chgIData(oldp+2306,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__unnamedblk4__DOT__i),32);
        bufp->chgBit(oldp+2307,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg 
                                               >> 0x13U)))));
        bufp->chgIData(oldp+2308,((0x7ffffU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg))),19);
        bufp->chgBit(oldp+2309,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg 
                                               >> 0x27U)))));
        bufp->chgIData(oldp+2310,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg 
                                                       >> 0x14U)))),19);
        bufp->chgBit(oldp+2311,((0x20U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regCount))));
        bufp->chgBit(oldp+2312,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regCount))));
        bufp->chgIData(oldp+2313,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0U] >> 0x14U))),32);
        bufp->chgBit(oldp+2314,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2315,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2316,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2317,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2318,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0U]))));
        bufp->chgIData(oldp+2319,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [1U] >> 0x14U))),32);
        bufp->chgBit(oldp+2320,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [1U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2321,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [1U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2322,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [1U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2323,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [1U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2324,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [1U]))));
        bufp->chgIData(oldp+2325,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [2U] >> 0x14U))),32);
        bufp->chgBit(oldp+2326,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [2U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2327,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [2U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2328,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [2U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2329,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [2U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2330,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [2U]))));
        bufp->chgIData(oldp+2331,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [3U] >> 0x14U))),32);
        bufp->chgBit(oldp+2332,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [3U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2333,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [3U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2334,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [3U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2335,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [3U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2336,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [3U]))));
        bufp->chgIData(oldp+2337,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [4U] >> 0x14U))),32);
        bufp->chgBit(oldp+2338,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [4U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2339,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [4U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2340,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [4U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2341,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [4U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2342,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [4U]))));
        bufp->chgIData(oldp+2343,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [5U] >> 0x14U))),32);
        bufp->chgBit(oldp+2344,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [5U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2345,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [5U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2346,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [5U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2347,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [5U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2348,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [5U]))));
        bufp->chgIData(oldp+2349,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [6U] >> 0x14U))),32);
        bufp->chgBit(oldp+2350,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [6U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2351,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [6U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2352,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [6U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2353,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [6U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2354,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [6U]))));
        bufp->chgIData(oldp+2355,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [7U] >> 0x14U))),32);
        bufp->chgBit(oldp+2356,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [7U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2357,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [7U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2358,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [7U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2359,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [7U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2360,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [7U]))));
        bufp->chgIData(oldp+2361,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [8U] >> 0x14U))),32);
        bufp->chgBit(oldp+2362,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [8U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2363,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [8U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2364,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [8U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2365,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [8U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2366,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [8U]))));
        bufp->chgIData(oldp+2367,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [9U] >> 0x14U))),32);
        bufp->chgBit(oldp+2368,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [9U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2369,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [9U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2370,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [9U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2371,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [9U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2372,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [9U]))));
        bufp->chgIData(oldp+2373,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0xaU] >> 0x14U))),32);
        bufp->chgBit(oldp+2374,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xaU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2375,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xaU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2376,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0xaU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2377,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0xaU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2378,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0xaU]))));
        bufp->chgIData(oldp+2379,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0xbU] >> 0x14U))),32);
        bufp->chgBit(oldp+2380,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xbU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2381,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xbU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2382,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0xbU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2383,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0xbU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2384,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0xbU]))));
        bufp->chgIData(oldp+2385,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0xcU] >> 0x14U))),32);
        bufp->chgBit(oldp+2386,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xcU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2387,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xcU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2388,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0xcU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2389,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0xcU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2390,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0xcU]))));
        bufp->chgIData(oldp+2391,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0xdU] >> 0x14U))),32);
        bufp->chgBit(oldp+2392,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xdU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2393,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xdU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2394,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0xdU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2395,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0xdU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2396,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0xdU]))));
        bufp->chgIData(oldp+2397,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0xeU] >> 0x14U))),32);
        bufp->chgBit(oldp+2398,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xeU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2399,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xeU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2400,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0xeU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2401,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0xeU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2402,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0xeU]))));
        bufp->chgIData(oldp+2403,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0xfU] >> 0x14U))),32);
        bufp->chgBit(oldp+2404,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xfU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2405,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0xfU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2406,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0xfU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2407,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0xfU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2408,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0xfU]))));
        bufp->chgIData(oldp+2409,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x10U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2410,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x10U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2411,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x10U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2412,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x10U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2413,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x10U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2414,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x10U]))));
        bufp->chgIData(oldp+2415,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x11U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2416,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x11U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2417,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x11U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2418,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x11U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2419,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x11U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2420,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x11U]))));
        bufp->chgIData(oldp+2421,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x12U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2422,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x12U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2423,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x12U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2424,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x12U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2425,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x12U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2426,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x12U]))));
        bufp->chgIData(oldp+2427,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x13U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2428,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x13U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2429,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x13U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2430,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x13U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2431,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x13U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2432,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x13U]))));
        bufp->chgIData(oldp+2433,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x14U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2434,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x14U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2435,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x14U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2436,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x14U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2437,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x14U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2438,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x14U]))));
        bufp->chgIData(oldp+2439,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x15U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2440,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x15U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2441,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x15U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2442,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x15U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2443,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x15U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2444,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x15U]))));
        bufp->chgIData(oldp+2445,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x16U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2446,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x16U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2447,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x16U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2448,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x16U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2449,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x16U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2450,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x16U]))));
        bufp->chgIData(oldp+2451,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x17U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2452,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x17U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2453,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x17U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2454,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x17U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2455,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x17U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2456,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x17U]))));
        bufp->chgIData(oldp+2457,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x18U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2458,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x18U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2459,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x18U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2460,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x18U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2461,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x18U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2462,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x18U]))));
        bufp->chgIData(oldp+2463,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x19U] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2464,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x19U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2465,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x19U] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2466,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x19U] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2467,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x19U] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2468,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x19U]))));
        bufp->chgIData(oldp+2469,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x1aU] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2470,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1aU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2471,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1aU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2472,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x1aU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2473,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x1aU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2474,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x1aU]))));
        bufp->chgIData(oldp+2475,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x1bU] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2476,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1bU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2477,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1bU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2478,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x1bU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2479,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x1bU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2480,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x1bU]))));
        bufp->chgIData(oldp+2481,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x1cU] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2482,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1cU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2483,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1cU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2484,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x1cU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2485,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x1cU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2486,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x1cU]))));
        bufp->chgIData(oldp+2487,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x1dU] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2488,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1dU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2489,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1dU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2490,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x1dU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2491,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x1dU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2492,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x1dU]))));
        bufp->chgIData(oldp+2493,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x1eU] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2494,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1eU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2495,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1eU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2496,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x1eU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2497,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x1eU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2498,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x1eU]))));
        bufp->chgIData(oldp+2499,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                           [0x1fU] 
                                           >> 0x14U))),32);
        bufp->chgBit(oldp+2500,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1fU] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2501,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                               [0x1fU] 
                                               >> 0x12U)))));
        bufp->chgCData(oldp+2502,((0xfU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                   [0x1fU] 
                                                   >> 0xeU)))),4);
        bufp->chgSData(oldp+2503,((0x1fffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                      [0x1fU] 
                                                      >> 1U)))),13);
        bufp->chgBit(oldp+2504,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                              [0x1fU]))));
        bufp->chgCData(oldp+2505,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regHeadStorage),5);
        bufp->chgCData(oldp+2506,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regTailStorage),5);
        bufp->chgSData(oldp+2507,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__resetIndex),10);
        bufp->chgCData(oldp+2508,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regCount),6);
        bufp->chgCData(oldp+2509,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[0]),4);
        bufp->chgCData(oldp+2510,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[1]),4);
        bufp->chgCData(oldp+2511,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[2]),4);
        bufp->chgCData(oldp+2512,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[3]),4);
        bufp->chgCData(oldp+2513,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[4]),4);
        bufp->chgCData(oldp+2514,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[5]),4);
        bufp->chgCData(oldp+2515,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[6]),4);
        bufp->chgCData(oldp+2516,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[7]),4);
        bufp->chgCData(oldp+2517,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__regCount),5);
        bufp->chgBit(oldp+2518,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__freeListReset));
        bufp->chgBit(oldp+2519,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__freeListResetCycleCount));
        bufp->chgSData(oldp+2520,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__flush),16);
        bufp->chgSData(oldp+2521,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__prevFlushAtRecovery),16);
        bufp->chgBit(oldp+2522,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__issueQueueReturnIndex));
        bufp->chgCData(oldp+2523,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__issueQueueReturnIndexCycleCount),3);
        bufp->chgCData(oldp+2524,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__returnIndexOffset),4);
        bufp->chgCData(oldp+2525,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__writePtr[0]),4);
        bufp->chgCData(oldp+2526,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__writePtr[1]),4);
        bufp->chgSData(oldp+2527,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [0U] >> 2U))),10);
        bufp->chgCData(oldp+2528,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [0U])),2);
        bufp->chgSData(oldp+2529,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [1U] >> 2U))),10);
        bufp->chgCData(oldp+2530,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [1U])),2);
        bufp->chgSData(oldp+2531,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [2U] >> 2U))),10);
        bufp->chgCData(oldp+2532,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [2U])),2);
        bufp->chgSData(oldp+2533,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [3U] >> 2U))),10);
        bufp->chgCData(oldp+2534,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [3U])),2);
        bufp->chgSData(oldp+2535,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [4U] >> 2U))),10);
        bufp->chgCData(oldp+2536,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [4U])),2);
        bufp->chgSData(oldp+2537,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [5U] >> 2U))),10);
        bufp->chgCData(oldp+2538,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [5U])),2);
        bufp->chgSData(oldp+2539,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [6U] >> 2U))),10);
        bufp->chgCData(oldp+2540,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [6U])),2);
        bufp->chgSData(oldp+2541,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [7U] >> 2U))),10);
        bufp->chgCData(oldp+2542,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [7U])),2);
        bufp->chgSData(oldp+2543,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [8U] >> 2U))),10);
        bufp->chgCData(oldp+2544,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [8U])),2);
        bufp->chgSData(oldp+2545,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [9U] >> 2U))),10);
        bufp->chgCData(oldp+2546,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [9U])),2);
        bufp->chgSData(oldp+2547,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [0xaU] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+2548,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [0xaU])),2);
        bufp->chgSData(oldp+2549,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [0xbU] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+2550,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [0xbU])),2);
        bufp->chgSData(oldp+2551,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [0xcU] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+2552,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [0xcU])),2);
        bufp->chgSData(oldp+2553,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [0xdU] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+2554,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [0xdU])),2);
        bufp->chgSData(oldp+2555,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [0xeU] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+2556,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [0xeU])),2);
        bufp->chgSData(oldp+2557,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                             [0xfU] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+2558,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                   [0xfU])),2);
        bufp->chgIData(oldp+2559,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk10__DOT__i),32);
        bufp->chgIData(oldp+2560,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk11__DOT__i),32);
        bufp->chgIData(oldp+2561,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk12__DOT__i),32);
        bufp->chgIData(oldp+2562,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk13__DOT__i),32);
        bufp->chgIData(oldp+2563,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk8__DOT__i),32);
        bufp->chgCData(oldp+2564,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__rstIndex),4);
        bufp->chgBit(oldp+2565,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                                [0U][0U]));
        bufp->chgBit(oldp+2566,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                                [0U][1U]));
        bufp->chgBit(oldp+2567,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                                [0U][2U]));
        bufp->chgBit(oldp+2568,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                                [1U][0U]));
        bufp->chgBit(oldp+2569,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                                [1U][1U]));
        bufp->chgBit(oldp+2570,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                                [1U][2U]));
        bufp->chgBit(oldp+2571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                       [0U][0U] >> 6U))));
        bufp->chgCData(oldp+2572,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                   [0U][0U])),6);
        bufp->chgBit(oldp+2573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                       [0U][1U] >> 6U))));
        bufp->chgCData(oldp+2574,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                   [0U][1U])),6);
        bufp->chgBit(oldp+2575,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                       [0U][2U] >> 6U))));
        bufp->chgCData(oldp+2576,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                   [0U][2U])),6);
        bufp->chgBit(oldp+2577,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                       [1U][0U] >> 6U))));
        bufp->chgCData(oldp+2578,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                   [1U][0U])),6);
        bufp->chgBit(oldp+2579,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                       [1U][1U] >> 6U))));
        bufp->chgCData(oldp+2580,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                   [1U][1U])),6);
        bufp->chgBit(oldp+2581,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                       [1U][2U] >> 6U))));
        bufp->chgCData(oldp+2582,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                   [1U][2U])),6);
        bufp->chgBit(oldp+2583,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegValid[0]));
        bufp->chgBit(oldp+2584,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegValid[1]));
        bufp->chgBit(oldp+2585,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2586,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+2587,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+2588,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                   [1U])),6);
        bufp->chgCData(oldp+2589,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr)),4);
        bufp->chgCData(oldp+2590,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2591,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                           >> 8U))),4);
        bufp->chgCData(oldp+2592,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                           >> 0xcU))),4);
        bufp->chgCData(oldp+2593,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                           >> 0x10U))),4);
        bufp->chgCData(oldp+2594,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                           >> 0x14U))),4);
        bufp->chgSData(oldp+2595,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__storeBitVectorReg),16);
        bufp->chgCData(oldp+2596,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__Vcellinp__producerMatrix__dispatchPtr[0]),4);
        bufp->chgCData(oldp+2597,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__Vcellinp__producerMatrix__dispatchPtr[1]),4);
        bufp->chgSData(oldp+2598,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__resetIndex),10);
        bufp->chgIData(oldp+2599,((0x3fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                                                >> 7U))),22);
        bufp->chgIData(oldp+2600,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__recoveredPC),32);
        bufp->chgBit(oldp+2601,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                       [0U] >> 0x13U))));
        bufp->chgIData(oldp+2602,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                   [0U])),19);
        bufp->chgBit(oldp+2603,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                       [1U] >> 0x13U))));
        bufp->chgIData(oldp+2604,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                   [1U])),19);
        bufp->chgIData(oldp+2605,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulDataOut[0]),32);
        bufp->chgBit(oldp+2606,((0xdU >= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regCount))));
        bufp->chgBit(oldp+2607,((0xdU >= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount))));
        bufp->chgBit(oldp+2608,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrValid[0]));
        bufp->chgBit(oldp+2609,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrValid[1]));
        bufp->chgCData(oldp+2610,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrPhase
                                  [0U]),5);
        bufp->chgCData(oldp+2611,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrPhase
                                  [1U]),5);
        bufp->chgIData(oldp+2612,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__DataOut[0]),32);
        bufp->chgBit(oldp+2613,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                       [0U] >> 5U))));
        bufp->chgCData(oldp+2614,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                   [0U])),5);
        bufp->chgBit(oldp+2615,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                       [1U] >> 5U))));
        bufp->chgCData(oldp+2616,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                   [1U])),5);
        bufp->chgBit(oldp+2617,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegA[0]));
        bufp->chgBit(oldp+2618,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegA[1]));
        bufp->chgBit(oldp+2619,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegB[0]));
        bufp->chgBit(oldp+2620,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegB[1]));
        bufp->chgBit(oldp+2621,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegC[0]));
        bufp->chgBit(oldp+2622,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegC[1]));
        bufp->chgBit(oldp+2623,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg[0]));
        bufp->chgBit(oldp+2624,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg[1]));
        bufp->chgBit(oldp+2625,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg[0]));
        bufp->chgBit(oldp+2626,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg[1]));
        bufp->chgBit(oldp+2627,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2628,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                   [0U])),6);
        bufp->chgBit(oldp+2629,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+2630,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                   [1U])),6);
        bufp->chgBit(oldp+2631,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2632,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+2633,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+2634,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                   [1U])),6);
        bufp->chgBit(oldp+2635,((0x3eU >= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount))));
        bufp->chgSData(oldp+2636,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__regBrGlobalHistory),10);
        bufp->chgBit(oldp+2637,((0x20U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regCount))));
        bufp->chgBit(oldp+2638,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regCount))));
        bufp->chgIData(oldp+2639,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0U] >> 2U))),32);
        bufp->chgCData(oldp+2640,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0U]))),2);
        bufp->chgIData(oldp+2641,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [1U] >> 2U))),32);
        bufp->chgCData(oldp+2642,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [1U]))),2);
        bufp->chgIData(oldp+2643,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [2U] >> 2U))),32);
        bufp->chgCData(oldp+2644,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [2U]))),2);
        bufp->chgIData(oldp+2645,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [3U] >> 2U))),32);
        bufp->chgCData(oldp+2646,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [3U]))),2);
        bufp->chgIData(oldp+2647,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [4U] >> 2U))),32);
        bufp->chgCData(oldp+2648,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [4U]))),2);
        bufp->chgIData(oldp+2649,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [5U] >> 2U))),32);
        bufp->chgCData(oldp+2650,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [5U]))),2);
        bufp->chgIData(oldp+2651,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [6U] >> 2U))),32);
        bufp->chgCData(oldp+2652,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [6U]))),2);
        bufp->chgIData(oldp+2653,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [7U] >> 2U))),32);
        bufp->chgCData(oldp+2654,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [7U]))),2);
        bufp->chgIData(oldp+2655,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [8U] >> 2U))),32);
        bufp->chgCData(oldp+2656,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [8U]))),2);
        bufp->chgIData(oldp+2657,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [9U] >> 2U))),32);
        bufp->chgCData(oldp+2658,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [9U]))),2);
        bufp->chgIData(oldp+2659,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0xaU] >> 2U))),32);
        bufp->chgCData(oldp+2660,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0xaU]))),2);
        bufp->chgIData(oldp+2661,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0xbU] >> 2U))),32);
        bufp->chgCData(oldp+2662,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0xbU]))),2);
        bufp->chgIData(oldp+2663,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0xcU] >> 2U))),32);
        bufp->chgCData(oldp+2664,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0xcU]))),2);
        bufp->chgIData(oldp+2665,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0xdU] >> 2U))),32);
        bufp->chgCData(oldp+2666,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0xdU]))),2);
        bufp->chgIData(oldp+2667,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0xeU] >> 2U))),32);
        bufp->chgCData(oldp+2668,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0xeU]))),2);
        bufp->chgIData(oldp+2669,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0xfU] >> 2U))),32);
        bufp->chgCData(oldp+2670,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0xfU]))),2);
        bufp->chgIData(oldp+2671,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x10U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2672,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x10U]))),2);
        bufp->chgIData(oldp+2673,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x11U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2674,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x11U]))),2);
        bufp->chgIData(oldp+2675,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x12U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2676,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x12U]))),2);
        bufp->chgIData(oldp+2677,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x13U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2678,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x13U]))),2);
        bufp->chgIData(oldp+2679,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x14U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2680,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x14U]))),2);
        bufp->chgIData(oldp+2681,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x15U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2682,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x15U]))),2);
        bufp->chgIData(oldp+2683,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x16U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2684,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x16U]))),2);
        bufp->chgIData(oldp+2685,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x17U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2686,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x17U]))),2);
        bufp->chgIData(oldp+2687,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x18U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2688,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x18U]))),2);
        bufp->chgIData(oldp+2689,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x19U] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2690,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x19U]))),2);
        bufp->chgIData(oldp+2691,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x1aU] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2692,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x1aU]))),2);
        bufp->chgIData(oldp+2693,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x1bU] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2694,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x1bU]))),2);
        bufp->chgIData(oldp+2695,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x1cU] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2696,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x1cU]))),2);
        bufp->chgIData(oldp+2697,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x1dU] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2698,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x1dU]))),2);
        bufp->chgIData(oldp+2699,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x1eU] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2700,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x1eU]))),2);
        bufp->chgIData(oldp+2701,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                           [0x1fU] 
                                           >> 2U))),32);
        bufp->chgCData(oldp+2702,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                                [0x1fU]))),2);
        bufp->chgCData(oldp+2703,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regHeadStorage),5);
        bufp->chgCData(oldp+2704,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regTailStorage),5);
        bufp->chgSData(oldp+2705,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__resetIndex),11);
        bufp->chgCData(oldp+2706,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regCount),6);
        bufp->chgBit(oldp+2707,(((~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__freeListReset)) 
                                 & (2U <= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__regCount)))));
        bufp->chgCData(oldp+2708,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr[0]),4);
        bufp->chgCData(oldp+2709,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr[1]),4);
        bufp->chgCData(oldp+2710,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr[0]),6);
        bufp->chgCData(oldp+2711,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr[1]),6);
        bufp->chgSData(oldp+2712,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [0U][4U] 
                                             >> 1U))),10);
        bufp->chgCData(oldp+2713,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [0U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+2714,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+2715,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+2716,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [0U][3U] 
                                           >> 0x17U))),4);
        bufp->chgBit(oldp+2717,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+2718,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                   [0U][3U] 
                                                   << 8U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                     [0U][2U] 
                                                     >> 0x18U)))),30);
        bufp->chgIData(oldp+2719,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [0U][2U] 
                                               >> 6U))),18);
        bufp->chgBit(oldp+2720,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+2721,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [0U][3U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+2722,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][3U] >> 6U))));
        bufp->chgSData(oldp+2723,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                              [0U][3U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                [0U][2U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+2724,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+2725,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [0U][2U] 
                                               >> 6U))),20);
        bufp->chgCData(oldp+2726,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+2727,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                   [0U][2U])),3);
        bufp->chgCData(oldp+2728,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2729,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2730,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2731,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2732,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2733,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2734,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2735,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2736,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2737,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2738,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2739,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2740,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2741,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2742,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                 [0U][0U])));
        bufp->chgSData(oldp+2743,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [1U][4U] 
                                             >> 1U))),10);
        bufp->chgCData(oldp+2744,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [1U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [1U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+2745,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+2746,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+2747,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [1U][3U] 
                                           >> 0x17U))),4);
        bufp->chgBit(oldp+2748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+2749,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                   [1U][3U] 
                                                   << 8U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                     [1U][2U] 
                                                     >> 0x18U)))),30);
        bufp->chgIData(oldp+2750,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [1U][2U] 
                                               >> 6U))),18);
        bufp->chgBit(oldp+2751,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+2752,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [1U][3U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+2753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][3U] >> 6U))));
        bufp->chgSData(oldp+2754,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                              [1U][3U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                [1U][2U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+2755,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+2756,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [1U][2U] 
                                               >> 6U))),20);
        bufp->chgCData(oldp+2757,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+2758,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                   [1U][2U])),3);
        bufp->chgCData(oldp+2759,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2760,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2761,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2762,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2763,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2764,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2765,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2766,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+2767,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2768,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2769,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2770,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2771,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2772,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2773,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                 [1U][0U])));
    }
}
