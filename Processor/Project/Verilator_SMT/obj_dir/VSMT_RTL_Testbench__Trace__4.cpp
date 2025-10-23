// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "VSMT_RTL_Testbench__Syms.h"


void VSMT_RTL_Testbench___024root__trace_chg_0_sub_4(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root__trace_chg_0_sub_4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode + 13252);
    VlWide<4>/*127:0*/ __Vtemp_1;
    VlWide<4>/*127:0*/ __Vtemp_2;
    VlWide<3>/*95:0*/ __Vtemp_3;
    VlWide<3>/*95:0*/ __Vtemp_4;
    VlWide<3>/*95:0*/ __Vtemp_7;
    VlWide<3>/*95:0*/ __Vtemp_8;
    VlWide<3>/*95:0*/ __Vtemp_11;
    // Body
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x34U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x7bU])))) {
        bufp->chgBit(oldp+0,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                    [1U][1U] >> 0x18U))));
        bufp->chgBit(oldp+1,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                    [1U][1U] >> 0x17U))));
        bufp->chgIData(oldp+2,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                            [1U][1U] 
                                            >> 3U))),20);
        bufp->chgIData(oldp+3,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                 [1U][1U] << 0x1dU) 
                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                   [1U][0U] >> 3U))),32);
        bufp->chgBit(oldp+4,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                    [1U][0U] >> 2U))));
        bufp->chgBit(oldp+5,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                                    [1U][0U] >> 1U))));
        bufp->chgBit(oldp+6,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__nextStage
                              [1U][0U])));
        bufp->chgIData(oldp+7,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__unnamedblk6__DOT__i),32);
        bufp->chgBit(oldp+8,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executeStore[0]));
        bufp->chgIData(oldp+9,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreAddr[0]),20);
        bufp->chgBit(oldp+10,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreWordWE[0]));
        bufp->chgCData(oldp+11,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreByteWE[0]),4);
        bufp->chgBit(oldp+12,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreCondEnabled[0]));
        bufp->chgBit(oldp+13,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreRegValid[0]));
        bufp->chgCData(oldp+14,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__executedStoreQueuePtrByStore[0]),4);
        bufp->chgBit(oldp+15,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqWE[0]));
        bufp->chgBit(oldp+16,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqWriteData
                                             [0U] >> 0x25U)))));
        bufp->chgIData(oldp+17,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqWriteData
                                         [0U] >> 5U))),32);
        bufp->chgBit(oldp+18,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqWriteData
                                             [0U] >> 4U)))));
        bufp->chgCData(oldp+19,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqWriteData
                                                [0U]))),4);
        bufp->chgIData(oldp+20,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqWriteStoreData[0]),32);
        bufp->chgBit(oldp+21,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__mdtWE[0]));
        bufp->chgSData(oldp+22,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                              [0U][4U] 
                                              >> 0x1eU)))),10);
        bufp->chgCData(oldp+23,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x1cU))),2);
        bufp->chgBit(oldp+24,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+25,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+26,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x19U))));
        bufp->chgBit(oldp+27,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x18U))));
        bufp->chgBit(oldp+28,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x17U))));
        bufp->chgBit(oldp+29,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x16U))));
        bufp->chgBit(oldp+30,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x15U))));
        bufp->chgBit(oldp+31,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][4U] >> 0x14U))));
        bufp->chgCData(oldp+32,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 0xeU))),6);
        bufp->chgCData(oldp+33,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 8U))),6);
        bufp->chgCData(oldp+34,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                         [0U][4U] >> 4U))),4);
        bufp->chgCData(oldp+35,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                 [0U][4U])),4);
        bufp->chgIData(oldp+36,(vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                [0U][3U]),32);
        bufp->chgBit(oldp+37,((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                               [0U][2U] >> 0x1fU)));
        bufp->chgCData(oldp+38,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                         [0U][2U] >> 0x1bU))),4);
        bufp->chgIData(oldp+39,(((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                  [0U][2U] << 5U) | 
                                 (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                  [0U][1U] >> 0x1bU))),32);
        bufp->chgCData(oldp+40,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x19U))),2);
        bufp->chgBit(oldp+41,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][1U] >> 0x18U))));
        bufp->chgBit(oldp+42,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][1U] >> 0x17U))));
        bufp->chgIData(oldp+43,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                             [0U][1U] 
                                             >> 3U))),20);
        bufp->chgIData(oldp+44,(((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                  [0U][1U] << 0x1dU) 
                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                    [0U][0U] >> 3U))),32);
        bufp->chgBit(oldp+45,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][0U] >> 2U))));
        bufp->chgBit(oldp+46,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [0U][0U] >> 1U))));
        bufp->chgBit(oldp+47,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                               [0U][0U])));
        bufp->chgSData(oldp+48,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                            [1U][5U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                              [1U][4U] 
                                              >> 0x1eU)))),10);
        bufp->chgCData(oldp+49,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                       [1U][4U] >> 0x1cU))),2);
        bufp->chgBit(oldp+50,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+51,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x1aU))));
        bufp->chgBit(oldp+52,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x19U))));
        bufp->chgBit(oldp+53,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x18U))));
        bufp->chgBit(oldp+54,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x17U))));
        bufp->chgBit(oldp+55,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x16U))));
        bufp->chgBit(oldp+56,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x15U))));
        bufp->chgBit(oldp+57,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][4U] >> 0x14U))));
        bufp->chgCData(oldp+58,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                          [1U][4U] 
                                          >> 0xeU))),6);
        bufp->chgCData(oldp+59,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                          [1U][4U] 
                                          >> 8U))),6);
        bufp->chgCData(oldp+60,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                         [1U][4U] >> 4U))),4);
        bufp->chgCData(oldp+61,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                 [1U][4U])),4);
        bufp->chgIData(oldp+62,(vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                [1U][3U]),32);
        bufp->chgBit(oldp+63,((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                               [1U][2U] >> 0x1fU)));
        bufp->chgCData(oldp+64,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                         [1U][2U] >> 0x1bU))),4);
        bufp->chgIData(oldp+65,(((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                  [1U][2U] << 5U) | 
                                 (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                  [1U][1U] >> 0x1bU))),32);
        bufp->chgCData(oldp+66,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x19U))),2);
        bufp->chgBit(oldp+67,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][1U] >> 0x18U))));
        bufp->chgBit(oldp+68,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][1U] >> 0x17U))));
        bufp->chgIData(oldp+69,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                             [1U][1U] 
                                             >> 3U))),20);
        bufp->chgIData(oldp+70,(((vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                  [1U][1U] << 0x1dU) 
                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                    [1U][0U] >> 3U))),32);
        bufp->chgBit(oldp+71,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][0U] >> 2U))));
        bufp->chgBit(oldp+72,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                                     [1U][0U] >> 1U))));
        bufp->chgBit(oldp+73,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__mtStageIF.__PVT__nextStage
                               [1U][0U])));
        bufp->chgBit(oldp+74,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executeStore[0]));
        bufp->chgBit(oldp+75,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreCondEnabled[0]));
        bufp->chgBit(oldp+76,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreRegValid[0]));
        bufp->chgBit(oldp+77,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
                                     [0U] >> 0x15U))));
        bufp->chgBit(oldp+78,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
                                     [0U] >> 0x14U))));
        bufp->chgIData(oldp+79,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreAddr
                                 [0U])),20);
        bufp->chgIData(oldp+80,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreData[0]),32);
        bufp->chgWData(oldp+81,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreVectorData[0]),128);
        bufp->chgBit(oldp+85,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreMemAccessMode
                                     [0U] >> 2U))));
        bufp->chgCData(oldp+86,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreMemAccessMode
                                 [0U])),2);
        bufp->chgCData(oldp+87,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedLoadQueuePtrByStore[0]),4);
        bufp->chgCData(oldp+88,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__executedStoreQueuePtrByStore[0]),4);
        bufp->chgBit(oldp+89,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__memAccessOrderViolation[0]));
        bufp->chgBit(oldp+90,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordEntry[0]));
        bufp->chgBit(oldp+91,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordEntry[1]));
        bufp->chgSData(oldp+92,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [0U][3U] 
                                           >> 0x13U))),10);
        bufp->chgCData(oldp+93,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                       [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+94,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                       [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+95,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                       [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+96,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                       [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+97,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                       [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+98,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                            [0U][3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                              [0U][2U] 
                                              >> 0x1bU)))),12);
        bufp->chgBit(oldp+99,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                     [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+100,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+101,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+102,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+103,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [0U][2U] 
                                           >> 0x11U))),5);
        bufp->chgBit(oldp+104,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+105,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+106,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+107,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+108,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [0U][2U] 
                                          >> 6U))),4);
        bufp->chgCData(oldp+109,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [0U][2U] 
                                          >> 2U))),4);
        bufp->chgBit(oldp+110,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][2U] >> 1U))));
        bufp->chgBit(oldp+111,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                [0U][2U])));
        bufp->chgCData(oldp+112,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+113,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+114,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+115,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+116,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+117,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+118,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+119,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+120,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+121,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+122,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+123,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+124,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+125,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+126,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                [0U][0U])));
        bufp->chgSData(oldp+127,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                            [1U][3U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+128,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+129,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+130,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+131,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][3U] >> 9U))),2);
        bufp->chgCData(oldp+132,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][3U] >> 7U))),2);
        bufp->chgSData(oldp+133,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                             [1U][3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                               [1U][2U] 
                                               >> 0x1bU)))),12);
        bufp->chgBit(oldp+134,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+135,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+136,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][2U] >> 0x18U))));
        bufp->chgCData(oldp+137,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+138,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [1U][2U] 
                                           >> 0x11U))),5);
        bufp->chgBit(oldp+139,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][2U] >> 0x10U))));
        bufp->chgCData(oldp+140,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+141,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                        [1U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+142,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][2U] >> 0xaU))));
        bufp->chgCData(oldp+143,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [1U][2U] 
                                          >> 6U))),4);
        bufp->chgCData(oldp+144,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [1U][2U] 
                                          >> 2U))),4);
        bufp->chgBit(oldp+145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][2U] >> 1U))));
        bufp->chgBit(oldp+146,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                [1U][2U])));
        bufp->chgCData(oldp+147,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                  [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+148,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+149,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                          [1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+150,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+151,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+152,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+153,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+154,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][1U] >> 3U))));
        bufp->chgCData(oldp+155,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                            [1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                              [1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+156,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+157,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+158,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                           [1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+159,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                      [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+160,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                              [1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+161,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memRecordData
                                [1U][0U])));
        bufp->chgBit(oldp+162,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [0U][8U] >> 0x11U))));
        bufp->chgBit(oldp+163,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [0U][8U] >> 0x10U))));
        bufp->chgSData(oldp+164,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                            [0U][8U] 
                                            >> 6U))),10);
        bufp->chgCData(oldp+165,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                        [0U][8U] >> 4U))),2);
        bufp->chgBit(oldp+166,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [0U][8U] >> 3U))));
        bufp->chgIData(oldp+167,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                   [0U][8U] << 0x1dU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                     [0U][7U] >> 3U))),32);
        bufp->chgBit(oldp+168,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [0U][7U] >> 2U))));
        bufp->chgBit(oldp+169,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [0U][7U] >> 1U))));
        bufp->chgIData(oldp+170,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                   [0U][7U] << 0x1fU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                     [0U][6U] >> 1U))),32);
        bufp->chgBit(oldp+171,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                [0U][6U])));
        bufp->chgIData(oldp+172,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                 [0U][5U]),32);
        bufp->chgIData(oldp+173,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                 [0U][4U]),32);
        __Vtemp_1[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [0U][0U];
        __Vtemp_1[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [0U][1U];
        __Vtemp_1[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [0U][2U];
        __Vtemp_1[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [0U][3U];
        bufp->chgWData(oldp+174,(__Vtemp_1),128);
        bufp->chgBit(oldp+178,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [1U][8U] >> 0x11U))));
        bufp->chgBit(oldp+179,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [1U][8U] >> 0x10U))));
        bufp->chgSData(oldp+180,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                            [1U][8U] 
                                            >> 6U))),10);
        bufp->chgCData(oldp+181,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                        [1U][8U] >> 4U))),2);
        bufp->chgBit(oldp+182,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [1U][8U] >> 3U))));
        bufp->chgIData(oldp+183,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                   [1U][8U] << 0x1dU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                     [1U][7U] >> 3U))),32);
        bufp->chgBit(oldp+184,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [1U][7U] >> 2U))));
        bufp->chgBit(oldp+185,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                      [1U][7U] >> 1U))));
        bufp->chgIData(oldp+186,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                   [1U][7U] << 0x1fU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                     [1U][6U] >> 1U))),32);
        bufp->chgBit(oldp+187,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                [1U][6U])));
        bufp->chgIData(oldp+188,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                 [1U][5U]),32);
        bufp->chgIData(oldp+189,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
                                 [1U][4U]),32);
        __Vtemp_2[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [1U][0U];
        __Vtemp_2[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [1U][1U];
        __Vtemp_2[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [1U][2U];
        __Vtemp_2[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
            [1U][3U];
        bufp->chgWData(oldp+190,(__Vtemp_2),128);
        bufp->chgBit(oldp+194,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__we[0]));
        bufp->chgBit(oldp+195,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__we[0]));
        bufp->chgCData(oldp+196,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__wa[0]),4);
        bufp->chgQData(oldp+197,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__wv[0]),38);
        bufp->chgBit(oldp+199,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__we[0]));
        bufp->chgCData(oldp+200,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__wa[0]),4);
        bufp->chgQData(oldp+201,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__wv[0]),38);
        bufp->chgBit(oldp+203,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__we
                               [0U]));
        bufp->chgCData(oldp+204,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__wa
                                 [0U]),4);
        bufp->chgQData(oldp+205,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__wv
                                 [0U]),38);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x35U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x7cU])))) {
        bufp->chgBit(oldp+207,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__stall));
        bufp->chgBit(oldp+208,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__clear));
        bufp->chgBit(oldp+209,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__flush[0]));
        bufp->chgBit(oldp+210,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__flush[1]));
        bufp->chgSData(oldp+211,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                            [0U][4U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+212,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                         [0U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [0U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgCData(oldp+213,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+214,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+215,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                          [0U][3U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+216,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+217,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                                  [0U][3U] 
                                                  << 8U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                                    [0U][2U] 
                                                    >> 0x18U)))),30);
        bufp->chgIData(oldp+218,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [0U][2U] 
                                              >> 6U))),18);
        bufp->chgBit(oldp+219,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+220,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [0U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][3U] >> 6U))));
        bufp->chgSData(oldp+222,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                             [0U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                               [0U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+223,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+224,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [0U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+225,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+226,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                  [0U][2U])),3);
        bufp->chgCData(oldp+227,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+228,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+229,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+230,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+231,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+232,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+233,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+234,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+235,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+236,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+238,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+239,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+240,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+241,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                [0U][0U])));
        bufp->chgSData(oldp+242,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                            [1U][4U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+243,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                         [1U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [1U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgCData(oldp+244,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+245,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+246,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                          [1U][3U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+247,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+248,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                                  [1U][3U] 
                                                  << 8U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                                    [1U][2U] 
                                                    >> 0x18U)))),30);
        bufp->chgIData(oldp+249,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [1U][2U] 
                                              >> 6U))),18);
        bufp->chgBit(oldp+250,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+251,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [1U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+252,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][3U] >> 6U))));
        bufp->chgSData(oldp+253,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                             [1U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                               [1U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+254,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+255,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [1U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+256,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                        [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+257,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                  [1U][2U])),3);
        bufp->chgCData(oldp+258,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                  [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+259,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                          [1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+260,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                          [1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+262,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+264,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+265,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][1U] >> 3U))));
        bufp->chgCData(oldp+266,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                            [1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+267,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+268,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+269,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                           [1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+270,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                      [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+271,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                              [1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+272,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__iqData
                                [1U][0U])));
        bufp->chgCData(oldp+273,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                [0U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+274,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                [0U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+275,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                  [0U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+276,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                              [0U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+277,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                         [0U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+278,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                     [0U]))),18);
        bufp->chgBit(oldp+279,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                              [0U] 
                                              >> 0x34U)))));
        bufp->chgIData(oldp+280,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                      [0U] 
                                                      >> 0x21U)))),19);
        bufp->chgBit(oldp+281,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgSData(oldp+282,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                    [0U] 
                                                    >> 0x16U)))),10);
        bufp->chgCData(oldp+283,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                [0U] 
                                                >> 0x14U)))),2);
        bufp->chgIData(oldp+284,((0xfffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                     [0U]))),20);
        bufp->chgCData(oldp+285,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                [1U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+286,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                [1U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+287,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                  [1U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+288,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                              [1U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+289,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                         [1U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+290,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                     [1U]))),18);
        bufp->chgBit(oldp+291,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                              [1U] 
                                              >> 0x34U)))));
        bufp->chgIData(oldp+292,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                      [1U] 
                                                      >> 0x21U)))),19);
        bufp->chgBit(oldp+293,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgSData(oldp+294,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                    [1U] 
                                                    >> 0x16U)))),10);
        bufp->chgCData(oldp+295,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                [1U] 
                                                >> 0x14U)))),2);
        bufp->chgIData(oldp+296,((0xfffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intOpInfo
                                                     [1U]))),20);
        bufp->chgBit(oldp+297,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+298,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                                      [0U] 
                                                      >> 0xdU)))),19);
        bufp->chgBit(oldp+299,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                              [0U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+300,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                                    [0U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+301,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                               [0U]))),2);
        bufp->chgBit(oldp+302,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+303,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                                      [1U] 
                                                      >> 0xdU)))),19);
        bufp->chgBit(oldp+304,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                              [1U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+305,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                                    [1U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+306,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__bPred
                                               [1U]))),2);
        bufp->chgIData(oldp+307,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__pc[0]),32);
        bufp->chgIData(oldp+308,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__pc[1]),32);
        bufp->chgBit(oldp+309,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpA
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+310,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpA
                                         [0U])),32);
        bufp->chgBit(oldp+311,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpA
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+312,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpA
                                         [1U])),32);
        bufp->chgBit(oldp+313,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+314,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                         [0U])),32);
        bufp->chgBit(oldp+315,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+316,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                         [1U])),32);
        bufp->chgBit(oldp+317,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__dataOut
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+318,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__dataOut
                                         [0U])),32);
        bufp->chgBit(oldp+319,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__dataOut
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+320,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__dataOut
                                         [1U])),32);
        bufp->chgBit(oldp+321,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__isCondEnabled[0]));
        bufp->chgBit(oldp+322,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__isCondEnabled[1]));
        bufp->chgCData(oldp+323,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                 [0U]),4);
        bufp->chgCData(oldp+324,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                 [1U]),4);
        bufp->chgBit(oldp+325,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftOperandType
                               [0U]));
        bufp->chgBit(oldp+326,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftOperandType
                               [1U]));
        bufp->chgCData(oldp+327,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                           [0U] >> 0x19U))),5);
        bufp->chgCData(oldp+328,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                        [0U] >> 0x17U))),2);
        bufp->chgBit(oldp+329,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                      [0U] >> 0x16U))));
        bufp->chgIData(oldp+330,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                              [0U] 
                                              >> 2U))),20);
        bufp->chgCData(oldp+331,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                  [0U])),2);
        bufp->chgCData(oldp+332,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                           [1U] >> 0x19U))),5);
        bufp->chgCData(oldp+333,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                        [1U] >> 0x17U))),2);
        bufp->chgBit(oldp+334,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                      [1U] >> 0x16U))));
        bufp->chgIData(oldp+335,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                              [1U] 
                                              >> 2U))),20);
        bufp->chgCData(oldp+336,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                  [1U])),2);
        bufp->chgIData(oldp+337,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftDataOut[0]),32);
        bufp->chgIData(oldp+338,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftDataOut[1]),32);
        bufp->chgBit(oldp+339,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftCarryOut[0]));
        bufp->chgBit(oldp+340,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftCarryOut[1]));
        bufp->chgBit(oldp+341,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__isBranch[0]));
        bufp->chgBit(oldp+342,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__isBranch[1]));
        bufp->chgBit(oldp+343,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__isJump[0]));
        bufp->chgBit(oldp+344,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__isJump[1]));
        bufp->chgBit(oldp+345,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brTaken[0]));
        bufp->chgBit(oldp+346,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brTaken[1]));
        bufp->chgBit(oldp+347,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [0U] 
                                              >> 0x38U)))));
        bufp->chgIData(oldp+348,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                                      [0U] 
                                                      >> 0x25U)))),19);
        bufp->chgBit(oldp+349,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [0U] 
                                              >> 0x24U)))));
        bufp->chgIData(oldp+350,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                                      [0U] 
                                                      >> 0x11U)))),19);
        bufp->chgBit(oldp+351,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [0U] 
                                              >> 0x10U)))));
        bufp->chgBit(oldp+352,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [0U] 
                                              >> 0xfU)))));
        bufp->chgBit(oldp+353,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [0U] 
                                              >> 0xeU)))));
        bufp->chgBit(oldp+354,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [0U] 
                                              >> 0xdU)))));
        bufp->chgBit(oldp+355,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [0U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+356,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                                    [0U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+357,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                               [0U]))),2);
        bufp->chgBit(oldp+358,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [1U] 
                                              >> 0x38U)))));
        bufp->chgIData(oldp+359,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                                      [1U] 
                                                      >> 0x25U)))),19);
        bufp->chgBit(oldp+360,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [1U] 
                                              >> 0x24U)))));
        bufp->chgIData(oldp+361,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                                      [1U] 
                                                      >> 0x11U)))),19);
        bufp->chgBit(oldp+362,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [1U] 
                                              >> 0x10U)))));
        bufp->chgBit(oldp+363,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [1U] 
                                              >> 0xfU)))));
        bufp->chgBit(oldp+364,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [1U] 
                                              >> 0xeU)))));
        bufp->chgBit(oldp+365,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [1U] 
                                              >> 0xdU)))));
        bufp->chgBit(oldp+366,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                              [1U] 
                                              >> 0xcU)))));
        bufp->chgSData(oldp+367,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                                    [1U] 
                                                    >> 2U)))),10);
        bufp->chgCData(oldp+368,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brResult
                                               [1U]))),2);
        bufp->chgBit(oldp+369,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__predMiss[0]));
        bufp->chgBit(oldp+370,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__predMiss[1]));
        bufp->chgBit(oldp+371,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__regValid[0]));
        bufp->chgBit(oldp+372,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__regValid[1]));
        bufp->chgCData(oldp+373,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                [0U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+374,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                [0U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+375,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                  [0U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+376,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                              [0U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+377,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                         [0U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+378,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                     [0U]))),18);
        bufp->chgCData(oldp+379,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                [1U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+380,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                [1U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+381,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                  [1U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+382,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                              [1U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+383,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                         [1U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+384,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__intSubInfo
                                                     [1U]))),18);
        bufp->chgCData(oldp+385,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                [0U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+386,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                [0U] 
                                                >> 0x35U)))),2);
        bufp->chgBit(oldp+387,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                              [0U] 
                                              >> 0x34U)))));
        bufp->chgIData(oldp+388,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                      [0U] 
                                                      >> 0x21U)))),19);
        bufp->chgBit(oldp+389,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgSData(oldp+390,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                    [0U] 
                                                    >> 0x16U)))),10);
        bufp->chgCData(oldp+391,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                [0U] 
                                                >> 0x14U)))),2);
        bufp->chgIData(oldp+392,((0xfffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                     [0U]))),20);
        bufp->chgCData(oldp+393,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                [1U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+394,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                [1U] 
                                                >> 0x35U)))),2);
        bufp->chgBit(oldp+395,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                              [1U] 
                                              >> 0x34U)))));
        bufp->chgIData(oldp+396,((0x7ffffU & (IData)(
                                                     (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                      [1U] 
                                                      >> 0x21U)))),19);
        bufp->chgBit(oldp+397,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgSData(oldp+398,((0x3ffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                    [1U] 
                                                    >> 0x16U)))),10);
        bufp->chgCData(oldp+399,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                [1U] 
                                                >> 0x14U)))),2);
        bufp->chgIData(oldp+400,((0xfffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__brSubInfo
                                                     [1U]))),20);
        bufp->chgSData(oldp+401,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                            [0U][7U] 
                                            >> 9U))),10);
        bufp->chgCData(oldp+402,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [0U][7U] >> 7U))),2);
        bufp->chgBit(oldp+403,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][7U] >> 6U))));
        bufp->chgSData(oldp+404,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                             [0U][7U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [0U][6U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+405,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [0U][6U] >> 0x1aU))),2);
        bufp->chgCData(oldp+406,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [0U][6U] >> 0x18U))),2);
        bufp->chgCData(oldp+407,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [0U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+408,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                          [0U][6U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+409,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][6U] >> 0x11U))));
        bufp->chgIData(oldp+410,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                  [0U][6U] 
                                                  << 0xdU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                    [0U][5U] 
                                                    >> 0x13U)))),30);
        bufp->chgIData(oldp+411,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [0U][5U] 
                                              >> 1U))),18);
        bufp->chgBit(oldp+412,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][6U] >> 0x15U))));
        bufp->chgIData(oldp+413,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [0U][6U] 
                                              >> 2U))),19);
        bufp->chgBit(oldp+414,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][6U] >> 1U))));
        bufp->chgSData(oldp+415,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                             [0U][6U] 
                                             << 9U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [0U][5U] 
                                               >> 0x17U)))),10);
        bufp->chgCData(oldp+416,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [0U][5U] >> 0x15U))),2);
        bufp->chgIData(oldp+417,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [0U][5U] 
                                              >> 1U))),20);
        bufp->chgCData(oldp+418,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                         [0U][5U] << 2U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0x1eU)))),3);
        bufp->chgCData(oldp+419,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [0U][4U] >> 0x1bU))),3);
        bufp->chgCData(oldp+420,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0x15U))),6);
        bufp->chgCData(oldp+421,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                          [0U][4U] 
                                          >> 0x11U))),4);
        bufp->chgCData(oldp+422,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                          [0U][4U] 
                                          >> 0xdU))),4);
        bufp->chgBit(oldp+423,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][4U] >> 0xcU))));
        bufp->chgCData(oldp+424,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 6U))),6);
        bufp->chgBit(oldp+425,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][4U] >> 5U))));
        bufp->chgCData(oldp+426,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                            [0U][4U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [0U][3U] 
                                              >> 0x1fU)))),6);
        bufp->chgBit(oldp+427,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][3U] >> 0x1eU))));
        bufp->chgCData(oldp+428,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x18U))),6);
        bufp->chgBit(oldp+429,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][3U] >> 0x17U))));
        bufp->chgBit(oldp+430,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][3U] >> 0x16U))));
        bufp->chgCData(oldp+431,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+432,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][3U] >> 0xfU))));
        bufp->chgIData(oldp+433,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [0U][3U] 
                                               << 4U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                 [0U][2U] 
                                                 >> 0x1cU)))),19);
        bufp->chgBit(oldp+434,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][2U] >> 0x1bU))));
        bufp->chgBit(oldp+435,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][2U] >> 0x1aU))));
        bufp->chgIData(oldp+436,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                   [0U][2U] << 6U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                     [0U][1U] >> 0x1aU))),32);
        bufp->chgBit(oldp+437,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][1U] >> 0x19U))));
        bufp->chgBit(oldp+438,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][1U] >> 0x18U))));
        bufp->chgIData(oldp+439,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [0U][1U] 
                                              >> 5U))),19);
        bufp->chgBit(oldp+440,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][1U] >> 4U))));
        bufp->chgIData(oldp+441,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [0U][1U] 
                                               << 0xfU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                 [0U][0U] 
                                                 >> 0x11U)))),19);
        bufp->chgBit(oldp+442,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+443,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+444,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+445,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][0U] >> 0xdU))));
        bufp->chgBit(oldp+446,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [0U][0U] >> 0xcU))));
        bufp->chgSData(oldp+447,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                            [0U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+448,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                  [0U][0U])),2);
        bufp->chgSData(oldp+449,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                            [1U][7U] 
                                            >> 9U))),10);
        bufp->chgCData(oldp+450,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [1U][7U] >> 7U))),2);
        bufp->chgBit(oldp+451,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][7U] >> 6U))));
        bufp->chgSData(oldp+452,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                             [1U][7U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [1U][6U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+453,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [1U][6U] >> 0x1aU))),2);
        bufp->chgCData(oldp+454,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [1U][6U] >> 0x18U))),2);
        bufp->chgCData(oldp+455,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [1U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+456,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                          [1U][6U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+457,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][6U] >> 0x11U))));
        bufp->chgIData(oldp+458,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                  [1U][6U] 
                                                  << 0xdU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                    [1U][5U] 
                                                    >> 0x13U)))),30);
        bufp->chgIData(oldp+459,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [1U][5U] 
                                              >> 1U))),18);
        bufp->chgBit(oldp+460,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][6U] >> 0x15U))));
        bufp->chgIData(oldp+461,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [1U][6U] 
                                              >> 2U))),19);
        bufp->chgBit(oldp+462,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][6U] >> 1U))));
        bufp->chgSData(oldp+463,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                             [1U][6U] 
                                             << 9U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [1U][5U] 
                                               >> 0x17U)))),10);
        bufp->chgCData(oldp+464,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [1U][5U] >> 0x15U))),2);
        bufp->chgIData(oldp+465,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [1U][5U] 
                                              >> 1U))),20);
        bufp->chgCData(oldp+466,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                         [1U][5U] << 2U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0x1eU)))),3);
        bufp->chgCData(oldp+467,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                        [1U][4U] >> 0x1bU))),3);
        bufp->chgCData(oldp+468,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0x15U))),6);
        bufp->chgCData(oldp+469,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                          [1U][4U] 
                                          >> 0x11U))),4);
        bufp->chgCData(oldp+470,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                          [1U][4U] 
                                          >> 0xdU))),4);
        bufp->chgBit(oldp+471,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][4U] >> 0xcU))));
        bufp->chgCData(oldp+472,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 6U))),6);
        bufp->chgBit(oldp+473,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][4U] >> 5U))));
        bufp->chgCData(oldp+474,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                            [1U][4U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [1U][3U] 
                                              >> 0x1fU)))),6);
        bufp->chgBit(oldp+475,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][3U] >> 0x1eU))));
        bufp->chgCData(oldp+476,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x18U))),6);
        bufp->chgBit(oldp+477,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][3U] >> 0x17U))));
        bufp->chgBit(oldp+478,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][3U] >> 0x16U))));
        bufp->chgCData(oldp+479,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+480,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][3U] >> 0xfU))));
        bufp->chgIData(oldp+481,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [1U][3U] 
                                               << 4U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                 [1U][2U] 
                                                 >> 0x1cU)))),19);
        bufp->chgBit(oldp+482,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][2U] >> 0x1bU))));
        bufp->chgBit(oldp+483,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][2U] >> 0x1aU))));
        bufp->chgIData(oldp+484,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                   [1U][2U] << 6U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                     [1U][1U] >> 0x1aU))),32);
        bufp->chgBit(oldp+485,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][1U] >> 0x19U))));
        bufp->chgBit(oldp+486,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][1U] >> 0x18U))));
        bufp->chgIData(oldp+487,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                              [1U][1U] 
                                              >> 5U))),19);
        bufp->chgBit(oldp+488,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][1U] >> 4U))));
        bufp->chgIData(oldp+489,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                               [1U][1U] 
                                               << 0xfU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                                 [1U][0U] 
                                                 >> 0x11U)))),19);
        bufp->chgBit(oldp+490,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][0U] >> 0x10U))));
        bufp->chgBit(oldp+491,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][0U] >> 0xfU))));
        bufp->chgBit(oldp+492,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][0U] >> 0xeU))));
        bufp->chgBit(oldp+493,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][0U] >> 0xdU))));
        bufp->chgBit(oldp+494,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                      [1U][0U] >> 0xcU))));
        bufp->chgSData(oldp+495,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                            [1U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+496,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__nextStage
                                  [1U][0U])),2);
        bufp->chgCData(oldp+497,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                 [0U]),4);
        bufp->chgIData(oldp+498,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpA
                                         [0U])),32);
        bufp->chgIData(oldp+499,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                         [0U])),32);
        bufp->chgBit(oldp+500,(((1U & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                          [0U] >> 3U))) 
                                && ((1U & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                              [0U] 
                                              >> 2U))) 
                                    && ((1U & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                                  [0U] 
                                                  >> 1U))) 
                                        && (1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                            [0U]))))));
        bufp->chgCData(oldp+501,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                 [1U]),4);
        bufp->chgIData(oldp+502,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpA
                                         [1U])),32);
        bufp->chgIData(oldp+503,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                         [1U])),32);
        bufp->chgBit(oldp+504,(((1U & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                          [1U] >> 3U))) 
                                && ((1U & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                              [1U] 
                                              >> 2U))) 
                                    && ((1U & (~ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                                  [1U] 
                                                  >> 1U))) 
                                        && (1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__aluCode
                                            [1U]))))));
        bufp->chgBit(oldp+505,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftOperandType
                               [0U]));
        bufp->chgCData(oldp+506,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                        [0U] >> 0x17U))),2);
        bufp->chgCData(oldp+507,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                           [0U] >> 0x19U))),5);
        bufp->chgCData(oldp+508,((0x1fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                                  [0U]))),5);
        bufp->chgIData(oldp+509,(vlSymsp->TOP__SMT_RTL_Testbench__core.intExStage__DOT____Vcellout__BlockShifter__BRA__0__KET____DOT__shifter__dataOut),32);
        bufp->chgBit(oldp+510,(vlSymsp->TOP__SMT_RTL_Testbench__core.intExStage__DOT____Vcellout__BlockShifter__BRA__0__KET____DOT__shifter__carryOut));
        bufp->chgCData(oldp+511,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__shiftAmount),5);
        bufp->chgWData(oldp+512,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftTmp),66);
        bufp->chgIData(oldp+515,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftHighIn),32);
        bufp->chgIData(oldp+516,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftLowIn),32);
        bufp->chgQData(oldp+517,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftOut),34);
        bufp->chgCData(oldp+519,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftAmount),6);
        bufp->chgBit(oldp+520,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__isShiftZero));
        bufp->chgBit(oldp+521,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftOperandType
                               [1U]));
        bufp->chgCData(oldp+522,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                        [1U] >> 0x17U))),2);
        bufp->chgCData(oldp+523,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__shiftImmIn
                                           [1U] >> 0x19U))),5);
        bufp->chgCData(oldp+524,((0x1fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__fuOpB
                                                  [1U]))),5);
        bufp->chgIData(oldp+525,(vlSymsp->TOP__SMT_RTL_Testbench__core.intExStage__DOT____Vcellout__BlockShifter__BRA__1__KET____DOT__shifter__dataOut),32);
        bufp->chgBit(oldp+526,(vlSymsp->TOP__SMT_RTL_Testbench__core.intExStage__DOT____Vcellout__BlockShifter__BRA__1__KET____DOT__shifter__carryOut));
        bufp->chgCData(oldp+527,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__shiftAmount),5);
        bufp->chgWData(oldp+528,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftTmp),66);
        bufp->chgIData(oldp+531,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftHighIn),32);
        bufp->chgIData(oldp+532,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftLowIn),32);
        bufp->chgQData(oldp+533,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftOut),34);
        bufp->chgCData(oldp+535,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftAmount),6);
        bufp->chgBit(oldp+536,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__isShiftZero));
        bufp->chgIData(oldp+537,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgBit(oldp+538,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__currentPC 
                                      >> 0x13U))));
        bufp->chgIData(oldp+539,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__currentPC)),19);
        bufp->chgIData(oldp+540,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__nextAddr),32);
        bufp->chgSData(oldp+541,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                            [0U][7U] 
                                            >> 9U))),10);
        bufp->chgCData(oldp+542,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [0U][7U] >> 7U))),2);
        bufp->chgBit(oldp+543,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][7U] >> 6U))));
        bufp->chgSData(oldp+544,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                             [0U][7U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [0U][6U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+545,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [0U][6U] >> 0x1aU))),2);
        bufp->chgCData(oldp+546,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [0U][6U] >> 0x18U))),2);
        bufp->chgCData(oldp+547,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [0U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+548,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                          [0U][6U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+549,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][6U] >> 0x11U))));
        bufp->chgIData(oldp+550,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                  [0U][6U] 
                                                  << 0xdU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                    [0U][5U] 
                                                    >> 0x13U)))),30);
        bufp->chgIData(oldp+551,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [0U][5U] 
                                              >> 1U))),18);
        bufp->chgBit(oldp+552,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][6U] >> 0x15U))));
        bufp->chgIData(oldp+553,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [0U][6U] 
                                              >> 2U))),19);
        bufp->chgBit(oldp+554,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][6U] >> 1U))));
        bufp->chgSData(oldp+555,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                             [0U][6U] 
                                             << 9U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [0U][5U] 
                                               >> 0x17U)))),10);
        bufp->chgCData(oldp+556,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [0U][5U] >> 0x15U))),2);
        bufp->chgIData(oldp+557,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [0U][5U] 
                                              >> 1U))),20);
        bufp->chgCData(oldp+558,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                         [0U][5U] << 2U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0x1eU)))),3);
        bufp->chgCData(oldp+559,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [0U][4U] >> 0x1bU))),3);
        bufp->chgCData(oldp+560,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0x15U))),6);
        bufp->chgCData(oldp+561,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 0x11U))),4);
        bufp->chgCData(oldp+562,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 0xdU))),4);
        bufp->chgBit(oldp+563,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][4U] >> 0xcU))));
        bufp->chgCData(oldp+564,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 6U))),6);
        bufp->chgBit(oldp+565,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][4U] >> 5U))));
        bufp->chgCData(oldp+566,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [0U][3U] 
                                              >> 0x1fU)))),6);
        bufp->chgBit(oldp+567,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x1eU))));
        bufp->chgCData(oldp+568,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x18U))),6);
        bufp->chgBit(oldp+569,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x17U))));
        bufp->chgBit(oldp+570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x16U))));
        bufp->chgCData(oldp+571,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+572,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][3U] >> 0xfU))));
        bufp->chgIData(oldp+573,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               << 4U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                 [0U][2U] 
                                                 >> 0x1cU)))),19);
        bufp->chgBit(oldp+574,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x1bU))));
        bufp->chgBit(oldp+575,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x1aU))));
        bufp->chgIData(oldp+576,(((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                   [0U][2U] << 6U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                     [0U][1U] >> 0x1aU))),32);
        bufp->chgBit(oldp+577,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x19U))));
        bufp->chgBit(oldp+578,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x18U))));
        bufp->chgIData(oldp+579,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 5U))),19);
        bufp->chgBit(oldp+580,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][1U] >> 4U))));
        bufp->chgIData(oldp+581,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [0U][1U] 
                                               << 0xfU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                 [0U][0U] 
                                                 >> 0x11U)))),19);
        bufp->chgBit(oldp+582,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+583,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+584,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+585,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xdU))));
        bufp->chgBit(oldp+586,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xcU))));
        bufp->chgSData(oldp+587,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                            [0U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+588,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                  [0U][0U])),2);
        bufp->chgSData(oldp+589,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                            [1U][7U] 
                                            >> 9U))),10);
        bufp->chgCData(oldp+590,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [1U][7U] >> 7U))),2);
        bufp->chgBit(oldp+591,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][7U] >> 6U))));
        bufp->chgSData(oldp+592,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                             [1U][7U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [1U][6U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+593,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [1U][6U] >> 0x1aU))),2);
        bufp->chgCData(oldp+594,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [1U][6U] >> 0x18U))),2);
        bufp->chgCData(oldp+595,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [1U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+596,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                          [1U][6U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+597,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][6U] >> 0x11U))));
        bufp->chgIData(oldp+598,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                  [1U][6U] 
                                                  << 0xdU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                    [1U][5U] 
                                                    >> 0x13U)))),30);
        bufp->chgIData(oldp+599,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [1U][5U] 
                                              >> 1U))),18);
        bufp->chgBit(oldp+600,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][6U] >> 0x15U))));
        bufp->chgIData(oldp+601,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [1U][6U] 
                                              >> 2U))),19);
        bufp->chgBit(oldp+602,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][6U] >> 1U))));
        bufp->chgSData(oldp+603,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                             [1U][6U] 
                                             << 9U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [1U][5U] 
                                               >> 0x17U)))),10);
        bufp->chgCData(oldp+604,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [1U][5U] >> 0x15U))),2);
        bufp->chgIData(oldp+605,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [1U][5U] 
                                              >> 1U))),20);
        bufp->chgCData(oldp+606,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                         [1U][5U] << 2U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 0x1eU)))),3);
        bufp->chgCData(oldp+607,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                        [1U][4U] >> 0x1bU))),3);
        bufp->chgCData(oldp+608,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 0x15U))),6);
        bufp->chgCData(oldp+609,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                          [1U][4U] 
                                          >> 0x11U))),4);
        bufp->chgCData(oldp+610,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                          [1U][4U] 
                                          >> 0xdU))),4);
        bufp->chgBit(oldp+611,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][4U] >> 0xcU))));
        bufp->chgCData(oldp+612,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 6U))),6);
        bufp->chgBit(oldp+613,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][4U] >> 5U))));
        bufp->chgCData(oldp+614,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [1U][3U] 
                                              >> 0x1fU)))),6);
        bufp->chgBit(oldp+615,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x1eU))));
        bufp->chgCData(oldp+616,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0x18U))),6);
        bufp->chgBit(oldp+617,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x17U))));
        bufp->chgBit(oldp+618,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x16U))));
        bufp->chgCData(oldp+619,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0x10U))),6);
        bufp->chgBit(oldp+620,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][3U] >> 0xfU))));
        bufp->chgIData(oldp+621,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               << 4U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                 [1U][2U] 
                                                 >> 0x1cU)))),19);
        bufp->chgBit(oldp+622,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x1bU))));
        bufp->chgBit(oldp+623,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x1aU))));
        bufp->chgIData(oldp+624,(((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                   [1U][2U] << 6U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                     [1U][1U] >> 0x1aU))),32);
        bufp->chgBit(oldp+625,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x19U))));
        bufp->chgBit(oldp+626,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x18U))));
        bufp->chgIData(oldp+627,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                              [1U][1U] 
                                              >> 5U))),19);
        bufp->chgBit(oldp+628,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][1U] >> 4U))));
        bufp->chgIData(oldp+629,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                               [1U][1U] 
                                               << 0xfU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                                 [1U][0U] 
                                                 >> 0x11U)))),19);
        bufp->chgBit(oldp+630,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x10U))));
        bufp->chgBit(oldp+631,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xfU))));
        bufp->chgBit(oldp+632,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xeU))));
        bufp->chgBit(oldp+633,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xdU))));
        bufp->chgBit(oldp+634,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xcU))));
        bufp->chgSData(oldp+635,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                            [1U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+636,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__intExStageIF.__PVT__nextStage
                                  [1U][0U])),2);
        bufp->chgBit(oldp+637,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+638,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+639,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+640,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+641,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+642,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+643,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+644,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+645,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+646,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+647,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+648,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+649,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+650,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+651,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+652,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+654,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                [0U])));
        bufp->chgBit(oldp+655,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 0x14U))));
        bufp->chgCData(oldp+656,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                        [1U] >> 0x12U))),2);
        bufp->chgBit(oldp+657,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 0x11U))));
        bufp->chgBit(oldp+658,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 0x10U))));
        bufp->chgBit(oldp+659,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 0xfU))));
        bufp->chgBit(oldp+660,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 0xeU))));
        bufp->chgBit(oldp+661,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 0xdU))));
        bufp->chgCData(oldp+662,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                        [1U] >> 0xbU))),2);
        bufp->chgBit(oldp+663,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 0xaU))));
        bufp->chgBit(oldp+664,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 9U))));
        bufp->chgBit(oldp+665,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 8U))));
        bufp->chgBit(oldp+666,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 7U))));
        bufp->chgBit(oldp+667,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+668,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                        [1U] >> 4U))),2);
        bufp->chgBit(oldp+669,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 3U))));
        bufp->chgBit(oldp+670,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 2U))));
        bufp->chgBit(oldp+671,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                      [1U] >> 1U))));
        bufp->chgBit(oldp+672,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlIn
                                [1U])));
        bufp->chgBit(oldp+673,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intDstRegDataOut
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+674,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intDstRegDataOut
                                         [0U])),32);
        bufp->chgBit(oldp+675,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intDstRegDataOut
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+676,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intDstRegDataOut
                                         [1U])),32);
        bufp->chgBit(oldp+677,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                      [0U][3U] >> 0x15U))));
        bufp->chgBit(oldp+678,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                      [0U][3U] >> 0x14U))));
        bufp->chgSData(oldp+679,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                            [0U][3U] 
                                            >> 0xaU))),10);
        bufp->chgCData(oldp+680,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                        [0U][3U] >> 8U))),2);
        bufp->chgIData(oldp+681,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                   [0U][3U] << 0x18U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                     [0U][2U] >> 8U))),32);
        bufp->chgIData(oldp+682,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                   [0U][2U] << 0x18U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                     [0U][1U] >> 8U))),32);
        bufp->chgIData(oldp+683,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                   [0U][1U] << 0x18U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                     [0U][0U] >> 8U))),32);
        bufp->chgCData(oldp+684,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                          [0U][0U] 
                                          >> 4U))),4);
        bufp->chgCData(oldp+685,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                        [0U][0U] >> 1U))),3);
        bufp->chgBit(oldp+686,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                [0U][0U])));
        bufp->chgBit(oldp+687,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                      [1U][3U] >> 0x15U))));
        bufp->chgBit(oldp+688,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                      [1U][3U] >> 0x14U))));
        bufp->chgSData(oldp+689,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                            [1U][3U] 
                                            >> 0xaU))),10);
        bufp->chgCData(oldp+690,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                        [1U][3U] >> 8U))),2);
        bufp->chgIData(oldp+691,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                   [1U][3U] << 0x18U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                     [1U][2U] >> 8U))),32);
        bufp->chgIData(oldp+692,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                   [1U][2U] << 0x18U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                     [1U][1U] >> 8U))),32);
        bufp->chgIData(oldp+693,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                   [1U][1U] << 0x18U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                     [1U][0U] >> 8U))),32);
        bufp->chgCData(oldp+694,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                          [1U][0U] 
                                          >> 4U))),4);
        bufp->chgCData(oldp+695,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                        [1U][0U] >> 1U))),3);
        bufp->chgBit(oldp+696,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
                                [1U][0U])));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x36U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x7dU])))) {
        bufp->chgBit(oldp+697,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__stall));
        bufp->chgBit(oldp+698,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__clear));
        bufp->chgBit(oldp+699,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__flush
                               [0U][0U]));
        bufp->chgBit(oldp+700,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__flush
                               [0U][1U]));
        bufp->chgBit(oldp+701,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__flush
                               [0U][2U]));
        bufp->chgSData(oldp+702,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                            [0U][0U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+703,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                        [0U][0U][2U] 
                                        >> 6U))),2);
        bufp->chgBit(oldp+704,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][0U][2U] 
                                      >> 5U))));
        bufp->chgCData(oldp+705,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                        [0U][0U][2U] 
                                        >> 3U))),2);
        bufp->chgCData(oldp+706,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                  [0U][0U][2U])),3);
        bufp->chgCData(oldp+707,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                  [0U][0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+708,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                          [0U][0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+709,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                          [0U][0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+710,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][0U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+711,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+712,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][0U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+713,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+714,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][0U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+715,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                            [0U][0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                              [0U][0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+716,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][0U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+717,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][0U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+718,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+719,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][0U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+720,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                              [0U][0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+721,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                [0U][0U][0U])));
        bufp->chgSData(oldp+722,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                            [0U][1U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+723,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                        [0U][1U][2U] 
                                        >> 6U))),2);
        bufp->chgBit(oldp+724,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][1U][2U] 
                                      >> 5U))));
        bufp->chgCData(oldp+725,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                        [0U][1U][2U] 
                                        >> 3U))),2);
        bufp->chgCData(oldp+726,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                  [0U][1U][2U])),3);
        bufp->chgCData(oldp+727,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                  [0U][1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+728,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                          [0U][1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+729,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                          [0U][1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+730,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][1U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+731,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+732,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][1U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+733,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+734,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][1U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+735,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                            [0U][1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                              [0U][1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+736,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][1U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+737,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][1U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+738,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+739,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][1U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+740,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                              [0U][1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+741,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                [0U][1U][0U])));
        bufp->chgSData(oldp+742,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                            [0U][2U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+743,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                        [0U][2U][2U] 
                                        >> 6U))),2);
        bufp->chgBit(oldp+744,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][2U][2U] 
                                      >> 5U))));
        bufp->chgCData(oldp+745,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                        [0U][2U][2U] 
                                        >> 3U))),2);
        bufp->chgCData(oldp+746,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                  [0U][2U][2U])),3);
        bufp->chgCData(oldp+747,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                  [0U][2U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+748,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                          [0U][2U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+749,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                          [0U][2U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+750,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][2U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+751,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][2U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+752,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][2U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+753,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][2U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+754,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][2U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+755,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                            [0U][2U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                              [0U][2U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+756,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][2U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+757,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][2U][0U] 
                                      >> 0x1bU))));
        bufp->chgCData(oldp+758,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                           [0U][2U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+759,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                      [0U][2U][0U] 
                                      >> 0x14U))));
        bufp->chgIData(oldp+760,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                              [0U][2U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+761,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__iqData
                                [0U][2U][0U])));
        bufp->chgBit(oldp+762,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__complexOpInfo
                                      [0U] >> 2U))));
        bufp->chgCData(oldp+763,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__complexOpInfo
                                  [0U])),2);
        bufp->chgCData(oldp+764,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__complexOpInfo
                                  [0U])),2);
        bufp->chgBit(oldp+765,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__mulSubInfo
                                      [0U] >> 2U))));
        bufp->chgCData(oldp+766,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__mulSubInfo
                                  [0U])),2);
        bufp->chgBit(oldp+767,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__divSubInfo
                                      [0U] >> 2U))));
        bufp->chgCData(oldp+768,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__divSubInfo
                                  [0U])),2);
        bufp->chgBit(oldp+769,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__fuOpA
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+770,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__fuOpA
                                         [0U])),32);
        bufp->chgBit(oldp+771,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__fuOpB
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+772,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__fuOpB
                                         [0U])),32);
        bufp->chgBit(oldp+773,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__regValid[0]));
        bufp->chgBit(oldp+774,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__dataOut
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+775,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__dataOut
                                         [0U])),32);
        bufp->chgSData(oldp+776,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                  [0U][3U] >> 0x16U)),10);
        bufp->chgCData(oldp+777,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                        [0U][3U] >> 0x14U))),2);
        bufp->chgBit(oldp+778,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][3U] >> 0x13U))));
        bufp->chgSData(oldp+779,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 9U))),10);
        bufp->chgCData(oldp+780,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                        [0U][3U] >> 7U))),2);
        bufp->chgBit(oldp+781,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][3U] >> 6U))));
        bufp->chgCData(oldp+782,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                        [0U][3U] >> 4U))),2);
        bufp->chgCData(oldp+783,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                        [0U][3U] >> 1U))),3);
        bufp->chgCData(oldp+784,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                            [0U][3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                              [0U][2U] 
                                              >> 0x1bU)))),6);
        bufp->chgCData(oldp+785,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                          [0U][2U] 
                                          >> 0x17U))),4);
        bufp->chgCData(oldp+786,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                          [0U][2U] 
                                          >> 0x13U))),4);
        bufp->chgBit(oldp+787,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][2U] >> 0x12U))));
        bufp->chgCData(oldp+788,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+789,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][2U] >> 0xbU))));
        bufp->chgCData(oldp+790,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 5U))),6);
        bufp->chgBit(oldp+791,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][2U] >> 4U))));
        bufp->chgCData(oldp+792,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                            [0U][2U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                              [0U][1U] 
                                              >> 0x1eU)))),6);
        bufp->chgBit(oldp+793,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][1U] >> 0x1dU))));
        bufp->chgBit(oldp+794,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][1U] >> 0x1cU))));
        bufp->chgCData(oldp+795,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0x16U))),6);
        bufp->chgBit(oldp+796,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+797,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                              [0U][1U] 
                                              >> 2U))),19);
        bufp->chgBit(oldp+798,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                      [0U][1U] >> 1U))));
        bufp->chgBit(oldp+799,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                [0U][1U])));
        bufp->chgIData(oldp+800,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__nextStage
                                 [0U][0U]),32);
        bufp->chgIData(oldp+801,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__unnamedblk8__DOT__i),32);
        bufp->chgIData(oldp+802,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__unnamedblk8__DOT__unnamedblk9__DOT__j),32);
        bufp->chgIData(oldp+803,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInA
                                 [0U]),32);
        bufp->chgIData(oldp+804,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInB
                                 [0U]),32);
        bufp->chgCData(oldp+805,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divCode
                                 [0U]),2);
        bufp->chgBit(oldp+806,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulGetUpper
                               [0U]));
        bufp->chgCData(oldp+807,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                 [0U]),2);
        bufp->chgBit(oldp+808,((3U != vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                [0U])));
        bufp->chgBit(oldp+809,((1U & (~ ((3U == vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                          [0U]) | (2U 
                                                   == 
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                                   [0U]))))));
        bufp->chgQData(oldp+810,(((3U != vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                   [0U]) ? (((QData)((IData)(
                                                             (vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInA
                                                              [0U] 
                                                              >> 0x1fU))) 
                                             << 0x20U) 
                                            | (QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInA
                                                              [0U])))
                                   : (QData)((IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInA
                                                     [0U])))),33);
        bufp->chgQData(oldp+812,((((3U == vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                    [0U]) | (2U == 
                                             vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                             [0U]))
                                   ? (QData)((IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInB
                                                     [0U]))
                                   : (((QData)((IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInB
                                                        [0U] 
                                                        >> 0x1fU))) 
                                       << 0x20U) | (QData)((IData)(
                                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInB
                                                                   [0U]))))),33);
        bufp->chgSData(oldp+814,((vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                  [0U][3U] >> 0x16U)),10);
        bufp->chgCData(oldp+815,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                        [0U][3U] >> 0x14U))),2);
        bufp->chgBit(oldp+816,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x13U))));
        bufp->chgSData(oldp+817,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 9U))),10);
        bufp->chgCData(oldp+818,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                        [0U][3U] >> 7U))),2);
        bufp->chgBit(oldp+819,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][3U] >> 6U))));
        bufp->chgCData(oldp+820,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                        [0U][3U] >> 4U))),2);
        bufp->chgCData(oldp+821,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                        [0U][3U] >> 1U))),3);
        bufp->chgCData(oldp+822,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                              [0U][2U] 
                                              >> 0x1bU)))),6);
        bufp->chgCData(oldp+823,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                          [0U][2U] 
                                          >> 0x17U))),4);
        bufp->chgCData(oldp+824,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                          [0U][2U] 
                                          >> 0x13U))),4);
        bufp->chgBit(oldp+825,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x12U))));
        bufp->chgCData(oldp+826,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                           [0U][2U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+827,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][2U] >> 0xbU))));
        bufp->chgCData(oldp+828,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                           [0U][2U] 
                                           >> 5U))),6);
        bufp->chgBit(oldp+829,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][2U] >> 4U))));
        bufp->chgCData(oldp+830,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 0x1eU)))),6);
        bufp->chgBit(oldp+831,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x1dU))));
        bufp->chgBit(oldp+832,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x1cU))));
        bufp->chgCData(oldp+833,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0x16U))),6);
        bufp->chgBit(oldp+834,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+835,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 2U))),19);
        bufp->chgBit(oldp+836,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                      [0U][1U] >> 1U))));
        bufp->chgBit(oldp+837,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                [0U][1U])));
        bufp->chgIData(oldp+838,(vlSymsp->TOP__SMT_RTL_Testbench__core__complexExStageIF.__PVT__nextStage
                                 [0U][0U]),32);
        bufp->chgBit(oldp+839,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+840,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                        [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+841,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 0x11U))));
        bufp->chgBit(oldp+842,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 0x10U))));
        bufp->chgBit(oldp+843,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 0xfU))));
        bufp->chgBit(oldp+844,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 0xeU))));
        bufp->chgBit(oldp+845,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+846,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                        [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+847,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 0xaU))));
        bufp->chgBit(oldp+848,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 9U))));
        bufp->chgBit(oldp+849,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 8U))));
        bufp->chgBit(oldp+850,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+851,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+852,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                        [0U] >> 4U))),2);
        bufp->chgBit(oldp+853,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 3U))));
        bufp->chgBit(oldp+854,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 2U))));
        bufp->chgBit(oldp+855,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                      [0U] >> 1U))));
        bufp->chgBit(oldp+856,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlIn
                                [0U])));
        bufp->chgBit(oldp+857,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexDstRegDataOut
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+858,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexDstRegDataOut
                                         [0U])),32);
        bufp->chgIData(oldp+859,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInA[0]),32);
        bufp->chgIData(oldp+860,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__dataInB[0]),32);
        bufp->chgBit(oldp+861,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulGetUpper[0]));
        bufp->chgCData(oldp+862,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulCode
                                 [0U]),2);
        bufp->chgCData(oldp+863,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divCode
                                 [0U]),2);
        bufp->chgBit(oldp+864,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordEntry[0]));
        bufp->chgSData(oldp+865,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                            [0U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+866,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                        [0U][2U] >> 6U))),2);
        bufp->chgBit(oldp+867,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                      [0U][2U] >> 5U))));
        bufp->chgCData(oldp+868,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                        [0U][2U] >> 3U))),2);
        bufp->chgCData(oldp+869,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                  [0U][2U])),3);
        bufp->chgCData(oldp+870,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+871,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+872,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+873,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+874,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+875,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+876,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+877,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+878,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+879,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+880,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+881,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+882,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+883,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+884,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexRecordData
                                [0U][0U])));
        bufp->chgCData(oldp+885,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                        [0U][4U] >> 7U))),3);
        bufp->chgCData(oldp+886,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                        [0U][4U] >> 4U))),3);
        bufp->chgSData(oldp+887,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                            [0U][3U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+888,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                  [0U][3U])),2);
        bufp->chgSData(oldp+889,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                            [0U][3U] 
                                            >> 0xeU))),10);
        bufp->chgCData(oldp+890,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                        [0U][3U] >> 0xcU))),2);
        bufp->chgSData(oldp+891,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                             [0U][4U] 
                                             << 6U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                               [0U][3U] 
                                               >> 0x1aU)))),10);
        bufp->chgCData(oldp+892,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                        [0U][3U] >> 0x18U))),2);
        bufp->chgIData(oldp+893,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                 [0U][2U]),32);
        bufp->chgIData(oldp+894,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                 [0U][1U]),32);
        bufp->chgIData(oldp+895,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
                                 [0U][0U]),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x37U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x7eU])))) {
        bufp->chgIData(oldp+896,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0xaU] 
                                  >> 8U)),24);
        bufp->chgBit(oldp+897,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0xaU] 
                                      >> 7U))));
        bufp->chgCData(oldp+898,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0xaU] 
                                        >> 4U))),3);
        bufp->chgBit(oldp+899,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0xaU] 
                                      >> 3U))));
        bufp->chgCData(oldp+900,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0xaU])),3);
        bufp->chgIData(oldp+901,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[9U] 
                                  >> 0xcU)),20);
        bufp->chgBit(oldp+902,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[9U] 
                                      >> 0xbU))));
        bufp->chgCData(oldp+903,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[9U] 
                                        >> 8U))),3);
        bufp->chgBit(oldp+904,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[9U] 
                                      >> 7U))));
        bufp->chgCData(oldp+905,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[9U] 
                                        >> 4U))),3);
        bufp->chgBit(oldp+906,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[9U] 
                                      >> 3U))));
        bufp->chgCData(oldp+907,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[9U])),3);
        bufp->chgIData(oldp+908,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[8U] 
                                  >> 0xcU)),20);
        bufp->chgBit(oldp+909,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[8U] 
                                      >> 0xbU))));
        bufp->chgCData(oldp+910,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[8U] 
                                        >> 8U))),3);
        bufp->chgBit(oldp+911,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[8U] 
                                      >> 7U))));
        bufp->chgCData(oldp+912,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[8U] 
                                        >> 4U))),3);
        bufp->chgBit(oldp+913,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[8U] 
                                      >> 3U))));
        bufp->chgCData(oldp+914,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[8U])),3);
        bufp->chgBit(oldp+915,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[7U] 
                                >> 0x1fU)));
        bufp->chgIData(oldp+916,((0x3ffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[7U] 
                                                >> 5U))),26);
        bufp->chgCData(oldp+917,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[7U])),5);
        bufp->chgIData(oldp+918,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[6U] 
                                  >> 2U)),30);
        bufp->chgCData(oldp+919,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[6U])),2);
        bufp->chgIData(oldp+920,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[5U]),32);
        bufp->chgIData(oldp+921,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[4U]),32);
        bufp->chgIData(oldp+922,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[3U]),32);
        bufp->chgIData(oldp+923,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[2U]),32);
        bufp->chgIData(oldp+924,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[1U]),32);
        bufp->chgIData(oldp+925,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0U] 
                                  >> 8U)),24);
        bufp->chgCData(oldp+926,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0U] 
                                        >> 5U))),3);
        bufp->chgBit(oldp+927,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0U] 
                                      >> 4U))));
        bufp->chgBit(oldp+928,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0U] 
                                      >> 3U))));
        bufp->chgBit(oldp+929,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0U] 
                                      >> 2U))));
        bufp->chgBit(oldp+930,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0U] 
                                      >> 1U))));
        bufp->chgBit(oldp+931,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__csrNext[0U])));
        bufp->chgIData(oldp+932,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__rv),32);
        bufp->chgIData(oldp+933,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                  >> 8U)),24);
        bufp->chgBit(oldp+934,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                      >> 7U))));
        bufp->chgCData(oldp+935,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                        >> 4U))),3);
        bufp->chgBit(oldp+936,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                      >> 3U))));
        bufp->chgCData(oldp+937,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv)),3);
        bufp->chgIData(oldp+938,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                  >> 0xcU)),20);
        bufp->chgBit(oldp+939,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                      >> 0xbU))));
        bufp->chgCData(oldp+940,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                        >> 8U))),3);
        bufp->chgBit(oldp+941,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                >> 0x1fU)));
        bufp->chgIData(oldp+942,((0x3ffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                                >> 5U))),26);
        bufp->chgCData(oldp+943,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv)),5);
        bufp->chgIData(oldp+944,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                  >> 2U)),30);
        bufp->chgCData(oldp+945,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv)),2);
        bufp->chgIData(oldp+946,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv),32);
        bufp->chgCData(oldp+947,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                        >> 5U))),3);
        bufp->chgBit(oldp+948,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                      >> 4U))));
        bufp->chgBit(oldp+949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                      >> 2U))));
        bufp->chgBit(oldp+950,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv 
                                      >> 1U))));
        bufp->chgBit(oldp+951,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__wv)));
        bufp->chgIData(oldp+952,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__csrUnit__DOT__mcycle),32);
        bufp->chgIData(oldp+953,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                 [0U]),32);
        bufp->chgIData(oldp+954,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                 [0U]),32);
        bufp->chgBit(oldp+955,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__is_divide
                               [0U]));
        bufp->chgBit(oldp+956,((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                [0U] >> 0x1fU)));
        bufp->chgBit(oldp+957,((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                [0U] >> 0x1fU)));
        bufp->chgCData(oldp+958,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                           [0U] >> 0x17U))),8);
        bufp->chgCData(oldp+959,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                           [0U] >> 0x17U))),8);
        bufp->chgIData(oldp+960,((0x7fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                  [0U])),23);
        bufp->chgIData(oldp+961,((0x7fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                  [0U])),23);
        bufp->chgBit(oldp+962,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_zero));
        bufp->chgBit(oldp+963,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rhs_is_zero));
        bufp->chgBit(oldp+964,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_inf));
        bufp->chgBit(oldp+965,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rhs_is_inf));
        bufp->chgBit(oldp+966,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_nan));
        bufp->chgBit(oldp+967,(((0xffU == (0xffU & 
                                           (vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                            [0U] >> 0x17U))) 
                                & (0U != (0x7fffffU 
                                          & vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                          [0U])))));
        bufp->chgBit(oldp+968,(((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                 [0U] >> 0x1fU) & (0x80000000U 
                                                   != 
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                                   [0U]))));
        bufp->chgBit(oldp+969,((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__is_divide
                                [0U] ? ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_nan) 
                                        | (((0xffU 
                                             == (0xffU 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                                    [0U] 
                                                    >> 0x17U))) 
                                            & (0U != 
                                               (0x7fffffU 
                                                & vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                                [0U]))) 
                                           | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_zero) 
                                               & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rhs_is_zero)) 
                                              | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_inf) 
                                                 & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rhs_is_inf)))))
                                 : ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_nan) 
                                    | ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                        [0U] >> 0x1fU) 
                                       & (0x80000000U 
                                          != vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                          [0U]))))));
        bufp->chgBit(oldp+970,((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__is_divide
                                [0U] & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA
                                         [0U] ^ vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB
                                         [0U]) >> 0x1fU))));
        bufp->chgSData(oldp+971,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_lhs_expo),10);
        bufp->chgSData(oldp+972,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_rhs_expo),10);
        bufp->chgIData(oldp+973,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_lhs_mant),24);
        bufp->chgIData(oldp+974,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_rhs_mant),24);
        bufp->chgBit(oldp+975,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__stall));
        bufp->chgBit(oldp+976,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__clear));
        bufp->chgBit(oldp+977,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__flush
                               [0U][0U]));
        bufp->chgBit(oldp+978,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__flush
                               [0U][1U]));
        bufp->chgBit(oldp+979,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__flush
                               [0U][2U]));
        bufp->chgBit(oldp+980,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__flush
                               [0U][3U]));
        bufp->chgBit(oldp+981,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__flush
                               [0U][4U]));
        bufp->chgSData(oldp+982,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][0U][2U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+983,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                        [0U][0U][2U] 
                                        >> 0x11U))),2);
        bufp->chgCData(oldp+984,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                        [0U][0U][2U] 
                                        >> 0xeU))),3);
        bufp->chgCData(oldp+985,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][0U][2U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+986,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                        [0U][0U][2U] 
                                        >> 6U))),3);
        bufp->chgCData(oldp+987,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                        [0U][0U][2U] 
                                        >> 4U))),2);
        bufp->chgCData(oldp+988,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                        [0U][0U][2U] 
                                        >> 2U))),2);
        bufp->chgCData(oldp+989,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                  [0U][0U][2U])),2);
        bufp->chgCData(oldp+990,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                  [0U][0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+991,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                          [0U][0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+992,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                          [0U][0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+993,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                      [0U][0U][1U] 
                                      >> 0x11U))));
        bufp->chgCData(oldp+994,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+995,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                      [0U][0U][1U] 
                                      >> 0xaU))));
        bufp->chgCData(oldp+996,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+997,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                      [0U][0U][1U] 
                                      >> 3U))));
        bufp->chgCData(oldp+998,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                              [0U][0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+999,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                      [0U][0U][0U] 
                                      >> 0x1cU))));
        bufp->chgBit(oldp+1000,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][0U][0U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1001,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1002,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][0U][0U] 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1003,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1004,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                 [0U][0U][0U])));
        bufp->chgSData(oldp+1005,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][1U][2U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1006,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][1U][2U] 
                                         >> 0x11U))),2);
        bufp->chgCData(oldp+1007,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][1U][2U] 
                                         >> 0xeU))),3);
        bufp->chgCData(oldp+1008,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][1U][2U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1009,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][1U][2U] 
                                         >> 6U))),3);
        bufp->chgCData(oldp+1010,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][1U][2U] 
                                         >> 4U))),2);
        bufp->chgCData(oldp+1011,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][1U][2U] 
                                         >> 2U))),2);
        bufp->chgCData(oldp+1012,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][1U][2U])),2);
        bufp->chgCData(oldp+1013,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1014,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1015,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1016,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][1U][1U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+1017,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1018,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][1U][1U] 
                                       >> 0xaU))));
        bufp->chgCData(oldp+1019,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1020,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][1U][1U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1021,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1022,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][1U][0U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1023,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][1U][0U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1024,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1025,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][1U][0U] 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1026,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1027,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                 [0U][1U][0U])));
        bufp->chgSData(oldp+1028,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][2U][2U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1029,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][2U][2U] 
                                         >> 0x11U))),2);
        bufp->chgCData(oldp+1030,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][2U][2U] 
                                         >> 0xeU))),3);
        bufp->chgCData(oldp+1031,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][2U][2U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1032,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][2U][2U] 
                                         >> 6U))),3);
        bufp->chgCData(oldp+1033,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][2U][2U] 
                                         >> 4U))),2);
        bufp->chgCData(oldp+1034,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][2U][2U] 
                                         >> 2U))),2);
        bufp->chgCData(oldp+1035,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][2U][2U])),2);
        bufp->chgCData(oldp+1036,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][2U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1037,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][2U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1038,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][2U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1039,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][2U][1U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+1040,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][2U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1041,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][2U][1U] 
                                       >> 0xaU))));
        bufp->chgCData(oldp+1042,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][2U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1043,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][2U][1U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1044,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][2U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [2U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1045,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][2U][0U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1046,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][2U][0U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1047,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][2U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1048,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][2U][0U] 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1049,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [2U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1050,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                 [0U][2U][0U])));
        bufp->chgSData(oldp+1051,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][3U][2U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1052,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][3U][2U] 
                                         >> 0x11U))),2);
        bufp->chgCData(oldp+1053,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][3U][2U] 
                                         >> 0xeU))),3);
        bufp->chgCData(oldp+1054,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][3U][2U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1055,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][3U][2U] 
                                         >> 6U))),3);
        bufp->chgCData(oldp+1056,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][3U][2U] 
                                         >> 4U))),2);
        bufp->chgCData(oldp+1057,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][3U][2U] 
                                         >> 2U))),2);
        bufp->chgCData(oldp+1058,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][3U][2U])),2);
        bufp->chgCData(oldp+1059,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][3U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1060,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][3U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1061,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][3U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1062,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][3U][1U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+1063,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][3U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1064,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][3U][1U] 
                                       >> 0xaU))));
        bufp->chgCData(oldp+1065,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][3U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1066,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][3U][1U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1067,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][3U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [3U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1068,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][3U][0U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1069,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][3U][0U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1070,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][3U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1071,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][3U][0U] 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1072,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [3U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1073,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                 [0U][3U][0U])));
        bufp->chgSData(oldp+1074,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][4U][2U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1075,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][4U][2U] 
                                         >> 0x11U))),2);
        bufp->chgCData(oldp+1076,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][4U][2U] 
                                         >> 0xeU))),3);
        bufp->chgCData(oldp+1077,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][4U][2U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1078,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][4U][2U] 
                                         >> 6U))),3);
        bufp->chgCData(oldp+1079,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][4U][2U] 
                                         >> 4U))),2);
        bufp->chgCData(oldp+1080,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                         [0U][4U][2U] 
                                         >> 2U))),2);
        bufp->chgCData(oldp+1081,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][4U][2U])),2);
        bufp->chgCData(oldp+1082,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                   [0U][4U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1083,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][4U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1084,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                           [0U][4U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1085,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][4U][1U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+1086,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][4U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1087,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][4U][1U] 
                                       >> 0xaU))));
        bufp->chgCData(oldp+1088,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][4U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1089,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][4U][1U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1090,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                             [0U][4U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [4U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1091,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][4U][0U] 
                                       >> 0x1cU))));
        bufp->chgBit(oldp+1092,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][4U][0U] 
                                       >> 0x1bU))));
        bufp->chgCData(oldp+1093,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                            [0U][4U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1094,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                       [0U][4U][0U] 
                                       >> 0x14U))));
        bufp->chgIData(oldp+1095,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                               [0U]
                                               [4U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1096,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__iqData
                                 [0U][4U][0U])));
        bufp->chgCData(oldp+1097,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpOpInfo
                                         [0U] >> 0xeU))),3);
        bufp->chgCData(oldp+1098,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpOpInfo
                                            [0U] >> 9U))),5);
        bufp->chgCData(oldp+1099,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpOpInfo
                                         [0U] >> 6U))),3);
        bufp->chgCData(oldp+1100,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpOpInfo
                                         [0U] >> 4U))),2);
        bufp->chgCData(oldp+1101,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpOpInfo
                                         [0U] >> 2U))),2);
        bufp->chgCData(oldp+1102,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpOpInfo
                                   [0U])),2);
        bufp->chgCData(oldp+1103,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__opType
                                  [0U]),3);
        bufp->chgCData(oldp+1104,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpuCode
                                  [0U]),5);
        bufp->chgCData(oldp+1105,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__rm
                                  [0U]),3);
        bufp->chgCData(oldp+1106,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__stRM
                                  [0U]),3);
        bufp->chgCData(oldp+1107,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__dynRM
                                  [0U]),3);
        bufp->chgBit(oldp+1108,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1109,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpA
                                          [0U])),32);
        bufp->chgBit(oldp+1110,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1111,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpB
                                          [0U])),32);
        bufp->chgBit(oldp+1112,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpC
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1113,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpC
                                          [0U])),32);
        bufp->chgBit(oldp+1114,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__regValid[0]));
        bufp->chgBit(oldp+1115,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__dataOut
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1116,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__dataOut
                                          [0U])),32);
        bufp->chgBit(oldp+1117,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fflagsOut
                                       [0U] >> 4U))));
        bufp->chgBit(oldp+1118,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fflagsOut
                                       [0U] >> 3U))));
        bufp->chgBit(oldp+1119,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fflagsOut
                                       [0U] >> 2U))));
        bufp->chgBit(oldp+1120,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fflagsOut
                                       [0U] >> 1U))));
        bufp->chgBit(oldp+1121,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fflagsOut
                                 [0U])));
        bufp->chgIData(oldp+1122,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS[0]),32);
        bufp->chgIData(oldp+1123,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS[0]),32);
        bufp->chgIData(oldp+1124,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend[0]),32);
        bufp->chgSData(oldp+1125,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                             [0U][4U] 
                                             >> 6U))),10);
        bufp->chgCData(oldp+1126,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                         [0U][4U] >> 4U))),2);
        bufp->chgBit(oldp+1127,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][4U] >> 3U))));
        bufp->chgSData(oldp+1128,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                              [0U][4U] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                                [0U][3U] 
                                                >> 0x19U)))),10);
        bufp->chgCData(oldp+1129,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                         [0U][3U] >> 0x17U))),2);
        bufp->chgCData(oldp+1130,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                         [0U][3U] >> 0x14U))),3);
        bufp->chgCData(oldp+1131,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 0xfU))),5);
        bufp->chgCData(oldp+1132,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                         [0U][3U] >> 0xcU))),3);
        bufp->chgCData(oldp+1133,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                         [0U][3U] >> 0xaU))),2);
        bufp->chgCData(oldp+1134,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                         [0U][3U] >> 8U))),2);
        bufp->chgCData(oldp+1135,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                         [0U][3U] >> 6U))),2);
        bufp->chgCData(oldp+1136,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                   [0U][3U])),6);
        bufp->chgCData(oldp+1137,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                   [0U][2U] >> 0x1cU)),4);
        bufp->chgCData(oldp+1138,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 0x18U))),4);
        bufp->chgBit(oldp+1139,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][2U] >> 0x17U))));
        bufp->chgCData(oldp+1140,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                            [0U][2U] 
                                            >> 0x11U))),6);
        bufp->chgBit(oldp+1141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+1142,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                            [0U][2U] 
                                            >> 0xaU))),6);
        bufp->chgBit(oldp+1143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][2U] >> 9U))));
        bufp->chgCData(oldp+1144,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                            [0U][2U] 
                                            >> 3U))),6);
        bufp->chgBit(oldp+1145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][2U] >> 2U))));
        bufp->chgBit(oldp+1146,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][2U] >> 1U))));
        bufp->chgCData(oldp+1147,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                             [0U][2U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                               [0U][1U] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+1148,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][1U] >> 0x1aU))));
        bufp->chgIData(oldp+1149,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                               [0U][1U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+1150,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][1U] >> 6U))));
        bufp->chgBit(oldp+1151,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][1U] >> 5U))));
        bufp->chgIData(oldp+1152,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                    [0U][1U] << 0x1bU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                      [0U][0U] >> 5U))),32);
        bufp->chgBit(oldp+1153,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][0U] >> 4U))));
        bufp->chgBit(oldp+1154,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+1155,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+1156,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+1157,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__nextStage
                                 [0U][0U])));
        bufp->chgIData(oldp+1158,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                  [0U]),32);
        bufp->chgIData(oldp+1159,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                  [0U]),32);
        bufp->chgIData(oldp+1160,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                  [0U]),32);
        bufp->chgSData(oldp+1161,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulres_expo),10);
        bufp->chgBit(oldp+1162,(((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__addend_is_inf) 
                                 | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_9))));
        bufp->chgBit(oldp+1163,((((0xffU == (0xffU 
                                             & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                                [0U] 
                                                >> 0x17U))) 
                                  & (0U != (0x7fffffU 
                                            & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                            [0U]))) 
                                 | (((0xffU == (0xffU 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                                   [0U] 
                                                   >> 0x17U))) 
                                     & (0U != (0x7fffffU 
                                               & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                               [0U]))) 
                                    | (((0xffU == (0xffU 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                                      [0U] 
                                                      >> 0x17U))) 
                                        & (0U != (0x7fffffU 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                                  [0U]))) 
                                       | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mullhs_is_zero) 
                                           & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulrhs_is_inf)) 
                                          | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mullhs_is_inf) 
                                              & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulrhs_is_zero)) 
                                             | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__is_sub) 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_9) 
                                                   & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__addend_is_inf))))))))));
        bufp->chgBit(oldp+1164,(((~ ((0U == (0xffU 
                                             & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                                [0U] 
                                                >> 0x17U))) 
                                     & (0U == (0x7fffffU 
                                               & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                               [0U])))) 
                                 & (VL_LTS_III(32, 0x31U, 
                                               VL_EXTENDS_II(32,10, 
                                                             (0x3ffU 
                                                              & ((IData)(0x17U) 
                                                                 + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10))))) 
                                    | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mullhs_is_zero) 
                                       | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulrhs_is_zero))))));
        bufp->chgBit(oldp+1165,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mul_sign));
        bufp->chgBit(oldp+1166,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__addend_is_inf)
                                        ? (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                           [0U] >> 0x1fU)
                                        : (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mul_sign)))));
        bufp->chgBit(oldp+1167,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                 [0U] >> 0x1fU)));
        bufp->chgBit(oldp+1168,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__is_sub));
        __Vtemp_3[0U] = (((0U != (0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                           [0U] >> 0x17U))) 
                          << 0x19U) | (0x1fffffcU & 
                                       (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                        [0U] << 2U)));
        __Vtemp_3[1U] = 0U;
        __Vtemp_3[2U] = 0U;
        bufp->chgWData(oldp+1169,(__Vtemp_3),77);
        __Vtemp_4[0U] = (((0U != (0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                           [0U] >> 0x17U))) 
                          << 0x18U) | (0xfffffeU & 
                                       (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                        [0U] << 1U)));
        __Vtemp_4[1U] = 0U;
        __Vtemp_4[2U] = 0U;
        bufp->chgWData(oldp+1172,(__Vtemp_4),77);
        __Vtemp_7[0U] = 0U;
        __Vtemp_7[1U] = (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                         [0U] << 0x13U);
        __Vtemp_7[2U] = (((0U != (0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                           [0U] >> 0x17U))) 
                          << 0xaU) | (0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                                [0U] 
                                                >> 0xdU)));
        VL_SHIFTR_WWI(75,75,10, __Vtemp_8, __Vtemp_7, 
                      (0x3ffU & ((IData)(0x31U) - ((IData)(0x17U) 
                                                   + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10)))));
        __Vtemp_11[0U] = ((__Vtemp_8[0U] << 1U) | (
                                                   VL_GTS_III(32, 0U, 
                                                              VL_EXTENDS_II(32,10, 
                                                                            (0x3ffU 
                                                                             & ((IData)(0x17U) 
                                                                                + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10))))) 
                                                   & (VL_GTS_III(32, 0xffffffe6U, 
                                                                 VL_EXTENDS_II(32,10, 
                                                                               (0x3ffU 
                                                                                & ((IData)(0x17U) 
                                                                                + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10)))))
                                                       ? 
                                                      (0U 
                                                       != vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_13)
                                                       : 
                                                      (0U 
                                                       != 
                                                       (0xffffffU 
                                                        & VL_SHIFTL_III(24,24,10, vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_13, 
                                                                        (0x3ffU 
                                                                         & ((IData)(0x31U) 
                                                                            + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10)))))))));
        __Vtemp_11[1U] = ((__Vtemp_8[0U] >> 0x1fU) 
                          | (__Vtemp_8[1U] << 1U));
        __Vtemp_11[2U] = ((__Vtemp_8[1U] >> 0x1fU) 
                          | (0xffeU & (__Vtemp_8[2U] 
                                       << 1U)));
        bufp->chgWData(oldp+1175,(__Vtemp_11),77);
        bufp->chgBit(oldp+1178,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                 [0U] >> 0x1fU)));
        bufp->chgBit(oldp+1179,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                 [0U] >> 0x1fU)));
        bufp->chgCData(oldp+1180,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                            [0U] >> 0x17U))),8);
        bufp->chgCData(oldp+1181,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                            [0U] >> 0x17U))),8);
        bufp->chgCData(oldp+1182,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                            [0U] >> 0x17U))),8);
        bufp->chgIData(oldp+1183,((0x7fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                   [0U])),23);
        bufp->chgIData(oldp+1184,((0x7fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                   [0U])),23);
        bufp->chgIData(oldp+1185,((0x7fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                   [0U])),23);
        bufp->chgBit(oldp+1186,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mullhs_is_zero));
        bufp->chgBit(oldp+1187,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulrhs_is_zero));
        bufp->chgBit(oldp+1188,(((0U == (0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                                  [0U] 
                                                  >> 0x17U))) 
                                 & (0U == (0x7fffffU 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                           [0U])))));
        bufp->chgBit(oldp+1189,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mullhs_is_inf));
        bufp->chgBit(oldp+1190,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulrhs_is_inf));
        bufp->chgBit(oldp+1191,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__addend_is_inf));
        bufp->chgBit(oldp+1192,(((0xffU == (0xffU & 
                                            (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                             [0U] >> 0x17U))) 
                                 & (0U != (0x7fffffU 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                           [0U])))));
        bufp->chgBit(oldp+1193,(((0xffU == (0xffU & 
                                            (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                             [0U] >> 0x17U))) 
                                 & (0U != (0x7fffffU 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                           [0U])))));
        bufp->chgBit(oldp+1194,(((0xffU == (0xffU & 
                                            (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                             [0U] >> 0x17U))) 
                                 & (0U != (0x7fffffU 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                           [0U])))));
        bufp->chgSData(oldp+1195,(((0U == (0xffU & 
                                           (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                            [0U] >> 0x17U)))
                                    ? 1U : (0xffU & 
                                            (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulLHS
                                             [0U] >> 0x17U)))),10);
        bufp->chgSData(oldp+1196,(((0U == (0xffU & 
                                           (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                            [0U] >> 0x17U)))
                                    ? 1U : (0xffU & 
                                            (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaMulRHS
                                             [0U] >> 0x17U)))),10);
        bufp->chgSData(oldp+1197,(((0U == (0xffU & 
                                           (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                            [0U] >> 0x17U)))
                                    ? 1U : (0xffU & 
                                            (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaAddend
                                             [0U] >> 0x17U)))),10);
        bufp->chgSData(oldp+1198,((0x3ffU & ((IData)(0x17U) 
                                             + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10)))),10);
        bufp->chgBit(oldp+1199,((VL_GTS_III(32, 0U, 
                                            VL_EXTENDS_II(32,10, 
                                                          (0x3ffU 
                                                           & ((IData)(0x17U) 
                                                              + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10))))) 
                                 & (VL_GTS_III(32, 0xffffffe6U, 
                                               VL_EXTENDS_II(32,10, 
                                                             (0x3ffU 
                                                              & ((IData)(0x17U) 
                                                                 + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10)))))
                                     ? (0U != vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_13)
                                     : (0U != (0xffffffU 
                                               & VL_SHIFTL_III(24,24,10, vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_13, 
                                                               (0x3ffU 
                                                                & ((IData)(0x31U) 
                                                                   + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10))))))))));
        bufp->chgIData(oldp+1200,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpA
                                          [0U])),32);
        bufp->chgIData(oldp+1201,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fuOpB
                                          [0U])),32);
        bufp->chgCData(oldp+1202,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fpuCode
                                  [0U]),5);
        bufp->chgCData(oldp+1203,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__rm
                                  [0U]),3);
        bufp->chgIData(oldp+1204,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__resultOut),32);
        bufp->chgBit(oldp+1205,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fflagsOut) 
                                       >> 4U))));
        bufp->chgBit(oldp+1206,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fflagsOut) 
                                       >> 3U))));
        bufp->chgBit(oldp+1207,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fflagsOut) 
                                       >> 2U))));
        bufp->chgBit(oldp+1208,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fflagsOut) 
                                       >> 1U))));
        bufp->chgBit(oldp+1209,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fflagsOut))));
        bufp->chgBit(oldp+1210,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_sign));
        bufp->chgBit(oldp+1211,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_sign));
        bufp->chgCData(oldp+1212,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_expo),8);
        bufp->chgCData(oldp+1213,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_expo),8);
        bufp->chgIData(oldp+1214,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_mant),23);
        bufp->chgIData(oldp+1215,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_mant),23);
        bufp->chgBit(oldp+1216,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_zero));
        bufp->chgBit(oldp+1217,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_zero));
        bufp->chgBit(oldp+1218,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_inf));
        bufp->chgBit(oldp+1219,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_inf));
        bufp->chgBit(oldp+1220,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_nan));
        bufp->chgBit(oldp+1221,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_nan));
        bufp->chgBit(oldp+1222,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_snan));
        bufp->chgBit(oldp+1223,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_snan));
        bufp->chgBit(oldp+1224,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_subnormal));
        bufp->chgBit(oldp+1225,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_normal));
        bufp->chgBit(oldp+1226,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_smaller));
        bufp->chgBit(oldp+1227,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_equal_rhs));
        bufp->chgBit(oldp+1228,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fmt_unsigned));
        bufp->chgIData(oldp+1229,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk9__DOT__i),32);
        bufp->chgIData(oldp+1230,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk9__DOT__unnamedblk10__DOT__j),32);
        bufp->chgSData(oldp+1231,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                              [0U][7U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                [0U][6U] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+1232,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+1233,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][6U] >> 0x17U))));
        bufp->chgSData(oldp+1234,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                             [0U][6U] 
                                             >> 0xdU))),10);
        bufp->chgCData(oldp+1235,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][6U] >> 0xbU))),2);
        bufp->chgCData(oldp+1236,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][6U] >> 8U))),3);
        bufp->chgCData(oldp+1237,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][6U] >> 5U))),3);
        bufp->chgCData(oldp+1238,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][6U] >> 3U))),2);
        bufp->chgCData(oldp+1239,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][6U] >> 1U))),2);
        bufp->chgSData(oldp+1240,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                              [0U][6U] 
                                              << 0xbU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                [0U][5U] 
                                                >> 0x15U)))),12);
        bufp->chgBit(oldp+1241,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][5U] >> 0x14U))));
        bufp->chgBit(oldp+1242,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][5U] >> 0x13U))));
        bufp->chgBit(oldp+1243,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][5U] >> 0x12U))));
        bufp->chgCData(oldp+1244,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][5U] >> 0x10U))),2);
        bufp->chgCData(oldp+1245,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 0xbU))),5);
        bufp->chgBit(oldp+1246,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][5U] >> 0xaU))));
        bufp->chgCData(oldp+1247,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][5U] >> 8U))),2);
        bufp->chgCData(oldp+1248,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][5U] >> 5U))),3);
        bufp->chgBit(oldp+1249,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][5U] >> 4U))));
        bufp->chgCData(oldp+1250,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                   [0U][5U])),4);
        bufp->chgCData(oldp+1251,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                   [0U][4U] >> 0x1cU)),4);
        bufp->chgBit(oldp+1252,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+1253,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][4U] >> 0x1aU))));
        bufp->chgCData(oldp+1254,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 0x14U))),6);
        bufp->chgCData(oldp+1255,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0x10U))),4);
        bufp->chgCData(oldp+1256,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0xcU))),4);
        bufp->chgBit(oldp+1257,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][4U] >> 0xbU))));
        bufp->chgCData(oldp+1258,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][4U] >> 4U))));
        bufp->chgCData(oldp+1260,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                             [0U][4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                               [0U][3U] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+1261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][3U] >> 0x1dU))));
        bufp->chgCData(oldp+1262,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 0x17U))),6);
        bufp->chgBit(oldp+1263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][3U] >> 0x16U))));
        bufp->chgBit(oldp+1264,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][3U] >> 0x15U))));
        bufp->chgCData(oldp+1265,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 0xfU))),6);
        bufp->chgBit(oldp+1266,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][3U] >> 0xeU))));
        bufp->chgIData(oldp+1267,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                [0U][3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                  [0U][2U] 
                                                  >> 0x1bU)))),19);
        bufp->chgBit(oldp+1268,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1269,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1270,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][2U] >> 0x18U))));
        bufp->chgIData(oldp+1271,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                    [0U][2U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                      [0U][1U] >> 0x18U))),32);
        bufp->chgIData(oldp+1272,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                    [0U][1U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                      [0U][0U] >> 0x18U))),32);
        bufp->chgCData(oldp+1273,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [0U][0U] >> 0x16U))),2);
        bufp->chgBit(oldp+1274,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1275,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1276,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                   [0U][0U])),20);
        bufp->chgSData(oldp+1277,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                              [1U][7U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                [1U][6U] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+1278,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+1279,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][6U] >> 0x17U))));
        bufp->chgSData(oldp+1280,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                             [1U][6U] 
                                             >> 0xdU))),10);
        bufp->chgCData(oldp+1281,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][6U] >> 0xbU))),2);
        bufp->chgCData(oldp+1282,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][6U] >> 8U))),3);
        bufp->chgCData(oldp+1283,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][6U] >> 5U))),3);
        bufp->chgCData(oldp+1284,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][6U] >> 3U))),2);
        bufp->chgCData(oldp+1285,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][6U] >> 1U))),2);
        bufp->chgSData(oldp+1286,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                              [1U][6U] 
                                              << 0xbU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                [1U][5U] 
                                                >> 0x15U)))),12);
        bufp->chgBit(oldp+1287,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][5U] >> 0x14U))));
        bufp->chgBit(oldp+1288,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][5U] >> 0x13U))));
        bufp->chgBit(oldp+1289,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][5U] >> 0x12U))));
        bufp->chgCData(oldp+1290,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][5U] >> 0x10U))),2);
        bufp->chgCData(oldp+1291,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [1U][5U] 
                                            >> 0xbU))),5);
        bufp->chgBit(oldp+1292,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][5U] >> 0xaU))));
        bufp->chgCData(oldp+1293,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][5U] >> 8U))),2);
        bufp->chgCData(oldp+1294,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][5U] >> 5U))),3);
        bufp->chgBit(oldp+1295,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][5U] >> 4U))));
        bufp->chgCData(oldp+1296,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                   [1U][5U])),4);
        bufp->chgCData(oldp+1297,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                   [1U][4U] >> 0x1cU)),4);
        bufp->chgBit(oldp+1298,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+1299,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][4U] >> 0x1aU))));
        bufp->chgCData(oldp+1300,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 0x14U))),6);
        bufp->chgCData(oldp+1301,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0x10U))),4);
        bufp->chgCData(oldp+1302,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0xcU))),4);
        bufp->chgBit(oldp+1303,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][4U] >> 0xbU))));
        bufp->chgCData(oldp+1304,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1305,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][4U] >> 4U))));
        bufp->chgCData(oldp+1306,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                             [1U][4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                               [1U][3U] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+1307,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][3U] >> 0x1dU))));
        bufp->chgCData(oldp+1308,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [1U][3U] 
                                            >> 0x17U))),6);
        bufp->chgBit(oldp+1309,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][3U] >> 0x16U))));
        bufp->chgBit(oldp+1310,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][3U] >> 0x15U))));
        bufp->chgCData(oldp+1311,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                            [1U][3U] 
                                            >> 0xfU))),6);
        bufp->chgBit(oldp+1312,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][3U] >> 0xeU))));
        bufp->chgIData(oldp+1313,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                [1U][3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                                  [1U][2U] 
                                                  >> 0x1bU)))),19);
        bufp->chgBit(oldp+1314,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1315,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1316,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][2U] >> 0x18U))));
        bufp->chgIData(oldp+1317,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                    [1U][2U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                      [1U][1U] >> 0x18U))),32);
        bufp->chgIData(oldp+1318,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                    [1U][1U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                      [1U][0U] >> 0x18U))),32);
        bufp->chgCData(oldp+1319,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                         [1U][0U] >> 0x16U))),2);
        bufp->chgBit(oldp+1320,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1321,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1322,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__nextStage
                                   [1U][0U])),20);
        bufp->chgBit(oldp+1323,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__refetchFromCSR));
        bufp->chgBit(oldp+1324,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__recoveredPC 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1325,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__recoveredPC)),19);
        bufp->chgSData(oldp+1326,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                              [0U][7U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                [0U][6U] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+1327,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+1328,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][6U] >> 0x17U))));
        bufp->chgSData(oldp+1329,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                             [0U][6U] 
                                             >> 0xdU))),10);
        bufp->chgCData(oldp+1330,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][6U] >> 0xbU))),2);
        bufp->chgCData(oldp+1331,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][6U] >> 8U))),3);
        bufp->chgCData(oldp+1332,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][6U] >> 5U))),3);
        bufp->chgCData(oldp+1333,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][6U] >> 3U))),2);
        bufp->chgCData(oldp+1334,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][6U] >> 1U))),2);
        bufp->chgSData(oldp+1335,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                              [0U][6U] 
                                              << 0xbU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                [0U][5U] 
                                                >> 0x15U)))),12);
        bufp->chgBit(oldp+1336,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][5U] >> 0x14U))));
        bufp->chgBit(oldp+1337,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][5U] >> 0x13U))));
        bufp->chgBit(oldp+1338,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][5U] >> 0x12U))));
        bufp->chgCData(oldp+1339,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][5U] >> 0x10U))),2);
        bufp->chgCData(oldp+1340,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            >> 0xbU))),5);
        bufp->chgBit(oldp+1341,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][5U] >> 0xaU))));
        bufp->chgCData(oldp+1342,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][5U] >> 8U))),2);
        bufp->chgCData(oldp+1343,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][5U] >> 5U))),3);
        bufp->chgBit(oldp+1344,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][5U] >> 4U))));
        bufp->chgCData(oldp+1345,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                   [0U][5U])),4);
        bufp->chgCData(oldp+1346,((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                   [0U][4U] >> 0x1cU)),4);
        bufp->chgBit(oldp+1347,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+1348,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x1aU))));
        bufp->chgCData(oldp+1349,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 0x14U))),6);
        bufp->chgCData(oldp+1350,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0x10U))),4);
        bufp->chgCData(oldp+1351,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0xcU))),4);
        bufp->chgBit(oldp+1352,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][4U] >> 0xbU))));
        bufp->chgCData(oldp+1353,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1354,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][4U] >> 4U))));
        bufp->chgCData(oldp+1355,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                             [0U][4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+1356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x1dU))));
        bufp->chgCData(oldp+1357,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0x17U))),6);
        bufp->chgBit(oldp+1358,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x16U))));
        bufp->chgBit(oldp+1359,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x15U))));
        bufp->chgCData(oldp+1360,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0xfU))),6);
        bufp->chgBit(oldp+1361,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][3U] >> 0xeU))));
        bufp->chgIData(oldp+1362,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                [0U][3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                  [0U][2U] 
                                                  >> 0x1bU)))),19);
        bufp->chgBit(oldp+1363,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1364,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1365,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x18U))));
        bufp->chgIData(oldp+1366,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                    [0U][2U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x18U))),32);
        bufp->chgIData(oldp+1367,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                    [0U][1U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x18U))),32);
        bufp->chgCData(oldp+1368,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [0U][0U] >> 0x16U))),2);
        bufp->chgBit(oldp+1369,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1370,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1371,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                   [0U][0U])),20);
        bufp->chgSData(oldp+1372,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                              [1U][7U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                [1U][6U] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+1373,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+1374,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][6U] >> 0x17U))));
        bufp->chgSData(oldp+1375,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                             [1U][6U] 
                                             >> 0xdU))),10);
        bufp->chgCData(oldp+1376,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][6U] >> 0xbU))),2);
        bufp->chgCData(oldp+1377,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][6U] >> 8U))),3);
        bufp->chgCData(oldp+1378,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][6U] >> 5U))),3);
        bufp->chgCData(oldp+1379,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][6U] >> 3U))),2);
        bufp->chgCData(oldp+1380,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][6U] >> 1U))),2);
        bufp->chgSData(oldp+1381,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                              [1U][6U] 
                                              << 0xbU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                [1U][5U] 
                                                >> 0x15U)))),12);
        bufp->chgBit(oldp+1382,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][5U] >> 0x14U))));
        bufp->chgBit(oldp+1383,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][5U] >> 0x13U))));
        bufp->chgBit(oldp+1384,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][5U] >> 0x12U))));
        bufp->chgCData(oldp+1385,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][5U] >> 0x10U))),2);
        bufp->chgCData(oldp+1386,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [1U][5U] 
                                            >> 0xbU))),5);
        bufp->chgBit(oldp+1387,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][5U] >> 0xaU))));
        bufp->chgCData(oldp+1388,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][5U] >> 8U))),2);
        bufp->chgCData(oldp+1389,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][5U] >> 5U))),3);
        bufp->chgBit(oldp+1390,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][5U] >> 4U))));
        bufp->chgCData(oldp+1391,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                   [1U][5U])),4);
        bufp->chgCData(oldp+1392,((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                   [1U][4U] >> 0x1cU)),4);
        bufp->chgBit(oldp+1393,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][4U] >> 0x1bU))));
        bufp->chgBit(oldp+1394,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][4U] >> 0x1aU))));
        bufp->chgCData(oldp+1395,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 0x14U))),6);
        bufp->chgCData(oldp+1396,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 0x10U))),4);
        bufp->chgCData(oldp+1397,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 0xcU))),4);
        bufp->chgBit(oldp+1398,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][4U] >> 0xbU))));
        bufp->chgCData(oldp+1399,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1400,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][4U] >> 4U))));
        bufp->chgCData(oldp+1401,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                             [1U][4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+1402,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x1dU))));
        bufp->chgCData(oldp+1403,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0x17U))),6);
        bufp->chgBit(oldp+1404,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x16U))));
        bufp->chgBit(oldp+1405,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x15U))));
        bufp->chgCData(oldp+1406,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0xfU))),6);
        bufp->chgBit(oldp+1407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][3U] >> 0xeU))));
        bufp->chgIData(oldp+1408,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                [1U][3U] 
                                                << 5U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                                  [1U][2U] 
                                                  >> 0x1bU)))),19);
        bufp->chgBit(oldp+1409,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1410,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1411,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x18U))));
        bufp->chgIData(oldp+1412,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                    [1U][2U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x18U))),32);
        bufp->chgIData(oldp+1413,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                    [1U][1U] << 8U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x18U))),32);
        bufp->chgCData(oldp+1414,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                         [1U][0U] >> 0x16U))),2);
        bufp->chgBit(oldp+1415,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1416,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1417,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__memExStageIF.__PVT__nextStage
                                   [1U][0U])),20);
        bufp->chgSData(oldp+1418,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                             [0U][4U] 
                                             >> 6U))),10);
        bufp->chgCData(oldp+1419,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                         [0U][4U] >> 4U))),2);
        bufp->chgBit(oldp+1420,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][4U] >> 3U))));
        bufp->chgSData(oldp+1421,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                              [0U][4U] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                                [0U][3U] 
                                                >> 0x19U)))),10);
        bufp->chgCData(oldp+1422,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                         [0U][3U] >> 0x17U))),2);
        bufp->chgCData(oldp+1423,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                         [0U][3U] >> 0x14U))),3);
        bufp->chgCData(oldp+1424,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0xfU))),5);
        bufp->chgCData(oldp+1425,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                         [0U][3U] >> 0xcU))),3);
        bufp->chgCData(oldp+1426,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                         [0U][3U] >> 0xaU))),2);
        bufp->chgCData(oldp+1427,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                         [0U][3U] >> 8U))),2);
        bufp->chgCData(oldp+1428,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                         [0U][3U] >> 6U))),2);
        bufp->chgCData(oldp+1429,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                   [0U][3U])),6);
        bufp->chgCData(oldp+1430,((vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                   [0U][2U] >> 0x1cU)),4);
        bufp->chgCData(oldp+1431,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                           [0U][2U] 
                                           >> 0x18U))),4);
        bufp->chgBit(oldp+1432,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x17U))));
        bufp->chgCData(oldp+1433,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            >> 0x11U))),6);
        bufp->chgBit(oldp+1434,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+1435,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            >> 0xaU))),6);
        bufp->chgBit(oldp+1436,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][2U] >> 9U))));
        bufp->chgCData(oldp+1437,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            >> 3U))),6);
        bufp->chgBit(oldp+1438,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][2U] >> 2U))));
        bufp->chgBit(oldp+1439,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][2U] >> 1U))));
        bufp->chgCData(oldp+1440,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                             [0U][2U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                               [0U][1U] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+1441,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x1aU))));
        bufp->chgIData(oldp+1442,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                               [0U][1U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+1443,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][1U] >> 6U))));
        bufp->chgBit(oldp+1444,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][1U] >> 5U))));
        bufp->chgIData(oldp+1445,(((vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                    [0U][1U] << 0x1bU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                      [0U][0U] >> 5U))),32);
        bufp->chgBit(oldp+1446,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][0U] >> 4U))));
        bufp->chgBit(oldp+1447,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+1448,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+1449,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+1450,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__fpExStageIF.__PVT__nextStage
                                 [0U][0U])));
        bufp->chgBit(oldp+1451,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 0x14U))));
        bufp->chgCData(oldp+1452,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                         [0U] >> 0x12U))),2);
        bufp->chgBit(oldp+1453,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 0x11U))));
        bufp->chgBit(oldp+1454,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 0x10U))));
        bufp->chgBit(oldp+1455,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 0xfU))));
        bufp->chgBit(oldp+1456,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 0xeU))));
        bufp->chgBit(oldp+1457,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 0xdU))));
        bufp->chgCData(oldp+1458,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                         [0U] >> 0xbU))),2);
        bufp->chgBit(oldp+1459,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 0xaU))));
        bufp->chgBit(oldp+1460,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 9U))));
        bufp->chgBit(oldp+1461,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 8U))));
        bufp->chgBit(oldp+1462,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+1463,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1464,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                         [0U] >> 4U))),2);
        bufp->chgBit(oldp+1465,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 3U))));
        bufp->chgBit(oldp+1466,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 2U))));
        bufp->chgBit(oldp+1467,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                       [0U] >> 1U))));
        bufp->chgBit(oldp+1468,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpCtrlIn
                                 [0U])));
        bufp->chgBit(oldp+1469,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpDstRegDataOut
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1470,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpDstRegDataOut
                                          [0U])),32);
        bufp->chgIData(oldp+1471,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInA[0]),32);
        bufp->chgIData(oldp+1472,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__dataInB[0]),32);
        bufp->chgBit(oldp+1473,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__is_divide[0]));
        bufp->chgCData(oldp+1474,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__rm
                                  [0U]),3);
        bufp->chgBit(oldp+1475,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordEntry[0]));
        bufp->chgSData(oldp+1476,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                             [0U][2U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1477,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                         [0U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+1478,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                         [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+1479,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                            [0U][2U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1480,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                         [0U][2U] >> 6U))),3);
        bufp->chgCData(oldp+1481,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                         [0U][2U] >> 4U))),2);
        bufp->chgCData(oldp+1482,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                         [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+1483,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                   [0U][2U])),2);
        bufp->chgCData(oldp+1484,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1485,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1486,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1487,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+1488,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1489,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+1490,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1491,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+1492,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1493,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+1494,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+1495,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1496,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1497,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1498,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpRecordData
                                 [0U][0U])));
        bufp->chgIData(oldp+1499,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrReadOut),32);
        bufp->chgIData(oldp+1500,((vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0xaU] 
                                   >> 8U)),24);
        bufp->chgBit(oldp+1501,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0xaU] 
                                       >> 7U))));
        bufp->chgCData(oldp+1502,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0xaU] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1503,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0xaU] 
                                       >> 3U))));
        bufp->chgCData(oldp+1504,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0xaU])),3);
        bufp->chgIData(oldp+1505,((vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[9U] 
                                   >> 0xcU)),20);
        bufp->chgBit(oldp+1506,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[9U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1507,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[9U] 
                                         >> 8U))),3);
        bufp->chgBit(oldp+1508,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[9U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1509,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[9U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1510,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[9U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1511,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[9U])),3);
        bufp->chgIData(oldp+1512,((vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[8U] 
                                   >> 0xcU)),20);
        bufp->chgBit(oldp+1513,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[8U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1514,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[8U] 
                                         >> 8U))),3);
        bufp->chgBit(oldp+1515,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[8U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1516,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[8U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1517,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[8U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1518,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[8U])),3);
        bufp->chgBit(oldp+1519,((vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[7U] 
                                 >> 0x1fU)));
        bufp->chgIData(oldp+1520,((0x3ffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[7U] 
                                                 >> 5U))),26);
        bufp->chgCData(oldp+1521,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[7U])),5);
        bufp->chgIData(oldp+1522,((vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[6U] 
                                   >> 2U)),30);
        bufp->chgCData(oldp+1523,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[6U])),2);
        bufp->chgIData(oldp+1524,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[5U]),32);
        bufp->chgIData(oldp+1525,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[4U]),32);
        bufp->chgIData(oldp+1526,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[3U]),32);
        bufp->chgIData(oldp+1527,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[2U]),32);
        bufp->chgIData(oldp+1528,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[1U]),32);
        bufp->chgIData(oldp+1529,((vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0U] 
                                   >> 8U)),24);
        bufp->chgCData(oldp+1530,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0U] 
                                         >> 5U))),3);
        bufp->chgBit(oldp+1531,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0U] 
                                       >> 4U))));
        bufp->chgBit(oldp+1532,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0U] 
                                       >> 3U))));
        bufp->chgBit(oldp+1533,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0U] 
                                       >> 2U))));
        bufp->chgBit(oldp+1534,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0U] 
                                       >> 1U))));
        bufp->chgBit(oldp+1535,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__csrWholeOut[0U])));
        bufp->chgBit(oldp+1536,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__triggerExcpt));
        bufp->chgIData(oldp+1537,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__excptTargetAddr),32);
        bufp->chgCData(oldp+1538,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__externalInterruptCodeInCSR),5);
        bufp->chgBit(oldp+1539,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflags) 
                                       >> 4U))));
        bufp->chgBit(oldp+1540,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflags) 
                                       >> 3U))));
        bufp->chgBit(oldp+1541,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflags) 
                                       >> 2U))));
        bufp->chgBit(oldp+1542,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflags) 
                                       >> 1U))));
        bufp->chgBit(oldp+1543,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__fflags))));
        bufp->chgCData(oldp+1544,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__frm),3);
        bufp->chgBit(oldp+1545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                       [0U][3U] >> 0x13U))));
        bufp->chgBit(oldp+1546,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                       [0U][3U] >> 0x12U))));
        bufp->chgSData(oldp+1547,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                             [0U][3U] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+1548,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                         [0U][3U] >> 6U))),2);
        bufp->chgIData(oldp+1549,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                    [0U][3U] << 0x1aU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                      [0U][2U] >> 6U))),32);
        bufp->chgIData(oldp+1550,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                    [0U][2U] << 0x1aU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                      [0U][1U] >> 6U))),32);
        bufp->chgIData(oldp+1551,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                    [0U][1U] << 0x1aU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                      [0U][0U] >> 6U))),32);
        bufp->chgCData(oldp+1552,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                         [0U][0U] >> 3U))),3);
        bufp->chgCData(oldp+1553,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                         [0U][0U] >> 1U))),2);
        bufp->chgBit(oldp+1554,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                 [0U][0U])));
        bufp->chgBit(oldp+1555,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                       [1U][3U] >> 0x13U))));
        bufp->chgBit(oldp+1556,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                       [1U][3U] >> 0x12U))));
        bufp->chgSData(oldp+1557,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                             [1U][3U] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+1558,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                         [1U][3U] >> 6U))),2);
        bufp->chgIData(oldp+1559,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                    [1U][3U] << 0x1aU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                      [1U][2U] >> 6U))),32);
        bufp->chgIData(oldp+1560,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                    [1U][2U] << 0x1aU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                      [1U][1U] >> 6U))),32);
        bufp->chgIData(oldp+1561,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                    [1U][1U] << 0x1aU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                      [1U][0U] >> 6U))),32);
        bufp->chgCData(oldp+1562,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                         [1U][0U] >> 3U))),3);
        bufp->chgCData(oldp+1563,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                         [1U][0U] >> 1U))),2);
        bufp->chgBit(oldp+1564,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
                                 [1U][0U])));
        bufp->chgCData(oldp+1565,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                            [0U][6U] 
                                            >> 1U))),5);
        bufp->chgCData(oldp+1566,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                             [0U][6U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                               [0U][5U] 
                                               >> 0x1cU)))),5);
        bufp->chgSData(oldp+1567,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                             [0U][4U] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+1568,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                   [0U][4U])),2);
        bufp->chgSData(oldp+1569,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                             [0U][4U] 
                                             >> 0xeU))),10);
        bufp->chgCData(oldp+1570,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                         [0U][4U] >> 0xcU))),2);
        bufp->chgSData(oldp+1571,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                              [0U][5U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                                [0U][4U] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+1572,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                         [0U][4U] >> 0x18U))),2);
        bufp->chgSData(oldp+1573,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                             [0U][5U] 
                                             >> 6U))),10);
        bufp->chgCData(oldp+1574,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                         [0U][5U] >> 4U))),2);
        bufp->chgSData(oldp+1575,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                             [0U][5U] 
                                             >> 0x12U))),10);
        bufp->chgCData(oldp+1576,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                         [0U][5U] >> 0x10U))),2);
        bufp->chgIData(oldp+1577,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                  [0U][3U]),32);
        bufp->chgIData(oldp+1578,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                  [0U][2U]),32);
        bufp->chgIData(oldp+1579,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                  [0U][1U]),32);
        bufp->chgIData(oldp+1580,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
                                  [0U][0U]),32);
        bufp->chgIData(oldp+1581,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwCommit),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x38U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x7fU])))) {
        bufp->chgBit(oldp+1582,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__reset));
        bufp->chgCData(oldp+1583,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedLoadQueuePtrByStore[0]),4);
        bufp->chgSData(oldp+1584,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__addrMatch[0]),16);
        bufp->chgCData(oldp+1585,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__pickedPtr[0]),4);
        bufp->chgBit(oldp+1586,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__picked[0]));
        bufp->chgBit(oldp+1587,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__violation[0]));
        bufp->chgIData(oldp+1588,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedStoreAddr[0]),20);
        bufp->chgBit(oldp+1589,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedStoreWordWE[0]));
        bufp->chgBit(oldp+1590,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__conflictLoadPC
                                       [0U] >> 0x13U))));
        bufp->chgIData(oldp+1591,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__conflictLoadPC
                                   [0U])),19);
        bufp->chgCData(oldp+1592,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__executedLoadQueuePtrByStore
                                  [0U]),4);
        bufp->chgSData(oldp+1593,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__addrMatch
                                  [0U]),16);
        bufp->chgCData(oldp+1594,(vlSymsp->TOP__SMT_RTL_Testbench__core.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr),4);
        bufp->chgBit(oldp+1595,(vlSymsp->TOP__SMT_RTL_Testbench__core.loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked));
        bufp->chgIData(oldp+1596,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__addrMatch
                                    [0U] << 0x10U) 
                                   | vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__addrMatch
                                   [0U])),32);
        bufp->chgSData(oldp+1597,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq),16);
        bufp->chgCData(oldp+1598,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant),4);
        bufp->chgIData(oldp+1599,((0x7fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__addrMatch
                                                   [0U] 
                                                   << 0x10U) 
                                                  | vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__addrMatch
                                                  [0U]))),31);
        bufp->chgIData(oldp+1600,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp),32);
        bufp->chgIData(oldp+1601,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk10__DOT__si),32);
        bufp->chgIData(oldp+1602,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk10__DOT__unnamedblk11__DOT__li),32);
        bufp->chgIData(oldp+1603,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk12__DOT__i),32);
        bufp->chgIData(oldp+1604,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk6__DOT__si),32);
        bufp->chgIData(oldp+1605,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk7__DOT__si),32);
        bufp->chgIData(oldp+1606,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk7__DOT__unnamedblk8__DOT__lqe),32);
        bufp->chgIData(oldp+1607,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk9__DOT__si),32);
        bufp->chgSData(oldp+1608,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dependStoreBitVector[0]),16);
        bufp->chgSData(oldp+1609,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dependStoreBitVector[1]),16);
        bufp->chgSData(oldp+1610,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__storeBitVector),16);
        bufp->chgBit(oldp+1611,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchStore[0]));
        bufp->chgBit(oldp+1612,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchStore[1]));
        bufp->chgBit(oldp+1613,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchLoad[0]));
        bufp->chgBit(oldp+1614,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchLoad[1]));
        bufp->chgSData(oldp+1615,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__notIssued),16);
        bufp->chgBit(oldp+1616,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__memDependencyPred[0]));
        bufp->chgBit(oldp+1617,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__memDependencyPred[1]));
        bufp->chgSData(oldp+1618,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__mdtWA[0]),10);
        bufp->chgBit(oldp+1619,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__mdtRV
                                [0U]));
        bufp->chgBit(oldp+1620,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__mdtRV
                                [1U]));
        bufp->chgBit(oldp+1621,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__prediction[0]));
        bufp->chgBit(oldp+1622,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__prediction[1]));
        bufp->chgBit(oldp+1623,(vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__memDependencyPred[0]));
        bufp->chgBit(oldp+1624,(vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__memDependencyPred[1]));
        bufp->chgBit(oldp+1625,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__conflict[0]));
        bufp->chgBit(oldp+1626,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__conflictLoadPC
                                       [0U] >> 0x13U))));
        bufp->chgIData(oldp+1627,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__conflictLoadPC
                                   [0U])),19);
        bufp->chgBit(oldp+1628,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memDependencyPred[0]));
        bufp->chgBit(oldp+1629,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memDependencyPred[1]));
        bufp->chgBit(oldp+1630,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__memDependencyPred[0]));
        bufp->chgBit(oldp+1631,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__memDependencyPred[1]));
        bufp->chgSData(oldp+1632,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__wa[0]),10);
        bufp->chgBit(oldp+1633,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__rv[0]));
        bufp->chgBit(oldp+1634,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__rv[1]));
        bufp->chgSData(oldp+1635,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),10);
        bufp->chgSData(oldp+1636,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),10);
        bufp->chgSData(oldp+1637,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),10);
        bufp->chgSData(oldp+1638,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),10);
        bufp->chgBit(oldp+1639,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]));
        bufp->chgBit(oldp+1640,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]));
        bufp->chgBit(oldp+1641,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+1642,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+1643,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [0U]));
        bufp->chgSData(oldp+1644,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__waBank
                                             [0U] >> 1U))),9);
        bufp->chgBit(oldp+1645,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                [0U]));
        bufp->chgSData(oldp+1646,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__raBank
                                             [0U] >> 1U))),9);
        bufp->chgBit(oldp+1647,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [1U]));
        bufp->chgSData(oldp+1648,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__waBank
                                             [1U] >> 1U))),9);
        bufp->chgBit(oldp+1649,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                [1U]));
        bufp->chgSData(oldp+1650,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__raBank
                                             [1U] >> 1U))),9);
        bufp->chgIData(oldp+1651,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__b),32);
        bufp->chgIData(oldp+1652,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+1653,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__b),32);
        bufp->chgIData(oldp+1654,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+1655,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+1656,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x39U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x80U])))) {
        bufp->chgIData(oldp+1657,(vlSymsp->TOP__SMT_RTL_Testbench__core.intExStage__DOT____Vcellout__BlockALU__BRA__0__KET____DOT__intALU__aluDataOut),32);
        bufp->chgBit(oldp+1658,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1659,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst)),32);
        bufp->chgIData(oldp+1660,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA),32);
        bufp->chgIData(oldp+1661,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB),32);
        bufp->chgIData(oldp+1662,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpA),32);
        bufp->chgIData(oldp+1663,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpB),32);
        bufp->chgBit(oldp+1664,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1665,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst)),32);
        bufp->chgIData(oldp+1666,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA),32);
        bufp->chgIData(oldp+1667,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB),32);
        bufp->chgBit(oldp+1668,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderOutOverflow));
        bufp->chgIData(oldp+1669,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpA),32);
        bufp->chgIData(oldp+1670,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpB),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x3aU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x81U])))) {
        bufp->chgIData(oldp+1671,(vlSymsp->TOP__SMT_RTL_Testbench__core.intExStage__DOT____Vcellout__BlockALU__BRA__1__KET____DOT__intALU__aluDataOut),32);
        bufp->chgBit(oldp+1672,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1673,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst)),32);
        bufp->chgIData(oldp+1674,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA),32);
        bufp->chgIData(oldp+1675,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB),32);
        bufp->chgIData(oldp+1676,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpA),32);
        bufp->chgIData(oldp+1677,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpB),32);
        bufp->chgBit(oldp+1678,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1679,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst)),32);
        bufp->chgIData(oldp+1680,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA),32);
        bufp->chgIData(oldp+1681,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB),32);
        bufp->chgBit(oldp+1682,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderOutOverflow));
        bufp->chgIData(oldp+1683,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpA),32);
        bufp->chgIData(oldp+1684,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpB),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x3bU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x82U])))) {
        bufp->chgBit(oldp+1685,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexExStage__DOT__isDiv[0]));
        bufp->chgBit(oldp+1686,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__finished[0]));
        bufp->chgBit(oldp+1687,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReq
                                [0U]));
        bufp->chgBit(oldp+1688,(vlSymsp->TOP__SMT_RTL_Testbench__core.mulDivUnit__DOT____Vcellout__BlockDivUnit__BRA__0__KET____DOT__divUnit__finished));
        bufp->chgBit(oldp+1689,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextIsSigned));
        bufp->chgIData(oldp+1690,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend),32);
        bufp->chgIData(oldp+1691,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor),32);
        bufp->chgIData(oldp+1692,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__quotient),32);
        bufp->chgIData(oldp+1693,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__remainder),32);
        bufp->chgQData(oldp+1694,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextZ),33);
        bufp->chgQData(oldp+1696,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextD),33);
        bufp->chgQData(oldp+1698,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ),33);
        bufp->chgQData(oldp+1700,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR),33);
        bufp->chgBit(oldp+1702,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextSigned));
        bufp->chgCData(oldp+1703,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter),6);
        bufp->chgCData(oldp+1704,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextPhase),2);
        bufp->chgIData(oldp+1705,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divDataOut[0]),32);
        bufp->chgBit(oldp+1706,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__divReq[0]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x3cU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x83U])))) {
        bufp->chgBit(oldp+1707,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__reqInterrupt));
        bufp->chgBit(oldp+1708,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__triggerInterrupt));
        bufp->chgBit(oldp+1709,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__reqTimerInterrupt));
        bufp->chgBit(oldp+1710,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__reqExternalInterrupt));
        bufp->chgCData(oldp+1711,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__interruptCode),5);
        bufp->chgBit(oldp+1712,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__interruptTargetAddr 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1713,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__interruptTargetAddr)),19);
        bufp->chgIData(oldp+1714,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0xaU] 
                                   >> 8U)),24);
        bufp->chgBit(oldp+1715,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0xaU] 
                                       >> 7U))));
        bufp->chgCData(oldp+1716,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0xaU] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1717,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0xaU] 
                                       >> 3U))));
        bufp->chgCData(oldp+1718,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0xaU])),3);
        bufp->chgIData(oldp+1719,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[9U] 
                                   >> 0xcU)),20);
        bufp->chgBit(oldp+1720,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[9U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1721,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[9U] 
                                         >> 8U))),3);
        bufp->chgBit(oldp+1722,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[9U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1723,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[9U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1724,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[9U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1725,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[9U])),3);
        bufp->chgIData(oldp+1726,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[8U] 
                                   >> 0xcU)),20);
        bufp->chgBit(oldp+1727,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[8U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1728,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[8U] 
                                         >> 8U))),3);
        bufp->chgBit(oldp+1729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[8U] 
                                       >> 7U))));
        bufp->chgCData(oldp+1730,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[8U] 
                                         >> 4U))),3);
        bufp->chgBit(oldp+1731,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[8U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1732,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[8U])),3);
        bufp->chgBit(oldp+1733,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[7U] 
                                 >> 0x1fU)));
        bufp->chgIData(oldp+1734,((0x3ffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[7U] 
                                                 >> 5U))),26);
        bufp->chgCData(oldp+1735,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[7U])),5);
        bufp->chgIData(oldp+1736,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[6U] 
                                   >> 2U)),30);
        bufp->chgCData(oldp+1737,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[6U])),2);
        bufp->chgIData(oldp+1738,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[5U]),32);
        bufp->chgIData(oldp+1739,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[4U]),32);
        bufp->chgIData(oldp+1740,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[3U]),32);
        bufp->chgIData(oldp+1741,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[2U]),32);
        bufp->chgIData(oldp+1742,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[1U]),32);
        bufp->chgIData(oldp+1743,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0U] 
                                   >> 8U)),24);
        bufp->chgCData(oldp+1744,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0U] 
                                         >> 5U))),3);
        bufp->chgBit(oldp+1745,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0U] 
                                       >> 4U))));
        bufp->chgBit(oldp+1746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0U] 
                                       >> 3U))));
        bufp->chgBit(oldp+1747,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0U] 
                                       >> 2U))));
        bufp->chgBit(oldp+1748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0U] 
                                       >> 1U))));
        bufp->chgBit(oldp+1749,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__csrReg[0U])));
        bufp->chgCData(oldp+1750,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__interruptCtrl__DOT__interruptCodeConv),5);
        bufp->chgBit(oldp+1751,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__stall));
        bufp->chgBit(oldp+1752,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__beginStall));
        bufp->chgBit(oldp+1753,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__writePC_FromOuter));
        bufp->chgBit(oldp+1754,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__triggerInterrupt));
        bufp->chgCData(oldp+1755,(vlSymsp->TOP__SMT_RTL_Testbench__core__csrUnitIF.__PVT__interruptCode),5);
        bufp->chgBit(oldp+1756,(vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__pcWE));
        bufp->chgBit(oldp+1757,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__interruptAddrIn 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1758,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__interruptAddrIn)),19);
        bufp->chgBit(oldp+1759,(vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__interruptAddrWE));
        bufp->chgBit(oldp+1760,(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStageSendBubbleLowerForInterrupt));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x3dU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x84U])))) {
        bufp->chgBit(oldp+1761,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__predNextPC 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1762,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__predNextPC)),19);
        bufp->chgIData(oldp+1763,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__unnamedblk1__DOT__i),32);
        bufp->chgSData(oldp+1764,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRA[0]),10);
        bufp->chgSData(oldp+1765,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRA[1]),10);
        bufp->chgBit(oldp+1766,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__pcIn 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1767,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__pcIn)),19);
        bufp->chgBit(oldp+1768,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__nextTagReg 
                                               >> 0x13U)))));
        bufp->chgIData(oldp+1769,((0x7ffffU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__nextTagReg))),19);
        bufp->chgBit(oldp+1770,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__nextTagReg 
                                               >> 0x27U)))));
        bufp->chgIData(oldp+1771,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__nextTagReg 
                                                       >> 0x14U)))),19);
        bufp->chgBit(oldp+1772,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__predNextPC 
                                       >> 0x13U))));
        bufp->chgIData(oldp+1773,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__predNextPC)),19);
        bufp->chgSData(oldp+1774,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__ra[0]),10);
        bufp->chgSData(oldp+1775,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__ra[1]),10);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x3eU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x85U])))) {
        bufp->chgBit(oldp+1776,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Req
                                [0U]));
        bufp->chgCData(oldp+1777,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextCounter),5);
        bufp->chgSData(oldp+1778,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[5U])),10);
        bufp->chgSData(oldp+1779,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[4U] 
                                   >> 0x16U)),10);
        bufp->chgIData(oldp+1780,((0xffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[4U] 
                                                 << 2U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[3U] 
                                                   >> 0x1eU)))),24);
        bufp->chgIData(oldp+1781,((0xffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[3U] 
                                                >> 6U))),24);
        bufp->chgSData(oldp+1782,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[3U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                                >> 0x1cU)))),10);
        bufp->chgBit(oldp+1783,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+1784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+1785,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                       >> 0x19U))));
        bufp->chgBit(oldp+1786,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1787,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1788,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                       >> 0x16U))));
        bufp->chgBit(oldp+1789,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                       >> 0x15U))));
        bufp->chgIData(oldp+1790,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[2U] 
                                    << 0xbU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[1U] 
                                                >> 0x15U))),32);
        bufp->chgIData(oldp+1791,((0x7ffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[1U] 
                                                  << 6U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[0U] 
                                                    >> 0x1aU)))),27);
        bufp->chgIData(oldp+1792,((0x3ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData[0U])),26);
        bufp->chgIData(oldp+1793,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextResult),32);
        bufp->chgBit(oldp+1794,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__isDivSqrt[0]));
        bufp->chgBit(oldp+1795,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__Req[0]));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x3fU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x86U])))) {
        bufp->chgBit(oldp+1796,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__scStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1797,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__scStage))));
        bufp->chgBit(oldp+1798,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__isStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1799,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__isStage))));
        bufp->chgCData(oldp+1800,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x16U] 
                                         >> 6U))),2);
        bufp->chgSData(oldp+1801,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1802,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1803,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1804,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1805,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                           >> 7U))),4);
        bufp->chgBit(oldp+1806,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                       >> 6U))));
        bufp->chgIData(oldp+1807,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                                   << 0x18U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x10U] 
                                                     >> 8U)))),30);
        bufp->chgIData(oldp+1808,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x10U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                                  >> 0x16U)))),18);
        bufp->chgBit(oldp+1809,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                       >> 0xaU))));
        bufp->chgIData(oldp+1810,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                                << 9U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x10U] 
                                                  >> 0x17U)))),19);
        bufp->chgBit(oldp+1811,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x10U] 
                                       >> 0x16U))));
        bufp->chgSData(oldp+1812,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x10U] 
                                             >> 0xcU))),10);
        bufp->chgCData(oldp+1813,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x10U] 
                                         >> 0xaU))),2);
        bufp->chgIData(oldp+1814,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x10U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                                  >> 0x16U)))),20);
        bufp->chgCData(oldp+1815,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                         >> 0x13U))),3);
        bufp->chgCData(oldp+1816,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                         >> 0x10U))),3);
        bufp->chgCData(oldp+1817,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+1818,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+1819,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+1820,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                       >> 1U))));
        bufp->chgCData(oldp+1821,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xfU] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+1822,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                       >> 0x1aU))));
        bufp->chgCData(oldp+1823,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                            >> 0x14U))),6);
        bufp->chgBit(oldp+1824,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+1825,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                            >> 0xdU))),6);
        bufp->chgBit(oldp+1826,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+1827,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1828,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1829,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                       >> 4U))));
        bufp->chgIData(oldp+1830,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xeU] 
                                                << 0xfU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xdU] 
                                                  >> 0x11U)))),19);
        bufp->chgBit(oldp+1831,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xdU] 
                                       >> 0x10U))));
        bufp->chgSData(oldp+1832,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x16U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1833,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                         >> 0x1aU))),2);
        bufp->chgCData(oldp+1834,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                         >> 0x18U))),2);
        bufp->chgCData(oldp+1835,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                         >> 0x16U))),2);
        bufp->chgCData(oldp+1836,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1837,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+1838,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                                   << 0xdU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x14U] 
                                                     >> 0x13U)))),30);
        bufp->chgIData(oldp+1839,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x14U] 
                                               >> 1U))),18);
        bufp->chgBit(oldp+1840,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                       >> 0x15U))));
        bufp->chgIData(oldp+1841,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                               >> 2U))),19);
        bufp->chgBit(oldp+1842,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                       >> 1U))));
        bufp->chgSData(oldp+1843,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x15U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x14U] 
                                                >> 0x17U)))),10);
        bufp->chgCData(oldp+1844,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x14U] 
                                         >> 0x15U))),2);
        bufp->chgIData(oldp+1845,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x14U] 
                                               >> 1U))),20);
        bufp->chgCData(oldp+1846,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x14U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                          >> 0x1eU)))),3);
        bufp->chgCData(oldp+1847,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                         >> 0x1bU))),3);
        bufp->chgCData(oldp+1848,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                            >> 0x15U))),6);
        bufp->chgCData(oldp+1849,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                           >> 0x11U))),4);
        bufp->chgCData(oldp+1850,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                           >> 0xdU))),4);
        bufp->chgBit(oldp+1851,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                       >> 0xcU))));
        bufp->chgCData(oldp+1852,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                            >> 6U))),6);
        bufp->chgBit(oldp+1853,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                       >> 5U))));
        bufp->chgCData(oldp+1854,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x13U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                               >> 0x1fU)))),6);
        bufp->chgBit(oldp+1855,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+1856,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                            >> 0x18U))),6);
        bufp->chgBit(oldp+1857,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1858,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                       >> 0x16U))));
        bufp->chgCData(oldp+1859,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                            >> 0x10U))),6);
        bufp->chgBit(oldp+1860,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                       >> 0xfU))));
        bufp->chgIData(oldp+1861,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x12U] 
                                                << 4U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                                  >> 0x1cU)))),19);
        bufp->chgBit(oldp+1862,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0x11U] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+1863,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xdU] 
                                       >> 0xfU))));
        bufp->chgSData(oldp+1864,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xdU] 
                                             >> 5U))),10);
        bufp->chgCData(oldp+1865,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xdU] 
                                         >> 3U))),2);
        bufp->chgBit(oldp+1866,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xdU] 
                                       >> 2U))));
        bufp->chgCData(oldp+1867,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xdU])),2);
        bufp->chgCData(oldp+1868,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                   >> 0x1dU)),3);
        bufp->chgCData(oldp+1869,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                            >> 0x17U))),6);
        bufp->chgCData(oldp+1870,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                           >> 0x13U))),4);
        bufp->chgCData(oldp+1871,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                           >> 0xfU))),4);
        bufp->chgBit(oldp+1872,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                       >> 0xeU))));
        bufp->chgCData(oldp+1873,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                            >> 8U))),6);
        bufp->chgBit(oldp+1874,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                       >> 7U))));
        bufp->chgCData(oldp+1875,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU] 
                                            >> 1U))),6);
        bufp->chgBit(oldp+1876,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xcU])));
        bufp->chgCData(oldp+1877,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xbU] 
                                   >> 0x1aU)),6);
        bufp->chgBit(oldp+1878,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xbU] 
                                       >> 0x19U))));
        bufp->chgBit(oldp+1879,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xbU] 
                                       >> 0x18U))));
        bufp->chgCData(oldp+1880,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xbU] 
                                            >> 0x12U))),6);
        bufp->chgBit(oldp+1881,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xbU] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+1882,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xbU] 
                                                << 2U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                                  >> 0x1eU)))),19);
        bufp->chgBit(oldp+1883,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+1884,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                         >> 0x1bU))),2);
        bufp->chgSData(oldp+1885,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                             >> 0x14U))),10);
        bufp->chgCData(oldp+1886,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                         >> 0x12U))),2);
        bufp->chgCData(oldp+1887,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                         >> 0xfU))),3);
        bufp->chgCData(oldp+1888,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+1889,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                         >> 0xaU))),2);
        bufp->chgCData(oldp+1890,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                         >> 8U))),2);
        bufp->chgSData(oldp+1891,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                                >> 0x1cU)))),12);
        bufp->chgBit(oldp+1892,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+1893,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+1894,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                       >> 0x19U))));
        bufp->chgCData(oldp+1895,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+1896,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                            >> 0x12U))),5);
        bufp->chgBit(oldp+1897,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+1898,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1899,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                         >> 0xcU))),3);
        bufp->chgBit(oldp+1900,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1901,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                           >> 7U))),4);
        bufp->chgCData(oldp+1902,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                           >> 3U))),4);
        bufp->chgBit(oldp+1903,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                       >> 2U))));
        bufp->chgBit(oldp+1904,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                       >> 1U))));
        bufp->chgCData(oldp+1905,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[5U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                               >> 0x1bU)))),6);
        bufp->chgCData(oldp+1906,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                           >> 0x17U))),4);
        bufp->chgCData(oldp+1907,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                           >> 0x13U))),4);
        bufp->chgBit(oldp+1908,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                       >> 0x12U))));
        bufp->chgCData(oldp+1909,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                            >> 0xcU))),6);
        bufp->chgBit(oldp+1910,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+1911,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+1912,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                       >> 4U))));
        bufp->chgCData(oldp+1913,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+1914,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+1915,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U] 
                                       >> 0x1cU))));
        bufp->chgCData(oldp+1916,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U] 
                                            >> 0x16U))),6);
        bufp->chgBit(oldp+1917,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U] 
                                       >> 0x15U))));
        bufp->chgIData(oldp+1918,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U] 
                                               >> 2U))),19);
        bufp->chgBit(oldp+1919,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U] 
                                       >> 1U))));
        bufp->chgSData(oldp+1920,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1921,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1922,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+1923,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1924,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1925,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                         >> 5U))),2);
        bufp->chgSData(oldp+1926,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0xaU] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                                >> 0x19U)))),12);
        bufp->chgBit(oldp+1927,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+1928,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+1929,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                       >> 0x16U))));
        bufp->chgCData(oldp+1930,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                         >> 0x14U))),2);
        bufp->chgCData(oldp+1931,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                            >> 0xfU))),5);
        bufp->chgBit(oldp+1932,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                       >> 0xeU))));
        bufp->chgCData(oldp+1933,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                         >> 0xcU))),2);
        bufp->chgCData(oldp+1934,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                         >> 9U))),3);
        bufp->chgBit(oldp+1935,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                       >> 8U))));
        bufp->chgCData(oldp+1936,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+1937,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[9U])),4);
        bufp->chgBit(oldp+1938,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1939,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+1940,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                            >> 0x18U))),6);
        bufp->chgCData(oldp+1941,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                           >> 0x14U))),4);
        bufp->chgCData(oldp+1942,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                           >> 0x10U))),4);
        bufp->chgBit(oldp+1943,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+1944,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                            >> 9U))),6);
        bufp->chgBit(oldp+1945,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                       >> 8U))));
        bufp->chgCData(oldp+1946,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                            >> 2U))),6);
        bufp->chgBit(oldp+1947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                       >> 1U))));
        bufp->chgCData(oldp+1948,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[8U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[7U] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+1949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[7U] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+1950,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[7U] 
                                       >> 0x19U))));
        bufp->chgCData(oldp+1951,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[7U] 
                                            >> 0x13U))),6);
        bufp->chgBit(oldp+1952,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[7U] 
                                       >> 0x12U))));
        bufp->chgIData(oldp+1953,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[7U] 
                                                << 1U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                                  >> 0x1fU)))),19);
        bufp->chgBit(oldp+1954,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[6U] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+1955,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[3U])));
        bufp->chgSData(oldp+1956,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                   >> 0x16U)),10);
        bufp->chgCData(oldp+1957,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                         >> 0x14U))),2);
        bufp->chgCData(oldp+1958,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                         >> 0x11U))),3);
        bufp->chgCData(oldp+1959,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1960,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+1961,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+1962,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+1963,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                         >> 3U))),2);
        bufp->chgCData(oldp+1964,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[2U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                               >> 0x1dU)))),6);
        bufp->chgCData(oldp+1965,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                           >> 0x19U))),4);
        bufp->chgCData(oldp+1966,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                           >> 0x15U))),4);
        bufp->chgBit(oldp+1967,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                       >> 0x14U))));
        bufp->chgCData(oldp+1968,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                            >> 0xeU))),6);
        bufp->chgBit(oldp+1969,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                       >> 0xdU))));
        bufp->chgCData(oldp+1970,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1971,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U] 
                                       >> 6U))));
        bufp->chgCData(oldp+1972,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[1U])),6);
        bufp->chgBit(oldp+1973,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+1974,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+1975,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0U] 
                                            >> 0x18U))),6);
        bufp->chgBit(oldp+1976,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0U] 
                                       >> 0x17U))));
        bufp->chgIData(oldp+1977,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0U] 
                                               >> 4U))),19);
        bufp->chgBit(oldp+1978,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+1979,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData[0U])),3);
        bufp->chgBit(oldp+1980,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pushEntry));
        bufp->chgBit(oldp+1981,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__popEntry));
        bufp->chgBit(oldp+1982,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__almostFull));
        bufp->chgBit(oldp+1983,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryValidIn));
        bufp->chgBit(oldp+1984,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryValidOut));
        bufp->chgCData(oldp+1985,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__validInstCountNext),6);
        bufp->chgCData(oldp+1986,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextIntervalIn),3);
        bufp->chgCData(oldp+1987,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextIntervalCount),3);
        bufp->chgBit(oldp+1988,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushInt[0]));
        bufp->chgBit(oldp+1989,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushInt[1]));
        bufp->chgBit(oldp+1990,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushMem[0]));
        bufp->chgBit(oldp+1991,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushMem[1]));
        bufp->chgBit(oldp+1992,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushComplex[0]));
        bufp->chgBit(oldp+1993,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushFP[0]));
        bufp->chgCData(oldp+1994,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x16U] 
                                         >> 6U))),2);
        bufp->chgSData(oldp+1995,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+1996,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+1997,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                         >> 0xdU))),2);
        bufp->chgCData(oldp+1998,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                         >> 0xbU))),2);
        bufp->chgCData(oldp+1999,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                           >> 7U))),4);
        bufp->chgBit(oldp+2000,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                       >> 6U))));
        bufp->chgIData(oldp+2001,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                                   << 0x18U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
                                                     >> 8U)))),30);
        bufp->chgIData(oldp+2002,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                                  >> 0x16U)))),18);
        bufp->chgBit(oldp+2003,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                       >> 0xaU))));
        bufp->chgIData(oldp+2004,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                                << 9U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
                                                  >> 0x17U)))),19);
        bufp->chgBit(oldp+2005,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
                                       >> 0x16U))));
        bufp->chgSData(oldp+2006,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
                                             >> 0xcU))),10);
        bufp->chgCData(oldp+2007,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
                                         >> 0xaU))),2);
        bufp->chgIData(oldp+2008,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x10U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                                  >> 0x16U)))),20);
        bufp->chgCData(oldp+2009,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                         >> 0x13U))),3);
        bufp->chgCData(oldp+2010,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                         >> 0x10U))),3);
        bufp->chgCData(oldp+2011,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                            >> 0xaU))),6);
        bufp->chgCData(oldp+2012,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2013,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2014,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                       >> 1U))));
        bufp->chgCData(oldp+2015,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xfU] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+2016,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                       >> 0x1aU))));
        bufp->chgCData(oldp+2017,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                            >> 0x14U))),6);
        bufp->chgBit(oldp+2018,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                       >> 0x13U))));
        bufp->chgCData(oldp+2019,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                            >> 0xdU))),6);
        bufp->chgBit(oldp+2020,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                       >> 0xcU))));
        bufp->chgBit(oldp+2021,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+2022,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+2023,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                       >> 4U))));
        bufp->chgIData(oldp+2024,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xeU] 
                                                << 0xfU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
                                                  >> 0x11U)))),19);
        bufp->chgBit(oldp+2025,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
                                       >> 0x10U))));
        bufp->chgSData(oldp+2026,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x16U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+2027,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                         >> 0x1aU))),2);
        bufp->chgCData(oldp+2028,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                         >> 0x18U))),2);
        bufp->chgCData(oldp+2029,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                         >> 0x16U))),2);
        bufp->chgCData(oldp+2030,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2031,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+2032,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                                   << 0xdU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x14U] 
                                                     >> 0x13U)))),30);
        bufp->chgIData(oldp+2033,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x14U] 
                                               >> 1U))),18);
        bufp->chgBit(oldp+2034,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                       >> 0x15U))));
        bufp->chgIData(oldp+2035,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                               >> 2U))),19);
        bufp->chgBit(oldp+2036,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                       >> 1U))));
        bufp->chgSData(oldp+2037,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x15U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x14U] 
                                                >> 0x17U)))),10);
        bufp->chgCData(oldp+2038,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x14U] 
                                         >> 0x15U))),2);
        bufp->chgIData(oldp+2039,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x14U] 
                                               >> 1U))),20);
        bufp->chgCData(oldp+2040,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x14U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                          >> 0x1eU)))),3);
        bufp->chgCData(oldp+2041,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                         >> 0x1bU))),3);
        bufp->chgCData(oldp+2042,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                            >> 0x15U))),6);
        bufp->chgCData(oldp+2043,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                           >> 0x11U))),4);
        bufp->chgCData(oldp+2044,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                           >> 0xdU))),4);
        bufp->chgBit(oldp+2045,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                       >> 0xcU))));
        bufp->chgCData(oldp+2046,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                            >> 6U))),6);
        bufp->chgBit(oldp+2047,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                       >> 5U))));
        bufp->chgCData(oldp+2048,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x13U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                               >> 0x1fU)))),6);
        bufp->chgBit(oldp+2049,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+2050,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                            >> 0x18U))),6);
        bufp->chgBit(oldp+2051,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+2052,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                       >> 0x16U))));
        bufp->chgCData(oldp+2053,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                            >> 0x10U))),6);
        bufp->chgBit(oldp+2054,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                       >> 0xfU))));
        bufp->chgIData(oldp+2055,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x12U] 
                                                << 4U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                                  >> 0x1cU)))),19);
        bufp->chgBit(oldp+2056,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0x11U] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+2057,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
                                       >> 0xfU))));
        bufp->chgSData(oldp+2058,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
                                             >> 5U))),10);
        bufp->chgCData(oldp+2059,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
                                         >> 3U))),2);
        bufp->chgBit(oldp+2060,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xdU] 
                                       >> 2U))));
        bufp->chgCData(oldp+2061,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xdU])),2);
        bufp->chgCData(oldp+2062,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                   >> 0x1dU)),3);
        bufp->chgCData(oldp+2063,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                            >> 0x17U))),6);
        bufp->chgCData(oldp+2064,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                           >> 0x13U))),4);
        bufp->chgCData(oldp+2065,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                           >> 0xfU))),4);
        bufp->chgBit(oldp+2066,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                       >> 0xeU))));
        bufp->chgCData(oldp+2067,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                            >> 8U))),6);
        bufp->chgBit(oldp+2068,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                       >> 7U))));
        bufp->chgCData(oldp+2069,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU] 
                                            >> 1U))),6);
        bufp->chgBit(oldp+2070,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xcU])));
        bufp->chgCData(oldp+2071,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xbU] 
                                   >> 0x1aU)),6);
        bufp->chgBit(oldp+2072,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xbU] 
                                       >> 0x19U))));
        bufp->chgBit(oldp+2073,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xbU] 
                                       >> 0x18U))));
        bufp->chgCData(oldp+2074,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xbU] 
                                            >> 0x12U))),6);
        bufp->chgBit(oldp+2075,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xbU] 
                                       >> 0x11U))));
        bufp->chgIData(oldp+2076,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xbU] 
                                                << 2U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                                  >> 0x1eU)))),19);
        bufp->chgBit(oldp+2077,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                       >> 0x1dU))));
        bufp->chgCData(oldp+2078,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                         >> 0x1bU))),2);
        bufp->chgSData(oldp+2079,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                             >> 0x14U))),10);
        bufp->chgCData(oldp+2080,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                         >> 0x12U))),2);
        bufp->chgCData(oldp+2081,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                         >> 0xfU))),3);
        bufp->chgCData(oldp+2082,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+2083,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                         >> 0xaU))),2);
        bufp->chgCData(oldp+2084,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                         >> 8U))),2);
        bufp->chgSData(oldp+2085,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                                >> 0x1cU)))),12);
        bufp->chgBit(oldp+2086,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                       >> 0x1bU))));
        bufp->chgBit(oldp+2087,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+2088,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                       >> 0x19U))));
        bufp->chgCData(oldp+2089,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                         >> 0x17U))),2);
        bufp->chgCData(oldp+2090,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                            >> 0x12U))),5);
        bufp->chgBit(oldp+2091,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                       >> 0x11U))));
        bufp->chgCData(oldp+2092,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+2093,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                         >> 0xcU))),3);
        bufp->chgBit(oldp+2094,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+2095,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                           >> 7U))),4);
        bufp->chgCData(oldp+2096,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                           >> 3U))),4);
        bufp->chgBit(oldp+2097,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                       >> 2U))));
        bufp->chgBit(oldp+2098,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                       >> 1U))));
        bufp->chgCData(oldp+2099,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[5U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                               >> 0x1bU)))),6);
        bufp->chgCData(oldp+2100,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                           >> 0x17U))),4);
        bufp->chgCData(oldp+2101,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                           >> 0x13U))),4);
        bufp->chgBit(oldp+2102,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                       >> 0x12U))));
        bufp->chgCData(oldp+2103,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                            >> 0xcU))),6);
        bufp->chgBit(oldp+2104,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                       >> 0xbU))));
        bufp->chgCData(oldp+2105,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                            >> 5U))),6);
        bufp->chgBit(oldp+2106,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                       >> 4U))));
        bufp->chgCData(oldp+2107,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[4U] 
                                             << 2U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
                                               >> 0x1eU)))),6);
        bufp->chgBit(oldp+2108,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
                                       >> 0x1dU))));
        bufp->chgBit(oldp+2109,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
                                       >> 0x1cU))));
        bufp->chgCData(oldp+2110,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
                                            >> 0x16U))),6);
        bufp->chgBit(oldp+2111,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
                                       >> 0x15U))));
        bufp->chgIData(oldp+2112,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
                                               >> 2U))),19);
        bufp->chgBit(oldp+2113,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U] 
                                       >> 1U))));
        bufp->chgSData(oldp+2114,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                             >> 0x11U))),10);
        bufp->chgCData(oldp+2115,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                         >> 0xfU))),2);
        bufp->chgCData(oldp+2116,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                         >> 0xcU))),3);
        bufp->chgCData(oldp+2117,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+2118,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+2119,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                         >> 5U))),2);
        bufp->chgSData(oldp+2120,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0xaU] 
                                              << 7U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                                >> 0x19U)))),12);
        bufp->chgBit(oldp+2121,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                       >> 0x18U))));
        bufp->chgBit(oldp+2122,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                       >> 0x17U))));
        bufp->chgBit(oldp+2123,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                       >> 0x16U))));
        bufp->chgCData(oldp+2124,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                         >> 0x14U))),2);
        bufp->chgCData(oldp+2125,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                            >> 0xfU))),5);
        bufp->chgBit(oldp+2126,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                       >> 0xeU))));
        bufp->chgCData(oldp+2127,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                         >> 0xcU))),2);
        bufp->chgCData(oldp+2128,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                         >> 9U))),3);
        bufp->chgBit(oldp+2129,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                       >> 8U))));
        bufp->chgCData(oldp+2130,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U] 
                                           >> 4U))),4);
        bufp->chgCData(oldp+2131,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[9U])),4);
        bufp->chgBit(oldp+2132,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+2133,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+2134,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                            >> 0x18U))),6);
        bufp->chgCData(oldp+2135,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                           >> 0x14U))),4);
        bufp->chgCData(oldp+2136,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                           >> 0x10U))),4);
        bufp->chgBit(oldp+2137,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                       >> 0xfU))));
        bufp->chgCData(oldp+2138,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                            >> 9U))),6);
        bufp->chgBit(oldp+2139,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                       >> 8U))));
        bufp->chgCData(oldp+2140,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                            >> 2U))),6);
        bufp->chgBit(oldp+2141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                       >> 1U))));
        bufp->chgCData(oldp+2142,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[8U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[7U] 
                                               >> 0x1bU)))),6);
        bufp->chgBit(oldp+2143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[7U] 
                                       >> 0x1aU))));
        bufp->chgBit(oldp+2144,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[7U] 
                                       >> 0x19U))));
        bufp->chgCData(oldp+2145,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[7U] 
                                            >> 0x13U))),6);
        bufp->chgBit(oldp+2146,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[7U] 
                                       >> 0x12U))));
        bufp->chgIData(oldp+2147,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[7U] 
                                                << 1U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                                  >> 0x1fU)))),19);
        bufp->chgBit(oldp+2148,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[6U] 
                                       >> 0x1eU))));
        bufp->chgBit(oldp+2149,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[3U])));
        bufp->chgSData(oldp+2150,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                   >> 0x16U)),10);
        bufp->chgCData(oldp+2151,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                         >> 0x14U))),2);
        bufp->chgCData(oldp+2152,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                         >> 0x11U))),3);
        bufp->chgCData(oldp+2153,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+2154,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                         >> 9U))),3);
        bufp->chgCData(oldp+2155,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                         >> 7U))),2);
        bufp->chgCData(oldp+2156,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                         >> 5U))),2);
        bufp->chgCData(oldp+2157,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                         >> 3U))),2);
        bufp->chgCData(oldp+2158,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[2U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                               >> 0x1dU)))),6);
        bufp->chgCData(oldp+2159,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                           >> 0x19U))),4);
        bufp->chgCData(oldp+2160,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                           >> 0x15U))),4);
        bufp->chgBit(oldp+2161,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                       >> 0x14U))));
        bufp->chgCData(oldp+2162,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                            >> 0xeU))),6);
        bufp->chgBit(oldp+2163,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                       >> 0xdU))));
        bufp->chgCData(oldp+2164,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+2165,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U] 
                                       >> 6U))));
        bufp->chgCData(oldp+2166,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[1U])),6);
        bufp->chgBit(oldp+2167,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
                                 >> 0x1fU)));
        bufp->chgBit(oldp+2168,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
                                       >> 0x1eU))));
        bufp->chgCData(oldp+2169,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
                                            >> 0x18U))),6);
        bufp->chgBit(oldp+2170,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
                                       >> 0x17U))));
        bufp->chgIData(oldp+2171,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
                                               >> 4U))),19);
        bufp->chgBit(oldp+2172,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0U] 
                                       >> 3U))));
        bufp->chgCData(oldp+2173,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplayEntry[0U])),3);
        bufp->chgBit(oldp+2174,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__nextReplay));
        bufp->chgCData(oldp+2175,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__nextHeadStorage),5);
        bufp->chgCData(oldp+2176,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__nextTailStorage),5);
        bufp->chgCData(oldp+2177,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__nextCount),6);
        bufp->chgWData(oldp+2178,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__recordData),712);
        bufp->chgIData(oldp+2201,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk10__DOT__i),32);
        bufp->chgIData(oldp+2202,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk11__DOT__i),32);
        bufp->chgIData(oldp+2203,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk12__DOT__i),32);
        bufp->chgIData(oldp+2204,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk13__DOT__i),32);
        bufp->chgIData(oldp+2205,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk14__DOT__i),32);
        bufp->chgIData(oldp+2206,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk15__DOT__i),32);
        bufp->chgIData(oldp+2207,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk16__DOT__i),32);
        bufp->chgIData(oldp+2208,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk17__DOT__i),32);
        bufp->chgIData(oldp+2209,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk18__DOT__i),32);
        bufp->chgIData(oldp+2210,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk19__DOT__i),32);
        bufp->chgIData(oldp+2211,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk20__DOT__i),32);
        bufp->chgIData(oldp+2212,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk21__DOT__i),32);
        bufp->chgIData(oldp+2213,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk22__DOT__i),32);
        bufp->chgIData(oldp+2214,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk23__DOT__i),32);
        bufp->chgIData(oldp+2215,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk24__DOT__i),32);
        bufp->chgIData(oldp+2216,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk25__DOT__i),32);
        bufp->chgIData(oldp+2217,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk26__DOT__i),32);
        bufp->chgIData(oldp+2218,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk27__DOT__i),32);
        bufp->chgIData(oldp+2219,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk28__DOT__i),32);
        bufp->chgIData(oldp+2220,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk29__DOT__i),32);
        bufp->chgIData(oldp+2221,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk30__DOT__i),32);
        bufp->chgIData(oldp+2222,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk31__DOT__i),32);
        bufp->chgIData(oldp+2223,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk32__DOT__i),32);
        bufp->chgIData(oldp+2224,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk33__DOT__i),32);
        bufp->chgIData(oldp+2225,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk34__DOT__i),32);
        bufp->chgIData(oldp+2226,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk35__DOT__i),32);
        bufp->chgIData(oldp+2227,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk36__DOT__i),32);
        bufp->chgIData(oldp+2228,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk37__DOT__i),32);
        bufp->chgIData(oldp+2229,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk38__DOT__i),32);
        bufp->chgIData(oldp+2230,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk39__DOT__i),32);
        bufp->chgIData(oldp+2231,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+2232,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+2233,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk7__DOT__i),32);
        bufp->chgIData(oldp+2234,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk8__DOT__i),32);
        bufp->chgIData(oldp+2235,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__unnamedblk9__DOT__i),32);
        bufp->chgBit(oldp+2236,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayEntry[0]));
        bufp->chgBit(oldp+2237,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayEntry[1]));
        bufp->chgSData(oldp+2238,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                             [0U][4U] 
                                             >> 1U))),10);
        bufp->chgCData(oldp+2239,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                          [0U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+2240,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+2241,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+2242,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                           [0U][3U] 
                                           >> 0x17U))),4);
        bufp->chgBit(oldp+2243,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+2244,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                                   [0U][3U] 
                                                   << 8U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                                     [0U][2U] 
                                                     >> 0x18U)))),30);
        bufp->chgIData(oldp+2245,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [0U][2U] 
                                               >> 6U))),18);
        bufp->chgBit(oldp+2246,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+2247,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [0U][3U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+2248,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][3U] >> 6U))));
        bufp->chgSData(oldp+2249,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                              [0U][3U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                                [0U][2U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+2250,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+2251,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [0U][2U] 
                                               >> 6U))),20);
        bufp->chgCData(oldp+2252,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+2253,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                   [0U][2U])),3);
        bufp->chgCData(oldp+2254,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2255,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2256,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2257,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2258,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2260,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2262,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2264,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2265,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2266,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2267,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2268,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                 [0U][0U])));
        bufp->chgSData(oldp+2269,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                             [1U][4U] 
                                             >> 1U))),10);
        bufp->chgCData(oldp+2270,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                          [1U][4U] 
                                          << 1U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                          [1U][3U] 
                                          >> 0x1fU)))),2);
        bufp->chgCData(oldp+2271,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+2272,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+2273,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                           [1U][3U] 
                                           >> 0x17U))),4);
        bufp->chgBit(oldp+2274,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+2275,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                                   [1U][3U] 
                                                   << 8U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                                     [1U][2U] 
                                                     >> 0x18U)))),30);
        bufp->chgIData(oldp+2276,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [1U][2U] 
                                               >> 6U))),18);
        bufp->chgBit(oldp+2277,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+2278,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [1U][3U] 
                                               >> 7U))),19);
        bufp->chgBit(oldp+2279,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][3U] >> 6U))));
        bufp->chgSData(oldp+2280,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                              [1U][3U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                                [1U][2U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+2281,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+2282,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [1U][2U] 
                                               >> 6U))),20);
        bufp->chgCData(oldp+2283,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                         [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+2284,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                   [1U][2U])),3);
        bufp->chgCData(oldp+2285,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2286,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2287,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2288,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2289,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2290,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2291,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2292,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+2293,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2294,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2295,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2296,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                            [1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2297,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2298,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2299,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intReplayData
                                 [1U][0U])));
        bufp->chgBit(oldp+2300,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayEntry[0]));
        bufp->chgSData(oldp+2301,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                             [0U][2U] 
                                             >> 8U))),10);
        bufp->chgCData(oldp+2302,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                         [0U][2U] >> 6U))),2);
        bufp->chgBit(oldp+2303,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                       [0U][2U] >> 5U))));
        bufp->chgCData(oldp+2304,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                         [0U][2U] >> 3U))),2);
        bufp->chgCData(oldp+2305,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                   [0U][2U])),3);
        bufp->chgCData(oldp+2306,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2307,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2308,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2309,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2310,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2311,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2312,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2314,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2315,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2316,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2317,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2318,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2319,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2320,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexReplayData
                                 [0U][0U])));
        bufp->chgBit(oldp+2321,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayEntry[0]));
        bufp->chgSData(oldp+2322,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                             [0U][2U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2323,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                         [0U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+2324,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                         [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+2325,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                            [0U][2U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+2326,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                         [0U][2U] >> 6U))),3);
        bufp->chgCData(oldp+2327,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                         [0U][2U] >> 4U))),2);
        bufp->chgCData(oldp+2328,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                         [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+2329,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                   [0U][2U])),2);
        bufp->chgCData(oldp+2330,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2331,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2332,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2333,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2334,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2335,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2336,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2337,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2338,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2339,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2340,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2341,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2342,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2343,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2344,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpReplayData
                                 [0U][0U])));
        bufp->chgBit(oldp+2345,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayEntry[0]));
        bufp->chgBit(oldp+2346,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayEntry[1]));
        bufp->chgSData(oldp+2347,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2348,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2349,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2350,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2351,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2352,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2353,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2354,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2355,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2357,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2358,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2359,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2360,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2361,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2362,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2363,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2364,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2365,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+2366,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                 [0U][2U])));
        bufp->chgCData(oldp+2367,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2368,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2369,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2370,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2371,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2372,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2373,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2374,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2375,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2376,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2377,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2378,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2379,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2380,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2381,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                 [0U][0U])));
        bufp->chgSData(oldp+2382,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                             [1U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+2383,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+2384,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+2385,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+2386,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][3U] >> 9U))),2);
        bufp->chgCData(oldp+2387,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][3U] >> 7U))),2);
        bufp->chgSData(oldp+2388,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                              [1U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                                [1U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+2389,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+2390,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+2391,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][2U] >> 0x18U))));
        bufp->chgCData(oldp+2392,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+2393,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [1U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+2394,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][2U] >> 0x10U))));
        bufp->chgCData(oldp+2395,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+2396,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                         [1U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+2397,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][2U] >> 0xaU))));
        bufp->chgCData(oldp+2398,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [1U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+2399,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [1U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+2400,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][2U] >> 1U))));
        bufp->chgBit(oldp+2401,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                 [1U][2U])));
        bufp->chgCData(oldp+2402,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2403,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2404,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2405,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2406,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2408,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2409,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+2410,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2411,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2412,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2413,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                            [1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2414,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2415,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2416,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memReplayData
                                 [1U][0U])));
        bufp->chgBit(oldp+2417,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__replay));
        bufp->chgBit(oldp+2418,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__scStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+2419,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__scStage))));
        bufp->chgBit(oldp+2420,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+2421,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStage))));
        bufp->chgBit(oldp+2422,(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__isStageStallUpper));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x40U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x87U])))) {
        bufp->chgBit(oldp+2423,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__isFlushed[0]));
        bufp->chgBit(oldp+2424,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__isFlushed[1]));
        bufp->chgBit(oldp+2425,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2426,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                                       [0U] 
                                                       >> 0xdU)))),19);
        bufp->chgBit(oldp+2427,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                               [0U] 
                                               >> 0xcU)))));
        bufp->chgSData(oldp+2428,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                                     [0U] 
                                                     >> 2U)))),10);
        bufp->chgCData(oldp+2429,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                                [0U]))),2);
        bufp->chgBit(oldp+2430,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2431,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                                       [1U] 
                                                       >> 0xdU)))),19);
        bufp->chgBit(oldp+2432,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                               [1U] 
                                               >> 0xcU)))));
        bufp->chgSData(oldp+2433,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                                     [1U] 
                                                     >> 2U)))),10);
        bufp->chgCData(oldp+2434,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__brPred
                                                [1U]))),2);
        bufp->chgIData(oldp+2435,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+2436,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+2437,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j),32);
        bufp->chgIData(oldp+2438,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk8__DOT__i),32);
        bufp->chgBit(oldp+2439,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__stall));
        bufp->chgBit(oldp+2440,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__clear));
        bufp->chgBit(oldp+2441,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__pcIn 
                                       >> 0x13U))));
        bufp->chgIData(oldp+2442,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__pcIn)),19);
        bufp->chgBit(oldp+2443,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__brPredTaken[0]));
        bufp->chgBit(oldp+2444,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__brPredTaken[1]));
        bufp->chgBit(oldp+2445,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__updateHistory[0]));
        bufp->chgBit(oldp+2446,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__updateHistory[1]));
        bufp->chgBit(oldp+2447,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtWE[0]));
        bufp->chgBit(oldp+2448,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtWE[1]));
        bufp->chgSData(oldp+2449,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtWA[0]),11);
        bufp->chgSData(oldp+2450,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtWA[1]),11);
        bufp->chgCData(oldp+2451,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtWV[0]),2);
        bufp->chgCData(oldp+2452,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtWV[1]),2);
        bufp->chgCData(oldp+2453,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtPrevValue[0]),2);
        bufp->chgCData(oldp+2454,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtPrevValue[1]),2);
        bufp->chgSData(oldp+2455,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtRA[0]),11);
        bufp->chgSData(oldp+2456,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtRA[1]),11);
        bufp->chgCData(oldp+2457,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtRV[0]),2);
        bufp->chgCData(oldp+2458,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtRV[1]),2);
        bufp->chgSData(oldp+2459,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__nextBrGlobalHistory),10);
        bufp->chgSData(oldp+2460,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__brGlobalHistory[0]),10);
        bufp->chgSData(oldp+2461,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__brGlobalHistory[1]),10);
        bufp->chgBit(oldp+2462,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__mispred));
        bufp->chgBit(oldp+2463,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__pushPhtQueue));
        bufp->chgBit(oldp+2464,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__popPhtQueue));
        bufp->chgBit(oldp+2465,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__updatePht));
        bufp->chgCData(oldp+2466,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__nextHeadStorage),5);
        bufp->chgCData(oldp+2467,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__nextTailStorage),5);
        bufp->chgCData(oldp+2468,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__nextCount),6);
        bufp->chgIData(oldp+2469,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__unnamedblk1__DOT__i),32);
        bufp->chgIData(oldp+2470,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2471,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2472,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+2473,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+2474,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+2475,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__unnamedblk7__DOT__i),32);
        bufp->chgBit(oldp+2476,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__updateBrHistory[0]));
        bufp->chgBit(oldp+2477,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__updateBrHistory[1]));
        bufp->chgBit(oldp+2478,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken[0]));
        bufp->chgBit(oldp+2479,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brPredTaken[1]));
        bufp->chgSData(oldp+2480,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory[0]),10);
        bufp->chgSData(oldp+2481,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__brGlobalHistory[1]),10);
        bufp->chgCData(oldp+2482,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue[0]),2);
        bufp->chgCData(oldp+2483,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__phtPrevValue[1]),2);
        bufp->chgBit(oldp+2484,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__we[0]));
        bufp->chgBit(oldp+2485,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__we[1]));
        bufp->chgSData(oldp+2486,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wa[0]),11);
        bufp->chgSData(oldp+2487,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wa[1]),11);
        bufp->chgCData(oldp+2488,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wv[0]),2);
        bufp->chgCData(oldp+2489,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__wv[1]),2);
        bufp->chgSData(oldp+2490,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__ra[0]),11);
        bufp->chgSData(oldp+2491,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__ra[1]),11);
        bufp->chgCData(oldp+2492,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__rv[0]),2);
        bufp->chgCData(oldp+2493,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__rv[1]),2);
        bufp->chgSData(oldp+2494,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),11);
        bufp->chgSData(oldp+2495,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),11);
        bufp->chgSData(oldp+2496,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),11);
        bufp->chgSData(oldp+2497,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),11);
        bufp->chgCData(oldp+2498,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]),2);
        bufp->chgCData(oldp+2499,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]),2);
        bufp->chgBit(oldp+2500,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+2501,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+2502,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [0U]));
        bufp->chgSData(oldp+2503,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__waBank
                                             [0U] >> 1U))),10);
        bufp->chgCData(oldp+2504,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [0U]),2);
        bufp->chgSData(oldp+2505,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__raBank
                                             [0U] >> 1U))),10);
        bufp->chgBit(oldp+2506,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__weBank
                                [1U]));
        bufp->chgSData(oldp+2507,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__waBank
                                             [1U] >> 1U))),10);
        bufp->chgCData(oldp+2508,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                  [1U]),2);
        bufp->chgSData(oldp+2509,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__raBank
                                             [1U] >> 1U))),10);
        bufp->chgIData(oldp+2510,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__b),32);
        bufp->chgIData(oldp+2511,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2512,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__b),32);
        bufp->chgIData(oldp+2513,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+2514,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+2515,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor__pht.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x41U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x88U])))) {
        bufp->chgSData(oldp+2516,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextSID),10);
        bufp->chgSData(oldp+2517,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                             [0U] >> 0x15U))),10);
        bufp->chgBit(oldp+2518,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                       [0U] >> 0x14U))));
        bufp->chgBit(oldp+2519,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                       [0U] >> 0x13U))));
        bufp->chgIData(oldp+2520,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                   [0U])),19);
        bufp->chgSData(oldp+2521,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                             [1U] >> 0x15U))),10);
        bufp->chgBit(oldp+2522,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                       [1U] >> 0x14U))));
        bufp->chgBit(oldp+2523,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                       [1U] >> 0x13U))));
        bufp->chgIData(oldp+2524,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__nextStage
                                   [1U])),19);
        bufp->chgIData(oldp+2525,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__fetchAddr),32);
        bufp->chgCData(oldp+2526,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__numValidInsns),3);
        bufp->chgIData(oldp+2527,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+2528,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__unnamedblk3__DOT__t),32);
        bufp->chgIData(oldp+2529,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+2530,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+2531,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__unnamedblk6__DOT__i),32);
        bufp->chgBit(oldp+2532,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
                                       [0U] >> 0xaU))));
        bufp->chgSData(oldp+2533,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
                                   [0U])),10);
    }
}
