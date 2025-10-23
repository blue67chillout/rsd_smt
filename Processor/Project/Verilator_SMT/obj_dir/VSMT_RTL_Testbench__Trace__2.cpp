// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "VSMT_RTL_Testbench__Syms.h"


void VSMT_RTL_Testbench___024root__trace_chg_0_sub_2(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root__trace_chg_0_sub_2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode + 8136);
    // Body
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x12U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x5aU])))) {
        bufp->chgCData(oldp+0,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__releasedStoreQueuePtr),4);
        bufp->chgCData(oldp+1,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadPtr[0]),4);
        bufp->chgCData(oldp+2,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadPtr[1]),4);
        bufp->chgBit(oldp+3,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                            [0U] >> 0x25U)))));
        bufp->chgIData(oldp+4,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                        [0U] >> 5U))),32);
        bufp->chgBit(oldp+5,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                            [0U] >> 4U)))));
        bufp->chgCData(oldp+6,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                               [0U]))),4);
        bufp->chgBit(oldp+7,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                            [1U] >> 0x25U)))));
        bufp->chgIData(oldp+8,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                        [1U] >> 5U))),32);
        bufp->chgBit(oldp+9,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                            [1U] >> 4U)))));
        bufp->chgCData(oldp+10,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__sqReadData
                                                [1U]))),4);
        bufp->chgIData(oldp+11,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreData),32);
        bufp->chgBit(oldp+12,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreCondEnabled));
        bufp->chgBit(oldp+13,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreWordWE));
        bufp->chgCData(oldp+14,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreByteWE),4);
        bufp->chgIData(oldp+15,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__retiredStoreLSQ_BlockAddr),20);
        bufp->chgCData(oldp+16,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__ra[0]),4);
        bufp->chgCData(oldp+17,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__ra[1]),4);
        bufp->chgQData(oldp+18,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__rv[0]),38);
        bufp->chgQData(oldp+20,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData.__PVT__rv[1]),38);
        bufp->chgCData(oldp+22,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__ra[0]),4);
        bufp->chgCData(oldp+23,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__ra[1]),4);
        bufp->chgQData(oldp+24,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__rv[0]),38);
        bufp->chgQData(oldp+26,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__rv[1]),38);
        bufp->chgCData(oldp+28,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__ra
                                [0U]),4);
        bufp->chgCData(oldp+29,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body.__PVT__ra
                                [1U]),4);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x13U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x5fU])))) {
        bufp->chgBit(oldp+30,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                     [0U] >> 0x13U))));
        bufp->chgBit(oldp+31,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                     [0U] >> 0x12U))));
        bufp->chgCData(oldp+32,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                         [0U] >> 0xeU))),4);
        bufp->chgSData(oldp+33,((0x1fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                            [0U] >> 1U))),13);
        bufp->chgBit(oldp+34,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                               [0U])));
        bufp->chgBit(oldp+35,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                     [1U] >> 0x13U))));
        bufp->chgBit(oldp+36,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                     [1U] >> 0x12U))));
        bufp->chgCData(oldp+37,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                         [1U] >> 0xeU))),4);
        bufp->chgSData(oldp+38,((0x1fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                                            [1U] >> 1U))),13);
        bufp->chgBit(oldp+39,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbRV
                               [1U])));
        bufp->chgBit(oldp+40,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbOut
                                     [0U] >> 0x13U))));
        bufp->chgIData(oldp+41,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbOut
                                 [0U])),19);
        bufp->chgBit(oldp+42,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbOut
                                     [1U] >> 0x13U))));
        bufp->chgIData(oldp+43,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbOut
                                 [1U])),19);
        bufp->chgBit(oldp+44,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbHit[0]));
        bufp->chgBit(oldp+45,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbHit[1]));
        bufp->chgBit(oldp+46,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__readIsCondBr[0]));
        bufp->chgBit(oldp+47,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__readIsCondBr[1]));
        bufp->chgBit(oldp+48,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut
                                     [0U] >> 0x13U))));
        bufp->chgIData(oldp+49,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut
                                 [0U])),19);
        bufp->chgBit(oldp+50,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut
                                     [1U] >> 0x13U))));
        bufp->chgIData(oldp+51,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbOut
                                 [1U])),19);
        bufp->chgBit(oldp+52,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit[0]));
        bufp->chgBit(oldp+53,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__btbHit[1]));
        bufp->chgBit(oldp+54,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__readIsCondBr[0]));
        bufp->chgBit(oldp+55,(vlSymsp->TOP__SMT_RTL_Testbench__core__ifStageIF.__PVT__readIsCondBr[1]));
        bufp->chgIData(oldp+56,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__rv[0]),20);
        bufp->chgIData(oldp+57,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__rv[1]),20);
        bufp->chgSData(oldp+58,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),10);
        bufp->chgSData(oldp+59,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),10);
        bufp->chgSData(oldp+60,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),10);
        bufp->chgSData(oldp+61,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),10);
        bufp->chgIData(oldp+62,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]),20);
        bufp->chgIData(oldp+63,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]),20);
        bufp->chgBit(oldp+64,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+65,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+66,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__weBank
                              [0U]));
        bufp->chgSData(oldp+67,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__waBank
                                           [0U] >> 1U))),9);
        bufp->chgIData(oldp+68,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                [0U]),20);
        bufp->chgSData(oldp+69,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__raBank
                                           [0U] >> 1U))),9);
        bufp->chgBit(oldp+70,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__weBank
                              [1U]));
        bufp->chgSData(oldp+71,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__waBank
                                           [1U] >> 1U))),9);
        bufp->chgIData(oldp+72,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                [1U]),20);
        bufp->chgSData(oldp+73,((0x1ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__raBank
                                           [1U] >> 1U))),9);
        bufp->chgIData(oldp+74,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__b),32);
        bufp->chgIData(oldp+75,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+76,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__b),32);
        bufp->chgIData(oldp+77,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+78,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__i),32);
        bufp->chgIData(oldp+79,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb__btbEntryArray.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x14U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x60U])))) {
        bufp->chgCData(oldp+80,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__rv[0]),7);
        bufp->chgCData(oldp+81,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__rv[1]),7);
        bufp->chgCData(oldp+82,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__rv[0]),7);
        bufp->chgCData(oldp+83,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__rv[1]),7);
        bufp->chgCData(oldp+84,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),5);
        bufp->chgCData(oldp+85,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),5);
        bufp->chgCData(oldp+86,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),5);
        bufp->chgCData(oldp+87,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),5);
        bufp->chgCData(oldp+88,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]),7);
        bufp->chgCData(oldp+89,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]),7);
        bufp->chgBit(oldp+90,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+91,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+92,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                              [0U]));
        bufp->chgCData(oldp+93,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                         [0U] >> 1U))),4);
        bufp->chgCData(oldp+94,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                [0U]),7);
        bufp->chgCData(oldp+95,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                         [0U] >> 1U))),4);
        bufp->chgBit(oldp+96,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                              [1U]));
        bufp->chgCData(oldp+97,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                         [1U] >> 1U))),4);
        bufp->chgCData(oldp+98,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                [1U]),7);
        bufp->chgCData(oldp+99,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                         [1U] >> 1U))),4);
        bufp->chgIData(oldp+100,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b),32);
        bufp->chgIData(oldp+101,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+102,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b),32);
        bufp->chgIData(oldp+103,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+104,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+105,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x15U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x61U])))) {
        bufp->chgCData(oldp+106,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__rv[0]),7);
        bufp->chgCData(oldp+107,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList.__PVT__rv[1]),7);
        bufp->chgCData(oldp+108,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__rv[0]),7);
        bufp->chgCData(oldp+109,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__rv[1]),7);
        bufp->chgCData(oldp+110,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),5);
        bufp->chgCData(oldp+111,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),5);
        bufp->chgCData(oldp+112,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),5);
        bufp->chgCData(oldp+113,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),5);
        bufp->chgCData(oldp+114,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]),7);
        bufp->chgCData(oldp+115,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]),7);
        bufp->chgBit(oldp+116,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+117,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+118,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                               [0U]));
        bufp->chgCData(oldp+119,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                          [0U] >> 1U))),4);
        bufp->chgCData(oldp+120,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                 [0U]),7);
        bufp->chgCData(oldp+121,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                          [0U] >> 1U))),4);
        bufp->chgBit(oldp+122,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                               [1U]));
        bufp->chgCData(oldp+123,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                          [1U] >> 1U))),4);
        bufp->chgCData(oldp+124,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                 [1U]),7);
        bufp->chgCData(oldp+125,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                          [1U] >> 1U))),4);
        bufp->chgIData(oldp+126,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b),32);
        bufp->chgIData(oldp+127,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+128,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b),32);
        bufp->chgIData(oldp+129,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+130,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+131,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x16U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x62U])))) {
        bufp->chgCData(oldp+132,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__rv[0]),7);
        bufp->chgCData(oldp+133,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList.__PVT__rv[1]),7);
        bufp->chgCData(oldp+134,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__rv[0]),7);
        bufp->chgCData(oldp+135,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__rv[1]),7);
        bufp->chgCData(oldp+136,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[0]),5);
        bufp->chgCData(oldp+137,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank[1]),5);
        bufp->chgCData(oldp+138,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[0]),5);
        bufp->chgCData(oldp+139,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank[1]),5);
        bufp->chgCData(oldp+140,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[0]),7);
        bufp->chgCData(oldp+141,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank[1]),7);
        bufp->chgBit(oldp+142,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[0]));
        bufp->chgBit(oldp+143,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank[1]));
        bufp->chgBit(oldp+144,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                               [0U]));
        bufp->chgCData(oldp+145,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                          [0U] >> 1U))),4);
        bufp->chgCData(oldp+146,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                 [0U]),7);
        bufp->chgCData(oldp+147,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                          [0U] >> 1U))),4);
        bufp->chgBit(oldp+148,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__weBank
                               [1U]));
        bufp->chgCData(oldp+149,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__waBank
                                          [1U] >> 1U))),4);
        bufp->chgCData(oldp+150,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__wvBank
                                 [1U]),7);
        bufp->chgCData(oldp+151,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__raBank
                                          [1U] >> 1U))),4);
        bufp->chgIData(oldp+152,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b),32);
        bufp->chgIData(oldp+153,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i),32);
        bufp->chgIData(oldp+154,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b),32);
        bufp->chgIData(oldp+155,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i),32);
        bufp->chgIData(oldp+156,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i),32);
        bufp->chgIData(oldp+157,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b),32);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x17U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x5cU])))) {
        bufp->chgBit(oldp+158,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__stall));
        bufp->chgBit(oldp+159,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__clear));
        bufp->chgBit(oldp+160,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__flush[0]));
        bufp->chgSData(oldp+161,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                            [0U][2U] 
                                            >> 8U))),10);
        bufp->chgCData(oldp+162,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                        [0U][2U] >> 6U))),2);
        bufp->chgBit(oldp+163,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                      [0U][2U] >> 5U))));
        bufp->chgCData(oldp+164,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                        [0U][2U] >> 3U))),2);
        bufp->chgCData(oldp+165,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                  [0U][2U])),3);
        bufp->chgCData(oldp+166,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+167,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+168,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+169,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+170,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+171,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+172,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+173,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+174,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+175,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+176,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+177,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+178,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+179,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+180,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__iqData
                                [0U][0U])));
        bufp->chgBit(oldp+181,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__mulOpInfo
                                      [0U] >> 2U))));
        bufp->chgCData(oldp+182,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__mulOpInfo
                                  [0U])),2);
        bufp->chgBit(oldp+183,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opSrc
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+184,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opSrc
                                           [0U] >> 0xeU))),6);
        bufp->chgBit(oldp+185,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opSrc
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+186,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opSrc
                                           [0U] >> 7U))),6);
        bufp->chgBit(oldp+187,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opSrc
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+188,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opSrc
                                  [0U])),6);
        bufp->chgBit(oldp+189,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opDst
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+190,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opDst
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+191,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__opDst
                                  [0U])),6);
        bufp->chgSData(oldp+192,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 0xeU))),10);
        bufp->chgCData(oldp+193,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                        [0U][5U] >> 0xcU))),2);
        bufp->chgBit(oldp+194,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][5U] >> 0xbU))));
        bufp->chgBit(oldp+195,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][5U] >> 0xaU))));
        bufp->chgBit(oldp+196,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][5U] >> 9U))));
        bufp->chgSData(oldp+197,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                             [0U][5U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                               [0U][4U] 
                                               >> 0x1fU)))),10);
        bufp->chgCData(oldp+198,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                        [0U][4U] >> 0x1dU))),2);
        bufp->chgBit(oldp+199,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][4U] >> 0x1cU))));
        bufp->chgCData(oldp+200,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                        [0U][4U] >> 0x1aU))),2);
        bufp->chgCData(oldp+201,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                        [0U][4U] >> 0x17U))),3);
        bufp->chgCData(oldp+202,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0x11U))),6);
        bufp->chgCData(oldp+203,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                          [0U][4U] 
                                          >> 0xdU))),4);
        bufp->chgCData(oldp+204,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                          [0U][4U] 
                                          >> 9U))),4);
        bufp->chgBit(oldp+205,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][4U] >> 8U))));
        bufp->chgCData(oldp+206,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+207,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][4U] >> 1U))));
        bufp->chgCData(oldp+208,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                            [0U][4U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                              [0U][3U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+209,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][3U] >> 0x1aU))));
        bufp->chgCData(oldp+210,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+211,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][3U] >> 0x13U))));
        bufp->chgBit(oldp+212,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][3U] >> 0x12U))));
        bufp->chgCData(oldp+213,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+214,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][3U] >> 0xbU))));
        bufp->chgIData(oldp+215,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                               [0U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                                 [0U][2U] 
                                                 >> 0x18U)))),19);
        bufp->chgBit(oldp+216,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][2U] >> 0x17U))));
        bufp->chgBit(oldp+217,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][2U] >> 0x16U))));
        bufp->chgIData(oldp+218,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                   [0U][2U] << 0xaU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                     [0U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+219,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+220,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                   [0U][1U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                     [0U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgCData(oldp+222,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                        [0U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+223,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 0x11U))));
        bufp->chgBit(oldp+224,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+225,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+226,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+227,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 0xdU))));
        bufp->chgCData(oldp+228,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                        [0U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+229,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+230,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 9U))));
        bufp->chgBit(oldp+231,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 8U))));
        bufp->chgBit(oldp+232,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 7U))));
        bufp->chgBit(oldp+233,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 6U))));
        bufp->chgCData(oldp+234,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                        [0U][0U] >> 4U))),2);
        bufp->chgBit(oldp+235,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 3U))));
        bufp->chgBit(oldp+236,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 2U))));
        bufp->chgBit(oldp+237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                      [0U][0U] >> 1U))));
        bufp->chgBit(oldp+238,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__nextStage
                                [0U][0U])));
        bufp->chgIData(oldp+239,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+240,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__complexRrStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgSData(oldp+241,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            >> 0xeU))),10);
        bufp->chgCData(oldp+242,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                        [0U][5U] >> 0xcU))),2);
        bufp->chgBit(oldp+243,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][5U] >> 0xbU))));
        bufp->chgBit(oldp+244,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][5U] >> 0xaU))));
        bufp->chgBit(oldp+245,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][5U] >> 9U))));
        bufp->chgSData(oldp+246,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                             [0U][5U] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                               [0U][4U] 
                                               >> 0x1fU)))),10);
        bufp->chgCData(oldp+247,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                        [0U][4U] >> 0x1dU))),2);
        bufp->chgBit(oldp+248,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][4U] >> 0x1cU))));
        bufp->chgCData(oldp+249,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                        [0U][4U] >> 0x1aU))),2);
        bufp->chgCData(oldp+250,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                        [0U][4U] >> 0x17U))),3);
        bufp->chgCData(oldp+251,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0x11U))),6);
        bufp->chgCData(oldp+252,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 0xdU))),4);
        bufp->chgCData(oldp+253,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 9U))),4);
        bufp->chgBit(oldp+254,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][4U] >> 8U))));
        bufp->chgCData(oldp+255,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+256,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][4U] >> 1U))));
        bufp->chgCData(oldp+257,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                              [0U][3U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+258,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x1aU))));
        bufp->chgCData(oldp+259,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+260,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x13U))));
        bufp->chgBit(oldp+261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x12U))));
        bufp->chgCData(oldp+262,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0xbU))));
        bufp->chgIData(oldp+264,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                                 [0U][2U] 
                                                 >> 0x18U)))),19);
        bufp->chgBit(oldp+265,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x17U))));
        bufp->chgBit(oldp+266,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x16U))));
        bufp->chgIData(oldp+267,(((vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                   [0U][2U] << 0xaU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                     [0U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+268,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+269,(((vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                   [0U][1U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                     [0U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+270,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgCData(oldp+271,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                        [0U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+272,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x11U))));
        bufp->chgBit(oldp+273,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+274,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+275,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+276,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xdU))));
        bufp->chgCData(oldp+277,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                        [0U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+278,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+279,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 9U))));
        bufp->chgBit(oldp+280,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 8U))));
        bufp->chgBit(oldp+281,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 7U))));
        bufp->chgBit(oldp+282,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 6U))));
        bufp->chgCData(oldp+283,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                        [0U][0U] >> 4U))),2);
        bufp->chgBit(oldp+284,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 3U))));
        bufp->chgBit(oldp+285,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 2U))));
        bufp->chgBit(oldp+286,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 1U))));
        bufp->chgBit(oldp+287,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage
                                [0U][0U])));
        bufp->chgBit(oldp+288,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumA
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+289,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumA
                                  [0U])),6);
        bufp->chgBit(oldp+290,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumB
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+291,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumB
                                  [0U])),6);
        bufp->chgBit(oldp+292,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumA
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+293,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumA
                                  [0U])),6);
        bufp->chgBit(oldp+294,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumB
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+295,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumB
                                  [0U])),6);
        bufp->chgBit(oldp+296,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhyDstRegNum
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+297,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhyDstRegNum
                                  [0U])),6);
        bufp->chgBit(oldp+298,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegA[0]));
        bufp->chgBit(oldp+299,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegB[0]));
        bufp->chgBit(oldp+300,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexWriteReg[0]));
        bufp->chgBit(oldp+301,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
                                      [0U] >> 0xdU))));
        bufp->chgBit(oldp+302,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
                                      [0U] >> 0xcU))));
        bufp->chgSData(oldp+303,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
                                            [0U] >> 2U))),10);
        bufp->chgCData(oldp+304,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
                                  [0U])),2);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x18U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x5dU])))) {
        bufp->chgIData(oldp+305,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__immOut[0]),32);
        bufp->chgIData(oldp+306,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__immOut[1]),32);
        bufp->chgIData(oldp+307,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__pc[0]),32);
        bufp->chgIData(oldp+308,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__pc[1]),32);
        bufp->chgBit(oldp+309,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandA
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+310,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandA
                                         [0U])),32);
        bufp->chgBit(oldp+311,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandA
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+312,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandA
                                         [1U])),32);
        bufp->chgBit(oldp+313,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandB
                                              [0U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+314,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandB
                                         [0U])),32);
        bufp->chgBit(oldp+315,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandB
                                              [1U] 
                                              >> 0x20U)))));
        bufp->chgIData(oldp+316,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__operandB
                                         [1U])),32);
        bufp->chgBit(oldp+317,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__stall));
        bufp->chgBit(oldp+318,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__clear));
        bufp->chgBit(oldp+319,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__flush[0]));
        bufp->chgBit(oldp+320,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__flush[1]));
        bufp->chgSData(oldp+321,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                            [0U][4U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+322,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                         [0U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [0U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgCData(oldp+323,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [0U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+324,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [0U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+325,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                          [0U][3U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+326,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][3U] >> 0x16U))));
        bufp->chgIData(oldp+327,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                                  [0U][3U] 
                                                  << 8U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                                    [0U][2U] 
                                                    >> 0x18U)))),30);
        bufp->chgIData(oldp+328,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [0U][2U] 
                                              >> 6U))),18);
        bufp->chgBit(oldp+329,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+330,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [0U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+331,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][3U] >> 6U))));
        bufp->chgSData(oldp+332,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                             [0U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                               [0U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+333,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [0U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+334,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [0U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+335,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [0U][2U] >> 3U))),3);
        bufp->chgCData(oldp+336,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                  [0U][2U])),3);
        bufp->chgCData(oldp+337,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                  [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+338,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                          [0U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+339,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                          [0U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+340,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+341,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+342,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+343,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+344,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][1U] >> 3U))));
        bufp->chgCData(oldp+345,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                            [0U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [0U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+346,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+347,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+348,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [0U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+349,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+350,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [0U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+351,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                [0U][0U])));
        bufp->chgSData(oldp+352,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                            [1U][4U] 
                                            >> 1U))),10);
        bufp->chgCData(oldp+353,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                         [1U][4U] << 1U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [1U][3U] 
                                           >> 0x1fU)))),2);
        bufp->chgCData(oldp+354,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [1U][3U] >> 0x1dU))),2);
        bufp->chgCData(oldp+355,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [1U][3U] >> 0x1bU))),2);
        bufp->chgCData(oldp+356,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                          [1U][3U] 
                                          >> 0x17U))),4);
        bufp->chgBit(oldp+357,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][3U] >> 0x16U))));
        bufp->chgIData(oldp+358,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                                  [1U][3U] 
                                                  << 8U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                                    [1U][2U] 
                                                    >> 0x18U)))),30);
        bufp->chgIData(oldp+359,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [1U][2U] 
                                              >> 6U))),18);
        bufp->chgBit(oldp+360,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][3U] >> 0x1aU))));
        bufp->chgIData(oldp+361,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [1U][3U] 
                                              >> 7U))),19);
        bufp->chgBit(oldp+362,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][3U] >> 6U))));
        bufp->chgSData(oldp+363,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                             [1U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                               [1U][2U] 
                                               >> 0x1cU)))),10);
        bufp->chgCData(oldp+364,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [1U][2U] >> 0x1aU))),2);
        bufp->chgIData(oldp+365,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [1U][2U] 
                                              >> 6U))),20);
        bufp->chgCData(oldp+366,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                        [1U][2U] >> 3U))),3);
        bufp->chgCData(oldp+367,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                  [1U][2U])),3);
        bufp->chgCData(oldp+368,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                  [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+369,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                          [1U][1U] 
                                          >> 0x16U))),4);
        bufp->chgCData(oldp+370,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                          [1U][1U] 
                                          >> 0x12U))),4);
        bufp->chgBit(oldp+371,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+372,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [1U][1U] 
                                           >> 0xbU))),6);
        bufp->chgBit(oldp+373,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+374,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [1U][1U] 
                                           >> 4U))),6);
        bufp->chgBit(oldp+375,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][1U] >> 3U))));
        bufp->chgCData(oldp+376,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                            [1U][1U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [1U][0U] 
                                              >> 0x1dU)))),6);
        bufp->chgBit(oldp+377,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+378,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+379,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                           [1U][0U] 
                                           >> 0x15U))),6);
        bufp->chgBit(oldp+380,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                      [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+381,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                              [1U][0U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+382,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__iqData
                                [1U][0U])));
        bufp->chgCData(oldp+383,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                [0U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+384,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                [0U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+385,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                  [0U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+386,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                              [0U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+387,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                         [0U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+388,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                     [0U]))),18);
        bufp->chgCData(oldp+389,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                [1U] 
                                                >> 0x37U)))),2);
        bufp->chgCData(oldp+390,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                [1U] 
                                                >> 0x35U)))),2);
        bufp->chgCData(oldp+391,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                  [1U] 
                                                  >> 0x31U)))),4);
        bufp->chgBit(oldp+392,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                              [1U] 
                                              >> 0x30U)))));
        bufp->chgIData(oldp+393,((0x3fffffffU & (IData)(
                                                        (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                         [1U] 
                                                         >> 0x12U)))),30);
        bufp->chgIData(oldp+394,((0x3ffffU & (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__intSubInfo
                                                     [1U]))),18);
        bufp->chgBit(oldp+395,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                      [0U] >> 0x14U))));
        bufp->chgCData(oldp+396,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                           [0U] >> 0xeU))),6);
        bufp->chgBit(oldp+397,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                      [0U] >> 0xdU))));
        bufp->chgCData(oldp+398,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                           [0U] >> 7U))),6);
        bufp->chgBit(oldp+399,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+400,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                  [0U])),6);
        bufp->chgBit(oldp+401,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                      [1U] >> 0x14U))));
        bufp->chgCData(oldp+402,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                           [1U] >> 0xeU))),6);
        bufp->chgBit(oldp+403,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                      [1U] >> 0xdU))));
        bufp->chgCData(oldp+404,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                           [1U] >> 7U))),6);
        bufp->chgBit(oldp+405,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+406,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opSrc
                                  [1U])),6);
        bufp->chgBit(oldp+407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opDst
                                      [0U] >> 7U))));
        bufp->chgBit(oldp+408,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opDst
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+409,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opDst
                                  [0U])),6);
        bufp->chgBit(oldp+410,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opDst
                                      [1U] >> 7U))));
        bufp->chgBit(oldp+411,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opDst
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+412,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__opDst
                                  [1U])),6);
        bufp->chgSData(oldp+413,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                            [0U][7U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+414,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][7U] >> 3U))),2);
        bufp->chgBit(oldp+415,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][7U] >> 2U))));
        bufp->chgSData(oldp+416,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                             [0U][7U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [0U][6U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+417,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+418,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][6U] >> 0x14U))),2);
        bufp->chgCData(oldp+419,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][6U] >> 0x12U))),2);
        bufp->chgCData(oldp+420,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                          [0U][6U] 
                                          >> 0xeU))),4);
        bufp->chgBit(oldp+421,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][6U] >> 0xdU))));
        bufp->chgIData(oldp+422,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                  [0U][6U] 
                                                  << 0x11U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                    [0U][5U] 
                                                    >> 0xfU)))),30);
        bufp->chgIData(oldp+423,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [0U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [0U][4U] 
                                                 >> 0x1dU)))),18);
        bufp->chgBit(oldp+424,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][6U] >> 0x11U))));
        bufp->chgIData(oldp+425,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [0U][6U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [0U][5U] 
                                                 >> 0x1eU)))),19);
        bufp->chgBit(oldp+426,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][5U] >> 0x1dU))));
        bufp->chgSData(oldp+427,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+428,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][5U] >> 0x11U))),2);
        bufp->chgIData(oldp+429,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [0U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [0U][4U] 
                                                 >> 0x1dU)))),20);
        bufp->chgCData(oldp+430,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+431,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][4U] >> 0x17U))),3);
        bufp->chgCData(oldp+432,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0x11U))),6);
        bufp->chgCData(oldp+433,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                          [0U][4U] 
                                          >> 0xdU))),4);
        bufp->chgCData(oldp+434,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                          [0U][4U] 
                                          >> 9U))),4);
        bufp->chgBit(oldp+435,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][4U] >> 8U))));
        bufp->chgCData(oldp+436,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+437,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][4U] >> 1U))));
        bufp->chgCData(oldp+438,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                            [0U][4U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                              [0U][3U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+439,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][3U] >> 0x1aU))));
        bufp->chgCData(oldp+440,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+441,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][3U] >> 0x13U))));
        bufp->chgBit(oldp+442,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][3U] >> 0x12U))));
        bufp->chgCData(oldp+443,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+444,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][3U] >> 0xbU))));
        bufp->chgIData(oldp+445,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [0U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [0U][2U] 
                                                 >> 0x18U)))),19);
        bufp->chgBit(oldp+446,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][2U] >> 0x17U))));
        bufp->chgBit(oldp+447,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][2U] >> 0x16U))));
        bufp->chgIData(oldp+448,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                   [0U][2U] << 0xaU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                     [0U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+449,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+450,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                   [0U][1U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                     [0U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+451,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgCData(oldp+452,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+453,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 0x11U))));
        bufp->chgBit(oldp+454,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+455,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+456,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+457,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 0xdU))));
        bufp->chgCData(oldp+458,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+459,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+460,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 9U))));
        bufp->chgBit(oldp+461,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 8U))));
        bufp->chgBit(oldp+462,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 7U))));
        bufp->chgBit(oldp+463,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 6U))));
        bufp->chgCData(oldp+464,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [0U][0U] >> 4U))),2);
        bufp->chgBit(oldp+465,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 3U))));
        bufp->chgBit(oldp+466,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 2U))));
        bufp->chgBit(oldp+467,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [0U][0U] >> 1U))));
        bufp->chgBit(oldp+468,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+469,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                            [1U][7U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+470,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][7U] >> 3U))),2);
        bufp->chgBit(oldp+471,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][7U] >> 2U))));
        bufp->chgSData(oldp+472,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                             [1U][7U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [1U][6U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+473,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+474,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][6U] >> 0x14U))),2);
        bufp->chgCData(oldp+475,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][6U] >> 0x12U))),2);
        bufp->chgCData(oldp+476,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                          [1U][6U] 
                                          >> 0xeU))),4);
        bufp->chgBit(oldp+477,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][6U] >> 0xdU))));
        bufp->chgIData(oldp+478,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                  [1U][6U] 
                                                  << 0x11U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                    [1U][5U] 
                                                    >> 0xfU)))),30);
        bufp->chgIData(oldp+479,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [1U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [1U][4U] 
                                                 >> 0x1dU)))),18);
        bufp->chgBit(oldp+480,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][6U] >> 0x11U))));
        bufp->chgIData(oldp+481,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [1U][6U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [1U][5U] 
                                                 >> 0x1eU)))),19);
        bufp->chgBit(oldp+482,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][5U] >> 0x1dU))));
        bufp->chgSData(oldp+483,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                            [1U][5U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+484,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][5U] >> 0x11U))),2);
        bufp->chgIData(oldp+485,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [1U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [1U][4U] 
                                                 >> 0x1dU)))),20);
        bufp->chgCData(oldp+486,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+487,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][4U] >> 0x17U))),3);
        bufp->chgCData(oldp+488,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0x11U))),6);
        bufp->chgCData(oldp+489,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                          [1U][4U] 
                                          >> 0xdU))),4);
        bufp->chgCData(oldp+490,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                          [1U][4U] 
                                          >> 9U))),4);
        bufp->chgBit(oldp+491,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][4U] >> 8U))));
        bufp->chgCData(oldp+492,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+493,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][4U] >> 1U))));
        bufp->chgCData(oldp+494,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                            [1U][4U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                              [1U][3U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+495,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][3U] >> 0x1aU))));
        bufp->chgCData(oldp+496,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+497,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][3U] >> 0x13U))));
        bufp->chgBit(oldp+498,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][3U] >> 0x12U))));
        bufp->chgCData(oldp+499,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+500,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][3U] >> 0xbU))));
        bufp->chgIData(oldp+501,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                               [1U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                                 [1U][2U] 
                                                 >> 0x18U)))),19);
        bufp->chgBit(oldp+502,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][2U] >> 0x17U))));
        bufp->chgBit(oldp+503,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][2U] >> 0x16U))));
        bufp->chgIData(oldp+504,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                   [1U][2U] << 0xaU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                     [1U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+505,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][1U] >> 0x15U))));
        bufp->chgIData(oldp+506,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                   [1U][1U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                     [1U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+507,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 0x14U))));
        bufp->chgCData(oldp+508,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+509,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 0x11U))));
        bufp->chgBit(oldp+510,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 0x10U))));
        bufp->chgBit(oldp+511,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 0xfU))));
        bufp->chgBit(oldp+512,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 0xeU))));
        bufp->chgBit(oldp+513,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 0xdU))));
        bufp->chgCData(oldp+514,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+515,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 0xaU))));
        bufp->chgBit(oldp+516,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 9U))));
        bufp->chgBit(oldp+517,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 8U))));
        bufp->chgBit(oldp+518,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 7U))));
        bufp->chgBit(oldp+519,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 6U))));
        bufp->chgCData(oldp+520,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                        [1U][0U] >> 4U))),2);
        bufp->chgBit(oldp+521,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 3U))));
        bufp->chgBit(oldp+522,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 2U))));
        bufp->chgBit(oldp+523,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                      [1U][0U] >> 1U))));
        bufp->chgBit(oldp+524,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__nextStage
                                [1U][0U])));
        bufp->chgIData(oldp+525,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+526,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgSData(oldp+527,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                            [0U][7U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+528,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][7U] >> 3U))),2);
        bufp->chgBit(oldp+529,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][7U] >> 2U))));
        bufp->chgSData(oldp+530,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                             [0U][7U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [0U][6U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+531,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+532,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][6U] >> 0x14U))),2);
        bufp->chgCData(oldp+533,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][6U] >> 0x12U))),2);
        bufp->chgCData(oldp+534,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                          [0U][6U] 
                                          >> 0xeU))),4);
        bufp->chgBit(oldp+535,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][6U] >> 0xdU))));
        bufp->chgIData(oldp+536,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                  [0U][6U] 
                                                  << 0x11U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                    [0U][5U] 
                                                    >> 0xfU)))),30);
        bufp->chgIData(oldp+537,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [0U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [0U][4U] 
                                                 >> 0x1dU)))),18);
        bufp->chgBit(oldp+538,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][6U] >> 0x11U))));
        bufp->chgIData(oldp+539,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [0U][6U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [0U][5U] 
                                                 >> 0x1eU)))),19);
        bufp->chgBit(oldp+540,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][5U] >> 0x1dU))));
        bufp->chgSData(oldp+541,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+542,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][5U] >> 0x11U))),2);
        bufp->chgIData(oldp+543,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [0U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [0U][4U] 
                                                 >> 0x1dU)))),20);
        bufp->chgCData(oldp+544,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+545,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][4U] >> 0x17U))),3);
        bufp->chgCData(oldp+546,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0x11U))),6);
        bufp->chgCData(oldp+547,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 0xdU))),4);
        bufp->chgCData(oldp+548,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          >> 9U))),4);
        bufp->chgBit(oldp+549,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][4U] >> 8U))));
        bufp->chgCData(oldp+550,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+551,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][4U] >> 1U))));
        bufp->chgCData(oldp+552,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                              [0U][3U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+553,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x1aU))));
        bufp->chgCData(oldp+554,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+555,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x13U))));
        bufp->chgBit(oldp+556,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0x12U))));
        bufp->chgCData(oldp+557,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+558,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][3U] >> 0xbU))));
        bufp->chgIData(oldp+559,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [0U][2U] 
                                                 >> 0x18U)))),19);
        bufp->chgBit(oldp+560,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x17U))));
        bufp->chgBit(oldp+561,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x16U))));
        bufp->chgIData(oldp+562,(((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                   [0U][2U] << 0xaU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                     [0U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+563,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+564,(((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                   [0U][1U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                     [0U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+565,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x14U))));
        bufp->chgCData(oldp+566,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+567,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x11U))));
        bufp->chgBit(oldp+568,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+569,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xdU))));
        bufp->chgCData(oldp+572,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+574,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 9U))));
        bufp->chgBit(oldp+575,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 8U))));
        bufp->chgBit(oldp+576,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 7U))));
        bufp->chgBit(oldp+577,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 6U))));
        bufp->chgCData(oldp+578,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [0U][0U] >> 4U))),2);
        bufp->chgBit(oldp+579,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 3U))));
        bufp->chgBit(oldp+580,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 2U))));
        bufp->chgBit(oldp+581,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 1U))));
        bufp->chgBit(oldp+582,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+583,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                            [1U][7U] 
                                            >> 5U))),10);
        bufp->chgCData(oldp+584,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][7U] >> 3U))),2);
        bufp->chgBit(oldp+585,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][7U] >> 2U))));
        bufp->chgSData(oldp+586,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                             [1U][7U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [1U][6U] 
                                               >> 0x18U)))),10);
        bufp->chgCData(oldp+587,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][6U] >> 0x16U))),2);
        bufp->chgCData(oldp+588,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][6U] >> 0x14U))),2);
        bufp->chgCData(oldp+589,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][6U] >> 0x12U))),2);
        bufp->chgCData(oldp+590,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                          [1U][6U] 
                                          >> 0xeU))),4);
        bufp->chgBit(oldp+591,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][6U] >> 0xdU))));
        bufp->chgIData(oldp+592,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                  [1U][6U] 
                                                  << 0x11U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                    [1U][5U] 
                                                    >> 0xfU)))),30);
        bufp->chgIData(oldp+593,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [1U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [1U][4U] 
                                                 >> 0x1dU)))),18);
        bufp->chgBit(oldp+594,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][6U] >> 0x11U))));
        bufp->chgIData(oldp+595,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [1U][6U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [1U][5U] 
                                                 >> 0x1eU)))),19);
        bufp->chgBit(oldp+596,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][5U] >> 0x1dU))));
        bufp->chgSData(oldp+597,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                            [1U][5U] 
                                            >> 0x13U))),10);
        bufp->chgCData(oldp+598,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][5U] >> 0x11U))),2);
        bufp->chgIData(oldp+599,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [1U][5U] 
                                               << 3U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [1U][4U] 
                                                 >> 0x1dU)))),20);
        bufp->chgCData(oldp+600,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+601,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][4U] >> 0x17U))),3);
        bufp->chgCData(oldp+602,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 0x11U))),6);
        bufp->chgCData(oldp+603,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                          [1U][4U] 
                                          >> 0xdU))),4);
        bufp->chgCData(oldp+604,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                          [1U][4U] 
                                          >> 9U))),4);
        bufp->chgBit(oldp+605,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][4U] >> 8U))));
        bufp->chgCData(oldp+606,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 2U))),6);
        bufp->chgBit(oldp+607,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][4U] >> 1U))));
        bufp->chgCData(oldp+608,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                              [1U][3U] 
                                              >> 0x1bU)))),6);
        bufp->chgBit(oldp+609,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x1aU))));
        bufp->chgCData(oldp+610,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0x14U))),6);
        bufp->chgBit(oldp+611,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x13U))));
        bufp->chgBit(oldp+612,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][3U] >> 0x12U))));
        bufp->chgCData(oldp+613,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 0xcU))),6);
        bufp->chgBit(oldp+614,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][3U] >> 0xbU))));
        bufp->chgIData(oldp+615,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                                 [1U][2U] 
                                                 >> 0x18U)))),19);
        bufp->chgBit(oldp+616,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x17U))));
        bufp->chgBit(oldp+617,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][2U] >> 0x16U))));
        bufp->chgIData(oldp+618,(((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                   [1U][2U] << 0xaU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                     [1U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+619,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x15U))));
        bufp->chgIData(oldp+620,(((vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                   [1U][1U] << 0xbU) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                     [1U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+621,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x14U))));
        bufp->chgCData(oldp+622,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+623,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x11U))));
        bufp->chgBit(oldp+624,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x10U))));
        bufp->chgBit(oldp+625,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xfU))));
        bufp->chgBit(oldp+626,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xeU))));
        bufp->chgBit(oldp+627,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xdU))));
        bufp->chgCData(oldp+628,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+629,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0xaU))));
        bufp->chgBit(oldp+630,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 9U))));
        bufp->chgBit(oldp+631,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 8U))));
        bufp->chgBit(oldp+632,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 7U))));
        bufp->chgBit(oldp+633,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 6U))));
        bufp->chgCData(oldp+634,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                        [1U][0U] >> 4U))),2);
        bufp->chgBit(oldp+635,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 3U))));
        bufp->chgBit(oldp+636,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 2U))));
        bufp->chgBit(oldp+637,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 1U))));
        bufp->chgBit(oldp+638,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage
                                [1U][0U])));
        bufp->chgBit(oldp+639,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+640,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA
                                  [0U])),6);
        bufp->chgBit(oldp+641,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+642,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA
                                  [1U])),6);
        bufp->chgBit(oldp+643,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+644,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB
                                  [0U])),6);
        bufp->chgBit(oldp+645,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+646,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB
                                  [1U])),6);
        bufp->chgBit(oldp+647,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+648,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA
                                  [0U])),6);
        bufp->chgBit(oldp+649,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+650,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA
                                  [1U])),6);
        bufp->chgBit(oldp+651,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+652,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB
                                  [0U])),6);
        bufp->chgBit(oldp+653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+654,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB
                                  [1U])),6);
        bufp->chgBit(oldp+655,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum
                                      [0U] >> 6U))));
        bufp->chgCData(oldp+656,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum
                                  [0U])),6);
        bufp->chgBit(oldp+657,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum
                                      [1U] >> 6U))));
        bufp->chgCData(oldp+658,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum
                                  [1U])),6);
        bufp->chgBit(oldp+659,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA[0]));
        bufp->chgBit(oldp+660,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA[1]));
        bufp->chgBit(oldp+661,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB[0]));
        bufp->chgBit(oldp+662,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB[1]));
        bufp->chgBit(oldp+663,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg[0]));
        bufp->chgBit(oldp+664,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg[1]));
        bufp->chgBit(oldp+665,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                      [0U] >> 0xdU))));
        bufp->chgBit(oldp+666,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                      [0U] >> 0xcU))));
        bufp->chgSData(oldp+667,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                            [0U] >> 2U))),10);
        bufp->chgCData(oldp+668,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                  [0U])),2);
        bufp->chgBit(oldp+669,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                      [1U] >> 0xdU))));
        bufp->chgBit(oldp+670,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                      [1U] >> 0xcU))));
        bufp->chgSData(oldp+671,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                            [1U] >> 2U))),10);
        bufp->chgCData(oldp+672,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
                                  [1U])),2);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x19U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x63U])))) {
        bufp->chgBit(oldp+673,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__pdStage) 
                                      >> 1U))));
        bufp->chgBit(oldp+674,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__pdStage))));
        bufp->chgBit(oldp+675,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__idStage) 
                                      >> 1U))));
        bufp->chgBit(oldp+676,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__idStage))));
        bufp->chgBit(oldp+677,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__stallByDecodeStage));
        bufp->chgBit(oldp+678,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__stall));
        bufp->chgBit(oldp+679,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__clear));
        bufp->chgSData(oldp+680,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 4U))),10);
        bufp->chgCData(oldp+681,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][4U] >> 2U))),2);
        bufp->chgBit(oldp+682,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][4U] >> 1U))));
        bufp->chgCData(oldp+683,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                         [0U][4U] << 2U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x1eU)))),3);
        bufp->chgCData(oldp+684,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][3U] >> 0x1cU))),2);
        bufp->chgCData(oldp+685,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][3U] >> 0x19U))),3);
        bufp->chgBit(oldp+686,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 0x18U))));
        bufp->chgCData(oldp+687,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x13U))),5);
        bufp->chgBit(oldp+688,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 0x12U))));
        bufp->chgCData(oldp+689,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0xdU))),5);
        bufp->chgBit(oldp+690,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 0xcU))));
        bufp->chgCData(oldp+691,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 7U))),5);
        bufp->chgCData(oldp+692,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                          [0U][3U] 
                                          >> 3U))),4);
        bufp->chgBit(oldp+693,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 2U))));
        bufp->chgIData(oldp+694,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                  [0U][3U] 
                                                  << 0x1cU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                    [0U][2U] 
                                                    >> 4U)))),30);
        bufp->chgBit(oldp+695,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 6U))));
        bufp->chgBit(oldp+696,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 5U))));
        bufp->chgBit(oldp+697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 4U))));
        bufp->chgCData(oldp+698,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][3U] >> 2U))),2);
        bufp->chgCData(oldp+699,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [0U][3U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][2U] 
                                              >> 0x1dU)))),5);
        bufp->chgBit(oldp+700,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][2U] >> 0x1cU))));
        bufp->chgCData(oldp+701,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][2U] >> 0x1aU))),2);
        bufp->chgSData(oldp+702,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [0U][2U] 
                                            >> 0x10U))),10);
        bufp->chgSData(oldp+703,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [0U][2U] 
                                            >> 4U))),12);
        bufp->chgSData(oldp+704,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][3U] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                [0U][2U] 
                                                >> 0x18U)))),15);
        bufp->chgIData(oldp+705,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][2U] 
                                              >> 4U))),20);
        bufp->chgCData(oldp+706,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][3U] >> 4U))),2);
        bufp->chgSData(oldp+707,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][3U] 
                                              << 0xeU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                [0U][2U] 
                                                >> 0x12U)))),16);
        bufp->chgSData(oldp+708,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                             [0U][2U] 
                                             >> 4U))),14);
        bufp->chgSData(oldp+709,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][3U] 
                                              << 0xaU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                [0U][2U] 
                                                >> 0x16U)))),15);
        bufp->chgIData(oldp+710,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][2U] 
                                              >> 4U))),18);
        bufp->chgCData(oldp+711,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][3U] >> 4U))),3);
        bufp->chgBit(oldp+712,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][3U] >> 3U))));
        bufp->chgIData(oldp+713,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                               [0U][3U] 
                                               << 0x10U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                 [0U][2U] 
                                                 >> 0x10U)))),19);
        bufp->chgCData(oldp+714,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 1U))),5);
        bufp->chgCData(oldp+715,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [0U][3U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][2U] 
                                              >> 0x1cU)))),5);
        bufp->chgCData(oldp+716,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][2U] >> 0x19U))),3);
        bufp->chgIData(oldp+717,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                               [0U][2U] 
                                               >> 4U))),21);
        bufp->chgCData(oldp+718,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+719,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [0U][2U])),2);
        bufp->chgCData(oldp+720,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [0U][1U] >> 0x1eU)),2);
        bufp->chgBit(oldp+721,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x1dU))));
        bufp->chgBit(oldp+722,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x1cU))));
        bufp->chgBit(oldp+723,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x1bU))));
        bufp->chgBit(oldp+724,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x1aU))));
        bufp->chgBit(oldp+725,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x19U))));
        bufp->chgBit(oldp+726,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x18U))));
        bufp->chgCData(oldp+727,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [0U][1U] >> 0x16U))),2);
        bufp->chgBit(oldp+728,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgBit(oldp+729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][1U] >> 0x14U))));
        bufp->chgIData(oldp+730,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [0U][1U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+731,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                [0U][1U])));
        bufp->chgIData(oldp+732,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [0U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+733,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [0U][0U] >> 0xcU))));
        bufp->chgSData(oldp+734,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [0U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+735,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [0U][0U])),2);
        bufp->chgSData(oldp+736,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 4U))),10);
        bufp->chgCData(oldp+737,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][4U] >> 2U))),2);
        bufp->chgBit(oldp+738,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][4U] >> 1U))));
        bufp->chgCData(oldp+739,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                         [1U][4U] << 2U) 
                                        | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x1eU)))),3);
        bufp->chgCData(oldp+740,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][3U] >> 0x1cU))),2);
        bufp->chgCData(oldp+741,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][3U] >> 0x19U))),3);
        bufp->chgBit(oldp+742,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 0x18U))));
        bufp->chgCData(oldp+743,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0x13U))),5);
        bufp->chgBit(oldp+744,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 0x12U))));
        bufp->chgCData(oldp+745,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 0xdU))),5);
        bufp->chgBit(oldp+746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 0xcU))));
        bufp->chgCData(oldp+747,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 7U))),5);
        bufp->chgCData(oldp+748,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                          [1U][3U] 
                                          >> 3U))),4);
        bufp->chgBit(oldp+749,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 2U))));
        bufp->chgIData(oldp+750,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                  [1U][3U] 
                                                  << 0x1cU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                    [1U][2U] 
                                                    >> 4U)))),30);
        bufp->chgBit(oldp+751,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 6U))));
        bufp->chgBit(oldp+752,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 5U))));
        bufp->chgBit(oldp+753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 4U))));
        bufp->chgCData(oldp+754,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][3U] >> 2U))),2);
        bufp->chgCData(oldp+755,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [1U][3U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][2U] 
                                              >> 0x1dU)))),5);
        bufp->chgBit(oldp+756,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][2U] >> 0x1cU))));
        bufp->chgCData(oldp+757,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][2U] >> 0x1aU))),2);
        bufp->chgSData(oldp+758,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [1U][2U] 
                                            >> 0x10U))),10);
        bufp->chgSData(oldp+759,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [1U][2U] 
                                            >> 4U))),12);
        bufp->chgSData(oldp+760,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][3U] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                [1U][2U] 
                                                >> 0x18U)))),15);
        bufp->chgIData(oldp+761,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][2U] 
                                              >> 4U))),20);
        bufp->chgCData(oldp+762,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][3U] >> 4U))),2);
        bufp->chgSData(oldp+763,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][3U] 
                                              << 0xeU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                [1U][2U] 
                                                >> 0x12U)))),16);
        bufp->chgSData(oldp+764,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                             [1U][2U] 
                                             >> 4U))),14);
        bufp->chgSData(oldp+765,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][3U] 
                                              << 0xaU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                [1U][2U] 
                                                >> 0x16U)))),15);
        bufp->chgIData(oldp+766,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][2U] 
                                              >> 4U))),18);
        bufp->chgCData(oldp+767,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][3U] >> 4U))),3);
        bufp->chgBit(oldp+768,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][3U] >> 3U))));
        bufp->chgIData(oldp+769,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                               [1U][3U] 
                                               << 0x10U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                                 [1U][2U] 
                                                 >> 0x10U)))),19);
        bufp->chgCData(oldp+770,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                           [1U][3U] 
                                           >> 1U))),5);
        bufp->chgCData(oldp+771,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [1U][3U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][2U] 
                                              >> 0x1cU)))),5);
        bufp->chgCData(oldp+772,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][2U] >> 0x19U))),3);
        bufp->chgIData(oldp+773,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                               [1U][2U] 
                                               >> 4U))),21);
        bufp->chgCData(oldp+774,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][2U] >> 2U))),2);
        bufp->chgCData(oldp+775,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [1U][2U])),2);
        bufp->chgCData(oldp+776,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [1U][1U] >> 0x1eU)),2);
        bufp->chgBit(oldp+777,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x1dU))));
        bufp->chgBit(oldp+778,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x1cU))));
        bufp->chgBit(oldp+779,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x1bU))));
        bufp->chgBit(oldp+780,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x1aU))));
        bufp->chgBit(oldp+781,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x19U))));
        bufp->chgBit(oldp+782,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x18U))));
        bufp->chgCData(oldp+783,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                        [1U][1U] >> 0x16U))),2);
        bufp->chgBit(oldp+784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x15U))));
        bufp->chgBit(oldp+785,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][1U] >> 0x14U))));
        bufp->chgIData(oldp+786,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                              [1U][1U] 
                                              >> 1U))),19);
        bufp->chgBit(oldp+787,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                [1U][1U])));
        bufp->chgIData(oldp+788,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [1U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+789,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                      [1U][0U] >> 0xcU))));
        bufp->chgSData(oldp+790,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                            [1U][0U] 
                                            >> 2U))),10);
        bufp->chgCData(oldp+791,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextStage
                                  [1U][0U])),2);
        bufp->chgBit(oldp+792,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__complete));
        bufp->chgBit(oldp+793,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__stallBranchResolver));
        bufp->chgCData(oldp+794,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__nextValidMOps),6);
        bufp->chgBit(oldp+795,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__orgPickedInsnLane));
        bufp->chgIData(oldp+796,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk11__DOT__i),32);
        bufp->chgIData(oldp+797,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk12__DOT__j),32);
        bufp->chgIData(oldp+798,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk13__DOT__j),32);
        bufp->chgIData(oldp+799,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk14__DOT__i),32);
        bufp->chgIData(oldp+800,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk15__DOT__i),32);
        bufp->chgIData(oldp+801,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j),32);
        bufp->chgIData(oldp+802,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk17__DOT__j),32);
        bufp->chgBit(oldp+803,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__stall));
        bufp->chgBit(oldp+804,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__clear));
        bufp->chgSData(oldp+805,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [0U][0xaU] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [0U][9U] 
                                               >> 0x1fU)))),10);
        bufp->chgBit(oldp+806,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][9U] >> 0x1eU))));
        bufp->chgIData(oldp+807,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [0U][9U] << 2U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                     [0U][8U] >> 0x1eU))),32);
        bufp->chgBit(oldp+808,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][8U] >> 0x1dU))));
        bufp->chgIData(oldp+809,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][8U] 
                                              >> 0xaU))),19);
        bufp->chgBit(oldp+810,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][8U] >> 9U))));
        bufp->chgIData(oldp+811,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [0U][8U] 
                                               << 0xaU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [0U][7U] 
                                                 >> 0x16U)))),19);
        bufp->chgBit(oldp+812,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][7U] >> 0x15U))));
        bufp->chgSData(oldp+813,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [0U][7U] 
                                            >> 0xbU))),10);
        bufp->chgCData(oldp+814,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][7U] >> 9U))),2);
        bufp->chgCData(oldp+815,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+816,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][2U] >> 0xcU))),2);
        bufp->chgCData(oldp+817,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][2U] >> 9U))),3);
        bufp->chgBit(oldp+818,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 8U))));
        bufp->chgCData(oldp+819,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][2U] 
                                           >> 3U))),5);
        bufp->chgBit(oldp+820,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 2U))));
        bufp->chgCData(oldp+821,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [0U][2U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][1U] 
                                              >> 0x1dU)))),5);
        bufp->chgBit(oldp+822,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][1U] >> 0x1cU))));
        bufp->chgCData(oldp+823,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0x17U))),5);
        bufp->chgCData(oldp+824,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                          [0U][1U] 
                                          >> 0x13U))),4);
        bufp->chgBit(oldp+825,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][1U] >> 0x12U))));
        bufp->chgIData(oldp+826,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                  [0U][1U] 
                                                  << 0xcU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                    [0U][0U] 
                                                    >> 0x14U)))),30);
        bufp->chgBit(oldp+827,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][1U] >> 0x16U))));
        bufp->chgBit(oldp+828,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][1U] >> 0x15U))));
        bufp->chgBit(oldp+829,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][1U] >> 0x14U))));
        bufp->chgCData(oldp+830,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][1U] >> 0x12U))),2);
        bufp->chgCData(oldp+831,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0xdU))),5);
        bufp->chgBit(oldp+832,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][1U] >> 0xcU))));
        bufp->chgCData(oldp+833,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][1U] >> 0xaU))),2);
        bufp->chgSData(oldp+834,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][1U])),10);
        bufp->chgSData(oldp+835,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][0U] >> 0x14U)),12);
        bufp->chgSData(oldp+836,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [0U][1U] 
                                             >> 8U))),15);
        bufp->chgIData(oldp+837,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [0U][1U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [0U][0U] 
                                                 >> 0x14U)))),20);
        bufp->chgCData(oldp+838,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][1U] >> 0x14U))),2);
        bufp->chgSData(oldp+839,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [0U][1U] 
                                             >> 2U))),16);
        bufp->chgSData(oldp+840,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][1U] 
                                              << 0xcU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [0U][0U] 
                                                >> 0x14U)))),14);
        bufp->chgSData(oldp+841,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [0U][1U] 
                                             >> 6U))),15);
        bufp->chgIData(oldp+842,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [0U][1U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [0U][0U] 
                                                 >> 0x14U)))),18);
        bufp->chgCData(oldp+843,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][1U] >> 0x14U))),3);
        bufp->chgBit(oldp+844,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][1U] >> 0x13U))));
        bufp->chgIData(oldp+845,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][1U])),19);
        bufp->chgCData(oldp+846,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0x11U))),5);
        bufp->chgCData(oldp+847,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][1U] 
                                           >> 0xcU))),5);
        bufp->chgCData(oldp+848,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][1U] >> 9U))),3);
        bufp->chgIData(oldp+849,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [0U][1U] 
                                                << 0xcU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                  [0U][0U] 
                                                  >> 0x14U)))),21);
        bufp->chgCData(oldp+850,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][0U] >> 0x12U))),2);
        bufp->chgCData(oldp+851,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][0U] >> 0x10U))),2);
        bufp->chgCData(oldp+852,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][0U] >> 0xeU))),2);
        bufp->chgBit(oldp+853,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 0xdU))));
        bufp->chgBit(oldp+854,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 0xcU))));
        bufp->chgBit(oldp+855,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 0xbU))));
        bufp->chgBit(oldp+856,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+857,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 9U))));
        bufp->chgBit(oldp+858,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 8U))));
        bufp->chgCData(oldp+859,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][0U] >> 6U))),2);
        bufp->chgBit(oldp+860,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 5U))));
        bufp->chgCData(oldp+861,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+862,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][4U] >> 0x18U))),2);
        bufp->chgCData(oldp+863,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][4U] >> 0x15U))),3);
        bufp->chgBit(oldp+864,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][4U] >> 0x14U))));
        bufp->chgCData(oldp+865,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0xfU))),5);
        bufp->chgBit(oldp+866,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][4U] >> 0xeU))));
        bufp->chgCData(oldp+867,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 9U))),5);
        bufp->chgBit(oldp+868,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][4U] >> 8U))));
        bufp->chgCData(oldp+869,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 3U))),5);
        bufp->chgCData(oldp+870,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][4U] 
                                           << 1U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x1fU)))),4);
        bufp->chgBit(oldp+871,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][3U] >> 0x1eU))));
        bufp->chgIData(oldp+872,((0x3fffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][3U])),30);
        bufp->chgBit(oldp+873,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][4U] >> 2U))));
        bufp->chgBit(oldp+874,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][4U] >> 1U))));
        bufp->chgBit(oldp+875,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                [0U][4U])));
        bufp->chgCData(oldp+876,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][3U] >> 0x1eU)),2);
        bufp->chgCData(oldp+877,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x19U))),5);
        bufp->chgBit(oldp+878,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][3U] >> 0x18U))));
        bufp->chgCData(oldp+879,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][3U] >> 0x16U))),2);
        bufp->chgSData(oldp+880,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 0xcU))),10);
        bufp->chgSData(oldp+881,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][3U])),12);
        bufp->chgSData(oldp+882,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][4U] 
                                              << 0xcU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [0U][3U] 
                                                >> 0x14U)))),15);
        bufp->chgIData(oldp+883,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][3U])),20);
        bufp->chgCData(oldp+884,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][4U])),2);
        bufp->chgSData(oldp+885,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [0U][3U] 
                                             >> 0xeU))),16);
        bufp->chgSData(oldp+886,((0x3fffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][3U])),14);
        bufp->chgSData(oldp+887,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][4U] 
                                              << 0xeU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [0U][3U] 
                                                >> 0x12U)))),15);
        bufp->chgIData(oldp+888,((0x3ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][3U])),18);
        bufp->chgCData(oldp+889,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][4U])),3);
        bufp->chgBit(oldp+890,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                [0U][3U] >> 0x1fU)));
        bufp->chgIData(oldp+891,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][3U] 
                                              >> 0xcU))),19);
        bufp->chgCData(oldp+892,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [0U][4U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][3U] 
                                              >> 0x1dU)))),5);
        bufp->chgCData(oldp+893,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][3U] 
                                           >> 0x18U))),5);
        bufp->chgCData(oldp+894,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][3U] >> 0x15U))),3);
        bufp->chgIData(oldp+895,((0x1fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][3U])),21);
        bufp->chgCData(oldp+896,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][2U] >> 0x1eU)),2);
        bufp->chgCData(oldp+897,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][2U] >> 0x1cU))),2);
        bufp->chgCData(oldp+898,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][2U] >> 0x1aU))),2);
        bufp->chgBit(oldp+899,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+900,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 0x18U))));
        bufp->chgBit(oldp+901,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 0x17U))));
        bufp->chgBit(oldp+902,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 0x16U))));
        bufp->chgBit(oldp+903,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 0x15U))));
        bufp->chgBit(oldp+904,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 0x14U))));
        bufp->chgCData(oldp+905,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][2U] >> 0x12U))),2);
        bufp->chgBit(oldp+906,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][2U] >> 0x11U))));
        bufp->chgCData(oldp+907,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][7U] >> 6U))),3);
        bufp->chgCData(oldp+908,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][7U] >> 4U))),2);
        bufp->chgCData(oldp+909,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][7U] >> 1U))),3);
        bufp->chgBit(oldp+910,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                [0U][7U])));
        bufp->chgCData(oldp+911,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][6U] >> 0x1bU)),5);
        bufp->chgBit(oldp+912,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 0x1aU))));
        bufp->chgCData(oldp+913,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][6U] 
                                           >> 0x15U))),5);
        bufp->chgBit(oldp+914,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 0x14U))));
        bufp->chgCData(oldp+915,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][6U] 
                                           >> 0xfU))),5);
        bufp->chgCData(oldp+916,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                          [0U][6U] 
                                          >> 0xbU))),4);
        bufp->chgBit(oldp+917,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 0xaU))));
        bufp->chgIData(oldp+918,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                  [0U][6U] 
                                                  << 0x14U) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                    [0U][5U] 
                                                    >> 0xcU)))),30);
        bufp->chgBit(oldp+919,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 0xeU))));
        bufp->chgBit(oldp+920,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 0xdU))));
        bufp->chgBit(oldp+921,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 0xcU))));
        bufp->chgCData(oldp+922,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][6U] >> 0xaU))),2);
        bufp->chgCData(oldp+923,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][6U] 
                                           >> 5U))),5);
        bufp->chgBit(oldp+924,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 4U))));
        bufp->chgCData(oldp+925,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][6U] >> 2U))),2);
        bufp->chgSData(oldp+926,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [0U][6U] 
                                             << 8U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [0U][5U] 
                                               >> 0x18U)))),10);
        bufp->chgSData(oldp+927,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 0xcU))),12);
        bufp->chgSData(oldp+928,((0x7fffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][6U])),15);
        bufp->chgIData(oldp+929,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][5U] >> 0xcU)),20);
        bufp->chgCData(oldp+930,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][6U] >> 0xcU))),2);
        bufp->chgSData(oldp+931,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][6U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [0U][5U] 
                                                >> 0x1aU)))),16);
        bufp->chgSData(oldp+932,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [0U][5U] 
                                             >> 0xcU))),14);
        bufp->chgSData(oldp+933,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][6U] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [0U][5U] 
                                                >> 0x1eU)))),15);
        bufp->chgIData(oldp+934,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [0U][5U] 
                                              >> 0xcU))),18);
        bufp->chgCData(oldp+935,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][6U] >> 0xcU))),3);
        bufp->chgBit(oldp+936,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][6U] >> 0xbU))));
        bufp->chgIData(oldp+937,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [0U][6U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [0U][5U] 
                                                 >> 0x18U)))),19);
        bufp->chgCData(oldp+938,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][6U] 
                                           >> 9U))),5);
        bufp->chgCData(oldp+939,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [0U][6U] 
                                           >> 4U))),5);
        bufp->chgCData(oldp+940,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][6U] >> 1U))),3);
        bufp->chgIData(oldp+941,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [0U][6U] 
                                                << 0x14U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                  [0U][5U] 
                                                  >> 0xcU)))),21);
        bufp->chgCData(oldp+942,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+943,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][5U] >> 8U))),2);
        bufp->chgCData(oldp+944,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [0U][5U] >> 6U))),2);
        bufp->chgBit(oldp+945,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][5U] >> 5U))));
        bufp->chgBit(oldp+946,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][5U] >> 4U))));
        bufp->chgBit(oldp+947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][5U] >> 3U))));
        bufp->chgBit(oldp+948,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][5U] >> 2U))));
        bufp->chgBit(oldp+949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][5U] >> 1U))));
        bufp->chgBit(oldp+950,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                [0U][5U])));
        bufp->chgCData(oldp+951,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [0U][4U] >> 0x1eU)),2);
        bufp->chgBit(oldp+952,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+953,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 4U))));
        bufp->chgBit(oldp+954,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 3U))));
        bufp->chgBit(oldp+955,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 2U))));
        bufp->chgBit(oldp+956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [0U][0U] >> 1U))));
        bufp->chgBit(oldp+957,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                [0U][0U])));
        bufp->chgSData(oldp+958,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [1U][0xaU] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][9U] 
                                               >> 0x1fU)))),10);
        bufp->chgBit(oldp+959,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][9U] >> 0x1eU))));
        bufp->chgIData(oldp+960,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][9U] << 2U) 
                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                     [1U][8U] >> 0x1eU))),32);
        bufp->chgBit(oldp+961,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][8U] >> 0x1dU))));
        bufp->chgIData(oldp+962,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [1U][8U] 
                                              >> 0xaU))),19);
        bufp->chgBit(oldp+963,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][8U] >> 9U))));
        bufp->chgIData(oldp+964,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][8U] 
                                               << 0xaU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][7U] 
                                                 >> 0x16U)))),19);
        bufp->chgBit(oldp+965,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][7U] >> 0x15U))));
        bufp->chgSData(oldp+966,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][7U] 
                                            >> 0xbU))),10);
        bufp->chgCData(oldp+967,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][7U] >> 9U))),2);
        bufp->chgCData(oldp+968,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+969,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][2U] >> 0xcU))),2);
        bufp->chgCData(oldp+970,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][2U] >> 9U))),3);
        bufp->chgBit(oldp+971,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][2U] >> 8U))));
        bufp->chgCData(oldp+972,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [1U][2U] 
                                           >> 3U))),5);
        bufp->chgBit(oldp+973,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][2U] >> 2U))));
        bufp->chgCData(oldp+974,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][2U] 
                                            << 3U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [1U][1U] 
                                              >> 0x1dU)))),5);
        bufp->chgBit(oldp+975,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][1U] >> 0x1cU))));
        bufp->chgCData(oldp+976,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 0x17U))),5);
        bufp->chgCData(oldp+977,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                          [1U][1U] 
                                          >> 0x13U))),4);
        bufp->chgBit(oldp+978,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][1U] >> 0x12U))));
        bufp->chgIData(oldp+979,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                  [1U][1U] 
                                                  << 0xcU) 
                                                 | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                    [1U][0U] 
                                                    >> 0x14U)))),30);
        bufp->chgBit(oldp+980,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][1U] >> 0x16U))));
        bufp->chgBit(oldp+981,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][1U] >> 0x15U))));
        bufp->chgBit(oldp+982,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][1U] >> 0x14U))));
        bufp->chgCData(oldp+983,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][1U] >> 0x12U))),2);
        bufp->chgCData(oldp+984,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 0xdU))),5);
        bufp->chgBit(oldp+985,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][1U] >> 0xcU))));
        bufp->chgCData(oldp+986,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][1U] >> 0xaU))),2);
        bufp->chgSData(oldp+987,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [1U][1U])),10);
        bufp->chgSData(oldp+988,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [1U][0U] >> 0x14U)),12);
        bufp->chgSData(oldp+989,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [1U][1U] 
                                             >> 8U))),15);
        bufp->chgIData(oldp+990,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][1U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][0U] 
                                                 >> 0x14U)))),20);
        bufp->chgCData(oldp+991,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][1U] >> 0x14U))),2);
        bufp->chgSData(oldp+992,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [1U][1U] 
                                             >> 2U))),16);
        bufp->chgSData(oldp+993,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [1U][1U] 
                                              << 0xcU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [1U][0U] 
                                                >> 0x14U)))),14);
        bufp->chgSData(oldp+994,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [1U][1U] 
                                             >> 6U))),15);
        bufp->chgIData(oldp+995,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][1U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][0U] 
                                                 >> 0x14U)))),18);
        bufp->chgCData(oldp+996,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                        [1U][1U] >> 0x14U))),3);
        bufp->chgBit(oldp+997,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                      [1U][1U] >> 0x13U))));
        bufp->chgIData(oldp+998,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                  [1U][1U])),19);
        bufp->chgCData(oldp+999,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [1U][1U] 
                                           >> 0x11U))),5);
        bufp->chgCData(oldp+1000,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1001,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][1U] >> 9U))),3);
        bufp->chgIData(oldp+1002,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][1U] 
                                                 << 0xcU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                   [1U][0U] 
                                                   >> 0x14U)))),21);
        bufp->chgCData(oldp+1003,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][0U] >> 0x12U))),2);
        bufp->chgCData(oldp+1004,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][0U] >> 0x10U))),2);
        bufp->chgCData(oldp+1005,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][0U] >> 0xeU))),2);
        bufp->chgBit(oldp+1006,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 0xdU))));
        bufp->chgBit(oldp+1007,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 0xcU))));
        bufp->chgBit(oldp+1008,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 0xbU))));
        bufp->chgBit(oldp+1009,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 0xaU))));
        bufp->chgBit(oldp+1010,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 9U))));
        bufp->chgBit(oldp+1011,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 8U))));
        bufp->chgCData(oldp+1012,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][0U] >> 6U))),2);
        bufp->chgBit(oldp+1013,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 5U))));
        bufp->chgCData(oldp+1014,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+1015,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][4U] >> 0x18U))),2);
        bufp->chgCData(oldp+1016,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][4U] >> 0x15U))),3);
        bufp->chgBit(oldp+1017,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][4U] >> 0x14U))));
        bufp->chgCData(oldp+1018,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 0xfU))),5);
        bufp->chgBit(oldp+1019,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][4U] >> 0xeU))));
        bufp->chgCData(oldp+1020,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 9U))),5);
        bufp->chgBit(oldp+1021,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][4U] >> 8U))));
        bufp->chgCData(oldp+1022,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 3U))),5);
        bufp->chgCData(oldp+1023,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][4U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [1U][3U] 
                                              >> 0x1fU)))),4);
        bufp->chgBit(oldp+1024,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][3U] >> 0x1eU))));
        bufp->chgIData(oldp+1025,((0x3fffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][3U])),30);
        bufp->chgBit(oldp+1026,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][4U] >> 2U))));
        bufp->chgBit(oldp+1027,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][4U] >> 1U))));
        bufp->chgBit(oldp+1028,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                 [1U][4U])));
        bufp->chgCData(oldp+1029,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][3U] >> 0x1eU)),2);
        bufp->chgCData(oldp+1030,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][3U] 
                                            >> 0x19U))),5);
        bufp->chgBit(oldp+1031,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][3U] >> 0x18U))));
        bufp->chgCData(oldp+1032,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][3U] >> 0x16U))),2);
        bufp->chgSData(oldp+1033,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [1U][3U] 
                                             >> 0xcU))),10);
        bufp->chgSData(oldp+1034,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][3U])),12);
        bufp->chgSData(oldp+1035,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][4U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][3U] 
                                                 >> 0x14U)))),15);
        bufp->chgIData(oldp+1036,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][3U])),20);
        bufp->chgCData(oldp+1037,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][4U])),2);
        bufp->chgSData(oldp+1038,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [1U][3U] 
                                              >> 0xeU))),16);
        bufp->chgSData(oldp+1039,((0x3fffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][3U])),14);
        bufp->chgSData(oldp+1040,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][4U] 
                                               << 0xeU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][3U] 
                                                 >> 0x12U)))),15);
        bufp->chgIData(oldp+1041,((0x3ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][3U])),18);
        bufp->chgCData(oldp+1042,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][4U])),3);
        bufp->chgBit(oldp+1043,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                 [1U][3U] >> 0x1fU)));
        bufp->chgIData(oldp+1044,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][3U] 
                                               >> 0xcU))),19);
        bufp->chgCData(oldp+1045,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [1U][4U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][3U] 
                                               >> 0x1dU)))),5);
        bufp->chgCData(oldp+1046,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][3U] 
                                            >> 0x18U))),5);
        bufp->chgCData(oldp+1047,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][3U] >> 0x15U))),3);
        bufp->chgIData(oldp+1048,((0x1fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][3U])),21);
        bufp->chgCData(oldp+1049,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][2U] >> 0x1eU)),2);
        bufp->chgCData(oldp+1050,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][2U] >> 0x1cU))),2);
        bufp->chgCData(oldp+1051,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][2U] >> 0x1aU))),2);
        bufp->chgBit(oldp+1052,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1053,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][2U] >> 0x18U))));
        bufp->chgBit(oldp+1054,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][2U] >> 0x17U))));
        bufp->chgBit(oldp+1055,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][2U] >> 0x16U))));
        bufp->chgBit(oldp+1056,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][2U] >> 0x15U))));
        bufp->chgBit(oldp+1057,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][2U] >> 0x14U))));
        bufp->chgCData(oldp+1058,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][2U] >> 0x12U))),2);
        bufp->chgBit(oldp+1059,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][2U] >> 0x11U))));
        bufp->chgCData(oldp+1060,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][7U] >> 6U))),3);
        bufp->chgCData(oldp+1061,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][7U] >> 4U))),2);
        bufp->chgCData(oldp+1062,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][7U] >> 1U))),3);
        bufp->chgBit(oldp+1063,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                 [1U][7U])));
        bufp->chgCData(oldp+1064,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][6U] >> 0x1bU)),5);
        bufp->chgBit(oldp+1065,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 0x1aU))));
        bufp->chgCData(oldp+1066,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][6U] 
                                            >> 0x15U))),5);
        bufp->chgBit(oldp+1067,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 0x14U))));
        bufp->chgCData(oldp+1068,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][6U] 
                                            >> 0xfU))),5);
        bufp->chgCData(oldp+1069,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                           [1U][6U] 
                                           >> 0xbU))),4);
        bufp->chgBit(oldp+1070,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 0xaU))));
        bufp->chgIData(oldp+1071,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                   [1U][6U] 
                                                   << 0x14U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                     [1U][5U] 
                                                     >> 0xcU)))),30);
        bufp->chgBit(oldp+1072,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 0xeU))));
        bufp->chgBit(oldp+1073,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 0xdU))));
        bufp->chgBit(oldp+1074,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 0xcU))));
        bufp->chgCData(oldp+1075,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][6U] >> 0xaU))),2);
        bufp->chgCData(oldp+1076,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][6U] 
                                            >> 5U))),5);
        bufp->chgBit(oldp+1077,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 4U))));
        bufp->chgCData(oldp+1078,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][6U] >> 2U))),2);
        bufp->chgSData(oldp+1079,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [1U][6U] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [1U][5U] 
                                                >> 0x18U)))),10);
        bufp->chgSData(oldp+1080,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                             [1U][5U] 
                                             >> 0xcU))),12);
        bufp->chgSData(oldp+1081,((0x7fffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][6U])),15);
        bufp->chgIData(oldp+1082,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][5U] >> 0xcU)),20);
        bufp->chgCData(oldp+1083,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][6U] >> 0xcU))),2);
        bufp->chgSData(oldp+1084,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][6U] 
                                               << 6U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][5U] 
                                                 >> 0x1aU)))),16);
        bufp->chgSData(oldp+1085,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                              [1U][5U] 
                                              >> 0xcU))),14);
        bufp->chgSData(oldp+1086,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][6U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][5U] 
                                                 >> 0x1eU)))),15);
        bufp->chgIData(oldp+1087,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                               [1U][5U] 
                                               >> 0xcU))),18);
        bufp->chgCData(oldp+1088,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][6U] >> 0xcU))),3);
        bufp->chgBit(oldp+1089,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][6U] >> 0xbU))));
        bufp->chgIData(oldp+1090,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                [1U][6U] 
                                                << 8U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                  [1U][5U] 
                                                  >> 0x18U)))),19);
        bufp->chgCData(oldp+1091,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][6U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1092,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                            [1U][6U] 
                                            >> 4U))),5);
        bufp->chgCData(oldp+1093,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][6U] >> 1U))),3);
        bufp->chgIData(oldp+1094,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                 [1U][6U] 
                                                 << 0x14U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                                   [1U][5U] 
                                                   >> 0xcU)))),21);
        bufp->chgCData(oldp+1095,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+1096,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][5U] >> 8U))),2);
        bufp->chgCData(oldp+1097,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                         [1U][5U] >> 6U))),2);
        bufp->chgBit(oldp+1098,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][5U] >> 5U))));
        bufp->chgBit(oldp+1099,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][5U] >> 4U))));
        bufp->chgBit(oldp+1100,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][5U] >> 3U))));
        bufp->chgBit(oldp+1101,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][5U] >> 2U))));
        bufp->chgBit(oldp+1102,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][5U] >> 1U))));
        bufp->chgBit(oldp+1103,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                 [1U][5U])));
        bufp->chgCData(oldp+1104,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                   [1U][4U] >> 0x1eU)),2);
        bufp->chgBit(oldp+1105,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1106,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 4U))));
        bufp->chgBit(oldp+1107,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 3U))));
        bufp->chgBit(oldp+1108,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+1109,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+1110,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__nextStage
                                 [1U][0U])));
        bufp->chgSData(oldp+1111,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [0U][9U] 
                                                >> 0x1fU)))),10);
        bufp->chgBit(oldp+1112,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][9U] >> 0x1eU))));
        bufp->chgIData(oldp+1113,(((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                    [0U][9U] << 2U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                      [0U][8U] >> 0x1eU))),32);
        bufp->chgBit(oldp+1114,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][8U] >> 0x1dU))));
        bufp->chgIData(oldp+1115,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][8U] 
                                               >> 0xaU))),19);
        bufp->chgBit(oldp+1116,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][8U] >> 9U))));
        bufp->chgIData(oldp+1117,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [0U][8U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [0U][7U] 
                                                  >> 0x16U)))),19);
        bufp->chgBit(oldp+1118,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][7U] >> 0x15U))));
        bufp->chgSData(oldp+1119,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [0U][7U] 
                                             >> 0xbU))),10);
        bufp->chgCData(oldp+1120,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][7U] >> 9U))),2);
        bufp->chgCData(oldp+1121,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+1122,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][2U] >> 0xcU))),2);
        bufp->chgCData(oldp+1123,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][2U] >> 9U))),3);
        bufp->chgBit(oldp+1124,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 8U))));
        bufp->chgCData(oldp+1125,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][2U] 
                                            >> 3U))),5);
        bufp->chgBit(oldp+1126,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 2U))));
        bufp->chgCData(oldp+1127,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [0U][2U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][1U] 
                                               >> 0x1dU)))),5);
        bufp->chgBit(oldp+1128,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x1cU))));
        bufp->chgCData(oldp+1129,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            >> 0x17U))),5);
        bufp->chgCData(oldp+1130,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                           [0U][1U] 
                                           >> 0x13U))),4);
        bufp->chgBit(oldp+1131,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x12U))));
        bufp->chgIData(oldp+1132,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [0U][1U] 
                                                   << 0xcU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                     [0U][0U] 
                                                     >> 0x14U)))),30);
        bufp->chgBit(oldp+1133,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x16U))));
        bufp->chgBit(oldp+1134,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x15U))));
        bufp->chgBit(oldp+1135,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x14U))));
        bufp->chgCData(oldp+1136,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][1U] >> 0x12U))),2);
        bufp->chgCData(oldp+1137,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1138,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][1U] >> 0xcU))));
        bufp->chgCData(oldp+1139,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][1U] >> 0xaU))),2);
        bufp->chgSData(oldp+1140,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][1U])),10);
        bufp->chgSData(oldp+1141,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][0U] >> 0x14U)),12);
        bufp->chgSData(oldp+1142,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 8U))),15);
        bufp->chgIData(oldp+1143,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [0U][1U] 
                                                << 0xcU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [0U][0U] 
                                                  >> 0x14U)))),20);
        bufp->chgCData(oldp+1144,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][1U] >> 0x14U))),2);
        bufp->chgSData(oldp+1145,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 2U))),16);
        bufp->chgSData(oldp+1146,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][1U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [0U][0U] 
                                                 >> 0x14U)))),14);
        bufp->chgSData(oldp+1147,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][1U] 
                                              >> 6U))),15);
        bufp->chgIData(oldp+1148,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [0U][1U] 
                                                << 0xcU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [0U][0U] 
                                                  >> 0x14U)))),18);
        bufp->chgCData(oldp+1149,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][1U] >> 0x14U))),3);
        bufp->chgBit(oldp+1150,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x13U))));
        bufp->chgIData(oldp+1151,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][1U])),19);
        bufp->chgCData(oldp+1152,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            >> 0x11U))),5);
        bufp->chgCData(oldp+1153,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1154,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][1U] >> 9U))),3);
        bufp->chgIData(oldp+1155,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [0U][1U] 
                                                 << 0xcU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [0U][0U] 
                                                   >> 0x14U)))),21);
        bufp->chgCData(oldp+1156,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][0U] >> 0x12U))),2);
        bufp->chgCData(oldp+1157,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][0U] >> 0x10U))),2);
        bufp->chgCData(oldp+1158,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][0U] >> 0xeU))),2);
        bufp->chgBit(oldp+1159,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xdU))));
        bufp->chgBit(oldp+1160,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xcU))));
        bufp->chgBit(oldp+1161,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xbU))));
        bufp->chgBit(oldp+1162,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+1163,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 9U))));
        bufp->chgBit(oldp+1164,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 8U))));
        bufp->chgCData(oldp+1165,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][0U] >> 6U))),2);
        bufp->chgBit(oldp+1166,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 5U))));
        bufp->chgCData(oldp+1167,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+1168,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][4U] >> 0x18U))),2);
        bufp->chgCData(oldp+1169,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][4U] >> 0x15U))),3);
        bufp->chgBit(oldp+1170,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x14U))));
        bufp->chgCData(oldp+1171,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 0xfU))),5);
        bufp->chgBit(oldp+1172,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][4U] >> 0xeU))));
        bufp->chgCData(oldp+1173,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 9U))),5);
        bufp->chgBit(oldp+1174,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][4U] >> 8U))));
        bufp->chgCData(oldp+1175,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 3U))),5);
        bufp->chgCData(oldp+1176,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][3U] 
                                              >> 0x1fU)))),4);
        bufp->chgBit(oldp+1177,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x1eU))));
        bufp->chgIData(oldp+1178,((0x3fffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][3U])),30);
        bufp->chgBit(oldp+1179,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][4U] >> 2U))));
        bufp->chgBit(oldp+1180,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][4U] >> 1U))));
        bufp->chgBit(oldp+1181,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [0U][4U])));
        bufp->chgCData(oldp+1182,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][3U] >> 0x1eU)),2);
        bufp->chgCData(oldp+1183,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0x19U))),5);
        bufp->chgBit(oldp+1184,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x18U))));
        bufp->chgCData(oldp+1185,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][3U] >> 0x16U))),2);
        bufp->chgSData(oldp+1186,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [0U][3U] 
                                             >> 0xcU))),10);
        bufp->chgSData(oldp+1187,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][3U])),12);
        bufp->chgSData(oldp+1188,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][4U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [0U][3U] 
                                                 >> 0x14U)))),15);
        bufp->chgIData(oldp+1189,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][3U])),20);
        bufp->chgCData(oldp+1190,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][4U])),2);
        bufp->chgSData(oldp+1191,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][3U] 
                                              >> 0xeU))),16);
        bufp->chgSData(oldp+1192,((0x3fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][3U])),14);
        bufp->chgSData(oldp+1193,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][4U] 
                                               << 0xeU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [0U][3U] 
                                                 >> 0x12U)))),15);
        bufp->chgIData(oldp+1194,((0x3ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][3U])),18);
        bufp->chgCData(oldp+1195,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][4U])),3);
        bufp->chgBit(oldp+1196,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [0U][3U] >> 0x1fU)));
        bufp->chgIData(oldp+1197,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               >> 0xcU))),19);
        bufp->chgCData(oldp+1198,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [0U][4U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               >> 0x1dU)))),5);
        bufp->chgCData(oldp+1199,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0x18U))),5);
        bufp->chgCData(oldp+1200,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][3U] >> 0x15U))),3);
        bufp->chgIData(oldp+1201,((0x1fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][3U])),21);
        bufp->chgCData(oldp+1202,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][2U] >> 0x1eU)),2);
        bufp->chgCData(oldp+1203,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][2U] >> 0x1cU))),2);
        bufp->chgCData(oldp+1204,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][2U] >> 0x1aU))),2);
        bufp->chgBit(oldp+1205,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1206,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x18U))));
        bufp->chgBit(oldp+1207,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x17U))));
        bufp->chgBit(oldp+1208,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x16U))));
        bufp->chgBit(oldp+1209,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x15U))));
        bufp->chgBit(oldp+1210,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x14U))));
        bufp->chgCData(oldp+1211,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][2U] >> 0x12U))),2);
        bufp->chgBit(oldp+1212,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x11U))));
        bufp->chgCData(oldp+1213,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][7U] >> 6U))),3);
        bufp->chgCData(oldp+1214,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][7U] >> 4U))),2);
        bufp->chgCData(oldp+1215,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][7U] >> 1U))),3);
        bufp->chgBit(oldp+1216,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [0U][7U])));
        bufp->chgCData(oldp+1217,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][6U] >> 0x1bU)),5);
        bufp->chgBit(oldp+1218,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 0x1aU))));
        bufp->chgCData(oldp+1219,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            >> 0x15U))),5);
        bufp->chgBit(oldp+1220,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 0x14U))));
        bufp->chgCData(oldp+1221,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            >> 0xfU))),5);
        bufp->chgCData(oldp+1222,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                           [0U][6U] 
                                           >> 0xbU))),4);
        bufp->chgBit(oldp+1223,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 0xaU))));
        bufp->chgIData(oldp+1224,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [0U][6U] 
                                                   << 0x14U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                     [0U][5U] 
                                                     >> 0xcU)))),30);
        bufp->chgBit(oldp+1225,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 0xeU))));
        bufp->chgBit(oldp+1226,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 0xdU))));
        bufp->chgBit(oldp+1227,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 0xcU))));
        bufp->chgCData(oldp+1228,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][6U] >> 0xaU))),2);
        bufp->chgCData(oldp+1229,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            >> 5U))),5);
        bufp->chgBit(oldp+1230,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 4U))));
        bufp->chgCData(oldp+1231,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][6U] >> 2U))),2);
        bufp->chgSData(oldp+1232,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][6U] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [0U][5U] 
                                                >> 0x18U)))),10);
        bufp->chgSData(oldp+1233,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [0U][5U] 
                                             >> 0xcU))),12);
        bufp->chgSData(oldp+1234,((0x7fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][6U])),15);
        bufp->chgIData(oldp+1235,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][5U] >> 0xcU)),20);
        bufp->chgCData(oldp+1236,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][6U] >> 0xcU))),2);
        bufp->chgSData(oldp+1237,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][6U] 
                                               << 6U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [0U][5U] 
                                                 >> 0x1aU)))),16);
        bufp->chgSData(oldp+1238,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [0U][5U] 
                                              >> 0xcU))),14);
        bufp->chgSData(oldp+1239,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][6U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [0U][5U] 
                                                 >> 0x1eU)))),15);
        bufp->chgIData(oldp+1240,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [0U][5U] 
                                               >> 0xcU))),18);
        bufp->chgCData(oldp+1241,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][6U] >> 0xcU))),3);
        bufp->chgBit(oldp+1242,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][6U] >> 0xbU))));
        bufp->chgIData(oldp+1243,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [0U][6U] 
                                                << 8U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [0U][5U] 
                                                  >> 0x18U)))),19);
        bufp->chgCData(oldp+1244,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1245,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            >> 4U))),5);
        bufp->chgCData(oldp+1246,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][6U] >> 1U))),3);
        bufp->chgIData(oldp+1247,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [0U][6U] 
                                                 << 0x14U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [0U][5U] 
                                                   >> 0xcU)))),21);
        bufp->chgCData(oldp+1248,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+1249,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][5U] >> 8U))),2);
        bufp->chgCData(oldp+1250,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [0U][5U] >> 6U))),2);
        bufp->chgBit(oldp+1251,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][5U] >> 5U))));
        bufp->chgBit(oldp+1252,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][5U] >> 4U))));
        bufp->chgBit(oldp+1253,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][5U] >> 3U))));
        bufp->chgBit(oldp+1254,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][5U] >> 2U))));
        bufp->chgBit(oldp+1255,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][5U] >> 1U))));
        bufp->chgBit(oldp+1256,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [0U][5U])));
        bufp->chgCData(oldp+1257,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [0U][4U] >> 0x1eU)),2);
        bufp->chgBit(oldp+1258,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 4U))));
        bufp->chgBit(oldp+1260,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+1261,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+1262,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+1263,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+1264,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [1U][9U] 
                                                >> 0x1fU)))),10);
        bufp->chgBit(oldp+1265,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][9U] >> 0x1eU))));
        bufp->chgIData(oldp+1266,(((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                    [1U][9U] << 2U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                      [1U][8U] >> 0x1eU))),32);
        bufp->chgBit(oldp+1267,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][8U] >> 0x1dU))));
        bufp->chgIData(oldp+1268,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][8U] 
                                               >> 0xaU))),19);
        bufp->chgBit(oldp+1269,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][8U] >> 9U))));
        bufp->chgIData(oldp+1270,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [1U][8U] 
                                                << 0xaU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [1U][7U] 
                                                  >> 0x16U)))),19);
        bufp->chgBit(oldp+1271,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][7U] >> 0x15U))));
        bufp->chgSData(oldp+1272,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [1U][7U] 
                                             >> 0xbU))),10);
        bufp->chgCData(oldp+1273,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][7U] >> 9U))),2);
        bufp->chgCData(oldp+1274,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+1275,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][2U] >> 0xcU))),2);
        bufp->chgCData(oldp+1276,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][2U] >> 9U))),3);
        bufp->chgBit(oldp+1277,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 8U))));
        bufp->chgCData(oldp+1278,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][2U] 
                                            >> 3U))),5);
        bufp->chgBit(oldp+1279,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 2U))));
        bufp->chgCData(oldp+1280,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [1U][2U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][1U] 
                                               >> 0x1dU)))),5);
        bufp->chgBit(oldp+1281,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x1cU))));
        bufp->chgCData(oldp+1282,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][1U] 
                                            >> 0x17U))),5);
        bufp->chgCData(oldp+1283,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                           [1U][1U] 
                                           >> 0x13U))),4);
        bufp->chgBit(oldp+1284,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x12U))));
        bufp->chgIData(oldp+1285,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [1U][1U] 
                                                   << 0xcU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                     [1U][0U] 
                                                     >> 0x14U)))),30);
        bufp->chgBit(oldp+1286,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x16U))));
        bufp->chgBit(oldp+1287,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x15U))));
        bufp->chgBit(oldp+1288,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x14U))));
        bufp->chgCData(oldp+1289,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][1U] >> 0x12U))),2);
        bufp->chgCData(oldp+1290,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][1U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1291,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][1U] >> 0xcU))));
        bufp->chgCData(oldp+1292,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][1U] >> 0xaU))),2);
        bufp->chgSData(oldp+1293,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][1U])),10);
        bufp->chgSData(oldp+1294,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][0U] >> 0x14U)),12);
        bufp->chgSData(oldp+1295,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][1U] 
                                              >> 8U))),15);
        bufp->chgIData(oldp+1296,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [1U][1U] 
                                                << 0xcU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [1U][0U] 
                                                  >> 0x14U)))),20);
        bufp->chgCData(oldp+1297,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][1U] >> 0x14U))),2);
        bufp->chgSData(oldp+1298,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][1U] 
                                              >> 2U))),16);
        bufp->chgSData(oldp+1299,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][1U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [1U][0U] 
                                                 >> 0x14U)))),14);
        bufp->chgSData(oldp+1300,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][1U] 
                                              >> 6U))),15);
        bufp->chgIData(oldp+1301,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [1U][1U] 
                                                << 0xcU) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [1U][0U] 
                                                  >> 0x14U)))),18);
        bufp->chgCData(oldp+1302,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][1U] >> 0x14U))),3);
        bufp->chgBit(oldp+1303,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x13U))));
        bufp->chgIData(oldp+1304,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][1U])),19);
        bufp->chgCData(oldp+1305,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][1U] 
                                            >> 0x11U))),5);
        bufp->chgCData(oldp+1306,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][1U] 
                                            >> 0xcU))),5);
        bufp->chgCData(oldp+1307,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][1U] >> 9U))),3);
        bufp->chgIData(oldp+1308,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [1U][1U] 
                                                 << 0xcU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [1U][0U] 
                                                   >> 0x14U)))),21);
        bufp->chgCData(oldp+1309,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][0U] >> 0x12U))),2);
        bufp->chgCData(oldp+1310,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][0U] >> 0x10U))),2);
        bufp->chgCData(oldp+1311,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][0U] >> 0xeU))),2);
        bufp->chgBit(oldp+1312,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xdU))));
        bufp->chgBit(oldp+1313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xcU))));
        bufp->chgBit(oldp+1314,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xbU))));
        bufp->chgBit(oldp+1315,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xaU))));
        bufp->chgBit(oldp+1316,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 9U))));
        bufp->chgBit(oldp+1317,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 8U))));
        bufp->chgCData(oldp+1318,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][0U] >> 6U))),2);
        bufp->chgBit(oldp+1319,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 5U))));
        bufp->chgCData(oldp+1320,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][4U] >> 0x1aU))),3);
        bufp->chgCData(oldp+1321,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][4U] >> 0x18U))),2);
        bufp->chgCData(oldp+1322,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][4U] >> 0x15U))),3);
        bufp->chgBit(oldp+1323,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][4U] >> 0x14U))));
        bufp->chgCData(oldp+1324,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 0xfU))),5);
        bufp->chgBit(oldp+1325,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][4U] >> 0xeU))));
        bufp->chgCData(oldp+1326,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 9U))),5);
        bufp->chgBit(oldp+1327,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][4U] >> 8U))));
        bufp->chgCData(oldp+1328,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 3U))),5);
        bufp->chgCData(oldp+1329,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][3U] 
                                              >> 0x1fU)))),4);
        bufp->chgBit(oldp+1330,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x1eU))));
        bufp->chgIData(oldp+1331,((0x3fffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][3U])),30);
        bufp->chgBit(oldp+1332,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][4U] >> 2U))));
        bufp->chgBit(oldp+1333,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][4U] >> 1U))));
        bufp->chgBit(oldp+1334,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [1U][4U])));
        bufp->chgCData(oldp+1335,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][3U] >> 0x1eU)),2);
        bufp->chgCData(oldp+1336,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0x19U))),5);
        bufp->chgBit(oldp+1337,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x18U))));
        bufp->chgCData(oldp+1338,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][3U] >> 0x16U))),2);
        bufp->chgSData(oldp+1339,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [1U][3U] 
                                             >> 0xcU))),10);
        bufp->chgSData(oldp+1340,((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][3U])),12);
        bufp->chgSData(oldp+1341,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][4U] 
                                               << 0xcU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [1U][3U] 
                                                 >> 0x14U)))),15);
        bufp->chgIData(oldp+1342,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][3U])),20);
        bufp->chgCData(oldp+1343,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][4U])),2);
        bufp->chgSData(oldp+1344,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][3U] 
                                              >> 0xeU))),16);
        bufp->chgSData(oldp+1345,((0x3fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][3U])),14);
        bufp->chgSData(oldp+1346,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][4U] 
                                               << 0xeU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [1U][3U] 
                                                 >> 0x12U)))),15);
        bufp->chgIData(oldp+1347,((0x3ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][3U])),18);
        bufp->chgCData(oldp+1348,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][4U])),3);
        bufp->chgBit(oldp+1349,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [1U][3U] >> 0x1fU)));
        bufp->chgIData(oldp+1350,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               >> 0xcU))),19);
        bufp->chgCData(oldp+1351,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [1U][4U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               >> 0x1dU)))),5);
        bufp->chgCData(oldp+1352,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0x18U))),5);
        bufp->chgCData(oldp+1353,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][3U] >> 0x15U))),3);
        bufp->chgIData(oldp+1354,((0x1fffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][3U])),21);
        bufp->chgCData(oldp+1355,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][2U] >> 0x1eU)),2);
        bufp->chgCData(oldp+1356,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][2U] >> 0x1cU))),2);
        bufp->chgCData(oldp+1357,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][2U] >> 0x1aU))),2);
        bufp->chgBit(oldp+1358,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1359,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x18U))));
        bufp->chgBit(oldp+1360,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x17U))));
        bufp->chgBit(oldp+1361,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x16U))));
        bufp->chgBit(oldp+1362,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x15U))));
        bufp->chgBit(oldp+1363,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x14U))));
        bufp->chgCData(oldp+1364,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][2U] >> 0x12U))),2);
        bufp->chgBit(oldp+1365,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x11U))));
        bufp->chgCData(oldp+1366,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][7U] >> 6U))),3);
        bufp->chgCData(oldp+1367,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][7U] >> 4U))),2);
        bufp->chgCData(oldp+1368,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][7U] >> 1U))),3);
        bufp->chgBit(oldp+1369,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [1U][7U])));
        bufp->chgCData(oldp+1370,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][6U] >> 0x1bU)),5);
        bufp->chgBit(oldp+1371,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 0x1aU))));
        bufp->chgCData(oldp+1372,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][6U] 
                                            >> 0x15U))),5);
        bufp->chgBit(oldp+1373,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 0x14U))));
        bufp->chgCData(oldp+1374,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][6U] 
                                            >> 0xfU))),5);
        bufp->chgCData(oldp+1375,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                           [1U][6U] 
                                           >> 0xbU))),4);
        bufp->chgBit(oldp+1376,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 0xaU))));
        bufp->chgIData(oldp+1377,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [1U][6U] 
                                                   << 0x14U) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                     [1U][5U] 
                                                     >> 0xcU)))),30);
        bufp->chgBit(oldp+1378,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 0xeU))));
        bufp->chgBit(oldp+1379,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 0xdU))));
        bufp->chgBit(oldp+1380,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 0xcU))));
        bufp->chgCData(oldp+1381,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][6U] >> 0xaU))),2);
        bufp->chgCData(oldp+1382,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][6U] 
                                            >> 5U))),5);
        bufp->chgBit(oldp+1383,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 4U))));
        bufp->chgCData(oldp+1384,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][6U] >> 2U))),2);
        bufp->chgSData(oldp+1385,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][6U] 
                                              << 8U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [1U][5U] 
                                                >> 0x18U)))),10);
        bufp->chgSData(oldp+1386,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                             [1U][5U] 
                                             >> 0xcU))),12);
        bufp->chgSData(oldp+1387,((0x7fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][6U])),15);
        bufp->chgIData(oldp+1388,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][5U] >> 0xcU)),20);
        bufp->chgCData(oldp+1389,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][6U] >> 0xcU))),2);
        bufp->chgSData(oldp+1390,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][6U] 
                                               << 6U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [1U][5U] 
                                                 >> 0x1aU)))),16);
        bufp->chgSData(oldp+1391,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                              [1U][5U] 
                                              >> 0xcU))),14);
        bufp->chgSData(oldp+1392,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][6U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [1U][5U] 
                                                 >> 0x1eU)))),15);
        bufp->chgIData(oldp+1393,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                               [1U][5U] 
                                               >> 0xcU))),18);
        bufp->chgCData(oldp+1394,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][6U] >> 0xcU))),3);
        bufp->chgBit(oldp+1395,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][6U] >> 0xbU))));
        bufp->chgIData(oldp+1396,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                [1U][6U] 
                                                << 8U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                  [1U][5U] 
                                                  >> 0x18U)))),19);
        bufp->chgCData(oldp+1397,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][6U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+1398,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                            [1U][6U] 
                                            >> 4U))),5);
        bufp->chgCData(oldp+1399,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][6U] >> 1U))),3);
        bufp->chgIData(oldp+1400,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                 [1U][6U] 
                                                 << 0x14U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                                   [1U][5U] 
                                                   >> 0xcU)))),21);
        bufp->chgCData(oldp+1401,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+1402,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][5U] >> 8U))),2);
        bufp->chgCData(oldp+1403,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                         [1U][5U] >> 6U))),2);
        bufp->chgBit(oldp+1404,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][5U] >> 5U))));
        bufp->chgBit(oldp+1405,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][5U] >> 4U))));
        bufp->chgBit(oldp+1406,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][5U] >> 3U))));
        bufp->chgBit(oldp+1407,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][5U] >> 2U))));
        bufp->chgBit(oldp+1408,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][5U] >> 1U))));
        bufp->chgBit(oldp+1409,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [1U][5U])));
        bufp->chgCData(oldp+1410,((vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                   [1U][4U] >> 0x1eU)),2);
        bufp->chgBit(oldp+1411,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1412,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 4U))));
        bufp->chgBit(oldp+1413,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 3U))));
        bufp->chgBit(oldp+1414,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 2U))));
        bufp->chgBit(oldp+1415,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+1416,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__pdStageIF.__PVT__nextStage
                                 [1U][0U])));
        bufp->chgSData(oldp+1417,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [0U][4U] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1418,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][4U] >> 2U))),2);
        bufp->chgBit(oldp+1419,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][4U] >> 1U))));
        bufp->chgCData(oldp+1420,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                          [0U][4U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                          [0U][3U] 
                                          >> 0x1eU)))),3);
        bufp->chgCData(oldp+1421,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][3U] >> 0x1cU))),2);
        bufp->chgCData(oldp+1422,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][3U] >> 0x19U))),3);
        bufp->chgBit(oldp+1423,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x18U))));
        bufp->chgCData(oldp+1424,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0x13U))),5);
        bufp->chgBit(oldp+1425,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x12U))));
        bufp->chgCData(oldp+1426,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1427,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 0xcU))));
        bufp->chgCData(oldp+1428,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1429,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                           [0U][3U] 
                                           >> 3U))),4);
        bufp->chgBit(oldp+1430,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 2U))));
        bufp->chgIData(oldp+1431,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                   [0U][3U] 
                                                   << 0x1cU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                     [0U][2U] 
                                                     >> 4U)))),30);
        bufp->chgBit(oldp+1432,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 6U))));
        bufp->chgBit(oldp+1433,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 5U))));
        bufp->chgBit(oldp+1434,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 4U))));
        bufp->chgCData(oldp+1435,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][3U] >> 2U))),2);
        bufp->chgCData(oldp+1436,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [0U][3U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][2U] 
                                               >> 0x1dU)))),5);
        bufp->chgBit(oldp+1437,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x1cU))));
        bufp->chgCData(oldp+1438,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][2U] >> 0x1aU))),2);
        bufp->chgSData(oldp+1439,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [0U][2U] 
                                             >> 0x10U))),10);
        bufp->chgSData(oldp+1440,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [0U][2U] 
                                             >> 4U))),12);
        bufp->chgSData(oldp+1441,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                 [0U][2U] 
                                                 >> 0x18U)))),15);
        bufp->chgIData(oldp+1442,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][2U] 
                                               >> 4U))),20);
        bufp->chgCData(oldp+1443,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][3U] >> 4U))),2);
        bufp->chgSData(oldp+1444,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               << 0xeU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                 [0U][2U] 
                                                 >> 0x12U)))),16);
        bufp->chgSData(oldp+1445,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                              [0U][2U] 
                                              >> 4U))),14);
        bufp->chgSData(oldp+1446,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][3U] 
                                               << 0xaU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                 [0U][2U] 
                                                 >> 0x16U)))),15);
        bufp->chgIData(oldp+1447,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][2U] 
                                               >> 4U))),18);
        bufp->chgCData(oldp+1448,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][3U] >> 4U))),3);
        bufp->chgBit(oldp+1449,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][3U] >> 3U))));
        bufp->chgIData(oldp+1450,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                [0U][3U] 
                                                << 0x10U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                  [0U][2U] 
                                                  >> 0x10U)))),19);
        bufp->chgCData(oldp+1451,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 1U))),5);
        bufp->chgCData(oldp+1452,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [0U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][2U] 
                                               >> 0x1cU)))),5);
        bufp->chgCData(oldp+1453,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][2U] >> 0x19U))),3);
        bufp->chgIData(oldp+1454,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                [0U][2U] 
                                                >> 4U))),21);
        bufp->chgCData(oldp+1455,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+1456,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [0U][2U])),2);
        bufp->chgCData(oldp+1457,((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [0U][1U] >> 0x1eU)),2);
        bufp->chgBit(oldp+1458,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x1dU))));
        bufp->chgBit(oldp+1459,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x1cU))));
        bufp->chgBit(oldp+1460,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x1bU))));
        bufp->chgBit(oldp+1461,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x1aU))));
        bufp->chgBit(oldp+1462,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x19U))));
        bufp->chgBit(oldp+1463,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x18U))));
        bufp->chgCData(oldp+1464,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [0U][1U] >> 0x16U))),2);
        bufp->chgBit(oldp+1465,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x15U))));
        bufp->chgBit(oldp+1466,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x14U))));
        bufp->chgIData(oldp+1467,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [0U][1U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1468,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                 [0U][1U])));
        bufp->chgIData(oldp+1469,((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [0U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+1470,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xcU))));
        bufp->chgSData(oldp+1471,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [0U][0U] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+1472,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [0U][0U])),2);
        bufp->chgSData(oldp+1473,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [1U][4U] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1474,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][4U] >> 2U))),2);
        bufp->chgBit(oldp+1475,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][4U] >> 1U))));
        bufp->chgCData(oldp+1476,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                          [1U][4U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                          [1U][3U] 
                                          >> 0x1eU)))),3);
        bufp->chgCData(oldp+1477,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][3U] >> 0x1cU))),2);
        bufp->chgCData(oldp+1478,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][3U] >> 0x19U))),3);
        bufp->chgBit(oldp+1479,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x18U))));
        bufp->chgCData(oldp+1480,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0x13U))),5);
        bufp->chgBit(oldp+1481,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x12U))));
        bufp->chgCData(oldp+1482,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1483,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 0xcU))));
        bufp->chgCData(oldp+1484,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 7U))),5);
        bufp->chgCData(oldp+1485,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                           [1U][3U] 
                                           >> 3U))),4);
        bufp->chgBit(oldp+1486,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 2U))));
        bufp->chgIData(oldp+1487,((0x3fffffffU & ((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                   [1U][3U] 
                                                   << 0x1cU) 
                                                  | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                     [1U][2U] 
                                                     >> 4U)))),30);
        bufp->chgBit(oldp+1488,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 6U))));
        bufp->chgBit(oldp+1489,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 5U))));
        bufp->chgBit(oldp+1490,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 4U))));
        bufp->chgCData(oldp+1491,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][3U] >> 2U))),2);
        bufp->chgCData(oldp+1492,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [1U][3U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][2U] 
                                               >> 0x1dU)))),5);
        bufp->chgBit(oldp+1493,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x1cU))));
        bufp->chgCData(oldp+1494,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][2U] >> 0x1aU))),2);
        bufp->chgSData(oldp+1495,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [1U][2U] 
                                             >> 0x10U))),10);
        bufp->chgSData(oldp+1496,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [1U][2U] 
                                             >> 4U))),12);
        bufp->chgSData(oldp+1497,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               << 8U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                 [1U][2U] 
                                                 >> 0x18U)))),15);
        bufp->chgIData(oldp+1498,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][2U] 
                                               >> 4U))),20);
        bufp->chgCData(oldp+1499,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][3U] >> 4U))),2);
        bufp->chgSData(oldp+1500,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               << 0xeU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                 [1U][2U] 
                                                 >> 0x12U)))),16);
        bufp->chgSData(oldp+1501,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                              [1U][2U] 
                                              >> 4U))),14);
        bufp->chgSData(oldp+1502,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][3U] 
                                               << 0xaU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                 [1U][2U] 
                                                 >> 0x16U)))),15);
        bufp->chgIData(oldp+1503,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][2U] 
                                               >> 4U))),18);
        bufp->chgCData(oldp+1504,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][3U] >> 4U))),3);
        bufp->chgBit(oldp+1505,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][3U] >> 3U))));
        bufp->chgIData(oldp+1506,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                [1U][3U] 
                                                << 0x10U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                  [1U][2U] 
                                                  >> 0x10U)))),19);
        bufp->chgCData(oldp+1507,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 1U))),5);
        bufp->chgCData(oldp+1508,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [1U][3U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][2U] 
                                               >> 0x1cU)))),5);
        bufp->chgCData(oldp+1509,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][2U] >> 0x19U))),3);
        bufp->chgIData(oldp+1510,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                                [1U][2U] 
                                                >> 4U))),21);
        bufp->chgCData(oldp+1511,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][2U] >> 2U))),2);
        bufp->chgCData(oldp+1512,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [1U][2U])),2);
        bufp->chgCData(oldp+1513,((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [1U][1U] >> 0x1eU)),2);
        bufp->chgBit(oldp+1514,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x1dU))));
        bufp->chgBit(oldp+1515,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x1cU))));
        bufp->chgBit(oldp+1516,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x1bU))));
        bufp->chgBit(oldp+1517,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x1aU))));
        bufp->chgBit(oldp+1518,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x19U))));
        bufp->chgBit(oldp+1519,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x18U))));
        bufp->chgCData(oldp+1520,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                         [1U][1U] >> 0x16U))),2);
        bufp->chgBit(oldp+1521,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x15U))));
        bufp->chgBit(oldp+1522,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x14U))));
        bufp->chgIData(oldp+1523,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                               [1U][1U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1524,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                 [1U][1U])));
        bufp->chgIData(oldp+1525,((vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [1U][0U] >> 0xdU)),19);
        bufp->chgBit(oldp+1526,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xcU))));
        bufp->chgSData(oldp+1527,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                             [1U][0U] 
                                             >> 2U))),10);
        bufp->chgCData(oldp+1528,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__idStageIF.__PVT__nextStage
                                   [1U][0U])),2);
        bufp->chgBit(oldp+1529,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [0U][2U] >> 0x10U))));
        bufp->chgBit(oldp+1530,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [0U][2U] >> 0xfU))));
        bufp->chgBit(oldp+1531,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [0U][2U] >> 0xeU))));
        bufp->chgSData(oldp+1532,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                             [0U][2U] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1533,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                         [0U][2U] >> 2U))),2);
        bufp->chgIData(oldp+1534,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                    [0U][2U] << 0x1eU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                      [0U][1U] >> 2U))),32);
        bufp->chgIData(oldp+1535,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                    [0U][1U] << 0x1eU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                      [0U][0U] >> 2U))),32);
        bufp->chgBit(oldp+1536,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+1537,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                 [0U][0U])));
        bufp->chgBit(oldp+1538,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [1U][2U] >> 0x10U))));
        bufp->chgBit(oldp+1539,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [1U][2U] >> 0xfU))));
        bufp->chgBit(oldp+1540,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [1U][2U] >> 0xeU))));
        bufp->chgSData(oldp+1541,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                             [1U][2U] 
                                             >> 4U))),10);
        bufp->chgCData(oldp+1542,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                         [1U][2U] >> 2U))),2);
        bufp->chgIData(oldp+1543,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                    [1U][2U] << 0x1eU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                      [1U][1U] >> 2U))),32);
        bufp->chgIData(oldp+1544,(((vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                    [1U][1U] << 0x1eU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                      [1U][0U] >> 2U))),32);
        bufp->chgBit(oldp+1545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                       [1U][0U] >> 1U))));
        bufp->chgBit(oldp+1546,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
                                 [1U][0U])));
        bufp->chgBit(oldp+1547,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdStagePipeCtrl) 
                                       >> 1U))));
        bufp->chgBit(oldp+1548,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdStagePipeCtrl))));
        bufp->chgBit(oldp+1549,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idStagePipeCtrl) 
                                       >> 1U))));
        bufp->chgBit(oldp+1550,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idStagePipeCtrl))));
        bufp->chgBit(oldp+1551,(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__stallByDecodeStage));
        bufp->chgBit(oldp+1552,(vlSymsp->TOP__SMT_RTL_Testbench__core__perfCounterIF.__PVT__branchPredMissDetectedOnDecode));
        bufp->chgBit(oldp+1553,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1554,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__pdStage))));
        bufp->chgBit(oldp+1555,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1556,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStage))));
        bufp->chgBit(oldp+1557,(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__idStageStallUpper));
        bufp->chgBit(oldp+1558,(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__stallByDecodeStage));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x1aU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x64U])))) {
        bufp->chgBit(oldp+1559,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__ifStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1560,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__ifStage))));
        bufp->chgBit(oldp+1561,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage))));
        bufp->chgBit(oldp+1562,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifStagePipeCtrl) 
                                       >> 1U))));
        bufp->chgBit(oldp+1563,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifStagePipeCtrl))));
        bufp->chgBit(oldp+1564,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__ifStage) 
                                       >> 1U))));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x1bU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x65U])))) {
        bufp->chgBit(oldp+1565,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__npStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1566,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__npStage))));
        bufp->chgBit(oldp+1567,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStage))));
        bufp->chgBit(oldp+1568,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npStagePipeCtrl) 
                                       >> 1U))));
        bufp->chgBit(oldp+1569,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npStagePipeCtrl))));
        bufp->chgBit(oldp+1570,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__npStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1571,(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__wholePipelineEmpty));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x1cU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x66U])))) {
        bufp->chgBit(oldp+1572,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__rnStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1573,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__controller__DOT__rnStage))));
        bufp->chgBit(oldp+1574,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage) 
                                       >> 1U))));
        bufp->chgBit(oldp+1575,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStage))));
        bufp->chgCData(oldp+1576,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__serializer__DOT__nextPhase),2);
        bufp->chgBit(oldp+1577,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnStagePipeCtrl) 
                                       >> 1U))));
        bufp->chgBit(oldp+1578,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnStagePipeCtrl))));
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x1eU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x67U])))) {
        bufp->chgIData(oldp+1579,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__immOut[0]),32);
        bufp->chgIData(oldp+1580,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__immOut[1]),32);
        bufp->chgIData(oldp+1581,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__pc[0]),32);
        bufp->chgIData(oldp+1582,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__pc[1]),32);
        bufp->chgBit(oldp+1583,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1584,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandA
                                          [0U])),32);
        bufp->chgBit(oldp+1585,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandA
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1586,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandA
                                          [1U])),32);
        bufp->chgBit(oldp+1587,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1588,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandB
                                          [0U])),32);
        bufp->chgBit(oldp+1589,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandB
                                               [1U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+1590,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__operandB
                                          [1U])),32);
        bufp->chgBit(oldp+1591,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__stall));
        bufp->chgBit(oldp+1592,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__clear));
        bufp->chgBit(oldp+1593,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__flush[0]));
        bufp->chgBit(oldp+1594,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__flush[1]));
        bufp->chgSData(oldp+1595,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                             [0U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1596,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+1597,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+1598,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+1599,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][3U] >> 9U))),2);
        bufp->chgCData(oldp+1600,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][3U] >> 7U))),2);
        bufp->chgSData(oldp+1601,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                                [0U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+1602,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1603,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1604,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][2U] >> 0x18U))));
        bufp->chgCData(oldp+1605,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+1606,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [0U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+1607,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][2U] >> 0x10U))));
        bufp->chgCData(oldp+1608,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+1609,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [0U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+1610,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][2U] >> 0xaU))));
        bufp->chgCData(oldp+1611,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [0U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+1612,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [0U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+1613,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][2U] >> 1U))));
        bufp->chgBit(oldp+1614,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                 [0U][2U])));
        bufp->chgCData(oldp+1615,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1616,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1617,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1618,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+1619,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1620,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+1621,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1622,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+1623,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1624,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+1625,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+1626,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1627,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1628,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1629,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                 [0U][0U])));
        bufp->chgSData(oldp+1630,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                             [1U][3U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1631,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][3U] >> 0x11U))),2);
        bufp->chgCData(oldp+1632,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][3U] >> 0xeU))),3);
        bufp->chgCData(oldp+1633,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][3U] >> 0xbU))),3);
        bufp->chgCData(oldp+1634,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][3U] >> 9U))),2);
        bufp->chgCData(oldp+1635,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][3U] >> 7U))),2);
        bufp->chgSData(oldp+1636,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                              [1U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                                [1U][2U] 
                                                >> 0x1bU)))),12);
        bufp->chgBit(oldp+1637,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][2U] >> 0x1aU))));
        bufp->chgBit(oldp+1638,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][2U] >> 0x19U))));
        bufp->chgBit(oldp+1639,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][2U] >> 0x18U))));
        bufp->chgCData(oldp+1640,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][2U] >> 0x16U))),2);
        bufp->chgCData(oldp+1641,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [1U][2U] 
                                            >> 0x11U))),5);
        bufp->chgBit(oldp+1642,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][2U] >> 0x10U))));
        bufp->chgCData(oldp+1643,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][2U] >> 0xeU))),2);
        bufp->chgCData(oldp+1644,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                         [1U][2U] >> 0xbU))),3);
        bufp->chgBit(oldp+1645,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][2U] >> 0xaU))));
        bufp->chgCData(oldp+1646,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [1U][2U] 
                                           >> 6U))),4);
        bufp->chgCData(oldp+1647,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [1U][2U] 
                                           >> 2U))),4);
        bufp->chgBit(oldp+1648,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][2U] >> 1U))));
        bufp->chgBit(oldp+1649,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                 [1U][2U])));
        bufp->chgCData(oldp+1650,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                   [1U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+1651,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [1U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+1652,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                           [1U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+1653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][1U] >> 0x11U))));
        bufp->chgCData(oldp+1654,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [1U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+1655,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][1U] >> 0xaU))));
        bufp->chgCData(oldp+1656,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [1U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+1657,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][1U] >> 3U))));
        bufp->chgCData(oldp+1658,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                             [1U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                               [1U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+1659,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+1660,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+1661,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                            [1U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+1662,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                       [1U][0U] >> 0x14U))));
        bufp->chgIData(oldp+1663,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                               [1U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+1664,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__iqData
                                 [1U][0U])));
        bufp->chgCData(oldp+1665,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x24U)))),3);
        bufp->chgCData(oldp+1666,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x21U)))),3);
        bufp->chgCData(oldp+1667,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x1fU)))),2);
        bufp->chgCData(oldp+1668,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0x1dU)))),2);
        bufp->chgSData(oldp+1669,((0xfffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                     [0U] 
                                                     >> 0x11U)))),12);
        bufp->chgBit(oldp+1670,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [0U] 
                                               >> 0x10U)))));
        bufp->chgBit(oldp+1671,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [0U] 
                                               >> 0xfU)))));
        bufp->chgBit(oldp+1672,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [0U] 
                                               >> 0xeU)))));
        bufp->chgCData(oldp+1673,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 0xcU)))),2);
        bufp->chgCData(oldp+1674,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                    [0U] 
                                                    >> 7U)))),5);
        bufp->chgBit(oldp+1675,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [0U] 
                                               >> 6U)))));
        bufp->chgCData(oldp+1676,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 4U)))),2);
        bufp->chgCData(oldp+1677,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [0U] 
                                                 >> 1U)))),3);
        bufp->chgBit(oldp+1678,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                              [0U]))));
        bufp->chgCData(oldp+1679,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x24U)))),3);
        bufp->chgCData(oldp+1680,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x21U)))),3);
        bufp->chgCData(oldp+1681,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x1fU)))),2);
        bufp->chgCData(oldp+1682,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0x1dU)))),2);
        bufp->chgSData(oldp+1683,((0xfffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                     [1U] 
                                                     >> 0x11U)))),12);
        bufp->chgBit(oldp+1684,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [1U] 
                                               >> 0x10U)))));
        bufp->chgBit(oldp+1685,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [1U] 
                                               >> 0xfU)))));
        bufp->chgBit(oldp+1686,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [1U] 
                                               >> 0xeU)))));
        bufp->chgCData(oldp+1687,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 0xcU)))),2);
        bufp->chgCData(oldp+1688,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                    [1U] 
                                                    >> 7U)))),5);
        bufp->chgBit(oldp+1689,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                               [1U] 
                                               >> 6U)))));
        bufp->chgCData(oldp+1690,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 4U)))),2);
        bufp->chgCData(oldp+1691,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                                 [1U] 
                                                 >> 1U)))),3);
        bufp->chgBit(oldp+1692,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__memOpInfo
                                              [1U]))));
        bufp->chgBit(oldp+1693,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                       [0U] >> 0x14U))));
        bufp->chgCData(oldp+1694,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                            [0U] >> 0xeU))),6);
        bufp->chgBit(oldp+1695,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                       [0U] >> 0xdU))));
        bufp->chgCData(oldp+1696,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                            [0U] >> 7U))),6);
        bufp->chgBit(oldp+1697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1698,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                   [0U])),6);
        bufp->chgBit(oldp+1699,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                       [1U] >> 0x14U))));
        bufp->chgCData(oldp+1700,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                            [1U] >> 0xeU))),6);
        bufp->chgBit(oldp+1701,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                       [1U] >> 0xdU))));
        bufp->chgCData(oldp+1702,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                            [1U] >> 7U))),6);
        bufp->chgBit(oldp+1703,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1704,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opSrc
                                   [1U])),6);
        bufp->chgBit(oldp+1705,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opDst
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+1706,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opDst
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1707,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opDst
                                   [0U])),6);
        bufp->chgBit(oldp+1708,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opDst
                                       [1U] >> 7U))));
        bufp->chgBit(oldp+1709,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opDst
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1710,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__opDst
                                   [1U])),6);
        bufp->chgSData(oldp+1711,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                              [0U][7U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                [0U][6U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1712,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][6U] >> 0x1aU))),2);
        bufp->chgBit(oldp+1713,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][6U] >> 0x19U))));
        bufp->chgSData(oldp+1714,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                             [0U][6U] 
                                             >> 0xfU))),10);
        bufp->chgCData(oldp+1715,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][6U] >> 0xdU))),2);
        bufp->chgCData(oldp+1716,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][6U] >> 0xaU))),3);
        bufp->chgCData(oldp+1717,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][6U] >> 7U))),3);
        bufp->chgCData(oldp+1718,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][6U] >> 5U))),2);
        bufp->chgCData(oldp+1719,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][6U] >> 3U))),2);
        bufp->chgSData(oldp+1720,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                              [0U][6U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                [0U][5U] 
                                                >> 0x17U)))),12);
        bufp->chgBit(oldp+1721,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][5U] >> 0x16U))));
        bufp->chgBit(oldp+1722,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][5U] >> 0x15U))));
        bufp->chgBit(oldp+1723,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][5U] >> 0x14U))));
        bufp->chgCData(oldp+1724,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][5U] >> 0x12U))),2);
        bufp->chgCData(oldp+1725,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1726,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][5U] >> 0xcU))));
        bufp->chgCData(oldp+1727,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+1728,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][5U] >> 7U))),3);
        bufp->chgBit(oldp+1729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][5U] >> 6U))));
        bufp->chgCData(oldp+1730,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [0U][5U] 
                                           >> 2U))),4);
        bufp->chgCData(oldp+1731,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [0U][5U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                              [0U][4U] 
                                              >> 0x1eU)))),4);
        bufp->chgBit(oldp+1732,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1733,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][4U] >> 0x1cU))));
        bufp->chgCData(oldp+1734,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 0x16U))),6);
        bufp->chgCData(oldp+1735,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0x12U))),4);
        bufp->chgCData(oldp+1736,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [0U][4U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1737,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][4U] >> 0xdU))));
        bufp->chgCData(oldp+1738,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1739,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][4U] >> 6U))));
        bufp->chgCData(oldp+1740,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                   [0U][4U])),6);
        bufp->chgBit(oldp+1741,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                 [0U][3U] >> 0x1fU)));
        bufp->chgCData(oldp+1742,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 0x19U))),6);
        bufp->chgBit(oldp+1743,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][3U] >> 0x18U))));
        bufp->chgBit(oldp+1744,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][3U] >> 0x17U))));
        bufp->chgCData(oldp+1745,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [0U][3U] 
                                            >> 0x11U))),6);
        bufp->chgBit(oldp+1746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][3U] >> 0x10U))));
        bufp->chgIData(oldp+1747,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                [0U][3U] 
                                                << 3U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                  [0U][2U] 
                                                  >> 0x1dU)))),19);
        bufp->chgBit(oldp+1748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][2U] >> 0x1cU))));
        bufp->chgBit(oldp+1749,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][2U] >> 0x1bU))));
        bufp->chgIData(oldp+1750,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                    [0U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                      [0U][1U] >> 0x1bU))),32);
        bufp->chgBit(oldp+1751,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][1U] >> 0x1aU))));
        bufp->chgIData(oldp+1752,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                    [0U][1U] << 6U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                      [0U][0U] >> 0x1aU))),32);
        bufp->chgBit(oldp+1753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0x19U))));
        bufp->chgCData(oldp+1754,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][0U] >> 0x17U))),2);
        bufp->chgBit(oldp+1755,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0x16U))));
        bufp->chgBit(oldp+1756,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1757,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0x14U))));
        bufp->chgBit(oldp+1758,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0x13U))));
        bufp->chgBit(oldp+1759,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0x12U))));
        bufp->chgCData(oldp+1760,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][0U] >> 0x10U))),2);
        bufp->chgBit(oldp+1761,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+1762,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+1763,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0xdU))));
        bufp->chgBit(oldp+1764,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0xcU))));
        bufp->chgBit(oldp+1765,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 0xbU))));
        bufp->chgCData(oldp+1766,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [0U][0U] >> 9U))),2);
        bufp->chgBit(oldp+1767,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 8U))));
        bufp->chgBit(oldp+1768,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 7U))));
        bufp->chgBit(oldp+1769,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 6U))));
        bufp->chgBit(oldp+1770,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [0U][0U] >> 5U))));
        bufp->chgCData(oldp+1771,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [0U][0U] 
                                           >> 1U))),4);
        bufp->chgBit(oldp+1772,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+1773,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                              [1U][7U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                [1U][6U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1774,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][6U] >> 0x1aU))),2);
        bufp->chgBit(oldp+1775,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][6U] >> 0x19U))));
        bufp->chgSData(oldp+1776,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                             [1U][6U] 
                                             >> 0xfU))),10);
        bufp->chgCData(oldp+1777,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][6U] >> 0xdU))),2);
        bufp->chgCData(oldp+1778,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][6U] >> 0xaU))),3);
        bufp->chgCData(oldp+1779,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][6U] >> 7U))),3);
        bufp->chgCData(oldp+1780,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][6U] >> 5U))),2);
        bufp->chgCData(oldp+1781,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][6U] >> 3U))),2);
        bufp->chgSData(oldp+1782,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                              [1U][6U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                [1U][5U] 
                                                >> 0x17U)))),12);
        bufp->chgBit(oldp+1783,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][5U] >> 0x16U))));
        bufp->chgBit(oldp+1784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][5U] >> 0x15U))));
        bufp->chgBit(oldp+1785,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][5U] >> 0x14U))));
        bufp->chgCData(oldp+1786,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][5U] >> 0x12U))),2);
        bufp->chgCData(oldp+1787,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [1U][5U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1788,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][5U] >> 0xcU))));
        bufp->chgCData(oldp+1789,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+1790,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][5U] >> 7U))),3);
        bufp->chgBit(oldp+1791,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][5U] >> 6U))));
        bufp->chgCData(oldp+1792,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [1U][5U] 
                                           >> 2U))),4);
        bufp->chgCData(oldp+1793,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [1U][5U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                              [1U][4U] 
                                              >> 0x1eU)))),4);
        bufp->chgBit(oldp+1794,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1795,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][4U] >> 0x1cU))));
        bufp->chgCData(oldp+1796,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 0x16U))),6);
        bufp->chgCData(oldp+1797,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0x12U))),4);
        bufp->chgCData(oldp+1798,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [1U][4U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1799,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][4U] >> 0xdU))));
        bufp->chgCData(oldp+1800,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [1U][4U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1801,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][4U] >> 6U))));
        bufp->chgCData(oldp+1802,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                   [1U][4U])),6);
        bufp->chgBit(oldp+1803,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                 [1U][3U] >> 0x1fU)));
        bufp->chgCData(oldp+1804,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [1U][3U] 
                                            >> 0x19U))),6);
        bufp->chgBit(oldp+1805,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][3U] >> 0x18U))));
        bufp->chgBit(oldp+1806,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][3U] >> 0x17U))));
        bufp->chgCData(oldp+1807,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                            [1U][3U] 
                                            >> 0x11U))),6);
        bufp->chgBit(oldp+1808,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][3U] >> 0x10U))));
        bufp->chgIData(oldp+1809,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                [1U][3U] 
                                                << 3U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                                  [1U][2U] 
                                                  >> 0x1dU)))),19);
        bufp->chgBit(oldp+1810,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][2U] >> 0x1cU))));
        bufp->chgBit(oldp+1811,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][2U] >> 0x1bU))));
        bufp->chgIData(oldp+1812,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                    [1U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                      [1U][1U] >> 0x1bU))),32);
        bufp->chgBit(oldp+1813,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][1U] >> 0x1aU))));
        bufp->chgIData(oldp+1814,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                    [1U][1U] << 6U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                      [1U][0U] >> 0x1aU))),32);
        bufp->chgBit(oldp+1815,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0x19U))));
        bufp->chgCData(oldp+1816,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][0U] >> 0x17U))),2);
        bufp->chgBit(oldp+1817,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0x16U))));
        bufp->chgBit(oldp+1818,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1819,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0x14U))));
        bufp->chgBit(oldp+1820,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0x13U))));
        bufp->chgBit(oldp+1821,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0x12U))));
        bufp->chgCData(oldp+1822,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][0U] >> 0x10U))),2);
        bufp->chgBit(oldp+1823,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0xfU))));
        bufp->chgBit(oldp+1824,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0xeU))));
        bufp->chgBit(oldp+1825,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0xdU))));
        bufp->chgBit(oldp+1826,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0xcU))));
        bufp->chgBit(oldp+1827,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 0xbU))));
        bufp->chgCData(oldp+1828,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                         [1U][0U] >> 9U))),2);
        bufp->chgBit(oldp+1829,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 8U))));
        bufp->chgBit(oldp+1830,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 7U))));
        bufp->chgBit(oldp+1831,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 6U))));
        bufp->chgBit(oldp+1832,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                       [1U][0U] >> 5U))));
        bufp->chgCData(oldp+1833,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                           [1U][0U] 
                                           >> 1U))),4);
        bufp->chgBit(oldp+1834,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__nextStage
                                 [1U][0U])));
        bufp->chgIData(oldp+1835,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+1836,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgSData(oldp+1837,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                              [0U][7U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                [0U][6U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1838,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 0x1aU))),2);
        bufp->chgBit(oldp+1839,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][6U] >> 0x19U))));
        bufp->chgSData(oldp+1840,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                             [0U][6U] 
                                             >> 0xfU))),10);
        bufp->chgCData(oldp+1841,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 0xdU))),2);
        bufp->chgCData(oldp+1842,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 0xaU))),3);
        bufp->chgCData(oldp+1843,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 7U))),3);
        bufp->chgCData(oldp+1844,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 5U))),2);
        bufp->chgCData(oldp+1845,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 3U))),2);
        bufp->chgSData(oldp+1846,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                              [0U][6U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                [0U][5U] 
                                                >> 0x17U)))),12);
        bufp->chgBit(oldp+1847,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][5U] >> 0x16U))));
        bufp->chgBit(oldp+1848,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][5U] >> 0x15U))));
        bufp->chgBit(oldp+1849,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][5U] >> 0x14U))));
        bufp->chgCData(oldp+1850,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][5U] >> 0x12U))),2);
        bufp->chgCData(oldp+1851,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1852,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][5U] >> 0xcU))));
        bufp->chgCData(oldp+1853,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+1854,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][5U] >> 7U))),3);
        bufp->chgBit(oldp+1855,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][5U] >> 6U))));
        bufp->chgCData(oldp+1856,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 2U))),4);
        bufp->chgCData(oldp+1857,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                              [0U][4U] 
                                              >> 0x1eU)))),4);
        bufp->chgBit(oldp+1858,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1859,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x1cU))));
        bufp->chgCData(oldp+1860,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 0x16U))),6);
        bufp->chgCData(oldp+1861,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0x12U))),4);
        bufp->chgCData(oldp+1862,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [0U][4U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1863,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 0xdU))));
        bufp->chgCData(oldp+1864,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1865,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 6U))));
        bufp->chgCData(oldp+1866,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                   [0U][4U])),6);
        bufp->chgBit(oldp+1867,((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                 [0U][3U] >> 0x1fU)));
        bufp->chgCData(oldp+1868,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0x19U))),6);
        bufp->chgBit(oldp+1869,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x18U))));
        bufp->chgBit(oldp+1870,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x17U))));
        bufp->chgCData(oldp+1871,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [0U][3U] 
                                            >> 0x11U))),6);
        bufp->chgBit(oldp+1872,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x10U))));
        bufp->chgIData(oldp+1873,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                [0U][3U] 
                                                << 3U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                  [0U][2U] 
                                                  >> 0x1dU)))),19);
        bufp->chgBit(oldp+1874,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x1cU))));
        bufp->chgBit(oldp+1875,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x1bU))));
        bufp->chgIData(oldp+1876,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                    [0U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x1bU))),32);
        bufp->chgBit(oldp+1877,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x1aU))));
        bufp->chgIData(oldp+1878,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                    [0U][1U] << 6U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x1aU))),32);
        bufp->chgBit(oldp+1879,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x19U))));
        bufp->chgCData(oldp+1880,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][0U] >> 0x17U))),2);
        bufp->chgBit(oldp+1881,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x16U))));
        bufp->chgBit(oldp+1882,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1883,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x14U))));
        bufp->chgBit(oldp+1884,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x13U))));
        bufp->chgBit(oldp+1885,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x12U))));
        bufp->chgCData(oldp+1886,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][0U] >> 0x10U))),2);
        bufp->chgBit(oldp+1887,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+1888,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+1889,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xdU))));
        bufp->chgBit(oldp+1890,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xcU))));
        bufp->chgBit(oldp+1891,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xbU))));
        bufp->chgCData(oldp+1892,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [0U][0U] >> 9U))),2);
        bufp->chgBit(oldp+1893,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 8U))));
        bufp->chgBit(oldp+1894,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 7U))));
        bufp->chgBit(oldp+1895,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 6U))));
        bufp->chgBit(oldp+1896,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 5U))));
        bufp->chgCData(oldp+1897,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [0U][0U] 
                                           >> 1U))),4);
        bufp->chgBit(oldp+1898,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                 [0U][0U])));
        bufp->chgSData(oldp+1899,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                              [1U][7U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                [1U][6U] 
                                                >> 0x1cU)))),10);
        bufp->chgCData(oldp+1900,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][6U] >> 0x1aU))),2);
        bufp->chgBit(oldp+1901,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][6U] >> 0x19U))));
        bufp->chgSData(oldp+1902,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                             [1U][6U] 
                                             >> 0xfU))),10);
        bufp->chgCData(oldp+1903,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][6U] >> 0xdU))),2);
        bufp->chgCData(oldp+1904,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][6U] >> 0xaU))),3);
        bufp->chgCData(oldp+1905,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][6U] >> 7U))),3);
        bufp->chgCData(oldp+1906,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][6U] >> 5U))),2);
        bufp->chgCData(oldp+1907,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][6U] >> 3U))),2);
        bufp->chgSData(oldp+1908,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                              [1U][6U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                [1U][5U] 
                                                >> 0x17U)))),12);
        bufp->chgBit(oldp+1909,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][5U] >> 0x16U))));
        bufp->chgBit(oldp+1910,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][5U] >> 0x15U))));
        bufp->chgBit(oldp+1911,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][5U] >> 0x14U))));
        bufp->chgCData(oldp+1912,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][5U] >> 0x12U))),2);
        bufp->chgCData(oldp+1913,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [1U][5U] 
                                            >> 0xdU))),5);
        bufp->chgBit(oldp+1914,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][5U] >> 0xcU))));
        bufp->chgCData(oldp+1915,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][5U] >> 0xaU))),2);
        bufp->chgCData(oldp+1916,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][5U] >> 7U))),3);
        bufp->chgBit(oldp+1917,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][5U] >> 6U))));
        bufp->chgCData(oldp+1918,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [1U][5U] 
                                           >> 2U))),4);
        bufp->chgCData(oldp+1919,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [1U][5U] 
                                            << 2U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                              [1U][4U] 
                                              >> 0x1eU)))),4);
        bufp->chgBit(oldp+1920,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][4U] >> 0x1dU))));
        bufp->chgBit(oldp+1921,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][4U] >> 0x1cU))));
        bufp->chgCData(oldp+1922,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 0x16U))),6);
        bufp->chgCData(oldp+1923,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 0x12U))),4);
        bufp->chgCData(oldp+1924,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [1U][4U] 
                                           >> 0xeU))),4);
        bufp->chgBit(oldp+1925,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][4U] >> 0xdU))));
        bufp->chgCData(oldp+1926,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [1U][4U] 
                                            >> 7U))),6);
        bufp->chgBit(oldp+1927,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][4U] >> 6U))));
        bufp->chgCData(oldp+1928,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                   [1U][4U])),6);
        bufp->chgBit(oldp+1929,((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                 [1U][3U] >> 0x1fU)));
        bufp->chgCData(oldp+1930,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0x19U))),6);
        bufp->chgBit(oldp+1931,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x18U))));
        bufp->chgBit(oldp+1932,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x17U))));
        bufp->chgCData(oldp+1933,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                            [1U][3U] 
                                            >> 0x11U))),6);
        bufp->chgBit(oldp+1934,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][3U] >> 0x10U))));
        bufp->chgIData(oldp+1935,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                [1U][3U] 
                                                << 3U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                                  [1U][2U] 
                                                  >> 0x1dU)))),19);
        bufp->chgBit(oldp+1936,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x1cU))));
        bufp->chgBit(oldp+1937,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][2U] >> 0x1bU))));
        bufp->chgIData(oldp+1938,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                    [1U][2U] << 5U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                      [1U][1U] >> 0x1bU))),32);
        bufp->chgBit(oldp+1939,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][1U] >> 0x1aU))));
        bufp->chgIData(oldp+1940,(((vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                    [1U][1U] << 6U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                      [1U][0U] >> 0x1aU))),32);
        bufp->chgBit(oldp+1941,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x19U))));
        bufp->chgCData(oldp+1942,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][0U] >> 0x17U))),2);
        bufp->chgBit(oldp+1943,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x16U))));
        bufp->chgBit(oldp+1944,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x15U))));
        bufp->chgBit(oldp+1945,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x14U))));
        bufp->chgBit(oldp+1946,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x13U))));
        bufp->chgBit(oldp+1947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0x12U))));
        bufp->chgCData(oldp+1948,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][0U] >> 0x10U))),2);
        bufp->chgBit(oldp+1949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xfU))));
        bufp->chgBit(oldp+1950,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xeU))));
        bufp->chgBit(oldp+1951,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xdU))));
        bufp->chgBit(oldp+1952,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xcU))));
        bufp->chgBit(oldp+1953,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 0xbU))));
        bufp->chgCData(oldp+1954,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                         [1U][0U] >> 9U))),2);
        bufp->chgBit(oldp+1955,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 8U))));
        bufp->chgBit(oldp+1956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 7U))));
        bufp->chgBit(oldp+1957,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 6U))));
        bufp->chgBit(oldp+1958,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                       [1U][0U] >> 5U))));
        bufp->chgCData(oldp+1959,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                           [1U][0U] 
                                           >> 1U))),4);
        bufp->chgBit(oldp+1960,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__memRrStageIF.__PVT__nextStage
                                 [1U][0U])));
        bufp->chgBit(oldp+1961,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1962,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA
                                   [0U])),6);
        bufp->chgBit(oldp+1963,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1964,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumA
                                   [1U])),6);
        bufp->chgBit(oldp+1965,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1966,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                   [0U])),6);
        bufp->chgBit(oldp+1967,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1968,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memSrcRegNumB
                                   [1U])),6);
        bufp->chgBit(oldp+1969,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1970,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA
                                   [0U])),6);
        bufp->chgBit(oldp+1971,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1972,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumA
                                   [1U])),6);
        bufp->chgBit(oldp+1973,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1974,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB
                                   [0U])),6);
        bufp->chgBit(oldp+1975,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1976,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhySrcRegNumB
                                   [1U])),6);
        bufp->chgBit(oldp+1977,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+1978,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+1979,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum
                                       [1U] >> 6U))));
        bufp->chgCData(oldp+1980,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memPhyDstRegNum
                                   [1U])),6);
        bufp->chgBit(oldp+1981,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA[0]));
        bufp->chgBit(oldp+1982,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegA[1]));
        bufp->chgBit(oldp+1983,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB[0]));
        bufp->chgBit(oldp+1984,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memReadRegB[1]));
        bufp->chgBit(oldp+1985,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memWriteReg[0]));
        bufp->chgBit(oldp+1986,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__memWriteReg[1]));
        bufp->chgBit(oldp+1987,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                       [0U] >> 0xdU))));
        bufp->chgBit(oldp+1988,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                       [0U] >> 0xcU))));
        bufp->chgSData(oldp+1989,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                             [0U] >> 2U))),10);
        bufp->chgCData(oldp+1990,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                   [0U])),2);
        bufp->chgBit(oldp+1991,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                       [1U] >> 0xdU))));
        bufp->chgBit(oldp+1992,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                       [1U] >> 0xcU))));
        bufp->chgSData(oldp+1993,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                             [1U] >> 2U))),10);
        bufp->chgCData(oldp+1994,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
                                   [1U])),2);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x1fU] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x68U])))) {
        bufp->chgBit(oldp+1995,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__stall));
        bufp->chgBit(oldp+1996,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__clear));
        bufp->chgBit(oldp+1997,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__flush[0]));
        bufp->chgSData(oldp+1998,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                             [0U][2U] 
                                             >> 0x13U))),10);
        bufp->chgCData(oldp+1999,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                         [0U][2U] >> 0x11U))),2);
        bufp->chgCData(oldp+2000,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                         [0U][2U] >> 0xeU))),3);
        bufp->chgCData(oldp+2001,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                            [0U][2U] 
                                            >> 9U))),5);
        bufp->chgCData(oldp+2002,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                         [0U][2U] >> 6U))),3);
        bufp->chgCData(oldp+2003,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                         [0U][2U] >> 4U))),2);
        bufp->chgCData(oldp+2004,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                         [0U][2U] >> 2U))),2);
        bufp->chgCData(oldp+2005,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                   [0U][2U])),2);
        bufp->chgCData(oldp+2006,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                   [0U][1U] >> 0x1aU)),6);
        bufp->chgCData(oldp+2007,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0x16U))),4);
        bufp->chgCData(oldp+2008,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                           [0U][1U] 
                                           >> 0x12U))),4);
        bufp->chgBit(oldp+2009,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                       [0U][1U] >> 0x11U))));
        bufp->chgCData(oldp+2010,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                            [0U][1U] 
                                            >> 0xbU))),6);
        bufp->chgBit(oldp+2011,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                       [0U][1U] >> 0xaU))));
        bufp->chgCData(oldp+2012,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                            [0U][1U] 
                                            >> 4U))),6);
        bufp->chgBit(oldp+2013,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                       [0U][1U] >> 3U))));
        bufp->chgCData(oldp+2014,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                             [0U][1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                               [0U][0U] 
                                               >> 0x1dU)))),6);
        bufp->chgBit(oldp+2015,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                       [0U][0U] >> 0x1cU))));
        bufp->chgBit(oldp+2016,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                       [0U][0U] >> 0x1bU))));
        bufp->chgCData(oldp+2017,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                            [0U][0U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2018,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                       [0U][0U] >> 0x14U))));
        bufp->chgIData(oldp+2019,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                               [0U][0U] 
                                               >> 1U))),19);
        bufp->chgBit(oldp+2020,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__iqData
                                 [0U][0U])));
        bufp->chgCData(oldp+2021,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__fpOpInfo
                                         [0U] >> 0xeU))),3);
        bufp->chgCData(oldp+2022,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__fpOpInfo
                                            [0U] >> 9U))),5);
        bufp->chgCData(oldp+2023,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__fpOpInfo
                                         [0U] >> 6U))),3);
        bufp->chgCData(oldp+2024,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__fpOpInfo
                                         [0U] >> 4U))),2);
        bufp->chgCData(oldp+2025,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__fpOpInfo
                                         [0U] >> 2U))),2);
        bufp->chgCData(oldp+2026,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__fpOpInfo
                                   [0U])),2);
        bufp->chgBit(oldp+2027,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__operandA
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2028,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__operandA
                                          [0U])),32);
        bufp->chgBit(oldp+2029,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__operandB
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2030,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__operandB
                                          [0U])),32);
        bufp->chgBit(oldp+2031,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__operandC
                                               [0U] 
                                               >> 0x20U)))));
        bufp->chgIData(oldp+2032,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__operandC
                                          [0U])),32);
        bufp->chgBit(oldp+2033,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opSrc
                                       [0U] >> 0x14U))));
        bufp->chgCData(oldp+2034,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opSrc
                                            [0U] >> 0xeU))),6);
        bufp->chgBit(oldp+2035,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opSrc
                                       [0U] >> 0xdU))));
        bufp->chgCData(oldp+2036,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opSrc
                                            [0U] >> 7U))),6);
        bufp->chgBit(oldp+2037,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opSrc
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2038,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opSrc
                                   [0U])),6);
        bufp->chgBit(oldp+2039,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opDst
                                       [0U] >> 7U))));
        bufp->chgBit(oldp+2040,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opDst
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2041,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__opDst
                                   [0U])),6);
        bufp->chgSData(oldp+2042,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                              [0U][7U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                                [0U][6U] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+2043,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+2044,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][6U] >> 0x17U))));
        bufp->chgBit(oldp+2045,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][6U] >> 0x16U))));
        bufp->chgBit(oldp+2046,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][6U] >> 0x15U))));
        bufp->chgSData(oldp+2047,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                             [0U][6U] 
                                             >> 0xbU))),10);
        bufp->chgCData(oldp+2048,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][6U] >> 9U))),2);
        bufp->chgCData(oldp+2049,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][6U] >> 6U))),3);
        bufp->chgCData(oldp+2050,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                            [0U][6U] 
                                            >> 1U))),5);
        bufp->chgCData(oldp+2051,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                          [0U][6U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                          [0U][5U] 
                                          >> 0x1eU)))),3);
        bufp->chgCData(oldp+2052,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][5U] >> 0x1cU))),2);
        bufp->chgCData(oldp+2053,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][5U] >> 0x1aU))),2);
        bufp->chgCData(oldp+2054,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][5U] >> 0x18U))),2);
        bufp->chgCData(oldp+2055,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 0x12U))),6);
        bufp->chgCData(oldp+2056,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                           [0U][5U] 
                                           >> 0xeU))),4);
        bufp->chgCData(oldp+2057,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                           [0U][5U] 
                                           >> 0xaU))),4);
        bufp->chgBit(oldp+2058,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][5U] >> 9U))));
        bufp->chgCData(oldp+2059,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                            [0U][5U] 
                                            >> 3U))),6);
        bufp->chgBit(oldp+2060,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][5U] >> 2U))));
        bufp->chgCData(oldp+2061,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                             [0U][5U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                               [0U][4U] 
                                               >> 0x1cU)))),6);
        bufp->chgBit(oldp+2062,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][4U] >> 0x1bU))));
        bufp->chgCData(oldp+2063,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2064,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][4U] >> 0x14U))));
        bufp->chgBit(oldp+2065,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][4U] >> 0x13U))));
        bufp->chgCData(oldp+2066,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                            [0U][4U] 
                                            >> 0xdU))),6);
        bufp->chgBit(oldp+2067,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][4U] >> 0xcU))));
        bufp->chgIData(oldp+2068,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                                [0U][4U] 
                                                << 7U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                                  [0U][3U] 
                                                  >> 0x19U)))),19);
        bufp->chgBit(oldp+2069,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][3U] >> 0x18U))));
        bufp->chgBit(oldp+2070,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][3U] >> 0x17U))));
        bufp->chgIData(oldp+2071,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                    [0U][3U] << 9U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                      [0U][2U] >> 0x17U))),32);
        bufp->chgBit(oldp+2072,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][2U] >> 0x16U))));
        bufp->chgIData(oldp+2073,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                    [0U][2U] << 0xaU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                      [0U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+2074,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+2075,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                    [0U][1U] << 0xbU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                      [0U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+2076,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 0x14U))));
        bufp->chgCData(oldp+2077,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+2078,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 0x11U))));
        bufp->chgBit(oldp+2079,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+2080,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+2081,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+2082,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 0xdU))));
        bufp->chgCData(oldp+2083,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+2084,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+2085,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 9U))));
        bufp->chgBit(oldp+2086,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 8U))));
        bufp->chgBit(oldp+2087,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 7U))));
        bufp->chgBit(oldp+2088,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 6U))));
        bufp->chgCData(oldp+2089,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                         [0U][0U] >> 4U))),2);
        bufp->chgBit(oldp+2090,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+2091,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2092,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2093,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__nextStage
                                 [0U][0U])));
        bufp->chgIData(oldp+2094,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__unnamedblk3__DOT__i),32);
        bufp->chgIData(oldp+2095,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__unnamedblk4__DOT__i),32);
        bufp->chgSData(oldp+2096,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                              [0U][7U] 
                                              << 6U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                                [0U][6U] 
                                                >> 0x1aU)))),10);
        bufp->chgCData(oldp+2097,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 0x18U))),2);
        bufp->chgBit(oldp+2098,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][6U] >> 0x17U))));
        bufp->chgBit(oldp+2099,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][6U] >> 0x16U))));
        bufp->chgBit(oldp+2100,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][6U] >> 0x15U))));
        bufp->chgSData(oldp+2101,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                             [0U][6U] 
                                             >> 0xbU))),10);
        bufp->chgCData(oldp+2102,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 9U))),2);
        bufp->chgCData(oldp+2103,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][6U] >> 6U))),3);
        bufp->chgCData(oldp+2104,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                            [0U][6U] 
                                            >> 1U))),5);
        bufp->chgCData(oldp+2105,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                          [0U][6U] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                          [0U][5U] 
                                          >> 0x1eU)))),3);
        bufp->chgCData(oldp+2106,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][5U] >> 0x1cU))),2);
        bufp->chgCData(oldp+2107,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][5U] >> 0x1aU))),2);
        bufp->chgCData(oldp+2108,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][5U] >> 0x18U))),2);
        bufp->chgCData(oldp+2109,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            >> 0x12U))),6);
        bufp->chgCData(oldp+2110,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 0xeU))),4);
        bufp->chgCData(oldp+2111,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                           [0U][5U] 
                                           >> 0xaU))),4);
        bufp->chgBit(oldp+2112,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][5U] >> 9U))));
        bufp->chgCData(oldp+2113,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                            [0U][5U] 
                                            >> 3U))),6);
        bufp->chgBit(oldp+2114,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][5U] >> 2U))));
        bufp->chgCData(oldp+2115,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                             [0U][5U] 
                                             << 4U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                               [0U][4U] 
                                               >> 0x1cU)))),6);
        bufp->chgBit(oldp+2116,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x1bU))));
        bufp->chgCData(oldp+2117,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 0x15U))),6);
        bufp->chgBit(oldp+2118,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x14U))));
        bufp->chgBit(oldp+2119,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 0x13U))));
        bufp->chgCData(oldp+2120,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                            [0U][4U] 
                                            >> 0xdU))),6);
        bufp->chgBit(oldp+2121,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][4U] >> 0xcU))));
        bufp->chgIData(oldp+2122,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                                [0U][4U] 
                                                << 7U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                                  [0U][3U] 
                                                  >> 0x19U)))),19);
        bufp->chgBit(oldp+2123,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x18U))));
        bufp->chgBit(oldp+2124,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][3U] >> 0x17U))));
        bufp->chgIData(oldp+2125,(((vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                    [0U][3U] << 9U) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                      [0U][2U] >> 0x17U))),32);
        bufp->chgBit(oldp+2126,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][2U] >> 0x16U))));
        bufp->chgIData(oldp+2127,(((vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                    [0U][2U] << 0xaU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                      [0U][1U] >> 0x16U))),32);
        bufp->chgBit(oldp+2128,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][1U] >> 0x15U))));
        bufp->chgIData(oldp+2129,(((vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                    [0U][1U] << 0xbU) 
                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                      [0U][0U] >> 0x15U))),32);
        bufp->chgBit(oldp+2130,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x14U))));
        bufp->chgCData(oldp+2131,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][0U] >> 0x12U))),2);
        bufp->chgBit(oldp+2132,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x11U))));
        bufp->chgBit(oldp+2133,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0x10U))));
        bufp->chgBit(oldp+2134,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xfU))));
        bufp->chgBit(oldp+2135,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xeU))));
        bufp->chgBit(oldp+2136,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xdU))));
        bufp->chgCData(oldp+2137,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][0U] >> 0xbU))),2);
        bufp->chgBit(oldp+2138,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 0xaU))));
        bufp->chgBit(oldp+2139,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 9U))));
        bufp->chgBit(oldp+2140,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 8U))));
        bufp->chgBit(oldp+2141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 7U))));
        bufp->chgBit(oldp+2142,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 6U))));
        bufp->chgCData(oldp+2143,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                         [0U][0U] >> 4U))),2);
        bufp->chgBit(oldp+2144,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 3U))));
        bufp->chgBit(oldp+2145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 2U))));
        bufp->chgBit(oldp+2146,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                       [0U][0U] >> 1U))));
        bufp->chgBit(oldp+2147,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__fpRrStageIF.__PVT__nextStage
                                 [0U][0U])));
        bufp->chgBit(oldp+2148,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumA
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2149,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumA
                                   [0U])),6);
        bufp->chgBit(oldp+2150,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumB
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2151,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumB
                                   [0U])),6);
        bufp->chgBit(oldp+2152,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumC
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2153,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpSrcRegNumC
                                   [0U])),6);
        bufp->chgBit(oldp+2154,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumA
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2155,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumA
                                   [0U])),6);
        bufp->chgBit(oldp+2156,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumB
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2157,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumB
                                   [0U])),6);
        bufp->chgBit(oldp+2158,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumC
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2159,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhySrcRegNumC
                                   [0U])),6);
        bufp->chgBit(oldp+2160,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhyDstRegNum
                                       [0U] >> 6U))));
        bufp->chgCData(oldp+2161,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpPhyDstRegNum
                                   [0U])),6);
        bufp->chgBit(oldp+2162,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegA[0]));
        bufp->chgBit(oldp+2163,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegB[0]));
        bufp->chgBit(oldp+2164,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpReadRegC[0]));
        bufp->chgBit(oldp+2165,(vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__fpWriteReg[0]));
        bufp->chgBit(oldp+2166,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
                                       [0U] >> 0xdU))));
        bufp->chgBit(oldp+2167,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
                                       [0U] >> 0xcU))));
        bufp->chgSData(oldp+2168,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
                                             [0U] >> 2U))),10);
        bufp->chgCData(oldp+2169,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
                                   [0U])),2);
    }
    if (VL_UNLIKELY(((((vlSelfRef.__Vm_traceActivity
                        [0x20U] | vlSelfRef.__Vm_traceActivity
                        [0x53U]) | vlSelfRef.__Vm_traceActivity
                       [0x69U]) | vlSelfRef.__Vm_traceActivity
                      [0x9cU])))) {
        bufp->chgSData(oldp+2170,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                  [0U]]),10);
        bufp->chgSData(oldp+2171,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__ra
                                  [1U]]),10);
        bufp->chgBit(oldp+2172,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [0U]]));
        bufp->chgBit(oldp+2173,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [1U]]));
        bufp->chgBit(oldp+2174,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [2U]]));
        bufp->chgBit(oldp+2175,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [3U]]));
        bufp->chgBit(oldp+2176,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [4U]]));
        bufp->chgBit(oldp+2177,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [5U]]));
        bufp->chgBit(oldp+2178,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [6U]]));
        bufp->chgBit(oldp+2179,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [7U]]));
        bufp->chgBit(oldp+2180,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [0U]]));
        bufp->chgBit(oldp+2181,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [1U]]));
        bufp->chgBit(oldp+2182,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [2U]]));
        bufp->chgBit(oldp+2183,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [3U]]));
        bufp->chgBit(oldp+2184,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [4U]]));
        bufp->chgBit(oldp+2185,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__5__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [5U]]));
        bufp->chgBit(oldp+2186,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__6__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [6U]]));
        bufp->chgBit(oldp+2187,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__7__KET____DOT__rBank__DOT__array
                                [vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body.__PVT__genblk1__DOT__lvt__DOT__rbReadAddr
                                [7U]]));
    }
    if (VL_UNLIKELY(((((vlSelfRef.__Vm_traceActivity
                        [0x20U] | vlSelfRef.__Vm_traceActivity
                        [0x53U]) | vlSelfRef.__Vm_traceActivity
                       [0x69U]) | vlSelfRef.__Vm_traceActivity
                      [0xa2U])))) {
        bufp->chgCData(oldp+2188,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [1U]]),3);
        bufp->chgCData(oldp+2189,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__0__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [2U]]),3);
        bufp->chgCData(oldp+2190,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__0__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [3U]]),3);
        bufp->chgCData(oldp+2191,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__0__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [4U]]),3);
        bufp->chgCData(oldp+2192,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__0__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [5U]]),3);
        bufp->chgCData(oldp+2193,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__0__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [6U]]),3);
        bufp->chgCData(oldp+2194,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__0__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [7U]]),3);
        bufp->chgCData(oldp+2195,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [0U]]),3);
        bufp->chgCData(oldp+2196,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__1__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [2U]]),3);
        bufp->chgCData(oldp+2197,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__1__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [3U]]),3);
    }
    if (VL_UNLIKELY(((((vlSelfRef.__Vm_traceActivity
                        [0x20U] | vlSelfRef.__Vm_traceActivity
                        [0x53U]) | vlSelfRef.__Vm_traceActivity
                       [0x69U]) | vlSelfRef.__Vm_traceActivity
                      [0xa3U])))) {
        bufp->chgCData(oldp+2198,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__1__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [4U]]),3);
        bufp->chgCData(oldp+2199,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__1__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [5U]]),3);
        bufp->chgCData(oldp+2200,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__1__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [6U]]),3);
        bufp->chgCData(oldp+2201,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__1__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [7U]]),3);
        bufp->chgCData(oldp+2202,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__2__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [0U]]),3);
        bufp->chgCData(oldp+2203,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__2__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [1U]]),3);
        bufp->chgCData(oldp+2204,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__2__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [3U]]),3);
        bufp->chgCData(oldp+2205,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__2__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [4U]]),3);
        bufp->chgCData(oldp+2206,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__2__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [5U]]),3);
        bufp->chgCData(oldp+2207,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__2__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [6U]]),3);
    }
    if (VL_UNLIKELY(((((vlSelfRef.__Vm_traceActivity
                        [0x20U] | vlSelfRef.__Vm_traceActivity
                        [0x53U]) | vlSelfRef.__Vm_traceActivity
                       [0x69U]) | vlSelfRef.__Vm_traceActivity
                      [0xa4U])))) {
        bufp->chgCData(oldp+2208,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__2__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [7U]]),3);
        bufp->chgCData(oldp+2209,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__3__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [0U]]),3);
        bufp->chgCData(oldp+2210,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__3__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [1U]]),3);
        bufp->chgCData(oldp+2211,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__3__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [2U]]),3);
        bufp->chgCData(oldp+2212,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__3__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [4U]]),3);
        bufp->chgCData(oldp+2213,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__3__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [5U]]),3);
        bufp->chgCData(oldp+2214,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__3__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [6U]]),3);
        bufp->chgCData(oldp+2215,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__3__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [7U]]),3);
        bufp->chgCData(oldp+2216,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__4__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [0U]]),3);
        bufp->chgCData(oldp+2217,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__4__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [1U]]),3);
    }
    if (VL_UNLIKELY(((((vlSelfRef.__Vm_traceActivity
                        [0x20U] | vlSelfRef.__Vm_traceActivity
                        [0x53U]) | vlSelfRef.__Vm_traceActivity
                       [0x69U]) | vlSelfRef.__Vm_traceActivity
                      [0xa5U])))) {
        bufp->chgCData(oldp+2218,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__4__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [2U]]),3);
        bufp->chgCData(oldp+2219,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__4__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [3U]]),3);
        bufp->chgCData(oldp+2220,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__4__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [5U]]),3);
        bufp->chgCData(oldp+2221,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__4__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [6U]]),3);
        bufp->chgCData(oldp+2222,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__4__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [7U]]),3);
        bufp->chgCData(oldp+2223,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__5__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [0U]]),3);
        bufp->chgCData(oldp+2224,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__5__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [1U]]),3);
        bufp->chgCData(oldp+2225,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__5__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [2U]]),3);
        bufp->chgCData(oldp+2226,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__5__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [3U]]),3);
        bufp->chgCData(oldp+2227,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__5__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [4U]]),3);
    }
    if (VL_UNLIKELY(((((vlSelfRef.__Vm_traceActivity
                        [0x20U] | vlSelfRef.__Vm_traceActivity
                        [0x53U]) | vlSelfRef.__Vm_traceActivity
                       [0x69U]) | vlSelfRef.__Vm_traceActivity
                      [0xa6U])))) {
        bufp->chgCData(oldp+2228,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__5__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [6U]]),3);
        bufp->chgCData(oldp+2229,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__5__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [7U]]),3);
        bufp->chgCData(oldp+2230,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__6__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [0U]]),3);
        bufp->chgCData(oldp+2231,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__6__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [1U]]),3);
        bufp->chgCData(oldp+2232,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__6__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [2U]]),3);
        bufp->chgCData(oldp+2233,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__6__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [3U]]),3);
        bufp->chgCData(oldp+2234,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__6__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [4U]]),3);
        bufp->chgCData(oldp+2235,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__6__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [5U]]),3);
        bufp->chgCData(oldp+2236,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__6__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [7U]]),3);
        bufp->chgCData(oldp+2237,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wj__BRA__7__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
                                  [vlSymsp->TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt.__PVT__wbReadAddr
                                  [0U]]),3);
    }
    if (VL_UNLIKELY(((vlSelfRef.__Vm_traceActivity[0x20U] 
                      | vlSelfRef.__Vm_traceActivity
                      [0x69U])))) {
        bufp->chgBit(oldp+2238,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__push));
        bufp->chgCData(oldp+2239,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__pushCount),2);
        bufp->chgBit(oldp+2240,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__update[0]));
        bufp->chgBit(oldp+2241,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__update[1]));
        bufp->chgSData(oldp+2242,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                     [0U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+2243,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                 [0U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+2244,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+2245,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                       [0U] 
                                                       >> 0x1fU)))),19);
        bufp->chgBit(oldp+2246,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x1eU)))));
        bufp->chgCData(oldp+2247,((0x1fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                    [0U] 
                                                    >> 0x19U)))),5);
        bufp->chgBit(oldp+2248,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x18U)))));
        bufp->chgBit(oldp+2249,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x17U)))));
        bufp->chgBit(oldp+2250,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x16U)))));
        bufp->chgBit(oldp+2251,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x15U)))));
        bufp->chgBit(oldp+2252,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x14U)))));
        bufp->chgBit(oldp+2253,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x13U)))));
        bufp->chgBit(oldp+2254,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x12U)))));
        bufp->chgBit(oldp+2255,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0x11U)))));
        bufp->chgCData(oldp+2256,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                    [0U] 
                                                    >> 0xbU)))),6);
        bufp->chgBit(oldp+2257,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [0U] 
                                               >> 0xaU)))));
        bufp->chgCData(oldp+2258,((0x3fU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                    [0U] 
                                                    >> 4U)))),6);
        bufp->chgCData(oldp+2259,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                  [0U]))),4);
        bufp->chgSData(oldp+2260,((0x3ffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                     [1U] 
                                                     >> 0x35U)))),10);
        bufp->chgCData(oldp+2261,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                 [1U] 
                                                 >> 0x33U)))),2);
        bufp->chgBit(oldp+2262,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                               [1U] 
                                               >> 0x32U)))));
        bufp->chgIData(oldp+2263,((0x7ffffU & (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__alEntry
                                                       [1U] 
                                                       >> 0x1fU)))),19);
    }
}
