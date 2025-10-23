// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Tracing implementation internals
#include "verilated_vcd_c.h"
#include "VSMT_RTL_Testbench__Syms.h"


VL_ATTR_COLD void VSMT_RTL_Testbench___024root__trace_full_0_sub_6(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd::Buffer* bufp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root__trace_full_0_sub_6\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    uint32_t* const oldp VL_ATTR_UNUSED = bufp->oldp(vlSymsp->__Vm_baseCode);
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
    bufp->fullBit(oldp+18542,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                     [1U][0U] >> 0x1cU))));
    bufp->fullBit(oldp+18543,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                     [1U][0U] >> 0x1bU))));
    bufp->fullCData(oldp+18544,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                          [1U][0U] 
                                          >> 0x15U))),6);
    bufp->fullBit(oldp+18545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                     [1U][0U] >> 0x14U))));
    bufp->fullIData(oldp+18546,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                                             [1U][0U] 
                                             >> 1U))),19);
    bufp->fullBit(oldp+18547,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__memEntry
                               [1U][0U])));
    bufp->fullSData(oldp+18548,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [0U][2U] 
                                           >> 0x13U))),10);
    bufp->fullCData(oldp+18549,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 0x11U))),2);
    bufp->fullCData(oldp+18550,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 0xeU))),3);
    bufp->fullCData(oldp+18551,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][2U] 
                                          >> 9U))),5);
    bufp->fullCData(oldp+18552,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 6U))),3);
    bufp->fullCData(oldp+18553,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 4U))),2);
    bufp->fullCData(oldp+18554,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [0U][2U] >> 2U))),2);
    bufp->fullCData(oldp+18555,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [0U][2U])),2);
    bufp->fullCData(oldp+18556,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [0U][1U] >> 0x1aU)),6);
    bufp->fullCData(oldp+18557,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [0U][1U] >> 0x16U))),4);
    bufp->fullCData(oldp+18558,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [0U][1U] >> 0x12U))),4);
    bufp->fullBit(oldp+18559,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][1U] >> 0x11U))));
    bufp->fullCData(oldp+18560,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][1U] 
                                          >> 0xbU))),6);
    bufp->fullBit(oldp+18561,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][1U] >> 0xaU))));
    bufp->fullCData(oldp+18562,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][1U] 
                                          >> 4U))),6);
    bufp->fullBit(oldp+18563,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][1U] >> 3U))));
    bufp->fullCData(oldp+18564,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [0U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [0U][0U] 
                                           >> 0x1dU)))),6);
    bufp->fullBit(oldp+18565,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][0U] >> 0x1cU))));
    bufp->fullBit(oldp+18566,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][0U] >> 0x1bU))));
    bufp->fullCData(oldp+18567,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [0U][0U] 
                                          >> 0x15U))),6);
    bufp->fullBit(oldp+18568,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [0U][0U] >> 0x14U))));
    bufp->fullIData(oldp+18569,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                             [0U][0U] 
                                             >> 1U))),19);
    bufp->fullBit(oldp+18570,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                               [0U][0U])));
    bufp->fullSData(oldp+18571,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [1U][2U] 
                                           >> 0x13U))),10);
    bufp->fullCData(oldp+18572,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 0x11U))),2);
    bufp->fullCData(oldp+18573,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 0xeU))),3);
    bufp->fullCData(oldp+18574,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][2U] 
                                          >> 9U))),5);
    bufp->fullCData(oldp+18575,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 6U))),3);
    bufp->fullCData(oldp+18576,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 4U))),2);
    bufp->fullCData(oldp+18577,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                       [1U][2U] >> 2U))),2);
    bufp->fullCData(oldp+18578,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [1U][2U])),2);
    bufp->fullCData(oldp+18579,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                 [1U][1U] >> 0x1aU)),6);
    bufp->fullCData(oldp+18580,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [1U][1U] >> 0x16U))),4);
    bufp->fullCData(oldp+18581,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                         [1U][1U] >> 0x12U))),4);
    bufp->fullBit(oldp+18582,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][1U] >> 0x11U))));
    bufp->fullCData(oldp+18583,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][1U] 
                                          >> 0xbU))),6);
    bufp->fullBit(oldp+18584,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][1U] >> 0xaU))));
    bufp->fullCData(oldp+18585,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][1U] 
                                          >> 4U))),6);
    bufp->fullBit(oldp+18586,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][1U] >> 3U))));
    bufp->fullCData(oldp+18587,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [1U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                           [1U][0U] 
                                           >> 0x1dU)))),6);
    bufp->fullBit(oldp+18588,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][0U] >> 0x1cU))));
    bufp->fullBit(oldp+18589,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][0U] >> 0x1bU))));
    bufp->fullCData(oldp+18590,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                          [1U][0U] 
                                          >> 0x15U))),6);
    bufp->fullBit(oldp+18591,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                     [1U][0U] >> 0x14U))));
    bufp->fullIData(oldp+18592,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                                             [1U][0U] 
                                             >> 1U))),19);
    bufp->fullBit(oldp+18593,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__fpEntry
                               [1U][0U])));
    bufp->fullCData(oldp+18594,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [0U] 
                                               >> 0x2fU)))),2);
    bufp->fullCData(oldp+18595,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [0U] 
                                               >> 0x2cU)))),3);
    bufp->fullBit(oldp+18596,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x2bU)))));
    bufp->fullBit(oldp+18597,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x2aU)))));
    bufp->fullBit(oldp+18598,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x29U)))));
    bufp->fullBit(oldp+18599,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x28U)))));
    bufp->fullCData(oldp+18600,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0x22U)))),6);
    bufp->fullBit(oldp+18601,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x21U)))));
    bufp->fullCData(oldp+18602,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0x1bU)))),6);
    bufp->fullBit(oldp+18603,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x1aU)))));
    bufp->fullCData(oldp+18604,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0x14U)))),6);
    bufp->fullBit(oldp+18605,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x13U)))));
    bufp->fullBit(oldp+18606,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [0U] >> 0x12U)))));
    bufp->fullCData(oldp+18607,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [0U] 
                                                  >> 0xcU)))),6);
    bufp->fullCData(oldp+18608,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [0U] 
                                                 >> 8U)))),4);
    bufp->fullCData(oldp+18609,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [0U] 
                                                 >> 4U)))),4);
    bufp->fullCData(oldp+18610,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                [0U]))),4);
    bufp->fullCData(oldp+18611,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [1U] 
                                               >> 0x2fU)))),2);
    bufp->fullCData(oldp+18612,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                               [1U] 
                                               >> 0x2cU)))),3);
    bufp->fullBit(oldp+18613,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x2bU)))));
    bufp->fullBit(oldp+18614,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x2aU)))));
    bufp->fullBit(oldp+18615,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x29U)))));
    bufp->fullBit(oldp+18616,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x28U)))));
    bufp->fullCData(oldp+18617,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0x22U)))),6);
    bufp->fullBit(oldp+18618,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x21U)))));
    bufp->fullCData(oldp+18619,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0x1bU)))),6);
    bufp->fullBit(oldp+18620,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x1aU)))));
    bufp->fullCData(oldp+18621,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0x14U)))),6);
    bufp->fullBit(oldp+18622,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x13U)))));
    bufp->fullBit(oldp+18623,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                             [1U] >> 0x12U)))));
    bufp->fullCData(oldp+18624,((0x3fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                  [1U] 
                                                  >> 0xcU)))),6);
    bufp->fullCData(oldp+18625,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [1U] 
                                                 >> 8U)))),4);
    bufp->fullCData(oldp+18626,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                 [1U] 
                                                 >> 4U)))),4);
    bufp->fullCData(oldp+18627,((0xfU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__schedulerEntry
                                                [1U]))),4);
    bufp->fullBit(oldp+18628,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [0U] >> 0x14U))));
    bufp->fullCData(oldp+18629,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [0U] >> 0xeU))),6);
    bufp->fullBit(oldp+18630,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [0U] >> 0xdU))));
    bufp->fullCData(oldp+18631,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [0U] >> 7U))),6);
    bufp->fullBit(oldp+18632,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [0U] >> 6U))));
    bufp->fullCData(oldp+18633,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                 [0U])),6);
    bufp->fullBit(oldp+18634,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [1U] >> 0x14U))));
    bufp->fullCData(oldp+18635,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [1U] >> 0xeU))),6);
    bufp->fullBit(oldp+18636,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [1U] >> 0xdU))));
    bufp->fullCData(oldp+18637,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                          [1U] >> 7U))),6);
    bufp->fullBit(oldp+18638,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                     [1U] >> 6U))));
    bufp->fullCData(oldp+18639,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opSrc
                                 [1U])),6);
    bufp->fullBit(oldp+18640,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                     [0U] >> 7U))));
    bufp->fullBit(oldp+18641,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                     [0U] >> 6U))));
    bufp->fullCData(oldp+18642,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                 [0U])),6);
    bufp->fullBit(oldp+18643,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                     [1U] >> 7U))));
    bufp->fullBit(oldp+18644,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                     [1U] >> 6U))));
    bufp->fullCData(oldp+18645,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__opDst
                                 [1U])),6);
    bufp->fullCData(oldp+18646,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                               [0U] 
                                               >> 0x37U)))),2);
    bufp->fullCData(oldp+18647,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                               [0U] 
                                               >> 0x35U)))),2);
    bufp->fullCData(oldp+18648,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                 [0U] 
                                                 >> 0x31U)))),4);
    bufp->fullBit(oldp+18649,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                             [0U] >> 0x30U)))));
    bufp->fullIData(oldp+18650,((0x3fffffffU & (IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                        [0U] 
                                                        >> 0x12U)))),30);
    bufp->fullIData(oldp+18651,((0x3ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                    [0U]))),18);
    bufp->fullCData(oldp+18652,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                               [1U] 
                                               >> 0x37U)))),2);
    bufp->fullCData(oldp+18653,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                               [1U] 
                                               >> 0x35U)))),2);
    bufp->fullCData(oldp+18654,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                 [1U] 
                                                 >> 0x31U)))),4);
    bufp->fullBit(oldp+18655,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                             [1U] >> 0x30U)))));
    bufp->fullIData(oldp+18656,((0x3fffffffU & (IData)(
                                                       (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                        [1U] 
                                                        >> 0x12U)))),30);
    bufp->fullIData(oldp+18657,((0x3ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__intSubInfo
                                                    [1U]))),18);
    bufp->fullCData(oldp+18658,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                               [0U] 
                                               >> 0x37U)))),2);
    bufp->fullCData(oldp+18659,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                               [0U] 
                                               >> 0x35U)))),2);
    bufp->fullBit(oldp+18660,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                             [0U] >> 0x34U)))));
    bufp->fullIData(oldp+18661,((0x7ffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                     [0U] 
                                                     >> 0x21U)))),19);
    bufp->fullBit(oldp+18662,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                             [0U] >> 0x20U)))));
    bufp->fullSData(oldp+18663,((0x3ffU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                   [0U] 
                                                   >> 0x16U)))),10);
    bufp->fullCData(oldp+18664,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                               [0U] 
                                               >> 0x14U)))),2);
    bufp->fullIData(oldp+18665,((0xfffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                    [0U]))),20);
    bufp->fullCData(oldp+18666,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                               [1U] 
                                               >> 0x37U)))),2);
    bufp->fullCData(oldp+18667,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                               [1U] 
                                               >> 0x35U)))),2);
    bufp->fullBit(oldp+18668,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                             [1U] >> 0x34U)))));
    bufp->fullIData(oldp+18669,((0x7ffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                     [1U] 
                                                     >> 0x21U)))),19);
    bufp->fullBit(oldp+18670,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                             [1U] >> 0x20U)))));
    bufp->fullSData(oldp+18671,((0x3ffU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                   [1U] 
                                                   >> 0x16U)))),10);
    bufp->fullCData(oldp+18672,((3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                               [1U] 
                                               >> 0x14U)))),2);
    bufp->fullIData(oldp+18673,((0xfffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__brSubInfo
                                                    [1U]))),20);
    bufp->fullBit(oldp+18674,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                     [0U] >> 2U))));
    bufp->fullCData(oldp+18675,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                 [0U])),2);
    bufp->fullBit(oldp+18676,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                     [1U] >> 2U))));
    bufp->fullCData(oldp+18677,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__mulSubInfo
                                 [1U])),2);
    bufp->fullBit(oldp+18678,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                     [0U] >> 2U))));
    bufp->fullCData(oldp+18679,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                 [0U])),2);
    bufp->fullBit(oldp+18680,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                     [1U] >> 2U))));
    bufp->fullCData(oldp+18681,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__divSubInfo
                                 [1U])),2);
    bufp->fullIData(oldp+18682,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__dsStage__DOT__unnamedblk1__DOT__i),32);
    bufp->fullCData(oldp+18683,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__regPhase
                                [0U]),2);
    bufp->fullBit(oldp+18684,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__finished[0]));
    bufp->fullCData(oldp+18685,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__regActiveListPtr[0]),6);
    bufp->fullBit(oldp+18686,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regPhase))));
    bufp->fullIData(oldp+18687,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regResult),32);
    bufp->fullCData(oldp+18688,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regPhase),2);
    bufp->fullCData(oldp+18689,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regCounter),5);
    bufp->fullSData(oldp+18690,((0x3ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[5U])),10);
    bufp->fullSData(oldp+18691,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                 >> 0x16U)),10);
    bufp->fullIData(oldp+18692,((0xffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[4U] 
                                               << 2U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                                 >> 0x1eU)))),24);
    bufp->fullIData(oldp+18693,((0xffffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                              >> 6U))),24);
    bufp->fullSData(oldp+18694,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[3U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                              >> 0x1cU)))),10);
    bufp->fullBit(oldp+18695,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                     >> 0x1bU))));
    bufp->fullBit(oldp+18696,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                     >> 0x1aU))));
    bufp->fullBit(oldp+18697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                     >> 0x19U))));
    bufp->fullBit(oldp+18698,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                     >> 0x18U))));
    bufp->fullBit(oldp+18699,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                     >> 0x17U))));
    bufp->fullBit(oldp+18700,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                     >> 0x16U))));
    bufp->fullBit(oldp+18701,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                     >> 0x15U))));
    bufp->fullIData(oldp+18702,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                  << 0xbU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[1U] 
                                              >> 0x15U))),32);
    bufp->fullIData(oldp+18703,((0x7ffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[1U] 
                                                << 6U) 
                                               | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[0U] 
                                                  >> 0x1aU)))),27);
    bufp->fullIData(oldp+18704,((0x3ffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[0U])),26);
    bufp->fullBit(oldp+18705,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__dividend_normalize));
    bufp->fullSData(oldp+18706,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__virtual_expo),10);
    bufp->fullBit(oldp+18707,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__subnormal));
    bufp->fullBit(oldp+18708,((1U & ((0x8000000U & 
                                      vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                      ? (VL_GTES_III(32, 0xffffffe8U, 
                                                     VL_EXTENDS_II(32,10, (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__virtual_expo))) 
                                         | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                            >> 0x17U))
                                      : (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                         >> 0x17U)))));
    bufp->fullIData(oldp+18709,((0x7ffffffU & ((0x8000000U 
                                                & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                                ? ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__dividend_normalize)
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
                                                : (
                                                   (1U 
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
    bufp->fullIData(oldp+18710,(((0x8000000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                  ? 0U : ((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[5U])
                                           ? 0x1600000U
                                           : 0x1800000U))),26);
    bufp->fullCData(oldp+18711,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__q),3);
    bufp->fullCData(oldp+18712,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__div),4);
    bufp->fullIData(oldp+18713,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rem),27);
    bufp->fullIData(oldp+18714,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__quo),26);
    bufp->fullQData(oldp+18715,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round),48);
    bufp->fullBit(oldp+18717,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away));
    bufp->fullBit(oldp+18718,((IData)(((0xfffffe000000ULL 
                                        == (0xfffffe000000ULL 
                                            & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round)) 
                                       & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away)))));
    bufp->fullIData(oldp+18719,((0x7fffffU & ((IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round 
                                                       >> 0x19U)) 
                                              + (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away)))),23);
    bufp->fullCData(oldp+18720,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__result_expo),8);
    bufp->fullBit(oldp+18721,((1U & ((0x8000000U & 
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
    bufp->fullIData(oldp+18722,((0x7f800000U | (0x80000000U 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                   << 5U)))),32);
    bufp->fullIData(oldp+18723,((((0x8000000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                   ? (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      >> 0x1aU) : (
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                                   >> 0x19U)) 
                                 << 0x1fU)),32);
    bufp->fullIData(oldp+18724,(((0x400000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                  ? ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                      << 0xbU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[1U] 
                                                  >> 0x15U))
                                  : ((0x800000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                      ? (((0x8000000U 
                                           & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
                                           ? (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                              >> 0x1aU)
                                           : (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U] 
                                              >> 0x19U)) 
                                         << 0x1fU) : 
                                     ((1U & ((0x8000000U 
                                              & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData[2U])
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
    bufp->fullIData(oldp+18725,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpDivSqrtUnit__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+18726,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__fmaDataOut[0]),32);
    bufp->fullIData(oldp+18727,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherDataOut[0]),32);
    bufp->fullBit(oldp+18728,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                     [0U] >> 4U))));
    bufp->fullBit(oldp+18729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                     [0U] >> 3U))));
    bufp->fullBit(oldp+18730,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                     [0U] >> 2U))));
    bufp->fullBit(oldp+18731,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                                     [0U] >> 1U))));
    bufp->fullBit(oldp+18732,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__otherFFlagsOut
                               [0U])));
    bufp->fullIData(oldp+18733,(((0x40U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])
                                  ? vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[1U]
                                  : vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__final_result)),32);
    bufp->fullSData(oldp+18734,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                           >> 7U))),10);
    bufp->fullBit(oldp+18735,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                     >> 6U))));
    bufp->fullBit(oldp+18736,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                     >> 5U))));
    bufp->fullBit(oldp+18737,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                     >> 4U))));
    bufp->fullBit(oldp+18738,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                     >> 3U))));
    bufp->fullBit(oldp+18739,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                     >> 2U))));
    bufp->fullBit(oldp+18740,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U] 
                                     >> 1U))));
    bufp->fullBit(oldp+18741,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[2U])));
    bufp->fullIData(oldp+18742,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[1U]),32);
    bufp->fullIData(oldp+18743,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg[0U]),32);
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
    bufp->fullWData(oldp+18744,(__Vtemp_7),76);
    bufp->fullSData(oldp+18747,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                           >> 7U))),10);
    bufp->fullBit(oldp+18748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U] 
                                     >> 0xcU))));
    bufp->fullBit(oldp+18749,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                     >> 6U))));
    bufp->fullBit(oldp+18750,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                     >> 5U))));
    bufp->fullBit(oldp+18751,((0U == ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[0U] 
                                       | vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[1U]) 
                                      | vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U]))));
    bufp->fullBit(oldp+18752,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                     >> 4U))));
    bufp->fullBit(oldp+18753,((1U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                      >> 3U) ^ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result[2U] 
                                                >> 0xcU)))));
    bufp->fullBit(oldp+18754,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                     >> 2U))));
    bufp->fullBit(oldp+18755,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                     >> 1U))));
    bufp->fullBit(oldp+18756,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U])));
    bufp->fullIData(oldp+18757,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[1U]),32);
    bufp->fullIData(oldp+18758,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[0U]),32);
    __Vtemp_11[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[3U] 
                       << 0xdU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                   >> 0x13U));
    __Vtemp_11[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[4U] 
                       << 0xdU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[3U] 
                                   >> 0x13U));
    __Vtemp_11[2U] = (0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[4U] 
                                >> 0x13U));
    bufp->fullWData(oldp+18759,(__Vtemp_11),76);
    bufp->fullCData(oldp+18762,((0xffU & (VL_GTES_III(32, 0U, 
                                                      VL_EXTENDS_II(32,10, (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__virtual_expo)))
                                           ? ((IData)(0x1aU) 
                                              - (0xffU 
                                                 & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                                    >> 9U)))
                                           : ((IData)(0x33U) 
                                              - (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__leading_zeros))))),8);
    bufp->fullSData(oldp+18763,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__virtual_expo),10);
    bufp->fullBit(oldp+18764,(VL_GTES_III(32, 0U, VL_EXTENDS_II(32,10, (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__virtual_expo)))));
    bufp->fullBit(oldp+18765,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 8U))));
    bufp->fullBit(oldp+18766,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 7U))));
    bufp->fullBit(oldp+18767,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 6U))));
    bufp->fullBit(oldp+18768,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 5U))));
    bufp->fullBit(oldp+18769,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 4U))));
    bufp->fullBit(oldp+18770,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 3U))));
    bufp->fullBit(oldp+18771,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 2U))));
    bufp->fullBit(oldp+18772,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                     >> 1U))));
    bufp->fullBit(oldp+18773,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U])));
    bufp->fullIData(oldp+18774,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[1U]),32);
    bufp->fullIData(oldp+18775,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[0U]),32);
    bufp->fullWData(oldp+18776,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_lhs),77);
    bufp->fullWData(oldp+18779,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_rhs),77);
    bufp->fullWData(oldp+18782,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_addend),77);
    bufp->fullWData(oldp+18785,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result),77);
    bufp->fullBit(oldp+18788,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__is_subtract));
    bufp->fullBit(oldp+18789,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg[2U] 
                                     >> 3U))));
    bufp->fullSData(oldp+18790,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg[2U] 
                                           >> 9U))),10);
    bufp->fullCData(oldp+18791,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__leading_zeros),8);
    __Vtemp_14[0U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                       << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                 >> 0x1cU));
    __Vtemp_14[1U] = ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                       << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[3U] 
                                 >> 0x1cU));
    __Vtemp_14[2U] = (0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[5U] 
                                 << 4U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[4U] 
                                           >> 0x1cU)));
    bufp->fullWData(oldp+18792,(__Vtemp_14),76);
    bufp->fullCData(oldp+18795,((0xffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                          >> 0x14U))),8);
    bufp->fullSData(oldp+18796,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                           >> 0xaU))),10);
    bufp->fullBit(oldp+18797,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 9U))));
    bufp->fullBit(oldp+18798,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 8U))));
    bufp->fullBit(oldp+18799,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 7U))));
    bufp->fullBit(oldp+18800,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 6U))));
    bufp->fullBit(oldp+18801,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 5U))));
    bufp->fullBit(oldp+18802,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 4U))));
    bufp->fullBit(oldp+18803,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 3U))));
    bufp->fullBit(oldp+18804,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 2U))));
    bufp->fullBit(oldp+18805,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                     >> 1U))));
    bufp->fullBit(oldp+18806,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])));
    bufp->fullIData(oldp+18807,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[1U]),32);
    bufp->fullIData(oldp+18808,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[0U]),32);
    bufp->fullIData(oldp+18809,((0xffffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U])),24);
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
    bufp->fullBit(oldp+18810,((0U != ((__Vtemp_18[0U] 
                                       | __Vtemp_18[1U]) 
                                      | (0xfffU & __Vtemp_18[2U])))));
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
    bufp->fullBit(oldp+18811,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                     & ((vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
                                         >> 1U) | (0U 
                                                   != 
                                                   ((__Vtemp_23[0U] 
                                                     | __Vtemp_23[1U]) 
                                                    | (0xfffU 
                                                       & __Vtemp_23[2U]))))))));
    bufp->fullBit(oldp+18812,((0xffffffU <= (0xffffffU 
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
    bufp->fullIData(oldp+18813,((0x7fffffU & (((vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U] 
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
    bufp->fullCData(oldp+18814,((0xffU & (((0x200U 
                                            & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])
                                            ? 0U : 
                                           ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                             << 0x16U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                               >> 0xaU))) 
                                          + (0xffffffU 
                                             <= (0xffffffU 
                                                 & vlSymsp->TOP__SMT_RTL_Testbench__core.fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0[0U]))))),8);
    bufp->fullBit(oldp+18815,((1U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                      >> 7U) | VL_LTES_III(32, 0xffU, 
                                                           VL_EXTENDS_II(32,10, 
                                                                         (0x3ffU 
                                                                          & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                                             >> 0xaU))))))));
    bufp->fullIData(oldp+18816,((0x7f800000U | (((0x80U 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U])
                                                  ? 
                                                 (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                  >> 2U)
                                                  : 
                                                 (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                  >> 3U)) 
                                                << 0x1fU))),32);
    bufp->fullIData(oldp+18817,((0x80000000U & (((~ 
                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U]) 
                                                 << 0x1fU) 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg[2U] 
                                                   << 0x1eU)))),32);
    bufp->fullIData(oldp+18818,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__final_result),32);
    bufp->fullIData(oldp+18819,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                         [0U] >> 5U))),32);
    bufp->fullBit(oldp+18820,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 4U)))));
    bufp->fullBit(oldp+18821,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 3U)))));
    bufp->fullBit(oldp+18822,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 2U)))));
    bufp->fullBit(oldp+18823,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 1U)))));
    bufp->fullBit(oldp+18824,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                            [0U]))));
    bufp->fullIData(oldp+18825,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                         [0U] >> 5U))),32);
    bufp->fullBit(oldp+18826,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 4U)))));
    bufp->fullBit(oldp+18827,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 3U)))));
    bufp->fullBit(oldp+18828,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 2U)))));
    bufp->fullBit(oldp+18829,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [0U] >> 1U)))));
    bufp->fullBit(oldp+18830,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                            [0U]))));
    bufp->fullIData(oldp+18831,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                         [1U] >> 5U))),32);
    bufp->fullBit(oldp+18832,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [1U] >> 4U)))));
    bufp->fullBit(oldp+18833,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [1U] >> 3U)))));
    bufp->fullBit(oldp+18834,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [1U] >> 2U)))));
    bufp->fullBit(oldp+18835,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [1U] >> 1U)))));
    bufp->fullBit(oldp+18836,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                            [1U]))));
    bufp->fullIData(oldp+18837,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                         [2U] >> 5U))),32);
    bufp->fullBit(oldp+18838,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [2U] >> 4U)))));
    bufp->fullBit(oldp+18839,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [2U] >> 3U)))));
    bufp->fullBit(oldp+18840,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [2U] >> 2U)))));
    bufp->fullBit(oldp+18841,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [2U] >> 1U)))));
    bufp->fullBit(oldp+18842,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                            [2U]))));
    bufp->fullIData(oldp+18843,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                         [3U] >> 5U))),32);
    bufp->fullBit(oldp+18844,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [3U] >> 4U)))));
    bufp->fullBit(oldp+18845,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [3U] >> 3U)))));
    bufp->fullBit(oldp+18846,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [3U] >> 2U)))));
    bufp->fullBit(oldp+18847,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                             [3U] >> 1U)))));
    bufp->fullBit(oldp+18848,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg
                                            [3U]))));
    bufp->fullIData(oldp+18849,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+18850,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk3__DOT__i),32);
    bufp->fullIData(oldp+18851,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__j),32);
    bufp->fullIData(oldp+18852,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpExStage__DOT__unnamedblk6__DOT__i),32);
    bufp->fullBit(oldp+18853,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__pipeReg
                                     [0U] >> 4U))));
    bufp->fullCData(oldp+18854,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__pipeReg
                                 [0U])),4);
    bufp->fullIData(oldp+18855,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpIsStage__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+18856,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRrStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+18857,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__fpRwStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullCData(oldp+18858,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regPhase),3);
    bufp->fullBit(oldp+18859,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regFlushStart));
    bufp->fullBit(oldp+18860,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regFlush));
    bufp->fullBit(oldp+18861,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regFlushReqAck));
    bufp->fullBit(oldp+18862,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__flushComplete));
    bufp->fullIData(oldp+18863,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                        [0U])),32);
    bufp->fullIData(oldp+18864,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                         [0U] >> 0x20U))),32);
    bufp->fullIData(oldp+18865,((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                        [1U])),32);
    bufp->fullIData(oldp+18866,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readLineInsnList
                                         [1U] >> 0x20U))),32);
    bufp->fullCData(oldp+18867,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__readNRUState),2);
    bufp->fullCData(oldp+18868,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__rstIndex),8);
    bufp->fullBit(oldp+18869,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__regMissValid));
    bufp->fullQData(oldp+18870,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                                  << 0x20U) | (QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))),64);
    bufp->fullQData(oldp+18872,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[2U])) 
                                  << 0x34U) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[1U])) 
                                                << 0x14U) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])) 
                                                  >> 0xcU)))),64);
    bufp->fullCData(oldp+18874,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__rstIndex),8);
    bufp->fullBit(oldp+18875,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U] 
                                     >> 0xbU))));
    bufp->fullSData(oldp+18876,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])),11);
    bufp->fullWData(oldp+18877,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv),76);
    bufp->fullQData(oldp+18880,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[2U])) 
                                  << 0x34U) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[1U])) 
                                                << 0x14U) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])) 
                                                  >> 0xcU)))),64);
    bufp->fullCData(oldp+18882,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__rstIndex),8);
    bufp->fullBit(oldp+18883,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U] 
                                     >> 0xbU))));
    bufp->fullSData(oldp+18884,((0x7ffU & vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv[0U])),11);
    bufp->fullWData(oldp+18885,(vlSymsp->TOP__SMT_RTL_Testbench__core.iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv),76);
    bufp->fullBit(oldp+18888,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__empty));
    bufp->fullCData(oldp+18889,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                       >> 9U))),3);
    bufp->fullCData(oldp+18890,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+18891,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                       >> 4U))),3);
    bufp->fullBit(oldp+18892,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 3U))));
    bufp->fullCData(oldp+18893,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                           >> 0x1eU)))),5);
    bufp->fullBit(oldp+18894,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 0x1dU))));
    bufp->fullCData(oldp+18895,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                          >> 0x18U))),5);
    bufp->fullBit(oldp+18896,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 0x17U))));
    bufp->fullCData(oldp+18897,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                          >> 0x12U))),5);
    bufp->fullCData(oldp+18898,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                         >> 0xeU))),4);
    bufp->fullBit(oldp+18899,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 0xdU))));
    bufp->fullIData(oldp+18900,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                   >> 0xfU)))),30);
    bufp->fullBit(oldp+18901,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+18902,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+18903,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+18904,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+18905,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                          >> 8U))),5);
    bufp->fullBit(oldp+18906,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 7U))));
    bufp->fullCData(oldp+18907,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                       >> 5U))),2);
    bufp->fullSData(oldp+18908,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                              >> 0x1bU)))),10);
    bufp->fullSData(oldp+18909,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                           >> 0xfU))),12);
    bufp->fullSData(oldp+18910,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                            >> 3U))),15);
    bufp->fullIData(oldp+18911,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                >> 0xfU)))),20);
    bufp->fullCData(oldp+18912,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                       >> 0xfU))),2);
    bufp->fullSData(oldp+18913,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                               >> 0x1dU)))),16);
    bufp->fullSData(oldp+18914,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                            >> 0xfU))),14);
    bufp->fullSData(oldp+18915,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                            >> 1U))),15);
    bufp->fullIData(oldp+18916,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                >> 0xfU)))),18);
    bufp->fullCData(oldp+18917,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                       >> 0xfU))),3);
    bufp->fullBit(oldp+18918,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                     >> 0xeU))));
    bufp->fullIData(oldp+18919,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                >> 0x1bU)))),19);
    bufp->fullCData(oldp+18920,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                          >> 0xcU))),5);
    bufp->fullCData(oldp+18921,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                          >> 7U))),5);
    bufp->fullCData(oldp+18922,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                       >> 4U))),3);
    bufp->fullIData(oldp+18923,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[1U] 
                                               << 0x11U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                                 >> 0xfU)))),21);
    bufp->fullCData(oldp+18924,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+18925,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+18926,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                       >> 9U))),2);
    bufp->fullBit(oldp+18927,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                     >> 8U))));
    bufp->fullBit(oldp+18928,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                     >> 7U))));
    bufp->fullBit(oldp+18929,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                     >> 6U))));
    bufp->fullBit(oldp+18930,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                     >> 5U))));
    bufp->fullBit(oldp+18931,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                     >> 4U))));
    bufp->fullBit(oldp+18932,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                     >> 3U))));
    bufp->fullCData(oldp+18933,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+18934,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0U])));
    bufp->fullCData(oldp+18935,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                       >> 0x15U))),3);
    bufp->fullCData(oldp+18936,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                       >> 0x13U))),2);
    bufp->fullCData(oldp+18937,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                       >> 0x10U))),3);
    bufp->fullBit(oldp+18938,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+18939,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                          >> 0xaU))),5);
    bufp->fullBit(oldp+18940,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 9U))));
    bufp->fullCData(oldp+18941,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                          >> 4U))),5);
    bufp->fullBit(oldp+18942,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 3U))));
    bufp->fullCData(oldp+18943,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                           >> 0x1eU)))),5);
    bufp->fullCData(oldp+18944,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                         >> 0x1aU))),4);
    bufp->fullBit(oldp+18945,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                     >> 0x19U))));
    bufp->fullIData(oldp+18946,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                   >> 0x1bU)))),30);
    bufp->fullBit(oldp+18947,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+18948,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+18949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+18950,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+18951,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                          >> 0x14U))),5);
    bufp->fullBit(oldp+18952,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+18953,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                       >> 0x11U))),2);
    bufp->fullSData(oldp+18954,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                           >> 7U))),10);
    bufp->fullSData(oldp+18955,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                              >> 0x1bU)))),12);
    bufp->fullSData(oldp+18956,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                            >> 0xfU))),15);
    bufp->fullIData(oldp+18957,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                >> 0x1bU)))),20);
    bufp->fullCData(oldp+18958,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                       >> 0x1bU))),2);
    bufp->fullSData(oldp+18959,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                            >> 9U))),16);
    bufp->fullSData(oldp+18960,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                               >> 0x1bU)))),14);
    bufp->fullSData(oldp+18961,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                            >> 0xdU))),15);
    bufp->fullIData(oldp+18962,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                >> 0x1bU)))),18);
    bufp->fullCData(oldp+18963,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                       >> 0x1bU))),3);
    bufp->fullBit(oldp+18964,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                     >> 0x1aU))));
    bufp->fullIData(oldp+18965,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                             >> 7U))),19);
    bufp->fullCData(oldp+18966,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                          >> 0x18U))),5);
    bufp->fullCData(oldp+18967,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                          >> 0x13U))),5);
    bufp->fullCData(oldp+18968,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                       >> 0x10U))),3);
    bufp->fullIData(oldp+18969,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                                 >> 0x1bU)))),21);
    bufp->fullCData(oldp+18970,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+18971,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+18972,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                       >> 0x15U))),2);
    bufp->fullBit(oldp+18973,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+18974,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 0x13U))));
    bufp->fullBit(oldp+18975,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 0x12U))));
    bufp->fullBit(oldp+18976,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+18977,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+18978,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+18979,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                       >> 0xdU))),2);
    bufp->fullBit(oldp+18980,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[2U] 
                                     >> 0xcU))));
    bufp->fullCData(oldp+18981,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                       >> 1U))),3);
    bufp->fullCData(oldp+18982,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                                  >> 0x1fU)))),2);
    bufp->fullCData(oldp+18983,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                       >> 0x1cU))),3);
    bufp->fullBit(oldp+18984,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+18985,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                          >> 0x16U))),5);
    bufp->fullBit(oldp+18986,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 0x15U))));
    bufp->fullCData(oldp+18987,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                          >> 0x10U))),5);
    bufp->fullBit(oldp+18988,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+18989,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                          >> 0xaU))),5);
    bufp->fullCData(oldp+18990,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                         >> 6U))),4);
    bufp->fullBit(oldp+18991,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 5U))));
    bufp->fullIData(oldp+18992,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                                 << 0x19U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                                   >> 7U)))),30);
    bufp->fullBit(oldp+18993,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 9U))));
    bufp->fullBit(oldp+18994,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 8U))));
    bufp->fullBit(oldp+18995,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 7U))));
    bufp->fullCData(oldp+18996,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+18997,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U])),5);
    bufp->fullBit(oldp+18998,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+18999,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                       >> 0x1dU))),2);
    bufp->fullSData(oldp+19000,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                           >> 0x13U))),10);
    bufp->fullSData(oldp+19001,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                           >> 7U))),12);
    bufp->fullSData(oldp+19002,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                               >> 0x1bU)))),15);
    bufp->fullIData(oldp+19003,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                             >> 7U))),20);
    bufp->fullCData(oldp+19004,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                       >> 7U))),2);
    bufp->fullSData(oldp+19005,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                             << 0xbU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                               >> 0x15U)))),16);
    bufp->fullSData(oldp+19006,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                            >> 7U))),14);
    bufp->fullSData(oldp+19007,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                               >> 0x19U)))),15);
    bufp->fullIData(oldp+19008,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                             >> 7U))),18);
    bufp->fullCData(oldp+19009,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                       >> 7U))),3);
    bufp->fullBit(oldp+19010,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                     >> 6U))));
    bufp->fullIData(oldp+19011,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                                >> 0x13U)))),19);
    bufp->fullCData(oldp+19012,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                          >> 4U))),5);
    bufp->fullCData(oldp+19013,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[6U] 
                                           << 1U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                           >> 0x1fU)))),5);
    bufp->fullCData(oldp+19014,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                       >> 0x1cU))),3);
    bufp->fullIData(oldp+19015,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                              >> 7U))),21);
    bufp->fullCData(oldp+19016,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+19017,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                       >> 3U))),2);
    bufp->fullCData(oldp+19018,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+19019,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[5U])));
    bufp->fullBit(oldp+19020,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                               >> 0x1fU)));
    bufp->fullBit(oldp+19021,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 0x1eU))));
    bufp->fullBit(oldp+19022,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+19023,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+19024,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+19025,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                       >> 0x19U))),2);
    bufp->fullBit(oldp+19026,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[4U] 
                                     >> 0x18U))));
    bufp->fullCData(oldp+19027,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                       >> 0xdU))),3);
    bufp->fullCData(oldp+19028,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+19029,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                       >> 8U))),3);
    bufp->fullBit(oldp+19030,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 7U))));
    bufp->fullCData(oldp+19031,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                          >> 2U))),5);
    bufp->fullBit(oldp+19032,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 1U))));
    bufp->fullCData(oldp+19033,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                           << 4U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                           >> 0x1cU)))),5);
    bufp->fullBit(oldp+19034,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+19035,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                          >> 0x16U))),5);
    bufp->fullCData(oldp+19036,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                         >> 0x12U))),4);
    bufp->fullBit(oldp+19037,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                     >> 0x11U))));
    bufp->fullIData(oldp+19038,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                                 << 0xdU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                   >> 0x13U)))),30);
    bufp->fullBit(oldp+19039,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                     >> 0x15U))));
    bufp->fullBit(oldp+19040,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+19041,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19042,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                       >> 0x11U))),2);
    bufp->fullCData(oldp+19043,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                          >> 0xcU))),5);
    bufp->fullBit(oldp+19044,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                     >> 0xbU))));
    bufp->fullCData(oldp+19045,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                       >> 9U))),2);
    bufp->fullSData(oldp+19046,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                              >> 0x1fU)))),10);
    bufp->fullSData(oldp+19047,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                           >> 0x13U))),12);
    bufp->fullSData(oldp+19048,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                            >> 7U))),15);
    bufp->fullIData(oldp+19049,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                >> 0x13U)))),20);
    bufp->fullCData(oldp+19050,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                       >> 0x13U))),2);
    bufp->fullSData(oldp+19051,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                            >> 1U))),16);
    bufp->fullSData(oldp+19052,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                             << 0xdU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                               >> 0x13U)))),14);
    bufp->fullSData(oldp+19053,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                            >> 5U))),15);
    bufp->fullIData(oldp+19054,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                >> 0x13U)))),18);
    bufp->fullCData(oldp+19055,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                       >> 0x13U))),3);
    bufp->fullBit(oldp+19056,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                     >> 0x12U))));
    bufp->fullIData(oldp+19057,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                >> 0x1fU)))),19);
    bufp->fullCData(oldp+19058,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                          >> 0x10U))),5);
    bufp->fullCData(oldp+19059,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                          >> 0xbU))),5);
    bufp->fullCData(oldp+19060,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                       >> 8U))),3);
    bufp->fullIData(oldp+19061,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[8U] 
                                               << 0xdU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                                 >> 0x13U)))),21);
    bufp->fullCData(oldp+19062,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                       >> 0x11U))),2);
    bufp->fullCData(oldp+19063,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                       >> 0xfU))),2);
    bufp->fullCData(oldp+19064,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                       >> 0xdU))),2);
    bufp->fullBit(oldp+19065,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                     >> 0xcU))));
    bufp->fullBit(oldp+19066,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                     >> 0xbU))));
    bufp->fullBit(oldp+19067,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                     >> 0xaU))));
    bufp->fullBit(oldp+19068,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                     >> 9U))));
    bufp->fullBit(oldp+19069,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                     >> 8U))));
    bufp->fullBit(oldp+19070,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                     >> 7U))));
    bufp->fullCData(oldp+19071,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                       >> 5U))),2);
    bufp->fullBit(oldp+19072,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[7U] 
                                     >> 4U))));
    bufp->fullCData(oldp+19073,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                       >> 0x19U))),3);
    bufp->fullCData(oldp+19074,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+19075,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                       >> 0x14U))),3);
    bufp->fullBit(oldp+19076,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19077,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                          >> 0xeU))),5);
    bufp->fullBit(oldp+19078,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                     >> 0xdU))));
    bufp->fullCData(oldp+19079,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                          >> 8U))),5);
    bufp->fullBit(oldp+19080,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                     >> 7U))));
    bufp->fullCData(oldp+19081,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                          >> 2U))),5);
    bufp->fullCData(oldp+19082,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                          >> 0x1eU)))),4);
    bufp->fullBit(oldp+19083,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                     >> 0x1dU))));
    bufp->fullIData(oldp+19084,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                 << 1U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                   >> 0x1fU)))),30);
    bufp->fullBit(oldp+19085,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                     >> 1U))));
    bufp->fullBit(oldp+19086,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU])));
    bufp->fullBit(oldp+19087,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+19088,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                       >> 0x1dU))),2);
    bufp->fullCData(oldp+19089,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                          >> 0x18U))),5);
    bufp->fullBit(oldp+19090,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                     >> 0x17U))));
    bufp->fullCData(oldp+19091,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                       >> 0x15U))),2);
    bufp->fullSData(oldp+19092,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                           >> 0xbU))),10);
    bufp->fullSData(oldp+19093,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                              >> 0x1fU)))),12);
    bufp->fullSData(oldp+19094,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                             << 0xdU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                               >> 0x13U)))),15);
    bufp->fullIData(oldp+19095,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                >> 0x1fU)))),20);
    bufp->fullCData(oldp+19096,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                  >> 0x1fU)))),2);
    bufp->fullSData(oldp+19097,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                            >> 0xdU))),16);
    bufp->fullSData(oldp+19098,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                               >> 0x1fU)))),14);
    bufp->fullSData(oldp+19099,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                 >> 0x11U)),15);
    bufp->fullIData(oldp+19100,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                >> 0x1fU)))),18);
    bufp->fullCData(oldp+19101,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                                  >> 0x1fU)))),3);
    bufp->fullBit(oldp+19102,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                     >> 0x1eU))));
    bufp->fullIData(oldp+19103,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                             >> 0xbU))),19);
    bufp->fullCData(oldp+19104,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                           << 4U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                           >> 0x1cU)))),5);
    bufp->fullCData(oldp+19105,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                          >> 0x17U))),5);
    bufp->fullCData(oldp+19106,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                       >> 0x14U))),3);
    bufp->fullIData(oldp+19107,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xaU] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                                 >> 0x1fU)))),21);
    bufp->fullCData(oldp+19108,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                       >> 0x1dU))),2);
    bufp->fullCData(oldp+19109,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                       >> 0x1bU))),2);
    bufp->fullCData(oldp+19110,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                       >> 0x19U))),2);
    bufp->fullBit(oldp+19111,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 0x18U))));
    bufp->fullBit(oldp+19112,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 0x17U))));
    bufp->fullBit(oldp+19113,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 0x16U))));
    bufp->fullBit(oldp+19114,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 0x15U))));
    bufp->fullBit(oldp+19115,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+19116,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19117,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                       >> 0x11U))),2);
    bufp->fullBit(oldp+19118,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[9U] 
                                     >> 0x10U))));
    bufp->fullCData(oldp+19119,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xeU] 
                                       >> 5U))),3);
    bufp->fullCData(oldp+19120,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xeU] 
                                       >> 3U))),2);
    bufp->fullCData(oldp+19121,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xeU])),3);
    bufp->fullBit(oldp+19122,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+19123,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                          >> 0x1aU))),5);
    bufp->fullBit(oldp+19124,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 0x19U))));
    bufp->fullCData(oldp+19125,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                          >> 0x14U))),5);
    bufp->fullBit(oldp+19126,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19127,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                          >> 0xeU))),5);
    bufp->fullCData(oldp+19128,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                         >> 0xaU))),4);
    bufp->fullBit(oldp+19129,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 9U))));
    bufp->fullIData(oldp+19130,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                                 << 0x15U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                                   >> 0xbU)))),30);
    bufp->fullBit(oldp+19131,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 0xdU))));
    bufp->fullBit(oldp+19132,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 0xcU))));
    bufp->fullBit(oldp+19133,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 0xbU))));
    bufp->fullCData(oldp+19134,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                       >> 9U))),2);
    bufp->fullCData(oldp+19135,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                          >> 4U))),5);
    bufp->fullBit(oldp+19136,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 3U))));
    bufp->fullCData(oldp+19137,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                       >> 1U))),2);
    bufp->fullSData(oldp+19138,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                            << 9U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                              >> 0x17U)))),10);
    bufp->fullSData(oldp+19139,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                           >> 0xbU))),12);
    bufp->fullSData(oldp+19140,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                               >> 0x1fU)))),15);
    bufp->fullIData(oldp+19141,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                             >> 0xbU))),20);
    bufp->fullCData(oldp+19142,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                       >> 0xbU))),2);
    bufp->fullSData(oldp+19143,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                               >> 0x19U)))),16);
    bufp->fullSData(oldp+19144,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                            >> 0xbU))),14);
    bufp->fullSData(oldp+19145,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                               >> 0x1dU)))),15);
    bufp->fullIData(oldp+19146,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                             >> 0xbU))),18);
    bufp->fullCData(oldp+19147,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                       >> 0xbU))),3);
    bufp->fullBit(oldp+19148,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                     >> 0xaU))));
    bufp->fullIData(oldp+19149,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                                >> 0x17U)))),19);
    bufp->fullCData(oldp+19150,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                          >> 8U))),5);
    bufp->fullCData(oldp+19151,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU] 
                                          >> 3U))),5);
    bufp->fullCData(oldp+19152,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xdU])),3);
    bufp->fullIData(oldp+19153,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                 >> 0xbU)),21);
    bufp->fullCData(oldp+19154,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                       >> 9U))),2);
    bufp->fullCData(oldp+19155,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+19156,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                       >> 5U))),2);
    bufp->fullBit(oldp+19157,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                     >> 4U))));
    bufp->fullBit(oldp+19158,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                     >> 3U))));
    bufp->fullBit(oldp+19159,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                     >> 2U))));
    bufp->fullBit(oldp+19160,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU] 
                                     >> 1U))));
    bufp->fullBit(oldp+19161,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xcU])));
    bufp->fullBit(oldp+19162,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+19163,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                       >> 0x1dU))),2);
    bufp->fullBit(oldp+19164,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__microOps[0xbU] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+19165,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 4U))));
    bufp->fullBit(oldp+19166,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 3U))));
    bufp->fullBit(oldp+19167,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 2U))));
    bufp->fullBit(oldp+19168,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 1U))));
    bufp->fullBit(oldp+19169,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo))));
    bufp->fullBit(oldp+19170,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 9U))));
    bufp->fullBit(oldp+19171,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 8U))));
    bufp->fullBit(oldp+19172,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 7U))));
    bufp->fullBit(oldp+19173,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 6U))));
    bufp->fullBit(oldp+19174,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnInfo) 
                                     >> 5U))));
    bufp->fullBit(oldp+19175,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__initiate));
    bufp->fullCData(oldp+19176,((0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0x19U)))),7);
    bufp->fullCData(oldp+19177,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0x14U)))),5);
    bufp->fullCData(oldp+19178,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0xfU)))),5);
    bufp->fullCData(oldp+19179,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                               >> 0xcU)))),3);
    bufp->fullCData(oldp+19180,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 7U)))),5);
    bufp->fullCData(oldp+19181,((0x7fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn))),7);
    bufp->fullCData(oldp+19182,((0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0x39U)))),7);
    bufp->fullCData(oldp+19183,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0x34U)))),5);
    bufp->fullCData(oldp+19184,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0x2fU)))),5);
    bufp->fullCData(oldp+19185,((7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                               >> 0x2cU)))),3);
    bufp->fullCData(oldp+19186,((0x1fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0x27U)))),5);
    bufp->fullCData(oldp+19187,((0x7fU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__isfIn 
                                                  >> 0x20U)))),7);
    bufp->fullBit(oldp+19188,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidIn[0]));
    bufp->fullBit(oldp+19189,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidIn[1]));
    bufp->fullBit(oldp+19190,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U])));
    bufp->fullIData(oldp+19191,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U] 
                                 >> 0xdU)),19);
    bufp->fullBit(oldp+19192,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U] 
                                     >> 0xcU))));
    bufp->fullSData(oldp+19193,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U] 
                                           >> 2U))),10);
    bufp->fullCData(oldp+19194,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[0U])),2);
    bufp->fullBit(oldp+19195,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[2U] 
                                     >> 1U))));
    bufp->fullIData(oldp+19196,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[2U] 
                                              << 0x12U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                                >> 0xeU)))),19);
    bufp->fullBit(oldp+19197,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                     >> 0xdU))));
    bufp->fullSData(oldp+19198,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                           >> 3U))),10);
    bufp->fullCData(oldp+19199,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredIn[1U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+19200,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                     [0U] >> 0x13U))));
    bufp->fullIData(oldp+19201,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                 [0U])),19);
    bufp->fullBit(oldp+19202,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                     [1U] >> 0x13U))));
    bufp->fullIData(oldp+19203,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pcIn
                                 [1U])),19);
    bufp->fullBit(oldp+19204,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidOut[0]));
    bufp->fullBit(oldp+19205,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnValidOut[1]));
    bufp->fullBit(oldp+19206,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushed[0]));
    bufp->fullBit(oldp+19207,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushed[1]));
    bufp->fullBit(oldp+19208,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushTriggering[0]));
    bufp->fullBit(oldp+19209,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__insnFlushTriggering[1]));
    bufp->fullBit(oldp+19210,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__flushTriggered));
    bufp->fullBit(oldp+19211,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                             [0U] >> 0x20U)))));
    bufp->fullIData(oldp+19212,((0x7ffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                     [0U] 
                                                     >> 0xdU)))),19);
    bufp->fullBit(oldp+19213,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                             [0U] >> 0xcU)))));
    bufp->fullSData(oldp+19214,((0x3ffU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                   [0U] 
                                                   >> 2U)))),10);
    bufp->fullCData(oldp+19215,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                              [0U]))),2);
    bufp->fullBit(oldp+19216,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                             [1U] >> 0x20U)))));
    bufp->fullIData(oldp+19217,((0x7ffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                     [1U] 
                                                     >> 0xdU)))),19);
    bufp->fullBit(oldp+19218,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                             [1U] >> 0xcU)))));
    bufp->fullSData(oldp+19219,((0x3ffU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                                   [1U] 
                                                   >> 2U)))),10);
    bufp->fullCData(oldp+19220,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__brPredOut
                                              [1U]))),2);
    bufp->fullBit(oldp+19221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__recoveredPC 
                                     >> 0x13U))));
    bufp->fullIData(oldp+19222,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__recoveredPC)),19);
    bufp->fullCData(oldp+19223,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__remainingValidMOps),6);
    bufp->fullCData(oldp+19224,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__curValidMOps),6);
    bufp->fullCData(oldp+19225,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__pickedValidMOps),6);
    bufp->fullCData(oldp+19226,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__serializedMOps),6);
    bufp->fullCData(oldp+19227,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPickedIndex[0]),3);
    bufp->fullCData(oldp+19228,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPickedIndex[1]),3);
    bufp->fullBit(oldp+19229,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPicked[0]));
    bufp->fullBit(oldp+19230,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__mopPicked[1]));
    bufp->fullBit(oldp+19231,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                     [0U] >> 0x13U))));
    bufp->fullIData(oldp+19232,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                 [0U])),19);
    bufp->fullBit(oldp+19233,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                     [1U] >> 0x13U))));
    bufp->fullIData(oldp+19234,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                 [1U])),19);
    bufp->fullBit(oldp+19235,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                     [2U] >> 0x13U))));
    bufp->fullIData(oldp+19236,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                 [2U])),19);
    bufp->fullBit(oldp+19237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                     [3U] >> 0x13U))));
    bufp->fullIData(oldp+19238,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                                 [3U])),19);
    bufp->fullBit(oldp+19239,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS 
                                     >> 0x13U))));
    bufp->fullIData(oldp+19240,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS)),19);
    bufp->fullBit(oldp+19241,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS));
    bufp->fullBit(oldp+19242,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__popRAS));
    bufp->fullCData(oldp+19243,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr),2);
    bufp->fullCData(oldp+19244,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS_Ptr),2);
    bufp->fullBit(oldp+19245,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                     [0U] >> 0x13U))));
    bufp->fullIData(oldp+19246,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                 [0U])),19);
    bufp->fullBit(oldp+19247,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                     [1U] >> 0x13U))));
    bufp->fullIData(oldp+19248,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                 [1U])),19);
    bufp->fullBit(oldp+19249,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                     [0U] >> 0x13U))));
    bufp->fullIData(oldp+19250,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                 [0U])),19);
    bufp->fullBit(oldp+19251,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                     [1U] >> 0x13U))));
    bufp->fullIData(oldp+19252,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                                 [1U])),19);
    bufp->fullIData(oldp+19253,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                 [0U] >> 0xcU)),20);
    bufp->fullCData(oldp+19254,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                          [0U] >> 7U))),5);
    bufp->fullCData(oldp+19255,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                 [0U])),7);
    bufp->fullIData(oldp+19256,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                 [1U] >> 0xcU)),20);
    bufp->fullCData(oldp+19257,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                          [1U] >> 7U))),5);
    bufp->fullCData(oldp+19258,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                 [1U])),7);
    bufp->fullBit(oldp+19259,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck));
    bufp->fullBit(oldp+19260,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrIncorrect));
    bufp->fullBit(oldp+19261,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane));
    bufp->fullCData(oldp+19262,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                                [0U]),2);
    bufp->fullCData(oldp+19263,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                                [1U]),2);
    bufp->fullBit(oldp+19264,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch[0]));
    bufp->fullBit(oldp+19265,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch[1]));
    bufp->fullIData(oldp+19266,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+19267,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19268,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk3__DOT__i),32);
    bufp->fullIData(oldp+19269,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i),32);
    bufp->fullIData(oldp+19270,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk5__DOT__i),32);
    bufp->fullIData(oldp+19271,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk6__DOT__i),32);
    bufp->fullIData(oldp+19272,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i),32);
    bufp->fullIData(oldp+19273,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk8__DOT__i),32);
    bufp->fullBit(oldp+19274,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__clear));
    bufp->fullBit(oldp+19275,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__sent));
    bufp->fullCData(oldp+19276,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__cur),6);
    bufp->fullIData(oldp+19277,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+19278,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__picker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__mn),32);
    bufp->fullIData(oldp+19279,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19280,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__unnamedblk9__DOT__i),32);
    bufp->fullBit(oldp+19281,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__empty));
    bufp->fullBit(oldp+19282,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regStall));
    bufp->fullSData(oldp+19283,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                           [0U] >> 0x15U))),10);
    bufp->fullBit(oldp+19284,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                     [0U] >> 0x14U))));
    bufp->fullBit(oldp+19285,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                     [0U] >> 0x13U))));
    bufp->fullIData(oldp+19286,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                 [0U])),19);
    bufp->fullSData(oldp+19287,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                           [1U] >> 0x15U))),10);
    bufp->fullBit(oldp+19288,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                     [1U] >> 0x14U))));
    bufp->fullBit(oldp+19289,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                     [1U] >> 0x13U))));
    bufp->fullIData(oldp+19290,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__pipeReg
                                 [1U])),19);
    bufp->fullBit(oldp+19291,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                             [0U] >> 0x20U)))));
    bufp->fullIData(oldp+19292,((0x7ffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                     [0U] 
                                                     >> 0xdU)))),19);
    bufp->fullBit(oldp+19293,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                             [0U] >> 0xcU)))));
    bufp->fullSData(oldp+19294,((0x3ffU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                   [0U] 
                                                   >> 2U)))),10);
    bufp->fullCData(oldp+19295,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                              [0U]))),2);
    bufp->fullBit(oldp+19296,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                             [1U] >> 0x20U)))));
    bufp->fullIData(oldp+19297,((0x7ffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                     [1U] 
                                                     >> 0xdU)))),19);
    bufp->fullBit(oldp+19298,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                             [1U] >> 0xcU)))));
    bufp->fullSData(oldp+19299,((0x3ffU & (IData)((
                                                   vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                                   [1U] 
                                                   >> 2U)))),10);
    bufp->fullCData(oldp+19300,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__regBrPred
                                              [1U]))),2);
    bufp->fullIData(oldp+19301,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__fetchAddrOut),32);
    bufp->fullIData(oldp+19302,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19303,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk3__DOT__i),32);
    bufp->fullIData(oldp+19304,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ifStage__DOT__unnamedblk4__DOT__i),32);
    bufp->fullIData(oldp+19305,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intExStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullBit(oldp+19306,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                     [0U] >> 4U))));
    bufp->fullCData(oldp+19307,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                 [0U])),4);
    bufp->fullBit(oldp+19308,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                     [1U] >> 4U))));
    bufp->fullCData(oldp+19309,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__pipeReg
                                 [1U])),4);
    bufp->fullIData(oldp+19310,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intIsStage__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+19311,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRrStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19312,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__intRwStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullQData(oldp+19313,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[3U])) 
                                  << 0x20U) | (QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[2U])))),64);
    bufp->fullIData(oldp+19315,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[3U]),32);
    bufp->fullIData(oldp+19316,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[2U]),32);
    bufp->fullQData(oldp+19317,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[1U])) 
                                  << 0x20U) | (QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[0U])))),64);
    bufp->fullIData(oldp+19319,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[1U]),32);
    bufp->fullIData(oldp+19320,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmReg[0U]),32);
    bufp->fullQData(oldp+19321,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[3U])) 
                                  << 0x20U) | (QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[2U])))),64);
    bufp->fullIData(oldp+19323,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[3U]),32);
    bufp->fullIData(oldp+19324,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[2U]),32);
    bufp->fullQData(oldp+19325,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[1U])) 
                                  << 0x20U) | (QData)((IData)(
                                                              vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[0U])))),64);
    bufp->fullIData(oldp+19327,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[1U]),32);
    bufp->fullIData(oldp+19328,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__tmNext[0U]),32);
    bufp->fullIData(oldp+19329,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__ioUnit__DOT__phyRawWriteAddr),20);
    bufp->fullCData(oldp+19330,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead),4);
    bufp->fullCData(oldp+19331,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail),4);
    bufp->fullCData(oldp+19332,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regCount),5);
    bufp->fullBit(oldp+19333,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0U] >> 0x2aU)))));
    bufp->fullBit(oldp+19334,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0U] >> 0x29U)))));
    bufp->fullIData(oldp+19335,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19336,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0U] >> 0x14U)))));
    bufp->fullBit(oldp+19337,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0U] >> 0x13U)))));
    bufp->fullIData(oldp+19338,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [0U]))),19);
    bufp->fullBit(oldp+19339,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [1U] >> 0x2aU)))));
    bufp->fullBit(oldp+19340,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [1U] >> 0x29U)))));
    bufp->fullIData(oldp+19341,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [1U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19342,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [1U] >> 0x14U)))));
    bufp->fullBit(oldp+19343,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [1U] >> 0x13U)))));
    bufp->fullIData(oldp+19344,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [1U]))),19);
    bufp->fullBit(oldp+19345,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [2U] >> 0x2aU)))));
    bufp->fullBit(oldp+19346,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [2U] >> 0x29U)))));
    bufp->fullIData(oldp+19347,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [2U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19348,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [2U] >> 0x14U)))));
    bufp->fullBit(oldp+19349,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [2U] >> 0x13U)))));
    bufp->fullIData(oldp+19350,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [2U]))),19);
    bufp->fullBit(oldp+19351,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [3U] >> 0x2aU)))));
    bufp->fullBit(oldp+19352,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [3U] >> 0x29U)))));
    bufp->fullIData(oldp+19353,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [3U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19354,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [3U] >> 0x14U)))));
    bufp->fullBit(oldp+19355,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [3U] >> 0x13U)))));
    bufp->fullIData(oldp+19356,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [3U]))),19);
    bufp->fullBit(oldp+19357,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [4U] >> 0x2aU)))));
    bufp->fullBit(oldp+19358,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [4U] >> 0x29U)))));
    bufp->fullIData(oldp+19359,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [4U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19360,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [4U] >> 0x14U)))));
    bufp->fullBit(oldp+19361,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [4U] >> 0x13U)))));
    bufp->fullIData(oldp+19362,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [4U]))),19);
    bufp->fullBit(oldp+19363,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [5U] >> 0x2aU)))));
    bufp->fullBit(oldp+19364,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [5U] >> 0x29U)))));
    bufp->fullIData(oldp+19365,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [5U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19366,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [5U] >> 0x14U)))));
    bufp->fullBit(oldp+19367,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [5U] >> 0x13U)))));
    bufp->fullIData(oldp+19368,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [5U]))),19);
    bufp->fullBit(oldp+19369,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [6U] >> 0x2aU)))));
    bufp->fullBit(oldp+19370,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [6U] >> 0x29U)))));
    bufp->fullIData(oldp+19371,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [6U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19372,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [6U] >> 0x14U)))));
    bufp->fullBit(oldp+19373,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [6U] >> 0x13U)))));
    bufp->fullIData(oldp+19374,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [6U]))),19);
    bufp->fullBit(oldp+19375,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [7U] >> 0x2aU)))));
    bufp->fullBit(oldp+19376,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [7U] >> 0x29U)))));
    bufp->fullIData(oldp+19377,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [7U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19378,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [7U] >> 0x14U)))));
    bufp->fullBit(oldp+19379,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [7U] >> 0x13U)))));
    bufp->fullIData(oldp+19380,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [7U]))),19);
    bufp->fullBit(oldp+19381,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [8U] >> 0x2aU)))));
    bufp->fullBit(oldp+19382,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [8U] >> 0x29U)))));
    bufp->fullIData(oldp+19383,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [8U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19384,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [8U] >> 0x14U)))));
    bufp->fullBit(oldp+19385,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [8U] >> 0x13U)))));
    bufp->fullIData(oldp+19386,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [8U]))),19);
    bufp->fullBit(oldp+19387,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [9U] >> 0x2aU)))));
    bufp->fullBit(oldp+19388,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [9U] >> 0x29U)))));
    bufp->fullIData(oldp+19389,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [9U] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19390,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [9U] >> 0x14U)))));
    bufp->fullBit(oldp+19391,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [9U] >> 0x13U)))));
    bufp->fullIData(oldp+19392,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [9U]))),19);
    bufp->fullBit(oldp+19393,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xaU] 
                                             >> 0x2aU)))));
    bufp->fullBit(oldp+19394,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xaU] 
                                             >> 0x29U)))));
    bufp->fullIData(oldp+19395,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xaU] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19396,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xaU] 
                                             >> 0x14U)))));
    bufp->fullBit(oldp+19397,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xaU] 
                                             >> 0x13U)))));
    bufp->fullIData(oldp+19398,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [0xaU]))),19);
    bufp->fullBit(oldp+19399,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xbU] 
                                             >> 0x2aU)))));
    bufp->fullBit(oldp+19400,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xbU] 
                                             >> 0x29U)))));
    bufp->fullIData(oldp+19401,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xbU] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19402,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xbU] 
                                             >> 0x14U)))));
    bufp->fullBit(oldp+19403,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xbU] 
                                             >> 0x13U)))));
    bufp->fullIData(oldp+19404,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [0xbU]))),19);
    bufp->fullBit(oldp+19405,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xcU] 
                                             >> 0x2aU)))));
    bufp->fullBit(oldp+19406,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xcU] 
                                             >> 0x29U)))));
    bufp->fullIData(oldp+19407,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xcU] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19408,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xcU] 
                                             >> 0x14U)))));
    bufp->fullBit(oldp+19409,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xcU] 
                                             >> 0x13U)))));
    bufp->fullIData(oldp+19410,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [0xcU]))),19);
    bufp->fullBit(oldp+19411,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xdU] 
                                             >> 0x2aU)))));
    bufp->fullBit(oldp+19412,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xdU] 
                                             >> 0x29U)))));
    bufp->fullIData(oldp+19413,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xdU] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19414,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xdU] 
                                             >> 0x14U)))));
    bufp->fullBit(oldp+19415,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xdU] 
                                             >> 0x13U)))));
    bufp->fullIData(oldp+19416,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [0xdU]))),19);
    bufp->fullBit(oldp+19417,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xeU] 
                                             >> 0x2aU)))));
    bufp->fullBit(oldp+19418,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xeU] 
                                             >> 0x29U)))));
    bufp->fullIData(oldp+19419,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xeU] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19420,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xeU] 
                                             >> 0x14U)))));
    bufp->fullBit(oldp+19421,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xeU] 
                                             >> 0x13U)))));
    bufp->fullIData(oldp+19422,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [0xeU]))),19);
    bufp->fullBit(oldp+19423,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xfU] 
                                             >> 0x2aU)))));
    bufp->fullBit(oldp+19424,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xfU] 
                                             >> 0x29U)))));
    bufp->fullIData(oldp+19425,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                     [0xfU] 
                                                     >> 0x15U)))),20);
    bufp->fullBit(oldp+19426,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xfU] 
                                             >> 0x14U)))));
    bufp->fullBit(oldp+19427,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                             [0xfU] 
                                             >> 0x13U)))));
    bufp->fullIData(oldp+19428,((0x7ffffU & (IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueue
                                                    [0xfU]))),19);
    bufp->fullBit(oldp+19429,((1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                            >> 0x15U)))));
    bufp->fullCData(oldp+19430,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                         >> 9U))),4);
    bufp->fullIData(oldp+19431,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19432,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk3__DOT__i),32);
    bufp->fullIData(oldp+19433,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__unnamedblk4__DOT__i),32);
    bufp->fullBit(oldp+19434,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__storeLoadForwardedReg[0]));
    bufp->fullIData(oldp+19435,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__forwardedLoadDataReg[0]),32);
    bufp->fullBit(oldp+19436,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__mshrReadHitReg[0]));
    bufp->fullQData(oldp+19437,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__mshrReadDataReg[0]),64);
    bufp->fullIData(oldp+19439,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__loadAddrReg[0]),32);
    bufp->fullBit(oldp+19440,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__loadMemAccessSizeReg
                                     [0U] >> 2U))));
    bufp->fullCData(oldp+19441,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__loadMemAccessSizeReg
                                 [0U])),2);
    bufp->fullIData(oldp+19442,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadStoreUnit__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+19443,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__maStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19444,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memExStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullBit(oldp+19445,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                     [0U] >> 4U))));
    bufp->fullCData(oldp+19446,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                 [0U])),4);
    bufp->fullBit(oldp+19447,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                     [1U] >> 4U))));
    bufp->fullCData(oldp+19448,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__pipeReg
                                 [1U])),4);
    bufp->fullIData(oldp+19449,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memIsStage__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+19450,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRrStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19451,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memRwStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullCData(oldp+19452,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__reqSerial),2);
    bufp->fullCData(oldp+19453,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__resultSerial),2);
    bufp->fullCData(oldp+19454,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__memoryAccessController__DOT__nextResultSerial),2);
    bufp->fullSData(oldp+19455,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                            [0U][7U] 
                                            << 6U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                              [0U][6U] 
                                              >> 0x1aU)))),10);
    bufp->fullCData(oldp+19456,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][6U] >> 0x18U))),2);
    bufp->fullBit(oldp+19457,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][6U] >> 0x17U))));
    bufp->fullSData(oldp+19458,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][6U] 
                                           >> 0xdU))),10);
    bufp->fullCData(oldp+19459,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][6U] >> 0xbU))),2);
    bufp->fullCData(oldp+19460,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][6U] >> 8U))),3);
    bufp->fullCData(oldp+19461,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][6U] >> 5U))),3);
    bufp->fullCData(oldp+19462,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][6U] >> 3U))),2);
    bufp->fullCData(oldp+19463,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][6U] >> 1U))),2);
    bufp->fullSData(oldp+19464,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                            [0U][6U] 
                                            << 0xbU) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                              [0U][5U] 
                                              >> 0x15U)))),12);
    bufp->fullBit(oldp+19465,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][5U] >> 0x14U))));
    bufp->fullBit(oldp+19466,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][5U] >> 0x13U))));
    bufp->fullBit(oldp+19467,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][5U] >> 0x12U))));
    bufp->fullCData(oldp+19468,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][5U] >> 0x10U))),2);
    bufp->fullCData(oldp+19469,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                          [0U][5U] 
                                          >> 0xbU))),5);
    bufp->fullBit(oldp+19470,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][5U] >> 0xaU))));
    bufp->fullCData(oldp+19471,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][5U] >> 8U))),2);
    bufp->fullCData(oldp+19472,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][5U] >> 5U))),3);
    bufp->fullBit(oldp+19473,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][5U] >> 4U))));
    bufp->fullCData(oldp+19474,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                 [0U][5U])),4);
    bufp->fullCData(oldp+19475,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                 [0U][4U] >> 0x1cU)),4);
    bufp->fullBit(oldp+19476,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][4U] >> 0x1bU))));
    bufp->fullBit(oldp+19477,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][4U] >> 0x1aU))));
    bufp->fullCData(oldp+19478,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                          [0U][4U] 
                                          >> 0x14U))),6);
    bufp->fullCData(oldp+19479,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                         [0U][4U] >> 0x10U))),4);
    bufp->fullCData(oldp+19480,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                         [0U][4U] >> 0xcU))),4);
    bufp->fullBit(oldp+19481,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][4U] >> 0xbU))));
    bufp->fullCData(oldp+19482,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                          [0U][4U] 
                                          >> 5U))),6);
    bufp->fullBit(oldp+19483,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][4U] >> 4U))));
    bufp->fullCData(oldp+19484,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                           [0U][3U] 
                                           >> 0x1eU)))),6);
    bufp->fullBit(oldp+19485,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][3U] >> 0x1dU))));
    bufp->fullCData(oldp+19486,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                          [0U][3U] 
                                          >> 0x17U))),6);
    bufp->fullBit(oldp+19487,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][3U] >> 0x16U))));
    bufp->fullBit(oldp+19488,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][3U] >> 0x15U))));
    bufp->fullCData(oldp+19489,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                          [0U][3U] 
                                          >> 0xfU))),6);
    bufp->fullBit(oldp+19490,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][3U] >> 0xeU))));
    bufp->fullIData(oldp+19491,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                                [0U][2U] 
                                                >> 0x1bU)))),19);
    bufp->fullBit(oldp+19492,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][2U] >> 0x1aU))));
    bufp->fullBit(oldp+19493,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][2U] >> 0x19U))));
    bufp->fullBit(oldp+19494,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][2U] >> 0x18U))));
    bufp->fullIData(oldp+19495,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                  [0U][2U] << 8U) | 
                                 (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                  [0U][1U] >> 0x18U))),32);
    bufp->fullIData(oldp+19496,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                  [0U][1U] << 8U) | 
                                 (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                  [0U][0U] >> 0x18U))),32);
    bufp->fullCData(oldp+19497,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                       [0U][0U] >> 0x16U))),2);
    bufp->fullBit(oldp+19498,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][0U] >> 0x15U))));
    bufp->fullBit(oldp+19499,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                     [0U][0U] >> 0x14U))));
    bufp->fullIData(oldp+19500,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldPipeReg
                                 [0U][0U])),20);
    bufp->fullSData(oldp+19501,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                            [0U][7U] 
                                            << 6U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                              [0U][6U] 
                                              >> 0x1aU)))),10);
    bufp->fullCData(oldp+19502,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][6U] >> 0x18U))),2);
    bufp->fullBit(oldp+19503,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][6U] >> 0x17U))));
    bufp->fullSData(oldp+19504,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][6U] 
                                           >> 0xdU))),10);
    bufp->fullCData(oldp+19505,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][6U] >> 0xbU))),2);
    bufp->fullCData(oldp+19506,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][6U] >> 8U))),3);
    bufp->fullCData(oldp+19507,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][6U] >> 5U))),3);
    bufp->fullCData(oldp+19508,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][6U] >> 3U))),2);
    bufp->fullCData(oldp+19509,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][6U] >> 1U))),2);
    bufp->fullSData(oldp+19510,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                            [0U][6U] 
                                            << 0xbU) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                              [0U][5U] 
                                              >> 0x15U)))),12);
    bufp->fullBit(oldp+19511,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][5U] >> 0x14U))));
    bufp->fullBit(oldp+19512,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][5U] >> 0x13U))));
    bufp->fullBit(oldp+19513,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][5U] >> 0x12U))));
    bufp->fullCData(oldp+19514,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][5U] >> 0x10U))),2);
    bufp->fullCData(oldp+19515,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                          [0U][5U] 
                                          >> 0xbU))),5);
    bufp->fullBit(oldp+19516,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][5U] >> 0xaU))));
    bufp->fullCData(oldp+19517,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][5U] >> 8U))),2);
    bufp->fullCData(oldp+19518,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][5U] >> 5U))),3);
    bufp->fullBit(oldp+19519,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][5U] >> 4U))));
    bufp->fullCData(oldp+19520,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                 [0U][5U])),4);
    bufp->fullCData(oldp+19521,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                 [0U][4U] >> 0x1cU)),4);
    bufp->fullBit(oldp+19522,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][4U] >> 0x1bU))));
    bufp->fullBit(oldp+19523,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][4U] >> 0x1aU))));
    bufp->fullCData(oldp+19524,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                          [0U][4U] 
                                          >> 0x14U))),6);
    bufp->fullCData(oldp+19525,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                         [0U][4U] >> 0x10U))),4);
    bufp->fullCData(oldp+19526,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                         [0U][4U] >> 0xcU))),4);
    bufp->fullBit(oldp+19527,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][4U] >> 0xbU))));
    bufp->fullCData(oldp+19528,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                          [0U][4U] 
                                          >> 5U))),6);
    bufp->fullBit(oldp+19529,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][4U] >> 4U))));
    bufp->fullCData(oldp+19530,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                           [0U][3U] 
                                           >> 0x1eU)))),6);
    bufp->fullBit(oldp+19531,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][3U] >> 0x1dU))));
    bufp->fullCData(oldp+19532,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                          [0U][3U] 
                                          >> 0x17U))),6);
    bufp->fullBit(oldp+19533,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][3U] >> 0x16U))));
    bufp->fullBit(oldp+19534,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][3U] >> 0x15U))));
    bufp->fullCData(oldp+19535,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                          [0U][3U] 
                                          >> 0xfU))),6);
    bufp->fullBit(oldp+19536,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][3U] >> 0xeU))));
    bufp->fullIData(oldp+19537,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                              [0U][3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                                [0U][2U] 
                                                >> 0x1bU)))),19);
    bufp->fullBit(oldp+19538,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][2U] >> 0x1aU))));
    bufp->fullBit(oldp+19539,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][2U] >> 0x19U))));
    bufp->fullBit(oldp+19540,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][2U] >> 0x18U))));
    bufp->fullIData(oldp+19541,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                  [0U][2U] << 8U) | 
                                 (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                  [0U][1U] >> 0x18U))),32);
    bufp->fullIData(oldp+19542,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                  [0U][1U] << 8U) | 
                                 (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                  [0U][0U] >> 0x18U))),32);
    bufp->fullCData(oldp+19543,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                       [0U][0U] >> 0x16U))),2);
    bufp->fullBit(oldp+19544,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][0U] >> 0x15U))));
    bufp->fullBit(oldp+19545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                     [0U][0U] >> 0x14U))));
    bufp->fullIData(oldp+19546,((0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stPipeReg
                                 [0U][0U])),20);
    bufp->fullSData(oldp+19547,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                           [0U][3U] 
                                           >> 0x13U))),10);
    bufp->fullCData(oldp+19548,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][3U] >> 0x11U))),2);
    bufp->fullCData(oldp+19549,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][3U] >> 0xeU))),3);
    bufp->fullCData(oldp+19550,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][3U] >> 0xbU))),3);
    bufp->fullCData(oldp+19551,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][3U] >> 9U))),2);
    bufp->fullCData(oldp+19552,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][3U] >> 7U))),2);
    bufp->fullSData(oldp+19553,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                            [0U][3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                              [0U][2U] 
                                              >> 0x1bU)))),12);
    bufp->fullBit(oldp+19554,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][2U] >> 0x1aU))));
    bufp->fullBit(oldp+19555,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][2U] >> 0x19U))));
    bufp->fullBit(oldp+19556,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][2U] >> 0x18U))));
    bufp->fullCData(oldp+19557,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0x16U))),2);
    bufp->fullCData(oldp+19558,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                          [0U][2U] 
                                          >> 0x11U))),5);
    bufp->fullBit(oldp+19559,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][2U] >> 0x10U))));
    bufp->fullCData(oldp+19560,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0xeU))),2);
    bufp->fullCData(oldp+19561,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                       [0U][2U] >> 0xbU))),3);
    bufp->fullBit(oldp+19562,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][2U] >> 0xaU))));
    bufp->fullCData(oldp+19563,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][2U] >> 6U))),4);
    bufp->fullCData(oldp+19564,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][2U] >> 2U))),4);
    bufp->fullBit(oldp+19565,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][2U] >> 1U))));
    bufp->fullBit(oldp+19566,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                               [0U][2U])));
    bufp->fullCData(oldp+19567,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                 [0U][1U] >> 0x1aU)),6);
    bufp->fullCData(oldp+19568,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][1U] >> 0x16U))),4);
    bufp->fullCData(oldp+19569,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                         [0U][1U] >> 0x12U))),4);
    bufp->fullBit(oldp+19570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][1U] >> 0x11U))));
    bufp->fullCData(oldp+19571,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                          [0U][1U] 
                                          >> 0xbU))),6);
    bufp->fullBit(oldp+19572,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][1U] >> 0xaU))));
    bufp->fullCData(oldp+19573,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                          [0U][1U] 
                                          >> 4U))),6);
    bufp->fullBit(oldp+19574,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][1U] >> 3U))));
    bufp->fullCData(oldp+19575,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                           [0U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                           [0U][0U] 
                                           >> 0x1dU)))),6);
    bufp->fullBit(oldp+19576,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][0U] >> 0x1cU))));
    bufp->fullBit(oldp+19577,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][0U] >> 0x1bU))));
    bufp->fullCData(oldp+19578,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                          [0U][0U] 
                                          >> 0x15U))),6);
    bufp->fullBit(oldp+19579,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                     [0U][0U] >> 0x14U))));
    bufp->fullIData(oldp+19580,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                                             [0U][0U] 
                                             >> 1U))),19);
    bufp->fullBit(oldp+19581,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__ldIqData
                               [0U][0U])));
    bufp->fullSData(oldp+19582,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                           [0U][3U] 
                                           >> 0x13U))),10);
    bufp->fullCData(oldp+19583,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][3U] >> 0x11U))),2);
    bufp->fullCData(oldp+19584,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][3U] >> 0xeU))),3);
    bufp->fullCData(oldp+19585,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][3U] >> 0xbU))),3);
    bufp->fullCData(oldp+19586,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][3U] >> 9U))),2);
    bufp->fullCData(oldp+19587,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][3U] >> 7U))),2);
    bufp->fullSData(oldp+19588,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                            [0U][3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                              [0U][2U] 
                                              >> 0x1bU)))),12);
    bufp->fullBit(oldp+19589,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][2U] >> 0x1aU))));
    bufp->fullBit(oldp+19590,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][2U] >> 0x19U))));
    bufp->fullBit(oldp+19591,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][2U] >> 0x18U))));
    bufp->fullCData(oldp+19592,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0x16U))),2);
    bufp->fullCData(oldp+19593,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                          [0U][2U] 
                                          >> 0x11U))),5);
    bufp->fullBit(oldp+19594,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][2U] >> 0x10U))));
    bufp->fullCData(oldp+19595,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0xeU))),2);
    bufp->fullCData(oldp+19596,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                       [0U][2U] >> 0xbU))),3);
    bufp->fullBit(oldp+19597,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][2U] >> 0xaU))));
    bufp->fullCData(oldp+19598,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][2U] >> 6U))),4);
    bufp->fullCData(oldp+19599,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][2U] >> 2U))),4);
    bufp->fullBit(oldp+19600,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][2U] >> 1U))));
    bufp->fullBit(oldp+19601,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                               [0U][2U])));
    bufp->fullCData(oldp+19602,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                 [0U][1U] >> 0x1aU)),6);
    bufp->fullCData(oldp+19603,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][1U] >> 0x16U))),4);
    bufp->fullCData(oldp+19604,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                         [0U][1U] >> 0x12U))),4);
    bufp->fullBit(oldp+19605,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][1U] >> 0x11U))));
    bufp->fullCData(oldp+19606,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                          [0U][1U] 
                                          >> 0xbU))),6);
    bufp->fullBit(oldp+19607,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][1U] >> 0xaU))));
    bufp->fullCData(oldp+19608,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                          [0U][1U] 
                                          >> 4U))),6);
    bufp->fullBit(oldp+19609,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][1U] >> 3U))));
    bufp->fullCData(oldp+19610,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                           [0U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                           [0U][0U] 
                                           >> 0x1dU)))),6);
    bufp->fullBit(oldp+19611,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][0U] >> 0x1cU))));
    bufp->fullBit(oldp+19612,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][0U] >> 0x1bU))));
    bufp->fullCData(oldp+19613,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                          [0U][0U] 
                                          >> 0x15U))),6);
    bufp->fullBit(oldp+19614,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                     [0U][0U] >> 0x14U))));
    bufp->fullIData(oldp+19615,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                                             [0U][0U] 
                                             >> 1U))),19);
    bufp->fullBit(oldp+19616,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__stIqData
                               [0U][0U])));
    bufp->fullIData(oldp+19617,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mtStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullCData(oldp+19618,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__regPhase
                                [0U]),2);
    bufp->fullCData(oldp+19619,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__regActiveListPtr[0]),6);
    bufp->fullBit(oldp+19620,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regIsSigned));
    bufp->fullIData(oldp+19621,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDividend),32);
    bufp->fullIData(oldp+19622,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivisor),32);
    bufp->fullCData(oldp+19623,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivCode),2);
    bufp->fullQData(oldp+19624,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ),33);
    bufp->fullQData(oldp+19626,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD),33);
    bufp->fullQData(oldp+19628,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ),33);
    bufp->fullQData(oldp+19630,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR),33);
    bufp->fullBit(oldp+19632,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regSigned));
    bufp->fullCData(oldp+19633,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter),6);
    bufp->fullCData(oldp+19634,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase),2);
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
    bufp->fullWData(oldp+19635,(__Vtemp_38),66);
    bufp->fullQData(oldp+19638,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcA_Reg),33);
    bufp->fullQData(oldp+19640,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcB_Reg),33);
    bufp->fullIData(oldp+19642,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+19643,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19644,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+19645,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+19646,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__mulDivUnit__DOT__unnamedblk1__DOT__i),32);
    bufp->fullBit(oldp+19647,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__threadCounter));
    bufp->fullSData(oldp+19648,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__sidFF__DOT__body),10);
    bufp->fullBit(oldp+19649,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__npStage__DOT__regStall));
    bufp->fullBit(oldp+19650,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                     [0U] >> 0x13U))));
    bufp->fullIData(oldp+19651,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                 [0U])),19);
    bufp->fullBit(oldp+19652,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                     [1U] >> 0x13U))));
    bufp->fullIData(oldp+19653,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pc__DOT__pcRegs
                                 [1U])),19);
    bufp->fullIData(oldp+19654,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__pc[0]),32);
    bufp->fullIData(oldp+19655,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__pc[1]),32);
    bufp->fullBit(oldp+19656,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC[0]));
    bufp->fullBit(oldp+19657,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC[1]));
    bufp->fullCData(oldp+19658,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 9U))),3);
    bufp->fullCData(oldp+19659,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+19660,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 4U))),3);
    bufp->fullBit(oldp+19661,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 3U))));
    bufp->fullCData(oldp+19662,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                           >> 0x1eU)))),5);
    bufp->fullBit(oldp+19663,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 0x1dU))));
    bufp->fullCData(oldp+19664,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                          >> 0x18U))),5);
    bufp->fullBit(oldp+19665,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 0x17U))));
    bufp->fullCData(oldp+19666,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                          >> 0x12U))),5);
    bufp->fullCData(oldp+19667,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                         >> 0xeU))),4);
    bufp->fullBit(oldp+19668,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 0xdU))));
    bufp->fullIData(oldp+19669,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                   >> 0xfU)))),30);
    bufp->fullBit(oldp+19670,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+19671,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+19672,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+19673,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+19674,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                          >> 8U))),5);
    bufp->fullBit(oldp+19675,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 7U))));
    bufp->fullCData(oldp+19676,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 5U))),2);
    bufp->fullSData(oldp+19677,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                              >> 0x1bU)))),10);
    bufp->fullSData(oldp+19678,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                           >> 0xfU))),12);
    bufp->fullSData(oldp+19679,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            >> 3U))),15);
    bufp->fullIData(oldp+19680,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                >> 0xfU)))),20);
    bufp->fullCData(oldp+19681,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0xfU))),2);
    bufp->fullSData(oldp+19682,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                               >> 0x1dU)))),16);
    bufp->fullSData(oldp+19683,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                            >> 0xfU))),14);
    bufp->fullSData(oldp+19684,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                            >> 1U))),15);
    bufp->fullIData(oldp+19685,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                >> 0xfU)))),18);
    bufp->fullCData(oldp+19686,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 0xfU))),3);
    bufp->fullBit(oldp+19687,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                     >> 0xeU))));
    bufp->fullIData(oldp+19688,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                >> 0x1bU)))),19);
    bufp->fullCData(oldp+19689,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                          >> 0xcU))),5);
    bufp->fullCData(oldp+19690,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                          >> 7U))),5);
    bufp->fullCData(oldp+19691,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                       >> 4U))),3);
    bufp->fullIData(oldp+19692,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[1U] 
                                               << 0x11U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                                 >> 0xfU)))),21);
    bufp->fullCData(oldp+19693,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+19694,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+19695,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 9U))),2);
    bufp->fullBit(oldp+19696,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                     >> 8U))));
    bufp->fullBit(oldp+19697,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                     >> 7U))));
    bufp->fullBit(oldp+19698,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                     >> 6U))));
    bufp->fullBit(oldp+19699,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                     >> 5U))));
    bufp->fullBit(oldp+19700,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                     >> 4U))));
    bufp->fullBit(oldp+19701,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                     >> 3U))));
    bufp->fullCData(oldp+19702,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+19703,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0U])));
    bufp->fullCData(oldp+19704,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x15U))),3);
    bufp->fullCData(oldp+19705,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x13U))),2);
    bufp->fullCData(oldp+19706,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x10U))),3);
    bufp->fullBit(oldp+19707,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+19708,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                          >> 0xaU))),5);
    bufp->fullBit(oldp+19709,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 9U))));
    bufp->fullCData(oldp+19710,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                          >> 4U))),5);
    bufp->fullBit(oldp+19711,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 3U))));
    bufp->fullCData(oldp+19712,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                           >> 0x1eU)))),5);
    bufp->fullCData(oldp+19713,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                         >> 0x1aU))),4);
    bufp->fullBit(oldp+19714,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                     >> 0x19U))));
    bufp->fullIData(oldp+19715,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                   >> 0x1bU)))),30);
    bufp->fullBit(oldp+19716,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+19717,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+19718,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+19719,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+19720,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                          >> 0x14U))),5);
    bufp->fullBit(oldp+19721,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19722,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x11U))),2);
    bufp->fullSData(oldp+19723,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                           >> 7U))),10);
    bufp->fullSData(oldp+19724,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                              >> 0x1bU)))),12);
    bufp->fullSData(oldp+19725,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                            >> 0xfU))),15);
    bufp->fullIData(oldp+19726,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                >> 0x1bU)))),20);
    bufp->fullCData(oldp+19727,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x1bU))),2);
    bufp->fullSData(oldp+19728,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                            >> 9U))),16);
    bufp->fullSData(oldp+19729,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                               >> 0x1bU)))),14);
    bufp->fullSData(oldp+19730,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                            >> 0xdU))),15);
    bufp->fullIData(oldp+19731,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                >> 0x1bU)))),18);
    bufp->fullCData(oldp+19732,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x1bU))),3);
    bufp->fullBit(oldp+19733,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                     >> 0x1aU))));
    bufp->fullIData(oldp+19734,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                             >> 7U))),19);
    bufp->fullCData(oldp+19735,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                          >> 0x18U))),5);
    bufp->fullCData(oldp+19736,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                          >> 0x13U))),5);
    bufp->fullCData(oldp+19737,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                       >> 0x10U))),3);
    bufp->fullIData(oldp+19738,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                                 >> 0x1bU)))),21);
    bufp->fullCData(oldp+19739,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+19740,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+19741,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0x15U))),2);
    bufp->fullBit(oldp+19742,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+19743,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 0x13U))));
    bufp->fullBit(oldp+19744,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 0x12U))));
    bufp->fullBit(oldp+19745,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+19746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+19747,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+19748,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                       >> 0xdU))),2);
    bufp->fullBit(oldp+19749,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[2U] 
                                     >> 0xcU))));
    bufp->fullCData(oldp+19750,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 1U))),3);
    bufp->fullCData(oldp+19751,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                                  >> 0x1fU)))),2);
    bufp->fullCData(oldp+19752,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 0x1cU))),3);
    bufp->fullBit(oldp+19753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+19754,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                          >> 0x16U))),5);
    bufp->fullBit(oldp+19755,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 0x15U))));
    bufp->fullCData(oldp+19756,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                          >> 0x10U))),5);
    bufp->fullBit(oldp+19757,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+19758,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                          >> 0xaU))),5);
    bufp->fullCData(oldp+19759,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                         >> 6U))),4);
    bufp->fullBit(oldp+19760,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 5U))));
    bufp->fullIData(oldp+19761,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                                 << 0x19U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                   >> 7U)))),30);
    bufp->fullBit(oldp+19762,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 9U))));
    bufp->fullBit(oldp+19763,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 8U))));
    bufp->fullBit(oldp+19764,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 7U))));
    bufp->fullCData(oldp+19765,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+19766,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U])),5);
    bufp->fullBit(oldp+19767,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+19768,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                       >> 0x1dU))),2);
    bufp->fullSData(oldp+19769,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                           >> 0x13U))),10);
    bufp->fullSData(oldp+19770,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                           >> 7U))),12);
    bufp->fullSData(oldp+19771,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                               >> 0x1bU)))),15);
    bufp->fullIData(oldp+19772,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                             >> 7U))),20);
    bufp->fullCData(oldp+19773,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 7U))),2);
    bufp->fullSData(oldp+19774,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                             << 0xbU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                               >> 0x15U)))),16);
    bufp->fullSData(oldp+19775,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                            >> 7U))),14);
    bufp->fullSData(oldp+19776,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                               >> 0x19U)))),15);
    bufp->fullIData(oldp+19777,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                             >> 7U))),18);
    bufp->fullCData(oldp+19778,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                       >> 7U))),3);
    bufp->fullBit(oldp+19779,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                     >> 6U))));
    bufp->fullIData(oldp+19780,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                                >> 0x13U)))),19);
    bufp->fullCData(oldp+19781,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                          >> 4U))),5);
    bufp->fullCData(oldp+19782,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[6U] 
                                           << 1U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                           >> 0x1fU)))),5);
    bufp->fullCData(oldp+19783,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                       >> 0x1cU))),3);
    bufp->fullIData(oldp+19784,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                              >> 7U))),21);
    bufp->fullCData(oldp+19785,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+19786,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                       >> 3U))),2);
    bufp->fullCData(oldp+19787,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+19788,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[5U])));
    bufp->fullBit(oldp+19789,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                               >> 0x1fU)));
    bufp->fullBit(oldp+19790,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 0x1eU))));
    bufp->fullBit(oldp+19791,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+19792,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+19793,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+19794,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                       >> 0x19U))),2);
    bufp->fullBit(oldp+19795,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[4U] 
                                     >> 0x18U))));
    bufp->fullCData(oldp+19796,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0xdU))),3);
    bufp->fullCData(oldp+19797,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+19798,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 8U))),3);
    bufp->fullBit(oldp+19799,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 7U))));
    bufp->fullCData(oldp+19800,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                          >> 2U))),5);
    bufp->fullBit(oldp+19801,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 1U))));
    bufp->fullCData(oldp+19802,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                           << 4U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                           >> 0x1cU)))),5);
    bufp->fullBit(oldp+19803,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+19804,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                          >> 0x16U))),5);
    bufp->fullCData(oldp+19805,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                         >> 0x12U))),4);
    bufp->fullBit(oldp+19806,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                     >> 0x11U))));
    bufp->fullIData(oldp+19807,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                                 << 0xdU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                   >> 0x13U)))),30);
    bufp->fullBit(oldp+19808,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                     >> 0x15U))));
    bufp->fullBit(oldp+19809,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+19810,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19811,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x11U))),2);
    bufp->fullCData(oldp+19812,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                          >> 0xcU))),5);
    bufp->fullBit(oldp+19813,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                     >> 0xbU))));
    bufp->fullCData(oldp+19814,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 9U))),2);
    bufp->fullSData(oldp+19815,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                              >> 0x1fU)))),10);
    bufp->fullSData(oldp+19816,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                           >> 0x13U))),12);
    bufp->fullSData(oldp+19817,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            >> 7U))),15);
    bufp->fullIData(oldp+19818,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                >> 0x13U)))),20);
    bufp->fullCData(oldp+19819,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x13U))),2);
    bufp->fullSData(oldp+19820,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            >> 1U))),16);
    bufp->fullSData(oldp+19821,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                             << 0xdU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                               >> 0x13U)))),14);
    bufp->fullSData(oldp+19822,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                            >> 5U))),15);
    bufp->fullIData(oldp+19823,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                >> 0x13U)))),18);
    bufp->fullCData(oldp+19824,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 0x13U))),3);
    bufp->fullBit(oldp+19825,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                     >> 0x12U))));
    bufp->fullIData(oldp+19826,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                >> 0x1fU)))),19);
    bufp->fullCData(oldp+19827,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                          >> 0x10U))),5);
    bufp->fullCData(oldp+19828,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                          >> 0xbU))),5);
    bufp->fullCData(oldp+19829,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                       >> 8U))),3);
    bufp->fullIData(oldp+19830,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[8U] 
                                               << 0xdU) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                                 >> 0x13U)))),21);
    bufp->fullCData(oldp+19831,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 0x11U))),2);
    bufp->fullCData(oldp+19832,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 0xfU))),2);
    bufp->fullCData(oldp+19833,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 0xdU))),2);
    bufp->fullBit(oldp+19834,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                     >> 0xcU))));
    bufp->fullBit(oldp+19835,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                     >> 0xbU))));
    bufp->fullBit(oldp+19836,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                     >> 0xaU))));
    bufp->fullBit(oldp+19837,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                     >> 9U))));
    bufp->fullBit(oldp+19838,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                     >> 8U))));
    bufp->fullBit(oldp+19839,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                     >> 7U))));
    bufp->fullCData(oldp+19840,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                       >> 5U))),2);
    bufp->fullBit(oldp+19841,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[7U] 
                                     >> 4U))));
    bufp->fullCData(oldp+19842,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 0x19U))),3);
    bufp->fullCData(oldp+19843,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+19844,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 0x14U))),3);
    bufp->fullBit(oldp+19845,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19846,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                          >> 0xeU))),5);
    bufp->fullBit(oldp+19847,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                     >> 0xdU))));
    bufp->fullCData(oldp+19848,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                          >> 8U))),5);
    bufp->fullBit(oldp+19849,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                     >> 7U))));
    bufp->fullCData(oldp+19850,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                          >> 2U))),5);
    bufp->fullCData(oldp+19851,((0xfU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                          << 2U) | 
                                         (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                          >> 0x1eU)))),4);
    bufp->fullBit(oldp+19852,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                     >> 0x1dU))));
    bufp->fullIData(oldp+19853,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                 << 1U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                   >> 0x1fU)))),30);
    bufp->fullBit(oldp+19854,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                     >> 1U))));
    bufp->fullBit(oldp+19855,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU])));
    bufp->fullBit(oldp+19856,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+19857,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                       >> 0x1dU))),2);
    bufp->fullCData(oldp+19858,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                          >> 0x18U))),5);
    bufp->fullBit(oldp+19859,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                     >> 0x17U))));
    bufp->fullCData(oldp+19860,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                       >> 0x15U))),2);
    bufp->fullSData(oldp+19861,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                           >> 0xbU))),10);
    bufp->fullSData(oldp+19862,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                            << 1U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                              >> 0x1fU)))),12);
    bufp->fullSData(oldp+19863,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                             << 0xdU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                               >> 0x13U)))),15);
    bufp->fullIData(oldp+19864,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                >> 0x1fU)))),20);
    bufp->fullCData(oldp+19865,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                  >> 0x1fU)))),2);
    bufp->fullSData(oldp+19866,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                            >> 0xdU))),16);
    bufp->fullSData(oldp+19867,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                               >> 0x1fU)))),14);
    bufp->fullSData(oldp+19868,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                 >> 0x11U)),15);
    bufp->fullIData(oldp+19869,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                >> 0x1fU)))),18);
    bufp->fullCData(oldp+19870,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                                  >> 0x1fU)))),3);
    bufp->fullBit(oldp+19871,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                     >> 0x1eU))));
    bufp->fullIData(oldp+19872,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                             >> 0xbU))),19);
    bufp->fullCData(oldp+19873,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                           << 4U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                           >> 0x1cU)))),5);
    bufp->fullCData(oldp+19874,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                          >> 0x17U))),5);
    bufp->fullCData(oldp+19875,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                       >> 0x14U))),3);
    bufp->fullIData(oldp+19876,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xaU] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                                 >> 0x1fU)))),21);
    bufp->fullCData(oldp+19877,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x1dU))),2);
    bufp->fullCData(oldp+19878,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x1bU))),2);
    bufp->fullCData(oldp+19879,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x19U))),2);
    bufp->fullBit(oldp+19880,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 0x18U))));
    bufp->fullBit(oldp+19881,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 0x17U))));
    bufp->fullBit(oldp+19882,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 0x16U))));
    bufp->fullBit(oldp+19883,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 0x15U))));
    bufp->fullBit(oldp+19884,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+19885,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19886,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                       >> 0x11U))),2);
    bufp->fullBit(oldp+19887,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[9U] 
                                     >> 0x10U))));
    bufp->fullCData(oldp+19888,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xeU] 
                                       >> 5U))),3);
    bufp->fullCData(oldp+19889,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xeU] 
                                       >> 3U))),2);
    bufp->fullCData(oldp+19890,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xeU])),3);
    bufp->fullBit(oldp+19891,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+19892,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                          >> 0x1aU))),5);
    bufp->fullBit(oldp+19893,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 0x19U))));
    bufp->fullCData(oldp+19894,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                          >> 0x14U))),5);
    bufp->fullBit(oldp+19895,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+19896,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                          >> 0xeU))),5);
    bufp->fullCData(oldp+19897,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                         >> 0xaU))),4);
    bufp->fullBit(oldp+19898,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 9U))));
    bufp->fullIData(oldp+19899,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                                 << 0x15U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                   >> 0xbU)))),30);
    bufp->fullBit(oldp+19900,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 0xdU))));
    bufp->fullBit(oldp+19901,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 0xcU))));
    bufp->fullBit(oldp+19902,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 0xbU))));
    bufp->fullCData(oldp+19903,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 9U))),2);
    bufp->fullCData(oldp+19904,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                          >> 4U))),5);
    bufp->fullBit(oldp+19905,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 3U))));
    bufp->fullCData(oldp+19906,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 1U))),2);
    bufp->fullSData(oldp+19907,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                            << 9U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                              >> 0x17U)))),10);
    bufp->fullSData(oldp+19908,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                           >> 0xbU))),12);
    bufp->fullSData(oldp+19909,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                             << 1U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                               >> 0x1fU)))),15);
    bufp->fullIData(oldp+19910,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                             >> 0xbU))),20);
    bufp->fullCData(oldp+19911,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0xbU))),2);
    bufp->fullSData(oldp+19912,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                               >> 0x19U)))),16);
    bufp->fullSData(oldp+19913,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                            >> 0xbU))),14);
    bufp->fullSData(oldp+19914,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                               >> 0x1dU)))),15);
    bufp->fullIData(oldp+19915,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                             >> 0xbU))),18);
    bufp->fullCData(oldp+19916,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                       >> 0xbU))),3);
    bufp->fullBit(oldp+19917,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                     >> 0xaU))));
    bufp->fullIData(oldp+19918,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                                >> 0x17U)))),19);
    bufp->fullCData(oldp+19919,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                          >> 8U))),5);
    bufp->fullCData(oldp+19920,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU] 
                                          >> 3U))),5);
    bufp->fullCData(oldp+19921,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xdU])),3);
    bufp->fullIData(oldp+19922,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                 >> 0xbU)),21);
    bufp->fullCData(oldp+19923,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                       >> 9U))),2);
    bufp->fullCData(oldp+19924,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+19925,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                       >> 5U))),2);
    bufp->fullBit(oldp+19926,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                     >> 4U))));
    bufp->fullBit(oldp+19927,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                     >> 3U))));
    bufp->fullBit(oldp+19928,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                     >> 2U))));
    bufp->fullBit(oldp+19929,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU] 
                                     >> 1U))));
    bufp->fullBit(oldp+19930,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xcU])));
    bufp->fullBit(oldp+19931,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+19932,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                       >> 0x1dU))),2);
    bufp->fullBit(oldp+19933,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__microOps[0xbU] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+19934,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 4U))));
    bufp->fullBit(oldp+19935,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 3U))));
    bufp->fullBit(oldp+19936,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 2U))));
    bufp->fullBit(oldp+19937,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 1U))));
    bufp->fullBit(oldp+19938,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo))));
    bufp->fullBit(oldp+19939,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 9U))));
    bufp->fullBit(oldp+19940,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 8U))));
    bufp->fullBit(oldp+19941,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 7U))));
    bufp->fullBit(oldp+19942,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 6U))));
    bufp->fullBit(oldp+19943,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__insnInfo) 
                                     >> 5U))));
    bufp->fullBit(oldp+19944,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__empty));
    bufp->fullBit(oldp+19945,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC
                              [0U]));
    bufp->fullCData(oldp+19946,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 9U))),3);
    bufp->fullCData(oldp+19947,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+19948,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 4U))),3);
    bufp->fullBit(oldp+19949,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 3U))));
    bufp->fullCData(oldp+19950,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                           >> 0x1eU)))),5);
    bufp->fullBit(oldp+19951,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 0x1dU))));
    bufp->fullCData(oldp+19952,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                          >> 0x18U))),5);
    bufp->fullBit(oldp+19953,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 0x17U))));
    bufp->fullCData(oldp+19954,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                          >> 0x12U))),5);
    bufp->fullCData(oldp+19955,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                         >> 0xeU))),4);
    bufp->fullBit(oldp+19956,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 0xdU))));
    bufp->fullIData(oldp+19957,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                   >> 0xfU)))),30);
    bufp->fullBit(oldp+19958,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+19959,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+19960,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+19961,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+19962,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                          >> 8U))),5);
    bufp->fullBit(oldp+19963,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 7U))));
    bufp->fullCData(oldp+19964,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 5U))),2);
    bufp->fullSData(oldp+19965,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                              >> 0x1bU)))),10);
    bufp->fullSData(oldp+19966,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                           >> 0xfU))),12);
    bufp->fullSData(oldp+19967,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            >> 3U))),15);
    bufp->fullIData(oldp+19968,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                >> 0xfU)))),20);
    bufp->fullCData(oldp+19969,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0xfU))),2);
    bufp->fullSData(oldp+19970,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                               >> 0x1dU)))),16);
    bufp->fullSData(oldp+19971,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                            >> 0xfU))),14);
    bufp->fullSData(oldp+19972,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                            >> 1U))),15);
    bufp->fullIData(oldp+19973,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                >> 0xfU)))),18);
    bufp->fullCData(oldp+19974,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 0xfU))),3);
    bufp->fullBit(oldp+19975,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                     >> 0xeU))));
    bufp->fullIData(oldp+19976,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                >> 0x1bU)))),19);
    bufp->fullCData(oldp+19977,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                          >> 0xcU))),5);
    bufp->fullCData(oldp+19978,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                          >> 7U))),5);
    bufp->fullCData(oldp+19979,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                       >> 4U))),3);
    bufp->fullIData(oldp+19980,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
                                               << 0x11U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                                 >> 0xfU)))),21);
    bufp->fullCData(oldp+19981,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+19982,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+19983,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 9U))),2);
    bufp->fullBit(oldp+19984,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                     >> 8U))));
    bufp->fullBit(oldp+19985,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                     >> 7U))));
    bufp->fullBit(oldp+19986,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                     >> 6U))));
    bufp->fullBit(oldp+19987,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                     >> 5U))));
    bufp->fullBit(oldp+19988,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                     >> 4U))));
    bufp->fullBit(oldp+19989,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                     >> 3U))));
    bufp->fullCData(oldp+19990,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+19991,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U])));
    bufp->fullCData(oldp+19992,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x15U))),3);
    bufp->fullCData(oldp+19993,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x13U))),2);
    bufp->fullCData(oldp+19994,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x10U))),3);
    bufp->fullBit(oldp+19995,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+19996,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                          >> 0xaU))),5);
    bufp->fullBit(oldp+19997,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 9U))));
    bufp->fullCData(oldp+19998,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                          >> 4U))),5);
    bufp->fullBit(oldp+19999,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20000,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                           >> 0x1eU)))),5);
    bufp->fullCData(oldp+20001,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                         >> 0x1aU))),4);
    bufp->fullBit(oldp+20002,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                     >> 0x19U))));
    bufp->fullIData(oldp+20003,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                   >> 0x1bU)))),30);
    bufp->fullBit(oldp+20004,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+20005,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+20006,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+20007,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+20008,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                          >> 0x14U))),5);
    bufp->fullBit(oldp+20009,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+20010,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x11U))),2);
    bufp->fullSData(oldp+20011,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                           >> 7U))),10);
    bufp->fullSData(oldp+20012,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                              >> 0x1bU)))),12);
    bufp->fullSData(oldp+20013,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                            >> 0xfU))),15);
    bufp->fullIData(oldp+20014,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                >> 0x1bU)))),20);
    bufp->fullCData(oldp+20015,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1bU))),2);
    bufp->fullSData(oldp+20016,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                            >> 9U))),16);
    bufp->fullSData(oldp+20017,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                               >> 0x1bU)))),14);
    bufp->fullSData(oldp+20018,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                            >> 0xdU))),15);
    bufp->fullIData(oldp+20019,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                >> 0x1bU)))),18);
    bufp->fullCData(oldp+20020,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1bU))),3);
    bufp->fullBit(oldp+20021,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1aU))));
    bufp->fullIData(oldp+20022,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                             >> 7U))),19);
    bufp->fullCData(oldp+20023,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                          >> 0x18U))),5);
    bufp->fullCData(oldp+20024,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                          >> 0x13U))),5);
    bufp->fullCData(oldp+20025,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                       >> 0x10U))),3);
    bufp->fullIData(oldp+20026,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                                 >> 0x1bU)))),21);
    bufp->fullCData(oldp+20027,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+20028,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+20029,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0x15U))),2);
    bufp->fullBit(oldp+20030,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+20031,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 0x13U))));
    bufp->fullBit(oldp+20032,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 0x12U))));
    bufp->fullBit(oldp+20033,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+20034,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+20035,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20036,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                       >> 0xdU))),2);
    bufp->fullBit(oldp+20037,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
                                     >> 0xcU))));
    bufp->fullCData(oldp+20038,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[7U] 
                                       >> 1U))),3);
    bufp->fullCData(oldp+20039,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[7U] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                                  >> 0x1fU)))),2);
    bufp->fullCData(oldp+20040,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 0x1cU))),3);
    bufp->fullBit(oldp+20041,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+20042,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                          >> 0x16U))),5);
    bufp->fullBit(oldp+20043,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 0x15U))));
    bufp->fullCData(oldp+20044,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                          >> 0x10U))),5);
    bufp->fullBit(oldp+20045,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20046,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                          >> 0xaU))),5);
    bufp->fullCData(oldp+20047,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                         >> 6U))),4);
    bufp->fullBit(oldp+20048,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 5U))));
    bufp->fullIData(oldp+20049,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                                 << 0x19U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                   >> 7U)))),30);
    bufp->fullBit(oldp+20050,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 9U))));
    bufp->fullBit(oldp+20051,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 8U))));
    bufp->fullBit(oldp+20052,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 7U))));
    bufp->fullCData(oldp+20053,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+20054,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U])),5);
    bufp->fullBit(oldp+20055,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+20056,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                       >> 0x1dU))),2);
    bufp->fullSData(oldp+20057,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                           >> 0x13U))),10);
    bufp->fullSData(oldp+20058,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                           >> 7U))),12);
    bufp->fullSData(oldp+20059,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                               >> 0x1bU)))),15);
    bufp->fullIData(oldp+20060,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                             >> 7U))),20);
    bufp->fullCData(oldp+20061,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 7U))),2);
    bufp->fullSData(oldp+20062,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                             << 0xbU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                               >> 0x15U)))),16);
    bufp->fullSData(oldp+20063,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                            >> 7U))),14);
    bufp->fullSData(oldp+20064,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                               >> 0x19U)))),15);
    bufp->fullIData(oldp+20065,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                             >> 7U))),18);
    bufp->fullCData(oldp+20066,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                       >> 7U))),3);
    bufp->fullBit(oldp+20067,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                     >> 6U))));
    bufp->fullIData(oldp+20068,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                                >> 0x13U)))),19);
    bufp->fullCData(oldp+20069,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                          >> 4U))),5);
    bufp->fullCData(oldp+20070,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
                                           << 1U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                           >> 0x1fU)))),5);
    bufp->fullCData(oldp+20071,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                       >> 0x1cU))),3);
    bufp->fullIData(oldp+20072,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                              >> 7U))),21);
    bufp->fullCData(oldp+20073,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+20074,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                       >> 3U))),2);
    bufp->fullCData(oldp+20075,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+20076,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U])));
    bufp->fullBit(oldp+20077,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                               >> 0x1fU)));
    bufp->fullBit(oldp+20078,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1eU))));
    bufp->fullBit(oldp+20079,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+20080,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+20081,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+20082,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                       >> 0x19U))),2);
    bufp->fullBit(oldp+20083,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
                                     >> 0x18U))));
    bufp->fullBit(oldp+20084,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                     >> 4U))));
    bufp->fullBit(oldp+20085,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                     >> 3U))));
    bufp->fullBit(oldp+20086,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                     >> 2U))));
    bufp->fullBit(oldp+20087,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo) 
                                     >> 1U))));
    bufp->fullBit(oldp+20088,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo))));
    bufp->fullCData(oldp+20089,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                 >> 0x19U)),7);
    bufp->fullCData(oldp+20090,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                          >> 0x14U))),5);
    bufp->fullCData(oldp+20091,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                          >> 0xfU))),5);
    bufp->fullCData(oldp+20092,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                       >> 0xcU))),3);
    bufp->fullCData(oldp+20093,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf 
                                          >> 7U))),5);
    bufp->fullCData(oldp+20094,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf)),7);
    bufp->fullCData(oldp+20095,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__rv32mFunct7),7);
    bufp->fullCData(oldp+20096,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__zbaFunct7),7);
    bufp->fullCData(oldp+20097,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__zicondFunct7),7);
    bufp->fullBit(oldp+20098,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined));
    bufp->fullBit(oldp+20099,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__illegalPC
                              [1U]));
    bufp->fullCData(oldp+20100,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 9U))),3);
    bufp->fullCData(oldp+20101,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+20102,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 4U))),3);
    bufp->fullBit(oldp+20103,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20104,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                           >> 0x1eU)))),5);
    bufp->fullBit(oldp+20105,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 0x1dU))));
    bufp->fullCData(oldp+20106,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                          >> 0x18U))),5);
    bufp->fullBit(oldp+20107,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 0x17U))));
    bufp->fullCData(oldp+20108,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                          >> 0x12U))),5);
    bufp->fullCData(oldp+20109,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                         >> 0xeU))),4);
    bufp->fullBit(oldp+20110,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 0xdU))));
    bufp->fullIData(oldp+20111,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                   >> 0xfU)))),30);
    bufp->fullBit(oldp+20112,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+20113,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+20114,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20115,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+20116,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                          >> 8U))),5);
    bufp->fullBit(oldp+20117,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 7U))));
    bufp->fullCData(oldp+20118,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 5U))),2);
    bufp->fullSData(oldp+20119,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                              >> 0x1bU)))),10);
    bufp->fullSData(oldp+20120,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                           >> 0xfU))),12);
    bufp->fullSData(oldp+20121,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            >> 3U))),15);
    bufp->fullIData(oldp+20122,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                >> 0xfU)))),20);
    bufp->fullCData(oldp+20123,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0xfU))),2);
    bufp->fullSData(oldp+20124,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                               >> 0x1dU)))),16);
    bufp->fullSData(oldp+20125,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                            >> 0xfU))),14);
    bufp->fullSData(oldp+20126,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                            >> 1U))),15);
    bufp->fullIData(oldp+20127,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                >> 0xfU)))),18);
    bufp->fullCData(oldp+20128,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 0xfU))),3);
    bufp->fullBit(oldp+20129,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                     >> 0xeU))));
    bufp->fullIData(oldp+20130,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                >> 0x1bU)))),19);
    bufp->fullCData(oldp+20131,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                          >> 0xcU))),5);
    bufp->fullCData(oldp+20132,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                          >> 7U))),5);
    bufp->fullCData(oldp+20133,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                       >> 4U))),3);
    bufp->fullIData(oldp+20134,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                               << 0x11U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                                 >> 0xfU)))),21);
    bufp->fullCData(oldp+20135,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+20136,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+20137,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 9U))),2);
    bufp->fullBit(oldp+20138,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                     >> 8U))));
    bufp->fullBit(oldp+20139,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                     >> 7U))));
    bufp->fullBit(oldp+20140,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                     >> 6U))));
    bufp->fullBit(oldp+20141,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                     >> 5U))));
    bufp->fullBit(oldp+20142,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                     >> 4U))));
    bufp->fullBit(oldp+20143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20144,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+20145,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U])));
    bufp->fullCData(oldp+20146,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x15U))),3);
    bufp->fullCData(oldp+20147,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x13U))),2);
    bufp->fullCData(oldp+20148,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x10U))),3);
    bufp->fullBit(oldp+20149,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20150,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                          >> 0xaU))),5);
    bufp->fullBit(oldp+20151,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 9U))));
    bufp->fullCData(oldp+20152,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                          >> 4U))),5);
    bufp->fullBit(oldp+20153,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20154,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                           >> 0x1eU)))),5);
    bufp->fullCData(oldp+20155,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                         >> 0x1aU))),4);
    bufp->fullBit(oldp+20156,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                     >> 0x19U))));
    bufp->fullIData(oldp+20157,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                   >> 0x1bU)))),30);
    bufp->fullBit(oldp+20158,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+20159,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+20160,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+20161,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+20162,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                          >> 0x14U))),5);
    bufp->fullBit(oldp+20163,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+20164,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x11U))),2);
    bufp->fullSData(oldp+20165,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                           >> 7U))),10);
    bufp->fullSData(oldp+20166,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                              >> 0x1bU)))),12);
    bufp->fullSData(oldp+20167,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                            >> 0xfU))),15);
    bufp->fullIData(oldp+20168,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                >> 0x1bU)))),20);
    bufp->fullCData(oldp+20169,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1bU))),2);
    bufp->fullSData(oldp+20170,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                            >> 9U))),16);
    bufp->fullSData(oldp+20171,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                               >> 0x1bU)))),14);
    bufp->fullSData(oldp+20172,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                            >> 0xdU))),15);
    bufp->fullIData(oldp+20173,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                >> 0x1bU)))),18);
    bufp->fullCData(oldp+20174,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x1bU))),3);
    bufp->fullBit(oldp+20175,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                     >> 0x1aU))));
    bufp->fullIData(oldp+20176,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                             >> 7U))),19);
    bufp->fullCData(oldp+20177,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                          >> 0x18U))),5);
    bufp->fullCData(oldp+20178,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                          >> 0x13U))),5);
    bufp->fullCData(oldp+20179,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                       >> 0x10U))),3);
    bufp->fullIData(oldp+20180,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                                 >> 0x1bU)))),21);
    bufp->fullCData(oldp+20181,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+20182,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+20183,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0x15U))),2);
    bufp->fullBit(oldp+20184,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+20185,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 0x13U))));
    bufp->fullBit(oldp+20186,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 0x12U))));
    bufp->fullBit(oldp+20187,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+20188,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+20189,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20190,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                       >> 0xdU))),2);
    bufp->fullBit(oldp+20191,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                     >> 0xcU))));
    bufp->fullCData(oldp+20192,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                                       >> 1U))),3);
    bufp->fullCData(oldp+20193,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                                        << 1U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                                  >> 0x1fU)))),2);
    bufp->fullCData(oldp+20194,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 0x1cU))),3);
    bufp->fullBit(oldp+20195,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+20196,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                          >> 0x16U))),5);
    bufp->fullBit(oldp+20197,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 0x15U))));
    bufp->fullCData(oldp+20198,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                          >> 0x10U))),5);
    bufp->fullBit(oldp+20199,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20200,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                          >> 0xaU))),5);
    bufp->fullCData(oldp+20201,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                         >> 6U))),4);
    bufp->fullBit(oldp+20202,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 5U))));
    bufp->fullIData(oldp+20203,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                                 << 0x19U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                   >> 7U)))),30);
    bufp->fullBit(oldp+20204,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 9U))));
    bufp->fullBit(oldp+20205,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 8U))));
    bufp->fullBit(oldp+20206,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 7U))));
    bufp->fullCData(oldp+20207,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+20208,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U])),5);
    bufp->fullBit(oldp+20209,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                               >> 0x1fU)));
    bufp->fullCData(oldp+20210,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                       >> 0x1dU))),2);
    bufp->fullSData(oldp+20211,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                           >> 0x13U))),10);
    bufp->fullSData(oldp+20212,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                           >> 7U))),12);
    bufp->fullSData(oldp+20213,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                               >> 0x1bU)))),15);
    bufp->fullIData(oldp+20214,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                             >> 7U))),20);
    bufp->fullCData(oldp+20215,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 7U))),2);
    bufp->fullSData(oldp+20216,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                             << 0xbU) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                               >> 0x15U)))),16);
    bufp->fullSData(oldp+20217,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                            >> 7U))),14);
    bufp->fullSData(oldp+20218,((0x7fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                             << 7U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                               >> 0x19U)))),15);
    bufp->fullIData(oldp+20219,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                             >> 7U))),18);
    bufp->fullCData(oldp+20220,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                       >> 7U))),3);
    bufp->fullBit(oldp+20221,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                     >> 6U))));
    bufp->fullIData(oldp+20222,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                                >> 0x13U)))),19);
    bufp->fullCData(oldp+20223,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                          >> 4U))),5);
    bufp->fullCData(oldp+20224,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                           << 1U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                           >> 0x1fU)))),5);
    bufp->fullCData(oldp+20225,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                       >> 0x1cU))),3);
    bufp->fullIData(oldp+20226,((0x1fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                              >> 7U))),21);
    bufp->fullCData(oldp+20227,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+20228,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                       >> 3U))),2);
    bufp->fullCData(oldp+20229,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+20230,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U])));
    bufp->fullBit(oldp+20231,((vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                               >> 0x1fU)));
    bufp->fullBit(oldp+20232,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1eU))));
    bufp->fullBit(oldp+20233,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+20234,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+20235,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+20236,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                       >> 0x19U))),2);
    bufp->fullBit(oldp+20237,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                     >> 0x18U))));
    bufp->fullBit(oldp+20238,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                     >> 4U))));
    bufp->fullBit(oldp+20239,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                     >> 3U))));
    bufp->fullBit(oldp+20240,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                     >> 2U))));
    bufp->fullBit(oldp+20241,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo) 
                                     >> 1U))));
    bufp->fullBit(oldp+20242,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo))));
    bufp->fullCData(oldp+20243,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                 >> 0x19U)),7);
    bufp->fullCData(oldp+20244,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                          >> 0x14U))),5);
    bufp->fullCData(oldp+20245,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                          >> 0xfU))),5);
    bufp->fullCData(oldp+20246,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                       >> 0xcU))),3);
    bufp->fullCData(oldp+20247,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
                                          >> 7U))),5);
    bufp->fullCData(oldp+20248,((0x7fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)),7);
    bufp->fullCData(oldp+20249,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__rv32mFunct7),7);
    bufp->fullCData(oldp+20250,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zbaFunct7),7);
    bufp->fullCData(oldp+20251,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zicondFunct7),7);
    bufp->fullBit(oldp+20252,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined));
    bufp->fullIData(oldp+20253,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+20254,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__pdStage__DOT__unnamedblk3__DOT__i),32);
    bufp->fullIData(oldp+20255,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[6U]),32);
    bufp->fullIData(oldp+20256,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[5U]),32);
    bufp->fullIData(oldp+20257,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[4U]),32);
    bufp->fullIData(oldp+20258,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[3U]),32);
    bufp->fullIData(oldp+20259,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[2U]),32);
    bufp->fullIData(oldp+20260,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[1U]),32);
    bufp->fullIData(oldp+20261,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__perfCounter__DOT__cur[0U]),32);
    bufp->fullCData(oldp+20262,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                       >> 0x15U))),2);
    bufp->fullBit(oldp+20263,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                     >> 0x14U))));
    bufp->fullIData(oldp+20264,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                                  << 0xcU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[2U] 
                                              >> 0x14U))),32);
    bufp->fullIData(oldp+20265,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[2U] 
                                  << 0xcU) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[1U] 
                                              >> 0x14U))),32);
    bufp->fullCData(oldp+20266,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[1U] 
                                         >> 0x10U))),4);
    bufp->fullIData(oldp+20267,(((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[1U] 
                                  << 0x10U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                               >> 0x10U))),32);
    bufp->fullCData(oldp+20268,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                          >> 0xaU))),6);
    bufp->fullCData(oldp+20269,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                          >> 4U))),6);
    bufp->fullBit(oldp+20270,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20271,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U])),3);
    bufp->fullCData(oldp+20272,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__phase),2);
    bufp->fullCData(oldp+20273,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__recoveryCount),7);
    bufp->fullBit(oldp+20274,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                     [0U] >> 7U))));
    bufp->fullBit(oldp+20275,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                     [0U] >> 6U))));
    bufp->fullCData(oldp+20276,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                 [0U])),6);
    bufp->fullBit(oldp+20277,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                     [1U] >> 7U))));
    bufp->fullBit(oldp+20278,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                     [1U] >> 6U))));
    bufp->fullCData(oldp+20279,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__regReleasedReg
                                 [1U])),6);
    bufp->fullIData(oldp+20280,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+20281,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk3__DOT__i),32);
    bufp->fullIData(oldp+20282,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk4__DOT__i),32);
    bufp->fullIData(oldp+20283,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__renameLogicCommitter__DOT__unnamedblk6__DOT__i),32);
    bufp->fullCData(oldp+20284,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x16U] 
                                       >> 6U))),2);
    bufp->fullSData(oldp+20285,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                           >> 0x11U))),10);
    bufp->fullCData(oldp+20286,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                       >> 0xfU))),2);
    bufp->fullCData(oldp+20287,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+20288,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+20289,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                         >> 7U))),4);
    bufp->fullBit(oldp+20290,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                     >> 6U))));
    bufp->fullIData(oldp+20291,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                                 << 0x18U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                                   >> 8U)))),30);
    bufp->fullIData(oldp+20292,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                              << 0xaU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                                >> 0x16U)))),18);
    bufp->fullBit(oldp+20293,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                     >> 0xaU))));
    bufp->fullIData(oldp+20294,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                              << 9U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                                >> 0x17U)))),19);
    bufp->fullBit(oldp+20295,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                     >> 0x16U))));
    bufp->fullSData(oldp+20296,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                           >> 0xcU))),10);
    bufp->fullCData(oldp+20297,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                       >> 0xaU))),2);
    bufp->fullIData(oldp+20298,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x10U] 
                                              << 0xaU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                                >> 0x16U)))),20);
    bufp->fullCData(oldp+20299,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                       >> 0x13U))),3);
    bufp->fullCData(oldp+20300,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                       >> 0x10U))),3);
    bufp->fullCData(oldp+20301,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                          >> 0xaU))),6);
    bufp->fullCData(oldp+20302,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                         >> 6U))),4);
    bufp->fullCData(oldp+20303,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                         >> 2U))),4);
    bufp->fullBit(oldp+20304,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                     >> 1U))));
    bufp->fullCData(oldp+20305,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xfU] 
                                           << 5U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                           >> 0x1bU)))),6);
    bufp->fullBit(oldp+20306,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                     >> 0x1aU))));
    bufp->fullCData(oldp+20307,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                          >> 0x14U))),6);
    bufp->fullBit(oldp+20308,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+20309,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                          >> 0xdU))),6);
    bufp->fullBit(oldp+20310,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                     >> 0xcU))));
    bufp->fullBit(oldp+20311,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                     >> 0xbU))));
    bufp->fullCData(oldp+20312,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                          >> 5U))),6);
    bufp->fullBit(oldp+20313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                     >> 4U))));
    bufp->fullIData(oldp+20314,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xeU] 
                                              << 0xfU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                                >> 0x11U)))),19);
    bufp->fullBit(oldp+20315,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                     >> 0x10U))));
    bufp->fullSData(oldp+20316,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x16U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                              >> 0x1cU)))),10);
    bufp->fullCData(oldp+20317,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                       >> 0x1aU))),2);
    bufp->fullCData(oldp+20318,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                       >> 0x18U))),2);
    bufp->fullCData(oldp+20319,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                       >> 0x16U))),2);
    bufp->fullCData(oldp+20320,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                         >> 0x12U))),4);
    bufp->fullBit(oldp+20321,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                     >> 0x11U))));
    bufp->fullIData(oldp+20322,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                                 << 0xdU) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                                   >> 0x13U)))),30);
    bufp->fullIData(oldp+20323,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                             >> 1U))),18);
    bufp->fullBit(oldp+20324,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                     >> 0x15U))));
    bufp->fullIData(oldp+20325,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                             >> 2U))),19);
    bufp->fullBit(oldp+20326,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                     >> 1U))));
    bufp->fullSData(oldp+20327,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x15U] 
                                            << 9U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                              >> 0x17U)))),10);
    bufp->fullCData(oldp+20328,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                       >> 0x15U))),2);
    bufp->fullIData(oldp+20329,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                             >> 1U))),20);
    bufp->fullCData(oldp+20330,((7U & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x14U] 
                                        << 2U) | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                                  >> 0x1eU)))),3);
    bufp->fullCData(oldp+20331,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                       >> 0x1bU))),3);
    bufp->fullCData(oldp+20332,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                          >> 0x15U))),6);
    bufp->fullCData(oldp+20333,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                         >> 0x11U))),4);
    bufp->fullCData(oldp+20334,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                         >> 0xdU))),4);
    bufp->fullBit(oldp+20335,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                     >> 0xcU))));
    bufp->fullCData(oldp+20336,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                          >> 6U))),6);
    bufp->fullBit(oldp+20337,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                     >> 5U))));
    bufp->fullCData(oldp+20338,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x13U] 
                                           << 1U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                           >> 0x1fU)))),6);
    bufp->fullBit(oldp+20339,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                     >> 0x1eU))));
    bufp->fullCData(oldp+20340,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                          >> 0x18U))),6);
    bufp->fullBit(oldp+20341,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                     >> 0x17U))));
    bufp->fullBit(oldp+20342,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                     >> 0x16U))));
    bufp->fullCData(oldp+20343,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                          >> 0x10U))),6);
    bufp->fullBit(oldp+20344,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                     >> 0xfU))));
    bufp->fullIData(oldp+20345,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x12U] 
                                              << 4U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                                >> 0x1cU)))),19);
    bufp->fullBit(oldp+20346,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0x11U] 
                                     >> 0x1bU))));
    bufp->fullBit(oldp+20347,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                     >> 0xfU))));
    bufp->fullSData(oldp+20348,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                           >> 5U))),10);
    bufp->fullCData(oldp+20349,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                       >> 3U))),2);
    bufp->fullBit(oldp+20350,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU] 
                                     >> 2U))));
    bufp->fullCData(oldp+20351,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xdU])),2);
    bufp->fullCData(oldp+20352,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                 >> 0x1dU)),3);
    bufp->fullCData(oldp+20353,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                          >> 0x17U))),6);
    bufp->fullCData(oldp+20354,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                         >> 0x13U))),4);
    bufp->fullCData(oldp+20355,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                         >> 0xfU))),4);
    bufp->fullBit(oldp+20356,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                     >> 0xeU))));
    bufp->fullCData(oldp+20357,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                          >> 8U))),6);
    bufp->fullBit(oldp+20358,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                     >> 7U))));
    bufp->fullCData(oldp+20359,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU] 
                                          >> 1U))),6);
    bufp->fullBit(oldp+20360,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xcU])));
    bufp->fullCData(oldp+20361,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                 >> 0x1aU)),6);
    bufp->fullBit(oldp+20362,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                     >> 0x19U))));
    bufp->fullBit(oldp+20363,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                     >> 0x18U))));
    bufp->fullCData(oldp+20364,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                          >> 0x12U))),6);
    bufp->fullBit(oldp+20365,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                     >> 0x11U))));
    bufp->fullIData(oldp+20366,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xbU] 
                                              << 2U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                                >> 0x1eU)))),19);
    bufp->fullBit(oldp+20367,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                     >> 0x1dU))));
    bufp->fullCData(oldp+20368,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                       >> 0x1bU))),2);
    bufp->fullSData(oldp+20369,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                           >> 0x14U))),10);
    bufp->fullCData(oldp+20370,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                       >> 0x12U))),2);
    bufp->fullCData(oldp+20371,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                       >> 0xfU))),3);
    bufp->fullCData(oldp+20372,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                       >> 0xcU))),3);
    bufp->fullCData(oldp+20373,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                       >> 0xaU))),2);
    bufp->fullCData(oldp+20374,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                       >> 8U))),2);
    bufp->fullSData(oldp+20375,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                              >> 0x1cU)))),12);
    bufp->fullBit(oldp+20376,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                     >> 0x1bU))));
    bufp->fullBit(oldp+20377,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                     >> 0x1aU))));
    bufp->fullBit(oldp+20378,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                     >> 0x19U))));
    bufp->fullCData(oldp+20379,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+20380,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                          >> 0x12U))),5);
    bufp->fullBit(oldp+20381,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                     >> 0x11U))));
    bufp->fullCData(oldp+20382,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0xfU))),2);
    bufp->fullCData(oldp+20383,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                       >> 0xcU))),3);
    bufp->fullBit(oldp+20384,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                     >> 0xbU))));
    bufp->fullCData(oldp+20385,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                         >> 7U))),4);
    bufp->fullCData(oldp+20386,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                         >> 3U))),4);
    bufp->fullBit(oldp+20387,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                     >> 2U))));
    bufp->fullBit(oldp+20388,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                     >> 1U))));
    bufp->fullCData(oldp+20389,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[5U] 
                                           << 5U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                           >> 0x1bU)))),6);
    bufp->fullCData(oldp+20390,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                         >> 0x17U))),4);
    bufp->fullCData(oldp+20391,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                         >> 0x13U))),4);
    bufp->fullBit(oldp+20392,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                     >> 0x12U))));
    bufp->fullCData(oldp+20393,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                          >> 0xcU))),6);
    bufp->fullBit(oldp+20394,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                     >> 0xbU))));
    bufp->fullCData(oldp+20395,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                          >> 5U))),6);
    bufp->fullBit(oldp+20396,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                     >> 4U))));
    bufp->fullCData(oldp+20397,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                           >> 0x1eU)))),6);
    bufp->fullBit(oldp+20398,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+20399,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                     >> 0x1cU))));
    bufp->fullCData(oldp+20400,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                          >> 0x16U))),6);
    bufp->fullBit(oldp+20401,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                     >> 0x15U))));
    bufp->fullIData(oldp+20402,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                             >> 2U))),19);
    bufp->fullBit(oldp+20403,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U] 
                                     >> 1U))));
    bufp->fullSData(oldp+20404,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                           >> 0x11U))),10);
    bufp->fullCData(oldp+20405,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                       >> 0xfU))),2);
    bufp->fullCData(oldp+20406,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                       >> 0xcU))),3);
    bufp->fullCData(oldp+20407,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                       >> 9U))),3);
    bufp->fullCData(oldp+20408,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+20409,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                       >> 5U))),2);
    bufp->fullSData(oldp+20410,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0xaU] 
                                            << 7U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                              >> 0x19U)))),12);
    bufp->fullBit(oldp+20411,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                     >> 0x18U))));
    bufp->fullBit(oldp+20412,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                     >> 0x17U))));
    bufp->fullBit(oldp+20413,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                     >> 0x16U))));
    bufp->fullCData(oldp+20414,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 0x14U))),2);
    bufp->fullCData(oldp+20415,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                          >> 0xfU))),5);
    bufp->fullBit(oldp+20416,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                     >> 0xeU))));
    bufp->fullCData(oldp+20417,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 0xcU))),2);
    bufp->fullCData(oldp+20418,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                       >> 9U))),3);
    bufp->fullBit(oldp+20419,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                     >> 8U))));
    bufp->fullCData(oldp+20420,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U] 
                                         >> 4U))),4);
    bufp->fullCData(oldp+20421,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[9U])),4);
    bufp->fullBit(oldp+20422,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                               >> 0x1fU)));
    bufp->fullBit(oldp+20423,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                     >> 0x1eU))));
    bufp->fullCData(oldp+20424,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                          >> 0x18U))),6);
    bufp->fullCData(oldp+20425,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                         >> 0x14U))),4);
    bufp->fullCData(oldp+20426,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                         >> 0x10U))),4);
    bufp->fullBit(oldp+20427,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20428,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                          >> 9U))),6);
    bufp->fullBit(oldp+20429,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                     >> 8U))));
    bufp->fullCData(oldp+20430,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                          >> 2U))),6);
    bufp->fullBit(oldp+20431,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                     >> 1U))));
    bufp->fullCData(oldp+20432,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[8U] 
                                           << 5U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                           >> 0x1bU)))),6);
    bufp->fullBit(oldp+20433,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                     >> 0x1aU))));
    bufp->fullBit(oldp+20434,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                     >> 0x19U))));
    bufp->fullCData(oldp+20435,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                          >> 0x13U))),6);
    bufp->fullBit(oldp+20436,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                     >> 0x12U))));
    bufp->fullIData(oldp+20437,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[7U] 
                                              << 1U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                                >> 0x1fU)))),19);
    bufp->fullBit(oldp+20438,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[6U] 
                                     >> 0x1eU))));
    bufp->fullBit(oldp+20439,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[3U])));
    bufp->fullSData(oldp+20440,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                 >> 0x16U)),10);
    bufp->fullCData(oldp+20441,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                       >> 0x14U))),2);
    bufp->fullCData(oldp+20442,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                       >> 0x11U))),3);
    bufp->fullCData(oldp+20443,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                          >> 0xcU))),5);
    bufp->fullCData(oldp+20444,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                       >> 9U))),3);
    bufp->fullCData(oldp+20445,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+20446,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                       >> 5U))),2);
    bufp->fullCData(oldp+20447,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                       >> 3U))),2);
    bufp->fullCData(oldp+20448,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[2U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                           >> 0x1dU)))),6);
    bufp->fullCData(oldp+20449,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                         >> 0x19U))),4);
    bufp->fullCData(oldp+20450,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                         >> 0x15U))),4);
    bufp->fullBit(oldp+20451,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                     >> 0x14U))));
    bufp->fullCData(oldp+20452,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                          >> 0xeU))),6);
    bufp->fullBit(oldp+20453,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                     >> 0xdU))));
    bufp->fullCData(oldp+20454,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                          >> 7U))),6);
    bufp->fullBit(oldp+20455,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U] 
                                     >> 6U))));
    bufp->fullCData(oldp+20456,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[1U])),6);
    bufp->fullBit(oldp+20457,((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                               >> 0x1fU)));
    bufp->fullBit(oldp+20458,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                     >> 0x1eU))));
    bufp->fullCData(oldp+20459,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                          >> 0x18U))),6);
    bufp->fullBit(oldp+20460,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                     >> 0x17U))));
    bufp->fullIData(oldp+20461,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                             >> 4U))),19);
    bufp->fullBit(oldp+20462,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20463,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayEntryOut[0U])),3);
    bufp->fullCData(oldp+20464,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regHeadStorage),5);
    bufp->fullCData(oldp+20465,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regTailStorage),5);
    bufp->fullBit(oldp+20466,((0x14U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regCount))));
    bufp->fullBit(oldp+20467,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regCount))));
    bufp->fullCData(oldp+20468,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__pointer__DOT__regCount),6);
    bufp->fullBit(oldp+20469,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__validInstCount))));
    bufp->fullCData(oldp+20470,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__validInstCount),6);
    bufp->fullCData(oldp+20471,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__intervalIn),3);
    bufp->fullCData(oldp+20472,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__intervalCount),3);
    bufp->fullCData(oldp+20473,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__canBeFlushedEntryCount),6);
    bufp->fullCData(oldp+20474,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushRangeHeadPtr),6);
    bufp->fullCData(oldp+20475,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushRangeTailPtr),6);
    bufp->fullBit(oldp+20476,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__flushAllInsns));
    bufp->fullBit(oldp+20477,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__replayReg));
    bufp->fullBit(oldp+20478,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrID[0]));
    bufp->fullBit(oldp+20479,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrID[1]));
    bufp->fullBit(oldp+20480,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrValid[0]));
    bufp->fullBit(oldp+20481,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrValid[1]));
    bufp->fullCData(oldp+20482,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrPhase
                                [0U]),5);
    bufp->fullCData(oldp+20483,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__replayQueue__DOT__mshrPhase
                                [1U]),5);
    bufp->fullBit(oldp+20484,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__regFlush));
    bufp->fullBit(oldp+20485,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__empty));
    bufp->fullCData(oldp+20486,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 9U))),3);
    bufp->fullCData(oldp+20487,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 7U))),2);
    bufp->fullCData(oldp+20488,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 4U))),3);
    bufp->fullBit(oldp+20489,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20490,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                           >> 0x1eU)))),5);
    bufp->fullBit(oldp+20491,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0x1dU))));
    bufp->fullCData(oldp+20492,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                          >> 0x18U))),5);
    bufp->fullBit(oldp+20493,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0x17U))));
    bufp->fullCData(oldp+20494,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                          >> 0x12U))),5);
    bufp->fullCData(oldp+20495,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                         >> 0xeU))),4);
    bufp->fullBit(oldp+20496,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0xdU))));
    bufp->fullIData(oldp+20497,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                                 << 0x11U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                   >> 0xfU)))),30);
    bufp->fullBit(oldp+20498,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+20499,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+20500,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20501,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+20502,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                          >> 8U))),5);
    bufp->fullBit(oldp+20503,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 7U))));
    bufp->fullCData(oldp+20504,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 5U))),2);
    bufp->fullSData(oldp+20505,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                              >> 0x1bU)))),10);
    bufp->fullSData(oldp+20506,((0xfffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                           >> 0xfU))),12);
    bufp->fullSData(oldp+20507,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            >> 3U))),15);
    bufp->fullIData(oldp+20508,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                >> 0xfU)))),20);
    bufp->fullCData(oldp+20509,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0xfU))),2);
    bufp->fullSData(oldp+20510,((0xffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                             << 3U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                               >> 0x1dU)))),16);
    bufp->fullSData(oldp+20511,((0x3fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                            >> 0xfU))),14);
    bufp->fullSData(oldp+20512,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                            >> 1U))),15);
    bufp->fullIData(oldp+20513,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                              << 0x11U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                >> 0xfU)))),18);
    bufp->fullCData(oldp+20514,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 0xfU))),3);
    bufp->fullBit(oldp+20515,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                     >> 0xeU))));
    bufp->fullIData(oldp+20516,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                >> 0x1bU)))),19);
    bufp->fullCData(oldp+20517,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                          >> 0xcU))),5);
    bufp->fullCData(oldp+20518,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                          >> 7U))),5);
    bufp->fullCData(oldp+20519,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                       >> 4U))),3);
    bufp->fullIData(oldp+20520,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[1U] 
                                               << 0x11U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                                 >> 0xfU)))),21);
    bufp->fullCData(oldp+20521,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 0xdU))),2);
    bufp->fullCData(oldp+20522,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 0xbU))),2);
    bufp->fullCData(oldp+20523,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 9U))),2);
    bufp->fullBit(oldp+20524,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                     >> 8U))));
    bufp->fullBit(oldp+20525,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                     >> 7U))));
    bufp->fullBit(oldp+20526,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                     >> 6U))));
    bufp->fullBit(oldp+20527,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                     >> 5U))));
    bufp->fullBit(oldp+20528,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                     >> 4U))));
    bufp->fullBit(oldp+20529,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20530,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U] 
                                       >> 1U))),2);
    bufp->fullBit(oldp+20531,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[0U])));
    bufp->fullCData(oldp+20532,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                       >> 0x15U))),3);
    bufp->fullCData(oldp+20533,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                       >> 0x13U))),2);
    bufp->fullCData(oldp+20534,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                       >> 0x10U))),3);
    bufp->fullBit(oldp+20535,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20536,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                          >> 0xaU))),5);
    bufp->fullBit(oldp+20537,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                     >> 9U))));
    bufp->fullCData(oldp+20538,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                          >> 4U))),5);
    bufp->fullBit(oldp+20539,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                     >> 3U))));
    bufp->fullCData(oldp+20540,((0x1fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[4U] 
                                           << 2U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                           >> 0x1eU)))),5);
    bufp->fullCData(oldp+20541,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                         >> 0x1aU))),4);
    bufp->fullBit(oldp+20542,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                     >> 0x19U))));
    bufp->fullIData(oldp+20543,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                                 << 5U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                   >> 0x1bU)))),30);
    bufp->fullBit(oldp+20544,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                     >> 0x1dU))));
    bufp->fullBit(oldp+20545,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                     >> 0x1cU))));
    bufp->fullBit(oldp+20546,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                     >> 0x1bU))));
    bufp->fullCData(oldp+20547,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+20548,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                          >> 0x14U))),5);
    bufp->fullBit(oldp+20549,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                     >> 0x13U))));
    bufp->fullCData(oldp+20550,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x11U))),2);
    bufp->fullSData(oldp+20551,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                           >> 7U))),10);
    bufp->fullSData(oldp+20552,((0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                            << 5U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                              >> 0x1bU)))),12);
    bufp->fullSData(oldp+20553,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                            >> 0xfU))),15);
    bufp->fullIData(oldp+20554,((0xfffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                >> 0x1bU)))),20);
    bufp->fullCData(oldp+20555,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x1bU))),2);
    bufp->fullSData(oldp+20556,((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                            >> 9U))),16);
    bufp->fullSData(oldp+20557,((0x3fffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                             << 5U) 
                                            | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                               >> 0x1bU)))),14);
    bufp->fullSData(oldp+20558,((0x7fffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                            >> 0xdU))),15);
    bufp->fullIData(oldp+20559,((0x3ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                              << 5U) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                >> 0x1bU)))),18);
    bufp->fullCData(oldp+20560,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x1bU))),3);
    bufp->fullBit(oldp+20561,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                     >> 0x1aU))));
    bufp->fullIData(oldp+20562,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                             >> 7U))),19);
    bufp->fullCData(oldp+20563,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                          >> 0x18U))),5);
    bufp->fullCData(oldp+20564,((0x1fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                          >> 0x13U))),5);
    bufp->fullCData(oldp+20565,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                       >> 0x10U))),3);
    bufp->fullIData(oldp+20566,((0x1fffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[3U] 
                                               << 5U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                                 >> 0x1bU)))),21);
    bufp->fullCData(oldp+20567,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x19U))),2);
    bufp->fullCData(oldp+20568,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x17U))),2);
    bufp->fullCData(oldp+20569,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0x15U))),2);
    bufp->fullBit(oldp+20570,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 0x14U))));
    bufp->fullBit(oldp+20571,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 0x13U))));
    bufp->fullBit(oldp+20572,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 0x12U))));
    bufp->fullBit(oldp+20573,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 0x11U))));
    bufp->fullBit(oldp+20574,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 0x10U))));
    bufp->fullBit(oldp+20575,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 0xfU))));
    bufp->fullCData(oldp+20576,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                       >> 0xdU))),2);
    bufp->fullBit(oldp+20577,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__opInfo[2U] 
                                     >> 0xcU))));
    bufp->fullBit(oldp+20578,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount))));
    bufp->fullBit(oldp+20579,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount))));
    bufp->fullBit(oldp+20580,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isEnv[0]));
    bufp->fullBit(oldp+20581,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__isEnv[1]));
    bufp->fullCData(oldp+20582,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__serializer__DOT__regPhase),2);
    bufp->fullIData(oldp+20583,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__rnStage__DOT__unnamedblk2__DOT__i),32);
    bufp->fullSData(oldp+20584,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued),16);
    bufp->fullSData(oldp+20585,(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushIQ_Entry),16);
    bufp->fullIData(oldp+20586,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+20587,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+20588,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__unnamedblk3__DOT__i),32);
    bufp->fullBit(oldp+20589,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__phase));
    bufp->fullCData(oldp+20590,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__unfinishedStoreNum),5);
    bufp->fullCData(oldp+20591,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__portMSHRPhase
                                [0U]),5);
    bufp->fullCData(oldp+20592,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__portMSHRPhase
                                [1U]),5);
    bufp->fullBit(oldp+20593,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                             >> 0x3cU)))));
    bufp->fullBit(oldp+20594,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                             >> 0x3bU)))));
    bufp->fullIData(oldp+20595,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                         >> 0x1bU))),32);
    bufp->fullIData(oldp+20596,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                                     >> 7U)))),20);
    bufp->fullBit(oldp+20597,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                             >> 6U)))));
    bufp->fullCData(oldp+20598,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                                 >> 2U)))),4);
    bufp->fullBit(oldp+20599,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg 
                                             >> 1U)))));
    bufp->fullBit(oldp+20600,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__tagStagePipeReg))));
    bufp->fullBit(oldp+20601,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                             >> 0x3cU)))));
    bufp->fullBit(oldp+20602,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                             >> 0x3bU)))));
    bufp->fullIData(oldp+20603,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                         >> 0x1bU))),32);
    bufp->fullIData(oldp+20604,((0xfffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                                     >> 7U)))),20);
    bufp->fullBit(oldp+20605,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                             >> 6U)))));
    bufp->fullCData(oldp+20606,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                                 >> 2U)))),4);
    bufp->fullBit(oldp+20607,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg 
                                             >> 1U)))));
    bufp->fullBit(oldp+20608,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__dataStagePipeReg))));
    bufp->fullBit(oldp+20609,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__headStoreHasAllocatedMSHRPipeReg));
    bufp->fullBit(oldp+20610,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__storeMSHRID));
    bufp->fullBit(oldp+20611,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__finishWriteBack));
    bufp->fullBit(oldp+20612,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__releaseStoreQueueHead));
    bufp->fullCData(oldp+20613,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__storeCommitter__DOT__releaseStoreQueueHeadEntryNum),2);
    bufp->fullCData(oldp+20614,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountComplex),4);
    bufp->fullCData(oldp+20615,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountFP),4);
    bufp->fullBit(oldp+20616,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountInt));
    bufp->fullCData(oldp+20617,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountMem),3);
    bufp->fullCData(oldp+20618,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr),6);
    bufp->fullCData(oldp+20619,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr),6);
    bufp->fullBit(oldp+20620,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__flushAllInsns));
    bufp->fullIData(oldp+20621,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk11__DOT__i),32);
    bufp->fullIData(oldp+20622,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk11__DOT__unnamedblk12__DOT__j),32);
    bufp->fullIData(oldp+20623,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk13__DOT__i),32);
    bufp->fullIData(oldp+20624,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk13__DOT__unnamedblk14__DOT__j),32);
    bufp->fullIData(oldp+20625,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk15__DOT__i),32);
    bufp->fullIData(oldp+20626,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j),32);
    bufp->fullIData(oldp+20627,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk17__DOT__i),32);
    bufp->fullIData(oldp+20628,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk17__DOT__unnamedblk18__DOT__j),32);
    bufp->fullIData(oldp+20629,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk19__DOT__i),32);
    bufp->fullIData(oldp+20630,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk19__DOT__unnamedblk20__DOT__j),32);
    bufp->fullIData(oldp+20631,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk21__DOT__i),32);
    bufp->fullIData(oldp+20632,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk21__DOT__unnamedblk22__DOT__j),32);
    bufp->fullIData(oldp+20633,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk23__DOT__i),32);
    bufp->fullIData(oldp+20634,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk23__DOT__unnamedblk24__DOT__j),32);
    bufp->fullIData(oldp+20635,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk25__DOT__i),32);
    bufp->fullIData(oldp+20636,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk25__DOT__unnamedblk26__DOT__j),32);
    bufp->fullIData(oldp+20637,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk27__DOT__i),32);
    bufp->fullIData(oldp+20638,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk27__DOT__unnamedblk28__DOT__j),32);
    bufp->fullIData(oldp+20639,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk29__DOT__i),32);
    bufp->fullIData(oldp+20640,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk29__DOT__unnamedblk30__DOT__j),32);
    bufp->fullIData(oldp+20641,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk9__DOT__i),32);
    bufp->fullIData(oldp+20642,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__wakeupPipelineRegister__DOT__unnamedblk9__DOT__unnamedblk10__DOT__j),32);
    bufp->fullBit(oldp+20643,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [0U][2U] >> 4U))));
    bufp->fullQData(oldp+20644,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                  [0U][2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                [0U][1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [0U][0U])) 
                                                  >> 4U)))),64);
    bufp->fullCData(oldp+20646,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [0U][0U] >> 2U))),2);
    bufp->fullBit(oldp+20647,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [0U][0U] >> 1U))));
    bufp->fullBit(oldp+20648,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                               [0U][0U])));
    bufp->fullBit(oldp+20649,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [1U][2U] >> 4U))));
    bufp->fullQData(oldp+20650,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                  [1U][2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                [1U][1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [1U][0U])) 
                                                  >> 4U)))),64);
    bufp->fullCData(oldp+20652,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [1U][0U] >> 2U))),2);
    bufp->fullBit(oldp+20653,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [1U][0U] >> 1U))));
    bufp->fullBit(oldp+20654,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                               [1U][0U])));
    bufp->fullBit(oldp+20655,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [2U][2U] >> 4U))));
    bufp->fullQData(oldp+20656,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                  [2U][2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                [2U][1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [2U][0U])) 
                                                  >> 4U)))),64);
    bufp->fullCData(oldp+20658,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [2U][0U] >> 2U))),2);
    bufp->fullBit(oldp+20659,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [2U][0U] >> 1U))));
    bufp->fullBit(oldp+20660,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                               [2U][0U])));
    bufp->fullBit(oldp+20661,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [3U][2U] >> 4U))));
    bufp->fullQData(oldp+20662,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                  [3U][2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                [3U][1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [3U][0U])) 
                                                  >> 4U)))),64);
    bufp->fullCData(oldp+20664,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [3U][0U] >> 2U))),2);
    bufp->fullBit(oldp+20665,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [3U][0U] >> 1U))));
    bufp->fullBit(oldp+20666,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                               [3U][0U])));
    bufp->fullBit(oldp+20667,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [4U][2U] >> 4U))));
    bufp->fullQData(oldp+20668,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                  [4U][2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                [4U][1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                                                  [4U][0U])) 
                                                  >> 4U)))),64);
    bufp->fullCData(oldp+20670,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                       [4U][0U] >> 2U))),2);
    bufp->fullBit(oldp+20671,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                                     [4U][0U] >> 1U))));
    bufp->fullBit(oldp+20672,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memPipeReg
                               [4U][0U])));
    bufp->fullBit(oldp+20673,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__prevMemReadAccessAck));
    bufp->fullBit(oldp+20674,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__prevMemWriteAccessAck));
    bufp->fullCData(oldp+20675,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__processLatencyCount),2);
    bufp->fullBit(oldp+20676,(((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__hasRequest) 
                               & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                  >> 4U))));
    bufp->fullIData(oldp+20677,(((vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                  << 0x1cU) | (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                                               >> 4U))),32);
    bufp->fullQData(oldp+20678,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U])) 
                                                  >> 4U)))),64);
    bufp->fullBit(oldp+20680,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__hasRequest));
    bufp->fullBit(oldp+20681,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__hasRequestReg));
    bufp->fullBit(oldp+20682,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                     >> 5U))));
    bufp->fullBit(oldp+20683,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[3U] 
                                     >> 4U))));
    bufp->fullCData(oldp+20684,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U] 
                                       >> 2U))),2);
    bufp->fullBit(oldp+20685,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U] 
                                     >> 1U))));
    bufp->fullBit(oldp+20686,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[0U])));
    bufp->fullBit(oldp+20687,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[3U] 
                                     >> 5U))));
    bufp->fullBit(oldp+20688,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[3U] 
                                     >> 4U))));
    bufp->fullIData(oldp+20689,(((vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[3U] 
                                  << 0x1cU) | (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[2U] 
                                               >> 4U))),32);
    bufp->fullQData(oldp+20690,((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[2U])) 
                                  << 0x3cU) | (((QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[1U])) 
                                                << 0x1cU) 
                                               | ((QData)((IData)(
                                                                  vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U])) 
                                                  >> 4U)))),64);
    bufp->fullCData(oldp+20692,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U] 
                                       >> 2U))),2);
    bufp->fullBit(oldp+20693,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U] 
                                     >> 1U))));
    bufp->fullBit(oldp+20694,((1U & vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestDataReg[0U])));
    bufp->fullBit(oldp+20695,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pop));
    bufp->fullBit(oldp+20696,((0x80U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regCount))));
    bufp->fullBit(oldp+20697,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regCount))));
    bufp->fullCData(oldp+20698,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage),7);
    bufp->fullCData(oldp+20699,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regTailStorage),7);
    bufp->fullCData(oldp+20700,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__count),5);
    bufp->fullCData(oldp+20701,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__countReg),5);
    bufp->fullIData(oldp+20702,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__randReg),32);
    bufp->fullIData(oldp+20703,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__randNext),32);
    bufp->fullCData(oldp+20704,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage),7);
    bufp->fullCData(oldp+20705,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__memReqQueue__DOT__pointer__DOT__regCount),8);
    bufp->fullIData(oldp+20706,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__unnamedblk1__DOT__i),32);
    bufp->fullIData(oldp+20707,(vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__unnamedblk2__DOT__i),32);
    bufp->fullBit(oldp+20708,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__prevLastCommittedPC 
                                     >> 0x13U))));
    bufp->fullIData(oldp+20709,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__prevLastCommittedPC)),19);
    bufp->fullIData(oldp+20710,(vlSymsp->TOP__SMT_RTL_Testbench__core__cmStage.__PVT__cycles),32);
    bufp->fullBit(oldp+20711,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__regRstIndex) 
                                     >> 6U))));
    bufp->fullCData(oldp+20712,((0x3fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__regRstIndex))),6);
    bufp->fullBit(oldp+20713,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__fpRstIndex) 
                                     >> 6U))));
    bufp->fullCData(oldp+20714,((0x3fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__registerFile.__PVT__fpRstIndex))),6);
    bufp->fullCData(oldp+20715,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regHead),4);
    bufp->fullCData(oldp+20716,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regTail),4);
    bufp->fullCData(oldp+20717,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount),5);
    bufp->fullBit(oldp+20718,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0U] >> 0x1aU))));
    bufp->fullBit(oldp+20719,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0U] >> 0x19U))));
    bufp->fullIData(oldp+20720,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [0U] >> 5U))),20);
    bufp->fullBit(oldp+20721,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0U] >> 4U))));
    bufp->fullCData(oldp+20722,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [0U])),4);
    bufp->fullBit(oldp+20723,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [1U] >> 0x1aU))));
    bufp->fullBit(oldp+20724,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [1U] >> 0x19U))));
    bufp->fullIData(oldp+20725,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [1U] >> 5U))),20);
    bufp->fullBit(oldp+20726,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [1U] >> 4U))));
    bufp->fullCData(oldp+20727,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [1U])),4);
    bufp->fullBit(oldp+20728,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [2U] >> 0x1aU))));
    bufp->fullBit(oldp+20729,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [2U] >> 0x19U))));
    bufp->fullIData(oldp+20730,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [2U] >> 5U))),20);
    bufp->fullBit(oldp+20731,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [2U] >> 4U))));
    bufp->fullCData(oldp+20732,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [2U])),4);
    bufp->fullBit(oldp+20733,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [3U] >> 0x1aU))));
    bufp->fullBit(oldp+20734,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [3U] >> 0x19U))));
    bufp->fullIData(oldp+20735,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [3U] >> 5U))),20);
    bufp->fullBit(oldp+20736,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [3U] >> 4U))));
    bufp->fullCData(oldp+20737,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [3U])),4);
    bufp->fullBit(oldp+20738,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [4U] >> 0x1aU))));
    bufp->fullBit(oldp+20739,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [4U] >> 0x19U))));
    bufp->fullIData(oldp+20740,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [4U] >> 5U))),20);
    bufp->fullBit(oldp+20741,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [4U] >> 4U))));
    bufp->fullCData(oldp+20742,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [4U])),4);
    bufp->fullBit(oldp+20743,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [5U] >> 0x1aU))));
    bufp->fullBit(oldp+20744,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [5U] >> 0x19U))));
    bufp->fullIData(oldp+20745,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [5U] >> 5U))),20);
    bufp->fullBit(oldp+20746,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [5U] >> 4U))));
    bufp->fullCData(oldp+20747,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [5U])),4);
    bufp->fullBit(oldp+20748,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [6U] >> 0x1aU))));
    bufp->fullBit(oldp+20749,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [6U] >> 0x19U))));
    bufp->fullIData(oldp+20750,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [6U] >> 5U))),20);
    bufp->fullBit(oldp+20751,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [6U] >> 4U))));
    bufp->fullCData(oldp+20752,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [6U])),4);
    bufp->fullBit(oldp+20753,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [7U] >> 0x1aU))));
    bufp->fullBit(oldp+20754,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [7U] >> 0x19U))));
    bufp->fullIData(oldp+20755,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [7U] >> 5U))),20);
    bufp->fullBit(oldp+20756,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [7U] >> 4U))));
    bufp->fullCData(oldp+20757,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [7U])),4);
    bufp->fullBit(oldp+20758,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [8U] >> 0x1aU))));
    bufp->fullBit(oldp+20759,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [8U] >> 0x19U))));
    bufp->fullIData(oldp+20760,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [8U] >> 5U))),20);
    bufp->fullBit(oldp+20761,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [8U] >> 4U))));
    bufp->fullCData(oldp+20762,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [8U])),4);
    bufp->fullBit(oldp+20763,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [9U] >> 0x1aU))));
    bufp->fullBit(oldp+20764,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [9U] >> 0x19U))));
    bufp->fullIData(oldp+20765,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [9U] >> 5U))),20);
    bufp->fullBit(oldp+20766,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [9U] >> 4U))));
    bufp->fullCData(oldp+20767,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [9U])),4);
    bufp->fullBit(oldp+20768,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xaU] >> 0x1aU))));
    bufp->fullBit(oldp+20769,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xaU] >> 0x19U))));
    bufp->fullIData(oldp+20770,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [0xaU] 
                                             >> 5U))),20);
    bufp->fullBit(oldp+20771,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xaU] >> 4U))));
    bufp->fullCData(oldp+20772,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [0xaU])),4);
    bufp->fullBit(oldp+20773,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xbU] >> 0x1aU))));
    bufp->fullBit(oldp+20774,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xbU] >> 0x19U))));
    bufp->fullIData(oldp+20775,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [0xbU] 
                                             >> 5U))),20);
    bufp->fullBit(oldp+20776,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xbU] >> 4U))));
    bufp->fullCData(oldp+20777,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [0xbU])),4);
    bufp->fullBit(oldp+20778,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xcU] >> 0x1aU))));
    bufp->fullBit(oldp+20779,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xcU] >> 0x19U))));
    bufp->fullIData(oldp+20780,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [0xcU] 
                                             >> 5U))),20);
    bufp->fullBit(oldp+20781,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xcU] >> 4U))));
    bufp->fullCData(oldp+20782,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [0xcU])),4);
    bufp->fullBit(oldp+20783,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xdU] >> 0x1aU))));
    bufp->fullBit(oldp+20784,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xdU] >> 0x19U))));
    bufp->fullIData(oldp+20785,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [0xdU] 
                                             >> 5U))),20);
    bufp->fullBit(oldp+20786,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xdU] >> 4U))));
    bufp->fullCData(oldp+20787,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [0xdU])),4);
    bufp->fullBit(oldp+20788,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xeU] >> 0x1aU))));
    bufp->fullBit(oldp+20789,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xeU] >> 0x19U))));
    bufp->fullIData(oldp+20790,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [0xeU] 
                                             >> 5U))),20);
    bufp->fullBit(oldp+20791,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xeU] >> 4U))));
    bufp->fullCData(oldp+20792,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [0xeU])),4);
    bufp->fullBit(oldp+20793,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xfU] >> 0x1aU))));
    bufp->fullBit(oldp+20794,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xfU] >> 0x19U))));
    bufp->fullIData(oldp+20795,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                             [0xfU] 
                                             >> 5U))),20);
    bufp->fullBit(oldp+20796,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                     [0xfU] >> 4U))));
    bufp->fullCData(oldp+20797,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueue
                                 [0xfU])),4);
    bufp->fullBit(oldp+20798,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHead));
    bufp->fullCData(oldp+20799,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__releaseStoreQueueHeadEntryNum),2);
    bufp->fullCData(oldp+20800,((0xfU & ((2U == (7U 
                                                 & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U]))
                                          ? ((IData)(1U) 
                                             + (0xfU 
                                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                   >> 5U)))
                                          : (0xfU & 
                                             (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                              >> 5U))))),4);
    bufp->fullCData(oldp+20801,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__nextHead),4);
    bufp->fullIData(oldp+20802,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__unnamedblk2__DOT__i),32);
    bufp->fullIData(oldp+20803,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__unnamedblk3__DOT__i),32);
    bufp->fullIData(oldp+20804,(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__unnamedblk4__DOT__i),32);
    bufp->fullBit(oldp+20805,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarReg[0]));
    bufp->fullBit(oldp+20806,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarReg[1]));
    bufp->fullCData(oldp+20807,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarRegNum[0]),7);
    bufp->fullCData(oldp+20808,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarRegNum[1]),7);
    bufp->fullCData(oldp+20809,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__scalarFreeListCount),6);
    bufp->fullBit(oldp+20810,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarFPReg[0]));
    bufp->fullBit(oldp+20811,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasePhyScalarFPReg[1]));
    bufp->fullCData(oldp+20812,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarFPRegNum[0]),7);
    bufp->fullCData(oldp+20813,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__releasedPhyScalarFPRegNum[1]),7);
    bufp->fullCData(oldp+20814,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList.__PVT__queuePointer__DOT__regCount),6);
    bufp->fullBit(oldp+20815,((1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtRecoveryIndex) 
                                     >> 5U))));
    bufp->fullCData(oldp+20816,((0x1fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtRecoveryIndex))),5);
    bufp->fullCData(oldp+20817,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogic.__PVT__rmtRecoveryCount),7);
    bufp->fullCData(oldp+20818,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regTail),6);
    bufp->fullCData(oldp+20819,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount),7);
    bufp->fullBit(oldp+20820,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[2U] 
                                     >> 6U))));
    bufp->fullIData(oldp+20821,((0x7ffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[2U] 
                                              << 0xdU) 
                                             | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                >> 0x13U)))),19);
    bufp->fullIData(oldp+20822,(((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                  << 0xdU) | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                              >> 0x13U))),32);
    bufp->fullCData(oldp+20823,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                          >> 0xdU))),6);
    bufp->fullCData(oldp+20824,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                         >> 5U))),4);
    bufp->fullBit(oldp+20825,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                     >> 4U))));
    bufp->fullCData(oldp+20826,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U])),4);
    bufp->fullCData(oldp+20827,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryEntryNum),7);
    bufp->fullCData(oldp+20828,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__nextRecoveryEntryNum),7);
    bufp->fullCData(oldp+20829,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__flushRangeHeadPtr),6);
    bufp->fullCData(oldp+20830,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__flushRangeTailPtr),6);
    bufp->fullBit(oldp+20831,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__regInRecovery));
    bufp->fullCData(oldp+20832,(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regHead),6);
    bufp->fullBit(oldp+20833,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                     [0U] >> 5U))));
    bufp->fullCData(oldp+20834,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                 [0U])),5);
    bufp->fullBit(oldp+20835,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                     [1U] >> 5U))));
    bufp->fullCData(oldp+20836,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWriteLogRegNum
                                 [1U])),5);
    bufp->fullCData(oldp+20837,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWritePhyRegNum[0]),6);
    bufp->fullCData(oldp+20838,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__rstWritePhyRegNum[1]),6);
    bufp->fullIData(oldp+20839,(vlSymsp->TOP__SMT_RTL_Testbench__core__rmt_wat.__PVT__unnamedblk5__DOT__i),32);
    bufp->fullBit(oldp+20840,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                     [0U] >> 5U))));
    bufp->fullCData(oldp+20841,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                 [0U])),5);
    bufp->fullBit(oldp+20842,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                     [1U] >> 5U))));
    bufp->fullCData(oldp+20843,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWriteLogRegNum
                                 [1U])),5);
    bufp->fullCData(oldp+20844,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWritePhyRegNum[0]),6);
    bufp->fullCData(oldp+20845,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__rstWritePhyRegNum[1]),6);
    bufp->fullCData(oldp+20846,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__readPhyRegNum[0]),6);
    bufp->fullCData(oldp+20847,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__readPhyRegNum[1]),6);
    bufp->fullIData(oldp+20848,(vlSymsp->TOP__SMT_RTL_Testbench__core__retirementRMT.__PVT__unnamedblk4__DOT__i),32);
    bufp->fullBit(oldp+20849,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg 
                                             >> 0x13U)))));
    bufp->fullIData(oldp+20850,((0x7ffffU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg))),19);
    bufp->fullBit(oldp+20851,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg 
                                             >> 0x27U)))));
    bufp->fullIData(oldp+20852,((0x7ffffU & (IData)(
                                                    (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__tagReg 
                                                     >> 0x14U)))),19);
    bufp->fullBit(oldp+20853,((0x20U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regCount))));
    bufp->fullBit(oldp+20854,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regCount))));
    bufp->fullIData(oldp+20855,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0U] >> 0x14U))),32);
    bufp->fullBit(oldp+20856,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0U] >> 0x13U)))));
    bufp->fullBit(oldp+20857,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0U] >> 0x12U)))));
    bufp->fullCData(oldp+20858,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20859,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20860,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0U]))));
    bufp->fullIData(oldp+20861,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [1U] >> 0x14U))),32);
    bufp->fullBit(oldp+20862,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [1U] >> 0x13U)))));
    bufp->fullBit(oldp+20863,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [1U] >> 0x12U)))));
    bufp->fullCData(oldp+20864,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [1U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20865,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [1U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20866,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [1U]))));
    bufp->fullIData(oldp+20867,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [2U] >> 0x14U))),32);
    bufp->fullBit(oldp+20868,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [2U] >> 0x13U)))));
    bufp->fullBit(oldp+20869,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [2U] >> 0x12U)))));
    bufp->fullCData(oldp+20870,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [2U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20871,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [2U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20872,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [2U]))));
    bufp->fullIData(oldp+20873,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [3U] >> 0x14U))),32);
    bufp->fullBit(oldp+20874,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [3U] >> 0x13U)))));
    bufp->fullBit(oldp+20875,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [3U] >> 0x12U)))));
    bufp->fullCData(oldp+20876,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [3U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20877,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [3U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20878,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [3U]))));
    bufp->fullIData(oldp+20879,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [4U] >> 0x14U))),32);
    bufp->fullBit(oldp+20880,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [4U] >> 0x13U)))));
    bufp->fullBit(oldp+20881,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [4U] >> 0x12U)))));
    bufp->fullCData(oldp+20882,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [4U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20883,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [4U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20884,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [4U]))));
    bufp->fullIData(oldp+20885,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [5U] >> 0x14U))),32);
    bufp->fullBit(oldp+20886,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [5U] >> 0x13U)))));
    bufp->fullBit(oldp+20887,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [5U] >> 0x12U)))));
    bufp->fullCData(oldp+20888,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [5U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20889,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [5U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20890,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [5U]))));
    bufp->fullIData(oldp+20891,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [6U] >> 0x14U))),32);
    bufp->fullBit(oldp+20892,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [6U] >> 0x13U)))));
    bufp->fullBit(oldp+20893,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [6U] >> 0x12U)))));
    bufp->fullCData(oldp+20894,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [6U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20895,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [6U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20896,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [6U]))));
    bufp->fullIData(oldp+20897,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [7U] >> 0x14U))),32);
    bufp->fullBit(oldp+20898,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [7U] >> 0x13U)))));
    bufp->fullBit(oldp+20899,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [7U] >> 0x12U)))));
    bufp->fullCData(oldp+20900,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [7U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20901,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [7U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20902,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [7U]))));
    bufp->fullIData(oldp+20903,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [8U] >> 0x14U))),32);
    bufp->fullBit(oldp+20904,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [8U] >> 0x13U)))));
    bufp->fullBit(oldp+20905,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [8U] >> 0x12U)))));
    bufp->fullCData(oldp+20906,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [8U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20907,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [8U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20908,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [8U]))));
    bufp->fullIData(oldp+20909,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [9U] >> 0x14U))),32);
    bufp->fullBit(oldp+20910,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [9U] >> 0x13U)))));
    bufp->fullBit(oldp+20911,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [9U] >> 0x12U)))));
    bufp->fullCData(oldp+20912,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [9U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20913,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [9U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20914,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [9U]))));
    bufp->fullIData(oldp+20915,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0xaU] >> 0x14U))),32);
    bufp->fullBit(oldp+20916,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xaU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20917,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xaU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20918,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0xaU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20919,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0xaU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20920,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0xaU]))));
    bufp->fullIData(oldp+20921,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0xbU] >> 0x14U))),32);
    bufp->fullBit(oldp+20922,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xbU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20923,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xbU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20924,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0xbU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20925,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0xbU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20926,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0xbU]))));
    bufp->fullIData(oldp+20927,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0xcU] >> 0x14U))),32);
    bufp->fullBit(oldp+20928,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xcU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20929,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xcU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20930,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0xcU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20931,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0xcU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20932,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0xcU]))));
    bufp->fullIData(oldp+20933,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0xdU] >> 0x14U))),32);
    bufp->fullBit(oldp+20934,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xdU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20935,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xdU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20936,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0xdU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20937,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0xdU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20938,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0xdU]))));
    bufp->fullIData(oldp+20939,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0xeU] >> 0x14U))),32);
    bufp->fullBit(oldp+20940,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xeU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20941,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xeU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20942,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0xeU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20943,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0xeU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20944,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0xeU]))));
    bufp->fullIData(oldp+20945,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0xfU] >> 0x14U))),32);
    bufp->fullBit(oldp+20946,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xfU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20947,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0xfU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20948,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0xfU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20949,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0xfU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20950,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0xfU]))));
    bufp->fullIData(oldp+20951,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x10U] >> 0x14U))),32);
    bufp->fullBit(oldp+20952,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x10U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20953,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x10U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20954,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x10U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20955,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x10U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20956,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x10U]))));
    bufp->fullIData(oldp+20957,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x11U] >> 0x14U))),32);
    bufp->fullBit(oldp+20958,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x11U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20959,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x11U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20960,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x11U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20961,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x11U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20962,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x11U]))));
    bufp->fullIData(oldp+20963,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x12U] >> 0x14U))),32);
    bufp->fullBit(oldp+20964,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x12U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20965,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x12U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20966,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x12U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20967,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x12U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20968,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x12U]))));
    bufp->fullIData(oldp+20969,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x13U] >> 0x14U))),32);
    bufp->fullBit(oldp+20970,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x13U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20971,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x13U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20972,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x13U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20973,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x13U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20974,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x13U]))));
    bufp->fullIData(oldp+20975,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x14U] >> 0x14U))),32);
    bufp->fullBit(oldp+20976,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x14U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20977,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x14U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20978,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x14U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20979,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x14U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20980,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x14U]))));
    bufp->fullIData(oldp+20981,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x15U] >> 0x14U))),32);
    bufp->fullBit(oldp+20982,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x15U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20983,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x15U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20984,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x15U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20985,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x15U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20986,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x15U]))));
    bufp->fullIData(oldp+20987,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x16U] >> 0x14U))),32);
    bufp->fullBit(oldp+20988,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x16U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20989,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x16U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20990,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x16U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20991,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x16U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20992,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x16U]))));
    bufp->fullIData(oldp+20993,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x17U] >> 0x14U))),32);
    bufp->fullBit(oldp+20994,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x17U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+20995,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x17U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+20996,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x17U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+20997,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x17U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+20998,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x17U]))));
    bufp->fullIData(oldp+20999,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x18U] >> 0x14U))),32);
    bufp->fullBit(oldp+21000,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x18U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21001,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x18U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21002,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x18U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21003,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x18U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21004,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x18U]))));
    bufp->fullIData(oldp+21005,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x19U] >> 0x14U))),32);
    bufp->fullBit(oldp+21006,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x19U] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21007,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x19U] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21008,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x19U] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21009,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x19U] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21010,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x19U]))));
    bufp->fullIData(oldp+21011,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x1aU] >> 0x14U))),32);
    bufp->fullBit(oldp+21012,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1aU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21013,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1aU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21014,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x1aU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21015,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x1aU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21016,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x1aU]))));
    bufp->fullIData(oldp+21017,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x1bU] >> 0x14U))),32);
    bufp->fullBit(oldp+21018,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1bU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21019,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1bU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21020,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x1bU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21021,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x1bU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21022,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x1bU]))));
    bufp->fullIData(oldp+21023,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x1cU] >> 0x14U))),32);
    bufp->fullBit(oldp+21024,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1cU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21025,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1cU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21026,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x1cU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21027,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x1cU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21028,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x1cU]))));
    bufp->fullIData(oldp+21029,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x1dU] >> 0x14U))),32);
    bufp->fullBit(oldp+21030,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1dU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21031,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1dU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21032,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x1dU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21033,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x1dU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21034,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x1dU]))));
    bufp->fullIData(oldp+21035,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x1eU] >> 0x14U))),32);
    bufp->fullBit(oldp+21036,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1eU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21037,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1eU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21038,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x1eU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21039,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x1eU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21040,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x1eU]))));
    bufp->fullIData(oldp+21041,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                         [0x1fU] >> 0x14U))),32);
    bufp->fullBit(oldp+21042,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1fU] 
                                             >> 0x13U)))));
    bufp->fullBit(oldp+21043,((1U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                             [0x1fU] 
                                             >> 0x12U)))));
    bufp->fullCData(oldp+21044,((0xfU & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                 [0x1fU] 
                                                 >> 0xeU)))),4);
    bufp->fullSData(oldp+21045,((0x1fffU & (IData)(
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                                    [0x1fU] 
                                                    >> 1U)))),13);
    bufp->fullBit(oldp+21046,((1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueue
                                            [0x1fU]))));
    bufp->fullCData(oldp+21047,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regHeadStorage),5);
    bufp->fullCData(oldp+21048,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regTailStorage),5);
    bufp->fullSData(oldp+21049,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__resetIndex),10);
    bufp->fullCData(oldp+21050,(vlSymsp->TOP__SMT_RTL_Testbench__core__btb.__PVT__btbQueuePointer__DOT__regCount),6);
    bufp->fullCData(oldp+21051,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[0]),4);
    bufp->fullCData(oldp+21052,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[1]),4);
    bufp->fullCData(oldp+21053,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[2]),4);
    bufp->fullCData(oldp+21054,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[3]),4);
    bufp->fullCData(oldp+21055,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[4]),4);
    bufp->fullCData(oldp+21056,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[5]),4);
    bufp->fullCData(oldp+21057,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[6]),4);
    bufp->fullCData(oldp+21058,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__releasePtr[7]),4);
    bufp->fullCData(oldp+21059,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__regCount),5);
    bufp->fullBit(oldp+21060,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__freeListReset));
    bufp->fullBit(oldp+21061,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__freeListResetCycleCount));
    bufp->fullSData(oldp+21062,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__flush),16);
    bufp->fullSData(oldp+21063,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__prevFlushAtRecovery),16);
    bufp->fullBit(oldp+21064,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__issueQueueReturnIndex));
    bufp->fullCData(oldp+21065,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__issueQueueReturnIndexCycleCount),3);
    bufp->fullCData(oldp+21066,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__returnIndexOffset),4);
    bufp->fullCData(oldp+21067,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__writePtr[0]),4);
    bufp->fullCData(oldp+21068,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__writePtr[1]),4);
    bufp->fullSData(oldp+21069,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [0U] >> 2U))),10);
    bufp->fullCData(oldp+21070,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [0U])),2);
    bufp->fullSData(oldp+21071,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [1U] >> 2U))),10);
    bufp->fullCData(oldp+21072,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [1U])),2);
    bufp->fullSData(oldp+21073,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [2U] >> 2U))),10);
    bufp->fullCData(oldp+21074,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [2U])),2);
    bufp->fullSData(oldp+21075,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [3U] >> 2U))),10);
    bufp->fullCData(oldp+21076,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [3U])),2);
    bufp->fullSData(oldp+21077,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [4U] >> 2U))),10);
    bufp->fullCData(oldp+21078,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [4U])),2);
    bufp->fullSData(oldp+21079,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [5U] >> 2U))),10);
    bufp->fullCData(oldp+21080,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [5U])),2);
    bufp->fullSData(oldp+21081,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [6U] >> 2U))),10);
    bufp->fullCData(oldp+21082,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [6U])),2);
    bufp->fullSData(oldp+21083,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [7U] >> 2U))),10);
    bufp->fullCData(oldp+21084,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [7U])),2);
    bufp->fullSData(oldp+21085,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [8U] >> 2U))),10);
    bufp->fullCData(oldp+21086,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [8U])),2);
    bufp->fullSData(oldp+21087,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [9U] >> 2U))),10);
    bufp->fullCData(oldp+21088,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [9U])),2);
    bufp->fullSData(oldp+21089,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [0xaU] >> 2U))),10);
    bufp->fullCData(oldp+21090,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [0xaU])),2);
    bufp->fullSData(oldp+21091,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [0xbU] >> 2U))),10);
    bufp->fullCData(oldp+21092,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [0xbU])),2);
    bufp->fullSData(oldp+21093,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [0xcU] >> 2U))),10);
    bufp->fullCData(oldp+21094,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [0xcU])),2);
    bufp->fullSData(oldp+21095,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [0xdU] >> 2U))),10);
    bufp->fullCData(oldp+21096,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [0xdU])),2);
    bufp->fullSData(oldp+21097,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [0xeU] >> 2U))),10);
    bufp->fullCData(oldp+21098,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [0xeU])),2);
    bufp->fullSData(oldp+21099,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                           [0xfU] >> 2U))),10);
    bufp->fullCData(oldp+21100,((3U & vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__opId
                                 [0xfU])),2);
    bufp->fullIData(oldp+21101,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk10__DOT__i),32);
    bufp->fullIData(oldp+21102,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk11__DOT__i),32);
    bufp->fullIData(oldp+21103,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk12__DOT__i),32);
    bufp->fullIData(oldp+21104,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk13__DOT__i),32);
    bufp->fullIData(oldp+21105,(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__unnamedblk8__DOT__i),32);
    bufp->fullCData(oldp+21106,(vlSymsp->TOP__SMT_RTL_Testbench__core__destinationRAM.__PVT__rstIndex),4);
    bufp->fullBit(oldp+21107,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                              [0U][0U]));
    bufp->fullBit(oldp+21108,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                              [0U][1U]));
    bufp->fullBit(oldp+21109,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                              [0U][2U]));
    bufp->fullBit(oldp+21110,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                              [1U][0U]));
    bufp->fullBit(oldp+21111,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                              [1U][1U]));
    bufp->fullBit(oldp+21112,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegValid
                              [1U][2U]));
    bufp->fullBit(oldp+21113,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                     [0U][0U] >> 6U))));
    bufp->fullCData(oldp+21114,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                 [0U][0U])),6);
    bufp->fullBit(oldp+21115,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                     [0U][1U] >> 6U))));
    bufp->fullCData(oldp+21116,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                 [0U][1U])),6);
    bufp->fullBit(oldp+21117,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                     [0U][2U] >> 6U))));
    bufp->fullCData(oldp+21118,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                 [0U][2U])),6);
    bufp->fullBit(oldp+21119,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                     [1U][0U] >> 6U))));
    bufp->fullCData(oldp+21120,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                 [1U][0U])),6);
    bufp->fullBit(oldp+21121,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                     [1U][1U] >> 6U))));
    bufp->fullCData(oldp+21122,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                 [1U][1U])),6);
    bufp->fullBit(oldp+21123,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                     [1U][2U] >> 6U))));
    bufp->fullCData(oldp+21124,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegNum
                                 [1U][2U])),6);
    bufp->fullBit(oldp+21125,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegValid[0]));
    bufp->fullBit(oldp+21126,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegValid[1]));
    bufp->fullBit(oldp+21127,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                     [0U] >> 6U))));
    bufp->fullCData(oldp+21128,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                 [0U])),6);
    bufp->fullBit(oldp+21129,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                     [1U] >> 6U))));
    bufp->fullCData(oldp+21130,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedDstRegNum
                                 [1U])),6);
    bufp->fullCData(oldp+21131,((0xfU & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr)),4);
    bufp->fullCData(oldp+21132,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                         >> 4U))),4);
    bufp->fullCData(oldp+21133,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                         >> 8U))),4);
    bufp->fullCData(oldp+21134,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                         >> 0xcU))),4);
    bufp->fullCData(oldp+21135,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                         >> 0x10U))),4);
    bufp->fullCData(oldp+21136,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__dispatchedSrcRegPtr 
                                         >> 0x14U))),4);
    bufp->fullSData(oldp+21137,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__PVT__storeBitVectorReg),16);
    bufp->fullCData(oldp+21138,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__Vcellinp__producerMatrix__dispatchPtr[0]),4);
    bufp->fullCData(oldp+21139,(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupLogic.__Vcellinp__producerMatrix__dispatchPtr[1]),4);
    bufp->fullSData(oldp+21140,(vlSymsp->TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor.__PVT__resetIndex),10);
    bufp->fullIData(oldp+21141,((0x3fffffU & (vlSymsp->TOP__SMT_RTL_Testbench__memory.__PVT__requestData[2U] 
                                              >> 7U))),22);
    bufp->fullIData(oldp+21142,(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__idStage__DOT__recoveredPC),32);
    bufp->fullBit(oldp+21143,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                     [0U] >> 0x13U))));
    bufp->fullIData(oldp+21144,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                 [0U])),19);
    bufp->fullBit(oldp+21145,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                     [1U] >> 0x13U))));
    bufp->fullIData(oldp+21146,((0x7ffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__rnStageIF.__PVT__pc
                                 [1U])),19);
    bufp->fullIData(oldp+21147,(vlSymsp->TOP__SMT_RTL_Testbench__core__mulDivUnitIF.__PVT__mulDataOut[0]),32);
    bufp->fullBit(oldp+21148,((0xdU >= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__loadQueue__DOT__loadQueuePointer__DOT__regCount))));
    bufp->fullBit(oldp+21149,((0xdU >= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount))));
    bufp->fullBit(oldp+21150,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrValid[0]));
    bufp->fullBit(oldp+21151,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrValid[1]));
    bufp->fullCData(oldp+21152,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrPhase
                                [0U]),5);
    bufp->fullCData(oldp+21153,(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__mshrPhase
                                [1U]),5);
    bufp->fullIData(oldp+21154,(vlSymsp->TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF.__PVT__DataOut[0]),32);
    bufp->fullBit(oldp+21155,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                     [0U] >> 5U))));
    bufp->fullCData(oldp+21156,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                 [0U])),5);
    bufp->fullBit(oldp+21157,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                     [1U] >> 5U))));
    bufp->fullCData(oldp+21158,((0x1fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__logSrcRegC
                                 [1U])),5);
    bufp->fullBit(oldp+21159,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegA[0]));
    bufp->fullBit(oldp+21160,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegA[1]));
    bufp->fullBit(oldp+21161,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegB[0]));
    bufp->fullBit(oldp+21162,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegB[1]));
    bufp->fullBit(oldp+21163,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegC[0]));
    bufp->fullBit(oldp+21164,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__readRegC[1]));
    bufp->fullBit(oldp+21165,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg[0]));
    bufp->fullBit(oldp+21166,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__writeReg[1]));
    bufp->fullBit(oldp+21167,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg[0]));
    bufp->fullBit(oldp+21168,(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__releaseReg[1]));
    bufp->fullBit(oldp+21169,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                     [0U] >> 6U))));
    bufp->fullCData(oldp+21170,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                 [0U])),6);
    bufp->fullBit(oldp+21171,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                     [1U] >> 6U))));
    bufp->fullCData(oldp+21172,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__phyReleasedReg
                                 [1U])),6);
    bufp->fullBit(oldp+21173,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                     [0U] >> 6U))));
    bufp->fullCData(oldp+21174,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                 [0U])),6);
    bufp->fullBit(oldp+21175,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                     [1U] >> 6U))));
    bufp->fullCData(oldp+21176,((0x3fU & vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__retRMT_ReadReg_PhyRegNum
                                 [1U])),6);
    bufp->fullBit(oldp+21177,((0x3eU >= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount))));
    bufp->fullSData(oldp+21178,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__regBrGlobalHistory),10);
    bufp->fullBit(oldp+21179,((0x20U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regCount))));
    bufp->fullBit(oldp+21180,((0U == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regCount))));
    bufp->fullIData(oldp+21181,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0U] >> 2U))),32);
    bufp->fullCData(oldp+21182,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0U]))),2);
    bufp->fullIData(oldp+21183,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [1U] >> 2U))),32);
    bufp->fullCData(oldp+21184,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [1U]))),2);
    bufp->fullIData(oldp+21185,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [2U] >> 2U))),32);
    bufp->fullCData(oldp+21186,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [2U]))),2);
    bufp->fullIData(oldp+21187,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [3U] >> 2U))),32);
    bufp->fullCData(oldp+21188,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [3U]))),2);
    bufp->fullIData(oldp+21189,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [4U] >> 2U))),32);
    bufp->fullCData(oldp+21190,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [4U]))),2);
    bufp->fullIData(oldp+21191,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [5U] >> 2U))),32);
    bufp->fullCData(oldp+21192,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [5U]))),2);
    bufp->fullIData(oldp+21193,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [6U] >> 2U))),32);
    bufp->fullCData(oldp+21194,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [6U]))),2);
    bufp->fullIData(oldp+21195,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [7U] >> 2U))),32);
    bufp->fullCData(oldp+21196,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [7U]))),2);
    bufp->fullIData(oldp+21197,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [8U] >> 2U))),32);
    bufp->fullCData(oldp+21198,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [8U]))),2);
    bufp->fullIData(oldp+21199,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [9U] >> 2U))),32);
    bufp->fullCData(oldp+21200,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [9U]))),2);
    bufp->fullIData(oldp+21201,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0xaU] >> 2U))),32);
    bufp->fullCData(oldp+21202,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0xaU]))),2);
    bufp->fullIData(oldp+21203,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0xbU] >> 2U))),32);
    bufp->fullCData(oldp+21204,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0xbU]))),2);
    bufp->fullIData(oldp+21205,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0xcU] >> 2U))),32);
    bufp->fullCData(oldp+21206,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0xcU]))),2);
    bufp->fullIData(oldp+21207,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0xdU] >> 2U))),32);
    bufp->fullCData(oldp+21208,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0xdU]))),2);
    bufp->fullIData(oldp+21209,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0xeU] >> 2U))),32);
    bufp->fullCData(oldp+21210,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0xeU]))),2);
    bufp->fullIData(oldp+21211,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0xfU] >> 2U))),32);
    bufp->fullCData(oldp+21212,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0xfU]))),2);
    bufp->fullIData(oldp+21213,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x10U] >> 2U))),32);
    bufp->fullCData(oldp+21214,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x10U]))),2);
    bufp->fullIData(oldp+21215,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x11U] >> 2U))),32);
    bufp->fullCData(oldp+21216,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x11U]))),2);
    bufp->fullIData(oldp+21217,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x12U] >> 2U))),32);
    bufp->fullCData(oldp+21218,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x12U]))),2);
    bufp->fullIData(oldp+21219,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x13U] >> 2U))),32);
    bufp->fullCData(oldp+21220,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x13U]))),2);
    bufp->fullIData(oldp+21221,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x14U] >> 2U))),32);
    bufp->fullCData(oldp+21222,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x14U]))),2);
    bufp->fullIData(oldp+21223,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x15U] >> 2U))),32);
    bufp->fullCData(oldp+21224,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x15U]))),2);
    bufp->fullIData(oldp+21225,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x16U] >> 2U))),32);
    bufp->fullCData(oldp+21226,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x16U]))),2);
    bufp->fullIData(oldp+21227,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x17U] >> 2U))),32);
    bufp->fullCData(oldp+21228,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x17U]))),2);
    bufp->fullIData(oldp+21229,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x18U] >> 2U))),32);
    bufp->fullCData(oldp+21230,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x18U]))),2);
    bufp->fullIData(oldp+21231,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x19U] >> 2U))),32);
    bufp->fullCData(oldp+21232,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x19U]))),2);
    bufp->fullIData(oldp+21233,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x1aU] >> 2U))),32);
    bufp->fullCData(oldp+21234,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x1aU]))),2);
    bufp->fullIData(oldp+21235,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x1bU] >> 2U))),32);
    bufp->fullCData(oldp+21236,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x1bU]))),2);
    bufp->fullIData(oldp+21237,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x1cU] >> 2U))),32);
    bufp->fullCData(oldp+21238,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x1cU]))),2);
    bufp->fullIData(oldp+21239,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x1dU] >> 2U))),32);
    bufp->fullCData(oldp+21240,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x1dU]))),2);
    bufp->fullIData(oldp+21241,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x1eU] >> 2U))),32);
    bufp->fullCData(oldp+21242,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x1eU]))),2);
    bufp->fullIData(oldp+21243,((IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                         [0x1fU] >> 2U))),32);
    bufp->fullCData(oldp+21244,((3U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueue
                                              [0x1fU]))),2);
    bufp->fullCData(oldp+21245,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regHeadStorage),5);
    bufp->fullCData(oldp+21246,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regTailStorage),5);
    bufp->fullSData(oldp+21247,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__resetIndex),11);
    bufp->fullCData(oldp+21248,(vlSymsp->TOP__SMT_RTL_Testbench__core__brPred__predictor.__PVT__phtQueuePointer__DOT__regCount),6);
    bufp->fullBit(oldp+21249,(((~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__freeListReset)) 
                               & (2U <= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__regCount)))));
    bufp->fullCData(oldp+21250,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr[0]),4);
    bufp->fullCData(oldp+21251,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr[1]),4);
    bufp->fullCData(oldp+21252,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr[0]),6);
    bufp->fullCData(oldp+21253,(vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr[1]),6);
    bufp->fullSData(oldp+21254,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [0U][4U] 
                                           >> 1U))),10);
    bufp->fullCData(oldp+21255,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                        [0U][4U] << 1U) 
                                       | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [0U][3U] 
                                          >> 0x1fU)))),2);
    bufp->fullCData(oldp+21256,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][3U] >> 0x1dU))),2);
    bufp->fullCData(oldp+21257,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][3U] >> 0x1bU))),2);
    bufp->fullCData(oldp+21258,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [0U][3U] >> 0x17U))),4);
    bufp->fullBit(oldp+21259,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][3U] >> 0x16U))));
    bufp->fullIData(oldp+21260,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                 [0U][3U] 
                                                 << 8U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                   [0U][2U] 
                                                   >> 0x18U)))),30);
    bufp->fullIData(oldp+21261,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [0U][2U] 
                                             >> 6U))),18);
    bufp->fullBit(oldp+21262,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][3U] >> 0x1aU))));
    bufp->fullIData(oldp+21263,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [0U][3U] 
                                             >> 7U))),19);
    bufp->fullBit(oldp+21264,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][3U] >> 6U))));
    bufp->fullSData(oldp+21265,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [0U][3U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                              [0U][2U] 
                                              >> 0x1cU)))),10);
    bufp->fullCData(oldp+21266,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][2U] >> 0x1aU))),2);
    bufp->fullIData(oldp+21267,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [0U][2U] 
                                             >> 6U))),20);
    bufp->fullCData(oldp+21268,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [0U][2U] >> 3U))),3);
    bufp->fullCData(oldp+21269,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                 [0U][2U])),3);
    bufp->fullCData(oldp+21270,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                 [0U][1U] >> 0x1aU)),6);
    bufp->fullCData(oldp+21271,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [0U][1U] >> 0x16U))),4);
    bufp->fullCData(oldp+21272,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [0U][1U] >> 0x12U))),4);
    bufp->fullBit(oldp+21273,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][1U] >> 0x11U))));
    bufp->fullCData(oldp+21274,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [0U][1U] 
                                          >> 0xbU))),6);
    bufp->fullBit(oldp+21275,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][1U] >> 0xaU))));
    bufp->fullCData(oldp+21276,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [0U][1U] 
                                          >> 4U))),6);
    bufp->fullBit(oldp+21277,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][1U] >> 3U))));
    bufp->fullCData(oldp+21278,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [0U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [0U][0U] 
                                           >> 0x1dU)))),6);
    bufp->fullBit(oldp+21279,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][0U] >> 0x1cU))));
    bufp->fullBit(oldp+21280,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][0U] >> 0x1bU))));
    bufp->fullCData(oldp+21281,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [0U][0U] 
                                          >> 0x15U))),6);
    bufp->fullBit(oldp+21282,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [0U][0U] >> 0x14U))));
    bufp->fullIData(oldp+21283,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [0U][0U] 
                                             >> 1U))),19);
    bufp->fullBit(oldp+21284,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                               [0U][0U])));
    bufp->fullSData(oldp+21285,((0x3ffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [1U][4U] 
                                           >> 1U))),10);
    bufp->fullCData(oldp+21286,((3U & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                        [1U][4U] << 1U) 
                                       | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [1U][3U] 
                                          >> 0x1fU)))),2);
    bufp->fullCData(oldp+21287,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][3U] >> 0x1dU))),2);
    bufp->fullCData(oldp+21288,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][3U] >> 0x1bU))),2);
    bufp->fullCData(oldp+21289,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [1U][3U] >> 0x17U))),4);
    bufp->fullBit(oldp+21290,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][3U] >> 0x16U))));
    bufp->fullIData(oldp+21291,((0x3fffffffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                 [1U][3U] 
                                                 << 8U) 
                                                | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                   [1U][2U] 
                                                   >> 0x18U)))),30);
    bufp->fullIData(oldp+21292,((0x3ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [1U][2U] 
                                             >> 6U))),18);
    bufp->fullBit(oldp+21293,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][3U] >> 0x1aU))));
    bufp->fullIData(oldp+21294,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [1U][3U] 
                                             >> 7U))),19);
    bufp->fullBit(oldp+21295,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][3U] >> 6U))));
    bufp->fullSData(oldp+21296,((0x3ffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                            [1U][3U] 
                                            << 4U) 
                                           | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                              [1U][2U] 
                                              >> 0x1cU)))),10);
    bufp->fullCData(oldp+21297,((3U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][2U] >> 0x1aU))),2);
    bufp->fullIData(oldp+21298,((0xfffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [1U][2U] 
                                             >> 6U))),20);
    bufp->fullCData(oldp+21299,((7U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                       [1U][2U] >> 3U))),3);
    bufp->fullCData(oldp+21300,((7U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                 [1U][2U])),3);
    bufp->fullCData(oldp+21301,((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                 [1U][1U] >> 0x1aU)),6);
    bufp->fullCData(oldp+21302,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [1U][1U] >> 0x16U))),4);
    bufp->fullCData(oldp+21303,((0xfU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                         [1U][1U] >> 0x12U))),4);
    bufp->fullBit(oldp+21304,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][1U] >> 0x11U))));
    bufp->fullCData(oldp+21305,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [1U][1U] 
                                          >> 0xbU))),6);
    bufp->fullBit(oldp+21306,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][1U] >> 0xaU))));
    bufp->fullCData(oldp+21307,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [1U][1U] 
                                          >> 4U))),6);
    bufp->fullBit(oldp+21308,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][1U] >> 3U))));
    bufp->fullCData(oldp+21309,((0x3fU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [1U][1U] 
                                           << 3U) | 
                                          (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                           [1U][0U] 
                                           >> 0x1dU)))),6);
    bufp->fullBit(oldp+21310,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][0U] >> 0x1cU))));
    bufp->fullBit(oldp+21311,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][0U] >> 0x1bU))));
    bufp->fullCData(oldp+21312,((0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                          [1U][0U] 
                                          >> 0x15U))),6);
    bufp->fullBit(oldp+21313,((1U & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                     [1U][0U] >> 0x14U))));
    bufp->fullIData(oldp+21314,((0x7ffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                             [1U][0U] 
                                             >> 1U))),19);
    bufp->fullBit(oldp+21315,((1U & vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                               [1U][0U])));
}
