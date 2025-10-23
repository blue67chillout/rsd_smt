// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11___stl_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM__0(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11___stl_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<4>/*127:0*/ __Vtemp_1;
    VlWide<4>/*127:0*/ __Vtemp_2;
    // Body
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr[1U] 
        = vlSelfRef.__PVT__wa[1U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[0U][1U][0U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [1U]][0U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[0U][1U][1U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [1U]][1U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[0U][1U][2U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [1U]][2U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[0U][1U][3U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [1U]][3U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[1U][0U][0U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [0U]][0U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[1U][0U][1U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [0U]][1U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[1U][0U][2U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [0U]][2U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue[1U][0U][3U] 
        = vlSelfRef.__PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array
        [vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadAddr
        [0U]][3U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][0U] 
        = vlSelfRef.__PVT__wv[0U][0U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][1U] 
        = vlSelfRef.__PVT__wv[0U][1U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][2U] 
        = vlSelfRef.__PVT__wv[0U][2U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][3U] 
        = vlSelfRef.__PVT__wv[0U][3U];
    __Vtemp_1[1U] = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
                     [0U][1U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
                     [1U][0U][1U]);
    __Vtemp_1[2U] = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
                     [0U][2U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
                     [1U][0U][2U]);
    __Vtemp_1[3U] = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
                     [0U][3U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
                     [1U][0U][3U]);
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][0U] 
        = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
           [0U][0U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
           [1U][0U][0U]);
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][1U] 
        = __Vtemp_1[1U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][2U] 
        = __Vtemp_1[2U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[0U][3U] 
        = __Vtemp_1[3U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][0U] 
        = vlSelfRef.__PVT__wv[1U][0U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][1U] 
        = vlSelfRef.__PVT__wv[1U][1U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][2U] 
        = vlSelfRef.__PVT__wv[1U][2U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][3U] 
        = vlSelfRef.__PVT__wv[1U][3U];
    __Vtemp_2[1U] = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
                     [1U][1U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
                     [0U][1U][1U]);
    __Vtemp_2[2U] = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
                     [1U][2U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
                     [0U][1U][2U]);
    __Vtemp_2[3U] = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
                     [1U][3U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
                     [0U][1U][3U]);
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][0U] 
        = (vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue
           [1U][0U] ^ vlSelfRef.__PVT__genblk1__DOT__body__DOT__wbReadValue
           [0U][1U][0U]);
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][1U] 
        = __Vtemp_2[1U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][2U] 
        = __Vtemp_2[2U];
    vlSelfRef.__PVT__genblk1__DOT__body__DOT__rwbWriteValue[1U][3U] 
        = __Vtemp_2[3U];
}

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11___ctor_var_reset(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__wv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ra[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__rv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->debugValue[__Vi0]);
    }
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__rwbWriteValue[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__body__DOT__wbReadAddr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__wbReadValue[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__body__DOT__rbReadAddr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__rbReadValue[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__debugValue[__Vi0]);
    }
    vlSelf->__PVT__genblk1__DOT__body__DOT__unnamedblk7__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->genblk1__DOT__body__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT____Vlvbound_hbbd7c02b__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->genblk1__DOT__body__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT____Vlvbound_hbbd7c02b__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->genblk1__DOT__body__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->genblk1__DOT__body__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT____Vlvbound_hbbd7c02b__0 = VL_RAND_RESET_I(1);
}
