// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___eval_initial__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht(VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___eval_initial__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__unnamedblk1__DOT__i = 0U;
    while (VL_GTS_III(32, 0x800U, vlSelfRef.__PVT__unnamedblk1__DOT__i)) {
        vlSelfRef.debugValue[(0x7ffU & vlSelfRef.__PVT__unnamedblk1__DOT__i)] 
            = (2U & vlSelfRef.debugValue[(0x7ffU & vlSelfRef.__PVT__unnamedblk1__DOT__i)]);
        vlSelfRef.debugValue[(0x7ffU & vlSelfRef.__PVT__unnamedblk1__DOT__i)] 
            = (1U & vlSelfRef.debugValue[(0x7ffU & vlSelfRef.__PVT__unnamedblk1__DOT__i)]);
        vlSelfRef.__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 2U;
        vlSelfRef.__PVT__unnamedblk1__DOT__i = ((IData)(1U) 
                                                + vlSelfRef.__PVT__unnamedblk1__DOT__i);
    }
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___act_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht__0(VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___act_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)] 
                 & (~ vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                goto __Vlabel1;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i);
        }
        __Vlabel1: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)] 
                 & vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                goto __Vlabel2;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i);
        }
        __Vlabel2: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__b = 2U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)) {
            if ((1U & (~ vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)];
                goto __Vlabel3;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i);
        }
        __Vlabel3: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)) {
            if ((1U & vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)];
                goto __Vlabel4;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i);
        }
        __Vlabel4: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__b = 2U;
    vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raReg
                  [0U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
                vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)];
                goto __Vlabel5;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b);
        }
        __Vlabel5: ;
    }
    vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raReg
                  [1U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
                vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)];
                goto __Vlabel6;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b);
        }
        __Vlabel6: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__i = 2U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___nba_sequent__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht__0(VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___nba_sequent__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*1:0*/ __VdlyVal__rvReg__v0;
    __VdlyVal__rvReg__v0 = 0;
    CData/*1:0*/ __VdlyVal__rvReg__v1;
    __VdlyVal__rvReg__v1 = 0;
    CData/*1:0*/ __VdlyVal__debugValue__v0;
    __VdlyVal__debugValue__v0 = 0;
    SData/*10:0*/ __VdlyDim0__debugValue__v0;
    __VdlyDim0__debugValue__v0 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v0;
    __VdlySet__debugValue__v0 = 0;
    CData/*1:0*/ __VdlyVal__debugValue__v1;
    __VdlyVal__debugValue__v1 = 0;
    SData/*10:0*/ __VdlyDim0__debugValue__v1;
    __VdlyDim0__debugValue__v1 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v1;
    __VdlySet__debugValue__v1 = 0;
    SData/*10:0*/ __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v0;
    __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v0 = 0;
    SData/*10:0*/ __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v1;
    __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v1 = 0;
    CData/*1:0*/ __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0;
    __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0 = 0;
    SData/*9:0*/ __VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0;
    __VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0 = 0;
    CData/*0:0*/ __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0;
    __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0 = 0;
    CData/*1:0*/ __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0;
    __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0 = 0;
    SData/*9:0*/ __VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0;
    __VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0 = 0;
    CData/*0:0*/ __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0;
    __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0 = 0;
    // Body
    if (VL_UNLIKELY(((vlSelfRef.__PVT__rvReg[0U] != 
                      vlSelfRef.__PVT__rv[0U])))) {
        VL_WRITEF_NX("The read output of a port(00000000) is incorrect\n",0);
    }
    if (VL_UNLIKELY(((vlSelfRef.__PVT__rvReg[1U] != 
                      vlSelfRef.__PVT__rv[1U])))) {
        VL_WRITEF_NX("The read output of a port(00000001) is incorrect\n",0);
    }
    if (VL_UNLIKELY((((1U & vlSelfRef.__PVT__ra[1U]) 
                      == (1U & vlSelfRef.__PVT__ra[0U]))))) {
        VL_WRITEF_NX("Multiple ports(00000001,00000000) read from the same bank.\n",0);
    }
    if (VL_UNLIKELY((((1U & vlSelfRef.__PVT__ra[0U]) 
                      == (1U & vlSelfRef.__PVT__ra[1U]))))) {
        VL_WRITEF_NX("Multiple ports(00000000,00000001) read from the same bank.\n",0);
    }
    __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0 = 0U;
    __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0 = 0U;
    if (VL_UNLIKELY((((vlSelfRef.__PVT__we[1U] & vlSelfRef.__PVT__we
                       [0U]) & ((1U & vlSelfRef.__PVT__wa
                                 [1U]) == (1U & vlSelfRef.__PVT__wa
                                           [0U])))))) {
        VL_WRITEF_NX("Multiple ports(00000001,00000000) write to the same bank.\n",0);
    }
    if (VL_UNLIKELY((((vlSelfRef.__PVT__we[0U] & vlSelfRef.__PVT__we
                       [1U]) & ((1U & vlSelfRef.__PVT__wa
                                 [0U]) == (1U & vlSelfRef.__PVT__wa
                                           [1U])))))) {
        VL_WRITEF_NX("Multiple ports(00000000,00000001) write to the same bank.\n",0);
    }
    __VdlySet__debugValue__v0 = 0U;
    __VdlySet__debugValue__v1 = 0U;
    vlSelfRef.__PVT__unnamedblk3__DOT__i = 2U;
    vlSelfRef.__PVT__unnamedblk4__DOT__i = 2U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 2U;
    __VdlyVal__rvReg__v0 = vlSelfRef.debugValue[vlSelfRef.__PVT__ra
        [0U]];
    __VdlyVal__rvReg__v1 = vlSelfRef.debugValue[vlSelfRef.__PVT__ra
        [1U]];
    if (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank
        [0U]) {
        __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0 
            = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank
            [0U];
        __VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0 
            = (0x3ffU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank
                         [0U] >> 1U));
        __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0 = 1U;
    }
    if (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank
        [1U]) {
        __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0 
            = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank
            [1U];
        __VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0 
            = (0x3ffU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank
                         [1U] >> 1U));
        __VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0 = 1U;
    }
    __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v0 
        = vlSelfRef.__PVT__ra[0U];
    __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v1 
        = vlSelfRef.__PVT__ra[1U];
    if (vlSelfRef.__PVT__we[0U]) {
        __VdlyVal__debugValue__v0 = vlSelfRef.__PVT__wv
            [0U];
        __VdlyDim0__debugValue__v0 = vlSelfRef.__PVT__wa
            [0U];
        __VdlySet__debugValue__v0 = 1U;
    }
    if (vlSelfRef.__PVT__we[1U]) {
        __VdlyVal__debugValue__v1 = vlSelfRef.__PVT__wv
            [1U];
        __VdlyDim0__debugValue__v1 = vlSelfRef.__PVT__wa
            [1U];
        __VdlySet__debugValue__v1 = 1U;
    }
    vlSelfRef.genblk1__DOT__rBank__DOT____Vcellout__genblk2__BRA__0__KET____DOT__rBank____pinNumber6 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array
        [(0x3ffU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                    [0U] >> 1U))];
    vlSelfRef.genblk1__DOT__rBank__DOT____Vcellout__genblk2__BRA__1__KET____DOT__rBank____pinNumber6 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array
        [(0x3ffU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                    [1U] >> 1U))];
    vlSelfRef.__PVT__rvReg[0U] = __VdlyVal__rvReg__v0;
    vlSelfRef.__PVT__rvReg[1U] = __VdlyVal__rvReg__v1;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raReg[0U] 
        = __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v0;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raReg[1U] 
        = __VdlyVal__genblk1__DOT__rBank__DOT__raReg__v1;
    if (__VdlySet__debugValue__v0) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v0] 
            = __VdlyVal__debugValue__v0;
    }
    if (__VdlySet__debugValue__v1) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v1] 
            = __VdlyVal__debugValue__v1;
    }
    if (__VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0) {
        vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array[__VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0] 
            = __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__rBank__DOT__array__v0;
    }
    if (__VdlySet__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0) {
        vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array[__VdlyDim0__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0] 
            = __VdlyVal__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__rBank__DOT__array__v0;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[0U] 
        = vlSelfRef.genblk1__DOT__rBank__DOT____Vcellout__genblk2__BRA__0__KET____DOT__rBank____pinNumber6;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[1U] 
        = vlSelfRef.genblk1__DOT__rBank__DOT____Vcellout__genblk2__BRA__1__KET____DOT__rBank____pinNumber6;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___nba_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht__0(VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2___nba_comb__TOP__SMT_RTL_Testbench__core__brPred__predictor__pht__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)] 
                 & (~ vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                goto __Vlabel7;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i);
        }
        __Vlabel7: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)] 
                 & vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i)];
                goto __Vlabel8;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__unnamedblk3__DOT__i);
        }
        __Vlabel8: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk2__DOT__b = 2U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)) {
            if ((1U & (~ vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)];
                goto __Vlabel9;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i);
        }
        __Vlabel9: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)) {
            if ((1U & vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i)];
                goto __Vlabel10;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__unnamedblk5__DOT__i);
        }
        __Vlabel10: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk4__DOT__b = 2U;
    vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raReg
                  [0U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
                vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)];
                goto __Vlabel11;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b);
        }
        __Vlabel11: ;
    }
    vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raReg
                  [1U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)) {
                vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b)];
                goto __Vlabel12;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__unnamedblk7__DOT__b);
        }
        __Vlabel12: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk6__DOT__i = 2U;
}
