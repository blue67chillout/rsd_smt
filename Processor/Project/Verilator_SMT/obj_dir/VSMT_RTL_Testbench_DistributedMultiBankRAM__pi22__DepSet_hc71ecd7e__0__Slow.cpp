// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___stl_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList__0(VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___stl_comb__TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[0U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(0xfU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                  [0U] >> 1U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[1U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(0xfU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                  [1U] >> 1U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (~ vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel1;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel1: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel2;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel2: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b = 2U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((1U & (~ vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel3;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel3: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((1U & vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel4;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel4: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b = 2U;
    vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__ra[0U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
                goto __Vlabel5;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b);
        }
        __Vlabel5: ;
    }
    vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__ra[1U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
                goto __Vlabel6;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b);
        }
        __Vlabel6: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i = 2U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___stl_comb__TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList__0(VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___stl_comb__TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[0U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(0xfU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                  [0U] >> 1U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[1U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(0xfU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                  [1U] >> 1U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (~ vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel7;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel7: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel8;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel8: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b = 2U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((1U & (~ vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel9;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel9: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((1U & vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel10;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel10: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b = 2U;
    vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__ra[0U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
                goto __Vlabel11;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b);
        }
        __Vlabel11: ;
    }
    vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__ra[1U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
                goto __Vlabel12;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b);
        }
        __Vlabel12: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i = 2U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___stl_comb__TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList__0(VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___stl_comb__TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[0U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(0xfU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                  [0U] >> 1U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[1U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(0xfU & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                  [1U] >> 1U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (~ vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel13;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel13: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] 
                    = vlSelfRef.__PVT__we[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
                    = vlSelfRef.__PVT__wa[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
                    = vlSelfRef.__PVT__wv[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel14;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel14: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b = 2U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((1U & (~ vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel15;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel15: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((1U & vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)])) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel16;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel16: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b = 2U;
    vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__ra[0U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
                goto __Vlabel17;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b);
        }
        __Vlabel17: ;
    }
    vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((1U & vlSelfRef.__PVT__ra[1U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
                goto __Vlabel18;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b);
        }
        __Vlabel18: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i = 2U;
}

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___ctor_var_reset(VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wa[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wv[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ra[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rv[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 32; ++__Vi0) {
        vlSelf->debugValue[__Vi0] = VL_RAND_RESET_I(7);
    }
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->__PVT__unnamedblk3__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__waBank[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__raBank[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__rvBank[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__wvBank[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__weBank[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0;
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(7);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(7);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
}
