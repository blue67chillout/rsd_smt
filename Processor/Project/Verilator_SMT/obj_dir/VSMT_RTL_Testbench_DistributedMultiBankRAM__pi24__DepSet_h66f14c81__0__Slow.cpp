// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24___stl_comb__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList__0(VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24___stl_comb__TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (0U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[0U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[0U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[0U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
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
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (1U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[1U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[1U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[1U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel2;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel2: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[2U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[2U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[2U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (2U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[2U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[2U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[2U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel3;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel3: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[3U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[3U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[3U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (3U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[3U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[3U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[3U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel4;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel4: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[4U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[4U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[4U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (4U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[4U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[4U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[4U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel5;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel5: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[5U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[5U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[5U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (5U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[5U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[5U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[5U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel6;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel6: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[6U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[6U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[6U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (6U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[6U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[6U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[6U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel7;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel7: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[7U] = 0U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[7U] 
        = vlSelfRef.__PVT__wa[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[7U] 
        = vlSelfRef.__PVT__wv[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)) {
            if ((vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)] 
                 & (7U == (7U & vlSelfRef.__PVT__wa
                           [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)])))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__weBank[7U] 
                    = vlSelfRef.__PVT__we[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__waBank[7U] 
                    = vlSelfRef.__PVT__wa[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__wvBank[7U] 
                    = vlSelfRef.__PVT__wv[(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i)];
                goto __Vlabel8;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i);
        }
        __Vlabel8: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b = 8U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[0U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((0U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
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
            if ((1U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[1U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel10;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel10: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[2U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((2U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[2U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel11;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel11: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[3U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((3U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[3U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel12;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel12: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[4U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((4U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[4U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel13;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel13: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[5U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((5U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[5U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel14;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel14: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[6U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((6U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[6U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel15;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel15: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[7U] 
        = vlSelfRef.__PVT__ra[0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)) {
            if ((7U == (7U & vlSelfRef.__PVT__ra[(1U 
                                                  & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)]))) {
                vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank[7U] 
                    = vlSelfRef.__PVT__ra[(1U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i)];
                goto __Vlabel16;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel16: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b = 8U;
    vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
        [0U];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0U;
    {
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((7U & vlSelfRef.__PVT__ra[0U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[0U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
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
        while (VL_GTS_III(32, 8U, vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
            if (((7U & vlSelfRef.__PVT__ra[1U]) == vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)) {
                vlSelfRef.__PVT__rv[1U] = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank
                    [(7U & vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b)];
                goto __Vlabel18;
            }
            vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b 
                = ((IData)(1U) + vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b);
        }
        __Vlabel18: ;
    }
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i = 2U;
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[0U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [0U] >> 3U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[1U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [1U] >> 3U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[2U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__2__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [2U] >> 3U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[3U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__3__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [3U] >> 3U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[4U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__4__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [4U] >> 3U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[5U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__5__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [5U] >> 3U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[6U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__6__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [6U] >> 3U))];
    vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__rvBank[7U] 
        = vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__7__KET____DOT__genblk1__DOT__rBank__DOT__array
        [(1U & (vlSelfRef.__PVT__genblk1__DOT__rBank__DOT__raBank
                [7U] >> 3U))];
}

VL_ATTR_COLD void VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24___ctor_var_reset(VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__wv[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ra[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rv[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->debugValue[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->__PVT__unnamedblk3__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__waBank[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__raBank[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__rvBank[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__wvBank[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__weBank[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__b = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__b = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__unnamedblk5__DOT__unnamedblk6__DOT__b = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__0__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__1__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__2__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__2__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__2__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__3__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__3__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__3__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__4__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__4__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__4__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__5__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__5__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__5__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__6__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__6__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__6__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__7__KET____DOT__genblk1__DOT__rBank__DOT__array[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__7__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__genblk1__DOT__rBank__DOT__genblk2__BRA__7__KET____DOT__genblk1__DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
}
