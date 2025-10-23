// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__fflagsState__0(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5___nba_sequent__TOP__SMT_RTL_Testbench__core__activeList__fflagsState__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*4:0*/ __VdlyVal__debugValue__v0;
    __VdlyVal__debugValue__v0 = 0;
    CData/*5:0*/ __VdlyDim0__debugValue__v0;
    __VdlyDim0__debugValue__v0 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v0;
    __VdlySet__debugValue__v0 = 0;
    CData/*4:0*/ __VdlyVal__debugValue__v1;
    __VdlyVal__debugValue__v1 = 0;
    CData/*5:0*/ __VdlyDim0__debugValue__v1;
    __VdlyDim0__debugValue__v1 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v1;
    __VdlySet__debugValue__v1 = 0;
    CData/*4:0*/ __VdlyVal__debugValue__v2;
    __VdlyVal__debugValue__v2 = 0;
    CData/*5:0*/ __VdlyDim0__debugValue__v2;
    __VdlyDim0__debugValue__v2 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v2;
    __VdlySet__debugValue__v2 = 0;
    // Body
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [0U]] != vlSelfRef.__PVT__rv[0U])))) {
        VL_WRITEF_NX("The read output of a port(00000000) is incorrect.\n",0);
    }
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [1U]] != vlSelfRef.__PVT__rv[1U])))) {
        VL_WRITEF_NX("The read output of a port(00000001) is incorrect.\n",0);
    }
    __VdlySet__debugValue__v0 = 0U;
    __VdlySet__debugValue__v1 = 0U;
    __VdlySet__debugValue__v2 = 0U;
    vlSelfRef.__PVT__unnamedblk1__DOT__i = 3U;
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
    if (vlSelfRef.__PVT__we[2U]) {
        __VdlyVal__debugValue__v2 = vlSelfRef.__PVT__wv
            [2U];
        __VdlyDim0__debugValue__v2 = vlSelfRef.__PVT__wa
            [2U];
        __VdlySet__debugValue__v2 = 1U;
    }
    if (__VdlySet__debugValue__v0) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v0] 
            = __VdlyVal__debugValue__v0;
    }
    if (__VdlySet__debugValue__v1) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v1] 
            = __VdlyVal__debugValue__v1;
    }
    if (__VdlySet__debugValue__v2) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v2] 
            = __VdlyVal__debugValue__v2;
    }
}
