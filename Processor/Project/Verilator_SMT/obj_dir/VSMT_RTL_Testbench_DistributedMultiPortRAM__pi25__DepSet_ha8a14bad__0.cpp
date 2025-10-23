// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__0(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25___nba_sequent__TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __VdlyVal__debugValue__v0;
    __VdlyVal__debugValue__v0 = 0;
    CData/*6:0*/ __VdlyDim0__debugValue__v0;
    __VdlyDim0__debugValue__v0 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v0;
    __VdlySet__debugValue__v0 = 0;
    CData/*0:0*/ __VdlyVal__debugValue__v1;
    __VdlyVal__debugValue__v1 = 0;
    CData/*6:0*/ __VdlyDim0__debugValue__v1;
    __VdlyDim0__debugValue__v1 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v1;
    __VdlySet__debugValue__v1 = 0;
    CData/*0:0*/ __VdlyVal__debugValue__v2;
    __VdlyVal__debugValue__v2 = 0;
    CData/*6:0*/ __VdlyDim0__debugValue__v2;
    __VdlyDim0__debugValue__v2 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v2;
    __VdlySet__debugValue__v2 = 0;
    CData/*0:0*/ __VdlyVal__debugValue__v3;
    __VdlyVal__debugValue__v3 = 0;
    CData/*6:0*/ __VdlyDim0__debugValue__v3;
    __VdlyDim0__debugValue__v3 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v3;
    __VdlySet__debugValue__v3 = 0;
    CData/*0:0*/ __VdlyVal__debugValue__v4;
    __VdlyVal__debugValue__v4 = 0;
    CData/*6:0*/ __VdlyDim0__debugValue__v4;
    __VdlyDim0__debugValue__v4 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v4;
    __VdlySet__debugValue__v4 = 0;
    CData/*0:0*/ __VdlyVal__debugValue__v5;
    __VdlyVal__debugValue__v5 = 0;
    CData/*6:0*/ __VdlyDim0__debugValue__v5;
    __VdlyDim0__debugValue__v5 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v5;
    __VdlySet__debugValue__v5 = 0;
    CData/*0:0*/ __VdlyVal__debugValue__v6;
    __VdlyVal__debugValue__v6 = 0;
    CData/*6:0*/ __VdlyDim0__debugValue__v6;
    __VdlyDim0__debugValue__v6 = 0;
    CData/*0:0*/ __VdlySet__debugValue__v6;
    __VdlySet__debugValue__v6 = 0;
    // Body
    __VdlySet__debugValue__v0 = 0U;
    __VdlySet__debugValue__v1 = 0U;
    __VdlySet__debugValue__v2 = 0U;
    __VdlySet__debugValue__v3 = 0U;
    __VdlySet__debugValue__v4 = 0U;
    __VdlySet__debugValue__v5 = 0U;
    __VdlySet__debugValue__v6 = 0U;
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [0U]] != vlSelfRef.__PVT__rv[0U])))) {
        VL_WRITEF_NX("The read output of a port(00000000) is incorrect.\n",0);
    }
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [1U]] != vlSelfRef.__PVT__rv[1U])))) {
        VL_WRITEF_NX("The read output of a port(00000001) is incorrect.\n",0);
    }
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [2U]] != vlSelfRef.__PVT__rv[2U])))) {
        VL_WRITEF_NX("The read output of a port(00000002) is incorrect.\n",0);
    }
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [3U]] != vlSelfRef.__PVT__rv[3U])))) {
        VL_WRITEF_NX("The read output of a port(00000003) is incorrect.\n",0);
    }
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [4U]] != vlSelfRef.__PVT__rv[4U])))) {
        VL_WRITEF_NX("The read output of a port(00000004) is incorrect.\n",0);
    }
    if (VL_UNLIKELY(((vlSelfRef.debugValue[vlSelfRef.__PVT__ra
                      [5U]] != vlSelfRef.__PVT__rv[5U])))) {
        VL_WRITEF_NX("The read output of a port(00000005) is incorrect.\n",0);
    }
    vlSelfRef.__PVT__unnamedblk1__DOT__i = 7U;
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
    if (vlSelfRef.__PVT__we[3U]) {
        __VdlyVal__debugValue__v3 = vlSelfRef.__PVT__wv
            [3U];
        __VdlyDim0__debugValue__v3 = vlSelfRef.__PVT__wa
            [3U];
        __VdlySet__debugValue__v3 = 1U;
    }
    if (vlSelfRef.__PVT__we[4U]) {
        __VdlyVal__debugValue__v4 = vlSelfRef.__PVT__wv
            [4U];
        __VdlyDim0__debugValue__v4 = vlSelfRef.__PVT__wa
            [4U];
        __VdlySet__debugValue__v4 = 1U;
    }
    if (vlSelfRef.__PVT__we[5U]) {
        __VdlyVal__debugValue__v5 = vlSelfRef.__PVT__wv
            [5U];
        __VdlyDim0__debugValue__v5 = vlSelfRef.__PVT__wa
            [5U];
        __VdlySet__debugValue__v5 = 1U;
    }
    if (vlSelfRef.__PVT__we[6U]) {
        __VdlyVal__debugValue__v6 = vlSelfRef.__PVT__wv
            [6U];
        __VdlyDim0__debugValue__v6 = vlSelfRef.__PVT__wa
            [6U];
        __VdlySet__debugValue__v6 = 1U;
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
    if (__VdlySet__debugValue__v3) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v3] 
            = __VdlyVal__debugValue__v3;
    }
    if (__VdlySet__debugValue__v4) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v4] 
            = __VdlyVal__debugValue__v4;
    }
    if (__VdlySet__debugValue__v5) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v5] 
            = __VdlyVal__debugValue__v5;
    }
    if (__VdlySet__debugValue__v6) {
        vlSelfRef.debugValue[__VdlyDim0__debugValue__v6] 
            = __VdlyVal__debugValue__v6;
    }
}
