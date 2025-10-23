// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___ctor_var_reset(VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__we = VL_RAND_RESET_I(1);
    vlSelf->__PVT__wa = VL_RAND_RESET_I(22);
    vlSelf->__PVT__wv = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__ra = VL_RAND_RESET_I(22);
    vlSelf->__PVT__rv = VL_RAND_RESET_Q(64);
    for (int __Vi0 = 0; __Vi0 < 2097152; ++__Vi0) {
        VL_RAND_RESET_W(128, vlSelf->array[__Vi0]);
    }
    vlSelf->__PVT__raReg = VL_RAND_RESET_I(22);
    vlSelf->__PVT__hexFileRA = VL_RAND_RESET_I(21);
    VL_RAND_RESET_W(128, vlSelf->__PVT__hexFileRV);
    vlSelf->__PVT__hexFileRAOffset = VL_RAND_RESET_I(1);
    vlSelf->__PVT__hexFileWA = VL_RAND_RESET_I(21);
    VL_RAND_RESET_W(128, vlSelf->__PVT__hexFileWV);
    vlSelf->__PVT__hexFileWAOffset = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(128, vlSelf->__PVT__tmpWriteEntry);
    VL_RAND_RESET_W(128, vlSelf->__PVT__dummyRV);
}
