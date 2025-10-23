// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___eval_initial__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram(VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20___eval_initial__TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<4>/*127:0*/ __Vtemp_1;
    // Body
    __Vtemp_1[0U] = 0x2e686578U;
    __Vtemp_1[1U] = 0x6772616dU;
    __Vtemp_1[2U] = 0x5f70726fU;
    __Vtemp_1[3U] = 0x74657374U;
    VL_READMEM_N(true, 128, 2097152, 0, VL_CVT_PACK_STR_NW(4, __Vtemp_1)
                 ,  &(vlSelfRef.array), 0, ~0ULL);
}
