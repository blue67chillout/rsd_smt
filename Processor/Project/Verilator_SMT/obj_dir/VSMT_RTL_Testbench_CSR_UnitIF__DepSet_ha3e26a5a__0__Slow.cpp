// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_CSR_UnitIF.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_CSR_UnitIF___ctor_var_reset(VSMT_RTL_Testbench_CSR_UnitIF* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_CSR_UnitIF___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rstStart = VL_RAND_RESET_I(1);
    vlSelf->__PVT__reqExternalInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__externalInterruptCode = VL_RAND_RESET_I(5);
    vlSelf->__PVT__tid = VL_RAND_RESET_I(1);
    vlSelf->__PVT__csrWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__csrReadOut = VL_RAND_RESET_I(32);
    vlSelf->__PVT__csrWriteIn = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(352, vlSelf->__PVT__csrWholeOut);
    vlSelf->__PVT__triggerExcpt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__excptCauseAddr = VL_RAND_RESET_I(20);
    vlSelf->__PVT__excptTargetAddr = VL_RAND_RESET_I(32);
    vlSelf->__PVT__triggerInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__interruptCode = VL_RAND_RESET_I(5);
    vlSelf->__PVT__reqTimerInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__externalInterruptCodeInCSR = VL_RAND_RESET_I(5);
    vlSelf->__PVT__commitNum = VL_RAND_RESET_I(2);
    vlSelf->__PVT__fflags = VL_RAND_RESET_I(5);
    vlSelf->__PVT__frm = VL_RAND_RESET_I(3);
    vlSelf->__PVT__fflagsWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fflagsData = VL_RAND_RESET_I(5);
}
