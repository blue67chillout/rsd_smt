// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_IO_UnitIF.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_IO_UnitIF___ctor_var_reset(VSMT_RTL_Testbench_IO_UnitIF* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                  VSMT_RTL_Testbench_IO_UnitIF___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rstStart = VL_RAND_RESET_I(1);
    vlSelf->__PVT__serialWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__serialWriteDataOut = VL_RAND_RESET_I(32);
    vlSelf->__PVT__ioWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__ioWriteDataIn = VL_RAND_RESET_I(32);
    vlSelf->__PVT__ioWriteAddrIn = VL_RAND_RESET_I(22);
    vlSelf->__PVT__ioReadDataOut = VL_RAND_RESET_I(32);
    vlSelf->__PVT__ioReadAddrIn = VL_RAND_RESET_I(22);
}
