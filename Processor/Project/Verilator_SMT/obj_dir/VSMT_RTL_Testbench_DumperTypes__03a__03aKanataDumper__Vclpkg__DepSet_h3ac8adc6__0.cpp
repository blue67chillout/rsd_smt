// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg.h"
#include "VSMT_RTL_Testbench__Syms.h"

VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper(VSMT_RTL_Testbench__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::new\n"); );
    // Init
    _ctor_var_reset(vlSymsp);
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::_ctor_var_reset(VSMT_RTL_Testbench__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::_ctor_var_reset\n"); );
    // Body
    (void)vlSymsp;  // Prevent unused variable warning
    __PVT__m_file = 0;
    __PVT__m_cycle = VL_RAND_RESET_I(32);
    __PVT__m_retireID = VL_RAND_RESET_I(32);
}
