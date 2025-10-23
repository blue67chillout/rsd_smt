// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench__Syms.h"
#include "VSMT_RTL_Testbench___024root.h"

void VSMT_RTL_Testbench___024root___ctor_var_reset(VSMT_RTL_Testbench___024root* vlSelf);

VSMT_RTL_Testbench___024root::VSMT_RTL_Testbench___024root(VSMT_RTL_Testbench__Syms* symsp, const char* v__name)
    : VerilatedModule{v__name}
    , __VdlySched{*symsp->_vm_contextp__}
    , vlSymsp{symsp}
 {
    // Reset structure values
    VSMT_RTL_Testbench___024root___ctor_var_reset(this);
}

void VSMT_RTL_Testbench___024root::__Vconfigure(bool first) {
    (void)first;  // Prevent unused variable warning
}

VSMT_RTL_Testbench___024root::~VSMT_RTL_Testbench___024root() {
}
