// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH___024UNIT_H_
#define VERILATED_VSMT_RTL_TESTBENCH___024UNIT_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench___024unit final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VlWide<3>/*75:0*/ __Vlvbound_h584fbd87__0;
    VlWide<3>/*75:0*/ __Vlvbound_h6d3762fc__0;
    VlWide<3>/*75:0*/ __Vlvbound_h3f3210e9__0;
    VlWide<3>/*75:0*/ __Vlvbound_h8cccd569__0;
    VlWide<3>/*75:0*/ __Vlvbound_hd9e74018__0;
    VlWide<3>/*75:0*/ __Vlvbound_ha3030f99__0;
    VlWide<3>/*75:0*/ __Vlvbound_h3a7cc217__0;
    VlWide<3>/*75:0*/ __Vlvbound_hab51bb0b__0;
    VlWide<3>/*75:0*/ __Vlvbound_hbda3652c__0;
    VlWide<3>/*75:0*/ __Vlvbound_h519fd50d__0;
    VlWide<3>/*75:0*/ __Vlvbound_hc3737b4c__0;
    VlWide<3>/*75:0*/ __Vlvbound_h2385d58f__0;
    VlWide<3>/*75:0*/ __Vlvbound_hd5dab3e3__0;
    VlWide<3>/*75:0*/ __Vlvbound_hacbf8ed4__0;
    VlWide<3>/*75:0*/ __Vlvbound_h862f4532__0;
    VlWide<3>/*75:0*/ __Vlvbound_hdb6811b6__0;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench___024unit(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench___024unit();
    VL_UNCOPYABLE(VSMT_RTL_Testbench___024unit);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
