// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_INITIALIZEDBLOCKRAM_FORNARROWREQUEST__PI20_H_
#define VERILATED_VSMT_RTL_TESTBENCH_INITIALIZEDBLOCKRAM_FORNARROWREQUEST__PI20_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__we,0,0);
    CData/*0:0*/ __PVT__hexFileRAOffset;
    CData/*0:0*/ __PVT__hexFileWAOffset;
    VL_IN(__PVT__wa,21,0);
    VL_IN(__PVT__ra,21,0);
    IData/*21:0*/ __PVT__raReg;
    IData/*20:0*/ __PVT__hexFileRA;
    VlWide<4>/*127:0*/ __PVT__hexFileRV;
    IData/*20:0*/ __PVT__hexFileWA;
    VlWide<4>/*127:0*/ __PVT__hexFileWV;
    VlWide<4>/*127:0*/ __PVT__tmpWriteEntry;
    VlWide<4>/*127:0*/ __PVT__dummyRV;
    VL_IN64(__PVT__wv,63,0);
    VL_OUT64(__PVT__rv,63,0);
    VlUnpacked<VlWide<4>/*127:0*/, 2097152> array;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
