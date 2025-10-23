// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_FPDIVSQRTUNITIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_FPDIVSQRTUNITIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_FPDivSqrtUnitIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*0:0*/ __PVT__stall;
    VlUnpacked<IData/*31:0*/, 1> __PVT__dataInA;
    VlUnpacked<IData/*31:0*/, 1> __PVT__dataInB;
    VlUnpacked<IData/*31:0*/, 1> __PVT__DataOut;
    VlUnpacked<CData/*4:0*/, 1> __PVT__FFlagsOut;
    VlUnpacked<CData/*0:0*/, 1> __PVT__is_divide;
    VlUnpacked<CData/*2:0*/, 1> __PVT__rm;
    VlUnpacked<CData/*0:0*/, 1> __PVT__Req;
    VlUnpacked<CData/*0:0*/, 1> __PVT__Reserved;
    VlUnpacked<CData/*0:0*/, 1> __PVT__Finished;
    VlUnpacked<CData/*0:0*/, 1> __PVT__Busy;
    VlUnpacked<CData/*0:0*/, 1> __PVT__Free;
    VlUnpacked<CData/*0:0*/, 1> __PVT__Acquire;
    VlUnpacked<CData/*0:0*/, 1> __PVT__Release;
    VlUnpacked<CData/*5:0*/, 1> __PVT__acquireActiveListPtr;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_FPDivSqrtUnitIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_FPDivSqrtUnitIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_FPDivSqrtUnitIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_FPDivSqrtUnitIF* obj);

#endif  // guard
