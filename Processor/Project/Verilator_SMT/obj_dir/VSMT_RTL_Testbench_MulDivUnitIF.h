// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_MULDIVUNITIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_MULDIVUNITIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_MulDivUnitIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*0:0*/ __PVT__dummy;
    CData/*0:0*/ __PVT__stall;
    VlUnpacked<IData/*31:0*/, 1> __PVT__dataInA;
    VlUnpacked<IData/*31:0*/, 1> __PVT__dataInB;
    VlUnpacked<IData/*31:0*/, 1> __PVT__mulDataOut;
    VlUnpacked<CData/*0:0*/, 1> __PVT__mulGetUpper;
    VlUnpacked<CData/*1:0*/, 1> __PVT__mulCode;
    VlUnpacked<IData/*31:0*/, 1> __PVT__divDataOut;
    VlUnpacked<CData/*1:0*/, 1> __PVT__divCode;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divReq;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divReserved;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divFinished;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divBusy;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divFree;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divAcquire;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divRelease;
    VlUnpacked<CData/*5:0*/, 1> __PVT__acquireActiveListPtr;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_MulDivUnitIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_MulDivUnitIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_MulDivUnitIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_MulDivUnitIF* obj);

#endif  // guard
