// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_ACTIVELISTIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_ACTIVELISTIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_ActiveListIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*1:0*/ __PVT__popHeadNum;
    CData/*1:0*/ __PVT__popTailNum;
    CData/*5:0*/ __PVT__detectedFlushRangeTailPtr;
    CData/*5:0*/ __PVT__exceptionOpPtr;
    VlUnpacked<CData/*0:0*/, 2> __PVT__pushTail;
    VlUnpacked<QData/*62:0*/, 2> __PVT__pushedTailData;
    VlUnpacked<CData/*5:0*/, 2> __PVT__pushedTailPtr;
    VlUnpacked<QData/*62:0*/, 2> __PVT__readData;
    VlUnpacked<CData/*3:0*/, 2> __PVT__headExecState;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intWrite;
    VlUnpacked<VlWide<3>/*71:0*/, 2> __PVT__intWriteData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexWrite;
    VlUnpacked<VlWide<3>/*71:0*/, 1> __PVT__complexWriteData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memWrite;
    VlUnpacked<VlWide<3>/*71:0*/, 2> __PVT__memWriteData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpWrite;
    VlUnpacked<VlWide<3>/*71:0*/, 1> __PVT__fpWriteData;
    VlUnpacked<CData/*4:0*/, 1> __PVT__fpFFlagsData;
    VlUnpacked<CData/*4:0*/, 2> __PVT__fflagsData;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_ActiveListIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_ActiveListIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_ActiveListIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_ActiveListIF* obj);

#endif  // guard
