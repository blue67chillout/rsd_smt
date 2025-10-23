// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_REGISTERFILEIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_REGISTERFILEIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RegisterFileIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    VlUnpacked<CData/*0:0*/, 2> __PVT__intSrcTidA;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intSrcTidB;
    VlUnpacked<CData/*6:0*/, 2> __PVT__intSrcRegNumA;
    VlUnpacked<CData/*6:0*/, 2> __PVT__intSrcRegNumB;
    VlUnpacked<QData/*32:0*/, 2> __PVT__intSrcRegDataA;
    VlUnpacked<QData/*32:0*/, 2> __PVT__intSrcRegDataB;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intDstTid;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intDstRegWE;
    VlUnpacked<CData/*6:0*/, 2> __PVT__intDstRegNum;
    VlUnpacked<QData/*32:0*/, 2> __PVT__intDstRegData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexSrcTidA;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexSrcTidB;
    VlUnpacked<CData/*6:0*/, 1> __PVT__complexSrcRegNumA;
    VlUnpacked<CData/*6:0*/, 1> __PVT__complexSrcRegNumB;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexSrcRegDataA;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexSrcRegDataB;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexDstTid;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexDstRegWE;
    VlUnpacked<CData/*6:0*/, 1> __PVT__complexDstRegNum;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexDstRegData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memSrcTidA;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memSrcTidB;
    VlUnpacked<CData/*6:0*/, 2> __PVT__memSrcRegNumA;
    VlUnpacked<CData/*6:0*/, 2> __PVT__memSrcRegNumB;
    VlUnpacked<QData/*32:0*/, 2> __PVT__memSrcRegDataA;
    VlUnpacked<QData/*32:0*/, 2> __PVT__memSrcRegDataB;
    VlUnpacked<CData/*0:0*/, 1> __PVT__memDstTid;
    VlUnpacked<CData/*0:0*/, 1> __PVT__memDstRegWE;
    VlUnpacked<CData/*6:0*/, 1> __PVT__memDstRegNum;
    VlUnpacked<QData/*32:0*/, 1> __PVT__memDstRegData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpSrcTidA;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpSrcTidB;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpSrcTidC;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpSrcRegNumA;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpSrcRegNumB;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpSrcRegNumC;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataA;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataB;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataC;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpDstTid;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpDstRegWE;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpDstRegNum;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpDstRegData;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RegisterFileIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RegisterFileIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RegisterFileIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_RegisterFileIF* obj);

#endif  // guard
