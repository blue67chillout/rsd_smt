// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_BYPASSNETWORKIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_BYPASSNETWORKIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_BypassNetworkIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    VlUnpacked<CData/*6:0*/, 2> __PVT__intPhySrcRegNumA;
    VlUnpacked<CData/*6:0*/, 2> __PVT__intPhySrcRegNumB;
    VlUnpacked<CData/*6:0*/, 2> __PVT__intPhyDstRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intReadRegA;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intReadRegB;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intWriteReg;
    VlUnpacked<IData/*20:0*/, 2> __PVT__intCtrlOut;
    VlUnpacked<IData/*20:0*/, 2> __PVT__intCtrlIn;
    VlUnpacked<QData/*32:0*/, 2> __PVT__intSrcRegDataOutA;
    VlUnpacked<QData/*32:0*/, 2> __PVT__intSrcRegDataOutB;
    VlUnpacked<QData/*32:0*/, 2> __PVT__intDstRegDataOut;
    VlUnpacked<CData/*6:0*/, 1> __PVT__complexPhySrcRegNumA;
    VlUnpacked<CData/*6:0*/, 1> __PVT__complexPhySrcRegNumB;
    VlUnpacked<CData/*6:0*/, 1> __PVT__complexPhyDstRegNum;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexReadRegA;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexReadRegB;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexWriteReg;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexSrcRegDataA;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexSrcRegDataB;
    VlUnpacked<IData/*20:0*/, 1> __PVT__complexCtrlOut;
    VlUnpacked<IData/*20:0*/, 1> __PVT__complexCtrlIn;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexSrcRegDataOutA;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexSrcRegDataOutB;
    VlUnpacked<QData/*32:0*/, 1> __PVT__complexDstRegDataOut;
    VlUnpacked<CData/*6:0*/, 2> __PVT__memPhySrcRegNumA;
    VlUnpacked<CData/*6:0*/, 2> __PVT__memPhySrcRegNumB;
    VlUnpacked<CData/*6:0*/, 2> __PVT__memPhyDstRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memReadRegA;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memReadRegB;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memWriteReg;
    VlUnpacked<IData/*20:0*/, 2> __PVT__memCtrlOut;
    VlUnpacked<IData/*20:0*/, 2> __PVT__memCtrlIn;
    VlUnpacked<QData/*32:0*/, 2> __PVT__memSrcRegDataOutA;
    VlUnpacked<QData/*32:0*/, 2> __PVT__memSrcRegDataOutB;
    VlUnpacked<QData/*32:0*/, 2> __PVT__memDstRegDataOut;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpPhySrcRegNumA;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpPhySrcRegNumB;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpPhySrcRegNumC;
    VlUnpacked<CData/*6:0*/, 1> __PVT__fpPhyDstRegNum;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpReadRegA;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpReadRegB;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpReadRegC;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpWriteReg;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataA;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataB;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataC;
    VlUnpacked<IData/*20:0*/, 1> __PVT__fpCtrlOut;
    VlUnpacked<IData/*20:0*/, 1> __PVT__fpCtrlIn;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataOutA;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataOutB;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpSrcRegDataOutC;
    VlUnpacked<QData/*32:0*/, 1> __PVT__fpDstRegDataOut;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_BypassNetworkIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_BypassNetworkIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_BypassNetworkIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_BypassNetworkIF* obj);

#endif  // guard
