// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_WAKEUPSELECTIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_WAKEUPSELECTIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_WakeupSelectIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    SData/*15:0*/ __PVT__opReady;
    VlUnpacked<CData/*0:0*/, 2> __PVT__write;
    VlUnpacked<CData/*3:0*/, 2> __PVT__writePtr;
    VlUnpacked<QData/*38:0*/, 2> __PVT__writeSrcTag;
    VlUnpacked<CData/*7:0*/, 2> __PVT__writeDstTag;
    VlUnpacked<CData/*0:0*/, 5> __PVT__wakeup;
    VlUnpacked<CData/*3:0*/, 6> __PVT__wakeupPtr;
    VlUnpacked<SData/*15:0*/, 6> __PVT__wakeupVector;
    VlUnpacked<CData/*7:0*/, 5> __PVT__wakeupDstTag;
    VlUnpacked<CData/*0:0*/, 6> __PVT__selected;
    VlUnpacked<CData/*3:0*/, 6> __PVT__selectedPtr;
    VlUnpacked<SData/*15:0*/, 6> __PVT__selectedVector;
    VlUnpacked<CData/*0:0*/, 6> __PVT__releaseEntry;
    VlUnpacked<CData/*3:0*/, 6> __PVT__releasePtr;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memDependencyPred;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dispatchStore;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dispatchLoad;
    VlUnpacked<CData/*0:0*/, 16> __PVT__intIssueReq;
    VlUnpacked<CData/*0:0*/, 16> __PVT__complexIssueReq;
    VlUnpacked<CData/*0:0*/, 16> __PVT__loadIssueReq;
    VlUnpacked<CData/*0:0*/, 16> __PVT__storeIssueReq;
    VlUnpacked<CData/*0:0*/, 16> __PVT__fpIssueReq;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_WakeupSelectIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_WakeupSelectIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_WakeupSelectIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_WakeupSelectIF* obj);

#endif  // guard
