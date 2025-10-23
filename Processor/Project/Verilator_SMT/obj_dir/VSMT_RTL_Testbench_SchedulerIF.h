// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_SCHEDULERIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_SCHEDULERIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_SchedulerIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    CData/*0:0*/ __PVT__replay;
    CData/*0:0*/ __PVT__stall;
    VlUnpacked<CData/*0:0*/, 2> __PVT__allocate;
    VlUnpacked<CData/*3:0*/, 2> __PVT__allocatedPtr;
    VlUnpacked<CData/*0:0*/, 2> __PVT__write;
    VlUnpacked<CData/*0:0*/, 2> __PVT__writeTid;
    VlUnpacked<CData/*3:0*/, 2> __PVT__writePtr;
    VlUnpacked<CData/*5:0*/, 2> __PVT__writeAL_Ptr;
    VlUnpacked<VlWide<5>/*138:0*/, 2> __PVT__intWriteData;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __PVT__memWriteData;
    VlUnpacked<QData/*48:0*/, 2> __PVT__writeSchedulerData;
    VlUnpacked<CData/*0:0*/, 6> __PVT__selected;
    VlUnpacked<CData/*3:0*/, 6> __PVT__selectedPtr;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intIssue;
    VlUnpacked<CData/*3:0*/, 2> __PVT__intIssuePtr;
    VlUnpacked<VlWide<5>/*138:0*/, 2> __PVT__intIssuedData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memIssue;
    VlUnpacked<CData/*3:0*/, 2> __PVT__memIssuePtr;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __PVT__memIssuedData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intRecordEntry;
    VlUnpacked<CData/*0:0*/, 2> __PVT__intReplayEntry;
    VlUnpacked<VlWide<5>/*138:0*/, 2> __PVT__intRecordData;
    VlUnpacked<VlWide<5>/*138:0*/, 2> __PVT__intReplayData;
    VlUnpacked<VlWide<3>/*81:0*/, 2> __PVT__complexWriteData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexIssue;
    VlUnpacked<CData/*3:0*/, 1> __PVT__complexIssuePtr;
    VlUnpacked<VlWide<3>/*81:0*/, 1> __PVT__complexIssuedData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexRecordEntry;
    VlUnpacked<CData/*0:0*/, 1> __PVT__complexReplayEntry;
    VlUnpacked<VlWide<3>/*81:0*/, 1> __PVT__complexRecordData;
    VlUnpacked<VlWide<3>/*81:0*/, 1> __PVT__complexReplayData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__divIsIssued;
    VlUnpacked<VlWide<3>/*92:0*/, 2> __PVT__fpWriteData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpIssue;
    VlUnpacked<CData/*3:0*/, 1> __PVT__fpIssuePtr;
    VlUnpacked<VlWide<3>/*92:0*/, 1> __PVT__fpIssuedData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpRecordEntry;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpReplayEntry;
    VlUnpacked<VlWide<3>/*92:0*/, 1> __PVT__fpRecordData;
    VlUnpacked<VlWide<3>/*92:0*/, 1> __PVT__fpReplayData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__fpDivSqrtIsIssued;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memReleaseEntry;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memRecordEntry;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memReplayEntry;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __PVT__memRecordData;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __PVT__memReplayData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__allocated;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memDependencyPred;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_SchedulerIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_SchedulerIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_SchedulerIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_SchedulerIF* obj);

#endif  // guard
