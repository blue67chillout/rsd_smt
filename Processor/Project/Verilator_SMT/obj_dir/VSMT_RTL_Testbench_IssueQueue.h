// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_ISSUEQUEUE_H_
#define VERILATED_VSMT_RTL_TESTBENCH_ISSUEQUEUE_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi10;
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11;
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi12;
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi9;
class VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_IssueQueue final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10* issueQueueFreeList;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi9* intPayloadRAM;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi10* complexPayloadRAM;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11* memPayloadRAM;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi12* fpPayloadRAM;

    // DESIGN SPECIFIC STATE
    CData/*0:0*/ __PVT__freeListReset;
    CData/*0:0*/ __PVT__freeListResetCycleCount;
    CData/*0:0*/ __PVT__issueQueueReturnIndex;
    CData/*2:0*/ __PVT__issueQueueReturnIndexCycleCount;
    CData/*3:0*/ __PVT__returnIndexOffset;
    CData/*0:0*/ __Vcellinp__issueQueueFreeList__rst;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__0__Vfuncout;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__1__Vfuncout;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__1__detectRange;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__1__headPtr;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__1__tailPtr;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__1__flushAllInsns;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__1__opPtr;
    SData/*15:0*/ __PVT__flush;
    SData/*15:0*/ __PVT__prevFlushAtRecovery;
    IData/*31:0*/ __PVT__unnamedblk7__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk8__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk10__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk11__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk12__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk13__DOT__i;
    VlUnpacked<CData/*0:0*/, 8> __PVT__releaseEntry;
    VlUnpacked<CData/*3:0*/, 8> __PVT__releasePtr;
    VlUnpacked<CData/*5:0*/, 16> __PVT__alPtrReg;
    VlUnpacked<CData/*3:0*/, 2> __PVT__writePtr;
    VlUnpacked<CData/*3:0*/, 6> __PVT__selectedPtr;
    VlUnpacked<CData/*3:0*/, 2> __Vcellout__issueQueueFreeList__poppedData;
    VlUnpacked<CData/*0:0*/, 2> __Vcellinp__issueQueueFreeList__pop;
    VlUnpacked<CData/*3:0*/, 2> __PVT__intIssuePtr;
    VlUnpacked<CData/*3:0*/, 2> __PVT__memIssuePtr;
    VlUnpacked<VlWide<5>/*138:0*/, 2> __PVT__intIssuedData;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __PVT__memIssuedData;
    VlUnpacked<CData/*3:0*/, 1> __PVT__complexIssuePtr;
    VlUnpacked<VlWide<3>/*81:0*/, 1> __PVT__complexIssuedData;
    VlUnpacked<CData/*3:0*/, 1> __PVT__fpIssuePtr;
    VlUnpacked<VlWide<3>/*92:0*/, 1> __PVT__fpIssuedData;
    VlUnpacked<VlWide<5>/*138:0*/, 2> __Vcellout__intPayloadRAM__rv;
    VlUnpacked<VlWide<5>/*138:0*/, 2> __Vcellinp__intPayloadRAM__wv;
    VlUnpacked<CData/*3:0*/, 2> __Vcellinp__intPayloadRAM__wa;
    VlUnpacked<CData/*0:0*/, 2> __Vcellinp__intPayloadRAM__we;
    VlUnpacked<VlWide<3>/*81:0*/, 1> __Vcellout__complexPayloadRAM__rv;
    VlUnpacked<VlWide<3>/*81:0*/, 2> __Vcellinp__complexPayloadRAM__wv;
    VlUnpacked<CData/*3:0*/, 2> __Vcellinp__complexPayloadRAM__wa;
    VlUnpacked<CData/*0:0*/, 2> __Vcellinp__complexPayloadRAM__we;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __Vcellout__memPayloadRAM__rv;
    VlUnpacked<VlWide<4>/*124:0*/, 2> __Vcellinp__memPayloadRAM__wv;
    VlUnpacked<CData/*3:0*/, 2> __Vcellinp__memPayloadRAM__wa;
    VlUnpacked<CData/*0:0*/, 2> __Vcellinp__memPayloadRAM__we;
    VlUnpacked<VlWide<3>/*92:0*/, 1> __Vcellout__fpPayloadRAM__rv;
    VlUnpacked<VlWide<3>/*92:0*/, 2> __Vcellinp__fpPayloadRAM__wv;
    VlUnpacked<CData/*3:0*/, 2> __Vcellinp__fpPayloadRAM__wa;
    VlUnpacked<CData/*0:0*/, 2> __Vcellinp__fpPayloadRAM__we;
    VlUnpacked<SData/*11:0*/, 16> __PVT__opId;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_IssueQueue(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_IssueQueue();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_IssueQueue);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
