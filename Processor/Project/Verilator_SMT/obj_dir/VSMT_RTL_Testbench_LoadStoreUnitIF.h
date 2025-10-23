// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_LOADSTOREUNITIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_LOADSTOREUNITIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_LoadStoreUnitIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    // Anonymous structures to workaround compiler member-count bugs
    struct {
        VL_IN8(__PVT__clk,0,0);
        VL_IN8(__PVT__rst,0,0);
        VL_IN8(__PVT__rstStart,0,0);
        CData/*0:0*/ __PVT__allocatable;
        CData/*0:0*/ __PVT__commitStore;
        CData/*1:0*/ __PVT__commitStoreNum;
        CData/*0:0*/ __PVT__releaseLoadQueue;
        CData/*1:0*/ __PVT__releaseLoadQueueEntryNum;
        CData/*0:0*/ __PVT__releaseStoreQueueHead;
        CData/*1:0*/ __PVT__releaseStoreQueueHeadEntryNum;
        CData/*3:0*/ __PVT__retiredStoreQueuePtr;
        CData/*0:0*/ __PVT__retiredStoreCondEnabled;
        CData/*0:0*/ __PVT__retiredStoreWordWE;
        CData/*3:0*/ __PVT__retiredStoreByteWE;
        CData/*0:0*/ __PVT__busyInRecovery;
        CData/*0:0*/ __PVT__dcWriteReq;
        CData/*0:0*/ __PVT__dcWriteBusy;
        CData/*0:0*/ __PVT__dcWriteHit;
        CData/*7:0*/ __PVT__dcWriteByteWE;
        CData/*0:0*/ __PVT__dcWriteUncachable;
        IData/*31:0*/ __PVT__retiredStoreData;
        IData/*19:0*/ __PVT__retiredStoreLSQ_BlockAddr;
        IData/*21:0*/ __PVT__dcWriteAddr;
        QData/*63:0*/ __PVT__dcWriteData;
        VlUnpacked<CData/*0:0*/, 2> __PVT__allocateLoadQueue;
        VlUnpacked<CData/*0:0*/, 2> __PVT__allocateStoreQueue;
        VlUnpacked<CData/*3:0*/, 2> __PVT__allocatedLoadQueuePtr;
        VlUnpacked<CData/*3:0*/, 2> __PVT__allocatedStoreQueuePtr;
        VlUnpacked<CData/*0:0*/, 1> __PVT__executeLoad;
        VlUnpacked<CData/*0:0*/, 1> __PVT__executedLoadRegValid;
        VlUnpacked<IData/*21:0*/, 1> __PVT__executedLoadAddr;
        VlUnpacked<CData/*1:0*/, 1> __PVT__executedLoadMemMapType;
        VlUnpacked<IData/*31:0*/, 1> __PVT__executedLoadData;
        VlUnpacked<IData/*19:0*/, 1> __PVT__executedLoadPC;
        VlUnpacked<VlWide<4>/*127:0*/, 1> __PVT__executedLoadVectorData;
        VlUnpacked<CData/*2:0*/, 1> __PVT__executedLoadMemAccessMode;
        VlUnpacked<CData/*3:0*/, 1> __PVT__executedStoreQueuePtrByLoad;
        VlUnpacked<CData/*3:0*/, 1> __PVT__executedLoadQueuePtrByLoad;
        VlUnpacked<CData/*0:0*/, 1> __PVT__executeStore;
        VlUnpacked<CData/*0:0*/, 1> __PVT__executedStoreCondEnabled;
        VlUnpacked<CData/*0:0*/, 1> __PVT__executedStoreRegValid;
        VlUnpacked<IData/*21:0*/, 1> __PVT__executedStoreAddr;
        VlUnpacked<IData/*31:0*/, 1> __PVT__executedStoreData;
        VlUnpacked<VlWide<4>/*127:0*/, 1> __PVT__executedStoreVectorData;
        VlUnpacked<CData/*2:0*/, 1> __PVT__executedStoreMemAccessMode;
        VlUnpacked<CData/*3:0*/, 1> __PVT__executedLoadQueuePtrByStore;
        VlUnpacked<CData/*3:0*/, 1> __PVT__executedStoreQueuePtrByStore;
        VlUnpacked<CData/*0:0*/, 1> __PVT__storeLoadForwarded;
        VlUnpacked<IData/*31:0*/, 1> __PVT__forwardedLoadData;
        VlUnpacked<CData/*0:0*/, 1> __PVT__forwardMiss;
        VlUnpacked<CData/*0:0*/, 1> __PVT__dcReadReq;
        VlUnpacked<CData/*0:0*/, 1> __PVT__dcReadBusy;
        VlUnpacked<CData/*0:0*/, 1> __PVT__dcReadHit;
        VlUnpacked<IData/*21:0*/, 1> __PVT__dcReadAddr;
        VlUnpacked<QData/*63:0*/, 1> __PVT__dcReadData;
        VlUnpacked<CData/*0:0*/, 1> __PVT__dcReadUncachable;
        VlUnpacked<CData/*5:0*/, 1> __PVT__dcReadActiveListPtr;
        VlUnpacked<CData/*0:0*/, 1> __PVT__loadHasAllocatedMSHR;
        VlUnpacked<CData/*0:0*/, 1> __PVT__loadMSHRID;
        VlUnpacked<CData/*0:0*/, 1> __PVT__storeHasAllocatedMSHR;
        VlUnpacked<CData/*0:0*/, 1> __PVT__storeMSHRID;
        VlUnpacked<CData/*0:0*/, 1> __PVT__mshrAddrHit;
        VlUnpacked<CData/*0:0*/, 1> __PVT__mshrAddrHitMSHRID;
        VlUnpacked<CData/*0:0*/, 1> __PVT__mshrReadHit;
    };
    struct {
        VlUnpacked<QData/*63:0*/, 1> __PVT__mshrReadData;
        VlUnpacked<CData/*0:0*/, 2> __PVT__makeMSHRCanBeInvalidDirect;
        VlUnpacked<CData/*0:0*/, 2> __PVT__mshrValid;
        VlUnpacked<CData/*4:0*/, 2> __PVT__mshrPhase;
        VlUnpacked<CData/*0:0*/, 1> __PVT__conflict;
        VlUnpacked<CData/*0:0*/, 1> __PVT__memAccessOrderViolation;
        VlUnpacked<IData/*19:0*/, 1> __PVT__conflictLoadPC;
    };

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_LoadStoreUnitIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_LoadStoreUnitIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_LoadStoreUnitIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_LoadStoreUnitIF* obj);

#endif  // guard
