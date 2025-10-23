// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DCACHEIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DCACHEIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DCacheIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    CData/*0:0*/ __PVT__memInSel;
    CData/*0:0*/ __PVT__memValid;
    CData/*0:0*/ __PVT__memWE;
    CData/*0:0*/ __PVT__dcFlushReqAck;
    CData/*0:0*/ __PVT__dcFlushComplete;
    IData/*21:0*/ __PVT__memAddr;
    QData/*63:0*/ __PVT__memData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__tagArrayWE;
    VlUnpacked<CData/*0:0*/, 2> __PVT__tagArrayWriteWay;
    VlUnpacked<CData/*7:0*/, 2> __PVT__tagArrayIndexIn;
    VlUnpacked<SData/*10:0*/, 2> __PVT__tagArrayDataIn;
    VlUnpacked<CData/*0:0*/, 2> __PVT__tagArrayValidIn;
    VlUnpacked<VlUnpacked<SData/*10:0*/, 2>, 2> __PVT__tagArrayDataOut;
    VlUnpacked<VlUnpacked<CData/*0:0*/, 2>, 2> __PVT__tagArrayValidOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dataArrayWE;
    VlUnpacked<CData/*7:0*/, 2> __PVT__dataArrayIndexIn;
    VlUnpacked<QData/*63:0*/, 2> __PVT__dataArrayDataIn;
    VlUnpacked<QData/*63:0*/, 2> __PVT__dataArrayDataOut;
    VlUnpacked<CData/*7:0*/, 2> __PVT__dataArrayByteWE_In;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dataArrayWriteWay;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dataArrayReadWay;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dataArrayDoesReadEvictedWay;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dataArrayDirtyIn;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dataArrayDirtyOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__replArrayWE;
    VlUnpacked<CData/*7:0*/, 2> __PVT__replArrayIndexIn;
    VlUnpacked<CData/*0:0*/, 2> __PVT__replArrayDataIn;
    VlUnpacked<CData/*0:0*/, 2> __PVT__replArrayDataOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__lsuCacheReq;
    VlUnpacked<CData/*0:0*/, 2> __PVT__lsuCacheGrt;
    VlUnpacked<VlWide<4>/*98:0*/, 2> __PVT__lsuMuxIn;
    VlUnpacked<VlWide<3>/*92:0*/, 2> __PVT__lsuMuxTagOut;
    VlUnpacked<VlWide<3>/*65:0*/, 2> __PVT__lsuMuxDataOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__mshrCacheReq;
    VlUnpacked<CData/*0:0*/, 2> __PVT__mshrCacheGrt;
    VlUnpacked<VlWide<4>/*98:0*/, 2> __PVT__mshrCacheMuxIn;
    VlUnpacked<VlWide<3>/*92:0*/, 2> __PVT__mshrCacheMuxTagOut;
    VlUnpacked<VlWide<3>/*65:0*/, 2> __PVT__mshrCacheMuxDataOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__cacheArrayInGrant;
    VlUnpacked<CData/*1:0*/, 2> __PVT__cacheArrayInSel;
    VlUnpacked<CData/*0:0*/, 4> __PVT__cacheArrayOutSel;
    VlUnpacked<CData/*0:0*/, 2> __PVT__mshrMemReq;
    VlUnpacked<CData/*0:0*/, 2> __PVT__mshrMemGrt;
    VlUnpacked<VlWide<3>/*86:0*/, 2> __PVT__mshrMemMuxIn;
    VlUnpacked<CData/*3:0*/, 2> __PVT__mshrMemMuxOut;
    VlUnpacked<CData/*0:0*/, 2> __PVT__initMSHR;
    VlUnpacked<IData/*21:0*/, 2> __PVT__initMSHR_Addr;
    VlUnpacked<CData/*5:0*/, 2> __PVT__initMSHR_ActiveListPtr;
    VlUnpacked<CData/*0:0*/, 2> __PVT__mshrValid;
    VlUnpacked<IData/*21:0*/, 2> __PVT__mshrAddr;
    VlUnpacked<CData/*4:0*/, 2> __PVT__mshrPhase;
    VlUnpacked<QData/*63:0*/, 2> __PVT__mshrData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__mshrCanBeInvalidDirect;
    VlUnpacked<CData/*0:0*/, 2> __PVT__isAllocatedByStore;
    VlUnpacked<CData/*0:0*/, 2> __PVT__isUncachable;
    VlUnpacked<CData/*0:0*/, 2> __PVT__lsuCacheGrtReg;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DCacheIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DCacheIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DCacheIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_DCacheIF* obj);

#endif  // guard
