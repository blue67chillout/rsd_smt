// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_CACHESYSTEMIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_CACHESYSTEMIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_CacheSystemIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*3:0*/ __PVT__icMemAccessReqAck;
    CData/*3:0*/ __PVT__dcMemAccessReqAck;
    CData/*0:0*/ __PVT__icFlushComplete;
    CData/*0:0*/ __PVT__icFlushReq;
    CData/*0:0*/ __PVT__dcFlushReq;
    CData/*0:0*/ __PVT__flushComplete;
    IData/*22:0*/ __PVT__icMemAccessReq;
    VlWide<3>/*66:0*/ __PVT__icMemAccessResult;
    VlWide<3>/*87:0*/ __PVT__dcMemAccessReq;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_CacheSystemIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_CacheSystemIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_CacheSystemIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_CacheSystemIF* obj);

#endif  // guard
