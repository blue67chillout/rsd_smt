// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_READYBITTABLE__S3_R7_E80_H_
#define VERILATED_VSMT_RTL_TESTBENCH_READYBITTABLE__S3_R7_E80_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80 final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25* radyBitTable;

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    CData/*6:0*/ __PVT__resetIndex;
    CData/*0:0*/ __Vlvbound_hdb6a0e9e__0;
    IData/*31:0*/ __PVT__unnamedblk3__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk6__DOT__k;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__unnamedblk5__DOT__unnamedblk7__DOT__k;
    VL_IN8(__PVT__wakeup[5],0,0);
    VL_IN8(__PVT__wakeupDstValid[5],0,0);
    VL_IN8(__PVT__wakeupDstRegNum[5],6,0);
    VL_IN8(__PVT__dispatch[2],0,0);
    VL_IN8(__PVT__dispatchedDstValid[2],0,0);
    VL_IN8(__PVT__dispatchedDstRegNum[2],6,0);
    VL_IN8(__PVT__dispatchedSrcValid[2][3],0,0);
    VL_IN8(__PVT__dispatchedSrcRegNum[2][3],6,0);
    VL_OUT8(__PVT__dispatchedSrcReady[2][3],0,0);
    VlUnpacked<CData/*0:0*/, 7> __PVT__readyWE;
    VlUnpacked<CData/*0:0*/, 7> __PVT__readyWV;
    VlUnpacked<CData/*6:0*/, 7> __PVT__readyWA;
    VlUnpacked<CData/*0:0*/, 6> __PVT__readyRV;
    VlUnpacked<CData/*6:0*/, 6> __PVT__readyRA;
    VlUnpacked<CData/*0:0*/, 6> __Vcellout__radyBitTable____pinNumber6;
    VlUnpacked<CData/*0:0*/, 7> __Vcellinp__radyBitTable____pinNumber4;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
