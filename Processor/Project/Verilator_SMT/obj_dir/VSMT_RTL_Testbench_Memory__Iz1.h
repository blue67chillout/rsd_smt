// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_MEMORY__IZ1_H_
#define VERILATED_VSMT_RTL_TESTBENCH_MEMORY__IZ1_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_InitializedBlockRAM__pi1;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_Memory__Iz1 final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_InitializedBlockRAM__pi1* body;

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__memAccessRE,0,0);
    VL_IN8(__PVT__memAccessWE,0,0);
    VL_OUT8(__PVT__memAccessBusy,0,0);
    VL_OUT8(__PVT__nextMemReadSerial,1,0);
    VL_OUT8(__PVT__nextMemWriteSerial,0,0);
    VL_OUT8(__PVT__memReadDataReady,0,0);
    VL_OUT8(__PVT__memReadSerial,1,0);
    VL_OUT8(__PVT__memAccessResponse,1,0);
    CData/*1:0*/ __PVT__nextNextMemReadSerial;
    CData/*0:0*/ __PVT__nextNextMemWriteSerial;
    CData/*0:0*/ __PVT__memReadAccessAck;
    CData/*0:0*/ __PVT__prevMemReadAccessAck;
    CData/*0:0*/ __PVT__memWriteAccessAck;
    CData/*0:0*/ __PVT__prevMemWriteAccessAck;
    CData/*1:0*/ __PVT__processLatencyCount;
    CData/*1:0*/ __PVT__nextProcessLatencyCount;
    CData/*0:0*/ __PVT__pushRequestQueue;
    CData/*0:0*/ __PVT__hasRequest;
    CData/*0:0*/ __PVT__hasRequestReg;
    CData/*0:0*/ __PVT__memReqQueue__DOT__pop;
    CData/*4:0*/ __PVT__memReqQueue__DOT__count;
    CData/*4:0*/ __PVT__memReqQueue__DOT__countReg;
    CData/*6:0*/ __PVT__memReqQueue__DOT__pointer__DOT__regHeadStorage;
    CData/*6:0*/ __PVT__memReqQueue__DOT__pointer__DOT__nextHeadStorage;
    CData/*6:0*/ __PVT__memReqQueue__DOT__pointer__DOT__regTailStorage;
    CData/*6:0*/ __PVT__memReqQueue__DOT__pointer__DOT__nextTailStorage;
    CData/*7:0*/ __PVT__memReqQueue__DOT__pointer__DOT__regCount;
    CData/*7:0*/ __PVT__memReqQueue__DOT__pointer__DOT__nextCount;
    VL_IN(__PVT__memAccessAddr,31,0);
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk2__DOT__i;
    IData/*31:0*/ __PVT__memReqQueue__DOT__randReg;
    IData/*31:0*/ __PVT__memReqQueue__DOT__randNext;
    IData/*31:0*/ __PVT__memReqQueue__DOT__RANDOM_VALUE;
    VL_IN64(__PVT__memAccessWriteData,63,0);
    VL_OUT64(__PVT__memReadData,63,0);
    VlWide<3>/*68:0*/ __PVT__nextMemPipeReg;
    VlWide<4>/*101:0*/ __PVT__pushedData;
    VlWide<4>/*101:0*/ __PVT__requestData;
    VlWide<4>/*101:0*/ __PVT__requestDataReg;
    VlWide<3>/*68:0*/ __Vlvbound_hd17915af__0;
    VlUnpacked<VlWide<3>/*68:0*/, 5> __PVT__memPipeReg;
    VlUnpacked<VlWide<4>/*101:0*/, 128> __PVT__memReqQueue__DOT__memoryRequestQueue;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_Memory__Iz1(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_Memory__Iz1();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_Memory__Iz1);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
