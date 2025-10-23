// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_CSR_UNITIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_CSR_UNITIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_CSR_UnitIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    VL_IN8(__PVT__reqExternalInterrupt,0,0);
    VL_IN8(__PVT__externalInterruptCode,4,0);
    CData/*0:0*/ __PVT__tid;
    CData/*0:0*/ __PVT__csrWE;
    CData/*0:0*/ __PVT__triggerExcpt;
    CData/*0:0*/ __PVT__triggerInterrupt;
    CData/*4:0*/ __PVT__interruptCode;
    CData/*0:0*/ __PVT__reqTimerInterrupt;
    CData/*4:0*/ __PVT__externalInterruptCodeInCSR;
    CData/*1:0*/ __PVT__commitNum;
    CData/*4:0*/ __PVT__fflags;
    CData/*2:0*/ __PVT__frm;
    CData/*0:0*/ __PVT__fflagsWE;
    CData/*4:0*/ __PVT__fflagsData;
    IData/*31:0*/ __PVT__csrReadOut;
    IData/*31:0*/ __PVT__csrWriteIn;
    IData/*19:0*/ __PVT__excptCauseAddr;
    IData/*31:0*/ __PVT__excptTargetAddr;
    VlWide<11>/*351:0*/ __PVT__csrWholeOut;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_CSR_UnitIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_CSR_UnitIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_CSR_UnitIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_CSR_UnitIF* obj);

#endif  // guard
