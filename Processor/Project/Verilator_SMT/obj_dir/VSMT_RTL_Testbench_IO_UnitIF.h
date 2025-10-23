// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_IO_UNITIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_IO_UNITIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_IO_UnitIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    VL_IN8(__PVT__rstStart,0,0);
    VL_OUT8(__PVT__serialWE,0,0);
    CData/*0:0*/ __PVT__ioWE;
    VL_OUT(__PVT__serialWriteDataOut,31,0);
    IData/*31:0*/ __PVT__ioWriteDataIn;
    IData/*21:0*/ __PVT__ioWriteAddrIn;
    IData/*31:0*/ __PVT__ioReadDataOut;
    IData/*21:0*/ __PVT__ioReadAddrIn;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_IO_UnitIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_IO_UnitIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_IO_UnitIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_IO_UnitIF* obj);

#endif  // guard
