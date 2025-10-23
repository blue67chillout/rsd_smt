// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DESTINATIONRAM_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DESTINATIONRAM_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DestinationRAM final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15* dstRAM;

    // DESIGN SPECIFIC STATE
    CData/*3:0*/ __PVT__rstIndex;
    IData/*31:0*/ __PVT__unnamedblk7__DOT__i;
    VlUnpacked<CData/*0:0*/, 2> __PVT__write;
    VlUnpacked<CData/*3:0*/, 2> __PVT__writePtr;
    VlUnpacked<CData/*7:0*/, 2> __PVT__writeData;
    VlUnpacked<CData/*3:0*/, 5> __PVT__readPtr;
    VlUnpacked<CData/*7:0*/, 5> __PVT__readData;
    VlUnpacked<CData/*7:0*/, 5> __Vcellout__dstRAM__rv;
    VlUnpacked<CData/*7:0*/, 2> __Vcellinp__dstRAM__wv;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DestinationRAM(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DestinationRAM();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DestinationRAM);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
