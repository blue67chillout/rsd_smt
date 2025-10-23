// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_MEMORYDEPENDENCYPREDICTOR_H_
#define VERILATED_VSMT_RTL_TESTBENCH_MEMORYDEPENDENCYPREDICTOR_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB1_W1;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_MemoryDependencyPredictor final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB1_W1* mdt;

    // DESIGN SPECIFIC STATE
    SData/*9:0*/ __PVT__resetIndex;
    SData/*9:0*/ __Vlvbound_hd4b5281c__0;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk5__DOT__i;
    VlUnpacked<CData/*0:0*/, 1> __PVT__mdtWE;
    VlUnpacked<SData/*9:0*/, 1> __PVT__mdtWA;
    VlUnpacked<CData/*0:0*/, 1> __PVT__mdtWV;
    VlUnpacked<SData/*9:0*/, 2> __PVT__mdtRA;
    VlUnpacked<CData/*0:0*/, 2> __PVT__mdtRV;
    VlUnpacked<CData/*0:0*/, 2> __PVT__prediction;
    VlUnpacked<CData/*0:0*/, 2> __Vcellout__mdt__rv;
    VlUnpacked<CData/*0:0*/, 1> __Vcellinp__mdt__wv;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_MemoryDependencyPredictor(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_MemoryDependencyPredictor();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_MemoryDependencyPredictor);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
