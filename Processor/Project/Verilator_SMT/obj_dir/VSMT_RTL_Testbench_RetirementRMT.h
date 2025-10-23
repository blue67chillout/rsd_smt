// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_RETIREMENTRMT_H_
#define VERILATED_VSMT_RTL_TESTBENCH_RETIREMENTRMT_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RetirementRMT final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8* regRMT;

    // DESIGN SPECIFIC STATE
    IData/*31:0*/ __PVT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__i;
    VlUnpacked<CData/*0:0*/, 2> __PVT__we;
    VlUnpacked<CData/*5:0*/, 2> __PVT__writeLogRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__writePhyRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__rstWriteLogRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__rstWritePhyRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__readLogRegNum;
    VlUnpacked<CData/*5:0*/, 2> __PVT__readPhyRegNum;
    VlUnpacked<CData/*5:0*/, 2> __Vcellinp__regRMT__ra;
    VlUnpacked<CData/*5:0*/, 2> __Vcellinp__regRMT__wa;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RetirementRMT(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RetirementRMT();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RetirementRMT);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
