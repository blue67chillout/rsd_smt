// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DISTRIBUTEDMULTIPORTRAM__PI19_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DISTRIBUTEDMULTIPORTRAM__PI19_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19 final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41* genblk1__DOT__body;

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
    VL_IN8(__PVT__we[2],0,0);
    VL_IN8(__PVT__wa[2],6,0);
    VL_IN64(__PVT__wv[2],32,0);
    VL_IN8(__PVT__ra[5],6,0);
    VL_OUT64(__PVT__rv[5],32,0);
    VlUnpacked<QData/*32:0*/, 128> debugValue;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
