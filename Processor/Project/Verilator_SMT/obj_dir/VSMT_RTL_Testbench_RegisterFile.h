// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_REGISTERFILE_H_
#define VERILATED_VSMT_RTL_TESTBENCH_REGISTERFILE_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi18;
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_RegisterFile final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi18* phyReg;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19* phyFPReg;

    // DESIGN SPECIFIC STATE
    CData/*6:0*/ __PVT__regRstIndex;
    CData/*6:0*/ __PVT__fpRstIndex;
    CData/*6:0*/ __Vlvbound_h8b146626__0;
    IData/*31:0*/ __PVT__unnamedblk6__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk10__DOT__i;
    VlUnpacked<CData/*0:0*/, 5> __PVT__regWE;
    VlUnpacked<CData/*6:0*/, 5> __PVT__dstRegNum;
    VlUnpacked<QData/*32:0*/, 5> __PVT__dstRegData;
    VlUnpacked<CData/*6:0*/, 11> __PVT__srcRegNum;
    VlUnpacked<QData/*32:0*/, 11> __PVT__srcRegData;
    VlUnpacked<CData/*0:0*/, 2> __PVT__fpRegWE;
    VlUnpacked<CData/*6:0*/, 2> __PVT__dstFPRegNum;
    VlUnpacked<QData/*32:0*/, 2> __PVT__dstFPRegData;
    VlUnpacked<CData/*6:0*/, 5> __PVT__srcFPRegNum;
    VlUnpacked<QData/*32:0*/, 5> __PVT__srcFPRegData;
    VlUnpacked<QData/*32:0*/, 11> __Vcellout__phyReg__rv;
    VlUnpacked<QData/*32:0*/, 5> __Vcellinp__phyReg__wv;
    VlUnpacked<QData/*32:0*/, 5> __Vcellout__phyFPReg__rv;
    VlUnpacked<QData/*32:0*/, 2> __Vcellinp__phyFPReg__wv;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_RegisterFile(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_RegisterFile();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_RegisterFile);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
