// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DUMPERTYPES__03A__03AREGISTERFILECSV_DUMPER__VCLPKG_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DUMPERTYPES__03A__03AREGISTERFILECSV_DUMPER__VCLPKG_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg final : public VerilatedModule {
  public:

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


class VSMT_RTL_Testbench__Syms;

class VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper : public VlClass {
  public:

    // DESIGN SPECIFIC STATE
    IData/*31:0*/ __PVT__m_file;
    IData/*31:0*/ __PVT__m_cycle;
    void __VnoInFunc_Close(VSMT_RTL_Testbench__Syms* __restrict vlSymsp);
    void __VnoInFunc_Dump(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, IData/*31:0*/ pc, VlUnpacked<IData/*31:0*/, 64> regData);
    void __VnoInFunc_Open(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, std::string fileName);
    void __VnoInFunc_ProceedCycle(VSMT_RTL_Testbench__Syms* __restrict vlSymsp);
  private:
    void _ctor_var_reset(VSMT_RTL_Testbench__Syms* __restrict vlSymsp);
  public:
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper(VSMT_RTL_Testbench__Syms* __restrict vlSymsp);
    std::string to_string() const;
    std::string to_string_middle() const;
    ~VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper() {}
};

std::string VL_TO_STRING(const VlClassRef<VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper>& obj);

#endif  // guard
