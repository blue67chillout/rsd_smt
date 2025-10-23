// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_DUMPERTYPES__03A__03ASERIALDUMPER__VCLPKG_H_
#define VERILATED_VSMT_RTL_TESTBENCH_DUMPERTYPES__03A__03ASERIALDUMPER__VCLPKG_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg final : public VerilatedModule {
  public:

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


class VSMT_RTL_Testbench__Syms;

class VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper : public VlClass {
  public:

    // DESIGN SPECIFIC STATE
    IData/*31:0*/ __PVT__m_file;
    std::string __PVT__m_str;
    void __VnoInFunc_CheckSignal(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, CData/*0:0*/ we, IData/*31:0*/ data, IData/*31:0*/ showOutput);
    void __VnoInFunc_DumpToFile(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, std::string fileName);
    void __VnoInFunc_Init(VSMT_RTL_Testbench__Syms* __restrict vlSymsp);
  private:
    void _ctor_var_reset(VSMT_RTL_Testbench__Syms* __restrict vlSymsp);
  public:
    VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper(VSMT_RTL_Testbench__Syms* __restrict vlSymsp);
    std::string to_string() const;
    std::string to_string_middle() const;
    ~VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper() {}
};

std::string VL_TO_STRING(const VlClassRef<VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper>& obj);

#endif  // guard
