// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Primary model header
//
// This header should be included by all source files instantiating the design.
// The class here is then constructed to instantiate the design.
// See the Verilator manual for examples.

#ifndef VERILATED_VSMT_RTL_TESTBENCH_H_
#define VERILATED_VSMT_RTL_TESTBENCH_H_  // guard

#include "verilated.h"
#include "svdpi.h"

class VSMT_RTL_Testbench__Syms;
class VSMT_RTL_Testbench___024root;
class VerilatedVcdC;
class VSMT_RTL_Testbench_DumperTypes;
class VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg;
class VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg;
class VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg;
class VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg;
class VSMT_RTL_Testbench_MemoryTypes;
class VSMT_RTL_Testbench_SMT_RTL_Testbench;
class VSMT_RTL_Testbench___024unit;


// This class is the main interface to the Verilated model
class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench VL_NOT_FINAL : public VerilatedModel {
  private:
    // Symbol table holding complete model state (owned by this class)
    VSMT_RTL_Testbench__Syms* const vlSymsp;

  public:

    // CONSTEXPR CAPABILITIES
    // Verilated with --trace?
    static constexpr bool traceCapable = true;

    // PORTS
    // The application code writes and reads these signals to
    // propagate new values into/out from the Verilated model.

    // CELLS
    // Public to allow access to /* verilator public */ items.
    // Otherwise the application code can consider these internals.
    VSMT_RTL_Testbench_SMT_RTL_Testbench* const SMT_RTL_Testbench;
    VSMT_RTL_Testbench___024unit* const __PVT____024unit;
    VSMT_RTL_Testbench_MemoryTypes* const MemoryTypes;
    VSMT_RTL_Testbench_DumperTypes* const __PVT__DumperTypes;
    VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg* const DumperTypes__03a__03aKanataDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg* const DumperTypes__03a__03aSerialDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg* const DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg* const DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg;

    // Root instance pointer to allow access to model internals,
    // including inlined /* verilator public_flat_* */ items.
    VSMT_RTL_Testbench___024root* const rootp;

    // CONSTRUCTORS
    /// Construct the model; called by application code
    /// If contextp is null, then the model will use the default global context
    /// If name is "", then makes a wrapper with a
    /// single model invisible with respect to DPI scope names.
    explicit VSMT_RTL_Testbench(VerilatedContext* contextp, const char* name = "TOP");
    explicit VSMT_RTL_Testbench(const char* name = "TOP");
    /// Destroy the model; called (often implicitly) by application code
    virtual ~VSMT_RTL_Testbench();
  private:
    VL_UNCOPYABLE(VSMT_RTL_Testbench);  ///< Copying not allowed

  public:
    // API METHODS
    /// Evaluate the model.  Application must call when inputs change.
    void eval() { eval_step(); eval_end_step(); }
    /// Evaluate when calling multiple units/models per time step.
    void eval_step();
    /// Evaluate at end of a timestep for tracing, when using eval_step().
    /// Application must call after all eval() and before time changes.
    void eval_end_step();
    /// Simulation complete, run final blocks.  Application must call on completion.
    void final();
    /// Are there scheduled events to handle?
    bool eventsPending();
    /// Returns time at next time slot. Aborts if !eventsPending()
    uint64_t nextTimeSlot();
    /// Trace signals in the model; called by application code
    void trace(VerilatedTraceBaseC* tfp, int levels, int options = 0) { contextp()->trace(tfp, levels, options); }
    /// Retrieve name of this model instance (as passed to constructor).
    const char* name() const;

    // Abstract methods from VerilatedModel
    const char* hierName() const override final;
    const char* modelName() const override final;
    unsigned threads() const override final;
    /// Prepare for cloning the model at the process level (e.g. fork in Linux)
    /// Release necessary resources. Called before cloning.
    void prepareClone() const;
    /// Re-init after cloning the model at the process level (e.g. fork in Linux)
    /// Re-allocate necessary resources. Called after cloning.
    void atClone() const;
    std::unique_ptr<VerilatedTraceConfig> traceConfig() const override final;
  private:
    // Internal functions - trace registration
    void traceBaseModel(VerilatedTraceBaseC* tfp, int levels, int options);
};

#endif  // guard
