// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_CONTROLLERIF_H_
#define VERILATED_VSMT_RTL_TESTBENCH_CONTROLLERIF_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_ControllerIF final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(__PVT__clk,0,0);
    VL_IN8(__PVT__rst,0,0);
    CData/*1:0*/ __PVT__npStage;
    CData/*1:0*/ __PVT__ifStage;
    CData/*1:0*/ __PVT__pdStage;
    CData/*1:0*/ __PVT__idStage;
    CData/*1:0*/ __PVT__rnStage;
    CData/*1:0*/ __PVT__dsStage;
    CData/*1:0*/ __PVT__scStage;
    CData/*1:0*/ __PVT__isStage;
    CData/*1:0*/ __PVT__backEnd;
    CData/*1:0*/ __PVT__cmStage;
    CData/*0:0*/ __PVT__ifStageEmpty;
    CData/*0:0*/ __PVT__pdStageEmpty;
    CData/*0:0*/ __PVT__idStageEmpty;
    CData/*0:0*/ __PVT__rnStageEmpty;
    CData/*0:0*/ __PVT__wholePipelineEmpty;
    CData/*0:0*/ __PVT__npStageSendBubbleLowerForInterrupt;
    CData/*0:0*/ __PVT__ifStageSendBubbleLower;
    CData/*0:0*/ __PVT__idStageStallUpper;
    CData/*0:0*/ __PVT__rnStageSendBubbleLower;
    CData/*0:0*/ __PVT__isStageStallUpper;
    CData/*0:0*/ __PVT__stallByDecodeStage;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_ControllerIF(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_ControllerIF();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_ControllerIF);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};

std::string VL_TO_STRING(const VSMT_RTL_Testbench_ControllerIF* obj);

#endif  // guard
