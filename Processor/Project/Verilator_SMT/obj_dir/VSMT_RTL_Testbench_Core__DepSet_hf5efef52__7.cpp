// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___nba_sequent__TOP__SMT_RTL_Testbench__core__1(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___nba_sequent__TOP__SMT_RTL_Testbench__core__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ idStage__DOT____Vlvbound_hf1753291__0;
    idStage__DOT____Vlvbound_hf1753291__0 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__261__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__261__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__261__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__261__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__261__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__261__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__265__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__265__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__265__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__265__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__265__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__265__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__269__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__269__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__269__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__269__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__269__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__269__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__278__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__278__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__278__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__278__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__278__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__278__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__282__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__282__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__282__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__282__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__282__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__282__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__291__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__291__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__291__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__291__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__291__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__291__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__295__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__295__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__295__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__295__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__295__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__295__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__299__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__299__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__299__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__299__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__299__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__299__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__308__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__308__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__308__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__308__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__308__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__308__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__312__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__312__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__312__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__312__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__312__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__312__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__322__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__322__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__322__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__322__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__322__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__322__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__326__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__326__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__326__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__326__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__326__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__326__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__330__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__330__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__330__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__330__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__330__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__330__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__334__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__334__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__334__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__334__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__334__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__334__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__343__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__343__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__343__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__343__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__343__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__343__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__347__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__347__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__347__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__347__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__347__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__347__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__356__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__356__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__356__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__356__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__356__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__356__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__360__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__360__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__360__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__360__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__360__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__360__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__364__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__364__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__364__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__364__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__364__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__364__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__372__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__372__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__372__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__372__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__372__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__372__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__376__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__376__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__376__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__376__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__376__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__376__opFunct12 = 0;
    CData/*3:0*/ __Vtask_RISCV_EmitZba__385__aluCode;
    __Vtask_RISCV_EmitZba__385__aluCode = 0;
    CData/*3:0*/ __Vtask_RISCV_EmitZicond__389__aluCode;
    __Vtask_RISCV_EmitZicond__389__aluCode = 0;
    VlWide<3>/*75:0*/ __Vtask_RISCV_DecodeOp__392__shiftOp;
    VL_ZERO_W(76, __Vtask_RISCV_DecodeOp__392__shiftOp);
    VlWide<3>/*75:0*/ __Vtask_RISCV_DecodeOp__392__rijOp;
    VL_ZERO_W(76, __Vtask_RISCV_DecodeOp__392__rijOp);
    VlWide<3>/*75:0*/ __Vtask_RISCV_DecodeOp__392__selectOp;
    VL_ZERO_W(76, __Vtask_RISCV_DecodeOp__392__selectOp);
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__398__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__398__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__398__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__398__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__398__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__398__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__402__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__402__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__402__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__402__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__402__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__402__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__406__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__406__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__406__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__406__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__406__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__406__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__415__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__415__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__415__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__415__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__415__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__415__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__419__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__419__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__419__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__419__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__419__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__419__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__428__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__428__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__428__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__428__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__428__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__428__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__432__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__432__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__432__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__432__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__432__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__432__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__436__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__436__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__436__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__436__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__436__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__436__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__444__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__444__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__444__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__444__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__444__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__444__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__448__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__448__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__448__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__448__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__448__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__448__opFunct12 = 0;
    VlWide<3>/*75:0*/ __Vtask_RISCV_DecodeOpImm__451__shiftOp;
    VL_ZERO_W(76, __Vtask_RISCV_DecodeOpImm__451__shiftOp);
    VlWide<3>/*75:0*/ __Vtask_RISCV_DecodeOpImm__451__rijOp;
    VL_ZERO_W(76, __Vtask_RISCV_DecodeOpImm__451__rijOp);
    VlWide<3>/*75:0*/ __Vtask_RISCV_DecodeOpImm__451__selectOp;
    VL_ZERO_W(76, __Vtask_RISCV_DecodeOpImm__451__selectOp);
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__459__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__459__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__459__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__459__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__459__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__459__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__463__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__463__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__463__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__463__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__463__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__463__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__471__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__471__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__471__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__471__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__471__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__471__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__475__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__475__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__475__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__475__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__475__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__475__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__479__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__479__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__479__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__479__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__479__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__479__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__488__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__488__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__488__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__488__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__488__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__488__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__492__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__492__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__492__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__492__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__492__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__492__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__501__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__501__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__501__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__501__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__501__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__501__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__505__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__505__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__505__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__505__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__505__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__505__opFunct12 = 0;
    IData/*31:0*/ __Vtask_RISCV_EmitIllegalOp__509__isfSystem;
    __Vtask_RISCV_EmitIllegalOp__509__isfSystem = 0;
    CData/*2:0*/ __Vtask_RISCV_EmitIllegalOp__509__opFunct3;
    __Vtask_RISCV_EmitIllegalOp__509__opFunct3 = 0;
    SData/*11:0*/ __Vtask_RISCV_EmitIllegalOp__509__opFunct12;
    __Vtask_RISCV_EmitIllegalOp__509__opFunct12 = 0;
    // Body
    vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined = 0U;
    if ((IData)((0U != (0xc0U & vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U])))) {
        vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined = 1U;
    }
    if ((IData)((0U != (0xc0000U & vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U])))) {
        vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined = 1U;
    }
    if ((IData)((0U != (0xc0000000U & vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U])))) {
        vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined = 1U;
    }
    if (((IData)(vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined) 
         | vlSelfRef.__PVT__pdStage__DOT__illegalPC
         [0U])) {
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__illegalPC 
            = vlSelfRef.__PVT__pdStage__DOT__illegalPC
            [0U];
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__illegalPC 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__illegalPC;
        __Vtask_RISCV_EmitIllegalOp__261__isfSystem = 0;
        __Vtask_RISCV_EmitIllegalOp__261__opFunct3 = 0;
        __Vtask_RISCV_EmitIllegalOp__261__opFunct12 = 0;
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo = 1U;
        vlSelf->__Vtask_RISCV_EmitIllegalOp__261__systemOp = VL_RAND_RESET_Q(53);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
            = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
            = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
            = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
            = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
            = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
            = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp) 
               | (0x80000000ULL | ((QData)((IData)(
                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__illegalPC)
                                                     ? 4U
                                                     : 3U))) 
                                   << 0x20U)));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U] 
            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U]) 
               | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
                                      << 7U))) << 8U));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[1U] 
            = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
                                    << 7U))) >> 0x18U) 
               | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
                                       << 7U)) >> 0x20U)) 
                  << 8U));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[2U] 
            = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__systemOp 
                                                << 7U)) 
                                            >> 0x20U)) 
                                   >> 0x18U)));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U] 
            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U] 
            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U] 
            = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__opInfo[0U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[0U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__opInfo[1U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[1U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__opInfo[2U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__261__opInfo[2U];
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[0U] 
            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[1U] 
            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[2U] 
            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[2U]) 
               | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[2U] 
            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[2U]) 
               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                  << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[3U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                             << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[4U] 
            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[4U]) 
               | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                << 0xcU)));
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__262__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[4U] 
            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[4U]) 
               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                  << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[5U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                          << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[6U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                          << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[7U] 
            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                       >> 8U));
        vlSelfRef.__Vfunc_ModifyMicroOp__263__src[0U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__opInfo[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__src[1U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__opInfo[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__src[2U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__opInfo[2U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__263__src[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__op[1U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__263__src[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__op[2U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__263__src[2U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U] 
            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U] 
            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U] 
            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[0U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__263__op[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[1U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__263__op[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[2U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__263__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[2U] 
            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[2U]) 
               | (vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[0U] 
                  << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[3U] 
            = ((vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[0U] 
                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[1U] 
                             << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[4U] 
            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[4U]) 
               | ((vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[1U] 
                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__263__Vfuncout[2U] 
                                << 0xcU)));
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[0U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[0U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[1U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[1U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[2U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[2U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[3U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[3U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[4U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[4U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[5U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[5U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[6U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[6U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps[7U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__260__microOps[7U];
    }
    vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
        = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg[1U][2U] 
            << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                        [1U][1U] >> 0x15U));
    vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__rv32mFunct7 
        = (vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
           >> 0x19U);
    vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zbaFunct7 
        = (vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
           >> 0x19U);
    vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zicondFunct7 
        = (vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf 
           >> 0x19U);
    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
    if ((0x40U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
        if ((0x20U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((0x10U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((8U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__265__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__265__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__265__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__265__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__265__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__266__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__267__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__267__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__267__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__267__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__267__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__267__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__267__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__264__microOps[7U];
                } else if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__269__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__269__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__269__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__269__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__269__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__270__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__271__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__271__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__271__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__271__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__271__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__271__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__271__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__268__microOps[7U];
                } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__isf 
                            = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][2U] << 0xbU) | 
                               (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][1U] >> 0x15U));
                        if ((0U == (7U & (vlSelfRef.__Vtask_RISCV_DecodeSystem__272__isf 
                                          >> 0xcU)))) {
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isf 
                                = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__isf;
                            vlSelf->__Vtask_RISCV_EmitSystemOp__273__systemOp = VL_RAND_RESET_Q(53);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                = vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isf;
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opFunct12 
                                = (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                   >> 0x14U);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                = (0xfffffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                = (0x1fbfffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                = (0x1ffeffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                = (0x107fffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                = (0x1fc1ffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                = (0x1fff07ffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                = (0x80000000ULL | 
                                   (0x1fffff00000000ULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp));
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__undefined = 0U;
                            if ((0x105U == (IData)(vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opFunct12))) {
                                vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U] 
                                    = (0xe0fU & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U]);
                                vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                    = (0x100000000ULL 
                                       | (0x1ffff8ffffffffULL 
                                          & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp));
                            } else {
                                if ((0U == (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                            >> 0x14U))) {
                                    vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                        = (0x1ffff8ffffffffULL 
                                           & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp);
                                } else if ((1U == (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                   >> 0x14U))) {
                                    vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                        = (0x100000000ULL 
                                           | (0x1ffff8ffffffffULL 
                                              & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp));
                                } else if ((0x302U 
                                            == (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                >> 0x14U))) {
                                    vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                        = (0x200000000ULL 
                                           | (0x1ffff8ffffffffULL 
                                              & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp));
                                } else {
                                    vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                        = (0x100000000ULL 
                                           | (0x1ffff8ffffffffULL 
                                              & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp));
                                    vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__undefined = 1U;
                                }
                                vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U] 
                                    = (0x160U | (0xe0fU 
                                                 & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U]));
                                if ((1U & (~ VL_ONEHOT_I(
                                                         (((0x302U 
                                                            == 
                                                            (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                             >> 0x14U)) 
                                                           << 2U) 
                                                          | (((1U 
                                                               == 
                                                               (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                                >> 0x14U)) 
                                                              << 1U) 
                                                             | (0U 
                                                                == 
                                                                (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                                 >> 0x14U)))))))) {
                                    if ((0U != (((0x302U 
                                                  == 
                                                  (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                   >> 0x14U)) 
                                                 << 2U) 
                                                | (((1U 
                                                     == 
                                                     (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                      >> 0x14U)) 
                                                    << 1U) 
                                                   | (0U 
                                                      == 
                                                      (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                       >> 0x14U)))))) {
                                        if (VL_UNLIKELY((
                                                         vlSymsp->_vm_contextp__->assertOn()))) {
                                            VL_WRITEF_NX("[%0t] %%Error: Decoder.sv:1137: Assertion failed in %N$unit.RISCV_EmitSystemOp: unique case, but multiple matches found for '12'h%x'\n",0,
                                                         64,
                                                         VL_TIME_UNITED_Q(1000),
                                                         -9,
                                                         vlSymsp->name(),
                                                         12,
                                                         (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__isfSystem 
                                                          >> 0x14U));
                                            VL_STOP_MT("Decoder/Decoder.sv", 1137, "");
                                        }
                                    }
                                }
                            }
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U] 
                                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U]) 
                                   | ((IData)((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                                  << 7U))) 
                                      << 8U));
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[1U] 
                                = (((IData)((0x2aULL 
                                             | (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                                << 7U))) 
                                    >> 0x18U) | ((IData)(
                                                         ((0x2aULL 
                                                           | (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                                              << 7U)) 
                                                          >> 0x20U)) 
                                                 << 8U));
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U] 
                                = ((0xff0U & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U]) 
                                   | (0xfffU & ((IData)(
                                                        ((0x2aULL 
                                                          | (vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__systemOp 
                                                             << 7U)) 
                                                         >> 0x20U)) 
                                                >> 0x18U)));
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U] 
                                = (0x400U | (0x1ffU 
                                             & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U]));
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U] 
                                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U] 
                                = ((0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U]) 
                                   | (0xffffffc0U & 
                                      ((IData)(vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__undefined) 
                                       << 7U)));
                            vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U] 
                                = (1U | vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[0U] 
                                = vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[0U];
                            vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[1U] 
                                = vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[1U];
                            vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[2U] 
                                = vlSelfRef.__Vtask_RISCV_EmitSystemOp__273__opInfo[2U];
                        } else {
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isf 
                                = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__isf;
                            vlSelf->__Vtask_RISCV_EmitCSR_Op__274__memOp = VL_RAND_RESET_Q(53);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isfSystem 
                                = vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isf;
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opFunct3 
                                = (7U & (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isfSystem 
                                         >> 0xcU));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = (0xfffffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = (0x1fbfffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = (0x1ffeffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = ((0x107fffffffffffULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                   | ((QData)((IData)(
                                                      (0x1fU 
                                                       & (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isfSystem 
                                                          >> 7U)))) 
                                      << 0x2fU));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = ((0x1fc1ffffffffffULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                   | ((QData)((IData)(
                                                      (0x1fU 
                                                       & (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isfSystem 
                                                          >> 0xfU)))) 
                                      << 0x29U));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = (0x1fff07ffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = (0x1fffffffc00fffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = (0x1ffff83fffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = ((0x1fffffff3fffffULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                   | ((QData)((IData)(
                                                      ((2U 
                                                        & (IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opFunct3))
                                                        ? 
                                                       ((1U 
                                                         & (IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opFunct3))
                                                         ? 3U
                                                         : 2U)
                                                        : 1U))) 
                                      << 0x16U));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = ((0x1ffffffffff000ULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                   | (IData)((IData)(
                                                     (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isfSystem 
                                                      >> 0x14U))));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = ((0x1ffffffeffffffULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                   | ((QData)((IData)(
                                                      (1U 
                                                       & (~ 
                                                          (((1U 
                                                             == (IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opFunct3)) 
                                                            | (2U 
                                                               == (IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opFunct3))) 
                                                           | (3U 
                                                              == (IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opFunct3))))))) 
                                      << 0x18U));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                = ((0x1fffffc1ffffffULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                   | ((QData)((IData)(
                                                      (0x1fU 
                                                       & (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isfSystem 
                                                          >> 0xfU)))) 
                                      << 0x19U));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U] 
                                = ((0x7fffU & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U]) 
                                   | ((IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                      << 0xfU));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[1U] 
                                = (((IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp) 
                                    >> 0x11U) | ((IData)(
                                                         (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                                          >> 0x20U)) 
                                                 << 0xfU));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[2U] 
                                = ((0xff0U & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[2U]) 
                                   | (0xfffU & ((IData)(
                                                        (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__memOp 
                                                         >> 0x20U)) 
                                                >> 0x11U)));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U] 
                                = ((0xffff80ffU & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U]) 
                                   | (0xffffff00U & 
                                      (0xa00U | ((0U 
                                                  != 
                                                  (0x1fU 
                                                   & (vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__isfSystem 
                                                      >> 7U))) 
                                                 << 8U))));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[2U] 
                                = (0x540U | (0xfU & 
                                             vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[2U]));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U] 
                                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U] 
                                = ((0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U]) 
                                   | (0xffffffc0U & 
                                      ((4U == (IData)(vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opFunct3)) 
                                       << 7U)));
                            vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U] 
                                = (1U | vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[0U] 
                                = vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[0U];
                            vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[1U] 
                                = vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[1U];
                            vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[2U] 
                                = vlSelfRef.__Vtask_RISCV_EmitCSR_Op__274__opInfo[2U];
                        }
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__275__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h519fd50d__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__opInfo[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__276__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__276__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__276__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__276__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__276__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__276__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__276__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeSystem__272__microOps[7U];
                    } else {
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__illegalPC 
                            = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                            [1U];
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__illegalPC 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__illegalPC;
                        __Vtask_RISCV_EmitIllegalOp__278__isfSystem = 0;
                        __Vtask_RISCV_EmitIllegalOp__278__opFunct3 = 0;
                        __Vtask_RISCV_EmitIllegalOp__278__opFunct12 = 0;
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                        vlSelf->__Vtask_RISCV_EmitIllegalOp__278__systemOp = VL_RAND_RESET_Q(53);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                            = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                            = (0x1fbfffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                            = (0x1ffeffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                            = (0x107fffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                            = (0x1fc1ffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                            = ((0x1fff0000000000ULL 
                                & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp) 
                               | (0x80000000ULL | ((QData)((IData)(
                                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__illegalPC)
                                                                     ? 4U
                                                                     : 3U))) 
                                                   << 0x20U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U] 
                            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U]) 
                               | ((IData)((0x2aULL 
                                           | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                                              << 7U))) 
                                  << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[1U] 
                            = (((IData)((0x2aULL | 
                                         (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                                          << 7U))) 
                                >> 0x18U) | ((IData)(
                                                     ((0x2aULL 
                                                       | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                                                          << 7U)) 
                                                      >> 0x20U)) 
                                             << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[2U] 
                            = (0xfffU & (0x560U | ((IData)(
                                                           ((0x2aULL 
                                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__systemOp 
                                                                << 7U)) 
                                                            >> 0x20U)) 
                                                   >> 0x18U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U] 
                            = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__opInfo[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__opInfo[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__opInfo[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__278__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__279__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__opInfo[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__opInfo[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__opInfo[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__280__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__280__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__280__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__280__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__280__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__280__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__280__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__277__microOps[7U];
                    }
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__282__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__282__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__282__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__282__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__282__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__283__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__284__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__284__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__284__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__284__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__284__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__284__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__284__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__281__microOps[7U];
                }
            } else if ((8U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                        if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__isf 
                                = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                    [1U][2U] << 0xbU) 
                                   | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                      [1U][1U] >> 0x15U));
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__isf 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__isf;
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__isfU 
                                = vlSelfRef.__Vtask_RISCV_EmitJAL__286__isf;
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[2U] 
                                = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[2U]);
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U] 
                                = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U]);
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U] 
                                = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U]);
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U] 
                                = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U]) 
                                   | (0xc0000000U & 
                                      (vlSelfRef.__Vtask_RISCV_EmitJAL__286__isfU 
                                       << 0x17U)));
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[2U] 
                                = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[2U]) 
                                   | (7U & (vlSelfRef.__Vtask_RISCV_EmitJAL__286__isfU 
                                            >> 9U)));
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U] 
                                = (0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U]);
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U] 
                                = (0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U]);
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U] 
                                = ((0xffff80ffU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U]) 
                                   | (0xffffff00U & 
                                      (0x4a00U | ((0U 
                                                   != 
                                                   (0x1fU 
                                                    & (vlSelfRef.__Vtask_RISCV_EmitJAL__286__isfU 
                                                       >> 7U))) 
                                                  << 8U))));
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[2U] 
                                = (0x420U | (0xfU & 
                                             vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[2U]));
                            VL_ASSIGNSEL_WQ(76,35,0xfU, vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo, 
                                            VL_EXTEND_QI(35,20, 
                                                         ([&]() {
                                            vlSelfRef.__Vfunc_GetJAL_Target__287__isfJAL 
                                                = vlSelfRef.__Vtask_RISCV_EmitJAL__286__isf;
                                            vlSelfRef.__Vfunc_GetJAL_Target__287__Vfuncout 
                                                = (
                                                   (0x80000U 
                                                    & (vlSelfRef.__Vfunc_GetJAL_Target__287__isfJAL 
                                                       >> 0xcU)) 
                                                   | ((0x7f800U 
                                                       & (vlSelfRef.__Vfunc_GetJAL_Target__287__isfJAL 
                                                          >> 1U)) 
                                                      | ((0x400U 
                                                          & (vlSelfRef.__Vfunc_GetJAL_Target__287__isfJAL 
                                                             >> 0xaU)) 
                                                         | (0x3ffU 
                                                            & (vlSelfRef.__Vfunc_GetJAL_Target__287__isfJAL 
                                                               >> 0x15U)))));
                                        }(), vlSelfRef.__Vfunc_GetJAL_Target__287__Vfuncout)));
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U] 
                                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U] 
                                = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U] 
                                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[0U] 
                                = vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[0U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[1U] 
                                = vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[1U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[2U] 
                                = vlSelfRef.__Vtask_RISCV_EmitJAL__286__opInfo[2U];
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[1U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[2U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U] 
                                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U]);
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U];
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[1U];
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[2U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[0U] 
                                = vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[1U] 
                                = vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[2U] 
                                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[2U]) 
                                   | vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[2U]);
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[1U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[2U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U] 
                                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U]);
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U];
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[1U];
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[2U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[2U] 
                                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[2U]) 
                                   | (vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U] 
                                      << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[3U] 
                                = ((vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U] 
                                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U] 
                                                 << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[4U] 
                                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[4U]) 
                                   | ((vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U] 
                                       >> 0x14U) | 
                                      (vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[2U] 
                                       << 0xcU)));
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[1U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[2U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U] 
                                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U]);
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[0U];
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[1U];
                            vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[2U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__288__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[4U] 
                                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[4U]) 
                                   | (vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U] 
                                      << 0x18U));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[5U] 
                                = ((vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[0U] 
                                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U] 
                                              << 0x18U));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[6U] 
                                = ((vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[1U] 
                                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[2U] 
                                              << 0x18U));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[7U] 
                                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h8cccd569__0[2U] 
                                           >> 8U));
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__src[0U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[0U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__src[1U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[1U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__src[2U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[2U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__289__src[0U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__op[1U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__289__src[1U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__op[2U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__289__src[2U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U] 
                                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U]);
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U] 
                                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U]);
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U] 
                                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U]);
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[0U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__289__op[0U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[1U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__289__op[1U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[2U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__289__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[2U] 
                                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[2U]) 
                                   | (vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[0U] 
                                      << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[3U] 
                                = ((vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[0U] 
                                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[1U] 
                                                 << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[4U] 
                                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[4U]) 
                                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[1U] 
                                       >> 0x14U) | 
                                      (vlSelfRef.__Vfunc_ModifyMicroOp__289__Vfuncout[2U] 
                                       << 0xcU)));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__insnInfo 
                                = (0x10U | (IData)(vlSelfRef.__Vtask_RISCV_DecodeJAL__285__insnInfo));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__insnInfo 
                                = ((0x17U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeJAL__285__insnInfo)) 
                                   | (0x7fffff8U & 
                                      ((vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[0U] 
                                        >> 5U) & ((1U 
                                                   == 
                                                   (0x1fU 
                                                    & ((vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[2U] 
                                                        << 2U) 
                                                       | (vlSelfRef.__Vtask_RISCV_DecodeJAL__285__brOp[1U] 
                                                          >> 0x1eU)))) 
                                                  << 3U))));
                            vlSelfRef.__Vtask_RISCV_DecodeJAL__285__insnInfo 
                                = (2U | (0x18U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeJAL__285__insnInfo)));
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[0U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[1U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[2U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[3U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[4U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[5U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[6U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__microOps[7U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo 
                                = vlSelfRef.__Vtask_RISCV_DecodeJAL__285__insnInfo;
                        } else {
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__illegalPC 
                                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                                [1U];
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__illegalPC 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__illegalPC;
                            __Vtask_RISCV_EmitIllegalOp__291__isfSystem = 0;
                            __Vtask_RISCV_EmitIllegalOp__291__opFunct3 = 0;
                            __Vtask_RISCV_EmitIllegalOp__291__opFunct12 = 0;
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                            vlSelf->__Vtask_RISCV_EmitIllegalOp__291__systemOp = VL_RAND_RESET_Q(53);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                = (0xfffffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                = (0x1fbfffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                = (0x1ffeffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                = (0x107fffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                = (0x1fc1ffffffffffULL 
                                   & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                = ((0x1fff0000000000ULL 
                                    & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp) 
                                   | (0x80000000ULL 
                                      | ((QData)((IData)(
                                                         ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__illegalPC)
                                                           ? 4U
                                                           : 3U))) 
                                         << 0x20U)));
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U] 
                                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U]) 
                                   | ((IData)((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                                  << 7U))) 
                                      << 8U));
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[1U] 
                                = (((IData)((0x2aULL 
                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                                << 7U))) 
                                    >> 0x18U) | ((IData)(
                                                         ((0x2aULL 
                                                           | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                                              << 7U)) 
                                                          >> 0x20U)) 
                                                 << 8U));
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[2U] 
                                = (0xfffU & (0x560U 
                                             | ((IData)(
                                                        ((0x2aULL 
                                                          | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__systemOp 
                                                             << 7U)) 
                                                         >> 0x20U)) 
                                                >> 0x18U)));
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U] 
                                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U] 
                                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U] 
                                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U]);
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__opInfo[0U] 
                                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[0U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__opInfo[1U] 
                                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[1U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__opInfo[2U] 
                                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__291__opInfo[2U];
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[1U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[2U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U] 
                                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U]);
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U];
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[1U];
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[0U] 
                                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[1U] 
                                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[2U] 
                                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[2U]) 
                                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[1U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[2U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U] 
                                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U]);
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U];
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[1U];
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[2U] 
                                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[2U]) 
                                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                      << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[3U] 
                                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                                 << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[4U] 
                                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[4U]) 
                                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                       >> 0x14U) | 
                                      (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                       << 0xcU)));
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[1U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[2U] = 0U;
                            vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U] 
                                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U]);
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[0U];
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[1U];
                            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                = vlSelfRef.__Vtask_EmitInvalidOp__292__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[4U] 
                                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[4U]) 
                                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                      << 0x18U));
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[5U] 
                                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                              << 0x18U));
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[6U] 
                                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                              << 0x18U));
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[7U] 
                                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                           >> 8U));
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__src[0U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__opInfo[0U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__src[1U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__opInfo[1U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__src[2U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__opInfo[2U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__293__src[0U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__op[1U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__293__src[1U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__op[2U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__293__src[2U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U] 
                                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U]);
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U] 
                                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U]);
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U] 
                                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U]);
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[0U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__293__op[0U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[1U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__293__op[1U];
                            vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[2U] 
                                = vlSelfRef.__Vfunc_ModifyMicroOp__293__op[2U];
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[2U] 
                                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[2U]) 
                                   | (vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[0U] 
                                      << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[3U] 
                                = ((vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[0U] 
                                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[1U] 
                                                 << 0xcU));
                            vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[4U] 
                                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[4U]) 
                                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[1U] 
                                       >> 0x14U) | 
                                      (vlSelfRef.__Vfunc_ModifyMicroOp__293__Vfuncout[2U] 
                                       << 0xcU)));
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[0U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[1U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[2U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[3U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[4U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[5U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[6U];
                            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__290__microOps[7U];
                        }
                    } else {
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__illegalPC 
                            = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                            [1U];
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__illegalPC 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__illegalPC;
                        __Vtask_RISCV_EmitIllegalOp__295__isfSystem = 0;
                        __Vtask_RISCV_EmitIllegalOp__295__opFunct3 = 0;
                        __Vtask_RISCV_EmitIllegalOp__295__opFunct12 = 0;
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                        vlSelf->__Vtask_RISCV_EmitIllegalOp__295__systemOp = VL_RAND_RESET_Q(53);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                            = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                            = (0x1fbfffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                            = (0x1ffeffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                            = (0x107fffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                            = (0x1fc1ffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                            = ((0x1fff0000000000ULL 
                                & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp) 
                               | (0x80000000ULL | ((QData)((IData)(
                                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__illegalPC)
                                                                     ? 4U
                                                                     : 3U))) 
                                                   << 0x20U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U] 
                            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U]) 
                               | ((IData)((0x2aULL 
                                           | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                                              << 7U))) 
                                  << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[1U] 
                            = (((IData)((0x2aULL | 
                                         (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                                          << 7U))) 
                                >> 0x18U) | ((IData)(
                                                     ((0x2aULL 
                                                       | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                                                          << 7U)) 
                                                      >> 0x20U)) 
                                             << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[2U] 
                            = (0xfffU & (0x560U | ((IData)(
                                                           ((0x2aULL 
                                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__systemOp 
                                                                << 7U)) 
                                                            >> 0x20U)) 
                                                   >> 0x18U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U] 
                            = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__opInfo[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__opInfo[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__opInfo[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__295__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__296__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__opInfo[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__opInfo[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__opInfo[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__297__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__297__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__297__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__297__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__297__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__297__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__297__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__294__microOps[7U];
                    }
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__299__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__299__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__299__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__299__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__299__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__300__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__301__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__301__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__301__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__301__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__301__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__301__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__301__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__298__microOps[7U];
                }
            } else if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__isf 
                            = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][2U] << 0xbU) | 
                               (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][1U] >> 0x15U));
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__isf 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__isf;
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__isfI 
                            = vlSelfRef.__Vtask_RISCV_EmitJALR__303__isf;
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[2U] 
                            = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[2U]);
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U] 
                            = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U] 
                            = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U] 
                            = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U]) 
                               | (0xc0000000U & (vlSelfRef.__Vtask_RISCV_EmitJALR__303__isfI 
                                                 << 0x17U)));
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[2U] 
                            = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[2U]) 
                               | (7U & (vlSelfRef.__Vtask_RISCV_EmitJALR__303__isfI 
                                        >> 9U)));
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U] 
                            = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U]) 
                               | (0x1f000000U & (vlSelfRef.__Vtask_RISCV_EmitJALR__303__isfI 
                                                 << 9U)));
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U] 
                            = (0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U] 
                            = ((0xffff80ffU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U]) 
                               | (0xffffff00U & (0xa00U 
                                                 | ((0U 
                                                     != 
                                                     (0x1fU 
                                                      & (vlSelfRef.__Vtask_RISCV_EmitJALR__303__isfI 
                                                         >> 7U))) 
                                                    << 8U))));
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[2U] 
                            = (0x430U | (0xfU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[2U]));
                        VL_ASSIGNSEL_WQ(76,35,0xfU, vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo, 
                                        VL_EXTEND_QI(35,20, 
                                                     ([&]() {
                                        vlSelfRef.__Vfunc_GetJALR_Target__304__isfJALR 
                                            = vlSelfRef.__Vtask_RISCV_EmitJALR__303__isfI;
                                        vlSelfRef.__Vfunc_GetJALR_Target__304__Vfuncout 
                                            = ((0xff000U 
                                                & ((- (IData)(
                                                              (vlSelfRef.__Vfunc_GetJALR_Target__304__isfJALR 
                                                               >> 0x1fU))) 
                                                   << 0xcU)) 
                                               | (vlSelfRef.__Vfunc_GetJALR_Target__304__isfJALR 
                                                  >> 0x14U));
                                    }(), vlSelfRef.__Vfunc_GetJALR_Target__304__Vfuncout)));
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U] 
                            = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitJALR__303__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__305__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hd9e74018__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__306__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__306__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__306__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__306__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__306__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__306__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__306__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo 
                            = (0x10U | (IData)(vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo 
                            = ((0x17U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo)) 
                               | (0x7fffff8U & ((vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[0U] 
                                                 >> 5U) 
                                                & ((1U 
                                                    == 
                                                    (0x1fU 
                                                     & ((vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[2U] 
                                                         << 2U) 
                                                        | (vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[1U] 
                                                           >> 0x1eU)))) 
                                                   << 3U))));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo 
                            = ((0x1bU & (IData)(vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo)) 
                               | ((IData)(((0x1000000U 
                                            == (0x1f000000U 
                                                & vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[1U])) 
                                           & (0U == 
                                              (0x1fU 
                                               & ((vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[2U] 
                                                   << 2U) 
                                                  | (vlSelfRef.__Vtask_RISCV_DecodeJALR__302__brOp[1U] 
                                                     >> 0x1eU)))))) 
                                  << 2U));
                        vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo 
                            = (0x1cU & (IData)(vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__microOps[7U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo 
                            = vlSelfRef.__Vtask_RISCV_DecodeJALR__302__insnInfo;
                    } else {
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__illegalPC 
                            = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                            [1U];
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__illegalPC 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__illegalPC;
                        __Vtask_RISCV_EmitIllegalOp__308__isfSystem = 0;
                        __Vtask_RISCV_EmitIllegalOp__308__opFunct3 = 0;
                        __Vtask_RISCV_EmitIllegalOp__308__opFunct12 = 0;
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                        vlSelf->__Vtask_RISCV_EmitIllegalOp__308__systemOp = VL_RAND_RESET_Q(53);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                            = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                            = (0x1fbfffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                            = (0x1ffeffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                            = (0x107fffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                            = (0x1fc1ffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                            = ((0x1fff0000000000ULL 
                                & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp) 
                               | (0x80000000ULL | ((QData)((IData)(
                                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__illegalPC)
                                                                     ? 4U
                                                                     : 3U))) 
                                                   << 0x20U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U] 
                            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U]) 
                               | ((IData)((0x2aULL 
                                           | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                                              << 7U))) 
                                  << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[1U] 
                            = (((IData)((0x2aULL | 
                                         (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                                          << 7U))) 
                                >> 0x18U) | ((IData)(
                                                     ((0x2aULL 
                                                       | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                                                          << 7U)) 
                                                      >> 0x20U)) 
                                             << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[2U] 
                            = (0xfffU & (0x560U | ((IData)(
                                                           ((0x2aULL 
                                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__systemOp 
                                                                << 7U)) 
                                                            >> 0x20U)) 
                                                   >> 0x18U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U] 
                            = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__opInfo[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__opInfo[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__opInfo[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__308__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__309__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__opInfo[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__opInfo[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__opInfo[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__310__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__310__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__310__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__310__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__310__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__310__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__310__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__307__microOps[7U];
                    }
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__312__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__312__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__312__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__312__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__312__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__313__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__314__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__314__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__314__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__314__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__314__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__314__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__314__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__311__microOps[7U];
                }
            } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__isf 
                        = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                            [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                 [1U][1U] 
                                                 >> 0x15U));
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__isf 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__isf;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0x12U;
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__isfR 
                        = vlSelfRef.__Vtask_RISCV_EmitBranch__316__isf;
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__brFunct3 
                        = (7U & (vlSelfRef.__Vtask_RISCV_EmitBranch__316__isfR 
                                 >> 0xcU));
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U] 
                        = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U]);
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U] 
                        = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U] 
                        = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U] 
                        = (0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U] 
                        = (0xff8U & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U]);
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U] 
                        = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U]) 
                           | (0x1f000000U & (vlSelfRef.__Vtask_RISCV_EmitBranch__316__isfR 
                                             << 9U)));
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U] 
                        = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U]) 
                           | (0x7c0000U & (vlSelfRef.__Vtask_RISCV_EmitBranch__316__isfR 
                                           >> 2U)));
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U] 
                        = (0x200U | (0xffff80ffU & 
                                     vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U]));
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U] 
                        = (0x20U | (0xe0fU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U]));
                    vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__funct3 
                        = vlSelfRef.__Vtask_RISCV_EmitBranch__316__brFunct3;
                    vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__condCode 
                        = ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__funct3))
                            ? ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__funct3))
                                ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__funct3))
                                    ? 7U : 6U) : ((1U 
                                                   & (IData)(vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__funct3))
                                                   ? 5U
                                                   : 4U))
                            : ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__funct3))
                                ? 2U : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__funct3))
                                         ? 1U : 0U)));
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__condCode 
                        = vlSelfRef.__Vtask_RISCV_DecodeBrFunct3__317__condCode;
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U] 
                        = ((0x1ffU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U]) 
                           | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitBranch__316__condCode) 
                                        << 9U)));
                    VL_ASSIGNSEL_WQ(76,35,0xfU, vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo, 
                                    VL_EXTEND_QI(35,20, 
                                                 ([&]() {
                                    vlSelfRef.__Vfunc_GetBranchDisplacement__318__isfBr 
                                        = vlSelfRef.__Vtask_RISCV_EmitBranch__316__isfR;
                                    vlSelfRef.__Vfunc_GetBranchDisplacement__318__Vfuncout 
                                        = (((0xff800U 
                                             & ((- (IData)(
                                                           (vlSelfRef.__Vfunc_GetBranchDisplacement__318__isfBr 
                                                            >> 0x1fU))) 
                                                << 0xbU)) 
                                            | (0x400U 
                                               & (vlSelfRef.__Vfunc_GetBranchDisplacement__318__isfBr 
                                                  << 3U))) 
                                           | ((0x3f0U 
                                               & (vlSelfRef.__Vfunc_GetBranchDisplacement__318__isfBr 
                                                  >> 0x15U)) 
                                              | (0xfU 
                                                 & (vlSelfRef.__Vfunc_GetBranchDisplacement__318__isfBr 
                                                    >> 8U))));
                                }(), vlSelfRef.__Vfunc_GetBranchDisplacement__318__Vfuncout)));
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U] 
                        = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__brOp[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__brOp[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__brOp[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitBranch__316__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__319__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_ha3030f99__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__brOp[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__brOp[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__brOp[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__320__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__320__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__320__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__320__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__320__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__320__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__320__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeBranch__315__microOps[7U];
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__322__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__322__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__322__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__322__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__322__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__323__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__324__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__324__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__324__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__324__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__324__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__324__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__324__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__321__microOps[7U];
                }
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__326__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__326__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__326__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__326__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__326__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__327__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__328__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__328__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__328__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__328__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__328__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__328__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__328__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__328__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__325__microOps[7U];
            }
        } else if ((0x10U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((8U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__330__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__330__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__330__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__330__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__330__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__331__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__332__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__332__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__332__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__332__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__332__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__332__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__332__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__332__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__329__microOps[7U];
            } else if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__334__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__334__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__334__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__334__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__334__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__335__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__336__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__336__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__336__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__336__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__336__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__336__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__336__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__336__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__333__microOps[7U];
            } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__isf 
                        = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                            [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                 [1U][1U] 
                                                 >> 0x15U));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__dstRegNum 
                        = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__isf 
                                    >> 7U));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__srcRegNumB 
                        = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__isf 
                                    >> 0x14U));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__srcRegNumA 
                        = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__isf 
                                    >> 0xfU));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__isf 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__isf;
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__isfR 
                        = vlSelfRef.__Vtask_RISCV_EmitFPOp__338__isf;
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rv32fFunct3 
                        = (7U & (vlSelfRef.__Vtask_RISCV_EmitFPOp__338__isfR 
                                 >> 0xcU));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rv32fFunct7 
                        = (vlSelfRef.__Vtask_RISCV_EmitFPOp__338__isfR 
                           >> 0x19U);
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fcvtfunct5 
                        = (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitFPOp__338__isfR 
                                    >> 0x14U));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rm 
                        = (7U & (vlSelfRef.__Vtask_RISCV_EmitFPOp__338__isfR 
                                 >> 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__fcvtfunct5 
                        = vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fcvtfunct5;
                    vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7 
                        = vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rv32fFunct7;
                    vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3 
                        = vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rv32fFunct3;
                    vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__fpuCode 
                        = ((0x40U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                            ? ((0x20U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                ? ((0x10U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                    ? ((8U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                        ? 0x13U : (
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                    ? 0x13U
                                                    : 
                                                   ((2U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                     ? 0x13U
                                                     : 
                                                    ((1U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3))
                                                       ? 0x10U
                                                       : 0xcU)))))
                                    : ((8U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                        ? ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                            ? 0x13U
                                            : ((2U 
                                                & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                ? 0x13U
                                                : (
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                    ? 0x13U
                                                    : 
                                                   ((0U 
                                                     == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__fcvtfunct5))
                                                     ? 0x11U
                                                     : 0x12U))))
                                        : ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                            ? 0x13U
                                            : ((2U 
                                                & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                ? 0x13U
                                                : (
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                    ? 0x13U
                                                    : 
                                                   ((0U 
                                                     == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__fcvtfunct5))
                                                     ? 0xaU
                                                     : 0xbU))))))
                                : ((0x10U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                    ? ((8U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                        ? 0x13U : (
                                                   (4U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                    ? 0x13U
                                                    : 
                                                   ((2U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                     ? 0x13U
                                                     : 
                                                    ((1U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((2U 
                                                       == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3))
                                                       ? 0xdU
                                                       : 
                                                      ((1U 
                                                        == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3))
                                                        ? 0xeU
                                                        : 0xfU))))))
                                    : 0x13U)) : ((0x20U 
                                                  & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                  ? 
                                                 ((0x10U 
                                                   & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                   ? 0x13U
                                                   : 
                                                  ((8U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                    ? 
                                                   ((4U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                     ? 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                       ? 0x13U
                                                       : 4U))
                                                     : 0x13U)
                                                    : 0x13U))
                                                  : 
                                                 ((0x10U 
                                                   & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                   ? 
                                                  ((8U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                    ? 0x13U
                                                    : 
                                                   ((4U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                     ? 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                       ? 0x13U
                                                       : 
                                                      ((0U 
                                                        == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3))
                                                        ? 8U
                                                        : 9U)))
                                                     : 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                       ? 0x13U
                                                       : 
                                                      ((0U 
                                                        == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3))
                                                        ? 5U
                                                        : 
                                                       ((1U 
                                                         == (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3))
                                                         ? 6U
                                                         : 7U))))))
                                                   : 
                                                  ((8U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                    ? 
                                                   ((4U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                     ? 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                       ? 0x13U
                                                       : 3U))
                                                     : 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                       ? 0x13U
                                                       : 2U)))
                                                    : 
                                                   ((4U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                     ? 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                       ? 0x13U
                                                       : 1U))
                                                     : 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                      ? 0x13U
                                                      : 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7))
                                                       ? 0x13U
                                                       : 0U)))))));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOpFunct3__339__fpuCode;
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__dstFP 
                        = (1U & (~ (((((((0xaU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)) 
                                         | (0xbU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                        | (0xcU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                       | (0xdU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                      | (0xeU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                     | (0xfU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                    | (0x10U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)))));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rs1FP 
                        = (1U & (~ (((0x11U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)) 
                                     | (0x12U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                    | (0x13U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)))));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__readrs2 
                        = (1U & (~ ((((((((4U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)) 
                                          | (0x11U 
                                             == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                         | (0x12U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                        | (0xaU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                       | (0xbU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                      | (0x13U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                     | (0xcU == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))) 
                                    | (0x10U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)))));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U] 
                        = ((0xff7U & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U]) 
                           | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__dstFP) 
                                        << 3U)));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U] 
                        = ((0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U]) 
                           | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rs1FP) 
                              << 0x1dU));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U] 
                        = (0x800000U | vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U] 
                        = (0x20000U | vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U] 
                        = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U]) 
                           | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__dstRegNum) 
                              << 0x1eU));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U] 
                        = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U]) 
                           | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__dstRegNum) 
                                        >> 2U)));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U] 
                        = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U]) 
                           | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__srcRegNumA) 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U] 
                        = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U]) 
                           | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__srcRegNumB) 
                              << 0x12U));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U] 
                        = ((0x7fffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U]) 
                           | ((IData)((QData)((IData)(
                                                      (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode) 
                                                        << 0x18U) 
                                                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rm) 
                                                          << 0x15U))))) 
                              << 0xfU));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U] 
                        = ((0xfffe0000U & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U]) 
                           | (((IData)((QData)((IData)(
                                                       (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode) 
                                                         << 0x18U) 
                                                        | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rm) 
                                                           << 0x15U))))) 
                               >> 0x11U) | ((IData)(
                                                    ((QData)((IData)(
                                                                     (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode) 
                                                                       << 0x18U) 
                                                                      | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__rm) 
                                                                         << 0x15U)))) 
                                                     >> 0x20U)) 
                                            << 0xfU)));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U] 
                        = ((0xfffffeffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U]) 
                           | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__dstFP) 
                               | (0U != (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__dstRegNum))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U] 
                        = ((0xffff81ffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U]) 
                           | (0xfffffe00U & (0x200U 
                                             | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__readrs2)
                                                  ? 0U
                                                  : 1U) 
                                                << 0xbU))));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U] 
                        = (0x180U | vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U] 
                        = ((0xf8fU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U]) 
                           | (0xfffU & ((((0U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)) 
                                          | (1U == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode)))
                                          ? 0U : ((2U 
                                                   == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))
                                                   ? 1U
                                                   : 
                                                  ((3U 
                                                    == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))
                                                    ? 2U
                                                    : 
                                                   ((4U 
                                                     == (IData)(vlSelfRef.__Vtask_RISCV_EmitFPOp__338__fpuCode))
                                                     ? 3U
                                                     : 5U)))) 
                                        << 4U)));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U] 
                        = (0x400U | (0x1ffU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U]));
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U] 
                        = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__fpOp[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__fpOp[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__fpOp[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitFPOp__338__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__340__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h2385d58f__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__fpOp[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__fpOp[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__fpOp[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__341__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__341__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__341__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__341__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__341__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__341__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__341__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPOp__337__microOps[7U];
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__343__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__343__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__343__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__343__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__343__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__344__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__345__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__345__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__345__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__345__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__345__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__345__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__345__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__342__microOps[7U];
                }
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__347__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__347__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__347__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__347__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__347__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__348__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__349__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__349__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__349__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__349__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__349__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__349__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__349__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__349__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__346__microOps[7U];
            }
        } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__isf 
                    = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                        [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                             [1U][1U] 
                                             >> 0x15U));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isf 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__isf;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4 
                    = vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isf;
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opCode 
                    = (0x7fU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__rm 
                    = (7U & (vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4 
                             >> 0xcU));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[2U] 
                    = (8U | vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[2U]);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = (0x20000000U | vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = (0x800000U | vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = (0x20000U | vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]) 
                       | (0xc0000000U & (vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4 
                                         << 0x17U)));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[2U] 
                    = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[2U]) 
                       | (7U & (vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4 
                                >> 9U)));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]) 
                       | (0x1f000000U & (vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4 
                                         << 9U)));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]) 
                       | (0x7c0000U & (vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4 
                                       >> 2U)));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = ((0xfffe0fffU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]) 
                       | (0x1f000U & (vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__isfR4 
                                      >> 0xfU)));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = ((0xffffff8fU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__rm) 
                          << 4U));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode 
                    = vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opCode;
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__fpuCode 
                    = ((0x40U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                        ? ((0x20U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                            ? 0x14U : ((0x10U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                        ? 0x14U : (
                                                   (8U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                    ? 
                                                   ((4U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                     ? 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                      ? 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                       ? 0x17U
                                                       : 0x14U)
                                                      : 0x14U)
                                                     : 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                      ? 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                       ? 0x16U
                                                       : 0x14U)
                                                      : 0x14U))
                                                    : 
                                                   ((4U 
                                                     & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                     ? 
                                                    ((2U 
                                                      & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                      ? 
                                                     ((1U 
                                                       & (IData)(vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode))
                                                       ? 0x15U
                                                       : 0x14U)
                                                      : 0x14U)
                                                     : 0x14U))))
                        : 0x14U);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__fpuCode 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOpFunct3__352__fpuCode;
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = ((0xfffff07fU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__fpuCode) 
                          << 7U));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U] 
                    = (0x100U | (0xffU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U]));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U] 
                    = (0xfffffff0U & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[2U] 
                    = (0x5c0U | (0xfU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[2U]));
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U] 
                    = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__fpOp[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__fpOp[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__fpOp[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitFPFMAOp__351__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__353__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hd5dab3e3__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__354__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__fpOp[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__fpOp[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__fpOp[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__354__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__354__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__354__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__354__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__354__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__354__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__354__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPFMAOp__350__microOps[7U];
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__356__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__356__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__356__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__356__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__356__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__357__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__358__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__358__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__358__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__358__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__358__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__358__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__358__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__358__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__355__microOps[7U];
            }
        } else {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__360__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__360__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__360__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__360__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__360__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__361__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__362__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__362__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__362__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__362__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__362__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__362__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__362__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__362__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__359__microOps[7U];
        }
    } else if ((0x20U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
        if ((0x10U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((8U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__364__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__364__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__364__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__364__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__364__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__365__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__366__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__366__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__366__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__366__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__366__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__366__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__366__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__366__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__363__microOps[7U];
            } else if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__isf 
                            = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][2U] << 0xbU) | 
                               (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][1U] >> 0x15U));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__dstRegNum 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__isf 
                                        >> 7U));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__isf 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__isf;
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__isfU 
                            = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__isf;
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[2U] 
                            = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[2U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__dstRegNum) 
                                  << 0x1eU));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[2U] 
                            = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[2U]) 
                               | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__dstRegNum) 
                                            >> 2U)));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = (0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = (0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = (0x2000U | vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__intOperandImmShift 
                            = (0x1c00003U | (0x3ffffcU 
                                             & (vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__isfU 
                                                >> 0xaU)));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U] 
                            = ((0x7fffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U]) 
                               | (vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__intOperandImmShift 
                                  << 0xfU));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = ((0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]) 
                               | (vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__intOperandImmShift 
                                  >> 0x11U));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U] 
                            = (0xfffc3fffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U] 
                            = ((0xfffffeffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U]) 
                               | ((0U != (IData)(vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__dstRegNum)) 
                                  << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U] 
                            = ((0xffff9fffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U]) 
                               | (((0x17U == (0x7fU 
                                              & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__isf))
                                    ? 2U : 0U) << 0xdU));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U] 
                            = (0xa00U | (0xffffe1ffU 
                                         & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U]));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[2U] 
                            = (0x400U | (0xfU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[2U]));
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U] 
                            = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__intOp[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__intOp[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__intOp[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__368__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__369__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__intOp[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__intOp[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__intOp[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__370__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__370__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__370__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__370__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__370__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__370__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__370__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__367__microOps[7U];
                    } else {
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__illegalPC 
                            = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                            [1U];
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__illegalPC 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__illegalPC;
                        __Vtask_RISCV_EmitIllegalOp__372__isfSystem = 0;
                        __Vtask_RISCV_EmitIllegalOp__372__opFunct3 = 0;
                        __Vtask_RISCV_EmitIllegalOp__372__opFunct12 = 0;
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                        vlSelf->__Vtask_RISCV_EmitIllegalOp__372__systemOp = VL_RAND_RESET_Q(53);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                            = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                            = (0x1fbfffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                            = (0x1ffeffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                            = (0x107fffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                            = (0x1fc1ffffffffffULL 
                               & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                            = ((0x1fff0000000000ULL 
                                & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp) 
                               | (0x80000000ULL | ((QData)((IData)(
                                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__illegalPC)
                                                                     ? 4U
                                                                     : 3U))) 
                                                   << 0x20U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U] 
                            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U]) 
                               | ((IData)((0x2aULL 
                                           | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                                              << 7U))) 
                                  << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[1U] 
                            = (((IData)((0x2aULL | 
                                         (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                                          << 7U))) 
                                >> 0x18U) | ((IData)(
                                                     ((0x2aULL 
                                                       | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                                                          << 7U)) 
                                                      >> 0x20U)) 
                                             << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[2U] 
                            = (0xfffU & (0x560U | ((IData)(
                                                           ((0x2aULL 
                                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__systemOp 
                                                                << 7U)) 
                                                            >> 0x20U)) 
                                                   >> 0x18U)));
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U] 
                            = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__opInfo[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__opInfo[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__opInfo[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__372__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__373__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__opInfo[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__opInfo[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__opInfo[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__374__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__374__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__374__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__374__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__374__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__374__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__374__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__371__microOps[7U];
                    }
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__376__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__376__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__376__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__376__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__376__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__377__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__378__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__378__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__378__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__378__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__378__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__378__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__378__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__375__microOps[7U];
                }
            } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    if ((1U == (IData)(vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__rv32mFunct7))) {
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__isf 
                            = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][2U] << 0xbU) | 
                               (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][1U] >> 0x15U));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__dstRegNum 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__isf 
                                        >> 7U));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__srcRegNumB 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__isf 
                                        >> 0x14U));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__srcRegNumA 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__isf 
                                        >> 0xfU));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__isf 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__isf;
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__isfR 
                            = vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__isf;
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__rv32mFunct3 
                            = (7U & (vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__isfR 
                                     >> 0xcU));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__isMul 
                            = (4U > (IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__rv32mFunct3));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[2U] 
                            = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[2U]);
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U] 
                            = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U] 
                            = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U] 
                            = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__dstRegNum) 
                                  << 0x1eU));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[2U] 
                            = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[2U]) 
                               | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__dstRegNum) 
                                            >> 2U)));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U] 
                            = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__srcRegNumA) 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U] 
                            = ((0xff81ffffU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U]) 
                               | (0xfffe0000U & (((IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__srcRegNumB) 
                                                  << 0x12U) 
                                                 | ((0U 
                                                     < (IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__rv32mFunct3)) 
                                                    << 0x11U))));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3 
                            = vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__rv32mFunct3;
                        if ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3))) {
                            if ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3))) {
                                if ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3))) {
                                    vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 0U;
                                    vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 3U;
                                } else {
                                    vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 0U;
                                    vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 2U;
                                }
                            } else if ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3))) {
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 0U;
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 1U;
                            } else {
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 0U;
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 0U;
                            }
                        } else if ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3))) {
                            if ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3))) {
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 3U;
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 0U;
                            } else {
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 2U;
                                vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 0U;
                            }
                        } else if ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3))) {
                            vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 1U;
                            vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 0U;
                        } else {
                            vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = 0U;
                            vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = 0U;
                        }
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__mulCode 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode;
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__divCode 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode;
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U] 
                            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U]) 
                               | ((IData)((QData)((IData)(
                                                          (2U 
                                                           | (0U 
                                                              != (IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__dstRegNum)))))) 
                                  << 8U));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U] 
                            = ((0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U]) 
                               | (((IData)((QData)((IData)(
                                                           (2U 
                                                            | (0U 
                                                               != (IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__dstRegNum)))))) 
                                   >> 0x18U) | ((IData)(
                                                        ((QData)((IData)(
                                                                         (2U 
                                                                          | (0U 
                                                                             != (IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__dstRegNum))))) 
                                                         >> 0x20U)) 
                                                << 8U)));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U] 
                            = ((0xfffe1fffU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U]) 
                               | (0xffffe000U & (((IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__mulCode) 
                                                  << 0xfU) 
                                                 | ((IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__divCode) 
                                                    << 0xdU))));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[2U] 
                            = ((0xfU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[2U]) 
                               | (0xff0U & (0x480U 
                                            | (((IData)(vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__isMul)
                                                 ? 0U
                                                 : 1U) 
                                               << 4U))));
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U] 
                            = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__complexOp[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__complexOp[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__complexOp[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitComplexOp__380__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__382__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hab51bb0b__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__complexOp[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__complexOp[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__complexOp[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__383__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__383__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__383__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__383__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__383__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__383__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__383__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeComplexOp__379__microOps[7U];
                    } else if ((0x10U == (IData)(vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zbaFunct7))) {
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__isf 
                            = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][2U] << 0xbU) | 
                               (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][1U] >> 0x15U));
                        __Vtask_RISCV_EmitZba__385__aluCode = 0;
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__dstRegNum 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeZba__384__isf 
                                        >> 7U));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__srcRegNumB 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeZba__384__isf 
                                        >> 0x14U));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__srcRegNumA 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeZba__384__isf 
                                        >> 0xfU));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__isf 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__isf;
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__isfR 
                            = vlSelfRef.__Vtask_RISCV_EmitZba__385__isf;
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3 
                            = (7U & (vlSelfRef.__Vtask_RISCV_EmitZba__385__isfR 
                                     >> 0xcU));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[2U] 
                            = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[2U]);
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U] 
                            = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U] 
                            = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U] 
                            = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__dstRegNum) 
                                  << 0x1eU));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[2U] 
                            = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[2U]) 
                               | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__dstRegNum) 
                                            >> 2U)));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U] 
                            = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__srcRegNumA) 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U] 
                            = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__srcRegNumB) 
                                  << 0x12U));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U] 
                            = (0x7fffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U] 
                            = (0x20e0U | (0xffffc000U 
                                          & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U]));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U] 
                            = ((0xfffc3fffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U]) 
                               | (((2U == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3))
                                    ? 0xaU : ((4U == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3))
                                               ? 0xbU
                                               : 0xcU)) 
                                  << 0xeU));
                        if ((1U & (~ VL_ONEHOT_I(((
                                                   (6U 
                                                    == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3)) 
                                                   << 2U) 
                                                  | (((4U 
                                                       == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3)) 
                                                      << 1U) 
                                                     | (2U 
                                                        == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3)))))))) {
                            if ((0U != (((6U == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3)) 
                                         << 2U) | (
                                                   ((4U 
                                                     == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3)) 
                                                    << 1U) 
                                                   | (2U 
                                                      == (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3)))))) {
                                if (VL_UNLIKELY((vlSymsp->_vm_contextp__->assertOn()))) {
                                    VL_WRITEF_NX("[%0t] %%Error: Decoder.sv:1552: Assertion failed in %N$unit.RISCV_EmitZba: unique case, but multiple matches found for '3'h%x'\n",0,
                                                 64,
                                                 VL_TIME_UNITED_Q(1000),
                                                 -9,
                                                 vlSymsp->name(),
                                                 3,
                                                 (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__zbaFunct3));
                                    VL_STOP_MT("Decoder/Decoder.sv", 1552, "");
                                }
                            }
                        }
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U] 
                            = ((0xffff80ffU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U]) 
                               | (0xffffff00U & (0x200U 
                                                 | ((0U 
                                                     != (IData)(vlSelfRef.__Vtask_RISCV_EmitZba__385__dstRegNum)) 
                                                    << 8U))));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[2U] 
                            = (0x400U | (0xfU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[2U]));
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U] 
                            = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__zbaOp[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__zbaOp[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__zbaOp[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitZba__385__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__386__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hacbf8ed4__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__zbaOp[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__zbaOp[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__zbaOp[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__387__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__387__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__387__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__387__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__387__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__387__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__387__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZba__384__microOps[7U];
                    } else if ((7U == (IData)(vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zicondFunct7))) {
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__isf 
                            = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][2U] << 0xbU) | 
                               (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][1U] >> 0x15U));
                        __Vtask_RISCV_EmitZicond__389__aluCode = 0;
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__dstRegNum 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeZicond__388__isf 
                                        >> 7U));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__srcRegNumB 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeZicond__388__isf 
                                        >> 0x14U));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__srcRegNumA 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeZicond__388__isf 
                                        >> 0xfU));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__isf 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__isf;
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__isfR 
                            = vlSelfRef.__Vtask_RISCV_EmitZicond__389__isf;
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__czeroFunct3 
                            = (7U & (vlSelfRef.__Vtask_RISCV_EmitZicond__389__isfR 
                                     >> 0xcU));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[2U] 
                            = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[2U]);
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U] 
                            = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U] 
                            = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U] 
                            = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__dstRegNum) 
                                  << 0x1eU));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[2U] 
                            = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[2U]) 
                               | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__dstRegNum) 
                                            >> 2U)));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U] 
                            = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__srcRegNumA) 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U] 
                            = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__srcRegNumB) 
                                  << 0x12U));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U] 
                            = ((0x7fffU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U]) 
                               | ((IData)((0x41c00000ULL 
                                           | ((QData)((IData)(
                                                              ((5U 
                                                                == (IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__czeroFunct3))
                                                                ? 8U
                                                                : 9U))) 
                                              << 0x1fU))) 
                                  << 0xfU));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U] 
                            = ((0xfffc0000U & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U]) 
                               | (((IData)((0x41c00000ULL 
                                            | ((QData)((IData)(
                                                               ((5U 
                                                                 == (IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__czeroFunct3))
                                                                 ? 8U
                                                                 : 9U))) 
                                               << 0x1fU))) 
                                   >> 0x11U) | ((IData)(
                                                        ((0x41c00000ULL 
                                                          | ((QData)((IData)(
                                                                             ((5U 
                                                                               == (IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__czeroFunct3))
                                                                               ? 8U
                                                                               : 9U))) 
                                                             << 0x1fU)) 
                                                         >> 0x20U)) 
                                                << 0xfU)));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U] 
                            = ((0xffff80ffU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U]) 
                               | (0xffffff00U & (0x200U 
                                                 | ((0U 
                                                     != (IData)(vlSelfRef.__Vtask_RISCV_EmitZicond__389__dstRegNum)) 
                                                    << 8U))));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[2U] 
                            = (0x400U | (0xfU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[2U]));
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U] 
                            = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__zicondOp[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__zicondOp[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__zicondOp[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitZicond__389__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__390__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h862f4532__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__zicondOp[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__zicondOp[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__zicondOp[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__391__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__391__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__391__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__391__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__391__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__391__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__391__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeZicond__388__microOps[7U];
                    } else {
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__isf 
                            = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][2U] << 0xbU) | 
                               (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                [1U][1U] >> 0x15U));
                        VL_ZERO_W(76, __Vtask_RISCV_DecodeOp__392__shiftOp);
                        VL_ZERO_W(76, __Vtask_RISCV_DecodeOp__392__rijOp);
                        VL_ZERO_W(76, __Vtask_RISCV_DecodeOp__392__selectOp);
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__dstRegNum 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeOp__392__isf 
                                        >> 7U));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__srcRegNumB 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeOp__392__isf 
                                        >> 0x14U));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__srcRegNumA 
                            = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeOp__392__isf 
                                        >> 0xfU));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__isf 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__isf;
                        vlSelf->__Vtask_RISCV_EmitOp__393__intOperandImmShift = VL_RAND_RESET_I(30);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__isfR 
                            = vlSelfRef.__Vtask_RISCV_EmitOp__393__isf;
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct3 
                            = (7U & (vlSelfRef.__Vtask_RISCV_EmitOp__393__isfR 
                                     >> 0xcU));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct7 
                            = (vlSelfRef.__Vtask_RISCV_EmitOp__393__isfR 
                               >> 0x19U);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__shiftFunct7 
                            = (vlSelfRef.__Vtask_RISCV_EmitOp__393__isfR 
                               >> 0x19U);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__isShift 
                            = ((1U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct3)) 
                               | (5U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct3)));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[2U] 
                            = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[2U]);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__dstRegNum) 
                                  << 0x1eU));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[2U] 
                            = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[2U]) 
                               | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__dstRegNum) 
                                            >> 2U)));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__srcRegNumA) 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__srcRegNumB) 
                                  << 0x12U));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = (0x2000U | vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift 
                            = ((0x1ffffffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift) 
                               | (((IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__isShift)
                                    ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitOp__393__isfR 
                                                >> 0x14U))
                                    : 0U) << 0x19U));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift 
                            = ((0x3e7fffffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift) 
                               | (((1U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct3))
                                    ? 0U : (((5U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct3)) 
                                             & (0U 
                                                == (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__shiftFunct7)))
                                             ? 1U : 
                                            (((5U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct3)) 
                                              & (0x20U 
                                                 == (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__shiftFunct7)))
                                              ? 2U : 3U))) 
                                  << 0x17U));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift 
                            = (0x400000U | (0x3f800000U 
                                            & vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U] 
                            = ((0x7fffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U]) 
                               | (vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift 
                                  << 0xfU));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = ((0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]) 
                               | (vlSelfRef.__Vtask_RISCV_EmitOp__393__intOperandImmShift 
                                  >> 0x11U));
                        vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct7 
                            = vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct7;
                        vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3 
                            = vlSelfRef.__Vtask_RISCV_EmitOp__393__opFunct3;
                        vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__aluCode 
                            = ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3))
                                ? ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3))
                                    ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3))
                                        ? 7U : 6U) : 
                                   ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3))
                                     ? 7U : 4U)) : 
                               ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3))
                                 ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3))
                                     ? 3U : 2U) : (
                                                   (1U 
                                                    & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct3))
                                                    ? 7U
                                                    : 
                                                   ((0U 
                                                     == (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__funct7))
                                                     ? 0U
                                                     : 1U))));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__aluCode 
                            = vlSelfRef.__Vtask_RISCV_DecodeOpFunct3__394__aluCode;
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U] 
                            = ((0xfffc3fffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U]) 
                               | ((IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__aluCode) 
                                  << 0xeU));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U] 
                            = ((0xffff80ffU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U]) 
                               | (0xffffff00U & (0x200U 
                                                 | ((0U 
                                                     != (IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__dstRegNum)) 
                                                    << 8U))));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[2U] 
                            = ((0xfU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[2U]) 
                               | (0xff0U & (0x400U 
                                            | (((IData)(vlSelfRef.__Vtask_RISCV_EmitOp__393__isShift)
                                                 ? 1U
                                                 : 0U) 
                                               << 4U))));
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U] 
                            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U] 
                            = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U] 
                            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U]);
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__intOp[0U] 
                            = vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__intOp[1U] 
                            = vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__intOp[2U] 
                            = vlSelfRef.__Vtask_RISCV_EmitOp__393__opInfo[2U];
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[0U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[1U] 
                            = vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[2U] 
                            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[2U]) 
                               | vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[2U]);
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[2U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[3U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U] 
                                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[4U]) 
                               | ((vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U] 
                                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[2U] 
                                                << 0xcU)));
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[1U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[2U] = 0U;
                        vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U] 
                            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U]);
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[0U];
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[1U];
                        vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[2U] 
                            = vlSelfRef.__Vtask_EmitInvalidOp__395__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[4U] 
                            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[4U]) 
                               | (vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U] 
                                  << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[5U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[0U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[6U] 
                            = ((vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[1U] 
                                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[2U] 
                                          << 0x18U));
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[7U] 
                            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h6d3762fc__0[2U] 
                                       >> 8U));
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__src[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__intOp[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__src[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__intOp[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__src[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__intOp[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__396__src[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__op[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__396__src[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__op[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__396__src[2U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U] 
                            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U] 
                            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U] 
                            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U]);
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[0U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__396__op[0U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[1U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__396__op[1U];
                        vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[2U] 
                            = vlSelfRef.__Vfunc_ModifyMicroOp__396__op[2U];
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[2U] 
                            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[2U]) 
                               | (vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[0U] 
                                  << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[3U] 
                            = ((vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[0U] 
                                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[1U] 
                                             << 0xcU));
                        vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[4U] 
                            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[4U]) 
                               | ((vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[1U] 
                                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__396__Vfuncout[2U] 
                                                << 0xcU)));
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[0U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[1U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[2U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[3U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[4U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[5U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[6U];
                        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                            = vlSelfRef.__Vtask_RISCV_DecodeOp__392__microOps[7U];
                    }
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__398__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__398__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__398__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__398__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__398__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__399__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__400__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__400__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__400__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__400__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__400__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__400__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__400__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__397__microOps[7U];
                }
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__402__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__402__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__402__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__402__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__402__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__403__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__404__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__404__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__404__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__404__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__404__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__404__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__404__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__404__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__401__microOps[7U];
            }
        } else if ((8U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__406__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__406__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__406__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__406__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__406__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__407__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__408__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__408__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__408__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__408__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__408__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__408__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__408__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__408__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__405__microOps[7U];
        } else if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__isf 
                        = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                            [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                 [1U][1U] 
                                                 >> 0x15U));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isf 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__isf;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfS 
                        = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isf;
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfI 
                        = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isf;
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__memFunct3 
                        = (7U & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfS 
                                 >> 0xcU));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad 
                        = (7U == (0x7fU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfI));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[2U] 
                        = (8U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[2U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = (0x800000U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]) 
                           | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad)
                                ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfI 
                                            >> 7U))
                                : 0U) << 0x1eU));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[2U] 
                        = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[2U]) 
                           | (0xfffU & (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad)
                                          ? (0x1fU 
                                             & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfI 
                                                >> 7U))
                                          : 0U) >> 2U)));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]) 
                           | (0x1f000000U & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfI 
                                             << 9U)));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]) 
                           | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad)
                                ? 0U : (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfS 
                                                 >> 0x14U))) 
                              << 0x12U));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U] 
                        = (0x7ffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = (0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U] 
                        = ((0xfffffeffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U]) 
                           | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U] 
                        = ((0xffff81ffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U]) 
                           | (0xfffffe00U & (0x200U 
                                             | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad)
                                                  ? 1U
                                                  : 0U) 
                                                << 0xbU))));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[2U] 
                        = ((0xfU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[2U]) 
                           | (0xff0U & (0x500U | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad)
                                                    ? 0U
                                                    : 1U) 
                                                  << 4U))));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U] 
                        = ((0xf8007fffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U]) 
                           | (0x7ff8000U & (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isLoad)
                                              ? (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfI 
                                                 >> 0x14U)
                                              : ((0xfe0U 
                                                  & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfS 
                                                     >> 0x14U)) 
                                                 | (0x1fU 
                                                    & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__isfS 
                                                       >> 7U)))) 
                                            << 0xfU)));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = (0x30000U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__funct3 
                        = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__memFunct3;
                    vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__memAccessMode 
                        = ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__funct3))
                            ? ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__funct3))
                                ? 4U : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__funct3))
                                         ? 1U : 0U))
                            : ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__funct3))
                                ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__funct3))
                                    ? 4U : 6U) : ((1U 
                                                   & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__funct3))
                                                   ? 5U
                                                   : 4U)));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__memAccessMode 
                        = vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__411__memAccessMode;
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U] 
                        = ((0xffff1fffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U]) 
                           | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__memAccessMode) 
                              << 0xdU));
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U] 
                        = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__memOp[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__memOp[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__memOp[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__410__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__412__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__memOp[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__memOp[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__memOp[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__413__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__413__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__413__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__413__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__413__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__413__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__413__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__409__microOps[7U];
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__415__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__415__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__415__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__415__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__415__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__416__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__417__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__417__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__417__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__417__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__417__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__417__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__417__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__414__microOps[7U];
                }
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__419__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__419__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__419__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__419__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__419__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__420__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__421__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__421__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__421__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__421__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__421__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__421__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__421__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__421__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__418__microOps[7U];
            }
        } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__isf 
                    = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                        [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                             [1U][1U] 
                                             >> 0x15U));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isf 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__isf;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfS 
                    = vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isf;
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfI 
                    = vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isf;
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__memFunct3 
                    = (7U & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfS 
                             >> 0xcU));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad 
                    = (3U == (0x7fU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfI));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[2U] 
                    = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[2U]);
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]) 
                       | (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad)
                            ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfI 
                                        >> 7U)) : 0U) 
                          << 0x1eU));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[2U] 
                    = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[2U]) 
                       | (0xfffU & (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad)
                                      ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfI 
                                                  >> 7U))
                                      : 0U) >> 2U)));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]) 
                       | (0x1f000000U & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfI 
                                         << 9U)));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]) 
                       | (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad)
                            ? 0U : (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfS 
                                             >> 0x14U))) 
                          << 0x12U));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U] 
                    = (0x7ffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = (0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U] 
                    = ((0xfffffeffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U]) 
                       | (((0U != (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfI 
                                            >> 7U))) 
                           & (IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad)) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U] 
                    = ((0xffff81ffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U]) 
                       | (0xfffffe00U & (0x200U | (
                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad)
                                                     ? 1U
                                                     : 0U) 
                                                   << 0xbU))));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[2U] 
                    = ((0xfU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[2U]) 
                       | (0xff0U & (0x500U | (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad)
                                                ? 0U
                                                : 1U) 
                                              << 4U))));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U] 
                    = ((0xf8007fffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U]) 
                       | (0x7ff8000U & (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isLoad)
                                          ? (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfI 
                                             >> 0x14U)
                                          : ((0xfe0U 
                                              & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfS 
                                                 >> 0x14U)) 
                                             | (0x1fU 
                                                & (vlSelfRef.__Vtask_RISCV_EmitMemOp__423__isfS 
                                                   >> 7U)))) 
                                        << 0xfU)));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = (0x30000U | vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__funct3 
                    = vlSelfRef.__Vtask_RISCV_EmitMemOp__423__memFunct3;
                vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__memAccessMode 
                    = ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__funct3))
                        ? ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__funct3))
                            ? 4U : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__funct3))
                                     ? 1U : 0U)) : 
                       ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__funct3))
                         ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__funct3))
                             ? 4U : 6U) : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__funct3))
                                            ? 5U : 4U)));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__memAccessMode 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__424__memAccessMode;
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U] 
                    = ((0xffff1fffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__423__memAccessMode) 
                          << 0xdU));
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U] 
                    = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__memOp[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__memOp[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__memOp[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitMemOp__423__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__425__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__426__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__memOp[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__memOp[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__memOp[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__426__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__426__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__426__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__426__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__426__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__426__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__426__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemOp__422__microOps[7U];
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__428__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__428__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__428__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__428__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__428__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__429__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__430__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__430__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__430__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__430__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__430__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__430__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__430__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__430__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__427__microOps[7U];
            }
        } else {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__432__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__432__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__432__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__432__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__432__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__433__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__434__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__434__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__434__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__434__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__434__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__434__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__434__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__434__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__431__microOps[7U];
        }
    } else if ((0x10U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
        if ((8U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__436__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__436__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__436__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__436__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__436__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__437__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__438__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__438__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__438__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__438__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__438__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__438__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__438__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__438__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__435__microOps[7U];
        } else if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__isf 
                        = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                            [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                 [1U][1U] 
                                                 >> 0x15U));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__dstRegNum 
                        = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__isf 
                                    >> 7U));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__isf 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__isf;
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__isfU 
                        = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__isf;
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[2U] 
                        = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[2U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]) 
                           | ((IData)(vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__dstRegNum) 
                              << 0x1eU));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[2U] 
                        = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[2U]) 
                           | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__dstRegNum) 
                                        >> 2U)));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = (0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = (0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = (0x2000U | vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__intOperandImmShift 
                        = (0x1c00003U | (0x3ffffcU 
                                         & (vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__isfU 
                                            >> 0xaU)));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U] 
                        = ((0x7fffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U]) 
                           | (vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__intOperandImmShift 
                              << 0xfU));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = ((0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]) 
                           | (vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__intOperandImmShift 
                              >> 0x11U));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U] 
                        = (0xfffc3fffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U] 
                        = ((0xfffffeffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U]) 
                           | ((0U != (IData)(vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__dstRegNum)) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U] 
                        = ((0xffff9fffU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U]) 
                           | (((0x17U == (0x7fU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__isf))
                                ? 2U : 0U) << 0xdU));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U] 
                        = (0xa00U | (0xffffe1ffU & 
                                     vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U]));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[2U] 
                        = (0x400U | (0xfU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[2U]));
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U] 
                        = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__intOp[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__intOp[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__intOp[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitUTypeInst__440__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__441__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h3f3210e9__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__intOp[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__intOp[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__intOp[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__442__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__442__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__442__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__442__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__442__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__442__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__442__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeUTypeInst__439__microOps[7U];
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__444__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__444__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__444__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__444__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__444__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__445__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__446__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__446__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__446__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__446__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__446__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__446__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__446__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__443__microOps[7U];
                }
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__448__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__448__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__448__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__448__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__448__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__449__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__450__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__450__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__450__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__450__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__450__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__450__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__450__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__450__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__447__microOps[7U];
            }
        } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__isf 
                    = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                        [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                             [1U][1U] 
                                             >> 0x15U));
                VL_ZERO_W(76, __Vtask_RISCV_DecodeOpImm__451__shiftOp);
                VL_ZERO_W(76, __Vtask_RISCV_DecodeOpImm__451__rijOp);
                VL_ZERO_W(76, __Vtask_RISCV_DecodeOpImm__451__selectOp);
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__dstRegNum 
                    = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__isf 
                                >> 7U));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__srcRegNumA 
                    = (0x1fU & (vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__isf 
                                >> 0xfU));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isf 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__isf;
                vlSelf->__Vtask_RISCV_EmitOpImm__452__intOperandImmShift = VL_RAND_RESET_I(30);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isfI 
                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isf;
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isfR 
                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isf;
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opFunct3 
                    = (7U & (vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isfR 
                             >> 0xcU));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__shiftFunct7 
                    = (vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isfR 
                       >> 0x19U);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isShift 
                    = ((1U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opFunct3)) 
                       | (5U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opFunct3)));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[2U] 
                    = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[2U]);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__dstRegNum) 
                          << 0x1eU));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[2U] 
                    = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[2U]) 
                       | (0xfffU & ((IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__dstRegNum) 
                                    >> 2U)));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__srcRegNumA) 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = (0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = (0xffffdfffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift 
                    = ((0x1ffffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift) 
                       | (((IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isShift)
                            ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isfR 
                                        >> 0x14U)) : 0U) 
                          << 0x19U));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift 
                    = ((0x3e7fffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift) 
                       | (((1U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opFunct3))
                            ? 0U : (((5U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opFunct3)) 
                                     & (0U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__shiftFunct7)))
                                     ? 1U : (((5U == (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opFunct3)) 
                                              & (0x20U 
                                                 == (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__shiftFunct7)))
                                              ? 2U : 3U))) 
                          << 0x17U));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift 
                    = (0x3fbfffffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift);
                VL_ASSIGNSEL_II(30,20,2U, vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift, 
                                ((IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isShift)
                                  ? ([&]() {
                                vlSelfRef.__Vfunc_ShamtExtention__453__isfR 
                                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isfR;
                                vlSelfRef.__Vfunc_ShamtExtention__453__Vfuncout 
                                    = (0x1fU & (vlSelfRef.__Vfunc_ShamtExtention__453__isfR 
                                                >> 0x14U));
                            }(), vlSelfRef.__Vfunc_ShamtExtention__453__Vfuncout)
                                  : ([&]() {
                                vlSelfRef.__Vfunc_I_TypeImmExtention__454__isfI 
                                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isfI;
                                vlSelfRef.__Vfunc_I_TypeImmExtention__454__Vfuncout 
                                    = ((0xff000U & 
                                        ((- (IData)(
                                                    (vlSelfRef.__Vfunc_I_TypeImmExtention__454__isfI 
                                                     >> 0x1fU))) 
                                         << 0xcU)) 
                                       | (vlSelfRef.__Vfunc_I_TypeImmExtention__454__isfI 
                                          >> 0x14U));
                            }(), vlSelfRef.__Vfunc_I_TypeImmExtention__454__Vfuncout)));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift 
                    = (1U | (0x3ffffffcU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U] 
                    = ((0x7fffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U]) 
                       | (vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift 
                          << 0xfU));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = ((0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]) 
                       | (vlSelfRef.__Vtask_RISCV_EmitOpImm__452__intOperandImmShift 
                          >> 0x11U));
                vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3 
                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opFunct3;
                vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__aluCode 
                    = ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3))
                        ? ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3))
                            ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3))
                                ? 7U : 6U) : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3))
                                               ? 7U
                                               : 4U))
                        : ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3))
                            ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3))
                                ? 3U : 2U) : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__funct3))
                                               ? 7U
                                               : 0U)));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__aluCode 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImmFunct3__455__aluCode;
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U] 
                    = ((0xfffc3fffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__aluCode) 
                          << 0xeU));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U] 
                    = ((0xffff80ffU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U]) 
                       | (0xffffff00U & (0xa00U | (
                                                   (0U 
                                                    != (IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__dstRegNum)) 
                                                   << 8U))));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[2U] 
                    = ((0xfU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[2U]) 
                       | (0xff0U & (0x400U | (((IData)(vlSelfRef.__Vtask_RISCV_EmitOpImm__452__isShift)
                                                ? 1U
                                                : 0U) 
                                              << 4U))));
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U] 
                    = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__intOp[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__intOp[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__intOp[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitOpImm__452__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__456__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h584fbd87__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__457__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__intOp[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__intOp[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__intOp[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__457__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__457__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__457__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__457__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__457__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__457__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__457__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeOpImm__451__microOps[7U];
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__459__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__459__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__459__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__459__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__459__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__460__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__461__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__461__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__461__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__461__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__461__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__461__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__461__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__461__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__458__microOps[7U];
            }
        } else {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__463__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__463__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__463__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__463__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__463__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__464__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__465__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__465__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__465__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__465__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__465__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__465__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__465__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__465__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__462__microOps[7U];
        }
    } else if ((8U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
        if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__isf 
                        = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                            [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                                 [1U][1U] 
                                                 >> 0x15U));
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__isf 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__isf;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__isfMiscMem 
                        = vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__isf;
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opFunct3 
                        = (7U & (vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__isfMiscMem 
                                 >> 0xcU));
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = (0x1fff07ffffffffULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = (0x1ffffe00000000ULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                        = ((0x1ffff9ffffffffULL & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp) 
                           | ((QData)((IData)(((0U 
                                                == (IData)(vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opFunct3))
                                                ? 2U
                                                : (
                                                   (1U 
                                                    == (IData)(vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opFunct3))
                                                    ? 3U
                                                    : 0U)))) 
                              << 0x21U));
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[2U] 
                        = (0xfffU & (0x550U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U] 
                        = ((0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U]) 
                           | (0x80U & ((~ (IData)((vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp 
                                                   >> 0x22U))) 
                                       << 7U)));
                    vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__miscMemOp[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__miscMemOp[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__miscMemOp[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitMiscMemOp__467__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__468__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hbda3652c__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__miscMemOp[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__miscMemOp[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__miscMemOp[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__469__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__469__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__469__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__469__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__469__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__469__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__469__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeMiscMem__466__microOps[7U];
                } else {
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__illegalPC 
                        = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                        [1U];
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__illegalPC 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__illegalPC;
                    __Vtask_RISCV_EmitIllegalOp__471__isfSystem = 0;
                    __Vtask_RISCV_EmitIllegalOp__471__opFunct3 = 0;
                    __Vtask_RISCV_EmitIllegalOp__471__opFunct12 = 0;
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                    vlSelf->__Vtask_RISCV_EmitIllegalOp__471__systemOp = VL_RAND_RESET_Q(53);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                        = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                        = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                        = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                        = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                        = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                        = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp) 
                           | (0x80000000ULL | ((QData)((IData)(
                                                               ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__illegalPC)
                                                                 ? 4U
                                                                 : 3U))) 
                                               << 0x20U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U] 
                        = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U]) 
                           | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                                                  << 7U))) 
                              << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[1U] 
                        = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                                                << 7U))) 
                            >> 0x18U) | ((IData)(((0x2aULL 
                                                   | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                                                      << 7U)) 
                                                  >> 0x20U)) 
                                         << 8U));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[2U] 
                        = (0xfffU & (0x560U | ((IData)(
                                                       ((0x2aULL 
                                                         | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__systemOp 
                                                            << 7U)) 
                                                        >> 0x20U)) 
                                               >> 0x18U)));
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U] 
                        = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U] 
                        = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U] 
                        = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U]);
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__opInfo[0U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__opInfo[1U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__opInfo[2U] 
                        = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__471__opInfo[2U];
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[0U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[1U] 
                        = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[2U] 
                        = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[2U]) 
                           | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[2U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[3U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[4U]) 
                           | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                               >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                            << 0xcU)));
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[1U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[2U] = 0U;
                    vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U] 
                        = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U]);
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[0U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[1U];
                    vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                        = vlSelfRef.__Vtask_EmitInvalidOp__472__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[4U] 
                        = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[4U]) 
                           | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                              << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[5U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[6U] 
                        = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                            >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                      << 0x18U));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[7U] 
                        = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                   >> 8U));
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__src[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__opInfo[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__src[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__opInfo[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__src[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__opInfo[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__473__src[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__op[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__473__src[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__op[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__473__src[2U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U] 
                        = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U] 
                        = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U] 
                        = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U]);
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[0U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__473__op[0U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[1U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__473__op[1U];
                    vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[2U] 
                        = vlSelfRef.__Vfunc_ModifyMicroOp__473__op[2U];
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[2U] 
                        = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[2U]) 
                           | (vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[0U] 
                              << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[3U] 
                        = ((vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[0U] 
                            >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[1U] 
                                         << 0xcU));
                    vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[4U] 
                        = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[4U]) 
                           | ((vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[1U] 
                               >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__473__Vfuncout[2U] 
                                            << 0xcU)));
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[0U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[1U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[2U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[3U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[4U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[5U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[6U];
                    vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                        = vlSelfRef.__Vtask_RISCV_DecodeIllegal__470__microOps[7U];
                }
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__475__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__475__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__475__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__475__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__475__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__476__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__477__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__477__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__477__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__477__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__477__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__477__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__477__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__477__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__474__microOps[7U];
            }
        } else {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__479__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__479__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__479__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__479__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__479__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__480__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__481__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__481__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__481__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__481__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__481__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__481__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__481__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__481__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__478__microOps[7U];
        }
    } else if ((4U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
        if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__isf 
                    = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                        [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                             [1U][1U] 
                                             >> 0x15U));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isf 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__isf;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfS 
                    = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isf;
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfI 
                    = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isf;
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__memFunct3 
                    = (7U & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfS 
                             >> 0xcU));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad 
                    = (7U == (0x7fU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfI));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[2U] 
                    = (8U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[2U]);
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = (0x800000U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]) 
                       | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad)
                            ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfI 
                                        >> 7U)) : 0U) 
                          << 0x1eU));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[2U] 
                    = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[2U]) 
                       | (0xfffU & (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad)
                                      ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfI 
                                                  >> 7U))
                                      : 0U) >> 2U)));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]) 
                       | (0x1f000000U & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfI 
                                         << 9U)));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]) 
                       | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad)
                            ? 0U : (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfS 
                                             >> 0x14U))) 
                          << 0x12U));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U] 
                    = (0x7ffffffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = (0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U] 
                    = ((0xfffffeffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U] 
                    = ((0xffff81ffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U]) 
                       | (0xfffffe00U & (0x200U | (
                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad)
                                                     ? 1U
                                                     : 0U) 
                                                   << 0xbU))));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[2U] 
                    = ((0xfU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[2U]) 
                       | (0xff0U & (0x500U | (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad)
                                                ? 0U
                                                : 1U) 
                                              << 4U))));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U] 
                    = ((0xf8007fffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U]) 
                       | (0x7ff8000U & (((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isLoad)
                                          ? (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfI 
                                             >> 0x14U)
                                          : ((0xfe0U 
                                              & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfS 
                                                 >> 0x14U)) 
                                             | (0x1fU 
                                                & (vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__isfS 
                                                   >> 7U)))) 
                                        << 0xfU)));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = (0x30000U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]);
                vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__funct3 
                    = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__memFunct3;
                vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__memAccessMode 
                    = ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__funct3))
                        ? ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__funct3))
                            ? 4U : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__funct3))
                                     ? 1U : 0U)) : 
                       ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__funct3))
                         ? ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__funct3))
                             ? 4U : 6U) : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__funct3))
                                            ? 5U : 4U)));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__memAccessMode 
                    = vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__484__memAccessMode;
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U] 
                    = ((0xffff1fffU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U]) 
                       | ((IData)(vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__memAccessMode) 
                          << 0xdU));
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U] 
                    = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__memOp[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__memOp[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__memOp[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitFPMemOp__483__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__485__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hc3737b4c__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__486__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__memOp[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__memOp[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__memOp[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__486__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__486__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__486__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__486__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__486__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__486__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__486__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeFPMemOp__482__microOps[7U];
            } else {
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__illegalPC 
                    = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                    [1U];
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__illegalPC 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__illegalPC;
                __Vtask_RISCV_EmitIllegalOp__488__isfSystem = 0;
                __Vtask_RISCV_EmitIllegalOp__488__opFunct3 = 0;
                __Vtask_RISCV_EmitIllegalOp__488__opFunct12 = 0;
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
                vlSelf->__Vtask_RISCV_EmitIllegalOp__488__systemOp = VL_RAND_RESET_Q(53);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                    = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                    = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                    = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                    = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                    = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                    = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp) 
                       | (0x80000000ULL | ((QData)((IData)(
                                                           ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__illegalPC)
                                                             ? 4U
                                                             : 3U))) 
                                           << 0x20U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U] 
                    = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U]) 
                       | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                                              << 7U))) 
                          << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[1U] 
                    = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                                            << 7U))) 
                        >> 0x18U) | ((IData)(((0x2aULL 
                                               | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                                                  << 7U)) 
                                              >> 0x20U)) 
                                     << 8U));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[2U] 
                    = (0xfffU & (0x560U | ((IData)(
                                                   ((0x2aULL 
                                                     | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__systemOp 
                                                        << 7U)) 
                                                    >> 0x20U)) 
                                           >> 0x18U)));
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U] 
                    = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U] 
                    = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U] 
                    = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U]);
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__opInfo[0U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__opInfo[1U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__opInfo[2U] 
                    = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__488__opInfo[2U];
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[0U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[1U] 
                    = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[2U] 
                    = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[2U]) 
                       | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[2U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[3U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[4U]) 
                       | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                           >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                        << 0xcU)));
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[1U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[2U] = 0U;
                vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U] 
                    = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U]);
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[0U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[1U];
                vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                    = vlSelfRef.__Vtask_EmitInvalidOp__489__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[4U] 
                    = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[4U]) 
                       | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                          << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[5U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[6U] 
                    = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                        >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                  << 0x18U));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[7U] 
                    = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                               >> 8U));
                vlSelfRef.__Vfunc_ModifyMicroOp__490__src[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__opInfo[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__src[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__opInfo[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__src[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__opInfo[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__490__src[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__op[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__490__src[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__op[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__490__src[2U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U] 
                    = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U] 
                    = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U] 
                    = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U]);
                vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[0U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__490__op[0U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[1U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__490__op[1U];
                vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[2U] 
                    = vlSelfRef.__Vfunc_ModifyMicroOp__490__op[2U];
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[2U] 
                    = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[2U]) 
                       | (vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[0U] 
                          << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[3U] 
                    = ((vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[0U] 
                        >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[1U] 
                                     << 0xcU));
                vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[4U] 
                    = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[4U]) 
                       | ((vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[1U] 
                           >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__490__Vfuncout[2U] 
                                        << 0xcU)));
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[0U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[1U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[2U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[3U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[4U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[5U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[6U];
                vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                    = vlSelfRef.__Vtask_RISCV_DecodeIllegal__487__microOps[7U];
            }
        } else {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__492__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__492__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__492__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__492__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__492__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__493__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__494__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__494__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__494__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__494__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__494__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__494__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__494__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__494__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__491__microOps[7U];
        }
    } else if ((2U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
        if ((1U & vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf)) {
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__isf 
                = ((vlSelfRef.__PVT__pdStage__DOT__pipeReg
                    [1U][2U] << 0xbU) | (vlSelfRef.__PVT__pdStage__DOT__pipeReg
                                         [1U][1U] >> 0x15U));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isf 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__isf;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 0U;
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfS 
                = vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isf;
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfI 
                = vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isf;
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__memFunct3 
                = (7U & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfS 
                         >> 0xcU));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad 
                = (3U == (0x7fU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfI));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[2U] 
                = (0xff7U & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[2U]);
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = (0xdfffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]);
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = (0xff7fffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]);
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = ((0x3fffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]) 
                   | (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad)
                        ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfI 
                                    >> 7U)) : 0U) << 0x1eU));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[2U] 
                = ((0xff8U & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[2U]) 
                   | (0xfffU & (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad)
                                  ? (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfI 
                                              >> 7U))
                                  : 0U) >> 2U)));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = ((0xe0ffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]) 
                   | (0x1f000000U & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfI 
                                     << 9U)));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = ((0xff83ffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]) 
                   | (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad)
                        ? 0U : (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfS 
                                         >> 0x14U))) 
                      << 0x12U));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U] 
                = (0x7ffffffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = (0xffffe000U & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]);
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U] 
                = ((0xfffffeffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U]) 
                   | (((0U != (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfI 
                                        >> 7U))) & (IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad)) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U] 
                = ((0xffff81ffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U]) 
                   | (0xfffffe00U & (0x200U | (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad)
                                                 ? 1U
                                                 : 0U) 
                                               << 0xbU))));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[2U] 
                = ((0xfU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[2U]) 
                   | (0xff0U & (0x500U | (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad)
                                            ? 0U : 1U) 
                                          << 4U))));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U] 
                = ((0xf8007fffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U]) 
                   | (0x7ff8000U & (((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isLoad)
                                      ? (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfI 
                                         >> 0x14U) : 
                                     ((0xfe0U & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfS 
                                                 >> 0x14U)) 
                                      | (0x1fU & (vlSelfRef.__Vtask_RISCV_EmitMemOp__496__isfS 
                                                  >> 7U)))) 
                                    << 0xfU)));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = (0x30000U | vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]);
            vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__funct3 
                = vlSelfRef.__Vtask_RISCV_EmitMemOp__496__memFunct3;
            vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__memAccessMode 
                = ((4U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__funct3))
                    ? ((2U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__funct3))
                        ? 4U : ((1U & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__funct3))
                                 ? 1U : 0U)) : ((2U 
                                                 & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__funct3))
                                                 ? 
                                                ((1U 
                                                  & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__funct3))
                                                  ? 4U
                                                  : 6U)
                                                 : 
                                                ((1U 
                                                  & (IData)(vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__funct3))
                                                  ? 5U
                                                  : 4U)));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__memAccessMode 
                = vlSelfRef.__Vtask_RISCV_DecodeMemAccessMode__497__memAccessMode;
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U] 
                = ((0xffff1fffU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U]) 
                   | ((IData)(vlSelfRef.__Vtask_RISCV_EmitMemOp__496__memAccessMode) 
                      << 0xdU));
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U] 
                = (0xfffffffeU & vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__memOp[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__memOp[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__memOp[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitMemOp__496__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__498__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_h3a7cc217__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__499__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__memOp[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__memOp[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__memOp[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__499__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__499__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__499__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__499__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__499__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__499__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__499__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeMemOp__495__microOps[7U];
        } else {
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__illegalPC 
                = vlSelfRef.__PVT__pdStage__DOT__illegalPC
                [1U];
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__illegalPC 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__illegalPC;
            __Vtask_RISCV_EmitIllegalOp__501__isfSystem = 0;
            __Vtask_RISCV_EmitIllegalOp__501__opFunct3 = 0;
            __Vtask_RISCV_EmitIllegalOp__501__opFunct12 = 0;
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
            vlSelf->__Vtask_RISCV_EmitIllegalOp__501__systemOp = VL_RAND_RESET_Q(53);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp) 
                   | (0x80000000ULL | ((QData)((IData)(
                                                       ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__illegalPC)
                                                         ? 4U
                                                         : 3U))) 
                                       << 0x20U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U] 
                = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U]) 
                   | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                                          << 7U))) 
                      << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[1U] 
                = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                                        << 7U))) >> 0x18U) 
                   | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                                           << 7U)) 
                               >> 0x20U)) << 8U));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[2U] 
                = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                                 | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__systemOp 
                                                    << 7U)) 
                                                >> 0x20U)) 
                                       >> 0x18U)));
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U] 
                = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U] 
                = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U] 
                = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U]);
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__opInfo[0U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__opInfo[1U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__opInfo[2U] 
                = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__501__opInfo[2U];
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[0U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[1U] 
                = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[2U] 
                = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[2U]) 
                   | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[2U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[3U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[4U]) 
                   | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                       >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                    << 0xcU)));
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[1U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[2U] = 0U;
            vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U] 
                = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U]);
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[0U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[1U];
            vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                = vlSelfRef.__Vtask_EmitInvalidOp__502__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[4U] 
                = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[4U]) 
                   | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                      << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[5U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[6U] 
                = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                    >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                              << 0x18U));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[7U] 
                = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                           >> 8U));
            vlSelfRef.__Vfunc_ModifyMicroOp__503__src[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__opInfo[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__src[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__opInfo[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__src[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__opInfo[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__503__src[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__op[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__503__src[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__op[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__503__src[2U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U] 
                = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U] 
                = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U] 
                = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U]);
            vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[0U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__503__op[0U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[1U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__503__op[1U];
            vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[2U] 
                = vlSelfRef.__Vfunc_ModifyMicroOp__503__op[2U];
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[2U] 
                = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[2U]) 
                   | (vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[0U] 
                      << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[3U] 
                = ((vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[0U] 
                    >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[1U] 
                                 << 0xcU));
            vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[4U] 
                = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[4U]) 
                   | ((vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[1U] 
                       >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__503__Vfuncout[2U] 
                                    << 0xcU)));
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[0U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[1U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[2U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[3U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[4U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[5U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[6U];
            vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
                = vlSelfRef.__Vtask_RISCV_DecodeIllegal__500__microOps[7U];
        }
    } else {
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__illegalPC 
            = vlSelfRef.__PVT__pdStage__DOT__illegalPC
            [1U];
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__illegalPC 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__illegalPC;
        __Vtask_RISCV_EmitIllegalOp__505__isfSystem = 0;
        __Vtask_RISCV_EmitIllegalOp__505__opFunct3 = 0;
        __Vtask_RISCV_EmitIllegalOp__505__opFunct12 = 0;
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
        vlSelf->__Vtask_RISCV_EmitIllegalOp__505__systemOp = VL_RAND_RESET_Q(53);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
            = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
            = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
            = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
            = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
            = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
            = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp) 
               | (0x80000000ULL | ((QData)((IData)(
                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__illegalPC)
                                                     ? 4U
                                                     : 3U))) 
                                   << 0x20U)));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U] 
            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U]) 
               | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
                                      << 7U))) << 8U));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[1U] 
            = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
                                    << 7U))) >> 0x18U) 
               | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
                                       << 7U)) >> 0x20U)) 
                  << 8U));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[2U] 
            = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__systemOp 
                                                << 7U)) 
                                            >> 0x20U)) 
                                   >> 0x18U)));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U] 
            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U] 
            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U] 
            = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__opInfo[0U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[0U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__opInfo[1U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[1U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__opInfo[2U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__505__opInfo[2U];
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[0U] 
            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[1U] 
            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[2U] 
            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[2U]) 
               | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[2U] 
            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[2U]) 
               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                  << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[3U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                             << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[4U] 
            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[4U]) 
               | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                << 0xcU)));
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__506__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[4U] 
            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[4U]) 
               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                  << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[5U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                          << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[6U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                          << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[7U] 
            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                       >> 8U));
        vlSelfRef.__Vfunc_ModifyMicroOp__507__src[0U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__opInfo[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__src[1U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__opInfo[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__src[2U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__opInfo[2U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__507__src[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__op[1U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__507__src[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__op[2U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__507__src[2U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U] 
            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U] 
            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U] 
            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[0U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__507__op[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[1U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__507__op[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[2U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__507__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[2U] 
            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[2U]) 
               | (vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[0U] 
                  << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[3U] 
            = ((vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[0U] 
                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[1U] 
                             << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[4U] 
            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[4U]) 
               | ((vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[1U] 
                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__507__Vfuncout[2U] 
                                << 0xcU)));
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[0U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[1U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[2U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[3U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[4U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[5U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[6U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__504__microOps[7U];
    }
    vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined = 0U;
    if ((IData)((0U != (0xc0U & vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U])))) {
        vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined = 1U;
    }
    if ((IData)((0U != (0xc0000U & vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U])))) {
        vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined = 1U;
    }
    if ((IData)((0U != (0xc0000000U & vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U])))) {
        vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined = 1U;
    }
    if (((IData)(vlSelfRef.__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined) 
         | vlSelfRef.__PVT__pdStage__DOT__illegalPC
         [1U])) {
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__illegalPC 
            = vlSelfRef.__PVT__pdStage__DOT__illegalPC
            [1U];
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__illegalPC 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__illegalPC;
        __Vtask_RISCV_EmitIllegalOp__509__isfSystem = 0;
        __Vtask_RISCV_EmitIllegalOp__509__opFunct3 = 0;
        __Vtask_RISCV_EmitIllegalOp__509__opFunct12 = 0;
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = 1U;
        vlSelf->__Vtask_RISCV_EmitIllegalOp__509__systemOp = VL_RAND_RESET_Q(53);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
            = (0xfffffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
            = (0x1fbfffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
            = (0x1ffeffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
            = (0x107fffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
            = (0x1fc1ffffffffffULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
            = ((0x1fff0000000000ULL & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp) 
               | (0x80000000ULL | ((QData)((IData)(
                                                   ((IData)(vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__illegalPC)
                                                     ? 4U
                                                     : 3U))) 
                                   << 0x20U)));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U] 
            = ((0xffU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U]) 
               | ((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
                                      << 7U))) << 8U));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[1U] 
            = (((IData)((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
                                    << 7U))) >> 0x18U) 
               | ((IData)(((0x2aULL | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
                                       << 7U)) >> 0x20U)) 
                  << 8U));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[2U] 
            = (0xfffU & (0x560U | ((IData)(((0x2aULL 
                                             | (vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__systemOp 
                                                << 7U)) 
                                            >> 0x20U)) 
                                   >> 0x18U)));
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U] 
            = (0x10U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U] 
            = (0xffffff3fU & vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U] 
            = (1U | vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U]);
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__opInfo[0U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[0U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__opInfo[1U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[1U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__opInfo[2U] 
            = vlSelfRef.__Vtask_RISCV_EmitIllegalOp__509__opInfo[2U];
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[0U] 
            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[1U] 
            = vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[2U] 
            = ((0xfffff000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[2U]) 
               | vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U]);
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[2U] 
            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[2U]) 
               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                  << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[3U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                             << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[4U] 
            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[4U]) 
               | ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                   >> 0x14U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                                << 0xcU)));
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[1U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[2U] = 0U;
        vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U] 
            = (0xffffffefU & vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U]);
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[0U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[1U];
        vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
            = vlSelfRef.__Vtask_EmitInvalidOp__510__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[4U] 
            = ((0xffffffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[4U]) 
               | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                  << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[5U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[0U] 
                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                          << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[6U] 
            = ((vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[1U] 
                >> 8U) | (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                          << 0x18U));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[7U] 
            = (0xfU & (vlSymsp->TOP____024unit.__Vlvbound_hdb6811b6__0[2U] 
                       >> 8U));
        vlSelfRef.__Vfunc_ModifyMicroOp__511__src[0U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__opInfo[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__src[1U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__opInfo[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__src[2U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__opInfo[2U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__511__src[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__op[1U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__511__src[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__op[2U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__511__src[2U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U] 
            = (0xfffffff9U & vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U] 
            = (0xffffffdfU & vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U] 
            = (8U | vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U]);
        vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[0U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__511__op[0U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[1U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__511__op[1U];
        vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[2U] 
            = vlSelfRef.__Vfunc_ModifyMicroOp__511__op[2U];
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[2U] 
            = ((0xfffU & vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[2U]) 
               | (vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[0U] 
                  << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[3U] 
            = ((vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[0U] 
                >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[1U] 
                             << 0xcU));
        vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[4U] 
            = ((0xff000000U & vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[4U]) 
               | ((vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[1U] 
                   >> 0x14U) | (vlSelfRef.__Vfunc_ModifyMicroOp__511__Vfuncout[2U] 
                                << 0xcU)));
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[0U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[0U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[1U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[1U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[2U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[2U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[3U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[3U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[4U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[4U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[5U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[5U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[6U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[6U];
        vlSelfRef.pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps[7U] 
            = vlSelfRef.__Vtask_RISCV_DecodeIllegal__508__microOps[7U];
    }
    vlSelfRef.__PVT__idStage__DOT__flushTriggered = 0U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS = 0U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__popRAS = 0U;
    vlSelfRef.__PVT__idStage__DOT__insnValidOut[0U] 
        = vlSelfRef.__PVT__idStage__DOT__insnValidIn
        [0U];
    vlSelfRef.__PVT__idStage__DOT__insnFlushed[0U] = 0U;
    vlSelfRef.__PVT__idStage__DOT__insnFlushTriggering[0U] = 0U;
    vlSelfRef.__PVT__idStage__DOT__brPredOut[0U] = 
        (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__idStage__DOT__brPredIn[1U])) 
                            << 0x20U) | (QData)((IData)(
                                                        vlSelfRef.__PVT__idStage__DOT__brPredIn[0U]))));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU[0U] 
        = (IData)(vlSelfRef.__PVT__idStage__DOT__isfIn);
    vlSelfRef.__PVT__idStage__DOT__insnValidOut[1U] 
        = vlSelfRef.__PVT__idStage__DOT__insnValidIn
        [1U];
    vlSelfRef.__PVT__idStage__DOT__insnFlushed[1U] = 0U;
    vlSelfRef.__PVT__idStage__DOT__insnFlushTriggering[1U] = 0U;
    vlSelfRef.__PVT__idStage__DOT__brPredOut[1U] = 
        (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__idStage__DOT__brPredIn[2U])) 
                            << 0x1fU) | ((QData)((IData)(
                                                         vlSelfRef.__PVT__idStage__DOT__brPredIn[1U])) 
                                         >> 1U)));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU[1U] 
        = (IData)((vlSelfRef.__PVT__idStage__DOT__isfIn 
                   >> 0x20U));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk2__DOT__i = 2U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck = 0U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrIncorrect = 0U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane = 0U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType[0U] 
        = ((2U & (IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo))
            ? 1U : ((0x10U & (IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo))
                     ? 2U : ((1U & (IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo))
                              ? 3U : 0U)));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType[1U] 
        = ((0x40U & (IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo))
            ? 1U : ((0x200U & (IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo))
                     ? 2U : ((0x20U & (IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo))
                              ? 3U : 0U)));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk3__DOT__i = 2U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)) {
            if ((1U & (~ vlSelfRef.__PVT__idStage__DOT__insnValidIn
                       [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)]))) {
                goto __Vlabel152;
            }
            if (((~ ((9U >= ((IData)(4U) + (0xfU & 
                                            ((IData)(5U) 
                                             * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)))) 
                     && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo) 
                               >> ((IData)(4U) + (0xfU 
                                                  & ((IData)(5U) 
                                                     * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i))))))) 
                 & ((0x41U >= ((IData)(0xcU) + (0x7fU 
                                                & ((IData)(0x21U) 
                                                   * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)))) 
                    && (1U & (vlSelfRef.__PVT__idStage__DOT__brPredIn[
                              (((IData)(0xcU) + (0x7fU 
                                                 & ((IData)(0x21U) 
                                                    * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i))) 
                               >> 5U)] >> (0x1fU & 
                                           ((IData)(0xcU) 
                                            + (0x7fU 
                                               & ((IData)(0x21U) 
                                                  * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i))))))))) {
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane 
                    = (1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i);
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck = 1U;
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrIncorrect = 1U;
                goto __Vlabel152;
            } else if (((1U == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                         [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)]) 
                        & ((0x6fU == (0x7fU & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                      [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)])) 
                           | ((0x41U >= ((IData)(0xcU) 
                                         + (0x7fU & 
                                            ((IData)(0x21U) 
                                             * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)))) 
                              && (1U & (vlSelfRef.__PVT__idStage__DOT__brPredIn[
                                        (((IData)(0xcU) 
                                          + (0x7fU 
                                             & ((IData)(0x21U) 
                                                * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i))) 
                                         >> 5U)] >> 
                                        (0x1fU & ((IData)(0xcU) 
                                                  + 
                                                  (0x7fU 
                                                   & ((IData)(0x21U) 
                                                      * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)))))))))) {
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS 
                    = ((9U >= ((IData)(3U) + (0xfU 
                                              & ((IData)(5U) 
                                                 * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)))) 
                       && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo) 
                                 >> ((IData)(3U) + 
                                     (0xfU & ((IData)(5U) 
                                              * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i))))));
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane 
                    = (1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i);
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck = 1U;
                goto __Vlabel152;
            } else if ((2U == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                        [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)])) {
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS 
                    = ((9U >= ((IData)(3U) + (0xfU 
                                              & ((IData)(5U) 
                                                 * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)))) 
                       && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo) 
                                 >> ((IData)(3U) + 
                                     (0xfU & ((IData)(5U) 
                                              * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i))))));
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__popRAS 
                    = ((9U >= ((IData)(2U) + (0xfU 
                                              & ((IData)(5U) 
                                                 * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)))) 
                       && (1U & ((IData)(vlSelfRef.__PVT__idStage__DOT__insnInfo) 
                                 >> ((IData)(2U) + 
                                     (0xfU & ((IData)(5U) 
                                              * vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i))))));
                if (vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__popRAS) {
                    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane 
                        = (1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i);
                    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck = 1U;
                    goto __Vlabel152;
                } else if (vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS) {
                    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane 
                        = (1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i);
                    goto __Vlabel152;
                }
            } else if ((3U == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                        [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i)])) {
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane 
                    = (1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i);
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck = 1U;
                vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrIncorrect = 1U;
                goto __Vlabel152;
            }
            vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i);
        }
        __Vlabel152: ;
    }
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC[0U] 
        = (0xfffffU & ((IData)(4U) + vlSelfRef.__PVT__idStage__DOT__pcIn
                       [0U]));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC[0U] 
        = (0xfffffU & ((1U == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                        [0U]) ? ((0x6fU == (0x7fU & 
                                            vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                            [0U])) ? 
                                 (vlSelfRef.__PVT__idStage__DOT__pcIn
                                  [0U] + ([&]() {
                            vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                = vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                [0U];
                            vlSelfRef.__Vfunc_GetJAL_Target__515__Vfuncout 
                                = ((0x80000U & (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                                >> 0xcU)) 
                                   | ((0x7f800U & (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                                   >> 1U)) 
                                      | ((0x400U & 
                                          (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                           >> 0xaU)) 
                                         | (0x3ffU 
                                            & (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                               >> 0x15U)))));
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__brDisp 
                                = vlSelfRef.__Vfunc_GetJAL_Target__515__Vfuncout;
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__Vfuncout 
                                = (((- (IData)((1U 
                                                & (vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__brDisp 
                                                   >> 0x13U)))) 
                                    << 0x15U) | (vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__brDisp 
                                                 << 1U));
                        }(), vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__Vfuncout))
                                  : (vlSelfRef.__PVT__idStage__DOT__pcIn
                                     [0U] + ([&]() {
                            vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                = vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                [0U];
                            vlSelfRef.__Vfunc_GetBranchDisplacement__517__Vfuncout 
                                = (((0xff800U & ((- (IData)(
                                                            (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                             >> 0x1fU))) 
                                                 << 0xbU)) 
                                    | (0x400U & (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                 << 3U))) 
                                   | ((0x3f0U & (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                 >> 0x15U)) 
                                      | (0xfU & (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                 >> 8U))));
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__brDisp 
                                = vlSelfRef.__Vfunc_GetBranchDisplacement__517__Vfuncout;
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__Vfuncout 
                                = (((- (IData)((1U 
                                                & (vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__brDisp 
                                                   >> 0x13U)))) 
                                    << 0x15U) | (vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__brDisp 
                                                 << 1U));
                        }(), vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__Vfuncout)))
                        : ((2U == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                            [0U]) ? vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                           [vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr]
                            : vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                           [0U])));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC[1U] 
        = (0xfffffU & ((IData)(4U) + vlSelfRef.__PVT__idStage__DOT__pcIn
                       [1U]));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC[1U] 
        = (0xfffffU & ((1U == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                        [1U]) ? ((0x6fU == (0x7fU & 
                                            vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                            [1U])) ? 
                                 (vlSelfRef.__PVT__idStage__DOT__pcIn
                                  [1U] + ([&]() {
                            vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                = vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                [1U];
                            vlSelfRef.__Vfunc_GetJAL_Target__515__Vfuncout 
                                = ((0x80000U & (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                                >> 0xcU)) 
                                   | ((0x7f800U & (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                                   >> 1U)) 
                                      | ((0x400U & 
                                          (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                           >> 0xaU)) 
                                         | (0x3ffU 
                                            & (vlSelfRef.__Vfunc_GetJAL_Target__515__isfJAL 
                                               >> 0x15U)))));
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__brDisp 
                                = vlSelfRef.__Vfunc_GetJAL_Target__515__Vfuncout;
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__Vfuncout 
                                = (((- (IData)((1U 
                                                & (vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__brDisp 
                                                   >> 0x13U)))) 
                                    << 0x15U) | (vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__brDisp 
                                                 << 1U));
                        }(), vlSelfRef.__Vfunc_ExtendBranchDisplacement__514__Vfuncout))
                                  : (vlSelfRef.__PVT__idStage__DOT__pcIn
                                     [1U] + ([&]() {
                            vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                = vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU
                                [1U];
                            vlSelfRef.__Vfunc_GetBranchDisplacement__517__Vfuncout 
                                = (((0xff800U & ((- (IData)(
                                                            (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                             >> 0x1fU))) 
                                                 << 0xbU)) 
                                    | (0x400U & (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                 << 3U))) 
                                   | ((0x3f0U & (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                 >> 0x15U)) 
                                      | (0xfU & (vlSelfRef.__Vfunc_GetBranchDisplacement__517__isfBr 
                                                 >> 8U))));
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__brDisp 
                                = vlSelfRef.__Vfunc_GetBranchDisplacement__517__Vfuncout;
                            vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__Vfuncout 
                                = (((- (IData)((1U 
                                                & (vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__brDisp 
                                                   >> 0x13U)))) 
                                    << 0x15U) | (vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__brDisp 
                                                 << 1U));
                        }(), vlSelfRef.__Vfunc_ExtendBranchDisplacement__516__Vfuncout)))
                        : ((2U == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType
                            [1U]) ? vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras
                           [vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr]
                            : vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
                           [1U])));
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk5__DOT__i = 2U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch[0U] 
        = ((0xfffffU & ((vlSelfRef.__PVT__idStage__DOT__brPredIn[1U] 
                         << 0x13U) | (vlSelfRef.__PVT__idStage__DOT__brPredIn[0U] 
                                      >> 0xdU))) != 
           vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
           [0U]);
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch[1U] 
        = ((0xfffffU & ((vlSelfRef.__PVT__idStage__DOT__brPredIn[2U] 
                         << 0x12U) | (vlSelfRef.__PVT__idStage__DOT__brPredIn[1U] 
                                      >> 0xeU))) != 
           vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
           [1U]);
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk6__DOT__i = 2U;
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i)) {
            if ((((vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch
                   [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i)] 
                   & (IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck)) 
                  | (IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrIncorrect)) 
                 & ((IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane) 
                    == vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i))) {
                vlSelfRef.__PVT__idStage__DOT__flushTriggered = 1U;
                vlSelfRef.__PVT__idStage__DOT__brPredOut[(1U 
                                                          & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i)] 
                    = ((0x1fffULL & vlSelfRef.__PVT__idStage__DOT__brPredOut
                        [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i)]) 
                       | ((QData)((IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
                                          [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i)])) 
                          << 0xdU));
                vlSelfRef.__PVT__idStage__DOT__brPredOut[(1U 
                                                          & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i)] 
                    = (0x1000ULL | vlSelfRef.__PVT__idStage__DOT__brPredOut
                       [(1U & vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i)]);
                goto __Vlabel153;
            }
            vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i 
                = ((IData)(1U) + vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i);
        }
        __Vlabel153: ;
    }
    vlSelfRef.__PVT__idStage__DOT__recoveredPC = vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC
        [vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane];
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS 
        = vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC
        [vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane];
    vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS_Ptr 
        = (3U & ((IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS)
                  ? ((IData)(1U) + (IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr))
                  : ((IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__popRAS)
                      ? ((IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr) 
                         - (IData)(1U)) : (IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr))));
    if (vlSelfRef.__PVT__idStage__DOT__flushTriggered) {
        if ((1U > (IData)(vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane))) {
            vlSelfRef.__PVT__idStage__DOT__insnValidOut[1U] = 0U;
            vlSelfRef.__PVT__idStage__DOT__insnFlushed[1U] = 1U;
        }
        vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk8__DOT__i = 2U;
        vlSelfRef.__PVT__idStage__DOT__insnFlushTriggering[vlSelfRef.__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane] = 1U;
    }
    idStage__DOT____Vlvbound_hf1753291__0 = (1U & vlSelfRef.__PVT__idStage__DOT__microOps[0U]);
    vlSelfRef.__PVT__idStage__DOT__serializedMOps = 
        ((0x3eU & (IData)(vlSelfRef.__PVT__idStage__DOT__serializedMOps)) 
         | (IData)(idStage__DOT____Vlvbound_hf1753291__0));
    idStage__DOT____Vlvbound_hf1753291__0 = (1U & (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[2U] 
                                                   >> 0xcU));
    vlSelfRef.__PVT__idStage__DOT__serializedMOps = 
        ((0x3dU & (IData)(vlSelfRef.__PVT__idStage__DOT__serializedMOps)) 
         | ((IData)(idStage__DOT____Vlvbound_hf1753291__0) 
            << 1U));
    idStage__DOT____Vlvbound_hf1753291__0 = (1U & (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[4U] 
                                                   >> 0x18U));
    vlSelfRef.__PVT__idStage__DOT__serializedMOps = 
        ((0x3bU & (IData)(vlSelfRef.__PVT__idStage__DOT__serializedMOps)) 
         | ((IData)(idStage__DOT____Vlvbound_hf1753291__0) 
            << 2U));
    idStage__DOT____Vlvbound_hf1753291__0 = (1U & (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[7U] 
                                                   >> 4U));
    vlSelfRef.__PVT__idStage__DOT__serializedMOps = 
        ((0x37U & (IData)(vlSelfRef.__PVT__idStage__DOT__serializedMOps)) 
         | ((IData)(idStage__DOT____Vlvbound_hf1753291__0) 
            << 3U));
    idStage__DOT____Vlvbound_hf1753291__0 = (1U & (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[9U] 
                                                   >> 0x10U));
    vlSelfRef.__PVT__idStage__DOT__serializedMOps = 
        ((0x2fU & (IData)(vlSelfRef.__PVT__idStage__DOT__serializedMOps)) 
         | ((IData)(idStage__DOT____Vlvbound_hf1753291__0) 
            << 4U));
    idStage__DOT____Vlvbound_hf1753291__0 = (1U & (
                                                   vlSelfRef.__PVT__idStage__DOT__microOps[0xbU] 
                                                   >> 0x1cU));
    vlSelfRef.__PVT__idStage__DOT__serializedMOps = 
        ((0x1fU & (IData)(vlSelfRef.__PVT__idStage__DOT__serializedMOps)) 
         | ((IData)(idStage__DOT____Vlvbound_hf1753291__0) 
            << 5U));
    if (vlSelfRef.__PVT__idStage__DOT__initiate) {
        vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0 
            = (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[0U] 
                     >> 4U));
        vlSelfRef.__PVT__idStage__DOT__curValidMOps 
            = ((0x3eU & (IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps)) 
               | (IData)(vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0));
        vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0 
            = (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[2U] 
                     >> 0x10U));
        vlSelfRef.__PVT__idStage__DOT__curValidMOps 
            = ((0x3dU & (IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps)) 
               | ((IData)(vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0) 
                  << 1U));
        vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0 
            = (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[4U] 
                     >> 0x1cU));
        vlSelfRef.__PVT__idStage__DOT__curValidMOps 
            = ((0x3bU & (IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps)) 
               | ((IData)(vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0) 
                  << 2U));
        vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0 
            = (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[7U] 
                     >> 8U));
        vlSelfRef.__PVT__idStage__DOT__curValidMOps 
            = ((0x37U & (IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps)) 
               | ((IData)(vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0) 
                  << 3U));
        vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0 
            = (1U & (vlSelfRef.__PVT__idStage__DOT__microOps[9U] 
                     >> 0x14U));
        vlSelfRef.__PVT__idStage__DOT__curValidMOps 
            = ((0x2fU & (IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps)) 
               | ((IData)(vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0) 
                  << 4U));
        vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0 
            = (1U & vlSelfRef.__PVT__idStage__DOT__microOps[0xcU]);
        vlSelfRef.__PVT__idStage__DOT__curValidMOps 
            = ((0x1fU & (IData)(vlSelfRef.__PVT__idStage__DOT__curValidMOps)) 
               | ((IData)(vlSelfRef.idStage__DOT____Vlvbound_h1adb7724__0) 
                  << 5U));
    } else {
        vlSelfRef.__PVT__idStage__DOT__curValidMOps 
            = vlSelfRef.__PVT__idStage__DOT__remainingValidMOps;
    }
    vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut[0U][0U] 
        = vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
        [0U][0U];
    vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut[0U][1U] 
        = vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
        [0U][1U];
    vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut[1U][0U] 
        = vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
        [1U][0U];
    vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut[1U][1U] 
        = vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOutTmp
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayDataOut[0U][0U] 
        = (0x7ffU & vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut
           [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayValidOut[0U][0U] 
        = (1U & (vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut
                 [0U][0U] >> 0xbU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayDataOut[1U][0U] 
        = (0x7ffU & vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut
           [1U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__tagArrayValidOut[1U][0U] 
        = (1U & (vlSelfRef.__PVT__dCache__DOT__array__DOT__tagArrayOut
                 [1U][0U] >> 0xbU));
}
