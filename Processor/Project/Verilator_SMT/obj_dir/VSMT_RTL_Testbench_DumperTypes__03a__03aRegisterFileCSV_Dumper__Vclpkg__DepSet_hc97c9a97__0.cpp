// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg.h"

void VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_Open(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, std::string fileName) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_Open\n"); );
    // Body
    this->__PVT__m_file = VL_FOPEN_NN(VL_CVT_PACK_STR_NN(fileName)
                                      , std::string{"w"});
    ;
    this->__PVT__m_cycle = 0xffffffffU;
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_Close(VSMT_RTL_Testbench__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_Close\n"); );
    // Body
    VL_FCLOSE_I(this->__PVT__m_file); }

void VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_ProceedCycle(VSMT_RTL_Testbench__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_ProceedCycle\n"); );
    // Body
    this->__PVT__m_cycle = ((IData)(1U) + this->__PVT__m_cycle);
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_Dump(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, IData/*31:0*/ pc, VlUnpacked<IData/*31:0*/, 64> regData) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::__VnoInFunc_Dump\n"); );
    // Body
    IData/*31:0*/ unnamedblk1__DOT__i;
    unnamedblk1__DOT__i = 0;
    VL_FWRITEF_NX(this->__PVT__m_file,"%11d,0x%04x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,0x%-x,",0,
                  32,this->__PVT__m_cycle,32,pc,32,
                  regData[0U],32,regData[1U],32,regData
                  [2U],32,regData[3U],32,regData[4U],
                  32,regData[5U],32,regData[6U],32,
                  regData[7U],32,regData[8U],32,regData
                  [9U],32,regData[0xaU],32,regData[0xbU],
                  32,regData[0xcU],32,regData[0xdU],
                  32,regData[0xeU],32,regData[0xfU],
                  32,regData[0x10U],32,regData[0x11U],
                  32,regData[0x12U],32,regData[0x13U],
                  32,regData[0x14U],32,regData[0x15U],
                  32,regData[0x16U],32,regData[0x17U],
                  32,regData[0x18U],32,regData[0x19U],
                  32,regData[0x1aU],32,regData[0x1bU],
                  32,regData[0x1cU],32,regData[0x1dU],
                  32,regData[0x1eU],32,regData[0x1fU],
                  32,regData[0x20U],32,regData[0x21U],
                  32,regData[0x22U],32,regData[0x23U],
                  32,regData[0x24U],32,regData[0x25U],
                  32,regData[0x26U],32,regData[0x27U],
                  32,regData[0x28U],32,regData[0x29U],
                  32,regData[0x2aU],32,regData[0x2bU],
                  32,regData[0x2cU],32,regData[0x2dU],
                  32,regData[0x2eU],32,regData[0x2fU],
                  32,regData[0x30U],32,regData[0x31U],
                  32,regData[0x32U],32,regData[0x33U],
                  32,regData[0x34U],32,regData[0x35U],
                  32,regData[0x36U],32,regData[0x37U],
                  32,regData[0x38U],32,regData[0x39U],
                  32,regData[0x3aU],32,regData[0x3bU],
                  32,regData[0x3cU],32,regData[0x3dU],
                  32,regData[0x3eU],32,regData[0x3fU]);
    unnamedblk1__DOT__i = 0x40U;
    VL_FWRITEF_NX(this->__PVT__m_file,"\n",0);
}

std::string VL_TO_STRING(const VlClassRef<VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper>& obj) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::VL_TO_STRING\n"); );
    // Body
    return (obj ? obj->to_string() : "null");
}

std::string VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::to_string() const {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::to_string\n"); );
    // Body
    return ("'{"s + to_string_middle() + "}");
}

std::string VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::to_string_middle() const {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper::to_string_middle\n"); );
    // Body
    std::string out;
    out += "m_file:" + VL_TO_STRING(__PVT__m_file);
    out += ", m_cycle:" + VL_TO_STRING(__PVT__m_cycle);
    return out;
}
