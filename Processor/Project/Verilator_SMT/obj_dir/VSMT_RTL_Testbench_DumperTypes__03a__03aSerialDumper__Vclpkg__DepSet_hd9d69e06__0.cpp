// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg.h"

void VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::__VnoInFunc_Init(VSMT_RTL_Testbench__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::__VnoInFunc_Init\n"); );
    // Body
    this->__PVT__m_str = std::string{};
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::__VnoInFunc_CheckSignal(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, CData/*0:0*/ we, IData/*31:0*/ data, IData/*31:0*/ showOutput) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::__VnoInFunc_CheckSignal\n"); );
    // Body
    CData/*7:0*/ ch;
    ch = 0;
    ch = (0xffU & data);
    if (((IData)(we) & (0U != (IData)(ch)))) {
        if (VL_UNLIKELY(((0U != showOutput)))) {
            VL_WRITEF_NX("%c",0,8,ch);
        }
        VL_SFORMAT_NX(64,this->__PVT__m_str,"%@%c",0,
                      -1,&(this->__PVT__m_str),8,(IData)(ch));
    }
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::__VnoInFunc_DumpToFile(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, std::string fileName) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::__VnoInFunc_DumpToFile\n"); );
    // Body
    this->__PVT__m_file = VL_FOPEN_NN(VL_CVT_PACK_STR_NN(fileName)
                                      , std::string{"w"});
    ;
    VL_FWRITEF_NX(this->__PVT__m_file,"%@",0,-1,&(this->__PVT__m_str));
    VL_FCLOSE_I(this->__PVT__m_file); }

std::string VL_TO_STRING(const VlClassRef<VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper>& obj) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::VL_TO_STRING\n"); );
    // Body
    return (obj ? obj->to_string() : "null");
}

std::string VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::to_string() const {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::to_string\n"); );
    // Body
    return ("'{"s + to_string_middle() + "}");
}

std::string VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::to_string_middle() const {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper::to_string_middle\n"); );
    // Body
    std::string out;
    out += "m_file:" + VL_TO_STRING(__PVT__m_file);
    out += ", m_str:" + VL_TO_STRING(__PVT__m_str);
    return out;
}
