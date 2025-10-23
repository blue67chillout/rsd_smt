// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg.h"

void VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_Open(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, std::string fileName) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_Open\n"); );
    // Body
    this->__PVT__m_cycle = 0xffffffffU;
    this->__PVT__m_retireID = 1U;
    this->__PVT__m_file = VL_FOPEN_NN(VL_CVT_PACK_STR_NN(fileName)
                                      , std::string{"w"});
    ;
    VL_FWRITEF_NX(this->__PVT__m_file,"RSD_Kanata\t0000\n#\tS:\n#\tstage_id\tvalid\tstall\tclear\tiid\tmid\n#\tL:\n#\tiid\tmid\tpc\tcode\n",0);
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_Close(VSMT_RTL_Testbench__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_Close\n"); );
    // Body
    VL_FCLOSE_I(this->__PVT__m_file); }

void VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_ProceedCycle(VSMT_RTL_Testbench__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_ProceedCycle\n"); );
    // Body
    this->__PVT__m_cycle = ((IData)(1U) + this->__PVT__m_cycle);
    VL_FWRITEF_NX(this->__PVT__m_file,"C\t          1\n#\tcycle:%0d\n",0,
                  32,this->__PVT__m_cycle);
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_DumpStage(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, IData/*31:0*/ stage, CData/*0:0*/ valid, CData/*0:0*/ stall, CData/*0:0*/ clear, IData/*31:0*/ sid, IData/*31:0*/ mid, std::string str) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_DumpStage\n"); );
    // Body
    if (VL_UNLIKELY((valid))) {
        VL_FWRITEF_NX(this->__PVT__m_file,"S\t%0d\t%0#\t%0#\t%0#\t%0d\t%0d\t%@\n",0,
                      32,stage,1,(IData)(valid),1,stall,
                      1,(IData)(clear),32,sid,32,mid,
                      -1,&(str));
    }
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_DumpInsnCode(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, IData/*31:0*/ sid, IData/*31:0*/ mid, IData/*31:0*/ pc, IData/*31:0*/ insn) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_DumpInsnCode\n"); );
    // Body
    VL_FWRITEF_NX(this->__PVT__m_file,"L\t%0d\t%0d\t%x\t%x\n",0,
                  32,sid,32,mid,32,pc,32,insn);
}

void VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_DumpCycle(VSMT_RTL_Testbench__Syms* __restrict vlSymsp, VlWide<92>/*2929:0*/ debugRegister) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::__VnoInFunc_DumpCycle\n"); );
    // Init
    VlWide<4>/*127:0*/ __Vtemp_4;
    VlWide<4>/*127:0*/ __Vtemp_8;
    // Body
    IData/*31:0*/ unnamedblk1__DOT__i;
    unnamedblk1__DOT__i = 0;
    IData/*31:0*/ unnamedblk2__DOT__i;
    unnamedblk2__DOT__i = 0;
    IData/*31:0*/ unnamedblk3__DOT__i;
    unnamedblk3__DOT__i = 0;
    IData/*31:0*/ unnamedblk4__DOT__i;
    unnamedblk4__DOT__i = 0;
    IData/*31:0*/ unnamedblk5__DOT__i;
    unnamedblk5__DOT__i = 0;
    IData/*31:0*/ unnamedblk6__DOT__i;
    unnamedblk6__DOT__i = 0;
    IData/*31:0*/ unnamedblk7__DOT__i;
    unnamedblk7__DOT__i = 0;
    IData/*31:0*/ unnamedblk8__DOT__i;
    unnamedblk8__DOT__i = 0;
    IData/*31:0*/ unnamedblk9__DOT__i;
    unnamedblk9__DOT__i = 0;
    IData/*31:0*/ unnamedblk10__DOT__i;
    unnamedblk10__DOT__i = 0;
    IData/*31:0*/ unnamedblk11__DOT__i;
    unnamedblk11__DOT__i = 0;
    IData/*31:0*/ unnamedblk12__DOT__i;
    unnamedblk12__DOT__i = 0;
    IData/*31:0*/ unnamedblk13__DOT__i;
    unnamedblk13__DOT__i = 0;
    IData/*31:0*/ unnamedblk14__DOT__i;
    unnamedblk14__DOT__i = 0;
    IData/*31:0*/ unnamedblk15__DOT__i;
    unnamedblk15__DOT__i = 0;
    IData/*31:0*/ unnamedblk16__DOT__i;
    unnamedblk16__DOT__i = 0;
    IData/*31:0*/ unnamedblk16__DOT__unnamedblk17__DOT__j;
    unnamedblk16__DOT__unnamedblk17__DOT__j = 0;
    IData/*31:0*/ unnamedblk18__DOT__i;
    unnamedblk18__DOT__i = 0;
    IData/*31:0*/ unnamedblk19__DOT__i;
    unnamedblk19__DOT__i = 0;
    IData/*31:0*/ unnamedblk20__DOT__i;
    unnamedblk20__DOT__i = 0;
    IData/*31:0*/ unnamedblk21__DOT__i;
    unnamedblk21__DOT__i = 0;
    IData/*31:0*/ unnamedblk22__DOT__i;
    unnamedblk22__DOT__i = 0;
    IData/*31:0*/ unnamedblk23__DOT__i;
    unnamedblk23__DOT__i = 0;
    IData/*31:0*/ unnamedblk24__DOT__i;
    unnamedblk24__DOT__i = 0;
    std::string str;
    std::string strAluCode;
    std::string strOpType;
    this->__VnoInFunc_ProceedCycle(vlSymsp);
    this->__VnoInFunc_DumpStage(vlSymsp, 0U, (1U & 
                                              (debugRegister[0x5bU] 
                                               >> 6U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x19U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x18U)), 
                                (0x3ffU & ((debugRegister[0x5bU] 
                                            << 4U) 
                                           | (debugRegister[0x5aU] 
                                              >> 0x1cU))), 0U, 
                                std::string{});
    unnamedblk1__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 0U, (1U & 
                                              (debugRegister[0x5bU] 
                                               >> 0x11U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x19U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x18U)), 
                                (0x3ffU & (debugRegister[0x5bU] 
                                           >> 7U)), 0U, 
                                std::string{});
    unnamedblk1__DOT__i = 2U;
    if ((4U & debugRegister[0x5aU])) {
        __Vtemp_4[0U] = 0x73735c6eU;
        __Vtemp_4[1U] = 0x652d6d69U;
        __Vtemp_4[2U] = 0x63616368U;
        __Vtemp_4[3U] = 0x692dU;
    } else {
        __Vtemp_4[0U] = 0U;
        __Vtemp_4[1U] = 0U;
        __Vtemp_4[2U] = 0U;
        __Vtemp_4[3U] = 0U;
    }
    str = VL_CVT_PACK_STR_NW(4, __Vtemp_4);
    this->__VnoInFunc_DumpStage(vlSymsp, 1U, (1U & 
                                              (debugRegister[0x5aU] 
                                               >> 0xeU)), 
                                (1U & ((debugRegister[7U] 
                                        >> 0x17U) & 
                                       (~ (debugRegister[0x5aU] 
                                           >> 3U)))), 
                                (1U & ((debugRegister[7U] 
                                        >> 0x16U) | 
                                       (debugRegister[0x5aU] 
                                        >> 3U))), (0x3ffU 
                                                   & (debugRegister[0x5aU] 
                                                      >> 4U)), 0U, str);
    unnamedblk2__DOT__i = 1U;
    if ((0x8000U & debugRegister[0x5aU])) {
        __Vtemp_8[0U] = 0x73735c6eU;
        __Vtemp_8[1U] = 0x652d6d69U;
        __Vtemp_8[2U] = 0x63616368U;
        __Vtemp_8[3U] = 0x692dU;
    } else {
        __Vtemp_8[0U] = 0U;
        __Vtemp_8[1U] = 0U;
        __Vtemp_8[2U] = 0U;
        __Vtemp_8[3U] = 0U;
    }
    str = VL_CVT_PACK_STR_NW(4, __Vtemp_8);
    this->__VnoInFunc_DumpStage(vlSymsp, 1U, (1U & 
                                              (debugRegister[0x5aU] 
                                               >> 0x1bU)), 
                                (1U & ((debugRegister[7U] 
                                        >> 0x17U) & 
                                       (~ (debugRegister[0x5aU] 
                                           >> 0x10U)))), 
                                (1U & ((debugRegister[7U] 
                                        >> 0x16U) | 
                                       (debugRegister[0x5aU] 
                                        >> 0x10U))), 
                                (0x3ffU & (debugRegister[0x5aU] 
                                           >> 0x11U)), 0U, str);
    unnamedblk2__DOT__i = 2U;
    strAluCode = VL_SFORMATF_N_NX("%0b",0,32,(0xfU 
                                              & (debugRegister[0x59U] 
                                                 >> 1U))) ;
    strOpType = VL_SFORMATF_N_NX("%0b",0,32,(7U & (
                                                   (debugRegister[0x59U] 
                                                    << 2U) 
                                                   | (debugRegister[0x58U] 
                                                      >> 0x1eU)))) ;
    this->__VnoInFunc_DumpStage(vlSymsp, 2U, (1U & 
                                              (debugRegister[0x59U] 
                                               >> 0xfU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x15U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x14U)), 
                                (0x3ffU & (debugRegister[0x59U] 
                                           >> 5U)), 0U, 
                                VL_CVT_PACK_STR_NN(
                                                   VL_CONCATN_NNN(
                                                                  VL_CONCATN_NNN(
                                                                                VL_CONCATN_NNN(
                                                                                VL_CONCATN_NNN(
                                                                                std::string{"optype:0b"}, strOpType), 
                                                                                std::string{" ALU-code:0b"}), strAluCode), 
                                                                  std::string{"\\n"})));
    unnamedblk3__DOT__i = 1U;
    strAluCode = VL_SFORMATF_N_NX("%0b",0,32,(0xfU 
                                              & (debugRegister[0x59U] 
                                                 >> 0x13U))) ;
    strOpType = VL_SFORMATF_N_NX("%0b",0,32,(7U & (
                                                   debugRegister[0x59U] 
                                                   >> 0x10U))) ;
    this->__VnoInFunc_DumpStage(vlSymsp, 2U, (1U & 
                                              (debugRegister[0x5aU] 
                                               >> 1U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x15U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x14U)), 
                                (0x3ffU & ((debugRegister[0x5aU] 
                                            << 9U) 
                                           | (debugRegister[0x59U] 
                                              >> 0x17U))), 0U, 
                                VL_CVT_PACK_STR_NN(
                                                   VL_CONCATN_NNN(
                                                                  VL_CONCATN_NNN(
                                                                                VL_CONCATN_NNN(
                                                                                VL_CONCATN_NNN(
                                                                                std::string{"optype:0b"}, strOpType), 
                                                                                std::string{" ALU-code:0b"}), strAluCode), 
                                                                  std::string{"\\n"})));
    unnamedblk3__DOT__i = 2U;
    str = std::string{};
    if ((0x20000000U & debugRegister[0x53U])) {
        str = std::string{"An undefined instruction is decoded."};
    }
    if ((0x10000000U & debugRegister[0x53U])) {
        str = std::string{"An unsupported instruction is decoded."};
    }
    if ((0x400U & debugRegister[0x56U])) {
        str = std::string{"Br-pred-miss-id\\n"};
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 3U, (1U & 
                                              (debugRegister[0x56U] 
                                               >> 0xcU)), (IData)(
                                                                  (0x80000U 
                                                                   == 
                                                                   (0x80200U 
                                                                    & debugRegister[7U]))), 
                                (1U & ((debugRegister[7U] 
                                        >> 0x12U) | 
                                       (debugRegister[0x56U] 
                                        >> 0xbU))), 
                                (0x3ffU & debugRegister[0x56U]), 
                                (debugRegister[0x55U] 
                                 >> 0x1eU), str);
    unnamedblk4__DOT__i = 1U;
    str = std::string{};
    if ((0x4000U & debugRegister[0x56U])) {
        str = std::string{"An undefined instruction is decoded."};
    }
    if ((0x2000U & debugRegister[0x56U])) {
        str = std::string{"An unsupported instruction is decoded."};
    }
    if ((0x8000000U & debugRegister[0x58U])) {
        str = std::string{"Br-pred-miss-id\\n"};
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 3U, (1U & 
                                              (debugRegister[0x58U] 
                                               >> 0x1dU)), (IData)(
                                                                   (0x80000U 
                                                                    == 
                                                                    (0x80200U 
                                                                     & debugRegister[7U]))), 
                                (1U & ((debugRegister[7U] 
                                        >> 0x12U) | 
                                       (debugRegister[0x58U] 
                                        >> 0x1cU))), 
                                (0x3ffU & (debugRegister[0x58U] 
                                           >> 0x11U)), 
                                (3U & (debugRegister[0x58U] 
                                       >> 0xfU)), str);
    unnamedblk4__DOT__i = 2U;
    if ((0x1000U & debugRegister[0x56U])) {
        this->__VnoInFunc_DumpInsnCode(vlSymsp, (0x3ffU 
                                                 & debugRegister[0x56U]), 
                                       (debugRegister[0x55U] 
                                        >> 0x1eU), 
                                       ((debugRegister[0x55U] 
                                         << 2U) | (
                                                   debugRegister[0x54U] 
                                                   >> 0x1eU)), 
                                       ((debugRegister[0x54U] 
                                         << 2U) | (
                                                   debugRegister[0x53U] 
                                                   >> 0x1eU)));
    }
    unnamedblk5__DOT__i = 1U;
    if ((0x20000000U & debugRegister[0x58U])) {
        this->__VnoInFunc_DumpInsnCode(vlSymsp, (0x3ffU 
                                                 & (debugRegister[0x58U] 
                                                    >> 0x11U)), 
                                       (3U & (debugRegister[0x58U] 
                                              >> 0xfU)), 
                                       ((debugRegister[0x58U] 
                                         << 0x11U) 
                                        | (debugRegister[0x57U] 
                                           >> 0xfU)), 
                                       ((debugRegister[0x57U] 
                                         << 0x11U) 
                                        | (debugRegister[0x56U] 
                                           >> 0xfU)));
    }
    unnamedblk5__DOT__i = 2U;
    this->__VnoInFunc_DumpStage(vlSymsp, 4U, (1U & 
                                              (debugRegister[0x53U] 
                                               >> 0xeU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x11U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x10U)), 
                                (0x3ffU & (debugRegister[0x53U] 
                                           >> 4U)), 
                                (3U & (debugRegister[0x53U] 
                                       >> 2U)), std::string{});
    unnamedblk6__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 4U, (1U & 
                                              (debugRegister[0x53U] 
                                               >> 0x1bU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x11U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0x10U)), 
                                (0x3ffU & (debugRegister[0x53U] 
                                           >> 0x11U)), 
                                (3U & (debugRegister[0x53U] 
                                       >> 0xfU)), std::string{});
    unnamedblk6__DOT__i = 2U;
    str = std::string{};
    str = std::string{"map: "};
    if ((0x100000U & debugRegister[0x4eU])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & (debugRegister[0x4eU] 
                                         >> 0xeU)),
                      7,(0x7fU & (debugRegister[0x4eU] 
                                  >> 7U)));
    }
    VL_SFORMAT_NX(64,str,"%@ = ",0,-1,&(str));
    if ((0x40000000U & debugRegister[0x4fU])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & (debugRegister[0x4fU] 
                                         >> 0x18U)),
                      7,(0x7fU & (debugRegister[0x4fU] 
                                  >> 0x11U)));
    }
    if ((0x10000U & debugRegister[0x4fU])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & (debugRegister[0x4fU] 
                                         >> 0xaU)),
                      7,(0x7fU & (debugRegister[0x4fU] 
                                  >> 3U)));
    }
    VL_SFORMAT_NX(64,str,"%@\\nprev: ",0,-1,&(str));
    if ((0x100000U & debugRegister[0x4eU])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & (debugRegister[0x4eU] 
                                         >> 0xeU)),
                      7,(0x7fU & debugRegister[0x4eU]));
    }
    VL_SFORMAT_NX(64,str,"%@\\nAL alloc: %0# ",0,-1,
                  &(str),6,(debugRegister[0x4dU] >> 0x1aU));
    VL_SFORMAT_NX(64,str,"%@\\nIQ alloc: %0# ",0,-1,
                  &(str),4,(0xfU & (debugRegister[0x4dU] 
                                    >> 0x16U)));
    this->__VnoInFunc_DumpStage(vlSymsp, 5U, (1U & 
                                              (debugRegister[0x50U] 
                                               >> 0xbU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xfU)), (1U 
                                                   & (debugRegister[7U] 
                                                      >> 0xeU)), 
                                (0x3ffU & (debugRegister[0x50U] 
                                           >> 1U)), 
                                (3U & ((debugRegister[0x50U] 
                                        << 1U) | (debugRegister[0x4fU] 
                                                  >> 0x1fU))), str);
    unnamedblk7__DOT__i = 1U;
    str = std::string{};
    str = std::string{"map: "};
    if ((0x400U & debugRegister[0x51U])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & (debugRegister[0x51U] 
                                         >> 4U)),7,
                      (0x7fU & ((debugRegister[0x51U] 
                                 << 3U) | (debugRegister[0x50U] 
                                           >> 0x1dU))));
    }
    VL_SFORMAT_NX(64,str,"%@ = ",0,-1,&(str));
    if ((0x100000U & debugRegister[0x52U])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & (debugRegister[0x52U] 
                                         >> 0xeU)),
                      7,(0x7fU & (debugRegister[0x52U] 
                                  >> 7U)));
    }
    if ((0x40U & debugRegister[0x52U])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & debugRegister[0x52U]),
                      7,(debugRegister[0x51U] >> 0x19U));
    }
    VL_SFORMAT_NX(64,str,"%@\\nprev: ",0,-1,&(str));
    if ((0x400U & debugRegister[0x51U])) {
        VL_SFORMAT_NX(64,str,"%@r%0#(p%0#), ",0,-1,
                      &(str),6,(0x3fU & (debugRegister[0x51U] 
                                         >> 4U)),7,
                      (0x7fU & (debugRegister[0x50U] 
                                >> 0x16U)));
    }
    VL_SFORMAT_NX(64,str,"%@\\nAL alloc: %0# ",0,-1,
                  &(str),6,(0x3fU & (debugRegister[0x50U] 
                                     >> 0x10U)));
    VL_SFORMAT_NX(64,str,"%@\\nIQ alloc: %0# ",0,-1,
                  &(str),4,(0xfU & (debugRegister[0x50U] 
                                    >> 0xcU)));
    this->__VnoInFunc_DumpStage(vlSymsp, 5U, (1U & 
                                              (debugRegister[0x53U] 
                                               >> 1U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xfU)), (1U 
                                                   & (debugRegister[7U] 
                                                      >> 0xeU)), 
                                (0x3ffU & ((debugRegister[0x53U] 
                                            << 9U) 
                                           | (debugRegister[0x52U] 
                                              >> 0x17U))), 
                                (3U & (debugRegister[0x52U] 
                                       >> 0x15U)), str);
    unnamedblk7__DOT__i = 2U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xeU] 
                                               >> 0x18U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[8U] 
                                                      >> 0x14U)), 
                                (0x3ffU & (debugRegister[8U] 
                                           >> 0xaU)), 
                                (3U & (debugRegister[8U] 
                                       >> 8U)), std::string{});
    unnamedblk8__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xeU] 
                                               >> 0x19U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[9U] 
                                                      >> 1U)), 
                                (0x3ffU & ((debugRegister[9U] 
                                            << 9U) 
                                           | (debugRegister[8U] 
                                              >> 0x17U))), 
                                (3U & (debugRegister[8U] 
                                       >> 0x15U)), 
                                std::string{});
    unnamedblk8__DOT__i = 2U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xeU] 
                                               >> 0x1aU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[9U] 
                                                      >> 0xeU)), 
                                (0x3ffU & (debugRegister[9U] 
                                           >> 4U)), 
                                (3U & (debugRegister[9U] 
                                       >> 2U)), std::string{});
    unnamedblk8__DOT__i = 3U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xeU] 
                                               >> 0x1bU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[9U] 
                                                      >> 0x1bU)), 
                                (0x3ffU & (debugRegister[9U] 
                                           >> 0x11U)), 
                                (3U & (debugRegister[9U] 
                                       >> 0xfU)), std::string{});
    unnamedblk8__DOT__i = 4U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xeU] 
                                               >> 0x1cU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xaU] 
                                                      >> 8U)), 
                                (0x3ffU & ((debugRegister[0xaU] 
                                            << 2U) 
                                           | (debugRegister[9U] 
                                              >> 0x1eU))), 
                                (3U & (debugRegister[9U] 
                                       >> 0x1cU)), 
                                std::string{});
    unnamedblk8__DOT__i = 5U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xeU] 
                                               >> 0x1dU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xaU] 
                                                      >> 0x15U)), 
                                (0x3ffU & (debugRegister[0xaU] 
                                           >> 0xbU)), 
                                (3U & (debugRegister[0xaU] 
                                       >> 9U)), std::string{});
    unnamedblk8__DOT__i = 6U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xeU] 
                                               >> 0x1eU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xbU] 
                                                      >> 2U)), 
                                (0x3ffU & ((debugRegister[0xbU] 
                                            << 8U) 
                                           | (debugRegister[0xaU] 
                                              >> 0x18U))), 
                                (3U & (debugRegister[0xaU] 
                                       >> 0x16U)), 
                                std::string{});
    unnamedblk8__DOT__i = 7U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (debugRegister[0xeU] 
                                              >> 0x1fU), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xbU] 
                                                      >> 0xfU)), 
                                (0x3ffU & (debugRegister[0xbU] 
                                           >> 5U)), 
                                (3U & (debugRegister[0xbU] 
                                       >> 3U)), std::string{});
    unnamedblk8__DOT__i = 8U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              debugRegister[0xfU]), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xbU] 
                                                      >> 0x1cU)), 
                                (0x3ffU & (debugRegister[0xbU] 
                                           >> 0x12U)), 
                                (3U & (debugRegister[0xbU] 
                                       >> 0x10U)), 
                                std::string{});
    unnamedblk8__DOT__i = 9U;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xfU] 
                                               >> 1U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xcU] 
                                                      >> 9U)), 
                                (0x3ffU & ((debugRegister[0xcU] 
                                            << 1U) 
                                           | (debugRegister[0xbU] 
                                              >> 0x1fU))), 
                                (3U & (debugRegister[0xbU] 
                                       >> 0x1dU)), 
                                std::string{});
    unnamedblk8__DOT__i = 0xaU;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xfU] 
                                               >> 2U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xcU] 
                                                      >> 0x16U)), 
                                (0x3ffU & (debugRegister[0xcU] 
                                           >> 0xcU)), 
                                (3U & (debugRegister[0xcU] 
                                       >> 0xaU)), std::string{});
    unnamedblk8__DOT__i = 0xbU;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xfU] 
                                               >> 3U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xdU] 
                                                      >> 3U)), 
                                (0x3ffU & ((debugRegister[0xdU] 
                                            << 7U) 
                                           | (debugRegister[0xcU] 
                                              >> 0x19U))), 
                                (3U & (debugRegister[0xcU] 
                                       >> 0x17U)), 
                                std::string{});
    unnamedblk8__DOT__i = 0xcU;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xfU] 
                                               >> 4U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xdU] 
                                                      >> 0x10U)), 
                                (0x3ffU & (debugRegister[0xdU] 
                                           >> 6U)), 
                                (3U & (debugRegister[0xdU] 
                                       >> 4U)), std::string{});
    unnamedblk8__DOT__i = 0xdU;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xfU] 
                                               >> 5U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xdU] 
                                                      >> 0x1dU)), 
                                (0x3ffU & (debugRegister[0xdU] 
                                           >> 0x13U)), 
                                (3U & (debugRegister[0xdU] 
                                       >> 0x11U)), 
                                std::string{});
    unnamedblk8__DOT__i = 0xeU;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xfU] 
                                               >> 6U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xeU] 
                                                      >> 0xaU)), 
                                (0x3ffU & debugRegister[0xeU]), 
                                (debugRegister[0xdU] 
                                 >> 0x1eU), std::string{});
    unnamedblk8__DOT__i = 0xfU;
    this->__VnoInFunc_DumpStage(vlSymsp, 6U, (1U & 
                                              (debugRegister[0xfU] 
                                               >> 7U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0xeU] 
                                                      >> 0x17U)), 
                                (0x3ffU & (debugRegister[0xeU] 
                                           >> 0xdU)), 
                                (3U & (debugRegister[0xeU] 
                                       >> 0xbU)), std::string{});
    unnamedblk8__DOT__i = 0x10U;
    str = std::string{};
    this->__VnoInFunc_DumpStage(vlSymsp, 7U, (1U & 
                                              (debugRegister[0x4dU] 
                                               >> 7U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x4dU] 
                                                      >> 6U)), 
                                (0x3ffU & ((debugRegister[0x4dU] 
                                            << 4U) 
                                           | (debugRegister[0x4cU] 
                                              >> 0x1cU))), 
                                (3U & (debugRegister[0x4cU] 
                                       >> 0x1aU)), str);
    unnamedblk9__DOT__i = 1U;
    str = std::string{};
    this->__VnoInFunc_DumpStage(vlSymsp, 7U, (1U & 
                                              (debugRegister[0x4dU] 
                                               >> 0x15U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x4dU] 
                                                      >> 0x14U)), 
                                (0x3ffU & (debugRegister[0x4dU] 
                                           >> 0xaU)), 
                                (3U & (debugRegister[0x4dU] 
                                       >> 8U)), str);
    unnamedblk9__DOT__i = 2U;
    str = std::string{};
    this->__VnoInFunc_DumpStage(vlSymsp, 7U, (1U & 
                                              (debugRegister[0x43U] 
                                               >> 0x15U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x43U] 
                                                      >> 0x14U)), 
                                (0x3ffU & (debugRegister[0x43U] 
                                           >> 0xaU)), 
                                (3U & (debugRegister[0x43U] 
                                       >> 8U)), str);
    unnamedblk10__DOT__i = 1U;
    str = std::string{};
    this->__VnoInFunc_DumpStage(vlSymsp, 7U, (1U & 
                                              (debugRegister[0x3dU] 
                                               >> 0x13U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x3dU] 
                                                      >> 0x12U)), 
                                (0x3ffU & (debugRegister[0x3dU] 
                                           >> 8U)), 
                                (3U & (debugRegister[0x3dU] 
                                       >> 6U)), str);
    unnamedblk11__DOT__i = 1U;
    str = std::string{};
    this->__VnoInFunc_DumpStage(vlSymsp, 7U, (1U & 
                                              (debugRegister[0x3eU] 
                                               >> 1U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & debugRegister[0x3eU]), 
                                (debugRegister[0x3dU] 
                                 >> 0x16U), (3U & (
                                                   debugRegister[0x3dU] 
                                                   >> 0x14U)), str);
    unnamedblk11__DOT__i = 2U;
    this->__VnoInFunc_DumpStage(vlSymsp, 8U, (1U & 
                                              (debugRegister[0x4cU] 
                                               >> 0xbU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x4cU] 
                                                      >> 0xaU)), 
                                (0x3ffU & debugRegister[0x4cU]), 
                                (debugRegister[0x4bU] 
                                 >> 0x1eU), std::string{});
    unnamedblk12__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 8U, (1U & 
                                              (debugRegister[0x4cU] 
                                               >> 0x19U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x4cU] 
                                                      >> 0x18U)), 
                                (0x3ffU & (debugRegister[0x4cU] 
                                           >> 0xeU)), 
                                (3U & (debugRegister[0x4cU] 
                                       >> 0xcU)), std::string{});
    unnamedblk12__DOT__i = 2U;
    this->__VnoInFunc_DumpStage(vlSymsp, 8U, (1U & 
                                              (debugRegister[0x43U] 
                                               >> 7U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x43U] 
                                                      >> 6U)), 
                                (0x3ffU & ((debugRegister[0x43U] 
                                            << 4U) 
                                           | (debugRegister[0x42U] 
                                              >> 0x1cU))), 
                                (3U & (debugRegister[0x42U] 
                                       >> 0x1aU)), 
                                std::string{});
    unnamedblk13__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 8U, (1U & 
                                              (debugRegister[0x3cU] 
                                               >> 0x17U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x3cU] 
                                                      >> 0x16U)), 
                                (0x3ffU & (debugRegister[0x3cU] 
                                           >> 0xcU)), 
                                (3U & (debugRegister[0x3cU] 
                                       >> 0xaU)), std::string{});
    unnamedblk14__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 8U, (1U & 
                                              (debugRegister[0x3dU] 
                                               >> 5U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x3dU] 
                                                      >> 4U)), 
                                (0x3ffU & ((debugRegister[0x3dU] 
                                            << 6U) 
                                           | (debugRegister[0x3cU] 
                                              >> 0x1aU))), 
                                (3U & (debugRegister[0x3cU] 
                                       >> 0x18U)), 
                                std::string{});
    unnamedblk14__DOT__i = 2U;
    str = std::string{};
    VL_SFORMAT_NX(64,str,"%@\\nd:0x%0x = fu(a:0x%0x, b:0x%0x), alu:0b%b, op:0b%b",0,
                  -1,&(str),32,((debugRegister[0x47U] 
                                 << 6U) | (debugRegister[0x46U] 
                                           >> 0x1aU)),
                  32,((debugRegister[0x46U] << 6U) 
                      | (debugRegister[0x45U] >> 0x1aU)),
                  32,((debugRegister[0x45U] << 6U) 
                      | (debugRegister[0x44U] >> 0x1aU)),
                  4,(0xfU & (debugRegister[0x44U] >> 0x16U)),
                  3,(7U & (debugRegister[0x44U] >> 0x13U)));
    if ((0x40000U & debugRegister[0x44U])) {
        VL_SFORMAT_NX(64,str,"%@\\nBr-pred-miss-ex",0,
                      -1,&(str));
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 9U, (1U & 
                                              (debugRegister[0x48U] 
                                               >> 7U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x48U] 
                                                      >> 6U)), 
                                (0x3ffU & ((debugRegister[0x48U] 
                                            << 4U) 
                                           | (debugRegister[0x47U] 
                                              >> 0x1cU))), 
                                (3U & (debugRegister[0x47U] 
                                       >> 0x1aU)), str);
    unnamedblk15__DOT__i = 1U;
    str = std::string{};
    VL_SFORMAT_NX(64,str,"%@\\nd:0x%0x = fu(a:0x%0x, b:0x%0x), alu:0b%b, op:0b%b",0,
                  -1,&(str),32,((debugRegister[0x4bU] 
                                 << 0x10U) | (debugRegister[0x4aU] 
                                              >> 0x10U)),
                  32,((debugRegister[0x4aU] << 0x10U) 
                      | (debugRegister[0x49U] >> 0x10U)),
                  32,((debugRegister[0x49U] << 0x10U) 
                      | (debugRegister[0x48U] >> 0x10U)),
                  4,(0xfU & (debugRegister[0x48U] >> 0xcU)),
                  3,(7U & (debugRegister[0x48U] >> 9U)));
    if ((0x100U & debugRegister[0x48U])) {
        VL_SFORMAT_NX(64,str,"%@\\nBr-pred-miss-ex",0,
                      -1,&(str));
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 9U, (1U & 
                                              (debugRegister[0x4bU] 
                                               >> 0x1dU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x4bU] 
                                                      >> 0x1cU)), 
                                (0x3ffU & (debugRegister[0x4bU] 
                                           >> 0x12U)), 
                                (3U & (debugRegister[0x4bU] 
                                       >> 0x10U)), str);
    unnamedblk15__DOT__i = 2U;
    str = std::string{};
    VL_SFORMAT_NX(64,str,"%@\\nfu(a:0x%0x, b:0x%0x)",0,
                  -1,&(str),32,((debugRegister[0x40U] 
                                 << 0x10U) | (debugRegister[0x3fU] 
                                              >> 0x10U)),
                  32,((debugRegister[0x3fU] << 0x10U) 
                      | (debugRegister[0x3eU] >> 0x10U)));
    this->__VnoInFunc_DumpStage(vlSymsp, 9U, (1U & 
                                              (debugRegister[0x42U] 
                                               >> 0x17U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x42U] 
                                                      >> 0x14U)), 
                                (0x3ffU & (debugRegister[0x41U] 
                                           >> 0x12U)), 
                                (3U & (debugRegister[0x41U] 
                                       >> 0x10U)), str);
    unnamedblk16__DOT__unnamedblk17__DOT__j = 1U;
    str = std::string{};
    this->__VnoInFunc_DumpStage(vlSymsp, 9U, (1U & 
                                              (debugRegister[0x42U] 
                                               >> 0x18U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x42U] 
                                                      >> 0x15U)), 
                                (0x3ffU & ((debugRegister[0x42U] 
                                            << 2U) 
                                           | (debugRegister[0x41U] 
                                              >> 0x1eU))), 
                                (3U & (debugRegister[0x41U] 
                                       >> 0x1cU)), str);
    unnamedblk16__DOT__unnamedblk17__DOT__j = 2U;
    str = std::string{};
    this->__VnoInFunc_DumpStage(vlSymsp, 9U, (1U & 
                                              (debugRegister[0x42U] 
                                               >> 0x19U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x42U] 
                                                      >> 0x16U)), 
                                (0x3ffU & (debugRegister[0x42U] 
                                           >> 0xaU)), 
                                (3U & (debugRegister[0x42U] 
                                       >> 8U)), str);
    unnamedblk16__DOT__unnamedblk17__DOT__j = 3U;
    unnamedblk16__DOT__i = 1U;
    str = std::string{};
    if ((4U == (7U & (debugRegister[0x35U] >> 5U)))) {
        VL_SFORMAT_NX(64,str,"%@\\nd:0x%0x = csr[0x%0x], csr[0x%0x] <= fu(0x%0x)",0,
                      -1,&(str),32,((debugRegister[0x38U] 
                                     << 0x18U) | (debugRegister[0x37U] 
                                                  >> 8U)),
                      32,((debugRegister[0x37U] << 0x18U) 
                          | (debugRegister[0x36U] >> 8U)),
                      32,((debugRegister[0x37U] << 0x18U) 
                          | (debugRegister[0x36U] >> 8U)),
                      32,((debugRegister[0x36U] << 0x18U) 
                          | (debugRegister[0x35U] >> 8U)));
    } else {
        VL_SFORMAT_NX(64,str,"%@\\nd:0x%0x = fu(a:0x%0x, b:0x%0x)\\nop:0b%b, size:0b%b, signed:0b%b",0,
                      -1,&(str),32,((debugRegister[0x38U] 
                                     << 0x18U) | (debugRegister[0x37U] 
                                                  >> 8U)),
                      32,((debugRegister[0x37U] << 0x18U) 
                          | (debugRegister[0x36U] >> 8U)),
                      32,((debugRegister[0x36U] << 0x18U) 
                          | (debugRegister[0x35U] >> 8U)),
                      3,(7U & (debugRegister[0x35U] 
                               >> 5U)),2,(3U & (debugRegister[0x35U] 
                                                >> 3U)),
                      1,(1U & (debugRegister[0x35U] 
                               >> 2U)));
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 9U, (1U & 
                                              (debugRegister[0x38U] 
                                               >> 0x15U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x38U] 
                                                      >> 0x14U)), 
                                (0x3ffU & (debugRegister[0x38U] 
                                           >> 0xaU)), 
                                (3U & (debugRegister[0x38U] 
                                       >> 8U)), str);
    unnamedblk18__DOT__i = 1U;
    str = std::string{};
    if ((4U == (7U & (debugRegister[0x38U] >> 0x19U)))) {
        VL_SFORMAT_NX(64,str,"%@\\nd:0x%0x = csr[0x%0x], csr[0x%0x] <= fu(0x%0x)",0,
                      -1,&(str),32,((debugRegister[0x3bU] 
                                     << 4U) | (debugRegister[0x3aU] 
                                               >> 0x1cU)),
                      32,((debugRegister[0x3aU] << 4U) 
                          | (debugRegister[0x39U] >> 0x1cU)),
                      32,((debugRegister[0x3aU] << 4U) 
                          | (debugRegister[0x39U] >> 0x1cU)),
                      32,((debugRegister[0x39U] << 4U) 
                          | (debugRegister[0x38U] >> 0x1cU)));
    } else {
        VL_SFORMAT_NX(64,str,"%@\\nd:0x%0x = fu(a:0x%0x, b:0x%0x)\\nop:0b%b, size:0b%b, signed:0b%b",0,
                      -1,&(str),32,((debugRegister[0x3bU] 
                                     << 4U) | (debugRegister[0x3aU] 
                                               >> 0x1cU)),
                      32,((debugRegister[0x3aU] << 4U) 
                          | (debugRegister[0x39U] >> 0x1cU)),
                      32,((debugRegister[0x39U] << 4U) 
                          | (debugRegister[0x38U] >> 0x1cU)),
                      3,(7U & (debugRegister[0x38U] 
                               >> 0x19U)),2,(3U & (
                                                   debugRegister[0x38U] 
                                                   >> 0x17U)),
                      1,(1U & (debugRegister[0x38U] 
                               >> 0x16U)));
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 9U, (1U & 
                                              (debugRegister[0x3cU] 
                                               >> 9U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x3cU] 
                                                      >> 8U)), 
                                (0x3ffU & ((debugRegister[0x3cU] 
                                            << 2U) 
                                           | (debugRegister[0x3bU] 
                                              >> 0x1eU))), 
                                (3U & (debugRegister[0x3bU] 
                                       >> 0x1cU)), str);
    unnamedblk18__DOT__i = 2U;
    str = std::string{};
    if ((2U & debugRegister[0x2cU])) {
        VL_SFORMAT_NX(64,str,"%@\\n = load([#0x%0x]) ",0,
                      -1,&(str),32,((debugRegister[0x2cU] 
                                     << 0x1fU) | (debugRegister[0x2bU] 
                                                  >> 1U)));
        if ((1U & debugRegister[0x2bU])) {
            VL_SFORMAT_NX(64,str,"%@\\nD$-miss. MSHR alloc: %0#",0,
                          -1,&(str),32,((debugRegister[0x2aU] 
                                         << 1U) | (
                                                   debugRegister[0x29U] 
                                                   >> 0x1fU)));
        } else if ((debugRegister[0x2aU] >> 0x1fU)) {
            VL_SFORMAT_NX(64,str,"%@\\nMSHR hit: %0#",0,
                          -1,&(str),32,((debugRegister[0x2aU] 
                                         << 1U) | (
                                                   debugRegister[0x29U] 
                                                   >> 0x1fU)));
        }
    }
    if ((0x40000000U & debugRegister[0x29U])) {
        VL_SFORMAT_NX(64,str,"%@\\nstore(#0x%0x, [#0x%0x])\\n",0,
                      -1,&(str),32,((debugRegister[0x28U] 
                                     << 2U) | (debugRegister[0x27U] 
                                               >> 0x1eU)),
                      32,((debugRegister[0x29U] << 2U) 
                          | (debugRegister[0x28U] >> 0x1eU)));
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 0xbU, (1U 
                                                & (debugRegister[0x2cU] 
                                                   >> 0xfU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x2cU] 
                                                      >> 0xeU)), 
                                (0x3ffU & (debugRegister[0x2cU] 
                                           >> 4U)), 
                                (3U & (debugRegister[0x2cU] 
                                       >> 2U)), str);
    unnamedblk19__DOT__i = 1U;
    str = std::string{};
    if ((0x80000U & debugRegister[0x34U])) {
        VL_SFORMAT_NX(64,str,"%@\\n = load([#0x%0x]) ",0,
                      -1,&(str),32,((debugRegister[0x34U] 
                                     << 0xdU) | (debugRegister[0x33U] 
                                                 >> 0x13U)));
        if ((0x40000U & debugRegister[0x33U])) {
            VL_SFORMAT_NX(64,str,"%@\\nD$-miss. MSHR alloc: %0#",0,
                          -1,&(str),32,((debugRegister[0x33U] 
                                         << 0xfU) | 
                                        (debugRegister[0x32U] 
                                         >> 0x11U)));
        } else if ((0x20000U & debugRegister[0x33U])) {
            VL_SFORMAT_NX(64,str,"%@\\nMSHR hit: %0#",0,
                          -1,&(str),32,((debugRegister[0x33U] 
                                         << 0xfU) | 
                                        (debugRegister[0x32U] 
                                         >> 0x11U)));
        }
    }
    if ((0x10000U & debugRegister[0x32U])) {
        VL_SFORMAT_NX(64,str,"%@\\nstore(#0x%0x, [#0x%0x])\\n",0,
                      -1,&(str),32,((debugRegister[0x31U] 
                                     << 0x10U) | (debugRegister[0x30U] 
                                                  >> 0x10U)),
                      32,((debugRegister[0x32U] << 0x10U) 
                          | (debugRegister[0x31U] >> 0x10U)));
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 0xbU, (1U 
                                                & (debugRegister[0x35U] 
                                                   >> 1U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & debugRegister[0x35U]), 
                                (debugRegister[0x34U] 
                                 >> 0x16U), (3U & (
                                                   debugRegister[0x34U] 
                                                   >> 0x14U)), str);
    unnamedblk19__DOT__i = 2U;
    str = std::string{};
    if ((1U & debugRegister[0x1eU])) {
        VL_SFORMAT_NX(64,str,"%@\\n#0x%0x = load()",0,
                      -1,&(str),32,debugRegister[0x1dU]);
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 0xaU, (1U 
                                                & (debugRegister[0x1eU] 
                                                   >> 0xeU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x1eU] 
                                                      >> 0xdU)), 
                                (0x3ffU & (debugRegister[0x1eU] 
                                           >> 3U)), 
                                (3U & (debugRegister[0x1eU] 
                                       >> 1U)), str);
    unnamedblk20__DOT__i = 1U;
    str = std::string{};
    if ((0x8000U & debugRegister[0x23U])) {
        VL_SFORMAT_NX(64,str,"%@\\n#0x%0x = load()",0,
                      -1,&(str),32,((debugRegister[0x23U] 
                                     << 0x11U) | (debugRegister[0x22U] 
                                                  >> 0xfU)));
    }
    this->__VnoInFunc_DumpStage(vlSymsp, 0xaU, (1U 
                                                & (debugRegister[0x23U] 
                                                   >> 0x1dU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x23U] 
                                                      >> 0x1cU)), 
                                (0x3ffU & (debugRegister[0x23U] 
                                           >> 0x12U)), 
                                (3U & (debugRegister[0x23U] 
                                       >> 0x10U)), str);
    unnamedblk20__DOT__i = 2U;
    this->__VnoInFunc_DumpStage(vlSymsp, 0xcU, (1U 
                                                & (debugRegister[0x44U] 
                                                   >> 3U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x44U] 
                                                      >> 2U)), 
                                (0x3ffU & ((debugRegister[0x44U] 
                                            << 8U) 
                                           | (debugRegister[0x43U] 
                                              >> 0x18U))), 
                                (3U & (debugRegister[0x43U] 
                                       >> 0x16U)), 
                                std::string{});
    unnamedblk21__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 0xcU, (1U 
                                                & (debugRegister[0x44U] 
                                                   >> 0x11U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x44U] 
                                                      >> 0x10U)), 
                                (0x3ffU & (debugRegister[0x44U] 
                                           >> 6U)), 
                                (3U & (debugRegister[0x44U] 
                                       >> 4U)), std::string{});
    unnamedblk21__DOT__i = 2U;
    this->__VnoInFunc_DumpStage(vlSymsp, 0xcU, (1U 
                                                & (debugRegister[0x3eU] 
                                                   >> 0xfU)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x3eU] 
                                                      >> 0xeU)), 
                                (0x3ffU & (debugRegister[0x3eU] 
                                           >> 4U)), 
                                (3U & (debugRegister[0x3eU] 
                                       >> 2U)), std::string{});
    unnamedblk22__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 0xcU, (1U 
                                                & (debugRegister[0x18U] 
                                                   >> 0x11U)), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x18U] 
                                                      >> 0x10U)), 
                                (0x3ffU & (debugRegister[0x18U] 
                                           >> 6U)), 
                                (3U & (debugRegister[0x18U] 
                                       >> 4U)), std::string{});
    unnamedblk23__DOT__i = 1U;
    this->__VnoInFunc_DumpStage(vlSymsp, 0xcU, (debugRegister[0x18U] 
                                                >> 0x1fU), 
                                (1U & (debugRegister[7U] 
                                       >> 0xdU)), (1U 
                                                   & (debugRegister[0x18U] 
                                                      >> 0x1eU)), 
                                (0x3ffU & (debugRegister[0x18U] 
                                           >> 0x14U)), 
                                (3U & (debugRegister[0x18U] 
                                       >> 0x12U)), 
                                std::string{});
    unnamedblk23__DOT__i = 2U;
    str = std::string{};
    str = std::string{"\\nrelease: "};
    if ((0x8000U & debugRegister[0xfU])) {
        VL_SFORMAT_NX(64,str,"%@p%0#, ",0,-1,&(str),
                      7,(0x7fU & (debugRegister[0xfU] 
                                  >> 8U)));
    }
    if ((0x10000000U & debugRegister[0xfU])) {
        this->__VnoInFunc_DumpStage(vlSymsp, 0xdU, 
                                    (1U & (debugRegister[0xfU] 
                                           >> 0x1cU)), 0U, 1U, 
                                    (0x3ffU & (debugRegister[0xfU] 
                                               >> 0x12U)), 
                                    (3U & (debugRegister[0xfU] 
                                           >> 0x10U)), str);
    } else if ((0x20000000U & debugRegister[0xfU])) {
        this->__VnoInFunc_DumpStage(vlSymsp, 0xeU, 
                                    (1U & (debugRegister[0xfU] 
                                           >> 0x1dU)), 0U, 0U, 
                                    (0x3ffU & (debugRegister[0xfU] 
                                               >> 0x12U)), 
                                    (3U & (debugRegister[0xfU] 
                                           >> 0x10U)), str);
    }
    unnamedblk24__DOT__i = 1U;
    str = std::string{};
    str = std::string{"\\nrelease: "};
    if ((0x20U & debugRegister[0x10U])) {
        VL_SFORMAT_NX(64,str,"%@p%0#, ",0,-1,&(str),
                      7,(0x7fU & ((debugRegister[0x10U] 
                                   << 2U) | (debugRegister[0xfU] 
                                             >> 0x1eU))));
    }
    if ((0x40000U & debugRegister[0x10U])) {
        this->__VnoInFunc_DumpStage(vlSymsp, 0xdU, 
                                    (1U & (debugRegister[0x10U] 
                                           >> 0x12U)), 0U, 1U, 
                                    (0x3ffU & (debugRegister[0x10U] 
                                               >> 8U)), 
                                    (3U & (debugRegister[0x10U] 
                                           >> 6U)), str);
    } else if ((0x80000U & debugRegister[0x10U])) {
        this->__VnoInFunc_DumpStage(vlSymsp, 0xeU, 
                                    (1U & (debugRegister[0x10U] 
                                           >> 0x13U)), 0U, 0U, 
                                    (0x3ffU & (debugRegister[0x10U] 
                                               >> 8U)), 
                                    (3U & (debugRegister[0x10U] 
                                           >> 6U)), str);
    }
    unnamedblk24__DOT__i = 2U;
}

std::string VL_TO_STRING(const VlClassRef<VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper>& obj) {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::VL_TO_STRING\n"); );
    // Body
    return (obj ? obj->to_string() : "null");
}

std::string VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::to_string() const {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::to_string\n"); );
    // Body
    return ("'{"s + to_string_middle() + "}");
}

std::string VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::to_string_middle() const {
    VL_DEBUG_IF(VL_DBG_MSGF("+            VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper::to_string_middle\n"); );
    // Body
    std::string out;
    out += "m_file:" + VL_TO_STRING(__PVT__m_file);
    out += ", m_cycle:" + VL_TO_STRING(__PVT__m_cycle);
    out += ", m_retireID:" + VL_TO_STRING(__PVT__m_retireID);
    return out;
}
