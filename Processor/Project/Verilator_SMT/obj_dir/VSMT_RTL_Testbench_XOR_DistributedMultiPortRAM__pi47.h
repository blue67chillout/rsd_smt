// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_XOR_DISTRIBUTEDMULTIPORTRAM__PI47_H_
#define VERILATED_VSMT_RTL_TESTBENCH_XOR_DISTRIBUTEDMULTIPORTRAM__PI47_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    // Anonymous structures to workaround compiler member-count bugs
    struct {
        VL_IN8(__PVT__clk,0,0);
        IData/*31:0*/ __PVT__unnamedblk7__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__0__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__1__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__2__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__3__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    };
    struct {
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__4__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__5__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__6__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__wj__BRA__7__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__2__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__2__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__2__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__2__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__3__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__3__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
    };
    struct {
        IData/*31:0*/ __PVT__rj__BRA__3__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__3__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__4__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__4__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__4__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__4__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__5__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__5__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__5__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__5__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__6__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__6__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__6__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__6__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__7__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__7__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__rj__BRA__7__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__rj__BRA__7__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        VL_IN8(__PVT__we[8],0,0);
        VL_IN8(__PVT__wa[8],5,0);
        VL_IN8(__PVT__wv[8],2,0);
        VL_IN8(__PVT__ra[2],5,0);
        VL_OUT8(__PVT__rv[2],2,0);
        VlUnpacked<CData/*2:0*/, 8> __PVT__rwbWriteValue;
        VlUnpacked<CData/*5:0*/, 8> __PVT__wbReadAddr;
        VlUnpacked<VlUnpacked<CData/*2:0*/, 8>, 8> __PVT__wbReadValue;
        VlUnpacked<CData/*5:0*/, 2> __PVT__rbReadAddr;
        VlUnpacked<VlUnpacked<CData/*2:0*/, 2>, 8> __PVT__rbReadValue;
        VlUnpacked<CData/*2:0*/, 64> __PVT__debugValue;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__0__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__0__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__0__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__0__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__0__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__0__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__1__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__1__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__1__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__1__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__1__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__1__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__2__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__2__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__2__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__2__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__2__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__2__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__2__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__3__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__3__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__3__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__3__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__3__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__3__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__3__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__4__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__4__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__4__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__4__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__4__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__4__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__4__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array;
    };
    struct {
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__5__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__5__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__5__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__5__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__5__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__5__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__5__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__6__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__6__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__6__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__6__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__6__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__6__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__6__KET____DOT__wi__BRA__7__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__7__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__7__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__7__KET____DOT__wi__BRA__2__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__7__KET____DOT__wi__BRA__3__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__7__KET____DOT__wi__BRA__4__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__7__KET____DOT__wi__BRA__5__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__wj__BRA__7__KET____DOT__wi__BRA__6__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__2__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__2__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__3__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__3__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__4__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__4__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__5__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__5__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__6__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__6__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__7__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*2:0*/, 64> __PVT__rj__BRA__7__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
    };

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
