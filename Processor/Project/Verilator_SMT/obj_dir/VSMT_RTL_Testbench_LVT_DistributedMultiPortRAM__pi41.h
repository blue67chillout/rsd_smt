// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_LVT_DISTRIBUTEDMULTIPORTRAM__PI41_H_
#define VERILATED_VSMT_RTL_TESTBENCH_LVT_DISTRIBUTEDMULTIPORTRAM__PI41_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41 final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    // Anonymous structures to workaround compiler member-count bugs
    struct {
        VL_IN8(__PVT__clk,0,0);
        IData/*31:0*/ __PVT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__unnamedblk7__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__i;
        IData/*31:0*/ __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j;
        VL_IN8(__PVT__we[2],0,0);
        VL_IN8(__PVT__wa[2],6,0);
        VL_IN64(__PVT__wv[2],32,0);
        VL_IN8(__PVT__ra[5],6,0);
        VL_OUT64(__PVT__rv[5],32,0);
        VlUnpacked<QData/*32:0*/, 128> debugValue;
        VlUnpacked<VlUnpacked<QData/*32:0*/, 2>, 5> __PVT__genblk1__DOT__rvBank;
        VlUnpacked<CData/*0:0*/, 2> __PVT__genblk1__DOT__lvi;
        VlUnpacked<CData/*0:0*/, 5> __PVT__genblk1__DOT__lvo;
        VlUnpacked<CData/*0:0*/, 2> __PVT__genblk1__DOT__lvt__DOT__rwbWriteValue;
        VlUnpacked<CData/*6:0*/, 2> __PVT__genblk1__DOT__lvt__DOT__wbReadAddr;
        VlUnpacked<VlUnpacked<CData/*0:0*/, 2>, 2> __PVT__genblk1__DOT__lvt__DOT__wbReadValue;
        VlUnpacked<CData/*6:0*/, 5> __PVT__genblk1__DOT__lvt__DOT__rbReadAddr;
        VlUnpacked<VlUnpacked<CData/*0:0*/, 5>, 2> __PVT__genblk1__DOT__lvt__DOT__rbReadValue;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__debugValue;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__wj__BRA__0__KET____DOT__wi__BRA__1__KET____DOT__genblk1__DOT__wBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__wj__BRA__1__KET____DOT__wi__BRA__0__KET____DOT__genblk1__DOT__wBank__DOT__array;
    };
    struct {
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__0__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<CData/*0:0*/, 128> __PVT__genblk1__DOT__lvt__DOT__rj__BRA__1__KET____DOT__ri__BRA__4__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__rBank__DOT__array;
        VlUnpacked<QData/*32:0*/, 128> __PVT__genblk1__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__rBank__DOT__array;
    };

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
