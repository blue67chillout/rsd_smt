// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_ACTIVELIST_H_
#define VERILATED_VSMT_RTL_TESTBENCH_ACTIVELIST_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiBankRAM__R2;
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi4;
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5;
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi6;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_ActiveList final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiBankRAM__R2* activeList;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi4* execState;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5* fflagsState;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi6* execStateRef;

    // DESIGN SPECIFIC STATE
    // Anonymous structures to workaround compiler member-count bugs
    struct {
        CData/*5:0*/ headPtr;
        CData/*1:0*/ __PVT__pushNum;
        CData/*6:0*/ __PVT__oldestAge;
        CData/*0:0*/ __PVT__exceptionDetected;
        CData/*2:0*/ __PVT__refetchType;
        CData/*2:0*/ __PVT__exceptionIndex;
        CData/*0:0*/ __PVT__startRecoveryAtCommit;
        CData/*6:0*/ __PVT__recoveryEntryNum;
        CData/*6:0*/ __PVT__nextRecoveryEntryNum;
        CData/*5:0*/ __PVT__flushRangeHeadPtr;
        CData/*5:0*/ __PVT__flushRangeTailPtr;
        CData/*0:0*/ __PVT__execStateIsDifferentFromRef;
        CData/*0:0*/ __PVT__regInRecovery;
        CData/*0:0*/ __PVT__nextInRecovery;
        CData/*0:0*/ __Vlvbound_h46781e09__0;
        CData/*5:0*/ __Vlvbound_h0432b08f__0;
        CData/*0:0*/ __Vlvbound_h6be00a99__0;
        CData/*5:0*/ __Vlvbound_h7dcaa4bf__0;
        CData/*4:0*/ __Vlvbound_hd8cbcf9b__0;
        CData/*5:0*/ __PVT__activeListPointer__DOT__regHead;
        CData/*5:0*/ __PVT__activeListPointer__DOT__nextHead;
        CData/*5:0*/ __PVT__activeListPointer__DOT__regTail;
        CData/*5:0*/ __PVT__activeListPointer__DOT__nextTail;
        CData/*6:0*/ __PVT__activeListPointer__DOT__regCount;
        CData/*6:0*/ __PVT__activeListPointer__DOT__nextCount;
        IData/*31:0*/ __PVT__unnamedblk9__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk10__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk11__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk13__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk14__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk15__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk16__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk17__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk18__DOT__i;
        IData/*31:0*/ __PVT__unnamedblk19__DOT__i;
        VlWide<3>/*70:0*/ __PVT__recoveryReg;
        VlWide<3>/*70:0*/ __PVT__nextRecoveryReg;
        VlUnpacked<CData/*5:0*/, 2> __PVT__headPtrList;
        VlUnpacked<CData/*5:0*/, 2> __PVT__tailPtrList;
        VlUnpacked<CData/*5:0*/, 2> __PVT__readPtrList;
        VlUnpacked<CData/*5:0*/, 2> __PVT__pushedTailPtr;
        VlUnpacked<CData/*0:0*/, 2> __PVT__pushTail;
        VlUnpacked<QData/*62:0*/, 2> __PVT__pushedTailData;
        VlUnpacked<QData/*62:0*/, 2> __PVT__readData;
        VlUnpacked<QData/*62:0*/, 2> __Vcellout__activeList__rv;
        VlUnpacked<QData/*62:0*/, 2> __Vcellinp__activeList__wv;
        VlUnpacked<CData/*0:0*/, 6> __PVT__we;
        VlUnpacked<VlWide<3>/*71:0*/, 6> __PVT__writeData;
        VlUnpacked<CData/*6:0*/, 6> __PVT__writeAge;
        VlUnpacked<CData/*0:0*/, 8> __PVT__esWE;
        VlUnpacked<CData/*5:0*/, 8> __PVT__esWA;
        VlUnpacked<CData/*5:0*/, 2> __PVT__esRA;
        VlUnpacked<CData/*0:0*/, 8> __PVT__esWV;
        VlUnpacked<CData/*0:0*/, 2> __PVT__esRV;
        VlUnpacked<CData/*3:0*/, 2> __PVT__headExecState;
        VlUnpacked<CData/*0:0*/, 2> __Vcellout__execState__rv;
        VlUnpacked<CData/*0:0*/, 8> __Vcellinp__execState__wv;
        VlUnpacked<CData/*0:0*/, 3> __PVT__ffsWE;
        VlUnpacked<CData/*5:0*/, 3> __PVT__ffsWA;
        VlUnpacked<CData/*5:0*/, 2> __PVT__ffsRA;
        VlUnpacked<CData/*4:0*/, 3> __PVT__ffsWV;
        VlUnpacked<CData/*4:0*/, 2> __PVT__ffsRV;
        VlUnpacked<CData/*4:0*/, 2> __Vcellout__fflagsState__rv;
        VlUnpacked<CData/*4:0*/, 3> __Vcellinp__fflagsState__wv;
    };
    struct {
        VlUnpacked<CData/*0:0*/, 8> __PVT__esRefWE;
        VlUnpacked<CData/*5:0*/, 8> __PVT__esRefWA;
        VlUnpacked<CData/*5:0*/, 2> __PVT__esRefRA;
        VlUnpacked<CData/*3:0*/, 8> __PVT__esRefWV;
        VlUnpacked<CData/*3:0*/, 2> __PVT__esRefRV;
        VlUnpacked<CData/*3:0*/, 2> __PVT__headExecStateRef;
    };

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_ActiveList(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_ActiveList();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_ActiveList);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
