// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_STOREQUEUE_H_
#define VERILATED_VSMT_RTL_TESTBENCH_STOREQUEUE_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_StoreQueue final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17* storeQueueData;

    // DESIGN SPECIFIC STATE
    CData/*3:0*/ __PVT__releasedStoreQueuePtr;
    CData/*1:0*/ __PVT__pushCount;
    CData/*0:0*/ __PVT__push;
    CData/*0:0*/ __Vcellout__genblk1__BRA__0__KET____DOT__picker__picked;
    CData/*3:0*/ __Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr;
    CData/*3:0*/ __PVT__storeQueuePointer__DOT__regHead;
    CData/*3:0*/ __PVT__storeQueuePointer__DOT__nextHead;
    CData/*3:0*/ __PVT__storeQueuePointer__DOT__regTail;
    CData/*3:0*/ __PVT__storeQueuePointer__DOT__nextTail;
    CData/*3:0*/ __PVT__storeQueuePointer__DOT__roundedSetTailPtr;
    CData/*4:0*/ __PVT__storeQueuePointer__DOT__regCount;
    CData/*4:0*/ __PVT__storeQueuePointer__DOT__nextCount;
    CData/*3:0*/ __PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant;
    SData/*15:0*/ __PVT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq;
    IData/*26:0*/ __PVT__headAddrEntry;
    IData/*31:0*/ __PVT__unnamedblk2__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk3__DOT__i;
    IData/*31:0*/ __PVT__unnamedblk4__DOT__i;
    IData/*31:0*/ __PVT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__2__Vfuncout;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__2__data;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__2__offset;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__2__width;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__2__unnamedblk1__DOT__i;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__2__ret;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__6__Vfuncout;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__6__data;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__6__offset;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__6__width;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__6__unnamedblk1__DOT__i;
    IData/*31:0*/ __Vfunc_LSQ_SelectBits__6__ret;
    IData/*19:0*/ __Vfunc_LSQ_ToBlockAddr__8__Vfuncout;
    IData/*21:0*/ __Vfunc_LSQ_ToBlockAddr__8__addr;
    QData/*37:0*/ __PVT__headDataEntry;
    VlUnpacked<IData/*26:0*/, 16> __PVT__storeQueue;
    VlUnpacked<CData/*0:0*/, 1> __PVT__executeStore;
    VlUnpacked<IData/*19:0*/, 1> __PVT__executedStoreAddr;
    VlUnpacked<CData/*0:0*/, 1> __PVT__executedStoreWordWE;
    VlUnpacked<CData/*3:0*/, 1> __PVT__executedStoreByteWE;
    VlUnpacked<CData/*0:0*/, 1> __PVT__executedStoreCondEnabled;
    VlUnpacked<CData/*0:0*/, 1> __PVT__executedStoreRegValid;
    VlUnpacked<CData/*3:0*/, 1> __PVT__executedStoreQueuePtrByStore;
    VlUnpacked<QData/*37:0*/, 1> __PVT__forwardedDataEntry;
    VlUnpacked<CData/*0:0*/, 1> __PVT__executedLoadWordRE;
    VlUnpacked<CData/*3:0*/, 1> __PVT__executedLoadByteRE;
    VlUnpacked<SData/*15:0*/, 1> __PVT__addrMatch;
    VlUnpacked<CData/*3:0*/, 1> __PVT__pickedPtr;
    VlUnpacked<CData/*0:0*/, 1> __PVT__picked;
    VlUnpacked<CData/*3:0*/, 1> __PVT__executedStoreQueuePtrByLoad;
    VlUnpacked<CData/*3:0*/, 2> __PVT__sqReadPtr;
    VlUnpacked<QData/*37:0*/, 2> __PVT__sqReadData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__sqWE;
    VlUnpacked<QData/*37:0*/, 1> __PVT__sqWriteData;
    VlUnpacked<IData/*31:0*/, 1> __PVT__sqWriteStoreData;
    VlUnpacked<QData/*37:0*/, 2> __Vcellout__storeQueueData__rv;
    VlUnpacked<QData/*37:0*/, 1> __Vcellinp__storeQueueData__wv;
    VlUnpacked<CData/*0:0*/, 1> __PVT__storeLoadForwarded;
    VlUnpacked<IData/*31:0*/, 1> __PVT__forwardedLoadData;
    VlUnpacked<CData/*0:0*/, 1> __PVT__forwardMiss;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_StoreQueue(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_StoreQueue();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_StoreQueue);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
