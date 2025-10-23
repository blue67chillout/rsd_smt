// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH___024ROOT_H_
#define VERILATED_VSMT_RTL_TESTBENCH___024ROOT_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_DumperTypes;
class VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg;
class VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg;
class VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg;
class VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg;
class VSMT_RTL_Testbench_MemoryTypes;
class VSMT_RTL_Testbench_SMT_RTL_Testbench;
class VSMT_RTL_Testbench___024unit;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench___024root final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_SMT_RTL_Testbench* SMT_RTL_Testbench;
    VSMT_RTL_Testbench___024unit* __PVT____024unit;
    VSMT_RTL_Testbench_MemoryTypes* MemoryTypes;
    VSMT_RTL_Testbench_DumperTypes* __PVT__DumperTypes;
    VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg* DumperTypes__03a__03aKanataDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg* DumperTypes__03a__03aSerialDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg* DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg* DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg;

    // DESIGN SPECIFIC STATE
    // Anonymous structures to workaround compiler member-count bugs
    struct {
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatable__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__allocatable__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__rnStage__DOT__serialize__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__idStageStallUpper__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__ifStageSendBubbleLower__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__npStageSendBubbleLowerForInterrupt__0;
        CData/*1:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__iCache__DOT__hitArray__0;
        CData/*1:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__flushNum__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__recoveryManagerIF____PVT__renameLogicRecoveryRMT__0;
        CData/*4:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__fflags__0;
        CData/*3:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__retiredStoreQueuePtr__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__storeCommitter__DOT__stallStoreTagStage__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__dcFlushReqAck__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerExcpt__0;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerInterrupt__0;
        CData/*4:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__interruptCode__0;
        CData/*0:0*/ __VstlDidInit;
        CData/*0:0*/ __VstlFirstIteration;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__recoveryManagerIF____PVT__renameLogicRecoveryRMT__1;
        CData/*0:0*/ __VicoDidInit;
        CData/*0:0*/ __VicoFirstIteration;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatable__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__allocatable__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__rnStage__DOT__serialize__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__idStageStallUpper__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__ifStageSendBubbleLower__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__npStageSendBubbleLowerForInterrupt__1;
        CData/*1:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__iCache__DOT__hitArray__1;
        CData/*1:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__flushNum__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__recoveryManagerIF____PVT__renameLogicRecoveryRMT__2;
        CData/*4:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__fflags__1;
        CData/*3:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__retiredStoreQueuePtr__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__storeCommitter__DOT__stallStoreTagStage__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__dcFlushReqAck__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerExcpt__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerInterrupt__1;
        CData/*4:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__interruptCode__1;
        CData/*0:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench____PVT__clk__0;
        CData/*0:0*/ __VactDidInit;
        CData/*0:0*/ __VactContinue;
        IData/*21:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ioUnitIF____PVT__ioReadAddrIn__0;
        IData/*21:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ioUnitIF____PVT__ioReadAddrIn__1;
        IData/*31:0*/ __VactIterCount;
        QData/*32:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst__0;
        QData/*32:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst__0;
        QData/*32:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst__1;
        QData/*32:0*/ __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__brPredTaken__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__updateBrHistory__0;
        VlUnpacked<CData/*1:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__brPred__predictor____PVT__phtRV__0;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegA__0;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegB__0;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegC__0;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyDstReg__0;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyPrevDstReg__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegA__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegB__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegC__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__prevDependIssueQueuePtr__0;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__pushedTailPtr__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedLoadQueuePtr__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedStoreQueuePtr__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__headExecState__0;
        VlUnpacked<CData/*0:0*/, 6> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__schedulerIF____PVT__selected__0;
    };
    struct {
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divBusy__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Busy__0;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlOut__0;
        VlUnpacked<IData/*31:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__shiftDataOut__0;
        VlUnpacked<IData/*31:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__aluDataOut__0;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlIn__0;
        VlUnpacked<QData/*32:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intDstRegDataOut__0;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlIn__0;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlIn__0;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlIn__0;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlOut__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divReserved__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divRelease__0;
        VlUnpacked<IData/*31:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divDataOut__0;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__memSrcRegNumB__0;
        VlUnpacked<CData/*6:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__fpSrcRegNumA__0;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlOut__0;
        VlUnpacked<IData/*21:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__dcReadAddr__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__storeLoadForwarded__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__forwardMiss__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__conflict__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__picked__0;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__pickedPtr__0;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__storeQueue____PVT__pickedPtr__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__lsuCacheGrt__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR__0;
        VlUnpacked<IData/*21:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_Addr__0;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_ActiveListPtr__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isAllocatedByStore__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isUncachable__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheGrt__0;
        VlUnpacked<VlWide<3>/*92:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxTagOut__0;
        VlUnpacked<VlWide<3>/*65:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxDataOut__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemMuxOut__0;
        VlUnpacked<CData/*1:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__cacheArrayInSel__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemReq__0;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlOut__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Reserved__0;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Release__0;
        VlUnpacked<CData/*6:0*/, 11> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcRegNum__0;
        VlUnpacked<CData/*6:0*/, 5> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcFPRegNum__0;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__ra__0;
        VlUnpacked<SData/*9:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__btb__btbEntryArray____PVT__ra__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList____PVT__poppedData__0;
        VlUnpacked<CData/*0:0*/, 16> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__wakeupLogic____PVT__opMatrixReady__0;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__genblk1__DOT__rBank__DOT__raBank__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__rv__0;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__rv__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__rv__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__0;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__0;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__0;
        VlUnpacked<CData/*3:0*/, 8> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__rvBank__0;
        VlUnpacked<VlUnpacked<CData/*0:0*/, 8>, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__0;
        VlUnpacked<CData/*2:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__0;
        VlUnpacked<CData/*1:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__0;
        VlUnpacked<VlUnpacked<CData/*4:0*/, 3>, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__0;
        VlUnpacked<VlUnpacked<CData/*3:0*/, 8>, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__0;
        VlUnpacked<CData/*2:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__lvo__0;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__brPredTaken__1;
    };
    struct {
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__updateBrHistory__1;
        VlUnpacked<CData/*1:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__brPred__predictor____PVT__phtRV__1;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegA__1;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegB__1;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegC__1;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyDstReg__1;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyPrevDstReg__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegA__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegB__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegC__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__prevDependIssueQueuePtr__1;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__pushedTailPtr__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedLoadQueuePtr__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedStoreQueuePtr__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__headExecState__1;
        VlUnpacked<CData/*0:0*/, 6> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__schedulerIF____PVT__selected__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divBusy__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Busy__1;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlOut__1;
        VlUnpacked<IData/*31:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__shiftDataOut__1;
        VlUnpacked<IData/*31:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__aluDataOut__1;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlIn__1;
        VlUnpacked<QData/*32:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intDstRegDataOut__1;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlIn__1;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlIn__1;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlIn__1;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlOut__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divReserved__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divRelease__1;
        VlUnpacked<IData/*31:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divDataOut__1;
        VlUnpacked<CData/*6:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__memSrcRegNumB__1;
        VlUnpacked<CData/*6:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__fpSrcRegNumA__1;
        VlUnpacked<IData/*20:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlOut__1;
        VlUnpacked<IData/*21:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__dcReadAddr__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__storeLoadForwarded__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__forwardMiss__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__conflict__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__picked__1;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__pickedPtr__1;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__storeQueue____PVT__pickedPtr__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__lsuCacheGrt__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR__1;
        VlUnpacked<IData/*21:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_Addr__1;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_ActiveListPtr__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isAllocatedByStore__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isUncachable__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheGrt__1;
        VlUnpacked<VlWide<3>/*92:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxTagOut__1;
        VlUnpacked<VlWide<3>/*65:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxDataOut__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemMuxOut__1;
        VlUnpacked<CData/*1:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__cacheArrayInSel__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemReq__1;
        VlUnpacked<IData/*20:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlOut__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Reserved__1;
        VlUnpacked<CData/*0:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Release__1;
        VlUnpacked<CData/*6:0*/, 11> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcRegNum__1;
        VlUnpacked<CData/*6:0*/, 5> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcFPRegNum__1;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__ra__1;
        VlUnpacked<SData/*9:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__btb__btbEntryArray____PVT__ra__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList____PVT__poppedData__1;
        VlUnpacked<CData/*0:0*/, 16> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__wakeupLogic____PVT__opMatrixReady__1;
        VlUnpacked<CData/*5:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__genblk1__DOT__rBank__DOT__raBank__1;
        VlUnpacked<CData/*0:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__rv__1;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__rv__1;
    };
    struct {
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__rv__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1;
        VlUnpacked<CData/*3:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1;
        VlUnpacked<CData/*3:0*/, 1> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__1;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__1;
        VlUnpacked<CData/*4:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__1;
        VlUnpacked<CData/*3:0*/, 8> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__rvBank__1;
        VlUnpacked<VlUnpacked<CData/*0:0*/, 8>, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__1;
        VlUnpacked<CData/*2:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__1;
        VlUnpacked<CData/*1:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__1;
        VlUnpacked<VlUnpacked<CData/*4:0*/, 3>, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__1;
        VlUnpacked<VlUnpacked<CData/*3:0*/, 8>, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__1;
        VlUnpacked<CData/*2:0*/, 2> __Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__lvo__1;
        VlUnpacked<CData/*0:0*/, 175> __Vm_traceActivity;
    };
    VlDelayScheduler __VdlySched;
    VlTriggerVec<100> __VstlTriggered;
    VlTriggerVec<2> __VicoTriggered;
    VlTriggerVec<101> __VactTriggered;
    VlTriggerVec<101> __VnbaTriggered;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench___024root(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench___024root();
    VL_UNCOPYABLE(VSMT_RTL_Testbench___024root);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
