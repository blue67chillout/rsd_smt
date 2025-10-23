// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Symbol table internal header
//
// Internal details; most calling programs do not need this header,
// unless using verilator public meta comments.

#ifndef VERILATED_VSMT_RTL_TESTBENCH__SYMS_H_
#define VERILATED_VSMT_RTL_TESTBENCH__SYMS_H_  // guard

#include "verilated.h"
#include "verilated_vcd_c.h"

// INCLUDE MODEL CLASS

#include "VSMT_RTL_Testbench.h"

// INCLUDE MODULE CLASSES
#include "VSMT_RTL_Testbench___024root.h"
#include "VSMT_RTL_Testbench_SMT_RTL_Testbench.h"
#include "VSMT_RTL_Testbench___024unit.h"
#include "VSMT_RTL_Testbench_MemoryTypes.h"
#include "VSMT_RTL_Testbench_DumperTypes.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench_Memory__Iz1.h"
#include "VSMT_RTL_Testbench_CommitStage.h"
#include "VSMT_RTL_Testbench_RegisterFile.h"
#include "VSMT_RTL_Testbench_StoreQueue.h"
#include "VSMT_RTL_Testbench_RenameLogic.h"
#include "VSMT_RTL_Testbench_ActiveList.h"
#include "VSMT_RTL_Testbench_RMT.h"
#include "VSMT_RTL_Testbench_RetirementRMT.h"
#include "VSMT_RTL_Testbench_BTB.h"
#include "VSMT_RTL_Testbench_BranchPredictor.h"
#include "VSMT_RTL_Testbench_IssueQueue.h"
#include "VSMT_RTL_Testbench_DestinationRAM.h"
#include "VSMT_RTL_Testbench_WakeupLogic.h"
#include "VSMT_RTL_Testbench_MemoryDependencyPredictor.h"
#include "VSMT_RTL_Testbench_InitializedBlockRAM__pi1.h"
#include "VSMT_RTL_Testbench_PreDecodeStageIF.h"
#include "VSMT_RTL_Testbench_DecodeStageIF.h"
#include "VSMT_RTL_Testbench_RenameStageIF.h"
#include "VSMT_RTL_Testbench_ScheduleStageIF.h"
#include "VSMT_RTL_Testbench_IntegerIssueStageIF.h"
#include "VSMT_RTL_Testbench_IntegerRegisterReadStageIF.h"
#include "VSMT_RTL_Testbench_IntegerExecutionStageIF.h"
#include "VSMT_RTL_Testbench_ComplexIntegerIssueStageIF.h"
#include "VSMT_RTL_Testbench_ComplexIntegerRegisterReadStageIF.h"
#include "VSMT_RTL_Testbench_ComplexIntegerExecutionStageIF.h"
#include "VSMT_RTL_Testbench_MemoryIssueStageIF.h"
#include "VSMT_RTL_Testbench_MemoryRegisterReadStageIF.h"
#include "VSMT_RTL_Testbench_MemoryExecutionStageIF.h"
#include "VSMT_RTL_Testbench_MemoryAccessStageIF.h"
#include "VSMT_RTL_Testbench_MemoryTagAccessStageIF.h"
#include "VSMT_RTL_Testbench_FPIssueStageIF.h"
#include "VSMT_RTL_Testbench_FPRegisterReadStageIF.h"
#include "VSMT_RTL_Testbench_FPExecutionStageIF.h"
#include "VSMT_RTL_Testbench_CommitStageIF.h"
#include "VSMT_RTL_Testbench_RegisterFileIF.h"
#include "VSMT_RTL_Testbench_BypassNetworkIF.h"
#include "VSMT_RTL_Testbench_MulDivUnitIF.h"
#include "VSMT_RTL_Testbench_LoadStoreUnitIF.h"
#include "VSMT_RTL_Testbench_FPDivSqrtUnitIF.h"
#include "VSMT_RTL_Testbench_RenameLogicIF.h"
#include "VSMT_RTL_Testbench_ActiveListIF.h"
#include "VSMT_RTL_Testbench_Gshare.h"
#include "VSMT_RTL_Testbench_SchedulerIF.h"
#include "VSMT_RTL_Testbench_WakeupSelectIF.h"
#include "VSMT_RTL_Testbench_CacheSystemIF.h"
#include "VSMT_RTL_Testbench_CacheFlushManagerIF.h"
#include "VSMT_RTL_Testbench_CSR_UnitIF.h"
#include "VSMT_RTL_Testbench_IO_UnitIF.h"
#include "VSMT_RTL_Testbench_DebugIF.h"
#include "VSMT_RTL_Testbench_PerformanceCounterIF.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__pi2.h"
#include "VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10.h"
#include "VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80.h"
#include "VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20.h"
#include "VSMT_RTL_Testbench_NextPCStageIF.h"
#include "VSMT_RTL_Testbench_FetchStageIF.h"
#include "VSMT_RTL_Testbench_DCacheIF.h"
#include "VSMT_RTL_Testbench_RecoveryManagerIF.h"
#include "VSMT_RTL_Testbench_ControllerIF.h"
#include "VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB14.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__R2.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi4.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi6.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi7.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8.h"
#include "VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB1_W1.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi9.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi10.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi12.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi18.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19.h"
#include "VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22.h"
#include "VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24.h"
#include "VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi28.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi29.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi30.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi31.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi32.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi38.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi40.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41.h"
#include "VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi45.h"
#include "VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg.h"
#include "VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg.h"

// DPI TYPES for DPI Export callbacks (Internal use)

// SYMS CLASS (contains all model state)
class alignas(VL_CACHE_LINE_BYTES)VSMT_RTL_Testbench__Syms final : public VerilatedSyms {
  public:
    // INTERNAL STATE
    VSMT_RTL_Testbench* const __Vm_modelp;
    bool __Vm_dumping = false;  // Dumping is active
    VerilatedMutex __Vm_dumperMutex;  // Protect __Vm_dumperp
    VerilatedVcdC* __Vm_dumperp VL_GUARDED_BY(__Vm_dumperMutex) = nullptr;  /// Trace class for $dump*
    bool __Vm_activity = false;  ///< Used by trace routines to determine change occurred
    uint32_t __Vm_baseCode = 0;  ///< Used by trace routines when tracing multiple models
    VlDeleter __Vm_deleter;
    bool __Vm_didInit = false;

    // MODULE INSTANCE STATE
    VSMT_RTL_Testbench___024root   TOP;
    VSMT_RTL_Testbench_DumperTypes__03a__03aKanataDumper__Vclpkg TOP__DumperTypes__03a__03aKanataDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg TOP__DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg TOP__DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg;
    VSMT_RTL_Testbench_DumperTypes__03a__03aSerialDumper__Vclpkg TOP__DumperTypes__03a__03aSerialDumper__Vclpkg;
    VSMT_RTL_Testbench_MemoryTypes TOP__MemoryTypes;
    VSMT_RTL_Testbench_SMT_RTL_Testbench TOP__SMT_RTL_Testbench;
    VSMT_RTL_Testbench_Core        TOP__SMT_RTL_Testbench__core;
    VSMT_RTL_Testbench_ActiveListIF TOP__SMT_RTL_Testbench__core__activeListIF;
    VSMT_RTL_Testbench_BypassNetworkIF TOP__SMT_RTL_Testbench__core__bypassNetworkIF;
    VSMT_RTL_Testbench_CacheFlushManagerIF TOP__SMT_RTL_Testbench__core__cacheFlushManagerIF;
    VSMT_RTL_Testbench_CacheSystemIF TOP__SMT_RTL_Testbench__core__cacheSystemIF;
    VSMT_RTL_Testbench_CommitStageIF TOP__SMT_RTL_Testbench__core__cmStageIF;
    VSMT_RTL_Testbench_ComplexIntegerExecutionStageIF TOP__SMT_RTL_Testbench__core__complexExStageIF;
    VSMT_RTL_Testbench_ComplexIntegerIssueStageIF TOP__SMT_RTL_Testbench__core__complexIsStageIF;
    VSMT_RTL_Testbench_ComplexIntegerRegisterReadStageIF TOP__SMT_RTL_Testbench__core__complexRrStageIF;
    VSMT_RTL_Testbench_CSR_UnitIF  TOP__SMT_RTL_Testbench__core__csrUnitIF;
    VSMT_RTL_Testbench_ControllerIF TOP__SMT_RTL_Testbench__core__ctrlIF;
    VSMT_RTL_Testbench_DCacheIF    TOP__SMT_RTL_Testbench__core__dCache__DOT__port;
    VSMT_RTL_Testbench_DebugIF     TOP__SMT_RTL_Testbench__core__debugIF;
    VSMT_RTL_Testbench_FPDivSqrtUnitIF TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF;
    VSMT_RTL_Testbench_FPExecutionStageIF TOP__SMT_RTL_Testbench__core__fpExStageIF;
    VSMT_RTL_Testbench_FPIssueStageIF TOP__SMT_RTL_Testbench__core__fpIsStageIF;
    VSMT_RTL_Testbench_FPRegisterReadStageIF TOP__SMT_RTL_Testbench__core__fpRrStageIF;
    VSMT_RTL_Testbench_DecodeStageIF TOP__SMT_RTL_Testbench__core__idStageIF;
    VSMT_RTL_Testbench_FetchStageIF TOP__SMT_RTL_Testbench__core__ifStageIF;
    VSMT_RTL_Testbench_IntegerExecutionStageIF TOP__SMT_RTL_Testbench__core__intExStageIF;
    VSMT_RTL_Testbench_IntegerIssueStageIF TOP__SMT_RTL_Testbench__core__intIsStageIF;
    VSMT_RTL_Testbench_IntegerRegisterReadStageIF TOP__SMT_RTL_Testbench__core__intRrStageIF;
    VSMT_RTL_Testbench_IO_UnitIF   TOP__SMT_RTL_Testbench__core__ioUnitIF;
    VSMT_RTL_Testbench_LoadStoreUnitIF TOP__SMT_RTL_Testbench__core__loadStoreUnitIF;
    VSMT_RTL_Testbench_MemoryAccessStageIF TOP__SMT_RTL_Testbench__core__maStageIF;
    VSMT_RTL_Testbench_MemoryExecutionStageIF TOP__SMT_RTL_Testbench__core__memExStageIF;
    VSMT_RTL_Testbench_MemoryIssueStageIF TOP__SMT_RTL_Testbench__core__memIsStageIF;
    VSMT_RTL_Testbench_MemoryRegisterReadStageIF TOP__SMT_RTL_Testbench__core__memRrStageIF;
    VSMT_RTL_Testbench_MemoryTagAccessStageIF TOP__SMT_RTL_Testbench__core__mtStageIF;
    VSMT_RTL_Testbench_MulDivUnitIF TOP__SMT_RTL_Testbench__core__mulDivUnitIF;
    VSMT_RTL_Testbench_NextPCStageIF TOP__SMT_RTL_Testbench__core__npStageIF;
    VSMT_RTL_Testbench_PreDecodeStageIF TOP__SMT_RTL_Testbench__core__pdStageIF;
    VSMT_RTL_Testbench_PerformanceCounterIF TOP__SMT_RTL_Testbench__core__perfCounterIF;
    VSMT_RTL_Testbench_RecoveryManagerIF TOP__SMT_RTL_Testbench__core__recoveryManagerIF;
    VSMT_RTL_Testbench_RegisterFileIF TOP__SMT_RTL_Testbench__core__registerFileIF;
    VSMT_RTL_Testbench_RenameLogicIF TOP__SMT_RTL_Testbench__core__renameLogicIF;
    VSMT_RTL_Testbench_RenameStageIF TOP__SMT_RTL_Testbench__core__rnStageIF;
    VSMT_RTL_Testbench_ScheduleStageIF TOP__SMT_RTL_Testbench__core__scStageIF;
    VSMT_RTL_Testbench_SchedulerIF TOP__SMT_RTL_Testbench__core__schedulerIF;
    VSMT_RTL_Testbench_WakeupSelectIF TOP__SMT_RTL_Testbench__core__wakeupSelectIF;
    VSMT_RTL_Testbench_ActiveList  TOP__SMT_RTL_Testbench__core__activeList;
    VSMT_RTL_Testbench_DistributedMultiBankRAM__R2 TOP__SMT_RTL_Testbench__core__activeList__activeList;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi4 TOP__SMT_RTL_Testbench__core__activeList__execState;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi28 TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body;
    VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47 TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body__genblk1__DOT__lvt;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi6 TOP__SMT_RTL_Testbench__core__activeList__execStateRef;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi30 TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body;
    VSMT_RTL_Testbench_XOR_DistributedMultiPortRAM__pi47 TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body__genblk1__DOT__lvt;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi5 TOP__SMT_RTL_Testbench__core__activeList__fflagsState;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi29 TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body;
    VSMT_RTL_Testbench_BranchPredictor TOP__SMT_RTL_Testbench__core__brPred;
    VSMT_RTL_Testbench_Gshare      TOP__SMT_RTL_Testbench__core__brPred__predictor;
    VSMT_RTL_Testbench_BlockMultiBankRAM__E800_EB2 TOP__SMT_RTL_Testbench__core__brPred__predictor__pht;
    VSMT_RTL_Testbench_BTB         TOP__SMT_RTL_Testbench__core__btb;
    VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB14 TOP__SMT_RTL_Testbench__core__btb__btbEntryArray;
    VSMT_RTL_Testbench_CommitStage TOP__SMT_RTL_Testbench__core__cmStage;
    VSMT_RTL_Testbench_DestinationRAM TOP__SMT_RTL_Testbench__core__destinationRAM;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi15 TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi38 TOP__SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__DOT__body;
    VSMT_RTL_Testbench_IssueQueue  TOP__SMT_RTL_Testbench__core__issueQueue;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi10 TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi12 TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi9 TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM;
    VSMT_RTL_Testbench_MultiWidthFreeList__P8_PB2_I10 TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList;
    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi24 TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi11 TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM;
    VSMT_RTL_Testbench_MemoryDependencyPredictor TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor;
    VSMT_RTL_Testbench_BlockMultiBankRAM__E400_EB1_W1 TOP__SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt;
    VSMT_RTL_Testbench_RegisterFile TOP__SMT_RTL_Testbench__core__registerFile;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi19 TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi41 TOP__SMT_RTL_Testbench__core__registerFile__phyFPReg__genblk1__DOT__body;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi18 TOP__SMT_RTL_Testbench__core__registerFile__phyReg;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi40 TOP__SMT_RTL_Testbench__core__registerFile__phyReg__genblk1__DOT__body;
    VSMT_RTL_Testbench_RenameLogic TOP__SMT_RTL_Testbench__core__renameLogic;
    VSMT_RTL_Testbench_MultiWidthFreeList__pi2 TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList;
    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22 TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList;
    VSMT_RTL_Testbench_MultiWidthFreeList__pi2 TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList;
    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22 TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList;
    VSMT_RTL_Testbench_MultiWidthFreeList__pi2 TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList;
    VSMT_RTL_Testbench_DistributedMultiBankRAM__pi22 TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList;
    VSMT_RTL_Testbench_RetirementRMT TOP__SMT_RTL_Testbench__core__retirementRMT;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi8 TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi32 TOP__SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__DOT__body;
    VSMT_RTL_Testbench_RMT         TOP__SMT_RTL_Testbench__core__rmt_wat;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi7 TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi31 TOP__SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__DOT__body;
    VSMT_RTL_Testbench_StoreQueue  TOP__SMT_RTL_Testbench__core__storeQueue;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi17 TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi39 TOP__SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__DOT__body;
    VSMT_RTL_Testbench_WakeupLogic TOP__SMT_RTL_Testbench__core__wakeupLogic;
    VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80 TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl;
    VSMT_RTL_Testbench_DistributedMultiPortRAM__pi25 TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable;
    VSMT_RTL_Testbench_LVT_DistributedMultiPortRAM__pi45 TOP__SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__DOT__body;
    VSMT_RTL_Testbench_Memory__Iz1 TOP__SMT_RTL_Testbench__memory;
    VSMT_RTL_Testbench_InitializedBlockRAM__pi1 TOP__SMT_RTL_Testbench__memory__body;
    VSMT_RTL_Testbench_InitializedBlockRAM_ForNarrowRequest__pi20 TOP__SMT_RTL_Testbench__memory__body__body__DOT__ram;
    VSMT_RTL_Testbench_DumperTypes TOP__DumperTypes;
    VSMT_RTL_Testbench___024unit   TOP____024unit;

    // SCOPE NAMES
    VerilatedScope __Vscope_MemoryTypes;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList__activeList;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList__execState;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList__execStateRef;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList__execState__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList__fflagsState;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__brPred__predictor__pht;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__btb__btbEntryArray;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__cmStage;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__complexExStage;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__complexExStage__unnamedblk8;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__csrUnit;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__destinationRAM__dstRAM;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__destinationRAM__dstRAM__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__issueQueue__intPayloadRAM;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__issueQueue__memPayloadRAM;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__memoryDependencyPredictor__mdt;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__registerFile__phyFPReg;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__registerFile__phyFPReg__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__registerFile__phyReg;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__registerFile__phyReg__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____scalarFreeList__freeList;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____scalarFreeList__freeList;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__retirementRMT__regRMT;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__retirementRMT__regRMT__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__rmt_wat__regRMT;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__rmt_wat__regRMT__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__storeQueue__storeQueueData;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__storeQueue__storeQueueData__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable;
    VerilatedScope __Vscope_SMT_RTL_Testbench__core__wakeupLogic__regReadyBitTbl__radyBitTable__genblk1__body;
    VerilatedScope __Vscope_SMT_RTL_Testbench__memory__body__body__ram;
    VerilatedScope __Vscope___024unit;
    VerilatedScope __Vscope___024unit__RISCV_EmitSystemOp;
    VerilatedScope __Vscope___024unit__RISCV_EmitZba;

    // CONSTRUCTORS
    VSMT_RTL_Testbench__Syms(VerilatedContext* contextp, const char* namep, VSMT_RTL_Testbench* modelp);
    ~VSMT_RTL_Testbench__Syms();

    // METHODS
    const char* name() { return TOP.name(); }
    void _traceDump();
    void _traceDumpOpen();
    void _traceDumpClose();
};

#endif  // guard
