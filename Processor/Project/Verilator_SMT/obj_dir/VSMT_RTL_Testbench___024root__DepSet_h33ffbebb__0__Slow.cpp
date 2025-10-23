// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench___024root.h"

VL_ATTR_COLD void VSMT_RTL_Testbench___024root___eval_final(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___eval_final\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
}

#ifdef VL_DEBUG
VL_ATTR_COLD void VSMT_RTL_Testbench___024root___dump_triggers__stl(VSMT_RTL_Testbench___024root* vlSelf);
#endif  // VL_DEBUG
VL_ATTR_COLD bool VSMT_RTL_Testbench___024root___eval_phase__stl(VSMT_RTL_Testbench___024root* vlSelf);

VL_ATTR_COLD void VSMT_RTL_Testbench___024root___eval_settle(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___eval_settle\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*31:0*/ __VstlIterCount;
    CData/*0:0*/ __VstlContinue;
    // Body
    __VstlIterCount = 0U;
    vlSelfRef.__VstlFirstIteration = 1U;
    __VstlContinue = 1U;
    while (__VstlContinue) {
        if (VL_UNLIKELY(((0x64U < __VstlIterCount)))) {
#ifdef VL_DEBUG
            VSMT_RTL_Testbench___024root___dump_triggers__stl(vlSelf);
#endif
            VL_FATAL_MT("SMT_RTL_Testbench.sv", 6, "", "Settle region did not converge.");
        }
        __VstlIterCount = ((IData)(1U) + __VstlIterCount);
        __VstlContinue = 0U;
        if (VSMT_RTL_Testbench___024root___eval_phase__stl(vlSelf)) {
            __VstlContinue = 1U;
        }
        vlSelfRef.__VstlFirstIteration = 0U;
    }
}

#ifdef VL_DEBUG
VL_ATTR_COLD void VSMT_RTL_Testbench___024root___dump_triggers__stl(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___dump_triggers__stl\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U & (~ vlSelfRef.__VstlTriggered.any()))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if ((1ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 0 is active: Internal 'stl' trigger - first iteration\n");
    }
    if ((2ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 1 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatable)\n");
    }
    if ((4ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 2 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.allocatable)\n");
    }
    if ((8ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 3 is active: @([hybrid] SMT_RTL_Testbench.core.rnStage.serialize)\n");
    }
    if ((0x10ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 4 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.idStageStallUpper)\n");
    }
    if ((0x20ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 5 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.ifStageSendBubbleLower)\n");
    }
    if ((0x40ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 6 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.npStageSendBubbleLowerForInterrupt)\n");
    }
    if ((0x80ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 7 is active: @([hybrid] SMT_RTL_Testbench.core.ifStageIF.brPredTaken)\n");
    }
    if ((0x100ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 8 is active: @([hybrid] SMT_RTL_Testbench.core.ifStageIF.updateBrHistory)\n");
    }
    if ((0x200ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 9 is active: @([hybrid] SMT_RTL_Testbench.core.brPred.predictor.phtRV)\n");
    }
    if ((0x400ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 10 is active: @([hybrid] SMT_RTL_Testbench.core.iCache.hitArray)\n");
    }
    if ((0x800ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 11 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegA)\n");
    }
    if ((0x1000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 12 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegB)\n");
    }
    if ((0x2000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 13 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegC)\n");
    }
    if ((0x4000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 14 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phyDstReg)\n");
    }
    if ((0x8000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 15 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phyPrevDstReg)\n");
    }
    if ((0x10000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 16 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegA)\n");
    }
    if ((0x20000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 17 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegB)\n");
    }
    if ((0x40000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 18 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegC)\n");
    }
    if ((0x80000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 19 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.prevDependIssueQueuePtr)\n");
    }
    if ((0x100000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 20 is active: @([hybrid] SMT_RTL_Testbench.core.activeListIF.pushedTailPtr)\n");
    }
    if ((0x200000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 21 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatedLoadQueuePtr)\n");
    }
    if ((0x400000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 22 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatedStoreQueuePtr)\n");
    }
    if ((0x800000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 23 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.flushNum)\n");
    }
    if ((0x1000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 24 is active: @([hybrid] SMT_RTL_Testbench.core.recoveryManagerIF.renameLogicRecoveryRMT)\n");
    }
    if ((0x2000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 25 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.fflags)\n");
    }
    if ((0x4000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 26 is active: @([hybrid] SMT_RTL_Testbench.core.activeListIF.headExecState)\n");
    }
    if ((0x8000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 27 is active: @([hybrid] SMT_RTL_Testbench.core.schedulerIF.selected)\n");
    }
    if ((0x10000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 28 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divBusy)\n");
    }
    if ((0x20000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 29 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Busy)\n");
    }
    if ((0x40000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 30 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intCtrlOut)\n");
    }
    if ((0x80000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 31 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.shiftDataOut)\n");
    }
    if ((0x100000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 32 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.aluDataOut)\n");
    }
    if ((0x200000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 33 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intCtrlIn)\n");
    }
    if ((0x400000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 34 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intDstRegDataOut)\n");
    }
    if ((0x800000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 35 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.complexCtrlIn)\n");
    }
    if ((0x1000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 36 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.memCtrlIn)\n");
    }
    if ((0x2000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 37 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.fpCtrlIn)\n");
    }
    if ((0x4000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 38 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.BlockALU[0].intALU.adderDst)\n");
    }
    if ((0x8000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 39 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.BlockALU[1].intALU.adderDst)\n");
    }
    if ((0x10000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 40 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.complexCtrlOut)\n");
    }
    if ((0x20000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 41 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divReserved)\n");
    }
    if ((0x40000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 42 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divRelease)\n");
    }
    if ((0x80000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 43 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divDataOut)\n");
    }
    if ((0x100000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 44 is active: @([hybrid] SMT_RTL_Testbench.core.registerFileIF.memSrcRegNumB)\n");
    }
    if ((0x200000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 45 is active: @([hybrid] SMT_RTL_Testbench.core.registerFileIF.fpSrcRegNumA)\n");
    }
    if ((0x400000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 46 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.memCtrlOut)\n");
    }
    if ((0x800000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 47 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.dcReadAddr)\n");
    }
    if ((0x1000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 48 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.storeLoadForwarded)\n");
    }
    if ((0x2000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 49 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.forwardMiss)\n");
    }
    if ((0x4000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 50 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.conflict)\n");
    }
    if ((0x8000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 51 is active: @([hybrid] SMT_RTL_Testbench.core.ioUnitIF.ioReadAddrIn)\n");
    }
    if ((0x10000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 52 is active: @([hybrid] SMT_RTL_Testbench.core.loadQueue.picked)\n");
    }
    if ((0x20000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 53 is active: @([hybrid] SMT_RTL_Testbench.core.loadQueue.pickedPtr)\n");
    }
    if ((0x40000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 54 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.retiredStoreQueuePtr)\n");
    }
    if ((0x80000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 55 is active: @([hybrid] SMT_RTL_Testbench.core.storeQueue.pickedPtr)\n");
    }
    if ((0x100000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 56 is active: @([hybrid] SMT_RTL_Testbench.core.storeCommitter.stallStoreTagStage)\n");
    }
    if ((0x200000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 57 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.lsuCacheGrt)\n");
    }
    if ((0x400000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 58 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR)\n");
    }
    if ((0x800000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 59 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR_Addr)\n");
    }
    if ((0x1000000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 60 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR_ActiveListPtr)\n");
    }
    if ((0x2000000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 61 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.isAllocatedByStore)\n");
    }
    if ((0x4000000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 62 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.isUncachable)\n");
    }
    if ((0x8000000000000000ULL & vlSelfRef.__VstlTriggered.word(0U))) {
        VL_DBG_MSGF("         'stl' region trigger index 63 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheGrt)\n");
    }
    if ((1ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 64 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheMuxTagOut)\n");
    }
    if ((2ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 65 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheMuxDataOut)\n");
    }
    if ((4ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 66 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrMemMuxOut)\n");
    }
    if ((8ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 67 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.dcFlushReqAck)\n");
    }
    if ((0x10ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 68 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.cacheArrayInSel)\n");
    }
    if ((0x20ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 69 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrMemReq)\n");
    }
    if ((0x40ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 70 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.fpCtrlOut)\n");
    }
    if ((0x80ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 71 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Reserved)\n");
    }
    if ((0x100ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 72 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Release)\n");
    }
    if ((0x200ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 73 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.triggerExcpt)\n");
    }
    if ((0x400ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 74 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.triggerInterrupt)\n");
    }
    if ((0x800ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 75 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.interruptCode)\n");
    }
    if ((0x1000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 76 is active: @([hybrid] SMT_RTL_Testbench.core.registerFile.srcRegNum)\n");
    }
    if ((0x2000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 77 is active: @([hybrid] SMT_RTL_Testbench.core.registerFile.srcFPRegNum)\n");
    }
    if ((0x4000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 78 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.activeList.ra)\n");
    }
    if ((0x8000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 79 is active: @([hybrid] SMT_RTL_Testbench.core.btb.btbEntryArray.ra)\n");
    }
    if ((0x10000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 80 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.issueQueueFreeList.poppedData)\n");
    }
    if ((0x20000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 81 is active: @([hybrid] SMT_RTL_Testbench.core.wakeupLogic.opMatrixReady)\n");
    }
    if ((0x40000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 82 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.activeList.genblk1.rBank.raBank)\n");
    }
    if ((0x80000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 83 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.rv)\n");
    }
    if ((0x100000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 84 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.rv)\n");
    }
    if ((0x200000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 85 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.rv)\n");
    }
    if ((0x400000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 86 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.intPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x800000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 87 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.complexPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x1000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 88 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.memPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x2000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 89 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.fpPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x4000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 90 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.scalarFPFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x8000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 91 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.genblk1[0].scalarFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x10000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 92 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.genblk1[1].scalarFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x20000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 93 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.issueQueueFreeList.freeList.genblk1.rBank.rvBank)\n");
    }
    if ((0x40000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 94 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x80000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 95 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x100000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 96 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x200000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 97 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x400000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 98 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x800000000ULL & vlSelfRef.__VstlTriggered.word(1U))) {
        VL_DBG_MSGF("         'stl' region trigger index 99 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.genblk1.lvo)\n");
    }
}
#endif  // VL_DEBUG

VL_ATTR_COLD void VSMT_RTL_Testbench___024root___eval_triggers__stl(VSMT_RTL_Testbench___024root* vlSelf);
VL_ATTR_COLD void VSMT_RTL_Testbench___024root___eval_stl(VSMT_RTL_Testbench___024root* vlSelf);

VL_ATTR_COLD bool VSMT_RTL_Testbench___024root___eval_phase__stl(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___eval_phase__stl\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __VstlExecute;
    // Body
    VSMT_RTL_Testbench___024root___eval_triggers__stl(vlSelf);
    __VstlExecute = vlSelfRef.__VstlTriggered.any();
    if (__VstlExecute) {
        VSMT_RTL_Testbench___024root___eval_stl(vlSelf);
    }
    return (__VstlExecute);
}

#ifdef VL_DEBUG
VL_ATTR_COLD void VSMT_RTL_Testbench___024root___dump_triggers__ico(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___dump_triggers__ico\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U & (~ vlSelfRef.__VicoTriggered.any()))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if ((1ULL & vlSelfRef.__VicoTriggered.word(0U))) {
        VL_DBG_MSGF("         'ico' region trigger index 0 is active: Internal 'ico' trigger - first iteration\n");
    }
    if ((2ULL & vlSelfRef.__VicoTriggered.word(0U))) {
        VL_DBG_MSGF("         'ico' region trigger index 1 is active: @([hybrid] SMT_RTL_Testbench.core.recoveryManagerIF.renameLogicRecoveryRMT)\n");
    }
}
#endif  // VL_DEBUG

#ifdef VL_DEBUG
VL_ATTR_COLD void VSMT_RTL_Testbench___024root___dump_triggers__act(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___dump_triggers__act\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U & (~ vlSelfRef.__VactTriggered.any()))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if ((1ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 0 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatable)\n");
    }
    if ((2ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 1 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.allocatable)\n");
    }
    if ((4ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 2 is active: @([hybrid] SMT_RTL_Testbench.core.rnStage.serialize)\n");
    }
    if ((8ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 3 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.idStageStallUpper)\n");
    }
    if ((0x10ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 4 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.ifStageSendBubbleLower)\n");
    }
    if ((0x20ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 5 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.npStageSendBubbleLowerForInterrupt)\n");
    }
    if ((0x40ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 6 is active: @([hybrid] SMT_RTL_Testbench.core.ifStageIF.brPredTaken)\n");
    }
    if ((0x80ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 7 is active: @([hybrid] SMT_RTL_Testbench.core.ifStageIF.updateBrHistory)\n");
    }
    if ((0x100ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 8 is active: @([hybrid] SMT_RTL_Testbench.core.brPred.predictor.phtRV)\n");
    }
    if ((0x200ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 9 is active: @([hybrid] SMT_RTL_Testbench.core.iCache.hitArray)\n");
    }
    if ((0x400ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 10 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegA)\n");
    }
    if ((0x800ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 11 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegB)\n");
    }
    if ((0x1000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 12 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegC)\n");
    }
    if ((0x2000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 13 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phyDstReg)\n");
    }
    if ((0x4000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 14 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phyPrevDstReg)\n");
    }
    if ((0x8000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 15 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegA)\n");
    }
    if ((0x10000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 16 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegB)\n");
    }
    if ((0x20000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 17 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegC)\n");
    }
    if ((0x40000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 18 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.prevDependIssueQueuePtr)\n");
    }
    if ((0x80000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 19 is active: @([hybrid] SMT_RTL_Testbench.core.activeListIF.pushedTailPtr)\n");
    }
    if ((0x100000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 20 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatedLoadQueuePtr)\n");
    }
    if ((0x200000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 21 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatedStoreQueuePtr)\n");
    }
    if ((0x400000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 22 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.flushNum)\n");
    }
    if ((0x800000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 23 is active: @([hybrid] SMT_RTL_Testbench.core.recoveryManagerIF.renameLogicRecoveryRMT)\n");
    }
    if ((0x1000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 24 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.fflags)\n");
    }
    if ((0x2000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 25 is active: @([hybrid] SMT_RTL_Testbench.core.activeListIF.headExecState)\n");
    }
    if ((0x4000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 26 is active: @([hybrid] SMT_RTL_Testbench.core.schedulerIF.selected)\n");
    }
    if ((0x8000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 27 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divBusy)\n");
    }
    if ((0x10000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 28 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Busy)\n");
    }
    if ((0x20000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 29 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intCtrlOut)\n");
    }
    if ((0x40000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 30 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.shiftDataOut)\n");
    }
    if ((0x80000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 31 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.aluDataOut)\n");
    }
    if ((0x100000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 32 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intCtrlIn)\n");
    }
    if ((0x200000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 33 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intDstRegDataOut)\n");
    }
    if ((0x400000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 34 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.complexCtrlIn)\n");
    }
    if ((0x800000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 35 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.memCtrlIn)\n");
    }
    if ((0x1000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 36 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.fpCtrlIn)\n");
    }
    if ((0x2000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 37 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.BlockALU[0].intALU.adderDst)\n");
    }
    if ((0x4000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 38 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.BlockALU[1].intALU.adderDst)\n");
    }
    if ((0x8000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 39 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.complexCtrlOut)\n");
    }
    if ((0x10000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 40 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divReserved)\n");
    }
    if ((0x20000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 41 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divRelease)\n");
    }
    if ((0x40000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 42 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divDataOut)\n");
    }
    if ((0x80000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 43 is active: @([hybrid] SMT_RTL_Testbench.core.registerFileIF.memSrcRegNumB)\n");
    }
    if ((0x100000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 44 is active: @([hybrid] SMT_RTL_Testbench.core.registerFileIF.fpSrcRegNumA)\n");
    }
    if ((0x200000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 45 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.memCtrlOut)\n");
    }
    if ((0x400000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 46 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.dcReadAddr)\n");
    }
    if ((0x800000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 47 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.storeLoadForwarded)\n");
    }
    if ((0x1000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 48 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.forwardMiss)\n");
    }
    if ((0x2000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 49 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.conflict)\n");
    }
    if ((0x4000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 50 is active: @([hybrid] SMT_RTL_Testbench.core.ioUnitIF.ioReadAddrIn)\n");
    }
    if ((0x8000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 51 is active: @([hybrid] SMT_RTL_Testbench.core.loadQueue.picked)\n");
    }
    if ((0x10000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 52 is active: @([hybrid] SMT_RTL_Testbench.core.loadQueue.pickedPtr)\n");
    }
    if ((0x20000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 53 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.retiredStoreQueuePtr)\n");
    }
    if ((0x40000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 54 is active: @([hybrid] SMT_RTL_Testbench.core.storeQueue.pickedPtr)\n");
    }
    if ((0x80000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 55 is active: @([hybrid] SMT_RTL_Testbench.core.storeCommitter.stallStoreTagStage)\n");
    }
    if ((0x100000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 56 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.lsuCacheGrt)\n");
    }
    if ((0x200000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 57 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR)\n");
    }
    if ((0x400000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 58 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR_Addr)\n");
    }
    if ((0x800000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 59 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR_ActiveListPtr)\n");
    }
    if ((0x1000000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 60 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.isAllocatedByStore)\n");
    }
    if ((0x2000000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 61 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.isUncachable)\n");
    }
    if ((0x4000000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 62 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheGrt)\n");
    }
    if ((0x8000000000000000ULL & vlSelfRef.__VactTriggered.word(0U))) {
        VL_DBG_MSGF("         'act' region trigger index 63 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheMuxTagOut)\n");
    }
    if ((1ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 64 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheMuxDataOut)\n");
    }
    if ((2ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 65 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrMemMuxOut)\n");
    }
    if ((4ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 66 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.dcFlushReqAck)\n");
    }
    if ((8ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 67 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.cacheArrayInSel)\n");
    }
    if ((0x10ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 68 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrMemReq)\n");
    }
    if ((0x20ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 69 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.fpCtrlOut)\n");
    }
    if ((0x40ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 70 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Reserved)\n");
    }
    if ((0x80ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 71 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Release)\n");
    }
    if ((0x100ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 72 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.triggerExcpt)\n");
    }
    if ((0x200ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 73 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.triggerInterrupt)\n");
    }
    if ((0x400ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 74 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.interruptCode)\n");
    }
    if ((0x800ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 75 is active: @([hybrid] SMT_RTL_Testbench.core.registerFile.srcRegNum)\n");
    }
    if ((0x1000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 76 is active: @([hybrid] SMT_RTL_Testbench.core.registerFile.srcFPRegNum)\n");
    }
    if ((0x2000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 77 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.activeList.ra)\n");
    }
    if ((0x4000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 78 is active: @([hybrid] SMT_RTL_Testbench.core.btb.btbEntryArray.ra)\n");
    }
    if ((0x8000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 79 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.issueQueueFreeList.poppedData)\n");
    }
    if ((0x10000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 80 is active: @([hybrid] SMT_RTL_Testbench.core.wakeupLogic.opMatrixReady)\n");
    }
    if ((0x20000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 81 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.activeList.genblk1.rBank.raBank)\n");
    }
    if ((0x40000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 82 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.rv)\n");
    }
    if ((0x80000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 83 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.rv)\n");
    }
    if ((0x100000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 84 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.rv)\n");
    }
    if ((0x200000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 85 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.intPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x400000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 86 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.complexPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x800000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 87 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.memPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x1000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 88 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.fpPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x2000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 89 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.scalarFPFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x4000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 90 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.genblk1[0].scalarFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x8000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 91 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.genblk1[1].scalarFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x10000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 92 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.issueQueueFreeList.freeList.genblk1.rBank.rvBank)\n");
    }
    if ((0x20000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 93 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x40000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 94 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x80000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 95 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x100000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 96 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x200000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 97 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x400000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 98 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x800000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 99 is active: @(posedge SMT_RTL_Testbench.clk)\n");
    }
    if ((0x1000000000ULL & vlSelfRef.__VactTriggered.word(1U))) {
        VL_DBG_MSGF("         'act' region trigger index 100 is active: @([true] __VdlySched.awaitingCurrentTime())\n");
    }
}
#endif  // VL_DEBUG

#ifdef VL_DEBUG
VL_ATTR_COLD void VSMT_RTL_Testbench___024root___dump_triggers__nba(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___dump_triggers__nba\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    if ((1U & (~ vlSelfRef.__VnbaTriggered.any()))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if ((1ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 0 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatable)\n");
    }
    if ((2ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 1 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.allocatable)\n");
    }
    if ((4ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 2 is active: @([hybrid] SMT_RTL_Testbench.core.rnStage.serialize)\n");
    }
    if ((8ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 3 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.idStageStallUpper)\n");
    }
    if ((0x10ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 4 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.ifStageSendBubbleLower)\n");
    }
    if ((0x20ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 5 is active: @([hybrid] SMT_RTL_Testbench.core.ctrlIF.npStageSendBubbleLowerForInterrupt)\n");
    }
    if ((0x40ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 6 is active: @([hybrid] SMT_RTL_Testbench.core.ifStageIF.brPredTaken)\n");
    }
    if ((0x80ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 7 is active: @([hybrid] SMT_RTL_Testbench.core.ifStageIF.updateBrHistory)\n");
    }
    if ((0x100ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 8 is active: @([hybrid] SMT_RTL_Testbench.core.brPred.predictor.phtRV)\n");
    }
    if ((0x200ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 9 is active: @([hybrid] SMT_RTL_Testbench.core.iCache.hitArray)\n");
    }
    if ((0x400ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 10 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegA)\n");
    }
    if ((0x800ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 11 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegB)\n");
    }
    if ((0x1000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 12 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phySrcRegC)\n");
    }
    if ((0x2000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 13 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phyDstReg)\n");
    }
    if ((0x4000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 14 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.phyPrevDstReg)\n");
    }
    if ((0x8000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 15 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegA)\n");
    }
    if ((0x10000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 16 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegB)\n");
    }
    if ((0x20000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 17 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.srcIssueQueuePtrRegC)\n");
    }
    if ((0x40000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 18 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.prevDependIssueQueuePtr)\n");
    }
    if ((0x80000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 19 is active: @([hybrid] SMT_RTL_Testbench.core.activeListIF.pushedTailPtr)\n");
    }
    if ((0x100000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 20 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatedLoadQueuePtr)\n");
    }
    if ((0x200000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 21 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.allocatedStoreQueuePtr)\n");
    }
    if ((0x400000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 22 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogicIF.flushNum)\n");
    }
    if ((0x800000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 23 is active: @([hybrid] SMT_RTL_Testbench.core.recoveryManagerIF.renameLogicRecoveryRMT)\n");
    }
    if ((0x1000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 24 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.fflags)\n");
    }
    if ((0x2000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 25 is active: @([hybrid] SMT_RTL_Testbench.core.activeListIF.headExecState)\n");
    }
    if ((0x4000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 26 is active: @([hybrid] SMT_RTL_Testbench.core.schedulerIF.selected)\n");
    }
    if ((0x8000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 27 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divBusy)\n");
    }
    if ((0x10000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 28 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Busy)\n");
    }
    if ((0x20000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 29 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intCtrlOut)\n");
    }
    if ((0x40000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 30 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.shiftDataOut)\n");
    }
    if ((0x80000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 31 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.aluDataOut)\n");
    }
    if ((0x100000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 32 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intCtrlIn)\n");
    }
    if ((0x200000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 33 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.intDstRegDataOut)\n");
    }
    if ((0x400000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 34 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.complexCtrlIn)\n");
    }
    if ((0x800000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 35 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.memCtrlIn)\n");
    }
    if ((0x1000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 36 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.fpCtrlIn)\n");
    }
    if ((0x2000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 37 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.BlockALU[0].intALU.adderDst)\n");
    }
    if ((0x4000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 38 is active: @([hybrid] SMT_RTL_Testbench.core.intExStage.BlockALU[1].intALU.adderDst)\n");
    }
    if ((0x8000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 39 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.complexCtrlOut)\n");
    }
    if ((0x10000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 40 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divReserved)\n");
    }
    if ((0x20000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 41 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divRelease)\n");
    }
    if ((0x40000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 42 is active: @([hybrid] SMT_RTL_Testbench.core.mulDivUnitIF.divDataOut)\n");
    }
    if ((0x80000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 43 is active: @([hybrid] SMT_RTL_Testbench.core.registerFileIF.memSrcRegNumB)\n");
    }
    if ((0x100000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 44 is active: @([hybrid] SMT_RTL_Testbench.core.registerFileIF.fpSrcRegNumA)\n");
    }
    if ((0x200000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 45 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.memCtrlOut)\n");
    }
    if ((0x400000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 46 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.dcReadAddr)\n");
    }
    if ((0x800000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 47 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.storeLoadForwarded)\n");
    }
    if ((0x1000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 48 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.forwardMiss)\n");
    }
    if ((0x2000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 49 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.conflict)\n");
    }
    if ((0x4000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 50 is active: @([hybrid] SMT_RTL_Testbench.core.ioUnitIF.ioReadAddrIn)\n");
    }
    if ((0x8000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 51 is active: @([hybrid] SMT_RTL_Testbench.core.loadQueue.picked)\n");
    }
    if ((0x10000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 52 is active: @([hybrid] SMT_RTL_Testbench.core.loadQueue.pickedPtr)\n");
    }
    if ((0x20000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 53 is active: @([hybrid] SMT_RTL_Testbench.core.loadStoreUnitIF.retiredStoreQueuePtr)\n");
    }
    if ((0x40000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 54 is active: @([hybrid] SMT_RTL_Testbench.core.storeQueue.pickedPtr)\n");
    }
    if ((0x80000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 55 is active: @([hybrid] SMT_RTL_Testbench.core.storeCommitter.stallStoreTagStage)\n");
    }
    if ((0x100000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 56 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.lsuCacheGrt)\n");
    }
    if ((0x200000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 57 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR)\n");
    }
    if ((0x400000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 58 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR_Addr)\n");
    }
    if ((0x800000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 59 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.initMSHR_ActiveListPtr)\n");
    }
    if ((0x1000000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 60 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.isAllocatedByStore)\n");
    }
    if ((0x2000000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 61 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.isUncachable)\n");
    }
    if ((0x4000000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 62 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheGrt)\n");
    }
    if ((0x8000000000000000ULL & vlSelfRef.__VnbaTriggered.word(0U))) {
        VL_DBG_MSGF("         'nba' region trigger index 63 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheMuxTagOut)\n");
    }
    if ((1ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 64 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrCacheMuxDataOut)\n");
    }
    if ((2ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 65 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrMemMuxOut)\n");
    }
    if ((4ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 66 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.dcFlushReqAck)\n");
    }
    if ((8ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 67 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.cacheArrayInSel)\n");
    }
    if ((0x10ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 68 is active: @([hybrid] SMT_RTL_Testbench.core.dCache.port.mshrMemReq)\n");
    }
    if ((0x20ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 69 is active: @([hybrid] SMT_RTL_Testbench.core.bypassNetworkIF.fpCtrlOut)\n");
    }
    if ((0x40ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 70 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Reserved)\n");
    }
    if ((0x80ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 71 is active: @([hybrid] SMT_RTL_Testbench.core.fpDivSqrtUnitIF.Release)\n");
    }
    if ((0x100ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 72 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.triggerExcpt)\n");
    }
    if ((0x200ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 73 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.triggerInterrupt)\n");
    }
    if ((0x400ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 74 is active: @([hybrid] SMT_RTL_Testbench.core.csrUnitIF.interruptCode)\n");
    }
    if ((0x800ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 75 is active: @([hybrid] SMT_RTL_Testbench.core.registerFile.srcRegNum)\n");
    }
    if ((0x1000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 76 is active: @([hybrid] SMT_RTL_Testbench.core.registerFile.srcFPRegNum)\n");
    }
    if ((0x2000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 77 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.activeList.ra)\n");
    }
    if ((0x4000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 78 is active: @([hybrid] SMT_RTL_Testbench.core.btb.btbEntryArray.ra)\n");
    }
    if ((0x8000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 79 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.issueQueueFreeList.poppedData)\n");
    }
    if ((0x10000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 80 is active: @([hybrid] SMT_RTL_Testbench.core.wakeupLogic.opMatrixReady)\n");
    }
    if ((0x20000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 81 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.activeList.genblk1.rBank.raBank)\n");
    }
    if ((0x40000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 82 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.rv)\n");
    }
    if ((0x80000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 83 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.rv)\n");
    }
    if ((0x100000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 84 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.rv)\n");
    }
    if ((0x200000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 85 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.intPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x400000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 86 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.complexPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x800000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 87 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.memPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x1000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 88 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.fpPayloadRAM.genblk1.body.rbReadAddr)\n");
    }
    if ((0x2000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 89 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.scalarFPFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x4000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 90 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.genblk1[0].scalarFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x8000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 91 is active: @([hybrid] SMT_RTL_Testbench.core.renameLogic.genblk1[1].scalarFreeList.freeList.genblk1.rBank.raBank)\n");
    }
    if ((0x10000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 92 is active: @([hybrid] SMT_RTL_Testbench.core.issueQueue.issueQueueFreeList.freeList.genblk1.rBank.rvBank)\n");
    }
    if ((0x20000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 93 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x40000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 94 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execState.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x80000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 95 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x100000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 96 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.fflagsState.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x200000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 97 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.genblk1.rvBank)\n");
    }
    if ((0x400000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 98 is active: @([hybrid] SMT_RTL_Testbench.core.activeList.execStateRef.genblk1.body.genblk1.lvo)\n");
    }
    if ((0x800000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 99 is active: @(posedge SMT_RTL_Testbench.clk)\n");
    }
    if ((0x1000000000ULL & vlSelfRef.__VnbaTriggered.word(1U))) {
        VL_DBG_MSGF("         'nba' region trigger index 100 is active: @([true] __VdlySched.awaitingCurrentTime())\n");
    }
}
#endif  // VL_DEBUG

VL_ATTR_COLD void VSMT_RTL_Testbench___024root____Vm_traceActivitySetAll(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root____Vm_traceActivitySetAll\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    IData/*31:0*/ __Vilp1;
    __Vilp1 = 0U;
    while ((__Vilp1 <= 0xaeU)) {
        vlSelfRef.__Vm_traceActivity[__Vilp1] = 1U;
        __Vilp1 = ((IData)(1U) + __Vilp1);
    }
}

VL_ATTR_COLD void VSMT_RTL_Testbench___024root___ctor_var_reset(VSMT_RTL_Testbench___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    VSMT_RTL_Testbench___024root___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatable__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__allocatable__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__rnStage__DOT__serialize__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__idStageStallUpper__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__ifStageSendBubbleLower__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__npStageSendBubbleLowerForInterrupt__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__brPredTaken__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__updateBrHistory__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__brPred__predictor____PVT__phtRV__0[__Vi0] = VL_RAND_RESET_I(2);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__iCache__DOT__hitArray__0 = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegA__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegB__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegC__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyDstReg__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyPrevDstReg__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegA__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegB__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegC__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__prevDependIssueQueuePtr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__pushedTailPtr__0[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedLoadQueuePtr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedStoreQueuePtr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__flushNum__0 = VL_RAND_RESET_I(2);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__recoveryManagerIF____PVT__renameLogicRecoveryRMT__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__fflags__0 = VL_RAND_RESET_I(5);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__headExecState__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__schedulerIF____PVT__selected__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divBusy__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Busy__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlOut__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__shiftDataOut__0[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__aluDataOut__0[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlIn__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intDstRegDataOut__0[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlIn__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlIn__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlIn__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst__0 = VL_RAND_RESET_Q(33);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst__0 = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlOut__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divReserved__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divRelease__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divDataOut__0[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__memSrcRegNumB__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__fpSrcRegNumA__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlOut__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__dcReadAddr__0[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__storeLoadForwarded__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__forwardMiss__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__conflict__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ioUnitIF____PVT__ioReadAddrIn__0 = VL_RAND_RESET_I(22);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__picked__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__pickedPtr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__retiredStoreQueuePtr__0 = VL_RAND_RESET_I(4);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__storeQueue____PVT__pickedPtr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__storeCommitter__DOT__stallStoreTagStage__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__lsuCacheGrt__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_Addr__0[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_ActiveListPtr__0[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isAllocatedByStore__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isUncachable__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheGrt__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxTagOut__0[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(66, vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxDataOut__0[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemMuxOut__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__dcFlushReqAck__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__cacheArrayInSel__0[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemReq__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlOut__0[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Reserved__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Release__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerExcpt__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerInterrupt__0 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__interruptCode__0 = VL_RAND_RESET_I(5);
    for (int __Vi0 = 0; __Vi0 < 11; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcRegNum__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcFPRegNum__0[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__ra__0[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__btb__btbEntryArray____PVT__ra__0[__Vi0] = VL_RAND_RESET_I(10);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList____PVT__poppedData__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__wakeupLogic____PVT__opMatrixReady__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__genblk1__DOT__rBank__DOT__raBank__0[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__rv__0[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__rv__0[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__rv__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__0[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__0[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__0[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__rvBank__0[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 8; ++__Vi1) {
            vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__0[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__0[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__0[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__0[__Vi0][__Vi1] = VL_RAND_RESET_I(5);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 8; ++__Vi1) {
            vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__0[__Vi0][__Vi1] = VL_RAND_RESET_I(4);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__lvo__0[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__VstlDidInit = 0;
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__recoveryManagerIF____PVT__renameLogicRecoveryRMT__1 = VL_RAND_RESET_I(1);
    vlSelf->__VicoDidInit = 0;
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatable__1 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__allocatable__1 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__rnStage__DOT__serialize__1 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__idStageStallUpper__1 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__ifStageSendBubbleLower__1 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ctrlIF____PVT__npStageSendBubbleLowerForInterrupt__1 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__brPredTaken__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ifStageIF____PVT__updateBrHistory__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__brPred__predictor____PVT__phtRV__1[__Vi0] = VL_RAND_RESET_I(2);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__iCache__DOT__hitArray__1 = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegA__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegB__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phySrcRegC__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyDstReg__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__phyPrevDstReg__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegA__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegB__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__srcIssueQueuePtrRegC__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__prevDependIssueQueuePtr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__pushedTailPtr__1[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedLoadQueuePtr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__allocatedStoreQueuePtr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogicIF____PVT__flushNum__1 = VL_RAND_RESET_I(2);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__recoveryManagerIF____PVT__renameLogicRecoveryRMT__2 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__fflags__1 = VL_RAND_RESET_I(5);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeListIF____PVT__headExecState__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__schedulerIF____PVT__selected__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divBusy__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Busy__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlOut__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__shiftDataOut__1[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__aluDataOut__1[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intCtrlIn__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__intDstRegDataOut__1[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlIn__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlIn__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlIn__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst__1 = VL_RAND_RESET_Q(33);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst__1 = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__complexCtrlOut__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divReserved__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divRelease__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__mulDivUnitIF____PVT__divDataOut__1[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__memSrcRegNumB__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFileIF____PVT__fpSrcRegNumA__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__memCtrlOut__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__dcReadAddr__1[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__storeLoadForwarded__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__forwardMiss__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__conflict__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__ioUnitIF____PVT__ioReadAddrIn__1 = VL_RAND_RESET_I(22);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__picked__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__loadQueue__DOT__pickedPtr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__loadStoreUnitIF____PVT__retiredStoreQueuePtr__1 = VL_RAND_RESET_I(4);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__storeQueue____PVT__pickedPtr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core____PVT__storeCommitter__DOT__stallStoreTagStage__1 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__lsuCacheGrt__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_Addr__1[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__initMSHR_ActiveListPtr__1[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isAllocatedByStore__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__isUncachable__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheGrt__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxTagOut__1[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(66, vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrCacheMuxDataOut__1[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemMuxOut__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__dcFlushReqAck__1 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__cacheArrayInSel__1[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__dCache__DOT__port____PVT__mshrMemReq__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__bypassNetworkIF____PVT__fpCtrlOut__1[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Reserved__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__fpDivSqrtUnitIF____PVT__Release__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerExcpt__1 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__triggerInterrupt__1 = VL_RAND_RESET_I(1);
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__csrUnitIF____PVT__interruptCode__1 = VL_RAND_RESET_I(5);
    for (int __Vi0 = 0; __Vi0 < 11; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcRegNum__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 5; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__registerFile____PVT__srcFPRegNum__1[__Vi0] = VL_RAND_RESET_I(7);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__ra__1[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__btb__btbEntryArray____PVT__ra__1[__Vi0] = VL_RAND_RESET_I(10);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList____PVT__poppedData__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__wakeupLogic____PVT__opMatrixReady__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__activeList____PVT__genblk1__DOT__rBank__DOT__raBank__1[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__rv__1[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__rv__1[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__rv__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM____PVT__genblk1__DOT__body__DOT__rbReadAddr__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__scalarFPFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__1[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__0__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__1[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__renameLogic__genblk1__BRA__1__KET____DOT__scalarFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__raBank__1[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList__freeList____PVT__genblk1__DOT__rBank__DOT__rvBank__1[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 8; ++__Vi1) {
            vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__1[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__1[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__lvo__1[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__fflagsState__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__1[__Vi0][__Vi1] = VL_RAND_RESET_I(5);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 8; ++__Vi1) {
            vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__rvBank__1[__Vi0][__Vi1] = VL_RAND_RESET_I(4);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench__core__activeList__execStateRef__genblk1__DOT__body____PVT__genblk1__DOT__lvo__1[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__Vtrigprevexpr___TOP__SMT_RTL_Testbench____PVT__clk__0 = VL_RAND_RESET_I(1);
    vlSelf->__VactDidInit = 0;
    for (int __Vi0 = 0; __Vi0 < 175; ++__Vi0) {
        vlSelf->__Vm_traceActivity[__Vi0] = 0;
    }
}
