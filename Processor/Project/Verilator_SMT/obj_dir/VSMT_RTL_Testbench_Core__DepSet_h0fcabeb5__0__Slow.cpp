// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_Core___ctor_var_reset(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelf->__PVT__clk = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rstStart = VL_RAND_RESET_I(1);
    vlSelf->__PVT__nextMemReadSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__nextMemWriteSerial = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memReadData = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__memReadDataReady = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memReadSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__memAccessResponse = VL_RAND_RESET_I(2);
    vlSelf->__PVT__memAccessReadBusy = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memAccessWriteBusy = VL_RAND_RESET_I(1);
    vlSelf->__PVT__reqExternalInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__externalInterruptCode = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(2930, vlSelf->__PVT__debugRegister);
    vlSelf->__PVT__lastCommittedPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__memAccessAddr = VL_RAND_RESET_I(22);
    vlSelf->__PVT__memAccessWriteData = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__memAccessRE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memAccessWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__serialWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__serialWriteData = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(2930, vlSelf->__PVT__debug__DOT__next);
    VL_RAND_RESET_W(2930, vlSelf->__PVT__debug__DOT__debugRegister);
    VL_RAND_RESET_W(224, vlSelf->__PVT__perfCounter__DOT__cur);
    VL_RAND_RESET_W(224, vlSelf->__PVT__perfCounter__DOT__next);
    vlSelf->__PVT__controller__DOT__npStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__ifStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__pdStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__idStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__rnStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__dsStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__scStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__isStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__backEnd = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__cmStage = VL_RAND_RESET_I(2);
    vlSelf->__PVT__controller__DOT__stallByDecodeStage = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memoryAccessController__DOT__icAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memoryAccessController__DOT__dcAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memoryAccessController__DOT__reqSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__memoryAccessController__DOT__nextReqSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__memoryAccessController__DOT__resultSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__memoryAccessController__DOT__nextResultSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__npStage__DOT__threadCounter = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStage__DOT__nextSID = VL_RAND_RESET_I(10);
    vlSelf->__PVT__npStage__DOT__predNextPC = VL_RAND_RESET_I(20);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__npStage__DOT__nextStage[__Vi0] = VL_RAND_RESET_I(31);
    }
    vlSelf->__PVT__npStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStage__DOT__regStall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStage__DOT__beginStall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStage__DOT__writePC_FromOuter = VL_RAND_RESET_I(1);
    vlSelf->__PVT__npStage__DOT__fetchAddr = VL_RAND_RESET_I(32);
    vlSelf->__PVT__npStage__DOT__numValidInsns = VL_RAND_RESET_I(3);
    vlSelf->__PVT__npStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__npStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__npStage__DOT__unnamedblk3__DOT__t = 0;
    vlSelf->__PVT__npStage__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__npStage__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__npStage__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__npStage__DOT__sidFF__DOT__body = VL_RAND_RESET_I(10);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pc__DOT__pcRegs[__Vi0] = VL_RAND_RESET_I(20);
    }
    vlSelf->__PVT__ifStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__ifStage__DOT__empty = VL_RAND_RESET_I(1);
    vlSelf->__PVT__ifStage__DOT__regStall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__ifStage__DOT__beginStall = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ifStage__DOT__pipeReg[__Vi0] = VL_RAND_RESET_I(31);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(96, vlSelf->__PVT__ifStage__DOT__nextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ifStage__DOT__isFlushed[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ifStage__DOT__brPred[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__ifStage__DOT__regBrPred[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__PVT__ifStage__DOT__fetchAddrOut = VL_RAND_RESET_I(32);
    vlSelf->__PVT__ifStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__ifStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__ifStage__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__ifStage__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__ifStage__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__ifStage__DOT__unnamedblk6__DOT__unnamedblk7__DOT__j = 0;
    vlSelf->__PVT__ifStage__DOT__unnamedblk8__DOT__i = 0;
    vlSelf->__PVT__iCache__DOT__regPhase = VL_RAND_RESET_I(3);
    vlSelf->__PVT__iCache__DOT__nextPhase = VL_RAND_RESET_I(3);
    vlSelf->__PVT__iCache__DOT__regFlushStart = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__nextFlushStart = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__regFlush = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__nextFlush = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__regFlushReqAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__nextFlushReqAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__flushComplete = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__iCache__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__iCache__DOT__hit = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__hitArray = VL_RAND_RESET_I(2);
    vlSelf->__PVT__iCache__DOT__hitWay = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__iCache__DOT__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__iCache__DOT__readIndex = VL_RAND_RESET_I(8);
    vlSelf->__PVT__iCache__DOT__nextReadIndex = VL_RAND_RESET_I(8);
    vlSelf->__PVT__iCache__DOT__readTag = VL_RAND_RESET_I(11);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__iCache__DOT__readLineInsnList[__Vi0] = VL_RAND_RESET_Q(64);
    }
    vlSelf->__PVT__iCache__DOT__updatedNRUState = VL_RAND_RESET_I(2);
    vlSelf->__PVT__iCache__DOT__readNRUState = VL_RAND_RESET_I(2);
    vlSelf->__PVT__iCache__DOT__wayToEvictOneHot = VL_RAND_RESET_I(2);
    vlSelf->__PVT__iCache__DOT__wayToEvict = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__nruStateWE = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__rstIndex = VL_RAND_RESET_I(8);
    vlSelf->iCache__DOT____Vcellinp__nruStateArray__rst = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__iCache__DOT__wordPtr[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->iCache__DOT____Vcellout__iCacheHitLogic__hitOut[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->iCache__DOT____Vcellinp__iCacheHitLogic__hitIn = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__regMissValid = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__nextMissValid = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__regMissIndex = VL_RAND_RESET_I(8);
    vlSelf->__PVT__iCache__DOT__nextMissIndex = VL_RAND_RESET_I(8);
    vlSelf->__PVT__iCache__DOT__regMissTag = VL_RAND_RESET_I(11);
    vlSelf->__PVT__iCache__DOT__nextMissTag = VL_RAND_RESET_I(11);
    vlSelf->__PVT__iCache__DOT__regSerial = VL_RAND_RESET_I(2);
    vlSelf->__PVT__iCache__DOT__nextSerial = VL_RAND_RESET_I(2);
    vlSelf->iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__valid = VL_RAND_RESET_I(1);
    vlSelf->iCache__DOT____Vcellout__genblk1__BRA__0__KET____DOT__array__hit = VL_RAND_RESET_I(1);
    vlSelf->iCache__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__array__rst = VL_RAND_RESET_I(1);
    vlSelf->iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__valid = VL_RAND_RESET_I(1);
    vlSelf->iCache__DOT____Vcellout__genblk1__BRA__1__KET____DOT__array__hit = VL_RAND_RESET_I(1);
    vlSelf->iCache__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__array__rst = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__iCache__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__iCache__DOT__nruStateArray__DOT__we = VL_RAND_RESET_I(1);
    vlSelf->__PVT__iCache__DOT__nruStateArray__DOT__writeNRUStateIndex = VL_RAND_RESET_I(8);
    vlSelf->__PVT__iCache__DOT__nruStateArray__DOT__writeNRUStateData = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__iCache__DOT__nruStateArray__DOT__nruStateArray__DOT__array[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__iCache__DOT__iCacheHitLogic__DOT__wordPtr[__Vi0] = VL_RAND_RESET_I(2);
    }
    vlSelf->__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__rstIndex = VL_RAND_RESET_I(8);
    VL_RAND_RESET_W(76, vlSelf->__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__writeWayData);
    VL_RAND_RESET_W(76, vlSelf->iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT____Vcellout__tagValidArray__rv);
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        VL_RAND_RESET_W(76, vlSelf->__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__tagValidArray__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__rstIndex = VL_RAND_RESET_I(8);
    VL_RAND_RESET_W(76, vlSelf->__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__writeWayData);
    VL_RAND_RESET_W(76, vlSelf->iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT____Vcellout__tagValidArray__rv);
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        VL_RAND_RESET_W(76, vlSelf->__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__tagValidArray__DOT__array[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(96, vlSelf->__PVT__pdStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pdStage__DOT__pc[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__pdStage__DOT__illegalPC[__Vi0] = VL_RAND_RESET_I(1);
    }
    VL_RAND_RESET_W(456, vlSelf->__PVT__pdStage__DOT__microOps);
    vlSelf->__PVT__pdStage__DOT__insnInfo = VL_RAND_RESET_I(10);
    vlSelf->__PVT__pdStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pdStage__DOT__clear = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pdStage__DOT__empty = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(329, vlSelf->__PVT__pdStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__pdStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__pdStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__pdStage__DOT__unnamedblk3__DOT__i = 0;
    VL_RAND_RESET_W(228, vlSelf->pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__microOps);
    vlSelf->pdStage__DOT____Vcellout__genblk1__BRA__0__KET____DOT__decoder__insnInfo = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(228, vlSelf->pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__microOps);
    vlSelf->pdStage__DOT____Vcellout__genblk1__BRA__1__KET____DOT__decoder__insnInfo = VL_RAND_RESET_I(5);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__isf = VL_RAND_RESET_I(32);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__rv32mFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__zbaFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__zicondFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__0__KET____DOT__decoder__DOT__undefined = VL_RAND_RESET_I(1);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__isf = VL_RAND_RESET_I(32);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__rv32mFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zbaFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__zicondFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__PVT__pdStage__DOT__genblk1__BRA__1__KET____DOT__decoder__DOT__undefined = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(329, vlSelf->__PVT__idStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__idStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__clear = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__empty = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(142, vlSelf->__PVT__idStage__DOT__nextStage[__Vi0]);
    }
    VL_RAND_RESET_W(456, vlSelf->__PVT__idStage__DOT__microOps);
    vlSelf->__PVT__idStage__DOT__insnInfo = VL_RAND_RESET_I(10);
    vlSelf->__PVT__idStage__DOT__initiate = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__complete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__isfIn = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__idStage__DOT__stallBranchResolver = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__insnValidIn[__Vi0] = VL_RAND_RESET_I(1);
    }
    VL_RAND_RESET_W(66, vlSelf->__PVT__idStage__DOT__brPredIn);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__pcIn[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__insnValidOut[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__insnFlushed[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__insnFlushTriggering[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__idStage__DOT__flushTriggered = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__brPredOut[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__PVT__idStage__DOT__recoveredPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__idStage__DOT__remainingValidMOps = VL_RAND_RESET_I(6);
    vlSelf->__PVT__idStage__DOT__nextValidMOps = VL_RAND_RESET_I(6);
    vlSelf->__PVT__idStage__DOT__curValidMOps = VL_RAND_RESET_I(6);
    vlSelf->__PVT__idStage__DOT__pickedValidMOps = VL_RAND_RESET_I(6);
    vlSelf->__PVT__idStage__DOT__serializedMOps = VL_RAND_RESET_I(6);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__mopPickedIndex[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__mopPicked[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__idStage__DOT__orgPickedInsnLane = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk9__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk11__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk12__DOT__j = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk11__DOT__unnamedblk13__DOT__j = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk14__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk15__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j = 0;
    vlSelf->__PVT__idStage__DOT__unnamedblk15__DOT__unnamedblk17__DOT__j = 0;
    vlSelf->idStage__DOT____Vlvbound_h1adb7724__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__ras[__Vi0] = VL_RAND_RESET_I(20);
    }
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS = VL_RAND_RESET_I(20);
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__pushRAS = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__popRAS = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__rasPtr = VL_RAND_RESET_I(2);
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextRAS_Ptr = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__decodedPC[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__nextPC[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__isfU[__Vi0] = VL_RAND_RESET_I(32);
    }
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrIncorrect = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrCheckLane = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__brTargetType[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__addrMismatch[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__decodeStageBranchResolver__DOT__unnamedblk8__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__picker__DOT__clear = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__picker__DOT__sent = VL_RAND_RESET_I(1);
    vlSelf->__PVT__idStage__DOT__picker__DOT__cur = VL_RAND_RESET_I(6);
    vlSelf->__PVT__idStage__DOT__picker__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__idStage__DOT__picker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__mn = 0;
    vlSelf->idStage__DOT__picker__DOT____Vlvbound_h6b5ccdc9__0 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(142, vlSelf->__PVT__rnStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__rnStage__DOT__regFlush = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rnStage__DOT__regRecoveredPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__rnStage__DOT__empty = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rnStage__DOT__serialize = VL_RAND_RESET_I(1);
    vlSelf->__PVT__rnStage__DOT__valid = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rnStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    VL_RAND_RESET_W(152, vlSelf->__PVT__rnStage__DOT__opInfo);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rnStage__DOT__alEntry[__Vi0] = VL_RAND_RESET_Q(63);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(215, vlSelf->__PVT__rnStage__DOT__nextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rnStage__DOT__isLoad[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rnStage__DOT__isStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rnStage__DOT__isBranch[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__rnStage__DOT__isEnv[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__rnStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__rnStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__rnStage__DOT__serializer__DOT__regPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__rnStage__DOT__serializer__DOT__nextPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__renameLogicCommitter__DOT__phase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__renameLogicCommitter__DOT__nextPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__renameLogicCommitter__DOT__recoveryCount = VL_RAND_RESET_I(7);
    vlSelf->__PVT__renameLogicCommitter__DOT__nextRecoveryCount = VL_RAND_RESET_I(7);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__renameLogicCommitter__DOT__regReleasedReg[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__renameLogicCommitter__DOT__nextReleasedReg[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__renameLogicCommitter__DOT__alReadData[__Vi0] = VL_RAND_RESET_Q(63);
    }
    vlSelf->__PVT__renameLogicCommitter__DOT__releaseNum = VL_RAND_RESET_I(2);
    vlSelf->__PVT__renameLogicCommitter__DOT__flushNum = VL_RAND_RESET_I(2);
    vlSelf->__PVT__renameLogicCommitter__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__renameLogicCommitter__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__renameLogicCommitter__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__renameLogicCommitter__DOT__unnamedblk6__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(215, vlSelf->__PVT__dsStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__dsStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dsStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(76, vlSelf->__PVT__dsStage__DOT__opInfo[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__PVT__dsStage__DOT__intEntry[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(82, vlSelf->__PVT__dsStage__DOT__complexEntry[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__dsStage__DOT__memEntry[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__PVT__dsStage__DOT__fpEntry[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__schedulerEntry[__Vi0] = VL_RAND_RESET_Q(49);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__opSrc[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__opDst[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__intSubInfo[__Vi0] = VL_RAND_RESET_Q(57);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__brSubInfo[__Vi0] = VL_RAND_RESET_Q(57);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__mulSubInfo[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dsStage__DOT__divSubInfo[__Vi0] = VL_RAND_RESET_I(3);
    }
    vlSelf->__PVT__dsStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__scStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__scStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__scStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__scStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__scStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__scStage__DOT__nextStage[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__scStage__DOT__issueQueuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__scStage__DOT__flushIQ_Entry = VL_RAND_RESET_I(16);
    VL_RAND_RESET_W(712, vlSelf->__PVT__replayQueue__DOT__recordData);
    VL_RAND_RESET_W(712, vlSelf->__PVT__replayQueue__DOT__replayEntryOut);
    vlSelf->__PVT__replayQueue__DOT__pushEntry = VL_RAND_RESET_I(1);
    vlSelf->__PVT__replayQueue__DOT__popEntry = VL_RAND_RESET_I(1);
    vlSelf->__PVT__replayQueue__DOT__almostFull = VL_RAND_RESET_I(1);
    vlSelf->__PVT__replayQueue__DOT__replayEntryValidIn = VL_RAND_RESET_I(1);
    vlSelf->__PVT__replayQueue__DOT__replayEntryValidOut = VL_RAND_RESET_I(1);
    vlSelf->__PVT__replayQueue__DOT__validInstCount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__replayQueue__DOT__validInstCountNext = VL_RAND_RESET_I(6);
    vlSelf->__PVT__replayQueue__DOT__intervalIn = VL_RAND_RESET_I(3);
    vlSelf->__PVT__replayQueue__DOT__nextIntervalIn = VL_RAND_RESET_I(3);
    vlSelf->__PVT__replayQueue__DOT__intervalCount = VL_RAND_RESET_I(3);
    vlSelf->__PVT__replayQueue__DOT__nextIntervalCount = VL_RAND_RESET_I(3);
    vlSelf->__PVT__replayQueue__DOT__canBeFlushedEntryCount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__replayQueue__DOT__flushRangeHeadPtr = VL_RAND_RESET_I(6);
    vlSelf->__PVT__replayQueue__DOT__flushRangeTailPtr = VL_RAND_RESET_I(6);
    vlSelf->__PVT__replayQueue__DOT__flushAllInsns = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__replayQueue__DOT__flushInt[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__replayQueue__DOT__flushMem[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__replayQueue__DOT__flushComplex[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__replayQueue__DOT__flushFP[__Vi0] = VL_RAND_RESET_I(1);
    }
    VL_RAND_RESET_W(712, vlSelf->__PVT__replayQueue__DOT__replayEntryReg);
    VL_RAND_RESET_W(712, vlSelf->__PVT__replayQueue__DOT__nextReplayEntry);
    vlSelf->__PVT__replayQueue__DOT__replayReg = VL_RAND_RESET_I(1);
    vlSelf->__PVT__replayQueue__DOT__nextReplay = VL_RAND_RESET_I(1);
    vlSelf->__PVT__replayQueue__DOT__mshrNotReady = VL_RAND_RESET_I(2);
    vlSelf->__PVT__replayQueue__DOT__targetMSHRValid = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__replayQueue__DOT__mshrID[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__replayQueue__DOT__mshrValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__replayQueue__DOT__mshrPhase[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__replayQueue__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk8__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk9__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk10__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk11__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk12__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk13__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk14__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk15__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk16__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk17__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk18__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk19__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk20__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk21__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk22__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk23__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk24__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk25__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk26__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk27__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk28__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk29__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk30__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk31__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk32__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk33__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk34__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk35__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk36__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk37__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk38__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__unnamedblk39__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__pointer__DOT__regHeadStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__replayQueue__DOT__pointer__DOT__nextHeadStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__replayQueue__DOT__pointer__DOT__regTailStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__replayQueue__DOT__pointer__DOT__nextTailStorage = VL_RAND_RESET_I(5);
    vlSelf->__PVT__replayQueue__DOT__pointer__DOT__regCount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__replayQueue__DOT__pointer__DOT__nextCount = VL_RAND_RESET_I(6);
    for (int __Vi0 = 0; __Vi0 < 32; ++__Vi0) {
        VL_RAND_RESET_W(712, vlSelf->__PVT__replayQueue__DOT__replayQueue__DOT__array[__Vi0]);
    }
    vlSelf->__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__replayQueue__DOT__replayQueue__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->replayQueue__DOT__replayQueue__DOT____Vlvbound_h5ab6bdce__0 = VL_RAND_RESET_I(1);
    vlSelf->__PVT__scheduler__DOT__notIssued = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__isInt = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__isComplex = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__isDiv = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__isLoad = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__isStore = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__isFP = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__isFPDivSqrt = VL_RAND_RESET_I(16);
    vlSelf->__PVT__scheduler__DOT__selectedVector = VL_RAND_RESET_I(16);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__scheduler__DOT__dispatchStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__scheduler__DOT__dispatchLoad[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__scheduler__DOT__canIssueDiv = VL_RAND_RESET_I(1);
    vlSelf->__PVT__scheduler__DOT__canIssueFPDivSqrt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__scheduler__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__scheduler__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__scheduler__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__scheduler__DOT__unnamedblk10__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 1; ++__Vi1) {
            vlSelf->__PVT__wakeupPipelineRegister__DOT__intPipeReg[__Vi0][__Vi1] = VL_RAND_RESET_I(27);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__PVT__wakeupPipelineRegister__DOT__memPipeReg[__Vi0][__Vi1] = VL_RAND_RESET_I(27);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__nextIntPipeReg[__Vi0] = VL_RAND_RESET_I(27);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__nextMemPipeReg[__Vi0] = VL_RAND_RESET_I(27);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 5; ++__Vi1) {
            vlSelf->__PVT__wakeupPipelineRegister__DOT__complexPipeReg[__Vi0][__Vi1] = VL_RAND_RESET_I(27);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__nextComplexPipeReg[__Vi0] = VL_RAND_RESET_I(27);
    }
    vlSelf->__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountComplex = VL_RAND_RESET_I(4);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__flushComplex[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__complexSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 7; ++__Vi1) {
            vlSelf->__PVT__wakeupPipelineRegister__DOT__fpPipeReg[__Vi0][__Vi1] = VL_RAND_RESET_I(27);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__nextFPPipeReg[__Vi0] = VL_RAND_RESET_I(27);
    }
    vlSelf->__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountFP = VL_RAND_RESET_I(4);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__flushFP[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__fpSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountInt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountMem = VL_RAND_RESET_I(3);
    vlSelf->__PVT__wakeupPipelineRegister__DOT__flushRangeHeadPtr = VL_RAND_RESET_I(6);
    vlSelf->__PVT__wakeupPipelineRegister__DOT__flushRangeTailPtr = VL_RAND_RESET_I(6);
    vlSelf->__PVT__wakeupPipelineRegister__DOT__flushAllInsns = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__flushInt[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__flushMem[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__intSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__wakeupPipelineRegister__DOT__memSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__wakeupPipelineRegister__DOT__flushIQ_Entry = VL_RAND_RESET_I(16);
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk3__DOT__unnamedblk4__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk5__DOT__unnamedblk6__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk7__DOT__unnamedblk8__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk9__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk9__DOT__unnamedblk10__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk11__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk11__DOT__unnamedblk12__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk13__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk13__DOT__unnamedblk14__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk15__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk15__DOT__unnamedblk16__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk25__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk25__DOT__unnamedblk26__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk27__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk27__DOT__unnamedblk28__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk29__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk29__DOT__unnamedblk30__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk17__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk17__DOT__unnamedblk18__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk19__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk19__DOT__unnamedblk20__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk21__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk21__DOT__unnamedblk22__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk23__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk23__DOT__unnamedblk24__DOT__j = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk31__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk32__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk33__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk34__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk35__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk36__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk37__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk38__DOT__i = 0;
    vlSelf->__PVT__wakeupPipelineRegister__DOT__unnamedblk39__DOT__i = 0;
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_he583563b__0 = VL_RAND_RESET_I(27);
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_hae8acb87__0 = VL_RAND_RESET_I(27);
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_h24455b40__0 = VL_RAND_RESET_I(27);
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_habb16b7b__0 = VL_RAND_RESET_I(27);
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_h3f8dce7c__0 = VL_RAND_RESET_I(27);
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_h1898fcb0__0 = VL_RAND_RESET_I(27);
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_heeee1c65__0 = VL_RAND_RESET_I(27);
    vlSelf->wakeupPipelineRegister__DOT____Vlvbound_h60c7c9ad__0 = VL_RAND_RESET_I(27);
    vlSelf->__PVT__selectLogic__DOT__intRequest = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__intGrant = VL_RAND_RESET_I(16);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__intSelected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__intSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__selectLogic__DOT__compRequest = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__compGrant = VL_RAND_RESET_I(16);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__compSelected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__compSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__selectLogic__DOT__loadRequest = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__storeRequest = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__loadGrant = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__storeGrant = VL_RAND_RESET_I(16);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__loadSelected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__storeSelected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__loadSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__storeSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__selectLogic__DOT__fpRequest = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__fpGrant = VL_RAND_RESET_I(16);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__fpSelected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__fpSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__portSelected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__portSelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__portSelectedVector[__Vi0] = VL_RAND_RESET_I(16);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__recoverySelected[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectLogic__DOT__recoverySelectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__selectLogic__DOT__intPicker__DOT__reqTmp = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__p = 0;
    vlSelf->__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0;
    vlSelf->__PVT__selectLogic__DOT__compPicker__DOT__reqTmp = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__p = 0;
    vlSelf->__PVT__selectLogic__DOT__compPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0;
    vlSelf->selectLogic__DOT__compPicker__DOT____Vlvbound_h1f89c34b__1 = VL_RAND_RESET_I(1);
    vlSelf->selectLogic__DOT__compPicker__DOT____Vlvbound_h54e39f63__1 = VL_RAND_RESET_I(4);
    vlSelf->__PVT__selectLogic__DOT__loadPicker__DOT__reqTmp = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__p = 0;
    vlSelf->__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0;
    vlSelf->selectLogic__DOT__loadPicker__DOT____Vlvbound_h1f89c34b__1 = VL_RAND_RESET_I(1);
    vlSelf->selectLogic__DOT__loadPicker__DOT____Vlvbound_h54e39f63__1 = VL_RAND_RESET_I(4);
    vlSelf->__PVT__selectLogic__DOT__storePicker__DOT__reqTmp = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__p = 0;
    vlSelf->__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0;
    vlSelf->selectLogic__DOT__storePicker__DOT____Vlvbound_h1f89c34b__1 = VL_RAND_RESET_I(1);
    vlSelf->selectLogic__DOT__storePicker__DOT____Vlvbound_h54e39f63__1 = VL_RAND_RESET_I(4);
    vlSelf->__PVT__selectLogic__DOT__fpPicker__DOT__reqTmp = VL_RAND_RESET_I(16);
    vlSelf->__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__p = 0;
    vlSelf->__PVT__selectLogic__DOT__fpPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0;
    vlSelf->selectLogic__DOT__fpPicker__DOT____Vlvbound_h1f89c34b__1 = VL_RAND_RESET_I(1);
    vlSelf->selectLogic__DOT__fpPicker__DOT____Vlvbound_h54e39f63__1 = VL_RAND_RESET_I(4);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intIsStage__DOT__pipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intIsStage__DOT__nextPipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__intIsStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__intIsStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intIsStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intIsStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(152, vlSelf->__PVT__intIsStage__DOT__nextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__PVT__intIsStage__DOT__issuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intIsStage__DOT__issueQueuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__intIsStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__intIsStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__intIsStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__intIsStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(152, vlSelf->__PVT__intRrStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__immOut[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__pc[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__operandA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__operandB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__PVT__intRrStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__intRrStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__PVT__intRrStage__DOT__iqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__intSubInfo[__Vi0] = VL_RAND_RESET_Q(57);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__opSrc[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRrStage__DOT__opDst[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(239, vlSelf->__PVT__intRrStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__intRrStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__intRrStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__intRrStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__intRrStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(239, vlSelf->__PVT__intExStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__intExStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__intExStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__PVT__intExStage__DOT__iqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__intOpInfo[__Vi0] = VL_RAND_RESET_Q(57);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__bPred[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__pc[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__fuOpA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__fuOpB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__dataOut[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__isCondEnabled[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__aluDataOut[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__aluCode[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__shiftOperandType[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__shiftImmIn[__Vi0] = VL_RAND_RESET_I(30);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__shiftDataOut[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__shiftCarryOut[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__isBranch[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__isJump[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__brTaken[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__brResult[__Vi0] = VL_RAND_RESET_Q(57);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__predMiss[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__intSubInfo[__Vi0] = VL_RAND_RESET_Q(57);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intExStage__DOT__brSubInfo[__Vi0] = VL_RAND_RESET_Q(57);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(243, vlSelf->__PVT__intExStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__intExStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__intExStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->intExStage__DOT____Vcellout__BlockALU__BRA__0__KET____DOT__intALU__aluDataOut = VL_RAND_RESET_I(32);
    vlSelf->intExStage__DOT____Vcellout__BlockALU__BRA__1__KET____DOT__intALU__aluDataOut = VL_RAND_RESET_I(32);
    vlSelf->intExStage__DOT____Vcellout__BlockShifter__BRA__0__KET____DOT__shifter__carryOut = VL_RAND_RESET_I(1);
    vlSelf->intExStage__DOT____Vcellout__BlockShifter__BRA__0__KET____DOT__shifter__dataOut = VL_RAND_RESET_I(32);
    vlSelf->intExStage__DOT____Vcellout__BlockShifter__BRA__1__KET____DOT__shifter__carryOut = VL_RAND_RESET_I(1);
    vlSelf->intExStage__DOT____Vcellout__BlockShifter__BRA__1__KET____DOT__shifter__dataOut = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__intExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__currentPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__intExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__nextAddr = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opDst = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__opB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__signedOpB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderDst = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderInB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adderOutOverflow = VL_RAND_RESET_I(1);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__0__KET____DOT__intALU__DOT__adder__DOT__tmpB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opDst = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__opB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__signedOpB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderDst = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderInB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adderOutOverflow = VL_RAND_RESET_I(1);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpA = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockALU__BRA__1__KET____DOT__intALU__DOT__adder__DOT__tmpB = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__shiftAmount = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(66, vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftTmp);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftHighIn = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftLowIn = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftOut = VL_RAND_RESET_Q(34);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__unifiedShiftAmount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__0__KET____DOT__shifter__DOT__isShiftZero = VL_RAND_RESET_I(1);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__shiftAmount = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(66, vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftTmp);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftHighIn = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftLowIn = VL_RAND_RESET_I(32);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftOut = VL_RAND_RESET_Q(34);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__unifiedShiftAmount = VL_RAND_RESET_I(6);
    vlSelf->__PVT__intExStage__DOT__BlockShifter__BRA__1__KET____DOT__shifter__DOT__isShiftZero = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(243, vlSelf->__PVT__intRwStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(72, vlSelf->__PVT__intRwStage__DOT__alWriteData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__PVT__intRwStage__DOT__iqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRwStage__DOT__brResult[__Vi0] = VL_RAND_RESET_Q(57);
    }
    vlSelf->__PVT__intRwStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__intRwStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRwStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRwStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRwStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intRwStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__intRwStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__intRwStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__intRwStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__intRwStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexIsStage__DOT__pipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexIsStage__DOT__nextPipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__complexIsStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__complexIsStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexIsStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexIsStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(96, vlSelf->__PVT__complexIsStage__DOT__nextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(82, vlSelf->__PVT__complexIsStage__DOT__issuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexIsStage__DOT__issueQueuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__complexIsStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__complexIsStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__complexIsStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__complexIsStage__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->complexIsStage__DOT____Vlvbound_hbbf83b6a__0 = VL_RAND_RESET_I(1);
    vlSelf->complexIsStage__DOT____Vlvbound_hbbf83b6a__1 = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(82, vlSelf->complexIsStage__DOT____Vlvbound_hfff70973__0);
    vlSelf->complexIsStage__DOT____Vlvbound_h144dd5b6__0 = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(82, vlSelf->complexIsStage__DOT____Vlvbound_hfff70973__1);
    vlSelf->complexIsStage__DOT____Vlvbound_h144dd5b6__1 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(96, vlSelf->__PVT__complexRrStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__complexRrStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__complexRrStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRrStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(82, vlSelf->__PVT__complexRrStage__DOT__iqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRrStage__DOT__mulOpInfo[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRrStage__DOT__opSrc[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRrStage__DOT__opDst[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(184, vlSelf->__PVT__complexRrStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__complexRrStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__complexRrStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__complexRrStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__complexRrStage__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->complexRrStage__DOT____Vlvbound_ha632c82d__0 = VL_RAND_RESET_I(1);
    vlSelf->__PVT__complexExStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__complexExStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            vlSelf->__PVT__complexExStage__DOT__flush[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            VL_RAND_RESET_W(96, vlSelf->__PVT__complexExStage__DOT__localPipeReg[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            VL_RAND_RESET_W(96, vlSelf->__PVT__complexExStage__DOT__nextLocalPipeReg[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(184, vlSelf->__PVT__complexExStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 3; ++__Vi1) {
            VL_RAND_RESET_W(82, vlSelf->__PVT__complexExStage__DOT__iqData[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__complexOpInfo[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__mulSubInfo[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__divSubInfo[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__fuOpA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__fuOpB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__dataOut[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__isDiv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexExStage__DOT__finished[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(128, vlSelf->__PVT__complexExStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__complexExStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__complexExStage__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->__PVT__complexExStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__complexExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__j = 0;
    vlSelf->__PVT__complexExStage__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__complexExStage__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__complexExStage__DOT__unnamedblk8__DOT__i = 0;
    vlSelf->__PVT__complexExStage__DOT__unnamedblk8__DOT__unnamedblk9__DOT__j = 0;
    vlSelf->complexExStage__DOT____Vlvbound_h8f502eaa__0 = VL_RAND_RESET_I(32);
    vlSelf->complexExStage__DOT____Vlvbound_h8f502eaa__1 = VL_RAND_RESET_I(32);
    vlSelf->complexExStage__DOT____Vlvbound_h7bb5401b__0 = VL_RAND_RESET_I(1);
    vlSelf->complexExStage__DOT____Vlvbound_h7bb5401b__1 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(128, vlSelf->__PVT__complexRwStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(72, vlSelf->__PVT__complexRwStage__DOT__alWriteData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__PVT__complexRwStage__DOT__iqData[__Vi0]);
    }
    vlSelf->__PVT__complexRwStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__complexRwStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRwStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRwStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRwStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexRwStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__complexRwStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__complexRwStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__complexRwStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__complexRwStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__regPhase[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__nextPhase[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__finished[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__rst_divider[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__regActiveListPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__nextActiveListPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->mulDivUnit__DOT____Vcellout__BlockDivUnit__BRA__0__KET____DOT__divUnit__finished = VL_RAND_RESET_I(1);
    vlSelf->__PVT__mulDivUnit__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__mulDivUnit__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->mulDivUnit__DOT____Vlvbound_ha7e1d1c3__0 = VL_RAND_RESET_I(6);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__pipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__fuOpA_signed = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__fuOpB_signed = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__fuOpA_sign = VL_RAND_RESET_I(1);
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__fuOpB_sign = VL_RAND_RESET_I(1);
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__unnamedblk3__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(66, vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcA_Reg = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__exSrcB_Reg = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT__unnamedblk3__DOT__i = 0;
    VL_RAND_RESET_W(66, vlSelf->mulDivUnit__DOT__BlockMulUnit__BRA__0__KET____DOT__mulUnit__DOT__mul__DOT____Vlvbound_hfbb92ea3__0);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regIsSigned = VL_RAND_RESET_I(1);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextIsSigned = VL_RAND_RESET_I(1);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDividend = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDividend = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivisor = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivisor = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__regDivCode = VL_RAND_RESET_I(2);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__nextDivCode = VL_RAND_RESET_I(2);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__quotient = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__remainder = VL_RAND_RESET_I(32);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regZ = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextZ = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regD = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextD = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regQ = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextQ = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regR = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextR = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regSigned = VL_RAND_RESET_I(1);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextSigned = VL_RAND_RESET_I(1);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regCounter = VL_RAND_RESET_I(6);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextCounter = VL_RAND_RESET_I(6);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__regPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__nextPhase = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memIsStage__DOT__pipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memIsStage__DOT__nextPipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__memIsStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memIsStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memIsStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memIsStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(143, vlSelf->__PVT__memIsStage__DOT__nextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__memIsStage__DOT__issuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memIsStage__DOT__issueQueuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__memIsStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__memIsStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__memIsStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__memIsStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(143, vlSelf->__PVT__memRrStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__immOut[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__pc[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__operandA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__operandB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__PVT__memRrStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memRrStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__memRrStage__DOT__iqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__memOpInfo[__Vi0] = VL_RAND_RESET_Q(39);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__opSrc[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRrStage__DOT__opDst[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(230, vlSelf->__PVT__memRrStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__memRrStage__DOT__mshrID = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memRrStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__memRrStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__memRrStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__memRrStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(230, vlSelf->__PVT__memExStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__memExStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memExStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__memExStage__DOT__iqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__memOpInfo[__Vi0] = VL_RAND_RESET_Q(39);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__fuOpA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__fuOpB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__addrOut[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__memMapType[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__phyAddrOut[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memExStage__DOT__isUncachable[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__memExStage__DOT__cacheFlushReq = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memExStage__DOT__isCSR = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(228, vlSelf->__PVT__memExStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__memExStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__memExStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__memExStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__memExStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(228, vlSelf->__PVT__mtStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(228, vlSelf->__PVT__mtStage__DOT__ldPipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(228, vlSelf->__PVT__mtStage__DOT__stPipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__mtStage__DOT__ldIqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__mtStage__DOT__stIqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__isLoad[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__isCSR[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__isENV[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__ldUpdate[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__ldRegValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__ldFlush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__isDiv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__isMul[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__isFenceI[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__storeForwardMiss[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(168, vlSelf->__PVT__mtStage__DOT__ldNextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__mtStage__DOT__ldRecordData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__ldMSHR_Allocated[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__ldMSHR_Hit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__ldMSHR_EntryID[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__isStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__stUpdate[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__stRegValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__stFlush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(168, vlSelf->__PVT__mtStage__DOT__stNextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__mtStage__DOT__stRecordData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__memAccessOrderViolation[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__mtStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(168, vlSelf->__PVT__mtStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__mtStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__mtStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__mtStage__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__mtStage__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->mtStage__DOT____Vlvbound_h00006eb8__0 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_hfffffe27__0 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h00006eb8__2 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_hfffffe27__1 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h9e758f27__1 = VL_RAND_RESET_I(32);
    vlSelf->mtStage__DOT____Vlvbound_hfffffe27__2 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h266aa6f3__1 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h9e758f27__2 = VL_RAND_RESET_I(32);
    vlSelf->mtStage__DOT____Vlvbound_hfffffe27__3 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h7586bee0__0 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h7586bee0__1 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h7586bee0__2 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h7586bee0__3 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h7586bee0__4 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h7586bee0__5 = VL_RAND_RESET_I(1);
    vlSelf->mtStage__DOT____Vlvbound_h9382e679__1 = VL_RAND_RESET_I(4);
    vlSelf->mtStage__DOT____Vlvbound_h9382e679__2 = VL_RAND_RESET_I(4);
    vlSelf->mtStage__DOT____Vlvbound_h9382e679__4 = VL_RAND_RESET_I(4);
    vlSelf->mtStage__DOT____Vlvbound_h9382e679__5 = VL_RAND_RESET_I(4);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(168, vlSelf->__PVT__maStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__isStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__isLoad[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__isCSR[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__isDiv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__isMul[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__maStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__maStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(141, vlSelf->__PVT__maStage__DOT__nextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__dataOut[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__ldDataOut[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__maStage__DOT__stDataOut[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__PVT__maStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk8__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk9__DOT__i = 0;
    vlSelf->__PVT__maStage__DOT__unnamedblk10__DOT__i = 0;
    vlSelf->maStage__DOT____Vlvbound_h84fde84e__0 = VL_RAND_RESET_I(32);
    vlSelf->maStage__DOT____Vlvbound_h84fde84e__1 = VL_RAND_RESET_I(32);
    vlSelf->maStage__DOT____Vlvbound_h84fde84e__2 = VL_RAND_RESET_I(32);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__storeLoadForwardedReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__forwardedLoadDataReg[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__mshrReadHitReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__mshrReadDataReg[__Vi0] = VL_RAND_RESET_Q(64);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__loadAddrReg[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__loadMemAccessSizeReg[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__loadLSQ_BlockData[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__shiftedLoadData[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadStoreUnit__DOT__extendedLoadData[__Vi0] = VL_RAND_RESET_I(32);
    }
    vlSelf->__PVT__loadStoreUnit__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__loadStoreUnit__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__loadStoreUnit__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->loadStoreUnit__DOT____Vlvbound_h69b218e7__1 = VL_RAND_RESET_I(1);
    vlSelf->loadStoreUnit__DOT____Vlvbound_ha5e91678__1 = VL_RAND_RESET_I(32);
    vlSelf->loadStoreUnit__DOT____Vlvbound_h7051832b__1 = VL_RAND_RESET_I(1);
    vlSelf->loadStoreUnit__DOT____Vlvbound_h89a273e2__1 = VL_RAND_RESET_Q(64);
    vlSelf->loadStoreUnit__DOT____Vlvbound_h793a8b1a__1 = VL_RAND_RESET_I(32);
    vlSelf->loadStoreUnit__DOT____Vlvbound_h5c56ac8f__1 = VL_RAND_RESET_I(3);
    vlSelf->__PVT__loadQueue__DOT__reset = VL_RAND_RESET_I(1);
    vlSelf->__PVT__loadQueue__DOT__push = VL_RAND_RESET_I(1);
    vlSelf->__PVT__loadQueue__DOT__pushCount = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__loadQueue[__Vi0] = VL_RAND_RESET_Q(43);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__executedLoadQueuePtrByLoad[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__executedLoadAddr[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__executedLoadWordRE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__executedLoadRegValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__executedLoadQueuePtrByStore[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__addrMatch[__Vi0] = VL_RAND_RESET_I(16);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__pickedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__picked[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__violation[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__executedStoreAddr[__Vi0] = VL_RAND_RESET_I(20);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__executedStoreWordWE[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__loadQueue__DOT__conflictLoadPC[__Vi0] = VL_RAND_RESET_I(20);
    }
    vlSelf->__PVT__loadQueue__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__picked = VL_RAND_RESET_I(1);
    vlSelf->loadQueue__DOT____Vcellout__genblk1__BRA__0__KET____DOT__picker__grantPtr = VL_RAND_RESET_I(4);
    vlSelf->__PVT__loadQueue__DOT__unnamedblk6__DOT__si = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk7__DOT__si = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk7__DOT__unnamedblk8__DOT__lqe = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk9__DOT__si = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk10__DOT__si = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk10__DOT__unnamedblk11__DOT__li = 0;
    vlSelf->__PVT__loadQueue__DOT__unnamedblk12__DOT__i = 0;
    vlSelf->loadQueue__DOT____Vlvbound_h177e4fd5__0 = VL_RAND_RESET_I(1);
    vlSelf->loadQueue__DOT____Vlvbound_h1a4b3925__0 = VL_RAND_RESET_I(20);
    vlSelf->loadQueue__DOT____Vlvbound_ha8f38411__0 = VL_RAND_RESET_I(1);
    vlSelf->loadQueue__DOT____Vlvbound_h8fa27459__0 = VL_RAND_RESET_I(20);
    vlSelf->__PVT__loadQueue__DOT__loadQueuePointer__DOT__regHead = VL_RAND_RESET_I(4);
    vlSelf->__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextHead = VL_RAND_RESET_I(4);
    vlSelf->__PVT__loadQueue__DOT__loadQueuePointer__DOT__regTail = VL_RAND_RESET_I(4);
    vlSelf->__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextTail = VL_RAND_RESET_I(4);
    vlSelf->__PVT__loadQueue__DOT__loadQueuePointer__DOT__roundedSetTailPtr = VL_RAND_RESET_I(4);
    vlSelf->__PVT__loadQueue__DOT__loadQueuePointer__DOT__regCount = VL_RAND_RESET_I(5);
    vlSelf->__PVT__loadQueue__DOT__loadQueuePointer__DOT__nextCount = VL_RAND_RESET_I(5);
    vlSelf->__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedReq = VL_RAND_RESET_I(16);
    vlSelf->__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__shiftedGrant = VL_RAND_RESET_I(4);
    vlSelf->__PVT__loadQueue__DOT__genblk1__BRA__0__KET____DOT__picker__DOT__genblk1__DOT__rightShifter__DOT__shiftTmp = VL_RAND_RESET_I(32);
    vlSelf->__PVT__storeCommitter__DOT__phase = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__nextPhase = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__unfinishedStoreNum = VL_RAND_RESET_I(5);
    vlSelf->__PVT__storeCommitter__DOT__nextUnfinishedStoreNum = VL_RAND_RESET_I(5);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__storeCommitter__DOT__portMSHRPhase[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__storeCommitter__DOT__stallStoreTagStage = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__tagStagePipeReg = VL_RAND_RESET_Q(61);
    vlSelf->__PVT__storeCommitter__DOT__nextTagStagePipeReg = VL_RAND_RESET_Q(61);
    vlSelf->__PVT__storeCommitter__DOT__dataStagePipeReg = VL_RAND_RESET_Q(61);
    vlSelf->__PVT__storeCommitter__DOT__nextDataStagePipeReg = VL_RAND_RESET_Q(61);
    vlSelf->__PVT__storeCommitter__DOT__headStoreHasAllocatedMSHRPipeReg = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__storeMSHRID = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__dcWriteReq = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__dcWriteAddr = VL_RAND_RESET_I(22);
    vlSelf->__PVT__storeCommitter__DOT__dcWriteData = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__storeCommitter__DOT__dcWriteUncachable = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__isIO = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__dcWriteByteWE = VL_RAND_RESET_I(8);
    vlSelf->__PVT__storeCommitter__DOT__isUncachable = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__retiredStoreQueuePtr = VL_RAND_RESET_I(4);
    vlSelf->__PVT__storeCommitter__DOT__finishWriteBack = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__releaseStoreQueueHead = VL_RAND_RESET_I(1);
    vlSelf->__PVT__storeCommitter__DOT__releaseStoreQueueHeadEntryNum = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__hit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missReq[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missAddr[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missIsUncachable[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missActiveListPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->__PVT__dCache__DOT__storedLineData = VL_RAND_RESET_Q(64);
    vlSelf->__PVT__dCache__DOT__storedLineByteWE = VL_RAND_RESET_I(8);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__dcReadAddrRegTagStg[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__dcReadAddrRegDataStg[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__dcReadReqReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuCacheGrtReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__dcReadUncachableReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__dcReadActiveListPtrReg[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->__PVT__dCache__DOT__dcWriteReqReg = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__dcWriteAddrReg = VL_RAND_RESET_I(22);
    vlSelf->__PVT__dCache__DOT__dcWriteUncachableReg = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuLoadHasAllocatedMSHR[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuLoadMSHRID[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuStoreHasAllocatedMSHR[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuStoreMSHRID[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuMSHRAddrHit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuMSHRAddrHitMSHRID[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuMSHRReadHit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuMSHRReadData[__Vi0] = VL_RAND_RESET_Q(64);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__mshrConflict[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__portInitMSHR[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__portInitMSHR_Addr[__Vi0] = VL_RAND_RESET_I(22);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__portInitMSHR_ActiveListPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__portIsAllocatedByStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__portIsUncachable[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__dCache__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk12__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk13__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk14__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk15__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk16__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk16__DOT__unnamedblk17__DOT__m = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk18__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk19__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk19__DOT__unnamedblk20__DOT__m = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk21__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk22__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk23__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk24__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__unnamedblk25__DOT__i = 0;
    vlSelf->dCache__DOT____Vlvbound_hc88cfe58__0 = VL_RAND_RESET_I(1);
    vlSelf->dCache__DOT____Vlvbound_he2e1fd55__0 = VL_RAND_RESET_I(22);
    vlSelf->dCache__DOT____Vlvbound_haac229c8__0 = VL_RAND_RESET_I(1);
    vlSelf->dCache__DOT____Vlvbound_h3ab68d0e__0 = VL_RAND_RESET_I(6);
    vlSelf->dCache__DOT____Vlvbound_h466a051b__0 = VL_RAND_RESET_I(1);
    vlSelf->dCache__DOT____Vlvbound_h41002977__0 = VL_RAND_RESET_I(1);
    vlSelf->dCache__DOT____Vlvbound_hf7e4173d__0 = VL_RAND_RESET_I(1);
    vlSelf->dCache__DOT____Vlvbound_h0bd7da04__0 = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__controller__DOT__regPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__dCache__DOT__controller__DOT__nextPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__dCache__DOT__controller__DOT__dcFlushReqAck = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__controller__DOT__dcFlushComplete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__controller__DOT__mshrBusy = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__controller__DOT__loadStoreBusy = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__controller__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__controller__DOT__unnamedblk2__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayWE[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            for (int __Vi2 = 0; __Vi2 < 2; ++__Vi2) {
                vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayByteWE[__Vi0][__Vi1][__Vi2] = VL_RAND_RESET_I(1);
            }
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayIndex[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayIn[__Vi0][__Vi1] = VL_RAND_RESET_I(8);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            for (int __Vi2 = 0; __Vi2 < 2; ++__Vi2) {
                vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayOut[__Vi0][__Vi1][__Vi2] = VL_RAND_RESET_I(8);
            }
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayDirtyIn[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayDirtyOut[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayReadWayReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayReadWay[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayDoesReadEvictedWayReg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayByteWE_Tmp[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayInTmp[__Vi0] = VL_RAND_RESET_Q(64);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__dataArrayOutTmp[__Vi0][__Vi1] = VL_RAND_RESET_Q(64);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__tagArrayWE[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__tagArrayIndex[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__tagArrayIn[__Vi0] = VL_RAND_RESET_I(12);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__tagArrayOut[__Vi0][__Vi1] = VL_RAND_RESET_I(12);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__tagArrayOutTmp[__Vi0][__Vi1] = VL_RAND_RESET_I(12);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__replArrayWE[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__replArrayWE_Flat[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__replArrayIndex[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__replArrayIn[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__replArrayInFlat[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__array__DOT__replArrayOut[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__replArrayOutFlat[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__replArrayResult[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__dCache__DOT__array__DOT__rstIndex = VL_RAND_RESET_I(8);
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk1__DOT__p = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__dirtyArray__rv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__dirtyArray__wv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__dirtyArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__0__KET____DOT__tagArray__rv[__Vi0] = VL_RAND_RESET_I(12);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__tagArray__wv[__Vi0] = VL_RAND_RESET_I(12);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__tagArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__rv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__wv[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__dirtyArray__rv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__dirtyArray__wv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__dirtyArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk1__BRA__1__KET____DOT__tagArray__rv[__Vi0] = VL_RAND_RESET_I(12);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__tagArray__wv[__Vi0] = VL_RAND_RESET_I(12);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__tagArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellout__genblk2__BRA__0__KET____DOT__replArray__rv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk2__BRA__0__KET____DOT__replArray__wv[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->dCache__DOT__array__DOT____Vcellinp__genblk2__BRA__0__KET____DOT__replArray__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__p = 0;
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__unnamedblk15__DOT__way = 0;
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__unnamedblk15__DOT__unnamedblk16__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk17__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk17__DOT__unnamedblk18__DOT__way = 0;
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk19__DOT__way = 0;
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk20__DOT__way = 0;
    vlSelf->__PVT__dCache__DOT__array__DOT__unnamedblk21__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__dirtyArray__DOT__array[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__0__KET____DOT__tagArray__DOT__array[__Vi0] = VL_RAND_RESET_I(12);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__0__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__1__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__2__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__3__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__4__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__5__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__6__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__genblk1__BRA__7__KET____DOT__dataArray__DOT__array[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__dirtyArray__DOT__array[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk1__BRA__1__KET____DOT__tagArray__DOT__array[__Vi0] = VL_RAND_RESET_I(12);
    }
    for (int __Vi0 = 0; __Vi0 < 256; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__array__DOT__genblk2__BRA__0__KET____DOT__replArray__DOT__array[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__req[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__grant[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayInSel[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayOutSel[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__cacheArrayInGrant[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk1__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk2__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk3__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk4__DOT__p = 0;
    vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk4__DOT__unnamedblk5__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk6__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayArbiter__DOT__unnamedblk7__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__portIn = VL_RAND_RESET_I(2);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__portInRegTagStg[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__portInRegGrantTagStg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__portOutRegTagStg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__portOutRegDataStg[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        VL_RAND_RESET_W(99, vlSelf->__PVT__dCache__DOT__arrayMux__DOT__muxIn[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(99, vlSelf->__PVT__dCache__DOT__arrayMux__DOT__muxInReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__PVT__dCache__DOT__arrayMux__DOT__muxTagOut[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        VL_RAND_RESET_W(66, vlSelf->__PVT__dCache__DOT__arrayMux__DOT__muxDataOut[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__arrayMux__DOT__tagHit[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__mshrConflict[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__mshrAddrHit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__mshrAddrHitMSHRID[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__mshrReadHit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__mshrReadData[__Vi0] = VL_RAND_RESET_Q(64);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__portMSHRData[__Vi0] = VL_RAND_RESET_Q(64);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__repIsHit[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__repHitWay[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__arrayMux__DOT__tagArrayDataOutTmp[__Vi0][__Vi1] = VL_RAND_RESET_I(11);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 2; ++__Vi1) {
            vlSelf->__PVT__dCache__DOT__arrayMux__DOT__tagArrayValidOutTmp[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__dataArrayDirtyOutTmp[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__dataArrayDataOutTmp[__Vi0] = VL_RAND_RESET_Q(64);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__replArrayDataOutTmp[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__arrayMux__DOT__isReplSameIndex[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk3__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk4__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk5__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk6__DOT__p = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk7__DOT__p = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk7__DOT__unnamedblk8__DOT__m = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk7__DOT__unnamedblk9__DOT__way = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk10__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk10__DOT__unnamedblk11__DOT__w = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk12__DOT__p = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk13__DOT__p = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk13__DOT__unnamedblk14__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__arrayMux__DOT__unnamedblk15__DOT__r = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__memArbiter__DOT__req[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__memArbiter__DOT__grant[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__dCache__DOT__memArbiter__DOT__memInSel = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__memArbiter__DOT__memValid = VL_RAND_RESET_I(1);
    vlSelf->__PVT__dCache__DOT__memArbiter__DOT__unnamedblk1__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__memArbiter__DOT__unnamedblk3__DOT__r = 0;
    vlSelf->__PVT__dCache__DOT__memMux__DOT__portIn = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(164, vlSelf->__PVT__dCache__DOT__missHandler__DOT__nextMSHR[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(164, vlSelf->__PVT__dCache__DOT__missHandler__DOT__mshr[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Entry[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missHandler__DOT__flushMSHR_Allocation[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missHandler__DOT__portIsAllocatedByStore[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missHandler__DOT__mergedLine[__Vi0] = VL_RAND_RESET_Q(64);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missHandler__DOT__mshrFlushComplete[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__dCache__DOT__missHandler__DOT__cycles[__Vi0] = VL_RAND_RESET_I(32);
    }
    vlSelf->__PVT__dCache__DOT__missHandler__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__missHandler__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__missHandler__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__missHandler__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__dCache__DOT__missHandler__DOT__unnamedblk6__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(141, vlSelf->__PVT__memRwStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__memRwStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__memRwStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRwStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRwStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRwStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(72, vlSelf->__PVT__memRwStage__DOT__alWriteData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRwStage__DOT__execState[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__memRwStage__DOT__mshrID = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memRwStage__DOT__makeMSHRCanBeInvalid[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__memRwStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__memRwStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__memRwStage__DOT__unnamedblk3__DOT__j = 0;
    vlSelf->__PVT__memRwStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__memRwStage__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__memRwStage__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__memRwStage__DOT__unnamedblk7__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpIsStage__DOT__pipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpIsStage__DOT__nextPipeReg[__Vi0] = VL_RAND_RESET_I(5);
    }
    vlSelf->__PVT__fpIsStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpIsStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpIsStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpIsStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(107, vlSelf->__PVT__fpIsStage__DOT__nextStage[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__PVT__fpIsStage__DOT__issuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpIsStage__DOT__issueQueuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__fpIsStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__fpIsStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__fpIsStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__fpIsStage__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->fpIsStage__DOT____Vlvbound_hbbf83b6a__0 = VL_RAND_RESET_I(1);
    vlSelf->fpIsStage__DOT____Vlvbound_hbbf83b6a__1 = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(93, vlSelf->fpIsStage__DOT____Vlvbound_hd7fbcd50__0);
    vlSelf->fpIsStage__DOT____Vlvbound_h144dd5b6__0 = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(93, vlSelf->fpIsStage__DOT____Vlvbound_hd7fbcd50__1);
    vlSelf->fpIsStage__DOT____Vlvbound_h144dd5b6__1 = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(107, vlSelf->__PVT__fpRrStage__DOT__pipeReg[__Vi0]);
    }
    vlSelf->__PVT__fpRrStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpRrStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__PVT__fpRrStage__DOT__iqData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrStage__DOT__fpOpInfo[__Vi0] = VL_RAND_RESET_I(17);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrStage__DOT__operandA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrStage__DOT__operandB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrStage__DOT__operandC[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrStage__DOT__opSrc[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRrStage__DOT__opDst[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(228, vlSelf->__PVT__fpRrStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__fpRrStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__fpRrStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__fpRrStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__fpRrStage__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->fpRrStage__DOT____Vlvbound_hc328d351__0 = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 5; ++__Vi1) {
            vlSelf->__PVT__fpExStage__DOT__flush[__Vi0][__Vi1] = VL_RAND_RESET_I(1);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 4; ++__Vi1) {
            VL_RAND_RESET_W(107, vlSelf->__PVT__fpExStage__DOT__localPipeReg[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 4; ++__Vi1) {
            VL_RAND_RESET_W(107, vlSelf->__PVT__fpExStage__DOT__nextLocalPipeReg[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(228, vlSelf->__PVT__fpExStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        for (int __Vi1 = 0; __Vi1 < 5; ++__Vi1) {
            VL_RAND_RESET_W(93, vlSelf->__PVT__fpExStage__DOT__iqData[__Vi0][__Vi1]);
        }
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fpOpInfo[__Vi0] = VL_RAND_RESET_I(17);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__opType[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fpuCode[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__rm[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__stRM[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__dynRM[__Vi0] = VL_RAND_RESET_I(3);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fuOpA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fuOpB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fuOpC[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__dataOut[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fflagsOut[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fmaDataOut[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__otherDataOut[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fmaMulLHS[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fmaMulRHS[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fmaAddend[__Vi0] = VL_RAND_RESET_I(32);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__addFFlagsOut[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__mulFFlagsOut[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__fmaFFlagsOut[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__otherFFlagsOut[__Vi0] = VL_RAND_RESET_I(5);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__isDivSqrt[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(144, vlSelf->__PVT__fpExStage__DOT__nextStage[__Vi0]);
    }
    vlSelf->__PVT__fpExStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__fpExStage__DOT__unnamedblk1__DOT__unnamedblk2__DOT__j = 0;
    vlSelf->__PVT__fpExStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__fpExStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__j = 0;
    vlSelf->__PVT__fpExStage__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__fpExStage__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__fpExStage__DOT__unnamedblk9__DOT__i = 0;
    vlSelf->__PVT__fpExStage__DOT__unnamedblk9__DOT__unnamedblk10__DOT__j = 0;
    vlSelf->fpExStage__DOT____Vlvbound_h67e5a0b6__1 = VL_RAND_RESET_I(32);
    vlSelf->fpExStage__DOT____Vlvbound_h67e5a0b6__2 = VL_RAND_RESET_I(32);
    vlSelf->fpExStage__DOT____Vlvbound_h67e5a0b6__3 = VL_RAND_RESET_I(32);
    vlSelf->fpExStage__DOT____Vlvbound_h21f88c3e__0 = VL_RAND_RESET_I(3);
    vlSelf->fpExStage__DOT____Vlvbound_h21f88c3e__1 = VL_RAND_RESET_I(3);
    vlSelf->fpExStage__DOT____Vlvbound_h41c08120__0 = VL_RAND_RESET_I(32);
    vlSelf->fpExStage__DOT____Vlvbound_h41c08120__1 = VL_RAND_RESET_I(32);
    vlSelf->fpExStage__DOT____Vlvbound_h41c08120__2 = VL_RAND_RESET_I(32);
    vlSelf->fpExStage__DOT____Vlvbound_hc7cf90cd__2 = VL_RAND_RESET_I(5);
    vlSelf->fpExStage__DOT____Vlvbound_h00bc0b09__0 = VL_RAND_RESET_I(1);
    vlSelf->fpExStage__DOT____Vlvbound_h00bc0b09__1 = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(77, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_lhs);
    VL_RAND_RESET_W(77, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_rhs);
    VL_RAND_RESET_W(77, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__multiplier_addend);
    VL_RAND_RESET_W(77, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__fma_result);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__is_subtract = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__is_sub = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mullhs_is_zero = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulrhs_is_zero = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mullhs_is_inf = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulrhs_is_inf = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__addend_is_inf = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mul_sign = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT__mulres_expo = VL_RAND_RESET_I(10);
    vlSelf->fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_9 = VL_RAND_RESET_I(1);
    vlSelf->fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_10 = VL_RAND_RESET_I(10);
    vlSelf->fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg0__DOT____VdfgRegularize_heeb2d446_0_13 = VL_RAND_RESET_I(24);
    VL_RAND_RESET_W(81, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg1__DOT__pipeReg);
    VL_RAND_RESET_W(81, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg2__DOT__pipeReg);
    VL_RAND_RESET_W(159, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__pipeReg);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__leading_zeros = VL_RAND_RESET_I(8);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__virtual_expo = VL_RAND_RESET_I(10);
    VL_RAND_RESET_W(168, vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__pipeReg);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT__final_result = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(99, vlSelf->fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg4__DOT____VdfgRegularize_hbad5a435_0_0);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__resultOut = VL_RAND_RESET_I(32);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fflagsOut = VL_RAND_RESET_I(5);
    for (int __Vi0 = 0; __Vi0 < 4; ++__Vi0) {
        vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__pipeReg[__Vi0] = VL_RAND_RESET_Q(37);
    }
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_sign = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_sign = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_expo = VL_RAND_RESET_I(8);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_expo = VL_RAND_RESET_I(8);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_mant = VL_RAND_RESET_I(23);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_mant = VL_RAND_RESET_I(23);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_zero = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_zero = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_inf = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_inf = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_nan = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_nan = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_snan = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__rhs_is_snan = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_subnormal = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_normal = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_is_smaller = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__lhs_equal_rhs = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__fmt_unsigned = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpOther__DOT__unnamedblk1__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(144, vlSelf->__PVT__fpRwStage__DOT__pipeReg[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(72, vlSelf->__PVT__fpRwStage__DOT__alWriteData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__PVT__fpRwStage__DOT__iqData[__Vi0]);
    }
    vlSelf->__PVT__fpRwStage__DOT__stall = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpRwStage__DOT__clear = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRwStage__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRwStage__DOT__update[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRwStage__DOT__valid[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpRwStage__DOT__regValid[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__PVT__fpRwStage__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__fpRwStage__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__fpRwStage__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__fpRwStage__DOT__unnamedblk4__DOT__i = 0;
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpDivSqrtUnit__DOT__regPhase[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpDivSqrtUnit__DOT__nextPhase[__Vi0] = VL_RAND_RESET_I(2);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpDivSqrtUnit__DOT__finished[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpDivSqrtUnit__DOT__flush[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpDivSqrtUnit__DOT__rst_divider[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpDivSqrtUnit__DOT__regActiveListPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpDivSqrtUnit__DOT__nextActiveListPtr[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->__PVT__fpDivSqrtUnit__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__fpDivSqrtUnit__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->fpDivSqrtUnit__DOT____Vlvbound_ha7e1d1c3__0 = VL_RAND_RESET_I(6);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regCounter = VL_RAND_RESET_I(5);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextCounter = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(170, vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regData);
    VL_RAND_RESET_W(170, vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextData);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__regResult = VL_RAND_RESET_I(32);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__nextResult = VL_RAND_RESET_I(32);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_zero = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rhs_is_zero = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_inf = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rhs_is_inf = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__lhs_is_nan = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_lhs_expo = VL_RAND_RESET_I(10);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_rhs_expo = VL_RAND_RESET_I(10);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_lhs_mant = VL_RAND_RESET_I(24);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__v_rhs_mant = VL_RAND_RESET_I(24);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__dividend_normalize = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__virtual_expo = VL_RAND_RESET_I(10);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__subnormal = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__q = VL_RAND_RESET_I(3);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__div = VL_RAND_RESET_I(4);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__rem = VL_RAND_RESET_I(27);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__quo = VL_RAND_RESET_I(26);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__before_round = VL_RAND_RESET_Q(48);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__round_away = VL_RAND_RESET_I(1);
    vlSelf->__PVT__fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__result_expo = VL_RAND_RESET_I(8);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__intRR[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__memRR[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__memEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__memMT[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__intBypassCtrl[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__complexBypassCtrl[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__memBypassCtrl[__Vi0] = VL_RAND_RESET_I(21);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassController__DOT__fpBypassCtrl[__Vi0] = VL_RAND_RESET_I(21);
    }
    vlSelf->__PVT__bypassController__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__bypassController__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__bypassController__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__bypassController__DOT__unnamedblk6__DOT__i = 0;
    vlSelf->__PVT__bypassController__DOT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntRR__DOT__body = VL_RAND_RESET_I(8);
    vlSelf->__PVT__bypassController__DOT__stgInt__BRA__0__KET____DOT__stgIntEX__DOT__body = VL_RAND_RESET_I(8);
    vlSelf->__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntRR__DOT__body = VL_RAND_RESET_I(8);
    vlSelf->__PVT__bypassController__DOT__stgInt__BRA__1__KET____DOT__stgIntEX__DOT__body = VL_RAND_RESET_I(8);
    vlSelf->__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemRR__DOT__body = VL_RAND_RESET_I(8);
    vlSelf->__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemEX__DOT__body = VL_RAND_RESET_I(8);
    vlSelf->__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMT__DOT__body = VL_RAND_RESET_I(8);
    vlSelf->__PVT__bypassController__DOT__stgMem__BRA__0__KET____DOT__stgMemMA__DOT__body = VL_RAND_RESET_I(8);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassNetwork__DOT__intDst[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassNetwork__DOT__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__bypassNetwork__DOT__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassNetwork__DOT__memDst[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassNetwork__DOT__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__bypassNetwork__DOT__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__PVT__bypassNetwork__DOT__unnamedblk1__DOT__i = 0;
    vlSelf->__PVT__bypassNetwork__DOT__unnamedblk2__DOT__i = 0;
    vlSelf->__PVT__bypassNetwork__DOT__unnamedblk3__DOT__i = 0;
    vlSelf->__PVT__bypassNetwork__DOT__unnamedblk4__DOT__i = 0;
    vlSelf->__PVT__bypassNetwork__DOT__unnamedblk5__DOT__i = 0;
    vlSelf->__PVT__bypassNetwork__DOT__stgInt__BRA__0__KET____DOT__stgIntEX__DOT__body = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__bypassNetwork__DOT__stgInt__BRA__0__KET____DOT__stgIntWB__DOT__body = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__bypassNetwork__DOT__stgInt__BRA__1__KET____DOT__stgIntEX__DOT__body = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__bypassNetwork__DOT__stgInt__BRA__1__KET____DOT__stgIntWB__DOT__body = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__bypassNetwork__DOT__stgMem__BRA__0__KET____DOT__stgMemMA__DOT__body = VL_RAND_RESET_Q(33);
    vlSelf->__PVT__bypassNetwork__DOT__stgMem__BRA__0__KET____DOT__stgMemWB__DOT__body = VL_RAND_RESET_Q(33);
    VL_RAND_RESET_W(119, vlSelf->__PVT__recoveryManager__DOT__regState);
    VL_RAND_RESET_W(119, vlSelf->__PVT__recoveryManager__DOT__nextState);
    vlSelf->__PVT__recoveryManager__DOT__toRecoveryPhase = VL_RAND_RESET_I(1);
    vlSelf->__PVT__recoveryManager__DOT__toCommitPhase = VL_RAND_RESET_I(1);
    vlSelf->__PVT__recoveryManager__DOT__refetchFromCSR = VL_RAND_RESET_I(1);
    vlSelf->__PVT__recoveryManager__DOT__recoveredPC = VL_RAND_RESET_I(20);
    vlSelf->__PVT__recoveryManager__DOT__exceptionOpPtr = VL_RAND_RESET_I(6);
    VL_RAND_RESET_W(352, vlSelf->__PVT__csrUnit__DOT__csrReg);
    VL_RAND_RESET_W(352, vlSelf->__PVT__csrUnit__DOT__csrNext);
    vlSelf->__PVT__csrUnit__DOT__rv = VL_RAND_RESET_I(32);
    vlSelf->__PVT__csrUnit__DOT__wv = VL_RAND_RESET_I(32);
    vlSelf->__PVT__csrUnit__DOT__mcycle = VL_RAND_RESET_I(32);
    vlSelf->__PVT__csrUnit__DOT__jumpTarget = VL_RAND_RESET_I(32);
    vlSelf->__PVT__csrUnit__DOT__regCommitNum = VL_RAND_RESET_I(2);
    vlSelf->__PVT__csrUnit__DOT__externalInterruptCodeReg = VL_RAND_RESET_I(5);
    vlSelf->__PVT__cacheFlushManager__DOT__icFlushReq = VL_RAND_RESET_I(1);
    vlSelf->__PVT__cacheFlushManager__DOT__dcFlushReq = VL_RAND_RESET_I(1);
    vlSelf->__PVT__cacheFlushManager__DOT__regPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__cacheFlushManager__DOT__nextPhase = VL_RAND_RESET_I(2);
    vlSelf->__PVT__cacheFlushManager__DOT__regIcFlushComplete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__cacheFlushManager__DOT__nextIcFlushComplete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__cacheFlushManager__DOT__regDcFlushComplete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__cacheFlushManager__DOT__nextDcFlushComplete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__cacheFlushManager__DOT__cacheFlushComplete = VL_RAND_RESET_I(1);
    vlSelf->__PVT__interruptCtrl__DOT__reqInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__interruptCtrl__DOT__triggerInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__interruptCtrl__DOT__reqTimerInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__interruptCtrl__DOT__reqExternalInterrupt = VL_RAND_RESET_I(1);
    vlSelf->__PVT__interruptCtrl__DOT__interruptCode = VL_RAND_RESET_I(5);
    vlSelf->__PVT__interruptCtrl__DOT__interruptTargetAddr = VL_RAND_RESET_I(20);
    VL_RAND_RESET_W(352, vlSelf->__PVT__interruptCtrl__DOT__csrReg);
    vlSelf->__PVT__interruptCtrl__DOT__interruptCodeConv = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(128, vlSelf->__PVT__ioUnit__DOT__tmReg);
    VL_RAND_RESET_W(128, vlSelf->__PVT__ioUnit__DOT__tmNext);
    vlSelf->__PVT__ioUnit__DOT__phyRawReadAddr = VL_RAND_RESET_I(20);
    vlSelf->__PVT__ioUnit__DOT__phyRawWriteAddr = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_StepOverCacheLine__0__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_StepOverCacheLine__0__pc1 = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_StepOverCacheLine__0__pc2 = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_StepOverCacheLine__1__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_StepOverCacheLine__1__pc1 = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_StepOverCacheLine__1__pc2 = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToAddrFromPC__2__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToAddrFromPC__2__pc = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToAddrFromPC__3__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToAddrFromPC__3__pc = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPhyAddrFromLogical__4__phyAddr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToPhyAddrFromLogical__6__phyAddr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_GetMemoryMapType__15__Vfuncout = VL_RAND_RESET_I(2);
    vlSelf->__Vfunc_GetMemoryMapType__15__addr = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__16__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__16__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__16__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__17__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__17__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__17__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__18__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__19__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__19__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__19__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__20__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__20__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__20__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__21__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__21__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__21__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__22__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__23__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__23__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__23__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeSystem__24__microOps);
    vlSelf->__Vtask_RISCV_DecodeSystem__24__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeSystem__24__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitSystemOp__25__opInfo);
    vlSelf->__Vtask_RISCV_EmitSystemOp__25__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitSystemOp__25__isfSystem = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitSystemOp__25__opFunct12 = VL_RAND_RESET_I(12);
    vlSelf->__Vtask_RISCV_EmitSystemOp__25__systemOp = VL_RAND_RESET_Q(53);
    vlSelf->__Vtask_RISCV_EmitSystemOp__25__undefined = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitCSR_Op__26__opInfo);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__26__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__26__isfSystem = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__26__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__26__memOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__27__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__28__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__28__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__28__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__29__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__29__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__29__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__30__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__30__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__30__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__31__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__32__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__32__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__32__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__33__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__33__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__33__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__34__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__34__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__34__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__35__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__36__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__36__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__36__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeJAL__37__microOps);
    vlSelf->__Vtask_RISCV_DecodeJAL__37__insnInfo = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeJAL__37__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeJAL__37__brOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitJAL__38__opInfo);
    vlSelf->__Vtask_RISCV_EmitJAL__38__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitJAL__38__isfU = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_GetJAL_Target__39__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetJAL_Target__39__isfJAL = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__40__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__41__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__41__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__41__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__42__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__42__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__42__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__43__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__43__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__43__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__44__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__45__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__45__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__45__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__46__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__46__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__46__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__47__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__47__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__47__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__48__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__49__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__49__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__49__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__50__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__50__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__50__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__51__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__51__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__51__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__52__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__53__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__53__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__53__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeJALR__54__microOps);
    vlSelf->__Vtask_RISCV_DecodeJALR__54__insnInfo = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeJALR__54__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeJALR__54__brOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitJALR__55__opInfo);
    vlSelf->__Vtask_RISCV_EmitJALR__55__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitJALR__55__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_GetJALR_Target__56__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetJALR_Target__56__isfJALR = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__57__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__58__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__58__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__58__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__59__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__59__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__59__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__60__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__60__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__60__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__61__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__62__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__62__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__62__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__63__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__63__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__63__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__64__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__64__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__64__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__65__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__66__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__66__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__66__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeBranch__67__microOps);
    vlSelf->__Vtask_RISCV_DecodeBranch__67__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeBranch__67__brOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitBranch__68__opInfo);
    vlSelf->__Vtask_RISCV_EmitBranch__68__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitBranch__68__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitBranch__68__brFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitBranch__68__condCode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeBrFunct3__69__condCode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeBrFunct3__69__funct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vfunc_GetBranchDisplacement__70__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetBranchDisplacement__70__isfBr = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__71__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__72__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__72__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__72__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__73__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__73__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__73__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__74__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__74__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__74__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__75__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__76__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__76__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__76__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__77__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__77__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__77__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__78__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__78__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__78__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__79__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__80__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__80__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__80__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__81__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__81__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__81__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__82__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__82__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__82__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__83__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__84__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__84__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__84__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__85__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__85__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__85__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__86__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__86__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__86__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__87__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__88__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__88__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__88__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPOp__89__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPOp__89__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPOp__89__fpOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPOp__90__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__rv32fFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__rv32fFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__fcvtfunct5 = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__dstFP = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__rs1FP = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__readrs2 = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__90__rm = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__91__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__91__rv32ffunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__91__rv32ffunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__91__fcvtfunct5 = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__92__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__93__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__93__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__93__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__94__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__94__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__94__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__95__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__95__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__95__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__96__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__97__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__97__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__97__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__98__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__98__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__98__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__99__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__99__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__99__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__100__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__101__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__101__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__101__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPFMAOp__102__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPFMAOp__102__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPFMAOp__102__fpOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPFMAOp__103__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__103__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__103__isfR4 = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__103__opCode = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__103__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__103__rm = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeFPFMAOpFunct3__104__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeFPFMAOpFunct3__104__opCode = VL_RAND_RESET_I(7);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__105__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__106__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__106__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__106__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__107__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__107__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__107__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__108__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__108__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__108__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__109__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__110__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__110__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__110__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__111__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__111__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__111__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__112__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__112__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__112__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__113__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__114__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__114__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__114__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__115__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__115__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__115__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__116__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__116__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__116__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__117__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__118__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__118__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__118__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeUTypeInst__119__microOps);
    vlSelf->__Vtask_RISCV_DecodeUTypeInst__119__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeUTypeInst__119__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitUTypeInst__120__opInfo);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__120__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__120__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__120__isfU = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__120__intOperandImmShift = VL_RAND_RESET_I(30);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__121__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__122__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__122__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__122__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__123__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__123__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__123__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__124__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__124__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__124__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__125__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__126__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__126__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__126__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__127__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__127__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__127__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__128__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__128__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__128__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__129__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__130__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__130__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__130__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeComplexOp__131__microOps);
    vlSelf->__Vtask_RISCV_DecodeComplexOp__131__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeComplexOp__131__complexOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitComplexOp__132__opInfo);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__rv32mFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__isMul = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__mulCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_EmitComplexOp__132__divCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_DecodeComplexOpFunct3__133__mulCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_DecodeComplexOpFunct3__133__divCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_DecodeComplexOpFunct3__133__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__134__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__135__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__135__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__135__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeZba__136__microOps);
    vlSelf->__Vtask_RISCV_DecodeZba__136__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeZba__136__zbaOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitZba__137__opInfo);
    vlSelf->__Vtask_RISCV_EmitZba__137__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZba__137__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZba__137__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZba__137__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZba__137__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZba__137__zbaFunct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__138__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__139__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__139__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__139__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeZicond__140__microOps);
    vlSelf->__Vtask_RISCV_DecodeZicond__140__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeZicond__140__zicondOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitZicond__141__opInfo);
    vlSelf->__Vtask_RISCV_EmitZicond__141__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZicond__141__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZicond__141__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZicond__141__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZicond__141__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZicond__141__czeroFunct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__142__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__143__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__143__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__143__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeOp__144__microOps);
    vlSelf->__Vtask_RISCV_DecodeOp__144__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeOp__144__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitOp__145__opInfo);
    vlSelf->__Vtask_RISCV_EmitOp__145__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOp__145__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOp__145__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOp__145__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOp__145__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOp__145__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitOp__145__opFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitOp__145__shiftFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitOp__145__intOperandImmShift = VL_RAND_RESET_I(30);
    vlSelf->__Vtask_RISCV_EmitOp__145__isShift = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitOp__145__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vtask_RISCV_DecodeOpFunct3__146__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vtask_RISCV_DecodeOpFunct3__146__funct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeOpFunct3__146__funct7 = VL_RAND_RESET_I(7);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__147__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__148__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__148__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__148__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__149__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__149__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__149__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__150__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__150__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__150__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__151__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__152__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__152__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__152__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__153__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__153__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__153__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__154__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__154__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__154__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__155__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__156__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__156__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__156__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__157__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__157__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__157__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__158__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__158__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__158__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__159__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__160__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__160__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__160__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPMemOp__161__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPMemOp__161__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPMemOp__161__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPMemOp__162__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__162__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__162__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__162__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__162__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__162__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__162__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__163__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__163__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__164__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__165__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__165__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__165__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__166__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__166__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__166__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__167__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__167__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__167__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__168__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__169__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__169__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__169__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__170__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__170__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__170__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__171__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__171__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__171__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__172__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__173__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__173__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__173__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeMemOp__174__microOps);
    vlSelf->__Vtask_RISCV_DecodeMemOp__174__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeMemOp__174__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitMemOp__175__opInfo);
    vlSelf->__Vtask_RISCV_EmitMemOp__175__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__175__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__175__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__175__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitMemOp__175__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitMemOp__175__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__176__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__176__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__177__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__178__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__178__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__178__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__179__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__179__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__179__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__180__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__180__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__180__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__181__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__182__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__182__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__182__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__183__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__183__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__183__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__184__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__184__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__184__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__185__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__186__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__186__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__186__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__187__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__187__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__187__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__188__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__188__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__188__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__189__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__190__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__190__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__190__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeUTypeInst__191__microOps);
    vlSelf->__Vtask_RISCV_DecodeUTypeInst__191__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeUTypeInst__191__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitUTypeInst__192__opInfo);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__192__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__192__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__192__isfU = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__192__intOperandImmShift = VL_RAND_RESET_I(30);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__193__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__194__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__194__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__194__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__195__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__195__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__195__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__196__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__196__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__196__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__197__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__198__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__198__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__198__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__199__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__199__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__199__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__200__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__200__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__200__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__201__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__202__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__202__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__202__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeOpImm__203__microOps);
    vlSelf->__Vtask_RISCV_DecodeOpImm__203__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeOpImm__203__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitOpImm__204__opInfo);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__shiftFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__intOperandImmShift = VL_RAND_RESET_I(30);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__isShift = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitOpImm__204__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_ShamtExtention__205__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ShamtExtention__205__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_I_TypeImmExtention__206__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_I_TypeImmExtention__206__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_DecodeOpImmFunct3__207__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vtask_RISCV_DecodeOpImmFunct3__207__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__208__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__209__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__209__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__209__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__210__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__210__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__210__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__211__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__211__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__211__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__212__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__213__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__213__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__213__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__214__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__214__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__214__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__215__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__215__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__215__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__216__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__217__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__217__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__217__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeMiscMem__218__microOps);
    vlSelf->__Vtask_RISCV_DecodeMiscMem__218__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeMiscMem__218__miscMemOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitMiscMemOp__219__opInfo);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__219__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__219__isfMiscMem = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__219__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__219__miscMemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__220__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__221__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__221__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__221__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__222__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__222__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__222__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__223__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__223__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__223__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__224__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__225__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__225__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__225__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__226__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__226__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__226__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__227__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__227__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__227__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__228__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__229__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__229__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__229__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__230__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__230__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__230__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__231__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__231__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__231__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__232__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__233__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__233__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__233__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPMemOp__234__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPMemOp__234__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPMemOp__234__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPMemOp__235__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__235__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__235__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__235__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__235__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__235__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__235__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__236__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__236__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__237__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__238__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__238__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__238__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__239__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__239__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__239__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__240__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__240__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__240__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__241__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__242__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__242__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__242__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__243__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__243__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__243__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__244__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__244__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__244__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__245__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__246__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__246__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__246__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeMemOp__247__microOps);
    vlSelf->__Vtask_RISCV_DecodeMemOp__247__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeMemOp__247__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitMemOp__248__opInfo);
    vlSelf->__Vtask_RISCV_EmitMemOp__248__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__248__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__248__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__248__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitMemOp__248__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitMemOp__248__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__249__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__249__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__250__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__251__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__251__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__251__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__252__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__252__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__252__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__253__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__253__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__253__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__254__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__255__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__255__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__255__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__256__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__256__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__256__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__257__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__257__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__257__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__258__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__259__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__259__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__259__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__260__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__260__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__260__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__261__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__261__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__261__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__262__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__263__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__263__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__263__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__264__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__264__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__264__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__265__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__265__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__265__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__266__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__267__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__267__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__267__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__268__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__268__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__268__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__269__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__269__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__269__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__270__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__271__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__271__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__271__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeSystem__272__microOps);
    vlSelf->__Vtask_RISCV_DecodeSystem__272__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeSystem__272__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitSystemOp__273__opInfo);
    vlSelf->__Vtask_RISCV_EmitSystemOp__273__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitSystemOp__273__isfSystem = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitSystemOp__273__opFunct12 = VL_RAND_RESET_I(12);
    vlSelf->__Vtask_RISCV_EmitSystemOp__273__systemOp = VL_RAND_RESET_Q(53);
    vlSelf->__Vtask_RISCV_EmitSystemOp__273__undefined = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitCSR_Op__274__opInfo);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__274__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__274__isfSystem = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__274__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitCSR_Op__274__memOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__275__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__276__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__276__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__276__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__277__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__277__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__277__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__278__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__278__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__278__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__279__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__280__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__280__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__280__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__281__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__281__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__281__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__282__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__282__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__282__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__283__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__284__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__284__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__284__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeJAL__285__microOps);
    vlSelf->__Vtask_RISCV_DecodeJAL__285__insnInfo = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeJAL__285__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeJAL__285__brOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitJAL__286__opInfo);
    vlSelf->__Vtask_RISCV_EmitJAL__286__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitJAL__286__isfU = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_GetJAL_Target__287__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetJAL_Target__287__isfJAL = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__288__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__289__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__289__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__289__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__290__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__290__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__290__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__291__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__291__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__291__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__292__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__293__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__293__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__293__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__294__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__294__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__294__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__295__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__295__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__295__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__296__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__297__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__297__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__297__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__298__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__298__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__298__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__299__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__299__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__299__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__300__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__301__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__301__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__301__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeJALR__302__microOps);
    vlSelf->__Vtask_RISCV_DecodeJALR__302__insnInfo = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeJALR__302__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeJALR__302__brOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitJALR__303__opInfo);
    vlSelf->__Vtask_RISCV_EmitJALR__303__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitJALR__303__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_GetJALR_Target__304__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetJALR_Target__304__isfJALR = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__305__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__306__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__306__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__306__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__307__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__307__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__307__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__308__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__308__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__308__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__309__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__310__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__310__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__310__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__311__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__311__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__311__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__312__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__312__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__312__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__313__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__314__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__314__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__314__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeBranch__315__microOps);
    vlSelf->__Vtask_RISCV_DecodeBranch__315__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeBranch__315__brOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitBranch__316__opInfo);
    vlSelf->__Vtask_RISCV_EmitBranch__316__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitBranch__316__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitBranch__316__brFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitBranch__316__condCode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeBrFunct3__317__condCode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeBrFunct3__317__funct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vfunc_GetBranchDisplacement__318__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetBranchDisplacement__318__isfBr = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__319__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__320__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__320__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__320__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__321__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__321__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__321__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__322__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__322__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__322__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__323__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__324__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__324__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__324__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__325__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__325__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__325__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__326__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__326__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__326__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__327__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__328__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__328__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__328__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__329__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__329__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__329__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__330__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__330__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__330__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__331__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__332__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__332__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__332__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__333__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__333__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__333__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__334__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__334__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__334__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__335__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__336__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__336__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__336__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPOp__337__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPOp__337__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPOp__337__fpOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPOp__338__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__rv32fFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__rv32fFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__fcvtfunct5 = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__dstFP = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__rs1FP = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__readrs2 = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPOp__338__rm = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__339__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__339__rv32ffunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_DecodeFPOpFunct3__339__fcvtfunct5 = VL_RAND_RESET_I(5);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__340__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__341__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__341__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__341__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__342__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__342__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__342__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__343__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__343__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__343__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__344__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__345__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__345__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__345__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__346__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__346__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__346__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__347__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__347__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__347__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__348__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__349__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__349__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__349__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPFMAOp__350__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPFMAOp__350__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPFMAOp__350__fpOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPFMAOp__351__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__351__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__351__isfR4 = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__351__opCode = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__351__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitFPFMAOp__351__rm = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeFPFMAOpFunct3__352__fpuCode = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_DecodeFPFMAOpFunct3__352__opCode = VL_RAND_RESET_I(7);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__353__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__354__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__354__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__354__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__355__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__355__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__355__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__356__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__356__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__356__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__357__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__358__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__358__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__358__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__359__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__359__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__359__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__360__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__360__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__360__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__361__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__362__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__362__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__362__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__363__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__363__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__363__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__364__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__364__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__364__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__365__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__366__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__366__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__366__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeUTypeInst__367__microOps);
    vlSelf->__Vtask_RISCV_DecodeUTypeInst__367__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeUTypeInst__367__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitUTypeInst__368__opInfo);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__368__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__368__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__368__isfU = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__368__intOperandImmShift = VL_RAND_RESET_I(30);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__369__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__370__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__370__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__370__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__371__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__371__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__371__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__372__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__372__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__372__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__373__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__374__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__374__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__374__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__375__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__375__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__375__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__376__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__376__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__376__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__377__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__378__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__378__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__378__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeComplexOp__379__microOps);
    vlSelf->__Vtask_RISCV_DecodeComplexOp__379__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeComplexOp__379__complexOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitComplexOp__380__opInfo);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__rv32mFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__isMul = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__mulCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_EmitComplexOp__380__divCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_DecodeComplexOpFunct3__381__mulCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_DecodeComplexOpFunct3__381__divCode = VL_RAND_RESET_I(2);
    vlSelf->__Vtask_RISCV_DecodeComplexOpFunct3__381__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__382__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__383__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__383__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__383__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeZba__384__microOps);
    vlSelf->__Vtask_RISCV_DecodeZba__384__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeZba__384__zbaOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitZba__385__opInfo);
    vlSelf->__Vtask_RISCV_EmitZba__385__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZba__385__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZba__385__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZba__385__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZba__385__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZba__385__zbaFunct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__386__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__387__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__387__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__387__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeZicond__388__microOps);
    vlSelf->__Vtask_RISCV_DecodeZicond__388__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeZicond__388__zicondOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitZicond__389__opInfo);
    vlSelf->__Vtask_RISCV_EmitZicond__389__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZicond__389__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZicond__389__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZicond__389__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitZicond__389__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitZicond__389__czeroFunct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__390__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__391__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__391__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__391__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeOp__392__microOps);
    vlSelf->__Vtask_RISCV_DecodeOp__392__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeOp__392__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitOp__393__opInfo);
    vlSelf->__Vtask_RISCV_EmitOp__393__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOp__393__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOp__393__srcRegNumB = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOp__393__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOp__393__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOp__393__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitOp__393__opFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitOp__393__shiftFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitOp__393__intOperandImmShift = VL_RAND_RESET_I(30);
    vlSelf->__Vtask_RISCV_EmitOp__393__isShift = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitOp__393__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vtask_RISCV_DecodeOpFunct3__394__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vtask_RISCV_DecodeOpFunct3__394__funct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeOpFunct3__394__funct7 = VL_RAND_RESET_I(7);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__395__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__396__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__396__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__396__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__397__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__397__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__397__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__398__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__398__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__398__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__399__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__400__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__400__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__400__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__401__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__401__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__401__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__402__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__402__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__402__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__403__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__404__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__404__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__404__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__405__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__405__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__405__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__406__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__406__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__406__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__407__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__408__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__408__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__408__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPMemOp__409__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPMemOp__409__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPMemOp__409__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPMemOp__410__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__410__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__410__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__410__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__410__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__410__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__410__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__411__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__411__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__412__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__413__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__413__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__413__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__414__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__414__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__414__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__415__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__415__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__415__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__416__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__417__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__417__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__417__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__418__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__418__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__418__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__419__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__419__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__419__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__420__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__421__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__421__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__421__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeMemOp__422__microOps);
    vlSelf->__Vtask_RISCV_DecodeMemOp__422__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeMemOp__422__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitMemOp__423__opInfo);
    vlSelf->__Vtask_RISCV_EmitMemOp__423__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__423__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__423__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__423__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitMemOp__423__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitMemOp__423__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__424__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__424__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__425__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__426__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__426__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__426__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__427__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__427__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__427__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__428__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__428__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__428__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__429__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__430__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__430__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__430__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__431__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__431__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__431__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__432__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__432__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__432__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__433__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__434__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__434__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__434__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__435__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__435__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__435__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__436__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__436__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__436__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__437__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__438__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__438__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__438__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeUTypeInst__439__microOps);
    vlSelf->__Vtask_RISCV_DecodeUTypeInst__439__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeUTypeInst__439__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitUTypeInst__440__opInfo);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__440__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__440__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__440__isfU = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitUTypeInst__440__intOperandImmShift = VL_RAND_RESET_I(30);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__441__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__442__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__442__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__442__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__443__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__443__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__443__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__444__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__444__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__444__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__445__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__446__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__446__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__446__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__447__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__447__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__447__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__448__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__448__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__448__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__449__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__450__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__450__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__450__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeOpImm__451__microOps);
    vlSelf->__Vtask_RISCV_DecodeOpImm__451__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeOpImm__451__intOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitOpImm__452__opInfo);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__srcRegNumA = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__dstRegNum = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__shiftFunct7 = VL_RAND_RESET_I(7);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__intOperandImmShift = VL_RAND_RESET_I(30);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__isShift = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitOpImm__452__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_ShamtExtention__453__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ShamtExtention__453__isfR = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_I_TypeImmExtention__454__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_I_TypeImmExtention__454__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_DecodeOpImmFunct3__455__aluCode = VL_RAND_RESET_I(4);
    vlSelf->__Vtask_RISCV_DecodeOpImmFunct3__455__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__456__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__457__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__457__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__457__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__458__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__458__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__458__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__459__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__459__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__459__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__460__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__461__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__461__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__461__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__462__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__462__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__462__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__463__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__463__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__463__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__464__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__465__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__465__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__465__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeMiscMem__466__microOps);
    vlSelf->__Vtask_RISCV_DecodeMiscMem__466__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeMiscMem__466__miscMemOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitMiscMemOp__467__opInfo);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__467__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__467__isfMiscMem = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__467__opFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitMiscMemOp__467__miscMemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__468__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__469__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__469__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__469__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__470__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__470__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__470__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__471__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__471__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__471__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__472__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__473__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__473__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__473__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__474__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__474__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__474__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__475__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__475__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__475__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__476__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__477__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__477__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__477__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__478__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__478__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__478__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__479__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__479__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__479__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__480__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__481__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__481__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__481__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeFPMemOp__482__microOps);
    vlSelf->__Vtask_RISCV_DecodeFPMemOp__482__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeFPMemOp__482__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitFPMemOp__483__opInfo);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__483__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__483__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__483__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__483__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__483__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitFPMemOp__483__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__484__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__484__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__485__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__486__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__486__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__486__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__487__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__487__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__487__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__488__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__488__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__488__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__489__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__490__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__490__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__490__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__491__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__491__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__491__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__492__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__492__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__492__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__493__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__494__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__494__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__494__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeMemOp__495__microOps);
    vlSelf->__Vtask_RISCV_DecodeMemOp__495__isf = VL_RAND_RESET_I(32);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeMemOp__495__memOp);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitMemOp__496__opInfo);
    vlSelf->__Vtask_RISCV_EmitMemOp__496__isf = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__496__isfS = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__496__isfI = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_RISCV_EmitMemOp__496__memFunct3 = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_EmitMemOp__496__isLoad = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitMemOp__496__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__497__memAccessMode = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_RISCV_DecodeMemAccessMode__497__funct3 = VL_RAND_RESET_I(3);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__498__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__499__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__499__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__499__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__500__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__500__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__500__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__501__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__501__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__501__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__502__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__503__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__503__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__503__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__504__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__504__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__504__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__505__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__505__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__505__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__506__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__507__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__507__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__507__op);
    VL_RAND_RESET_W(228, vlSelf->__Vtask_RISCV_DecodeIllegal__508__microOps);
    vlSelf->__Vtask_RISCV_DecodeIllegal__508__illegalPC = VL_RAND_RESET_I(1);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_DecodeIllegal__508__opInfo);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_RISCV_EmitIllegalOp__509__opInfo);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__509__illegalPC = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_RISCV_EmitIllegalOp__509__systemOp = VL_RAND_RESET_Q(53);
    VL_RAND_RESET_W(76, vlSelf->__Vtask_EmitInvalidOp__510__op);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__511__Vfuncout);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__511__src);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_ModifyMicroOp__511__op);
    vlSelf->__Vfunc_idStage__DOT__ToInsnLane__512__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_ExtendBranchDisplacement__514__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ExtendBranchDisplacement__514__brDisp = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetJAL_Target__515__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetJAL_Target__515__isfJAL = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ExtendBranchDisplacement__516__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ExtendBranchDisplacement__516__brDisp = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetBranchDisplacement__517__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_GetBranchDisplacement__517__isfBr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_SelectiveFlushDetector__518__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__518__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__518__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__518__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__518__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__518__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__519__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__519__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__519__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__519__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__519__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__519__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__520__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__520__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__520__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__520__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__520__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__520__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__521__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__521__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__521__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__521__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__521__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__521__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__522__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__522__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__522__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__522__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__522__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__522__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__523__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__523__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__523__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__523__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__523__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__523__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__524__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__524__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__524__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__524__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__524__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__524__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__525__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__525__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__525__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__525__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__525__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__525__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__526__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__527__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__528__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__529__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__530__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__530__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__530__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__530__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__530__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__530__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__531__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectOperandIntReg__534__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_SelectOperandIntReg__535__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_SelectiveFlushDetector__536__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__538__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_ToPC_FromAddr__540__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__540__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ExtendBranchDisplacement__541__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ExtendBranchDisplacement__541__brDisp = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_AddJALR_TargetOffset__542__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_AddJALR_TargetOffset__542__data = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_AddJALR_TargetOffset__542__disp = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_AddJALR_TargetOffset__542__target = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ExtendJALR_Target__543__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ExtendJALR_Target__543__brDisp = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_SelectiveFlushDetector__548__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__549__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__549__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__549__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__549__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__549__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__549__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__550__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__551__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__552__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__553__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__554__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__555__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__Vfuncout = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__dividend = VL_RAND_RESET_Q(33);
    vlSelf->__Vfunc_mulDivUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__divUnit__DOT__div__DOT__CountSignificantBits__556__unnamedblk1__DOT__i = 0;
    vlSelf->__Vfunc_SelectiveFlushDetector__557__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__557__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__557__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__557__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__557__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__557__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__558__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectOperand__559__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_SelectOperand__560__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_SelectiveFlushDetector__561__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__562__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_ToPhyAddrFromLogical__563__phyAddr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_GetMemoryMapType__564__Vfuncout = VL_RAND_RESET_I(2);
    vlSelf->__Vfunc_SelectiveFlushDetector__566__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_IsMisalignedAddress__567__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_IsMisalignedAddress__567__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_IsMisalignedAddress__567__size = VL_RAND_RESET_I(2);
    vlSelf->__Vfunc_SelectiveFlushDetector__568__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_IsMisalignedAddress__569__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_IsMisalignedAddress__569__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_IsMisalignedAddress__569__size = VL_RAND_RESET_I(2);
    vlSelf->__Vfunc_SelectiveFlushDetector__570__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftForwardedData__571__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftForwardedData__571__srcLine = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftForwardedData__571__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftForwardedData__571__data = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__572__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__572__srcLine = VL_RAND_RESET_Q(64);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__572__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__572__data = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__573__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__573__srcLine = VL_RAND_RESET_Q(64);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__573__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ShiftCacheLineData__573__data = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_loadStoreUnit__DOT__ExtendLoadData__574__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_LSQ_SelectBits__577__Vfuncout = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__577__data = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__577__offset = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__577__width = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__577__unnamedblk1__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__577__ret = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__580__Vfuncout = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__580__data = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__580__offset = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__580__width = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__580__unnamedblk1__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__580__ret = 0;
    vlSelf->__Vfunc_LoadQueuePtrToAge__581__Vfuncout = VL_RAND_RESET_I(5);
    vlSelf->__Vfunc_LoadQueuePtrToAge__581__ptr = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_LoadQueuePtrToAge__581__head = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_LoadQueuePtrToAge__581__age = VL_RAND_RESET_I(5);
    vlSelf->__Vfunc_LoadQueuePtrToAge__582__Vfuncout = VL_RAND_RESET_I(5);
    vlSelf->__Vfunc_LoadQueuePtrToAge__582__ptr = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_LoadQueuePtrToAge__582__head = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_LoadQueuePtrToAge__582__age = VL_RAND_RESET_I(5);
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__584__ret = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__586__ret = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_LSQ_ToFullAddrFromBlockAddr__587__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_LSQ_ToFullAddrFromBlockAddr__587__blockAddr = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__Vfuncout = VL_RAND_RESET_Q(64);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__data = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheLine__588__line = VL_RAND_RESET_Q(64);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__Vfuncout = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__wordWE = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__byteWE = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__blockAddr = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__ret = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__589__we = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_LSQ_SelectBits__590__Vfuncout = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__590__data = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__590__offset = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__590__width = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__590__unnamedblk1__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__590__ret = 0;
    vlSelf->__Vfunc_LSQ_ToFullAddrFromBlockAddr__591__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_LSQ_ToFullAddrFromBlockAddr__591__blockAddr = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__Vfuncout = VL_RAND_RESET_Q(64);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__data = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheLine__592__line = VL_RAND_RESET_Q(64);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__Vfuncout = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__wordWE = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__byteWE = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__blockAddr = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__ret = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_storeCommitter__DOT__GenerateDCacheWriteEnable__593__we = VL_RAND_RESET_I(4);
    vlSelf->__Vfunc_LSQ_SelectBits__594__Vfuncout = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__594__data = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__594__offset = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__594__width = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__594__unnamedblk1__DOT__i = 0;
    vlSelf->__Vfunc_LSQ_SelectBits__594__ret = 0;
    vlSelf->__Vfunc_LSQ_ToScalarWordDataFromBlockData__595__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_LSQ_ToScalarWordDataFromBlockData__595__ret = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__596__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_LSQ_ToFullPhyAddrFromBlockAddrAndWordWE__596__ret = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__601__Vfuncout = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__601__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__602__Vfuncout = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__602__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__606__Vfuncout = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__606__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToTagPartFromFullAddr__607__Vfuncout = VL_RAND_RESET_I(11);
    vlSelf->__Vfunc_ToTagPartFromFullAddr__607__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_SelectiveFlushDetector__608__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__609__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_ToLineAddrFromFullAddr__612__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToLineAddrFromFullAddr__612__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToLineAddrFromFullAddr__613__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_ToLineAddrFromFullAddr__613__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vtask_MergeStoreDataToLine__614__dstLine = VL_RAND_RESET_Q(64);
    vlSelf->__Vtask_MergeStoreDataToLine__614__fetchedLine = VL_RAND_RESET_Q(64);
    vlSelf->__Vtask_MergeStoreDataToLine__614__storedLine = VL_RAND_RESET_Q(64);
    vlSelf->__Vtask_MergeStoreDataToLine__614__storedDirty = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_BuildFullAddr__615__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_BuildFullAddr__615__index = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_BuildFullAddr__615__tag = VL_RAND_RESET_I(11);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__616__Vfuncout = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_ToIndexPartFromFullAddr__616__addr = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_BuildFullAddr__617__Vfuncout = VL_RAND_RESET_I(22);
    vlSelf->__Vfunc_BuildFullAddr__617__index = VL_RAND_RESET_I(8);
    vlSelf->__Vfunc_BuildFullAddr__617__tag = VL_RAND_RESET_I(11);
    vlSelf->__Vfunc_SelectiveFlushDetector__618__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__619__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__619__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__619__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__619__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__619__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__619__opPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__620__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__621__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__622__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__623__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__leading_zeros_count__624__Vfuncout = VL_RAND_RESET_I(7);
    VL_RAND_RESET_W(76, vlSelf->__Vfunc_fpExStage__DOT__genblk2__BRA__0__KET____DOT__fpFMA__DOT__stg3__DOT__leading_zeros_count__624__x);
    vlSelf->__Vtask_FP32CVT_I2F__625__lhs = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__625__fmt_unsigned = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__625__rm = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_FP32CVT_I2F__625__result = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__625__fflags = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_FP32CVT_I2F__625__lhs_is_neg = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__625__lzc = VL_RAND_RESET_I(6);
    vlSelf->__Vtask_FP32CVT_I2F__625__abs_lhs = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__625__shifted_lhs = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__625__expo = VL_RAND_RESET_I(8);
    vlSelf->__Vtask_FP32CVT_I2F__625__mant = VL_RAND_RESET_I(23);
    vlSelf->__Vtask_FP32CVT_I2F__625__round_up = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__625__exp_plus_one = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__625__lsb = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__625__guard = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__625__sticky = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_leading_zeros_count__626__Vfuncout = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_leading_zeros_count__626__x = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__627__lhs = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__627__fmt_unsigned = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__627__rm = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_FP32CVT_I2F__627__result = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__627__fflags = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_FP32CVT_I2F__627__lhs_is_neg = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__627__lzc = VL_RAND_RESET_I(6);
    vlSelf->__Vtask_FP32CVT_I2F__627__abs_lhs = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__627__shifted_lhs = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_I2F__627__expo = VL_RAND_RESET_I(8);
    vlSelf->__Vtask_FP32CVT_I2F__627__mant = VL_RAND_RESET_I(23);
    vlSelf->__Vtask_FP32CVT_I2F__627__round_up = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__627__exp_plus_one = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__627__lsb = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__627__guard = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_I2F__627__sticky = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_leading_zeros_count__628__Vfuncout = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_leading_zeros_count__628__x = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_F2I__629__lhs = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_F2I__629__fmt_unsigned = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__rm = VL_RAND_RESET_I(3);
    vlSelf->__Vtask_FP32CVT_F2I__629__result = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_F2I__629__fflags = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_FP32CVT_F2I__629__sign = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__expo = VL_RAND_RESET_I(8);
    vlSelf->__Vtask_FP32CVT_F2I__629__mant = VL_RAND_RESET_I(23);
    vlSelf->__Vtask_FP32CVT_F2I__629__shift_amount = VL_RAND_RESET_I(5);
    vlSelf->__Vtask_FP32CVT_F2I__629__int_result = VL_RAND_RESET_I(32);
    vlSelf->__Vtask_FP32CVT_F2I__629__lower_bits = VL_RAND_RESET_I(23);
    vlSelf->__Vtask_FP32CVT_F2I__629__round_up = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__lsb = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__guard = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__sticky = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__is_invalid = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__lhs_is_neg = VL_RAND_RESET_I(1);
    vlSelf->__Vtask_FP32CVT_F2I__629__lhs_is_nan = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__630__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__631__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__632__Vfuncout = VL_RAND_RESET_I(10);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__632__x = VL_RAND_RESET_I(23);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__633__Vfuncout = VL_RAND_RESET_I(10);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__633__x = VL_RAND_RESET_I(23);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__634__Vfuncout = VL_RAND_RESET_I(10);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__634__x = VL_RAND_RESET_I(23);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__635__Vfuncout = VL_RAND_RESET_I(10);
    vlSelf->__Vfunc_fpDivSqrtUnit__DOT__BlockDivUnit__BRA__0__KET____DOT__fpDivSqrter__DOT__leading_zeros_count__635__x = VL_RAND_RESET_I(23);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__637__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__637__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__637__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__637__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__638__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__638__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__638__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__638__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__639__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__639__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__639__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__639__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__640__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__640__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__640__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__640__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__641__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__641__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__641__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__641__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__642__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__642__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__642__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__642__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__643__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__643__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__643__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__643__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__644__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__644__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__644__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__644__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__645__intEX[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__645__intWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__645__memMA[__Vi0] = VL_RAND_RESET_I(8);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassController__DOT__SelectReg__645__memWB[__Vi0] = VL_RAND_RESET_I(8);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__646__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__646__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__646__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__646__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__646__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__647__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__647__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__647__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__647__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__647__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__648__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__648__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__648__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__648__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__648__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__649__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__649__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__649__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__649__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__649__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__650__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__650__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__650__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__650__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__650__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__651__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__651__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__651__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__651__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__651__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__652__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__652__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__652__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__652__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__652__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__653__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__653__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__653__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__653__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__653__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__654__Vfuncout = VL_RAND_RESET_Q(33);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__654__intEX[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__654__intWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__654__memMA[__Vi0] = VL_RAND_RESET_Q(33);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__Vfunc_bypassNetwork__DOT__SelectData__654__memWB[__Vi0] = VL_RAND_RESET_Q(33);
    }
    vlSelf->__Vfunc_ToPC_FromAddr__656__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__656__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToPC_FromAddr__657__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__657__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToPC_FromAddr__658__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__658__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToPC_FromAddr__659__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__659__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToPC_FromAddr__660__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__660__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToPC_FromAddr__661__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__661__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToPC_FromAddr__662__Vfuncout = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToPC_FromAddr__662__addr = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToAddrFromPC__663__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToAddrFromPC__663__pc = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToAddrFromPC__664__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToAddrFromPC__664__pc = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToAddrFromPC__665__Vfuncout = VL_RAND_RESET_I(32);
    vlSelf->__Vfunc_ToAddrFromPC__665__pc = VL_RAND_RESET_I(20);
    vlSelf->__Vfunc_ToTrapCodeFromExecState__666__Vfuncout = VL_RAND_RESET_I(5);
    vlSelf->__Vfunc_ToTrapCodeFromExecState__666__state = VL_RAND_RESET_I(4);
}
