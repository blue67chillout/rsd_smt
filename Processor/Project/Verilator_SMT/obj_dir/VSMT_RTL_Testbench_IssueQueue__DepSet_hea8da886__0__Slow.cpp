// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_IssueQueue.h"

VL_ATTR_COLD void VSMT_RTL_Testbench_IssueQueue___ctor_var_reset(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___ctor_var_reset\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__releaseEntry[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 8; ++__Vi0) {
        vlSelf->__PVT__releasePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    vlSelf->__PVT__freeListReset = VL_RAND_RESET_I(1);
    vlSelf->__PVT__freeListResetCycleCount = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__alPtrReg[__Vi0] = VL_RAND_RESET_I(6);
    }
    vlSelf->__PVT__flush = VL_RAND_RESET_I(16);
    vlSelf->__PVT__prevFlushAtRecovery = VL_RAND_RESET_I(16);
    vlSelf->__PVT__issueQueueReturnIndex = VL_RAND_RESET_I(1);
    vlSelf->__PVT__issueQueueReturnIndexCycleCount = VL_RAND_RESET_I(3);
    vlSelf->__PVT__returnIndexOffset = VL_RAND_RESET_I(4);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__writePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 6; ++__Vi0) {
        vlSelf->__PVT__selectedPtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellout__issueQueueFreeList__poppedData[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__issueQueueFreeList__pop[__Vi0] = VL_RAND_RESET_I(1);
    }
    vlSelf->__Vcellinp__issueQueueFreeList__rst = VL_RAND_RESET_I(1);
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__intIssuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__PVT__memIssuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__PVT__intIssuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__PVT__memIssuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__complexIssuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(82, vlSelf->__PVT__complexIssuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        vlSelf->__PVT__fpIssuePtr[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__PVT__fpIssuedData[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__Vcellout__intPayloadRAM__rv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(139, vlSelf->__Vcellinp__intPayloadRAM__wv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__intPayloadRAM__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__intPayloadRAM__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(82, vlSelf->__Vcellout__complexPayloadRAM__rv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(82, vlSelf->__Vcellinp__complexPayloadRAM__wv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__complexPayloadRAM__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__complexPayloadRAM__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__Vcellout__memPayloadRAM__rv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(125, vlSelf->__Vcellinp__memPayloadRAM__wv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__memPayloadRAM__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__memPayloadRAM__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 1; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__Vcellout__fpPayloadRAM__rv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        VL_RAND_RESET_W(93, vlSelf->__Vcellinp__fpPayloadRAM__wv[__Vi0]);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__fpPayloadRAM__wa[__Vi0] = VL_RAND_RESET_I(4);
    }
    for (int __Vi0 = 0; __Vi0 < 2; ++__Vi0) {
        vlSelf->__Vcellinp__fpPayloadRAM__we[__Vi0] = VL_RAND_RESET_I(1);
    }
    for (int __Vi0 = 0; __Vi0 < 16; ++__Vi0) {
        vlSelf->__PVT__opId[__Vi0] = VL_RAND_RESET_I(12);
    }
    vlSelf->__PVT__unnamedblk7__DOT__i = 0;
    vlSelf->__PVT__unnamedblk8__DOT__i = 0;
    vlSelf->__PVT__unnamedblk10__DOT__i = 0;
    vlSelf->__PVT__unnamedblk11__DOT__i = 0;
    vlSelf->__PVT__unnamedblk12__DOT__i = 0;
    vlSelf->__PVT__unnamedblk13__DOT__i = 0;
    vlSelf->__Vfunc_SelectiveFlushDetector__0__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__1__Vfuncout = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__1__detectRange = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__1__headPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__1__tailPtr = VL_RAND_RESET_I(6);
    vlSelf->__Vfunc_SelectiveFlushDetector__1__flushAllInsns = VL_RAND_RESET_I(1);
    vlSelf->__Vfunc_SelectiveFlushDetector__1__opPtr = VL_RAND_RESET_I(6);
}
